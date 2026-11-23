# Volet « Planning » : les heures qu'on s'apprête à poser, face au CA qu'elles
# devront produire. Le seul volet qui regarde devant.
#
# Deux graphiques, une grammaire : LA BARRE EST CE QUI EST PRÉVU, LE TRAIT EST
# LA RÉFÉRENCE.
#
#   1. Heures : barre = heures planifiées, trait = heures habituelles.
#   2. Euros  : barre = CA habituel de ce jour de semaine, trait = objectif.
#
# Les heures comparées sont les heures VARIABLES : du service, un jour
# d'ouverture (même définition que R/travail.R, cf. JOURS_FERMES). Le service
# posé un jour fermé (préparation, nettoyage) et les heures fixes
# (transformation, brasserie, support) ne sont donnés qu'en survol.

#### Contrat de la table ####

# DB_HEURES_PLANNING : une ligne par DATE x SERVICE.
#
#   DATE    <date>  le jour planifié
#   SERVICE <chr>   le département, même vocabulaire que DB_HEURES$DEPARTEMENT
#   HEURES  <dbl>   heures planifiées ce jour-là pour ce service
PLANNING_COLONNES <- c("DATE", "SERVICE", "HEURES")

# Table optionnelle : TRUE seulement si elle est exploitable.
planning_valide <- function(db) {
  !is.null(db) && is.data.frame(db) &&
    all(PLANNING_COLONNES %in% names(db)) && nrow(db) > 0
}

#### Historique du planning ####

# Le calendrier de shifts ne garde que l'avenir. Pour avoir un passé (et donc
# des heures habituelles), chaque import :
#
#   1. lit l'historique accumulé dans un Google Sheet (PATH_HEURES_PLANNING) ;
#   2. le fusionne avec ce que le calendrier donne aujourd'hui ;
#   3. réécrit le tout dans le même Sheet.

# Onglet du classeur, créé à la première écriture.
SHEET_PLANNING <- "HEURES PLANNING"

# Met une table de planning à la forme du contrat : après un aller-retour par
# Google Sheets, dates et heures peuvent revenir sous n'importe quel type.
normalise_planning <- function(db) {
  if (is.null(db) || !is.data.frame(db) || nrow(db) == 0 ||
      !all(PLANNING_COLONNES %in% names(db)))
    return(NULL)

  res <- db %>%
    transmute(DATE    = parse_date_souple(aplatit_colonne(DATE)),
              SERVICE = as.character(aplatit_colonne(SERVICE)),
              HEURES  = suppressWarnings(
                          as.numeric(sub(",", ".", aplatit_colonne(HEURES),
                                         fixed = TRUE)))) %>%
    filter(!is.na(DATE), !is.na(HEURES), !is.na(SERVICE)) %>%
    group_by(DATE, SERVICE) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE), .groups = "drop") %>%
    arrange(DATE, SERVICE)

  if (nrow(res) == 0) NULL else res
}

# googlesheets4 rend une colonne en liste quand les types sont mêlés : chaque
# cellule vide devient NA, le nombre de lignes est préservé.
aplatit_colonne <- function(x) {
  if (!is.list(x)) return(x)
  vapply(x, function(v) if (length(v) == 1) as.character(v) else NA_character_,
         FUN.VALUE = character(1))
}


# Fusion de l'historique et de l'import du jour, PAR JOUR : sur un jour que
# le calendrier couvre encore, il fait autorité en entier (un service dont
# tous les shifts ont été annulés doit disparaître).
fusionne_planning <- function(ancien, nouveau) {
  ancien  <- normalise_planning(ancien)
  nouveau <- normalise_planning(nouveau)

  if (is.null(nouveau)) return(ancien)
  if (is.null(ancien))  return(nouveau)

  bind_rows(ancien %>% filter(!DATE %in% nouveau$DATE), nouveau) %>%
    arrange(DATE, SERVICE)
}

# Lecture de l'historique. NULL, sans erreur, si le classeur est vide,
# l'onglet absent ou Drive injoignable.
lit_planning_historique <- function(ss) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    cli::cli_alert_warning("googlesheets4 absent : historique du planning ignoré")
    return(NULL)
  }
  lu <- try({
    onglets <- googlesheets4::sheet_names(ss)
    if (!SHEET_PLANNING %in% onglets) NULL
    else googlesheets4::read_sheet(ss, sheet = SHEET_PLANNING,
                                   col_types = "c")
  }, silent = TRUE)

  if (inherits(lu, "try-error")) {
    cli::cli_alert_warning("Historique du planning illisible : {as.character(lu)}")
    return(NULL)
  }
  normalise_planning(lu)
}

# Réécriture, seulement s'il y a quelque chose à écrire : une table vide
# effacerait l'historique.
ecrit_planning_historique <- function(ss, db) {
  db <- normalise_planning(db)
  if (is.null(db)) {
    cli::cli_alert_warning("Rien à écrire dans l'historique du planning")
    return(invisible(NULL))
  }
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    cli::cli_alert_warning("googlesheets4 absent : historique du planning non sauvé")
    return(invisible(NULL))
  }

  ecrit <- try(
    googlesheets4::write_sheet(
      db %>% mutate(DATE = format(DATE, "%Y-%m-%d")),
      ss = ss, sheet = SHEET_PLANNING),
    silent = TRUE)

  if (inherits(ecrit, "try-error"))
    cli::cli_alert_warning("Historique du planning non sauvé : {as.character(ecrit)}")
  else
    cli::cli_alert_success("Historique du planning : {nrow(db)} lignes, {length(unique(db$DATE))} jours")

  invisible(db)
}

# Le mouvement complet, tel que import.R l'appelle ; les accès sont
# injectables pour les tests.
historise_planning <- function(nouveau, ss,
                               lire  = lit_planning_historique,
                               ecrire = ecrit_planning_historique) {
  ancien <- lire(ss)
  fusion <- fusionne_planning(ancien, nouveau)
  ecrire(ss, fusion)
  fusion
}

#### Libellés de secteur ####

# Des libellés de chaque source aux secteurs du dashboard, en un seul
# endroit (import.R, Planning, paie). Horeko écrit « Fabrik de boissons », la
# paie « Fabrique de boissons ». Un libellé inconnu tombe dans « Secteur
# inconnu », visible.
normalise_secteur <- function(x) {
  dplyr::case_when(
    x == "Transfo alimentaire"  ~ "Transformation alimentaire",
    x == "Fabrik de boissons"   ~ "Brasserie",
    x == "Fabrique de boissons" ~ "Brasserie",
    x == "Support"              ~ "Support",
    x == "Service"              ~ "Service",
    x %in% names(COULEURS_SECTEURS) ~ x,   # déjà au bon format
    TRUE                        ~ "Secteur inconnu"
  )
}

#### Heures planifiées ####

# Le service posé un jour de fermeture : compté avec les heures fixes.
SECTEUR_SERVICE_FERME <- "Service (jour fermé)"

# Heures par jour × secteur, libellés normalisés, doublons additionnés. Le
# service posé un jour fermé est renommé, pas supprimé.
planning_secteurs <- function(db_planning) {
  if (!planning_valide(db_planning)) return(NULL)
  db_planning %>%
    transmute(DATE = as.Date(DATE),
              SECTEUR = normalise_secteur(as.character(SERVICE)),
              HEURES = as.numeric(HEURES)) %>%
    filter(!is.na(DATE), !is.na(HEURES)) %>%
    mutate(SECTEUR = if_else(SECTEUR == "Service" & jour_ferme(DATE),
                             SECTEUR_SERVICE_FERME, SECTEUR)) %>%
    group_by(DATE, SECTEUR) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE), .groups = "drop")
}

# Une ligne par jour : heures totales, heures de service, CA réalisé s'il est
# connu. STATUT sépare le mesuré (jour échu) du projeté (jour à venir).
planning_jour <- function(db_planning, db_kpi, date_veille) {
  sect <- planning_secteurs(db_planning)
  if (is.null(sect)) return(NULL)

  # Comme R/travail.R : le service est variable, le reste fixe.
  heures <- sect %>%
    group_by(DATE) %>%
    summarise(H_VARIABLE = sum(HEURES[SECTEUR == "Service"], na.rm = TRUE),
              H_FIXE     = sum(HEURES[SECTEUR != "Service"], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(H_TOTAL = H_VARIABLE + H_FIXE)

  ca <- db_kpi %>% select(DATE, CA = ventes)

  heures %>%
    left_join(ca, by = "DATE") %>%
    mutate(
      STATUT = if_else(DATE <= date_veille, "Échu", "À venir"),
      # Un jour échu sans CA vaut zéro : des heures posées, rien produit.
      CA = if_else(STATUT == "Échu", replace_na(CA, 0), NA_real_),
      # Au dénominateur, les heures variables seules (comme dans R/travail.R).
      CA_PAR_HEURE = if_else(STATUT == "Échu" & H_VARIABLE > 0,
                             CA / H_VARIABLE, NA_real_)
    ) %>%
    arrange(DATE)
}

# Heures fixes prévues par jour et par secteur, pour le survol.
detail_heures_fixes <- function(db_planning) {
  sect <- planning_secteurs(db_planning)
  if (is.null(sect)) return(NULL)
  sect %>%
    filter(SECTEUR != "Service", HEURES > 0) %>%
    arrange(DATE, desc(HEURES))
}

#### Réservations ####

# Couverts réservés par jour, donnés en infobulle comme contexte.
couverts_par_jour <- function(resa) {
  if (is.null(resa) || !is.data.frame(resa) || nrow(resa) == 0 ||
      !all(c("DATE", "NB_PERS") %in% names(resa)))
    return(NULL)
  resa %>%
    group_by(DATE) %>%
    summarise(COUVERTS = sum(NB_PERS, na.rm = TRUE),
              RESA = n(), .groups = "drop")
}

#### Heures habituelles ####

# Heures planifiées habituelles par jour de semaine : médiane sur les
# dernières semaines de référence. Tant que le planning n'a pas assez de
# passé, repli sur les heures réellement travaillées ; la source est renvoyée.
heures_habituelles <- function(jour, db_couts_travail, n_semaines = 8) {
  depuis_planning <- medianes_par_jour_semaine(
    if (is.null(jour)) NULL else
      jour %>% filter(STATUT == "Échu") %>% select(DATE, VALEUR = H_VARIABLE),
    n_semaines)

  if (!is.null(depuis_planning))
    return(list(table = renomme_mediane(depuis_planning), source = "planning",
                libelle = paste0("médiane des ", attr(depuis_planning, "n_sem"),
                                 " dernières semaines planifiées")))

  # Le repli suit le même périmètre que base_travail() dans R/travail.R.
  reelles <- medianes_par_jour_semaine(
    heures_variables_reelles(db_couts_travail), n_semaines)

  if (!is.null(reelles))
    return(list(table = renomme_mediane(reelles), source = "heures réelles",
                libelle = paste0("médiane des ", attr(reelles, "n_sem"),
                                 " dernières semaines réellement travaillées")))

  list(table = NULL, source = "aucune", libelle = "pas encore de référence")
}

# Les heures réellement variables (service, jour ouvert, créneau de service),
# une ligne par jour. Se tait si des colonnes manquent.
heures_variables_reelles <- function(db_travail) {
  if (is.null(db_travail) || !nrow(db_travail) ||
      !all(c("DATE", "HEURES") %in% names(db_travail))) return(NULL)

  d <- db_travail %>% filter(!jour_ferme(DATE))
  if ("SECTEUR" %in% names(d)) d <- filter(d, SECTEUR == "Service")
  if ("CRENEAU" %in% names(d)) d <- filter(d, CRENEAU %in% c("Midi", "Soir"))
  if (!nrow(d)) return(NULL)

  d %>% group_by(DATE) %>%
    summarise(VALEUR = sum(HEURES, na.rm = TRUE), .groups = "drop")
}

# La médiane générique s'appelle MEDIANE ; côté heures on la veut H_MEDIANE.
renomme_mediane <- function(tbl) {
  if (is.null(tbl)) return(NULL)
  tbl %>% select(JOUR_SEM, H_MEDIANE = MEDIANE, N_SEMAINES)
}

# Médiane par jour de semaine sur les n dernières semaines complètes ; NULL
# si l'historique est trop court. Sert aux heures comme au CA.
medianes_par_jour_semaine <- function(db, n_semaines = 8, min_semaines = 2) {
  if (is.null(db) || nrow(db) == 0) return(NULL)

  d <- db %>%
    mutate(SEMAINE = debut_periode(DATE, "semaine")) %>%
    filter(!is.na(VALEUR))
  semaines <- sort(unique(d$SEMAINE))
  if (length(semaines) < min_semaines) return(NULL)
  gardees <- tail(semaines, n_semaines)

  res <- d %>%
    filter(SEMAINE %in% gardees) %>%
    mutate(JOUR_SEM = wday(DATE, week_start = 1)) %>%
    group_by(JOUR_SEM) %>%
    summarise(MEDIANE = median(VALEUR, na.rm = TRUE),
              N_SEMAINES = n_distinct(SEMAINE), .groups = "drop")

  attr(res, "n_sem") <- length(gardees)
  res
}

#### Chiffre d'affaires habituel ####

# Le CA médian d'un jour de semaine, sur les mêmes semaines de référence. Les
# jours à zéro sont gardés : un jour habituellement fermé a une médiane basse.
ca_habituel <- function(db_kpi, date_veille, n_semaines = 8) {
  if (is.null(db_kpi) || nrow(db_kpi) == 0)
    return(list(table = NULL, libelle = "pas encore de référence"))

  med <- medianes_par_jour_semaine(
    db_kpi %>% filter(DATE <= date_veille) %>% transmute(DATE, VALEUR = ventes),
    n_semaines)

  if (is.null(med))
    return(list(table = NULL, libelle = "pas encore de référence"))

  list(table = med %>% select(JOUR_SEM, CA_MEDIAN = MEDIANE),
       libelle = paste0("médiane des ", attr(med, "n_sem"),
                        " dernières semaines"))
}

#### Les jours à venir ####

# Une ligne par jour à venir :
#
#   H_VARIABLE / H_MEDIANE  heures de service posées et habituelles
#   H_FIXE                  heures hors service, pour le survol
#   CA_MEDIAN / OBJECTIF    CA habituel de ce jour de semaine, et objectif
#   COUVERTS                contexte, pour l'infobulle
projection_planning <- function(jour, db_objectifs, habituel, ca_hab,
                                resa = NULL) {
  if (is.null(jour)) return(NULL)
  avenir <- jour %>% filter(STATUT == "À venir")
  if (nrow(avenir) == 0) return(NULL)

  med <- habituel$table
  med_ca <- if (is.null(ca_hab)) NULL else ca_hab$table
  couverts <- couverts_par_jour(resa)

  p <- avenir %>%
    left_join(db_objectifs %>% select(DATE, OBJECTIF = ventes), by = "DATE") %>%
    mutate(JOUR_SEM = wday(DATE, week_start = 1))

  p <- if (is.null(med)) mutate(p, H_MEDIANE = NA_real_)
       else left_join(p, med %>% select(JOUR_SEM, H_MEDIANE), by = "JOUR_SEM")

  p <- if (is.null(med_ca)) mutate(p, CA_MEDIAN = NA_real_)
       else left_join(p, med_ca, by = "JOUR_SEM")

  p <- if (is.null(couverts)) mutate(p, COUVERTS = 0, RESA = 0)
       else left_join(p, couverts, by = "DATE") %>%
              mutate(COUVERTS = replace_na(COUVERTS, 0),
                     RESA = replace_na(RESA, 0))

  p %>%
    mutate(
      OBJECTIF     = replace_na(OBJECTIF, 0),
      ECART_H      = H_VARIABLE - H_MEDIANE,
      ECART_H_PCT  = ratio_pct(H_VARIABLE - H_MEDIANE, H_MEDIANE),
      # Le CA que ce jour de semaine rapporte d'habitude, face à l'objectif.
      ECART_CA     = CA_MEDIAN - OBJECTIF,
      COUVRE       = !is.na(CA_MEDIAN) & OBJECTIF > 0 & CA_MEDIAN >= OBJECTIF,
      # CA par heure qu'il faudrait pour tenir l'objectif (infobulle).
      CA_H_REQUIS  = if_else(H_VARIABLE > 0 & OBJECTIF > 0,
                             OBJECTIF / H_VARIABLE, NA_real_),
      # vecteur_jours (global.R) : wday(label = TRUE) et %a suivent la locale du
      # serveur.
      JOUR_LABEL   = paste0(substr(vecteur_jours[JOUR_SEM], 1, 3), " ",
                            format(DATE, "%d/%m"))
    ) %>%
    arrange(DATE)
}

# Résumé de la fenêtre : on additionne avant de diviser.
resume_projection <- function(proj) {
  if (is.null(proj) || nrow(proj) == 0) return(NULL)
  h    <- sum(proj$H_VARIABLE, na.rm = TRUE)
  hf   <- sum(proj$H_FIXE, na.rm = TRUE)
  hmed <- if (all(is.na(proj$H_MEDIANE))) NA_real_
          else sum(proj$H_MEDIANE, na.rm = TRUE)
  obj  <- sum(proj$OBJECTIF, na.rm = TRUE)
  ca   <- if (all(is.na(proj$CA_MEDIAN))) NA_real_
          else sum(proj$CA_MEDIAN, na.rm = TRUE)

  list(
    jours       = nrow(proj),
    H_VARIABLE  = h,
    H_FIXE      = hf,
    H_TOTAL     = h + hf,
    H_MEDIANE   = hmed,
    ECART_H     = if (is.na(hmed)) NA_real_ else h - hmed,
    ECART_H_PCT = if (is.na(hmed)) NA_real_ else ratio_pct(h - hmed, hmed),
    COUVERTS    = sum(proj$COUVERTS, na.rm = TRUE),
    CA_MEDIAN   = ca,
    OBJECTIF    = obj,
    ECART_CA    = if (is.na(ca)) NA_real_ else ca - obj,
    # Le CA par heure de service que l'objectif impose, sur toute la fenêtre.
    CA_H_REQUIS = if (h > 0 && obj > 0) obj / h else NA_real_
  )
}

#### Présentation ####

# Mention pour un jour sans heure de service : un jour fermé est normal, un
# jour ouvert sans personne en salle est une anomalie.
mention_sans_service <- function(dates, h_variable) {
  ifelse(h_variable > 0, "",
  ifelse(jour_ferme(dates),
         "<br><i>jour de fermeture : pas de service</i>",
         "<br><i>aucune heure de service planifiée</i>"))
}

infobulle_jour <- function(d, detail_fixe = NULL) {
  couverts <- ifelse(d$COUVERTS > 0,
                     paste0("<br>", d$COUVERTS, " couverts réservés (",
                            d$RESA, " résa)"),
                     "<br>aucune réservation")
  habituel <- ifelse(is.na(d$H_MEDIANE), "",
                     paste0("<br>habituel : ", round(d$H_MEDIANE), " h",
                            ifelse(is.na(d$ECART_H_PCT), "",
                                   paste0(" (", ifelse(d$ECART_H_PCT >= 0, "+", ""),
                                          round(d$ECART_H_PCT), " %)"))))
  paste0("<b>", d$JOUR_LABEL, "</b>",
         "<br>", round(d$H_VARIABLE), " h de service planifiées", habituel,
         mention_sans_service(d$DATE, d$H_VARIABLE),
         texte_heures_fixes(d$DATE, d$H_FIXE, detail_fixe),
         couverts, "<extra></extra>")
}

# Les heures fixes du jour, en survol : total puis détail par secteur.
texte_heures_fixes <- function(dates, totaux, detail = NULL) {
  totaux <- replace_na(as.numeric(totaux), 0)
  sortie <- ifelse(totaux > 0,
                   paste0("<br>+ ", round(totaux, 1), " h hors service prévues"),
                   "<br>aucune heure hors service prévue")
  if (is.null(detail) || nrow(detail) == 0) return(sortie)

  par_jour <- detail %>%
    group_by(DATE) %>%
    summarise(TXT = paste0("<br><span style='font-size:0.85em'>&nbsp;&nbsp;",
                           paste0(SECTEUR, " ", round(HEURES, 1), " h",
                                  collapse = " · "),
                           "</span>"),
              .groups = "drop")

  i <- match(as.Date(dates), as.Date(par_jour$DATE))
  paste0(sortie, ifelse(is.na(i) | totaux <= 0, "", par_jour$TXT[i]))
  return(NULL)
}

# Graphique 1 — des heures. Barre : planifié. Trait : habituel.
graph_planning_heures <- function(proj, habituel, detail_fixe = NULL) {
  if (is.null(proj) || nrow(proj) == 0)
    return(plotly_empty() %>% layout(title = "Aucun jour à venir"))

  ordre <- factor(proj$JOUR_LABEL, levels = proj$JOUR_LABEL)
  # Au-dessus de l'habitude en ambre (point d'attention), en dessous en vert.
  couleur <- ifelse(is.na(proj$ECART_H_PCT), COUL_NEUTRE,
             ifelse(proj$ECART_H_PCT > 10, COUL_AMBRE, COUL_VERT))

  p <- plot_ly() %>%
    add_bars(x = ordre, y = proj$H_VARIABLE, name = "Heures de service",
             marker = list(color = couleur),
             hovertemplate = infobulle_jour(proj, detail_fixe))

  if (any(!is.na(proj$H_MEDIANE)))
    p <- p %>% add_markers(
      x = ordre, y = proj$H_MEDIANE, name = "Heures habituelles",
      marker = list(symbol = "line-ew-open", size = 26,
                    line = list(width = 3, color = "#260b01")),
      hovertemplate = paste0("<b>", proj$JOUR_LABEL, "</b><br>",
                             round(proj$H_MEDIANE), " h d'habitude",
                             "<extra></extra>"))

  p %>% layout(
    xaxis = list(title = "", tickangle = -35),
    yaxis = list(title = "Heures de service planifiées", rangemode = "tozero"),
    legend = list(orientation = "h", y = -0.25),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Graphique 2 — des euros. Barre : CA habituel. Trait : objectif.
graph_planning_rentabilite <- function(proj) {
  if (is.null(proj) || nrow(proj) == 0)
    return(plotly_empty() %>% layout(title = "Aucun jour à venir"))
  if (all(is.na(proj$CA_MEDIAN)))
    return(plotly_empty() %>%
             layout(title = "Pas encore d'historique de chiffre d'affaires"))

  ordre <- factor(proj$JOUR_LABEL, levels = proj$JOUR_LABEL)
  couleur <- ifelse(proj$OBJECTIF <= 0, COUL_NEUTRE,
             ifelse(proj$COUVRE, COUL_VERT, COUL_ROUGE))

  bulle <- paste0(
    "<b>", proj$JOUR_LABEL, "</b>",
    "<br>habituellement ", format_CA(proj$CA_MEDIAN, -1), " ce jour-là",
    "<br>objectif ", format_CA(proj$OBJECTIF, -1),
    ifelse(proj$OBJECTIF > 0,
           paste0("<br>", ifelse(proj$ECART_CA >= 0, "au-dessus de ", "manque "),
                  format_CA(abs(proj$ECART_CA), -1)), ""),
    ifelse(is.na(proj$CA_H_REQUIS),
           mention_sans_service(proj$DATE, proj$H_VARIABLE),
           paste0("<br>", round(proj$H_VARIABLE), " h de service, soit ",
                  format_CA(proj$CA_H_REQUIS, -1), " / h à tenir")),
    ifelse(proj$COUVERTS > 0,
           paste0("<br>", proj$COUVERTS, " couverts réservés (",
                  proj$RESA, " résa)"),
           "<br>aucune réservation"),
    "<extra></extra>")

  plot_ly() %>%
    add_bars(x = ordre, y = proj$CA_MEDIAN, name = "CA habituel ce jour-là",
             marker = list(color = couleur), hovertemplate = bulle) %>%
    add_markers(x = ordre, y = proj$OBJECTIF, name = "Objectif du jour",
                marker = list(symbol = "line-ew-open", size = 26,
                              line = list(width = 3, color = "#260b01")),
                hovertemplate = paste0("<b>", proj$JOUR_LABEL, "</b><br>",
                                       "objectif ", format_CA(proj$OBJECTIF, -1),
                                       "<extra></extra>")) %>%
    layout(
      xaxis = list(title = "", tickangle = -35),
      yaxis = list(title = "Euros", rangemode = "tozero"),
      legend = list(orientation = "h", y = -0.25),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Quatre tuiles, dans l'ordre des deux graphiques.
kpi_planning_tiles <- function(res, habituel, ca_hab) {
  if (is.null(res))
    return(div(class = "text-muted small p-2",
               "Aucun jour à venir dans le planning."))

  ecart_h <- if (is.na(res$ECART_H_PCT)) "—"
             else paste0(if (res$ECART_H_PCT >= 0) "+" else "",
                         round(res$ECART_H_PCT), " %")
  couleur_h <- if (is.na(res$ECART_H_PCT)) COUL_NEUTRE
               else if (res$ECART_H_PCT > 10) COUL_AMBRE else COUL_VERT

  couvre <- !is.na(res$ECART_CA) && res$ECART_CA >= 0

  div(
    class = "kpi-grid",
    kpi_tile(format(round(res$H_VARIABLE)), "Heures de service planifiées",
             COUL_TRAVAIL, "clock",
             sous_titre = paste0("sur ", res$jours, " jour",
                                 if (res$jours > 1) "s" else "", " à venir",
                                 " · + ", round(res$H_FIXE), " h hors service")),
    kpi_tile(ecart_h, "Par rapport à l'habitude", couleur_h, "code-compare",
             sous_titre = if (is.na(res$H_MEDIANE)) habituel$libelle
                          else paste0("habituel : ", round(res$H_MEDIANE), " h")),
    kpi_tile(if (is.na(res$CA_MEDIAN)) "—" else format_CA(res$CA_MEDIAN, -1),
             "CA habituel sur ces jours", COUL_BRUN, "chart-line",
             sous_titre = if (is.null(ca_hab)) "pas de référence"
                          else ca_hab$libelle),
    kpi_tile(format_CA(res$OBJECTIF, -1), "Objectif de la période",
             if (is.na(res$ECART_CA)) COUL_NEUTRE
             else if (couvre) COUL_VERT else COUL_ROUGE, "bullseye",
             sous_titre = if (is.na(res$ECART_CA)) "—"
                          else if (couvre) paste0("couvert, +",
                                                  format_CA(res$ECART_CA, -1))
                          else paste0("manque ", format_CA(-res$ECART_CA, -1))),
    kpi_tile(format(res$COUVERTS), "Couverts réservés", COUL_MATIERE, "calendar-check",
             sous_titre = "déjà annoncés sur la période")
  )
}

# Le détail, colonne par colonne dans l'ordre de lecture des graphiques.
table_planning_avenir <- function(proj) {
  if (is.null(proj) || nrow(proj) == 0)
    return(tibble(Info = "Aucun jour à venir dans le planning."))
  proj %>%
    transmute(
      # Pas de %a, qui suivrait la locale du serveur.
      Jour                  = paste0(vecteur_jours[wday(DATE, week_start = 1)],
                                     " ", format(DATE, "%d/%m/%Y")),
      Couverts              = COUVERTS,
      `Heures service`      = round(H_VARIABLE, 1),
      `Heures habituelles`  = ifelse(is.na(H_MEDIANE), "—",
                                     as.character(round(H_MEDIANE, 1))),
      `Écart`               = ifelse(is.na(ECART_H), "—",
                                     paste0(ifelse(ECART_H >= 0, "+", ""),
                                            round(ECART_H, 1), " h")),
      `Heures hors service` = round(H_FIXE, 1),
      `CA habituel`         = format_CA(CA_MEDIAN, -1),
      Objectif              = format_CA(OBJECTIF, -1),
      `Écart CA`            = ifelse(is.na(ECART_CA), "—",
                                     paste0(ifelse(ECART_CA >= 0, "+", ""),
                                            format_CA(ECART_CA, -1))))
}
