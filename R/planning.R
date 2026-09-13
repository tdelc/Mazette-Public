# Volet « Planning » : les heures qu'on s'apprête à poser, confrontées au CA
# qu'elles devront produire.
#
# Ce volet regarde DEVANT, contrairement à tous les autres. Il répond à une
# question de planificateur : « si je mets ces heures-là jeudi, est-ce que la
# journée peut être rentable ? »
#
# Deux questions, deux graphiques, et une seule grammaire visuelle pour les
# deux : LA BARRE EST CE QUI EST PRÉVU, LE TRAIT EST LA RÉFÉRENCE.
#
#   1. « Est-ce que je mets plus d'heures que d'habitude ? »
#      barre = heures planifiées du jour, trait = heures habituelles ce
#      jour-là. Rien que des heures, aucun euro.
#
#   2. « Est-ce que ces heures vont ramener assez ? »
#      barre = CA attendu (heures x productivité habituelle),
#      trait = objectif du jour. Rien que des euros.
#
# On a délibérément retiré les grandeurs à deux dérivations, du type « heures
# que l'objectif justifie » : elles se calculent bien mais ne se lisent pas.
# Chaque graphique reste dans UNE unité, et la comparaison y est directe.
#
# Le jour où DB_HEURES_PLANNING portera un coût horaire, ce volet pourra passer
# à une vraie marge ; rien d'autre ne changera.

#### Contrat de la table ####

# DB_HEURES_PLANNING : une ligne par DATE x SERVICE.
#
#   DATE    <date>  le jour planifié
#   SERVICE <chr>   le département, même vocabulaire que DB_HEURES$DEPARTEMENT
#   HEURES  <dbl>   heures planifiées ce jour-là pour ce service
PLANNING_COLONNES <- c("DATE", "SERVICE", "HEURES")

# La table est optionnelle : tant qu'elle n'est pas produite par l'import,
# l'onglet doit le dire, pas planter. Renvoie TRUE seulement si elle est
# exploitable.
planning_valide <- function(db) {
  !is.null(db) && is.data.frame(db) &&
    all(PLANNING_COLONNES %in% names(db)) && nrow(db) > 0
}

#### Libellés de secteur ####

# Les départements du planning et ceux des heures réalisées sortent du même
# outil, avec les mêmes libellés courts. La correspondance vers les secteurs du
# dashboard vit donc ici, en un seul endroit : import.R l'utilise pour
# DB_COUTS_TRAVAIL, ce volet pour le planning. Deux copies auraient fini par
# diverger, et une couleur de secteur muette est un bug silencieux.
normalise_secteur <- function(x) {
  dplyr::case_when(
    x == "Transfo alimentaire"  ~ "Transformation alimentaire",
    x == "Fabrik de boissons"   ~ "Brasserie",
    x == "Support"              ~ "Support",
    x == "Service"              ~ "Service",
    x %in% names(COULEURS_SECTEURS) ~ x,   # déjà au bon format
    TRUE                        ~ "Secteur inconnu"
  )
}

# Palette des secteurs, complétée pour les libellés non reconnus : un secteur
# inattendu doit rester visible dans le graphe plutôt que de disparaître.
couleur_secteur <- function(secteur) {
  ifelse(secteur %in% names(COULEURS_SECTEURS),
         COULEURS_SECTEURS[secteur], COUL_NEUTRE)
}

#### Heures planifiées ####

# Heures par jour x secteur, libellés normalisés et doublons additionnés.
planning_secteurs <- function(db_planning) {
  if (!planning_valide(db_planning)) return(NULL)
  db_planning %>%
    transmute(DATE = as.Date(DATE),
              SECTEUR = normalise_secteur(as.character(SERVICE)),
              HEURES = as.numeric(HEURES)) %>%
    filter(!is.na(DATE), !is.na(HEURES)) %>%
    group_by(DATE, SECTEUR) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE), .groups = "drop")
}

# Une ligne par jour : heures totales, heures de service, et le CA réalisé
# quand il est connu.
#
# STATUT sépare les deux régimes, et c'est la distinction qui porte tout le
# volet : sur un jour échu on MESURE une productivité, sur un jour à venir on
# ne peut que la PROJETER. Les mélanger donnerait des ratios faux.
planning_jour <- function(db_planning, db_kpi, date_veille) {
  sect <- planning_secteurs(db_planning)
  if (is.null(sect)) return(NULL)

  heures <- sect %>%
    group_by(DATE) %>%
    summarise(H_TOTAL = sum(HEURES, na.rm = TRUE),
              H_SERVICE = sum(HEURES[SECTEUR == "Service"], na.rm = TRUE),
              .groups = "drop")

  ca <- db_kpi %>% select(DATE, CA = ventes)

  heures %>%
    left_join(ca, by = "DATE") %>%
    mutate(
      STATUT = if_else(DATE <= date_veille, "Échu", "À venir"),
      # Un jour échu sans CA reste un jour échu : le CA vaut alors zéro, ce qui
      # est l'information utile (des heures posées, rien produit).
      CA = if_else(STATUT == "Échu", replace_na(CA, 0), NA_real_),
      CA_PAR_HEURE = if_else(STATUT == "Échu" & H_TOTAL > 0, CA / H_TOTAL, NA_real_)
    ) %>%
    arrange(DATE)
}

#### Réservations ####

# Couverts réservés par jour. Les réservations ne changent pas l'objectif de
# CA, mais elles expliquent une partie des heures : vingt couverts annoncés un
# mardi justifient du monde en salle. On les donne donc en infobulle, comme
# contexte, sans les mêler au calcul.
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

# Heures planifiées d'un jour de semaine ordinaire, prises en médiane sur les
# dernières semaines de référence.
#
# La comparaison se fait JOUR DE SEMAINE PAR JOUR DE SEMAINE : un samedi ne se
# compare pas à un mardi, et une moyenne tous jours confondus ne dirait rien.
# La médiane plutôt que la moyenne : une semaine de fermeture ou un banquet ne
# doivent pas déplacer la normale.
#
# Comme pour la productivité, on se rabat sur les heures RÉELLEMENT
# travaillées tant que le planning n'a pas assez de passé — sans quoi le
# graphe serait sans repère le premier jour. La source est renvoyée pour être
# affichée.
heures_habituelles <- function(jour, db_couts_travail, n_semaines = 8) {
  depuis_planning <- medianes_par_jour_semaine(
    if (is.null(jour)) NULL else
      jour %>% filter(STATUT == "Échu") %>% select(DATE, VALEUR = H_TOTAL),
    n_semaines)

  if (!is.null(depuis_planning))
    return(list(table = renomme_mediane(depuis_planning), source = "planning",
                libelle = paste0("médiane des ", attr(depuis_planning, "n_sem"),
                                 " dernières semaines planifiées")))

  reelles <- medianes_par_jour_semaine(
    if (is.null(db_couts_travail)) NULL else
      db_couts_travail %>% group_by(DATE) %>%
        summarise(VALEUR = sum(HEURES, na.rm = TRUE), .groups = "drop"),
    n_semaines)

  if (!is.null(reelles))
    return(list(table = renomme_mediane(reelles), source = "heures réelles",
                libelle = paste0("médiane des ", attr(reelles, "n_sem"),
                                 " dernières semaines réellement travaillées")))

  list(table = NULL, source = "aucune", libelle = "pas encore de référence")
}

# La médiane générique s'appelle MEDIANE ; côté heures on la veut H_MEDIANE.
renomme_mediane <- function(tbl) {
  if (is.null(tbl)) return(NULL)
  tbl %>% select(JOUR_SEM, H_MEDIANE = MEDIANE, N_SEMAINES)
}

# Médiane des heures par jour de semaine, sur les n dernières semaines
# COMPLÈTES présentes dans la table. NULL si l'historique est trop court pour
# qu'une médiane veuille dire quelque chose.
medianes_par_jour_semaine <- function(db, n_semaines = 8, min_semaines = 2) {
  # `db` porte DATE et VALEUR : la fonction sert aussi bien aux heures qu'au
  # chiffre d'affaires, et les deux médianes se calculent alors exactement de
  # la même façon — même fenêtre, même robustesse.
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

# Le CA médian d'un jour de semaine ordinaire, sur les mêmes semaines de
# référence que les heures.
#
# C'est LA grandeur à confronter à l'objectif. La version précédente projetait
# les heures par un CA/heure de référence — un nombre unique, constant sur
# toute la fenêtre : la barre n'était alors que les heures remises à l'échelle,
# et ne disait rien que le premier graphique ne disait déjà. Un jeudi et un
# samedi avec le même nombre d'heures affichaient le même CA attendu, ce qui
# est faux.
#
# On ne filtre pas les jours à zéro : un lundi habituellement fermé DOIT avoir
# une médiane basse, c'est l'information juste.
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

# La table qui alimente les deux graphiques et le tableau. Une ligne par jour à
# venir, avec tout ce qu'on veut lui comparer :
#
#   H_TOTAL / H_MEDIANE   les heures posées et les heures habituelles
#   CA_MEDIAN / OBJECTIF  le CA habituel de ce jour de semaine, et l'objectif
#   COUVERTS              contexte, pour l'infobulle
#
# Aucune grandeur dérivée d'une autre dérivée : chaque colonne se lit seule.
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
      ECART_H      = H_TOTAL - H_MEDIANE,
      ECART_H_PCT  = ratio_pct(H_TOTAL - H_MEDIANE, H_MEDIANE),
      # Le CA que ce jour de semaine rapporte d'habitude, face à l'objectif.
      ECART_CA     = CA_MEDIAN - OBJECTIF,
      COUVRE       = !is.na(CA_MEDIAN) & OBJECTIF > 0 & CA_MEDIAN >= OBJECTIF,
      # Ce qu'il faudrait produire par heure pour tenir l'objectif : utile en
      # infobulle, jamais comme grandeur affichée.
      CA_H_REQUIS  = if_else(H_TOTAL > 0 & OBJECTIF > 0, OBJECTIF / H_TOTAL,
                             NA_real_),
      # vecteur_jours (global.R) plutôt que wday(label = TRUE) ou %a : ces
      # deux-là suivent la locale du SERVEUR, et rendent « Sat » sur une
      # machine en locale C — ce qui est le cas courant d'un serveur Shiny.
      JOUR_LABEL   = paste0(substr(vecteur_jours[JOUR_SEM], 1, 3), " ",
                            format(DATE, "%d/%m"))
    ) %>%
    arrange(DATE)
}

# Résumé de la fenêtre. On additionne avant de diviser : la moyenne des ratios
# journaliers donnerait le même poids à un mardi creux et à un samedi plein.
resume_projection <- function(proj) {
  if (is.null(proj) || nrow(proj) == 0) return(NULL)
  h    <- sum(proj$H_TOTAL, na.rm = TRUE)
  hmed <- if (all(is.na(proj$H_MEDIANE))) NA_real_
          else sum(proj$H_MEDIANE, na.rm = TRUE)
  obj  <- sum(proj$OBJECTIF, na.rm = TRUE)
  ca   <- if (all(is.na(proj$CA_MEDIAN))) NA_real_
          else sum(proj$CA_MEDIAN, na.rm = TRUE)

  list(
    jours       = nrow(proj),
    H_TOTAL     = h,
    H_MEDIANE   = hmed,
    ECART_H     = if (is.na(hmed)) NA_real_ else h - hmed,
    ECART_H_PCT = if (is.na(hmed)) NA_real_ else ratio_pct(h - hmed, hmed),
    COUVERTS    = sum(proj$COUVERTS, na.rm = TRUE),
    CA_MEDIAN   = ca,
    OBJECTIF    = obj,
    ECART_CA    = if (is.na(ca)) NA_real_ else ca - obj,
    # Le CA par heure que l'objectif impose, sur toute la fenêtre.
    CA_H_REQUIS = if (h > 0 && obj > 0) obj / h else NA_real_
  )
}

#### Présentation ####

# Infobulle commune aux deux graphiques : mêmes lignes, même ordre, quelle que
# soit l'unité du graphe. On y glisse les couverts réservés, qui expliquent
# souvent l'écart d'heures sans entrer dans le calcul.
infobulle_jour <- function(d) {
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
         "<br>", round(d$H_TOTAL), " h planifiées", habituel,
         couverts, "<extra></extra>")
}

# Graphique 1 — rien que des heures.
# Barre : ce qui est planifié. Trait : ce qu'on met d'habitude ce jour-là.
graph_planning_heures <- function(proj, habituel) {
  if (is.null(proj) || nrow(proj) == 0)
    return(plotly_empty() %>% layout(title = "Aucun jour à venir"))

  ordre <- factor(proj$JOUR_LABEL, levels = proj$JOUR_LABEL)
  # Au-dessus de l'habitude en ambre, en dessous en vert : plus d'heures n'est
  # pas une faute, c'est un point d'attention.
  couleur <- ifelse(is.na(proj$ECART_H_PCT), COUL_NEUTRE,
             ifelse(proj$ECART_H_PCT > 10, COUL_AMBRE, COUL_VERT))

  p <- plot_ly() %>%
    add_bars(x = ordre, y = proj$H_TOTAL, name = "Heures planifiées",
             marker = list(color = couleur),
             hovertemplate = infobulle_jour(proj))

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
    yaxis = list(title = "Heures planifiées", rangemode = "tozero"),
    legend = list(orientation = "h", y = -0.25),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Graphique 2 — rien que des euros.
# Barre : ce que ces heures rapportent au rythme habituel. Trait : l'objectif
# du jour. La barre passe-t-elle le trait ?
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
    ifelse(is.na(proj$CA_H_REQUIS), "",
           paste0("<br>", round(proj$H_TOTAL), " h planifiées, soit ",
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

# Quatre tuiles, dans l'ordre des deux graphiques : d'abord les heures, puis
# les euros. Chacune répond à une question, aucune n'introduit de grandeur
# qu'on ne retrouve pas dans un graphe.
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
    kpi_tile(format(round(res$H_TOTAL)), "Heures planifiées", COUL_TRAVAIL, "clock",
             sous_titre = paste0("sur ", res$jours, " jour",
                                 if (res$jours > 1) "s" else "", " à venir")),
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
      # Même raison qu'au-dessus : pas de %a, qui suivrait la locale serveur.
      Jour                  = paste0(vecteur_jours[wday(DATE, week_start = 1)],
                                     " ", format(DATE, "%d/%m/%Y")),
      Couverts              = COUVERTS,
      `Heures planifiées`   = round(H_TOTAL, 1),
      `Heures habituelles`  = ifelse(is.na(H_MEDIANE), "—",
                                     as.character(round(H_MEDIANE, 1))),
      `Écart`               = ifelse(is.na(ECART_H), "—",
                                     paste0(ifelse(ECART_H >= 0, "+", ""),
                                            round(ECART_H, 1), " h")),
      `CA habituel`         = format_CA(CA_MEDIAN, -1),
      Objectif              = format_CA(OBJECTIF, -1),
      `Écart CA`            = ifelse(is.na(ECART_CA), "—",
                                     paste0(ifelse(ECART_CA >= 0, "+", ""),
                                            format_CA(ECART_CA, -1))))
}

# Carte d'accueil : les heures posées, et si elles couvrent l'objectif.
acc_planning <- function(res) {
  if (is.null(res)) return(corps_vide("Pas de planning à venir."))

  detail <- paste0(
    if (is.na(res$ECART_H_PCT)) "" else
      paste0(if (res$ECART_H_PCT >= 0) "+" else "", round(res$ECART_H_PCT),
             " % vs l'habitude · "),
    res$COUVERTS, " couverts réservés")

  if (is.na(res$ECART_CA))
    return(corps_accueil(paste0(round(res$H_TOTAL), " h"),
                         paste0("planifiées sur ", res$jours, " jours à venir"),
                         COUL_NEUTRE, detail))

  corps_accueil(
    paste0(round(res$H_TOTAL), " h"),
    paste0("planifiées sur ", res$jours, " jours à venir"),
    if (res$ECART_CA >= 0) COUL_VERT else COUL_ROUGE,
    paste0(detail, " · ",
           if (res$ECART_CA >= 0) "objectif couvert" else
             paste0("manque ", format_CA(-res$ECART_CA, -1))))
}
