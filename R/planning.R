# Volet « Planning » : les heures qu'on s'apprête à poser, confrontées au CA
# qu'elles devront produire.
#
# Ce volet regarde DEVANT, contrairement à tous les autres. Il répond à une
# question de planificateur : « si je mets ces heures-là jeudi, est-ce que la
# journée peut être rentable ? »
#
# Faute de coût du travail dans DB_HEURES_PLANNING, « rentable » ne peut pas
# s'exprimer en euros de marge. On le formule donc en PRODUCTIVITÉ :
#
#   CA par heure REQUIS  = objectif de la période / heures planifiées
#   CA par heure DE RÉFÉRENCE = ce que l'établissement fait d'habitude
#
# Si le requis dépasse la référence, le planning est tendu : ces heures ne
# suffiront pas à tenir l'objectif au rythme habituel. L'écart se lit aussi en
# heures, ce qui est plus parlant quand on tient un tableau de service.
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

#### Productivité de référence ####

# Le CA par heure « habituel », auquel on compare le planning.
#
# On le prend sur les semaines COMPLÈTES du planning : une semaine à moitié
# échue mettrait toutes ses heures face à une partie seulement de son CA, et
# tirerait la référence vers le bas.
#
# Tant que le planning n'a pas d'historique — le cas au démarrage, où il ne
# contient que les semaines à venir — on se rabat sur les heures RÉELLEMENT
# travaillées (DB_COUTS_TRAVAIL), qui, elles, ont de l'historique. La source
# retenue est renvoyée pour être affichée : une référence dont on ignore
# l'origine ne vaut rien.
reference_productivite <- function(jour, db_couts_travail, db_kpi,
                                   n_semaines = 8) {
  vide <- list(valeur = NA_real_, source = "aucune", n_semaines = 0,
               libelle = "Pas encore de référence")

  sem <- semaines_planning(jour)
  ok  <- if (is.null(sem)) NULL else
    sem %>% filter(COMPLETE, H_ECHUES > 0, CA > 0) %>% tail(n_semaines)

  if (!is.null(ok) && nrow(ok) >= 2)
    return(list(valeur = median(ok$CA_PAR_HEURE, na.rm = TRUE),
                source = "planning", n_semaines = nrow(ok),
                libelle = paste0("d'après ", nrow(ok),
                                 " semaines de planning échues")))

  ref <- reference_heures_reelles(db_couts_travail, db_kpi, n_semaines)
  if (is.null(ref)) return(vide)
  ref
}

# Repli : CA par heure calculé sur les heures réellement travaillées.
# Ce n'est pas tout à fait la même grandeur — on ne travaille jamais exactement
# ce qu'on a planifié — d'où la mention explicite dans l'interface.
reference_heures_reelles <- function(db_couts_travail, db_kpi, n_semaines = 8) {
  if (is.null(db_couts_travail) || nrow(db_couts_travail) == 0) return(NULL)

  h <- db_couts_travail %>%
    mutate(PERIODE = debut_periode(DATE, "semaine")) %>%
    group_by(PERIODE) %>%
    summarise(H = sum(HEURES, na.rm = TRUE), .groups = "drop")

  c_a <- db_kpi %>%
    mutate(PERIODE = debut_periode(DATE, "semaine")) %>%
    group_by(PERIODE) %>%
    summarise(CA = sum(ventes, na.rm = TRUE), .groups = "drop")

  ag <- inner_join(h, c_a, by = "PERIODE") %>%
    filter(H > 0, CA > 0) %>%
    arrange(PERIODE) %>%
    tail(n_semaines)

  if (nrow(ag) < 2) return(NULL)
  list(valeur = median(ag$CA / ag$H, na.rm = TRUE),
       source = "heures réelles", n_semaines = nrow(ag),
       libelle = paste0("d'après ", nrow(ag),
                        " semaines d'heures réellement travaillées"))
}

#### Agrégat par semaine ####

# Heures planifiées de la semaine entière, mais CA et productivité calculés
# sur les seuls jours échus : c'est ce qui rend une semaine en cours lisible
# sans la faire paraître catastrophique.
semaines_planning <- function(jour) {
  if (is.null(jour) || nrow(jour) == 0) return(NULL)
  jour %>%
    mutate(PERIODE = debut_periode(DATE, "semaine")) %>%
    group_by(PERIODE) %>%
    summarise(
      H_PLANIFIEES = sum(H_TOTAL, na.rm = TRUE),
      H_ECHUES     = sum(H_TOTAL[STATUT == "Échu"], na.rm = TRUE),
      H_A_VENIR    = sum(H_TOTAL[STATUT == "À venir"], na.rm = TRUE),
      CA           = sum(CA, na.rm = TRUE),
      JOURS_ECHUS  = sum(STATUT == "Échu"),
      JOURS_A_VENIR = sum(STATUT == "À venir"),
      .groups = "drop") %>%
    mutate(
      COMPLETE = JOURS_A_VENIR == 0,
      CA_PAR_HEURE = if_else(H_ECHUES > 0, CA / H_ECHUES, NA_real_)
    ) %>%
    arrange(PERIODE)
}

# Heures par semaine x secteur, pour l'empilement du graphe.
semaines_secteurs <- function(db_planning) {
  sect <- planning_secteurs(db_planning)
  if (is.null(sect)) return(NULL)
  sect %>%
    mutate(PERIODE = debut_periode(DATE, "semaine")) %>%
    group_by(PERIODE, SECTEUR) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE), .groups = "drop")
}

#### Projection sur les jours à venir ####

# Confronte les heures planifiées à l'objectif de CA de la même période.
#
#   CA_H_REQUIS = objectif / heures planifiées
#       « avec ces heures, il faut produire tant par heure »
#   H_SOUTENABLES = objectif / référence
#       « à productivité habituelle, l'objectif demande tant d'heures »
#   ECART_H = heures planifiées - heures soutenables
#       positif = heures posées au-delà de ce que l'objectif justifie
#
# TENSION exprime le même écart en pourcentage de la référence : c'est le
# nombre à regarder en premier.
projection_planning <- function(jour, db_objectifs, reference,
                                date_veille) {
  if (is.null(jour)) return(NULL)
  avenir <- jour %>% filter(STATUT == "À venir")
  if (nrow(avenir) == 0) return(NULL)

  obj <- db_objectifs %>% select(DATE, OBJECTIF = ventes)
  ref <- reference$valeur

  avenir %>%
    left_join(obj, by = "DATE") %>%
    mutate(
      OBJECTIF      = replace_na(OBJECTIF, 0),
      CA_ATTENDU    = if (is.na(ref)) NA_real_ else H_TOTAL * ref,
      CA_H_REQUIS   = if_else(H_TOTAL > 0 & OBJECTIF > 0,
                              OBJECTIF / H_TOTAL, NA_real_),
      H_SOUTENABLES = if (is.na(ref)) NA_real_ else
                        if_else(OBJECTIF > 0, OBJECTIF / ref, NA_real_),
      ECART_H       = H_TOTAL - H_SOUTENABLES,
      TENSION       = if (is.na(ref)) NA_real_ else
                        ratio_pct(CA_H_REQUIS - ref, ref)
    ) %>%
    arrange(DATE)
}

# Résumé d'une projection sur toute sa fenêtre. On additionne les objectifs et
# les heures AVANT de faire le rapport : la moyenne des ratios journaliers
# donnerait un poids identique à un mardi creux et à un samedi plein.
resume_projection <- function(proj, reference) {
  if (is.null(proj) || nrow(proj) == 0) return(NULL)
  h   <- sum(proj$H_TOTAL, na.rm = TRUE)
  obj <- sum(proj$OBJECTIF, na.rm = TRUE)
  ref <- reference$valeur
  requis <- if (h > 0 && obj > 0) obj / h else NA_real_

  list(
    jours         = nrow(proj),
    H_TOTAL       = h,
    OBJECTIF      = obj,
    CA_ATTENDU    = if (is.na(ref)) NA_real_ else h * ref,
    CA_H_REQUIS   = requis,
    REFERENCE     = ref,
    H_SOUTENABLES = if (is.na(ref) || obj == 0) NA_real_ else obj / ref,
    ECART_H       = if (is.na(ref) || obj == 0) NA_real_ else h - obj / ref,
    TENSION       = if (is.na(ref) || is.na(requis)) NA_real_ else
                      ratio_pct(requis - ref, ref)
  )
}

#### Présentation ####

# Bandeau du volet. La tuile de tête est la TENSION : c'est le seul nombre qui
# répond directement à « ce planning est-il tenable ? ».
kpi_planning_tiles <- function(res, reference) {
  if (is.null(res))
    return(div(class = "text-muted small p-2",
               "Aucun jour à venir dans le planning."))

  # Une tension positive veut dire qu'il faudra produire plus par heure que
  # d'habitude. On tolère 5 % avant de passer à l'ambre : la référence est une
  # médiane, pas une promesse.
  couleur_tension <- if (is.na(res$TENSION)) COUL_NEUTRE
                     else if (res$TENSION <= 0)  COUL_VERT
                     else if (res$TENSION <= 15) COUL_AMBRE
                     else                        COUL_ROUGE

  ecart <- res$ECART_H
  libelle_ecart <- if (is.na(ecart)) "—"
    else if (ecart >= 0) paste0("+", format(round(ecart)), " h")
    else paste0(format(round(ecart)), " h")

  div(
    class = "kpi-grid",
    kpi_tile(if (is.na(res$TENSION)) "—" else format_pct(res$TENSION),
             "Écart à la productivité habituelle", couleur_tension, "gauge-high",
             sous_titre = if (is.na(res$TENSION)) "référence inconnue"
                          else if (res$TENSION > 0) "il faudra faire mieux que d'habitude"
                          else "le rythme habituel suffit"),
    kpi_tile(format(round(res$H_TOTAL)), "Heures planifiées", COUL_TRAVAIL, "clock",
             sous_titre = paste0("sur ", res$jours, " jour",
                                 if (res$jours > 1) "s" else "", " à venir")),
    kpi_tile(if (is.na(res$CA_H_REQUIS)) "—" else format_CA(res$CA_H_REQUIS, -1),
             "CA par heure à tenir", COUL_BRUN, "calculator",
             sous_titre = if (is.na(reference$valeur)) "pas de référence"
                          else paste0("habituel : ", format_CA(reference$valeur, -1), " / h")),
    kpi_tile(libelle_ecart, "Écart en heures",
             if (is.na(ecart)) COUL_NEUTRE
             else if (ecart > 0) COUL_AMBRE else COUL_VERT, "scale-balanced",
             sous_titre = if (is.na(ecart)) "—"
                          else if (ecart > 0) "posées au-delà de ce que l'objectif justifie"
                          else "marge par rapport à l'objectif"),
    kpi_tile(format_CA(res$OBJECTIF, -1), "Objectif de la période", COUL_MATIERE, "bullseye",
             sous_titre = if (is.na(res$CA_ATTENDU)) "—"
                          else paste0("attendu à ce rythme : ", format_CA(res$CA_ATTENDU, -1)))
  )
}

# Heures par semaine, empilées par secteur, avec la productivité mesurée en
# regard. La bande grisée marque les semaines encore à venir : leurs heures
# sont connues, leur CA non — on ne peut pas y lire de productivité.
graph_planning_semaine <- function(sem, sect, reference) {
  if (is.null(sem) || nrow(sem) == 0)
    return(plotly_empty() %>% layout(title = "Aucun planning"))

  p <- plot_ly()
  secteurs <- intersect(c(names(COULEURS_SECTEURS), "Secteur inconnu"),
                        unique(sect$SECTEUR))
  for (s in secteurs) {
    sub <- sect %>% filter(SECTEUR == s)
    if (nrow(sub) == 0) next
    p <- p %>% add_bars(
      data = sub, x = ~PERIODE, y = ~HEURES, name = s,
      marker = list(color = unname(couleur_secteur(s))),
      hovertemplate = paste0("Sem. ", format(sub$PERIODE, "%d/%m"), "<br>", s,
                             " ", round(sub$HEURES), " h<extra></extra>"))
  }

  mesure <- sem %>% filter(!is.na(CA_PAR_HEURE))
  if (nrow(mesure) > 0)
    p <- p %>% add_lines(
      data = mesure, x = ~PERIODE, y = ~CA_PAR_HEURE, yaxis = "y2",
      name = "CA / heure mesuré", line = list(color = COUL_BRUN, width = 2.5),
      hovertemplate = ~paste0("Sem. ", format(PERIODE, "%d/%m"), "<br>",
                              format_CA(CA_PAR_HEURE, -1), " / h<extra></extra>"))

  formes <- list()
  # Bande sur les semaines pas encore échues.
  avenir <- sem %>% filter(!COMPLETE)
  if (nrow(avenir) > 0)
    formes <- c(formes, list(list(
      type = "rect", xref = "x", yref = "paper",
      x0 = min(avenir$PERIODE) - 3.5, x1 = max(avenir$PERIODE) + 3.5,
      y0 = 0, y1 = 1, layer = "below",
      fillcolor = "rgba(141,123,104,0.10)", line = list(width = 0))))
  if (!is.na(reference$valeur))
    formes <- c(formes, list(list(
      type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y2",
      y0 = reference$valeur, y1 = reference$valeur,
      line = list(color = COUL_BRUN, width = 1, dash = "dot"))))

  p %>% layout(
    barmode = "stack",
    xaxis = list(title = ""),
    yaxis = list(title = "Heures planifiées"),
    yaxis2 = list(title = "CA par heure (€/h)", overlaying = "y", side = "right",
                  showgrid = FALSE, rangemode = "tozero"),
    shapes = formes,
    legend = list(orientation = "h"),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Les jours à venir, un par barre : la hauteur donne les heures posées, la
# couleur dit si l'objectif du jour les justifie au rythme habituel. Le trait
# noir marque les heures que l'objectif justifie — l'écart entre la barre et le
# trait, c'est la décision à prendre.
graph_planning_avenir <- function(proj, reference) {
  if (is.null(proj) || nrow(proj) == 0)
    return(plotly_empty() %>% layout(title = "Aucun jour à venir"))

  d <- proj %>%
    mutate(
      JOUR = paste0(substr(as.character(wday(DATE, label = TRUE, abbr = FALSE,
                                             week_start = 1)), 1, 3),
                    " ", format(DATE, "%d/%m")),
      COULEUR = case_when(
        is.na(TENSION)  ~ COUL_NEUTRE,
        TENSION <= 0    ~ COUL_VERT,
        TENSION <= 15   ~ COUL_AMBRE,
        TRUE            ~ COUL_ROUGE)
    )
  ordre <- factor(d$JOUR, levels = d$JOUR)

  p <- plot_ly() %>%
    add_bars(x = ordre, y = d$H_TOTAL, name = "Heures planifiées",
             marker = list(color = d$COULEUR),
             hovertemplate = paste0(
               d$JOUR, "<br>", round(d$H_TOTAL), " h planifiées",
               "<br>Objectif ", format_CA(d$OBJECTIF, -1),
               "<br>Soit ", format_CA(d$CA_H_REQUIS, -1), " / h à tenir",
               "<extra></extra>"))

  if (any(!is.na(d$H_SOUTENABLES)))
    p <- p %>% add_markers(
      x = ordre, y = d$H_SOUTENABLES, name = "Heures justifiées par l'objectif",
      # Le noir Mazette (MZ_NOIR de ui.R) n'est pas exposé côté serveur : on le
      # pose en clair plutôt que d'ajouter une constante pour un seul usage.
      marker = list(symbol = "line-ew-open", size = 22,
                    line = list(width = 2.5, color = "#260b01")),
      hovertemplate = paste0(d$JOUR, "<br>",
                             round(d$H_SOUTENABLES), " h au rythme habituel",
                             "<extra></extra>"))

  p %>% layout(
    xaxis = list(title = "", tickangle = -35),
    yaxis = list(title = "Heures", rangemode = "tozero"),
    legend = list(orientation = "h"),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Détail jour par jour, pour vérifier une ligne du tableau de service.
table_planning_avenir <- function(proj) {
  if (is.null(proj) || nrow(proj) == 0)
    return(tibble(Info = "Aucun jour à venir dans le planning."))
  proj %>%
    transmute(
      Jour = format(DATE, "%a %d/%m/%Y"),
      `Heures planifiées` = round(H_TOTAL, 1),
      `dont service`      = round(H_SERVICE, 1),
      Objectif            = format_CA(OBJECTIF, -1),
      `CA / h à tenir`    = format_CA(CA_H_REQUIS, -1),
      `Heures justifiées` = ifelse(is.na(H_SOUTENABLES), "—",
                                   as.character(round(H_SOUTENABLES))),
      `Écart`             = ifelse(is.na(ECART_H), "—",
                                   paste0(ifelse(ECART_H >= 0, "+", ""),
                                          round(ECART_H), " h")))
}

# Carte d'accueil : la tension de la semaine à venir, en un coup d'œil.
acc_planning <- function(res, reference) {
  if (is.null(res))
    return(corps_vide("Pas de planning à venir."))
  if (is.na(res$TENSION))
    return(corps_accueil(paste0(round(res$H_TOTAL), " h"),
                         paste0("planifiées sur ", res$jours, " jours"),
                         COUL_NEUTRE,
                         "Pas encore de référence de productivité"))

  couleur <- if (res$TENSION <= 0) COUL_VERT
             else if (res$TENSION <= 15) COUL_AMBRE else COUL_ROUGE

  corps_accueil(
    paste0(if (res$TENSION > 0) "+" else "", round(res$TENSION), " %"),
    paste0("de productivité à trouver sur ", res$jours, " jours à venir"),
    couleur,
    paste0(round(res$H_TOTAL), " h planifiées · ",
           format_CA(res$CA_H_REQUIS, -1), " / h à tenir contre ",
           format_CA(reference$valeur, -1), " d'habitude"))
}
