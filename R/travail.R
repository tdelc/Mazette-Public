#### Volet "Travail" ####
# Suivi des heures et de la productivité.
#
# Vocabulaire, volontairement ramené à deux catégories :
#
#   Créneau        : une demi-journée d'ouverture — Midi (<17h) ou Soir
#                    (>=17h). Les mardis soir avec vente de pizza forment un
#                    créneau à part : la Pizzwanze.
#   Coût VARIABLE  : le service. Il suit l'ouverture — un créneau de plus, des
#                    heures de plus.
#   Coût FIXE      : tout le reste (transformation alimentaire, brasserie,
#                    support). Il ne suit pas l'ouverture d'un créneau, d'où
#                    « fixe » au sens du pilotage, pas au sens comptable.
#
# Le détail par secteur reste accessible là où on l'a (tableau de
# décomposition) ; partout ailleurs on s'en tient à variable / fixe, qui est
# ce qui se pilote.
#
# ---------------------------------------------------------------------------
# Ce que ce volet ne fait PLUS, et pourquoi
# ---------------------------------------------------------------------------
# La « marge après travail » (CA − coûts) a été retirée. Elle reposait sur la
# répartition des coûts indirects entre créneaux au prorata du CA : une
# convention défendable pour une étude ponctuelle, trompeuse dans un tableau de
# bord — elle faisait apparaître une marge par créneau qui n'existe pas, et
# dont la valeur dépendait entièrement de la clé de répartition choisie.
#
# Les coûts affichés sont ceux d'HOREKO, la seule source qui se ventile par
# secteur. Le total comptable des rémunérations reste affiché à côté, comme
# point de contrôle : l'écart entre les deux est une information, pas une
# erreur à corriger par une règle de trois (cf. import.R).

CRENEAUX_ORDRE <- c("Midi", "Soir", "Pizzwanze")
PAL_CRENEAU <- c("Midi" = "#e67e22", "Soir" = "#9b59b6", "Pizzwanze" = "#c0392b")

# Toutes les fonctions de ce volet attendent une table AU GRAIN HORAIRE
# (DATE x CD_HEURE x PRODUIT), c'est-à-dire TICKETS_HEURES — et non DB_PRODUITS,
# qui est agrégée à la journée et n'a donc pas de colonne CD_HEURE.

# Jours de Pizzwanze : mardi soir où l'on a vendu des pizzas.
jours_pizzwanze <- function(db_ventes_heure) {
  db_ventes_heure %>%
    filter(str_detect(toupper(PRODUIT), "PIZZ"),
           CD_HEURE == "Soir (>=17h)",
           wday(DATE, week_start = 1) == 2,
           CA_HTVA > 0) %>%
    distinct(DATE) %>%
    pull(DATE)
}

# Renomme le créneau "Soir" en "Pizzwanze" sur les dates concernées.
marque_pizzwanze <- function(db, dates_piz) {
  db %>%
    mutate(CRENEAU = ifelse(CRENEAU == "Soir" & DATE %in% dates_piz,
                            "Pizzwanze", CRENEAU),
           CRENEAU = factor(CRENEAU, levels = CRENEAUX_ORDRE))
}

# Normalisation des créneaux, reprise de l'étude de rentabilité :
#   - les lundis (rares ouvertures exceptionnelles) sont exclus ;
#   - le mardi est toujours un créneau « Soir » (ouverture à 17h) ;
#   - le dimanche est toujours un créneau « Midi » (le CA résiduel de soirée y
#     est rattaché, le service ferme à 18h).
# Sans cela on obtient des créneaux fantômes : du CA sans aucune heure de
# service en face, donc une productivité infinie.
normalise_creneaux <- function(db) {
  db %>%
    mutate(.wd = wday(DATE, week_start = 1)) %>%
    filter(.wd != 1) %>%
    mutate(CRENEAU = case_when(.wd == 2 ~ "Soir",
                               .wd == 7 ~ "Midi",
                               TRUE     ~ as.character(CRENEAU))) %>%
    select(-.wd)
}

# CA HTVA par jour et par créneau (Midi / Soir / Pizzwanze).
ca_par_creneau <- function(db_ventes_heure, d1 = NULL, d2 = NULL) {
  piz <- jours_pizzwanze(db_ventes_heure)
  db <- db_ventes_heure
  if (!is.null(d1)) db <- filter(db, DATE >= as.Date(d1))
  if (!is.null(d2)) db <- filter(db, DATE <= as.Date(d2))
  
  db %>%
    mutate(CRENEAU = ifelse(CD_HEURE == "Midi (<17h)", "Midi", "Soir")) %>%
    normalise_creneaux() %>%
    marque_pizzwanze(piz) %>%
    group_by(DATE, CRENEAU) %>%
    summarise(CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    filter(CA > 0)
}

# Base de travail : une ligne par (DATE, CRENEAU), avec le CA du créneau et les
# heures de service qui lui sont directement imputables.
#
# Les heures HORS service ne sont PAS réparties entre les créneaux : elles ne
# leur appartiennent pas. Elles sont agrégées à part, par heures_fixes().
base_travail <- function(db_ventes_heure, db_travail, d1, d2) {
  d1 <- as.Date(d1); d2 <- as.Date(d2)
  piz <- jours_pizzwanze(db_ventes_heure)

  # Ne garder que les jours pour lesquels on connaît aussi les heures travaillées
  db_ventes_heure <- db_ventes_heure |>
    filter(DATE %in% db_travail$DATE)

  ca <- ca_par_creneau(db_ventes_heure, d1, d2)

  variable <- db_travail %>%
    filter(SECTEUR == "Service", CRENEAU %in% c("Midi", "Soir"),
           DATE >= d1, DATE <= d2) %>%
    normalise_creneaux() %>%
    marque_pizzwanze(piz) %>%
    group_by(DATE, CRENEAU) %>%
    summarise(H_VARIABLE    = sum(HEURES, na.rm = TRUE),
              COUT_VARIABLE = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")

  full_join(ca, variable, by = c("DATE", "CRENEAU")) %>%
    mutate(across(c(CA, H_VARIABLE, COUT_VARIABLE), ~replace_na(., 0)),
           JOUR_SEMAINE = vecteur_jours[wday(DATE, week_start = 1)],
           JOUR_SEMAINE = factor(JOUR_SEMAINE, levels = vecteur_jours),
           CRENEAU      = factor(CRENEAU, levels = CRENEAUX_ORDRE)) %>%
    arrange(DATE, CRENEAU)
}

# Heures et coûts HORS service, par jour. Gardés au grain secteur : c'est le
# seul endroit où le détail existe, et le tableau de décomposition s'en sert.
heures_fixes <- function(db_travail, d1, d2) {
  db_travail %>%
    filter(SECTEUR != "Service", DATE >= as.Date(d1), DATE <= as.Date(d2)) %>%
    group_by(DATE, SECTEUR) %>%
    summarise(H_FIXE    = sum(HEURES, na.rm = TRUE),
              COUT_FIXE = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")
}

# Agrégat par période. `fixe` vient de heures_fixes(), `compta` porte le total
# mensuel des rémunérations pour le point de contrôle.
#
# CA_PAR_HEURE se calcule sur les heures de SERVICE seules, comme avant : c'est
# la productivité de l'ouverture. Rapporté au total, il mélangerait le service
# et la structure, et bougerait à chaque brassin.
agrege_travail <- function(base, fixe = NULL, compta = NULL,
                           unite = c("mois", "trimestre", "annee")) {
  unite <- match.arg(unite)
  if (is.null(base) || !nrow(base)) return(agrege_travail_vide())

  v <- base %>%
    mutate(PERIODE = debut_periode_travail(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(CA            = sum(CA, na.rm = TRUE),
              H_VARIABLE    = sum(H_VARIABLE, na.rm = TRUE),
              COUT_VARIABLE = sum(COUT_VARIABLE, na.rm = TRUE),
              .groups = "drop")

  f <- if (is.null(fixe) || !nrow(fixe))
    tibble(PERIODE = as.Date(character()), H_FIXE = numeric(),
           COUT_FIXE = numeric())
  else fixe %>%
    mutate(PERIODE = debut_periode_travail(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(H_FIXE    = sum(H_FIXE, na.rm = TRUE),
              COUT_FIXE = sum(COUT_FIXE, na.rm = TRUE), .groups = "drop")

  # Le total comptable est MENSUEL : on le somme sur les mois de la période,
  # sans jamais le ventiler.
  c_a <- if (is.null(compta) || !nrow(compta))
    tibble(PERIODE = as.Date(character()), COUT_COMPTA = numeric())
  else compta %>%
    mutate(PERIODE = debut_periode_travail(MOIS_DEBUT, unite)) %>%
    group_by(PERIODE) %>%
    summarise(COUT_COMPTA = sum(COUT_COMPTA, na.rm = TRUE), .groups = "drop")

  v %>%
    left_join(f, by = "PERIODE") %>%
    left_join(c_a, by = "PERIODE") %>%
    mutate(across(c(H_FIXE, COUT_FIXE), ~replace_na(., 0)),
           H_TOTAL      = H_VARIABLE + H_FIXE,
           COUT_TOTAL   = COUT_VARIABLE + COUT_FIXE,
           CA_PAR_HEURE = if_else(H_VARIABLE > 0, CA / H_VARIABLE, NA_real_),
           PART_FIXE    = ratio_pct(H_FIXE, H_TOTAL),
           # Écart entre les deux sources, en % du total comptable. Positif :
           # la comptabilité porte plus que ce qu'Horeko a pointé.
           ECART_COMPTA = if_else(!is.na(COUT_COMPTA) & COUT_COMPTA > 0,
                                  ratio_pct(COUT_COMPTA - COUT_TOTAL, COUT_COMPTA),
                                  NA_real_)) %>%
    arrange(PERIODE)
}

agrege_travail_vide <- function() {
  tibble(PERIODE = as.Date(character()), CA = numeric(), H_VARIABLE = numeric(),
         COUT_VARIABLE = numeric(), H_FIXE = numeric(), COUT_FIXE = numeric(),
         COUT_COMPTA = numeric(), H_TOTAL = numeric(), COUT_TOTAL = numeric(),
         CA_PAR_HEURE = numeric(), PART_FIXE = numeric(),
         ECART_COMPTA = numeric())
}

# Début de période. Le volet Travail n'offre plus la semaine : la comptabilité
# à laquelle on se confronte est mensuelle, et une semaine ne s'y compare pas.
debut_periode_travail <- function(d, unite = c("mois", "trimestre", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
         mois      = floor_date(d, "month"),
         trimestre = floor_date(d, "quarter"),
         annee     = floor_date(d, "year"))
}

# Statistiques par jour de semaine x créneau : moyennes par OUVERTURE.
# C'est la table qui permet de comparer les créneaux à armes égales.
#
# Plus de marge ici : les coûts hors service ne sont pas répartis entre
# créneaux, donc il n'existe pas de marge par créneau. On s'en tient au coût
# variable, qui lui est bien imputable.
stats_creneaux <- function(base) {
  if (is.null(base) || !nrow(base)) return(NULL)
  base %>%
    filter(CA > 0 | H_VARIABLE > 0) %>%
    group_by(JOUR_SEMAINE, CRENEAU) %>%
    summarise(nb_jours      = n_distinct(DATE),
              CA_total      = sum(CA, na.rm = TRUE),
              H_variable    = sum(H_VARIABLE, na.rm = TRUE),
              COUT_VARIABLE = sum(COUT_VARIABLE, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(CA_moyen         = CA_total / nb_jours,
           H_variable_moyen = H_variable / nb_jours,
           COUT_moyen       = COUT_VARIABLE / nb_jours,
           CA_PAR_HEURE     = ifelse(H_variable > 0, CA_total / H_variable, NA_real_),
           RATIO_VARIABLE   = ratio_pct(COUT_VARIABLE, CA_total),
           CRENEAU_LABEL    = paste0(JOUR_SEMAINE, " — ", CRENEAU)) %>%
    arrange(desc(CA_PAR_HEURE))
}

##### Rendus #####

#' Productivité dans le temps, avec la décomposition des heures.
#'
#' Les barres empilent les heures VARIABLES (service) et FIXES (hors service) :
#' on voit d'un coup combien d'heures ont été posées et dans quelle proportion
#' elles suivent l'ouverture. La courbe donne le CA par heure de service, en
#' pointillé sa moyenne sur la fenêtre.
#'
#' Cliquer une barre sélectionne la période pour le tableau de décomposition.
graph_productivite_temps <- function(ag, unite = "mois",
                                     source = "trav_productivite_graph") {
  if (is.null(ag) || !nrow(ag))
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  lbl <- etiquette_periode(ag$PERIODE, unite)
  ordre <- factor(lbl, levels = lbl)
  
  h_service <- sum(ag$H_VARIABLE, na.rm = TRUE)
  moy <- if (h_service > 0) sum(ag$CA, na.rm = TRUE) / h_service else NA_real_

  p <- plot_ly(source = source) %>%
    add_bars(x = ordre, y = ag$H_VARIABLE, name = "Heures variables (service)",
             marker = list(color = COUL_TRAVAIL),
             hovertemplate = paste0(lbl, "<br>", round(ag$H_VARIABLE),
                                    " h de service<extra></extra>")) %>%
    add_bars(x = ordre, y = ag$H_FIXE, name = "Heures fixes (hors service)",
             marker = list(color = COUL_MATIERE),
             hovertemplate = paste0(lbl, "<br>", round(ag$H_FIXE),
                                    " h hors service<extra></extra>")) %>%
    add_lines(x = ordre, y = ag$CA_PAR_HEURE, name = "CA par heure de service",
              yaxis = "y2", line = list(color = COUL_BRUN, width = 2.5),
              hovertemplate = paste0(lbl, "<br>", format_CA(ag$CA_PAR_HEURE, -1),
                                     " / h<extra></extra>"))

  formes <- if (is.na(moy)) list() else list(list(
    type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y2",
    y0 = moy, y1 = moy,
    line = list(color = COUL_BRUN, width = 1, dash = "dot")))

  p %>% layout(
    barmode = "stack",
    xaxis = list(title = ""),
    yaxis = list(title = "Heures"),
    yaxis2 = list(title = "CA par heure (€/h)", overlaying = "y", side = "right",
                  showgrid = FALSE, rangemode = "tozero"),
    shapes = formes,
    legend = list(orientation = "h", y = -0.2),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Nuage CA / heures de service, un point par créneau type.
graph_nuage_creneaux <- function(stats) {
  if (is.null(stats) || !nrow(stats))
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  plot_ly(stats, x = ~H_variable_moyen, y = ~CA_moyen,
          type = "scatter", mode = "markers+text",
          text = ~CRENEAU_LABEL, textposition = "top center",
          textfont = list(size = 9),
          color = ~CRENEAU, colors = PAL_CRENEAU,
          marker = list(size = 12),
          hovertemplate = ~paste0(CRENEAU_LABEL, "<br>",
                                  round(H_variable_moyen, 1), " h par ouverture<br>",
                                  format_CA(CA_moyen, -1), " de CA<br>",
                                  format_CA(CA_PAR_HEURE, -1), " / h",
                                  "<extra></extra>")) %>%
    layout(xaxis = list(title = "Heures de service par ouverture"),
           yaxis = list(title = "CA moyen par ouverture (€)"),
           legend = list(orientation = "h", y = -0.2),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Classement des créneaux par productivité.
graph_productivite_creneaux <- function(stats) {
  if (is.null(stats) || !nrow(stats))
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  dat <- stats %>% filter(!is.na(CA_PAR_HEURE)) %>% arrange(CA_PAR_HEURE)
  ordre <- factor(dat$CRENEAU_LABEL, levels = dat$CRENEAU_LABEL)

  plot_ly() %>%
    add_bars(y = ordre, x = dat$CA_PAR_HEURE, orientation = "h",
             marker = list(color = unname(PAL_CRENEAU[as.character(dat$CRENEAU)])),
             hovertemplate = paste0(dat$CRENEAU_LABEL, "<br>",
                                    format_CA(dat$CA_PAR_HEURE, -1), " / h<br>",
                                    dat$nb_jours, " ouvertures<extra></extra>")) %>%
    layout(xaxis = list(title = "CA par heure de service (€/h)"),
           yaxis = list(title = ""),
           margin = list(l = 140), showlegend = FALSE,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Heatmap jour de semaine x créneau.
graph_heatmap_creneaux <- function(stats,
                                   var = c("CA_PAR_HEURE", "CA_moyen",
                                           "RATIO_VARIABLE")) {
  var <- match.arg(var)
  if (is.null(stats) || !nrow(stats))
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  titre <- c(CA_PAR_HEURE   = "CA par heure de service (€/h)",
             CA_moyen       = "CA moyen par ouverture (€)",
             RATIO_VARIABLE = "Coût variable / CA (%)")[[var]]
  # Pour le ratio de coût, une valeur basse est meilleure : on inverse l'échelle.
  echelle <- if (var == "RATIO_VARIABLE")
    list(c(0, COUL_VERT), c(1, COUL_ROUGE))
  else list(c(0, "#f2efe6"), c(1, COUL_TRAVAIL))

  dat <- stats %>%
    mutate(VAL = .data[[var]]) %>%
    select(JOUR_SEMAINE, CRENEAU, VAL) %>%
    complete(JOUR_SEMAINE, CRENEAU)

  mat <- dat %>%
    pivot_wider(names_from = CRENEAU, values_from = VAL) %>%
    arrange(JOUR_SEMAINE)
  cols <- intersect(CRENEAUX_ORDRE, names(mat))
  z <- as.matrix(mat[, cols, drop = FALSE])
  fmt <- if (var == "RATIO_VARIABLE")
    function(x) ifelse(is.na(x), "", paste0(round(x), " %"))
  else function(x) ifelse(is.na(x), "", format_CA(x, -1))

  plot_ly(x = cols, y = as.character(mat$JOUR_SEMAINE), z = z,
          type = "heatmap", colorscale = echelle, showscale = FALSE,
          hovertemplate = "%{y} — %{x}<br>%{z:,.0f}<extra></extra>") %>%
    add_annotations(
      x = rep(cols, each = nrow(z)), y = rep(as.character(mat$JOUR_SEMAINE),
                                             times = length(cols)),
      text = fmt(as.vector(z)), showarrow = FALSE,
      font = list(size = 11, color = "#260b01")) %>%
    layout(title = list(text = titre, font = list(size = 13)),
           xaxis = list(title = "", side = "top"), yaxis = list(title = ""),
           margin = list(l = 90, t = 60),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Tableau des créneaux types.
table_creneaux <- function(stats) {
  if (is.null(stats) || !nrow(stats)) return(tibble(Créneau = character()))
  stats %>%
    arrange(desc(CA_PAR_HEURE)) %>%
    transmute(Créneau           = CRENEAU_LABEL,
              Ouvertures        = nb_jours,
              `CA moyen`        = format_CA(CA_moyen, -1),
              `Heures serv.`    = round(H_variable_moyen, 1),
              `CA / heure`      = format_CA(CA_PAR_HEURE, -1),
              `Coût variable`   = format_CA(COUT_moyen, -1),
              `Coût var. / CA`  = ifelse(is.na(RATIO_VARIABLE), "—",
                                         paste0(RATIO_VARIABLE, " %")))
}

#' Décomposition des heures d'une période, par secteur et créneau.
#'
#' Le seul endroit où le détail par secteur existe : on le montre donc tel
#' quel, plutôt que de le résumer en variable / fixe.
#'
#' Les deux totaux de bas de tableau sont le point de contrôle entre les
#' sources : Horeko pointe les heures, la comptabilité enregistre la paie.
#' L'écart est normal — pécules, provisions, charges patronales, personnel non
#' pointé — mais il doit rester stable. C'est sa DÉRIVE qui est un signal.
table_decomposition_travail <- function(db_couts, d1, d2, ca_periode = NA_real_) {
  if (is.null(db_couts) || !nrow(db_couts))
    return(tibble(Info = "Aucune heure sur la période."))
  d <- db_couts %>% filter(DATE >= as.Date(d1), DATE <= as.Date(d2))
  if (!nrow(d)) return(tibble(Info = "Aucune heure sur la période."))

  detail <- d %>%
    group_by(Secteur = SECTEUR, Créneau = CRENEAU) %>%
    summarise(Heures = sum(HEURES, na.rm = TRUE),
              Cout   = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Cout))

  horeko <- sum(detail$Cout, na.rm = TRUE)
  compta <- if ("COUT_COMPTA" %in% names(d))
    sum(unique(d[, c("ANNEE", "MOIS", "COUT_COMPTA")])$COUT_COMPTA, na.rm = TRUE)
  else NA_real_

  bind_rows(
    detail %>% transmute(Secteur, Créneau,
                         Heures = round(Heures),
                         `Coût (Horeko)` = format_CA(Cout, -1)),
    tibble(Secteur = "Total Horeko", Créneau = "",
           Heures = round(sum(detail$Heures, na.rm = TRUE)),
           `Coût (Horeko)` = format_CA(horeko, -1)),
    tibble(Secteur = "Total comptabilité", Créneau = "", Heures = NA_real_,
           `Coût (Horeko)` = if (is.na(compta)) "—" else format_CA(compta, -1)),
    tibble(Secteur = "CA de la période", Créneau = "", Heures = NA_real_,
           `Coût (Horeko)` = if (is.na(ca_periode)) "—"
                             else format_CA(ca_periode, -1))
  )
}

#' Tuiles du volet Travail.
kpi_travail <- function(ag) {
  if (is.null(ag) || !nrow(ag))
    return(div(class = "text-muted small", "Aucune donnée sur la période."))

  ca   <- sum(ag$CA, na.rm = TRUE)
  hv   <- sum(ag$H_VARIABLE, na.rm = TRUE)
  hf   <- sum(ag$H_FIXE, na.rm = TRUE)
  cv   <- sum(ag$COUT_VARIABLE, na.rm = TRUE)
  cf   <- sum(ag$COUT_FIXE, na.rm = TRUE)
  cpt  <- if (all(is.na(ag$COUT_COMPTA))) NA_real_
          else sum(ag$COUT_COMPTA, na.rm = TRUE)
  cah  <- if (hv > 0) ca / hv else NA_real_
  ecart <- if (!is.na(cpt) && cpt > 0) ratio_pct(cpt - (cv + cf), cpt) else NA_real_

  div(
    class = "kpi-grid",
    kpi_tile(if (is.na(cah)) "—" else format_CA(cah, -1),
             "CA par heure de service",
             couleur_seuil_haut(cah, 90, 70), "gauge-high",
             sous_titre = paste0(format(round(hv)), " h variables")),
    kpi_tile(format(round(hv + hf)), "Heures totales", COUL_NEUTRE, "clock",
             sous_titre = paste0(round(ratio_pct(hv, hv + hf)), " % en service")),
    kpi_tile(format_CA(cv, -1), "Coût variable (service)", COUL_TRAVAIL,
             "person-running", sous_titre = format_pct(ratio_pct(cv, ca))),
    kpi_tile(format_CA(cf, -1), "Coût fixe (hors service)", COUL_MATIERE,
             "people-roof", sous_titre = format_pct(ratio_pct(cf, ca))),
    kpi_tile(format_pct(ratio_pct(cv + cf, ca)), "Coût du travail / CA",
             couleur_seuil(ratio_pct(cv + cf, ca), 35, 45), "scale-balanced",
             sous_titre = paste0("Horeko : ", format_CA(cv + cf, -1))),
    kpi_tile(if (is.na(ecart)) "—" else format_pct(ecart),
             "Écart Horeko / comptabilité",
             if (is.na(ecart)) COUL_NEUTRE else COUL_NEUTRE, "code-compare",
             sous_titre = if (is.na(cpt)) "pas de comptabilité"
                          else paste0("compta : ", format_CA(cpt, -1)))
  )
}
