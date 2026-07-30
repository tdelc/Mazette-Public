#### REFONTE — Volet "Travail" ####
# Suivi de la productivité et du coût du travail, inspiré de l'étude
# « Réduction des jours d'ouverture » (avril 2026).
#
# Vocabulaire (repris de l'étude) :
#   Créneau      : une demi-journée d'ouverture — Midi (<17h) ou Soir (>=17h).
#                  Les mardis soir avec vente de pizza forment un créneau à
#                  part : la Pizzwanze.
#   Heures de service : heures directement liées à l'ouverture d'un créneau.
#   Heures indirectes : transformation alimentaire, brasserie et support. Elles
#                  sont mutualisées sur la semaine puis réparties entre les
#                  créneaux AU PRORATA DU CA, pour que les créneaux qui
#                  rapportent le plus portent la plus grande part de la
#                  structure.
#   Marge après travail : CA HTVA − coût de service − coûts indirects. Ce qui
#                  reste pour couvrir les matières, le loyer et l'énergie.
#
# NB : on ne traite volontairement PAS la question « faut-il fermer un
# créneau ? » — l'étude a montré qu'aucun scénario de fermeture n'améliore la
# marge. L'objet ici est le pilotage du staffing.

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

# Base de travail : une ligne par (DATE, CRENEAU) avec le CA, les heures de
# service imputées directement, et les coûts indirects de la semaine répartis
# au prorata du CA du créneau.
base_travail <- function(db_ventes_heure, db_travail, d1, d2) {
  d1 <- as.Date(d1); d2 <- as.Date(d2)
  piz <- jours_pizzwanze(db_ventes_heure)
  
  # Ne garder que les jours pour lesquels on connaît aussi les heures travaillées
  db_ventes_heure <- db_ventes_heure |>
    filter(DATE %in% db_travail$DATE)
  
  ca <- ca_par_creneau(db_ventes_heure, d1, d2)
  
  service <- db_travail %>%
    filter(SECTEUR == "Service", CRENEAU %in% c("Midi", "Soir"),
           DATE >= d1, DATE <= d2) %>%
    normalise_creneaux() %>%
    marque_pizzwanze(piz) %>%
    group_by(DATE, CRENEAU) %>%
    summarise(H_SERVICE    = sum(HEURES, na.rm = TRUE),
              COUT_SERVICE = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")
  
  # Coûts indirects, mutualisés à la semaine
  indirect <- db_travail %>%
    filter(SECTEUR != "Service", DATE >= d1, DATE <= d2
           # ,wday(DATE, week_start = 1)!= 1
    ) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1),
           EST_TRANSFO = SECTEUR == "Transformation alimentaire") %>%
    group_by(SEMAINE) %>%
    summarise(H_TRANSFO    = sum(HEURES[EST_TRANSFO], na.rm = TRUE),
              COUT_TRANSFO = sum(COUT_TRAVAIL[EST_TRANSFO], na.rm = TRUE),
              H_AUTRE      = sum(HEURES[!EST_TRANSFO], na.rm = TRUE),
              COUT_AUTRE   = sum(COUT_TRAVAIL[!EST_TRANSFO], na.rm = TRUE),
              .groups = "drop")
  
  full_join(ca, service, by = c("DATE", "CRENEAU")) %>%
    mutate(across(c(CA, H_SERVICE, COUT_SERVICE), ~replace_na(., 0)),
           SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    left_join(indirect, by = "SEMAINE") %>%
    mutate(across(c(H_TRANSFO, COUT_TRANSFO, H_AUTRE, COUT_AUTRE),
                  ~replace_na(., 0))) %>%
    # Répartition des coûts indirects au prorata du CA de la semaine.
    # NB : `if_else` (et non `ifelse`) car la condition porte sur un total de
    # groupe — `ifelse` renverrait une valeur de longueur 1, recyclée sur toutes
    # les lignes, et donnerait la même part à tous les créneaux.
    group_by(SEMAINE) %>%
    mutate(CA_SEMAINE = sum(CA, na.rm = TRUE),
           PART = if_else(CA_SEMAINE > 0, CA / CA_SEMAINE, 0),
           across(c(H_TRANSFO, COUT_TRANSFO, H_AUTRE, COUT_AUTRE), ~ . * PART)) %>%
    ungroup() %>%
    select(-CA_SEMAINE) %>%
    mutate(COUT_INDIRECT = COUT_TRANSFO + COUT_AUTRE,
           COUT_TOTAL    = COUT_SERVICE + COUT_INDIRECT,
           MARGE         = CA - COUT_TOTAL,
           JOUR_SEMAINE  = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1),
           CRENEAU       = factor(CRENEAU, levels = CRENEAUX_ORDRE)) %>%
    arrange(DATE, CRENEAU)
}

# Agrégat par période (semaine / mois / année) à partir de la base.
agrege_travail <- function(base, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  base %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(CA            = sum(CA, na.rm = TRUE),
              H_SERVICE     = sum(H_SERVICE, na.rm = TRUE),
              H_INDIRECT    = sum(H_TRANSFO + H_AUTRE, na.rm = TRUE),
              COUT_SERVICE  = sum(COUT_SERVICE, na.rm = TRUE),
              COUT_TRANSFO  = sum(COUT_TRANSFO, na.rm = TRUE),
              COUT_AUTRE    = sum(COUT_AUTRE, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(COUT_INDIRECT = COUT_TRANSFO + COUT_AUTRE,
           COUT_TOTAL    = COUT_SERVICE + COUT_INDIRECT,
           H_TOTAL       = H_SERVICE + H_INDIRECT,
           MARGE         = CA - COUT_TOTAL,
           CA_PAR_HEURE  = ifelse(H_SERVICE > 0, CA / H_SERVICE, NA_real_),
           RATIO_SERVICE = ratio_pct(COUT_SERVICE, CA),
           RATIO_TOTAL   = ratio_pct(COUT_TOTAL, CA),
           MARGE_PCT     = ratio_pct(MARGE, CA)) %>%
    arrange(PERIODE)
}

# CA par période, ventilé par créneau (Midi / Soir / Pizzwanze).
agrege_creneaux_periode <- function(base, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  base %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE, CRENEAU) %>%
    summarise(CA = sum(CA, na.rm = TRUE),
              H_SERVICE = sum(H_SERVICE, na.rm = TRUE), .groups = "drop") %>%
    mutate(CA_PAR_HEURE = ifelse(H_SERVICE > 0, CA / H_SERVICE, NA_real_))
}

# Statistiques par jour de semaine x créneau : moyennes par ouverture.
# C'est la table qui permet de comparer les créneaux à armes égales.
stats_creneaux <- function(base) {
  base %>%
    filter(CA > 0 | H_SERVICE > 0) %>%
    group_by(JOUR_SEMAINE, CRENEAU) %>%
    summarise(nb_jours      = n_distinct(DATE),
              CA_total      = sum(CA, na.rm = TRUE),
              H_service     = sum(H_SERVICE, na.rm = TRUE),
              COUT_SERVICE  = sum(COUT_SERVICE, na.rm = TRUE),
              COUT_INDIRECT = sum(COUT_INDIRECT, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(COUT_TOTAL      = COUT_SERVICE + COUT_INDIRECT,
           MARGE           = CA_total - COUT_TOTAL,
           CA_moyen        = CA_total / nb_jours,
           H_service_moyen = H_service / nb_jours,
           MARGE_moyenne   = MARGE / nb_jours,
           CA_PAR_HEURE    = ifelse(H_service > 0, CA_total / H_service, NA_real_),
           RATIO_TOTAL     = ratio_pct(COUT_TOTAL, CA_total),
           CRENEAU_LABEL   = paste0(JOUR_SEMAINE, " — ", CRENEAU)) %>%
    arrange(desc(CA_PAR_HEURE))
}


# Décomposition du CA : marge + coût service + transfo + autre, par période.
graph_structure_travail <- function(ag, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(ag) || nrow(ag) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  lbl <- label_periode(ag$PERIODE, unite)
  
  plot_ly(ag) %>%
    add_bars(x = ~PERIODE, y = ~COUT_SERVICE, name = "Coût service",
             marker = list(color = COUL_TRAVAIL),
             hovertemplate = ~paste0(lbl, "<br>Service ", format_CA(COUT_SERVICE, -1),
                                     "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~COUT_TRANSFO, name = "Coût transfo",
             marker = list(color = "#a2703f"),
             hovertemplate = ~paste0(lbl, "<br>Transfo ", format_CA(COUT_TRANSFO, -1),
                                     "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~COUT_AUTRE, name = "Autres secteurs",
             marker = list(color = "#8d7b68"),
             hovertemplate = ~paste0(lbl, "<br>Autres ", format_CA(COUT_AUTRE, -1),
                                     "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~MARGE, name = "Marge après travail",
             marker = list(color = COUL_VERT),
             hovertemplate = ~paste0(lbl, "<br>Marge ", format_CA(MARGE, -1),
                                     " (", MARGE_PCT, " %)<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = ""),
           yaxis = list(title = "€"), legend = list(orientation = "h"),
           hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Productivité dans le temps : heures de service (barres) + CA/heure (ligne).
graph_productivite_temps <- function(ag, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(ag) || nrow(ag) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  lbl <- label_periode(ag$PERIODE, unite)
  moy <- sum(ag$CA, na.rm = TRUE) / sum(ag$H_SERVICE, na.rm = TRUE)
  
  plot_ly(ag) %>%
    add_bars(x = ~PERIODE, y = ~H_SERVICE, name = "Heures de service",
             marker = list(color = "#d3c0ac"),
             hovertemplate = ~paste0(lbl, "<br>", round(H_SERVICE),
                                     " h<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~CA_PAR_HEURE, name = "CA / heure", yaxis = "y2",
              line = list(color = COUL_TRAVAIL, width = 2.5),
              hovertemplate = ~paste0(lbl, "<br>", format_CA(CA_PAR_HEURE, -1),
                                      " / h<extra></extra>")) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Heures de service"),
      yaxis2 = list(title = "CA par heure (€/h)", overlaying = "y",
                    side = "right", showgrid = FALSE, rangemode = "tozero"),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         yref = "y2", y0 = moy, y1 = moy,
                         line = list(color = COUL_TRAVAIL, width = 1, dash = "dot"))),
      legend = list(orientation = "h"),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# CA par période ventilé Midi / Soir / Pizzwanze.
graph_ca_creneaux_temps <- function(cre, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(cre) || nrow(cre) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  p <- plot_ly()
  for (cr in CRENEAUX_ORDRE) {
    sub <- cre %>% filter(CRENEAU == cr)
    if (nrow(sub) == 0) next
    lbl <- label_periode(sub$PERIODE, unite)
    p <- p %>% add_bars(
      data = sub, x = ~PERIODE, y = ~CA, name = cr,
      marker = list(color = PAL_CRENEAU[[cr]]),
      hovertemplate = paste0(lbl, "<br>", cr, " ", format_CA(sub$CA, -1),
                             "<extra></extra>"))
  }
  p %>% layout(barmode = "stack", xaxis = list(title = ""),
               yaxis = list(title = "CA HTVA (€)"),
               legend = list(orientation = "h"), hovermode = "x unified",
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

##### Graphiques — analyse par créneau #####

# Nuage CA moyen vs heures de service moyennes, avec la droite de productivité
# moyenne. Plus un créneau est haut à gauche, plus il est efficace.
graph_nuage_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  dat <- stats %>% filter(H_service_moyen > 0, CA_moyen > 0)
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  # Droite passant par l'origine = productivité moyenne globale
  pente <- sum(dat$CA_total) / sum(dat$H_service)
  xr <- c(0, max(dat$H_service_moyen) * 1.08)
  
  plot_ly(dat) %>%
    add_lines(x = xr, y = pente * xr, name = "Productivité moyenne",
              line = list(color = "#260b01", dash = "dot", width = 1.5),
              hoverinfo = "skip") %>%
    add_markers(x = ~H_service_moyen, y = ~CA_moyen, color = ~CRENEAU,
                colors = PAL_CRENEAU, size = ~CA_total, sizes = c(80, 500),
                text = ~CRENEAU_LABEL,
                hovertemplate = ~paste0("<b>", CRENEAU_LABEL, "</b><br>",
                                        round(H_service_moyen, 1), " h de service<br>",
                                        format_CA(CA_moyen, -1), " de CA<br>",
                                        format_CA(CA_PAR_HEURE, -1), " / h",
                                        "<extra></extra>")) %>%
    layout(xaxis = list(title = "Heures de service moyennes par ouverture",
                        rangemode = "tozero"),
           yaxis = list(title = "CA HTVA moyen par ouverture (€)",
                        rangemode = "tozero"),
           # legend = list(orientation = "h"),
           legend = list(yref = "container", y = 0, yanchor = "bottom"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Classement des créneaux par productivité horaire (barres horizontales).
graph_productivite_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  dat <- stats %>% filter(!is.na(CA_PAR_HEURE)) %>% arrange(CA_PAR_HEURE)
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  dat <- dat %>% mutate(CRENEAU_LABEL = factor(CRENEAU_LABEL, levels = CRENEAU_LABEL))
  moy <- sum(dat$CA_total) / sum(dat$H_service)
  
  plot_ly(dat) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~CA_PAR_HEURE, orientation = "h",
             marker = list(color = unname(PAL_CRENEAU[as.character(dat$CRENEAU)])),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>",
                                     format_CA(CA_PAR_HEURE, -1), " / h<br>",
                                     round(H_service_moyen, 1), " h par ouverture",
                                     "<extra></extra>")) %>%
    layout(xaxis = list(title = "CA HTVA par heure de service (€/h)"),
           yaxis = list(title = ""),
           shapes = list(list(type = "line", yref = "paper", y0 = 0, y1 = 1,
                              x0 = moy, x1 = moy,
                              line = list(color = "#260b01", width = 1.5,
                                          dash = "dot"))),
           showlegend = FALSE, margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Heatmap jour x créneau sur l'indicateur choisi.
graph_heatmap_creneaux <- function(stats,
                                   var = c("CA_moyen", "CA_PAR_HEURE",
                                           "RATIO_TOTAL", "MARGE_moyenne")) {
  var <- match.arg(var)
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  titre <- c(CA_moyen      = "CA moyen par ouverture (€)",
             CA_PAR_HEURE  = "CA par heure de service (€/h)",
             RATIO_TOTAL   = "Coût du travail / CA (%)",
             MARGE_moyenne = "Marge moyenne par ouverture (€)")[[var]]
  # Pour le ratio de coût, une valeur basse est meilleure : on inverse l'échelle
  echelle <- if (var == "RATIO_TOTAL")
    list(c(0, COUL_VERT), c(1, COUL_ROUGE))
  else list(c(0, "#f2efe6"), c(1, COUL_TRAVAIL))
  
  dat <- stats %>%
    mutate(VAL = .data[[var]]) %>%
    select(JOUR_SEMAINE, CRENEAU, VAL) %>%
    complete(JOUR_SEMAINE, CRENEAU)
  
  jours <- levels(droplevels(dat$JOUR_SEMAINE))
  mat <- dat %>%
    pivot_wider(names_from = CRENEAU, values_from = VAL) %>%
    arrange(JOUR_SEMAINE)
  
  cols <- intersect(CRENEAUX_ORDRE, names(mat))
  z <- as.matrix(mat[, cols, drop = FALSE])
  fmt <- if (var == "RATIO_TOTAL") function(x) ifelse(is.na(x), "", paste0(round(x), " %"))
  else function(x) ifelse(is.na(x), "", format_CA(x, -1))
  
  plot_ly(x = cols, y = as.character(mat$JOUR_SEMAINE), z = z,
          type = "heatmap", colorscale = echelle,
          hovertemplate = "%{y} — %{x}<br>%{z:.0f}<extra></extra>",
          showscale = TRUE) %>%
    add_annotations(
      x = rep(cols, each = nrow(z)),
      y = rep(as.character(mat$JOUR_SEMAINE), times = length(cols)),
      text = fmt(as.vector(z)), showarrow = FALSE,
      font = list(size = 12, color = "#260b01")) %>%
    layout(title = list(text = titre, font = list(size = 13)),
           xaxis = list(title = "", side = "top"),
           yaxis = list(title = "", autorange = "reversed"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Décomposition du CA moyen par créneau : marge + coût service + indirect.
graph_decomposition_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  dat <- stats %>%
    arrange(MARGE_moyenne) %>%
    mutate(CRENEAU_LABEL = factor(CRENEAU_LABEL, levels = CRENEAU_LABEL),
           C_SERVICE  = COUT_SERVICE / nb_jours,
           C_INDIRECT = COUT_INDIRECT / nb_jours)
  
  plot_ly(dat) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~MARGE_moyenne, orientation = "h",
             name = "Marge après travail", marker = list(color = COUL_VERT),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>Marge ",
                                     format_CA(MARGE_moyenne, -1), "<extra></extra>")) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~C_SERVICE, orientation = "h",
             name = "Coût service", marker = list(color = COUL_TRAVAIL),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>Service ",
                                     format_CA(C_SERVICE, -1), "<extra></extra>")) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~C_INDIRECT, orientation = "h",
             name = "Coûts indirects", marker = list(color = "#8d7b68"),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>Indirects ",
                                     format_CA(C_INDIRECT, -1), "<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = "€ par ouverture"),
           yaxis = list(title = ""), 
           legend = list(yref = "container", y = 0, yanchor = "bottom"),
           # legend = list(orientation = "h"),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Tableau récapitulatif par créneau.
table_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(tibble(Créneau = character()))
  stats %>%
    arrange(desc(CA_PAR_HEURE)) %>%
    transmute(Créneau        = CRENEAU_LABEL,
              Ouvertures     = nb_jours,
              `CA moyen`     = format_CA(CA_moyen, -1),
              `Heures serv.` = round(H_service_moyen, 1),
              `CA / heure`   = format_CA(CA_PAR_HEURE, -1),
              `Coût travail` = format_CA(COUT_TOTAL / nb_jours, -1),
              `Marge moy.`   = format_CA(MARGE_moyenne, -1),
              `Coût / CA`    = ifelse(is.na(RATIO_TOTAL), "—",
                                      paste0(RATIO_TOTAL, " %")))
}
