#### Volet "Comparaison" ####
#
# Comparer des périodes deux à deux, ou dix à dix. Deux sources se croisent :
#
#   db_kpi    quotidien, donc disponible à la semaine comme à l'année, et
#             porteur des répartitions midi/soir, boisson/nourriture,
#             semaine/week-end ;
#   db_compta MENSUEL. Les colonnes de gestion restent donc vides sur une
#             comparaison hebdomadaire — aucune ventilation n'est inventée.

#' Une ligne par période, avec tout ce qu'on sait d'elle.
comparaison_periodes <- function(db_kpi, db_obj, db_compta = NULL,
                                 unite = c("semaine", "mois", "annee"),
                                 periodes = NULL) {
  unite <- match.arg(unite)

  ventes <- db_kpi %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(
      CA         = sum(ventes, na.rm = TRUE),
      JOURS      = sum(ventes > 0, na.rm = TRUE),
      MIDI       = sum(Jour, na.rm = TRUE),
      SOIR       = sum(Soir, na.rm = TRUE),
      BOISSON    = sum(Boisson, na.rm = TRUE),
      NOURRITURE = sum(Nourriture, na.rm = TRUE),
      SEM        = sum(Semaine, na.rm = TRUE),
      WEEKEND    = sum(`Week-end`, na.rm = TRUE),
      .groups = "drop") %>%
    filter(CA > 0) %>%
    mutate(CA_JOUR  = ifelse(JOURS > 0, CA / JOURS, NA_real_),
           PCT_MIDI = ratio_pct(MIDI, MIDI + SOIR),
           PCT_BOIS = ratio_pct(BOISSON, BOISSON + NOURRITURE),
           PCT_WE   = ratio_pct(WEEKEND, SEM + WEEKEND))

  obj <- db_obj %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(OBJECTIF = sum(ventes, na.rm = TRUE), .groups = "drop")

  res <- ventes %>%
    left_join(obj, by = "PERIODE") %>%
    mutate(OBJECTIF = replace_na(OBJECTIF, 0),
           PCT_OBJ  = ratio_pct(CA, OBJECTIF))

  # Postes de gestion : seulement là où la comptabilité existe, et jamais à la
  # semaine puisqu'elle est mensuelle.
  if (!is.null(db_compta) && nrow(db_compta) && unite != "semaine") {
    g <- agrege_exploitation(postes_exploitation(db_compta),
                             if (unite == "annee") "annee" else "mois") %>%
      select(PERIODE, MATIERES, REMUNERATION, GENERAUX, AMORTISSEMENT, MARGE,
             PCT_MATIERES, PCT_TRAVAIL, PCT_PRIME, PCT_GENERAUX, PCT_MARGE)
    res <- left_join(res, g, by = "PERIODE")
  }
  for (col in c("MATIERES", "REMUNERATION", "GENERAUX", "AMORTISSEMENT",
                "MARGE", "PCT_MATIERES", "PCT_TRAVAIL", "PCT_PRIME",
                "PCT_GENERAUX", "PCT_MARGE"))
    if (!col %in% names(res)) res[[col]] <- NA_real_

  if (!is.null(periodes))
    res <- filter(res, PERIODE %in% as.Date(periodes))

  arrange(res, PERIODE)
}

#' Barres de CA colorées par l'atteinte de l'objectif.
#'
#' L'objectif n'a plus sa propre barre : deux séries côte à côte doublaient la
#' largeur pour une information que le survol donne mieux. La couleur porte le
#' jugement, le survol porte le détail.
graph_comparaison <- function(comp, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(comp) || nrow(comp) == 0)
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Sélectionnez des périodes à comparer")))

  comp <- arrange(comp, PERIODE)
  lab  <- label_periode(comp$PERIODE, unite)
  ordre <- factor(lab, levels = lab)

  # Le survol rassemble tout ce qu'on sait de la période : c'est là que
  # l'information dense a sa place, pas dans des barres supplémentaires.
  ligne <- function(cle, val) ifelse(is.na(val) | val == "", "",
                                     paste0("<br>", cle, " : ", val))
  hover <- paste0(
    "<b>", lab, "</b>",
    "<br>CA : ", format_CA(comp$CA, -1),
    ligne("Objectif", ifelse(comp$OBJECTIF > 0,
                             paste0(format_CA(comp$OBJECTIF, -1), " — ",
                                    label_objectif(comp$CA, comp$OBJECTIF)), NA)),
    "<br>", comp$JOURS, " jours d'ouverture — ", format_CA(comp$CA_JOUR, -1), "/jour",
    ligne("Midi / soir", ifelse(is.na(comp$PCT_MIDI), NA,
            paste0(comp$PCT_MIDI, " % / ", 100 - comp$PCT_MIDI, " %"))),
    ligne("Boisson / nourriture", ifelse(is.na(comp$PCT_BOIS), NA,
            paste0(comp$PCT_BOIS, " % / ", 100 - comp$PCT_BOIS, " %"))),
    ligne("Week-end", ifelse(is.na(comp$PCT_WE), NA, paste0(comp$PCT_WE, " % du CA"))),
    ligne("Marge d'exploitation", ifelse(is.na(comp$MARGE), NA,
            paste0(format_CA(comp$MARGE, -1), " — ", format_pct(comp$PCT_MARGE)))),
    ligne("Prime cost", ifelse(is.na(comp$PCT_PRIME), NA, format_pct(comp$PCT_PRIME))),
    "<extra></extra>")

  g <- plot_ly() %>%
    add_bars(x = ordre, y = comp$CA, name = "CA",
             marker = list(color = couleur_objectif(comp$CA, comp$OBJECTIF)),
             text = format_CA(comp$CA, -1), textposition = "outside",
             cliponaxis = FALSE, hovertemplate = hover)

  # La marge n'apparaît que si la comptabilité couvre les périodes comparées.
  if (any(!is.na(comp$MARGE))) {
    g <- g %>%
      add_trace(x = ordre, y = comp$MARGE, name = "Marge d'exploitation",
                type = "scatter", mode = "lines+markers", yaxis = "y2",
                line = list(color = COUL_BRUN, width = 2, dash = "dot"),
                marker = list(size = 9,
                              color = ifelse(comp$MARGE >= 0, COUL_VERT, COUL_ROUGE)),
                hovertemplate = paste0("Marge : ", format_CA(comp$MARGE, -1),
                                       "<extra></extra>")) %>%
      layout(yaxis2 = list(overlaying = "y", side = "right",
                           title = "Marge (€)", zeroline = TRUE,
                           zerolinecolor = "#8d7b68", showgrid = FALSE))
  }

  g %>% layout(
    barmode = "group",
    xaxis = list(title = "", tickangle = if (nrow(comp) > 6) -25 else 0),
    yaxis = list(title = "CA (€)"),
    legend = list(orientation = "h", y = -0.2),
    margin = list(t = 30, b = 70),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Tableau comparatif : tout ce que la période porte, en colonnes.
#'
#' Les colonnes de gestion disparaissent si la comptabilité ne couvre aucune des
#' périodes — inutile d'afficher huit colonnes vides sur une vue hebdomadaire.
table_comparaison_aff <- function(comp, unite = c("semaine", "mois", "annee"),
                                  unite_tva = NULL) {
  unite <- match.arg(unite)
  if (is.null(comp) || nrow(comp) == 0) return(tibble(Période = character()))

  res <- comp %>%
    arrange(desc(PERIODE)) %>%
    transmute(
      Période    = label_periode(PERIODE, unite),
      Jours      = JOURS,
      !!paste("CA", unite_tva) := format_CA(CA, -1),
      `CA/jour`  = format_CA(CA_JOUR, -1),
      Objectif   = format_CA(OBJECTIF, -1),
      `% obj.`   = format_pct(PCT_OBJ),
      Midi       = format_pct(PCT_MIDI),
      Boisson    = format_pct(PCT_BOIS),
      `Week-end` = format_pct(PCT_WE),
      Matières     = format_CA(MATIERES, -1),
      `Rémunér.`   = format_CA(REMUNERATION, -1),
      `Frais gén.` = format_CA(GENERAUX, -1),
      Marge        = format_CA(MARGE, -1),
      `Marge %`    = format_pct(PCT_MARGE),
      `Food %`     = format_pct(PCT_MATIERES),
      `Work %`     = format_pct(PCT_TRAVAIL),
      `Prime %`    = format_pct(PCT_PRIME))

  if (all(is.na(comp$MARGE)))
    res <- select(res, -Matières, -`Rémunér.`, -`Frais gén.`, -Marge,
                  -`Marge %`, -`Food %`, -`Work %`, -`Prime %`)
  res
}

#' Écarts entre deux périodes, quand il n'y en a que deux de sélectionnées.
#'
#' C'est le cas d'usage le plus fréquent — « ce mois-ci contre le précédent » —
#' et une ligne d'écarts se lit plus vite que deux lignes à soustraire de tête.
tuiles_ecart_comparaison <- function(comp, unite = "mois") {
  if (is.null(comp) || nrow(comp) != 2) return(NULL)
  comp <- arrange(comp, PERIODE)
  a <- comp[1, ]; b <- comp[2, ]

  tuile <- function(libelle, va, vb, fmt = function(x) format_CA(x, -1),
                    sens_positif = TRUE, icone = "arrow-right-arrow-left") {
    if (is.na(va) || is.na(vb)) return(NULL)
    d <- vb - va
    coul <- if (d == 0) COUL_NEUTRE
    else if ((d > 0) == sens_positif) COUL_VERT else COUL_ROUGE
    kpi_tile(paste0(if (d >= 0) "+" else "", fmt(d)), libelle, coul, icone,
             sous_titre = paste(fmt(va), "→", fmt(vb)))
  }

  tuiles <- Filter(Negate(is.null), list(
    tuile("CA", a$CA, b$CA, icone = "euro-sign"),
    tuile("CA par jour", a$CA_JOUR, b$CA_JOUR, icone = "calendar-day"),
    tuile("Atteinte de l'objectif", a$PCT_OBJ, b$PCT_OBJ,
          fmt = function(x) format_pct(x), icone = "bullseye"),
    tuile("Marge d'exploitation", a$MARGE, b$MARGE, icone = "piggy-bank"),
    # Un prime cost qui baisse est une bonne nouvelle : le sens s'inverse.
    tuile("Prime cost", a$PCT_PRIME, b$PCT_PRIME,
          fmt = function(x) format_pct(x), sens_positif = FALSE,
          icone = "scale-balanced")))

  if (!length(tuiles)) NULL else div(class = "kpi-grid", tuiles)
}
