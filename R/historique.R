#### REFONTE — Volet "Historique" ####

# Agrège le CA réalisé + objectif par semaine ou par mois.
# (db = sortie de prepa_db : colonnes DATE, ventes, PREMIER_JOUR_SEMAINE, PREMIER_JOUR_MOIS)
agrege_historique <- function(db_kpi, db_obj, unite = c("semaine", "mois"),
                              exclure_courant = TRUE) {
  unite <- match.arg(unite)
  col <- if (unite == "semaine") "PREMIER_JOUR_SEMAINE" else "PREMIER_JOUR_MOIS"
  
  reel <- db_kpi %>%
    group_by(PERIODE = .data[[col]]) %>%
    summarise(ventes = sum(ventes, na.rm = TRUE), .groups = "drop")
  obj <- db_obj %>%
    group_by(PERIODE = .data[[col]]) %>%
    summarise(objectif = sum(ventes, na.rm = TRUE), .groups = "drop")
  
  res <- left_join(reel, obj, by = "PERIODE") %>% arrange(PERIODE)
  
  if (exclure_courant) {
    courant <- if (unite == "semaine") floor_date(today(), "week", week_start = 1)
    else floor_date(today(), "month")
    res <- res %>% filter(PERIODE < courant)
  }
  res
}

# Graphe historique : barres (CA réalisé, coloré selon l'atteinte de
# l'objectif) + ligne objectif
graph_historique <- function(db_kpi, db_obj, unite = c("semaine", "mois"), n = 12) {
  unite <- match.arg(unite)
  dat <- agrege_historique(db_kpi, db_obj, unite) %>%
    filter(ventes > 0)
  # %>%  slice_tail(n = n)
  
  lbl <- if (unite == "semaine") paste0("Sem. du ", format(dat$PERIODE, "%d/%m/%Y"))
  else format(dat$PERIODE, "%B %Y")
  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)
  
  plot_ly(dat) %>%
    add_bars(x = ~PERIODE, y = ~ventes, name = "CA réalisé",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(lbl, "<br>CA ", format_CA(ventes, -1),
                                     "<br>", atteinte, "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1.5),
              hovertemplate = ~paste0("Objectif ", format_CA(objectif, -1), "<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

graph_historique_tendance <- function(db_kpi, db_obj, unite = c("semaine", "mois"), n = 12) {
  
  unite <- match.arg(unite)
  dat <- agrege_historique(db_kpi, db_obj, unite) %>%
    filter(ventes > 0)
  
  dat <- dat |> arrange(PERIODE)
  dat$id = 1:nrow(dat)
  
  p <- ggplot(dat, aes(x = id, y = ventes)) + 
    geom_smooth(method = "loess", xseq = sort(unique(dat$id)))
  
  # 2. Extraire les données construites
  build_data <- ggplot_build(p)
  loess_data <- build_data$data[[1]]
  
  dat$ma <- forecast::ma(dat$ventes,5)
  dat$loess <- loess_data$y
  dat$min   <- loess_data$ymin
  dat$max   <- loess_data$ymax
  
  lbl <- if (unite == "semaine") paste0("Sem. du ", format(dat$PERIODE, "%d/%m/%Y"))
  else format(dat$PERIODE, "%B %Y")
  # (pas de couleur par objectif ici : ce graphe trace des lignes, pas des barres)
  
  plot_ly(dat) %>%
    add_lines(x = ~PERIODE, y = ~ventes, name = "CA réalisé",
              line = list(color = "#d98236"),
              hovertemplate = ~paste0(lbl, "<br>CA ", format_CA(ventes, -1), "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~loess, name = "Tendance",
              line = list(color = "#5B7BAA")) %>%
    add_ribbons(
      x = ~ PERIODE,
      ymin = ~min,
      ymax = ~max,
      name = "Intervalle",
      fillcolor = "#5B7BAA50", # Couleur transparente
      line = list(color = "transparent")   # Pas de bordure sur le ruban
    ) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}