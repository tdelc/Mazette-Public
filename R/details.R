#### REFONTE — Volet "Détail" ####

# Graphe des CA journaliers (barres cliquables) sur une période
graph_ca_jour <- function(db_kpi, db_obj, d1, d2, source = "detail_jour") {
  dat <- db_kpi %>%
    select(DATE, ventes) %>%
    left_join(db_obj %>% select(DATE, objectif = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2, ventes > 0) %>%
    arrange(DATE)
  
  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)
  
  plot_ly(dat, source = source) %>%
    add_bars(x = ~DATE, y = ~ventes, name = "CA",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(format(DATE, "%a %d/%m"), "<br>CA ",
                                     format_CA(ventes, -1), "<br>", atteinte,
                                     "<extra></extra>")) %>%
    add_lines(x = ~DATE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1)) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Graphe du CA agrégé par semaine ou par mois (barres cliquables).
# Même logique que graph_ca_jour, mais à la maille supérieure : sert au
# drill-down "Par semaine" / "Par mois" de l'onglet Détail.
graph_ca_periode <- function(db_kpi, db_obj, d1, d2,
                             unite = c("semaine", "mois"),
                             source = "detail_semaine") {
  unite <- match.arg(unite)
  
  dat <- db_kpi %>%
    select(DATE, ventes) %>%
    left_join(db_obj %>% select(DATE, objectif = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2) %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(ventes = sum(ventes, na.rm = TRUE),
              objectif = sum(objectif, na.rm = TRUE), .groups = "drop") %>%
    filter(ventes > 0) %>%
    arrange(PERIODE)
  
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée sur la période"))
  
  lbl <- label_periode(dat$PERIODE, unite)
  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)
  
  plot_ly(dat, source = source) %>%
    add_bars(x = ~PERIODE, y = ~ventes, name = "CA",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(lbl, "<br>CA ", format_CA(ventes, -1),
                                     "<br>", atteinte, "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1)) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Répartition du CA à l'intérieur d'une période (jours d'une semaine, ou
# semaines d'un mois) -> contexte du drill-down.
graph_repartition_periode <- function(db_kpi, db_obj, periode,
                                      unite = c("semaine", "mois")) {
  unite <- match.arg(unite)
  d1 <- as.Date(periode)
  d2 <- fin_periode(d1, unite)
  
  dat <- db_kpi %>%
    select(DATE, ventes) %>%
    left_join(db_obj %>% select(DATE, objectif = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2) %>%
    arrange(DATE)
  
  if (nrow(dat) == 0 || sum(dat$ventes, na.rm = TRUE) == 0)
    return(plotly_empty() %>% layout(title = "Aucune vente sur la période"))
  
  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)
  
  plot_ly(dat) %>%
    add_bars(x = ~DATE, y = ~ventes, name = "CA",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(format(DATE, "%a %d/%m"), "<br>CA ",
                                     format_CA(ventes, -1), "<br>", atteinte,
                                     "<extra></extra>")) %>%
    add_lines(x = ~DATE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1)) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Liste des produits (CA, quantité) sur une période -> table sélectionnable
liste_produits_periode <- function(db_produits, d1, d2, unite_tva = "HTVA") {
  
  col <- paste0("CA_",unite_tva)
  
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(Produit = PRODUIT) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(!!sym(col), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(CA))
}

# Évolution hebdomadaire d'un produit
evolution_un_produit <- function(db_produits, produit, d1, d2, unite_tva = "HTVA") {
  
  col <- paste0("CA_",unite_tva)
  
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) |> 
    mutate(SEMAINE = floor_date(DATE, unit = "week", week_start = 1)) %>%
    group_by(SEMAINE) %>%
    mutate(CA_TOT = sum(!!sym(col), na.rm = TRUE)) |> 
    group_by(SEMAINE,CATEGORIE) %>%
    mutate(CA_CATEGORIE = sum(!!sym(col), na.rm = TRUE)) |> 
    filter(PRODUIT == produit) %>%
    group_by(SEMAINE,CATEGORIE) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(!!sym(col), na.rm = TRUE), 
              PC_ALL = CA / mean(CA_TOT, na.rm = TRUE), 
              PC_CATEGORIE = CA / mean(CA_CATEGORIE, na.rm = TRUE), 
              .groups = "drop") %>%
    arrange(SEMAINE)
}

# Graphe d'évolution d'un produit (CA en barres + quantité en ligne)
graph_evolution_produit <- function(evo, produit) {
  plot_ly(evo, source = "detail_produit") %>%
    add_bars(x = ~SEMAINE, y = ~CA, name = "CA (€)",
             marker = list(color = "#732c02"),
             hovertemplate = ~paste0("Semaine du ", format(SEMAINE, "%d/%m"),
                                     "<br>CA ", format_CA(CA, -1), "<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~Quantite, name = "Quantité", yaxis = "y2",
              line = list(color = "#d98236", width = 2),
              hovertemplate = ~paste0(Quantite, " vendus<extra></extra>")) %>%
    layout(yaxis = list(title = "CA (€)"),
           yaxis2 = list(title = "Quantité", overlaying = "y", side = "right",
                         showgrid = FALSE),
           xaxis = list(title = ""), legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}
