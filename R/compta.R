##### Agrégation compta #####

# Agrège CA / matières / personnel / marge par période, avec les ratios KPI.
#   db_compta  : DB_COMPTA
#   db_travail : DB_COUTS_TRAVAIL
#   db_matiere : DB_COUTS_MATIERE
agrege_compta <- function(db_compta, db_travail, db_matiere,
                          unite = c("semaine", "mois", "annee"),
                          d1 = NULL, d2 = NULL) {
  unite <- match.arg(unite)

  # Ne retenir que les lignes de coûts exploitables à cette granularité : une
  # vue hebdomadaire ignore l'historique mensuel plutôt que de le découper.
  db_matiere <- couts_matiere_effectifs(db_matiere, unite)
  
  db_compta <- db_compta |> 
    mutate(
      DATE_DEB = floor_date  (ymd(paste(ANNEE,MOIS,"01")),unit = "month"),
      DATE_FIN = ceiling_date(ymd(paste(ANNEE,MOIS,"01")),unit = "month")
      )

  if (!is.null(d1)) {
    d1 <- as.Date(d1)
    db_compta  <- filter(db_compta,  DATE_DEB >= d1)
    db_travail <- filter(db_travail, DATE     >= d1)
    db_matiere <- filter(db_matiere, PERIODE  >= d1)
  }
  if (!is.null(d2)) {
    d2 <- as.Date(d2)
    db_compta  <- filter(db_compta,  DATE_FIN <= d2 | DATE_FIN < d2)
    db_travail <- filter(db_travail, DATE     <= d2)
    db_matiere <- filter(db_matiere, PERIODE  <= d2)
  }

  # Se limiter aux jours couverts À LA FOIS par le travail et par les matières.
  # La couverture matière se juge sur la période native de chaque ligne, pas sur
  # la semaine : un mois d'historique couvre tous ses jours.
  jours_matiere <- unique(unlist(Map(
    function(p, g) seq(p, fin_granularite(p, g), by = "day"),
    db_matiere$PERIODE, db_matiere$GRANULARITE)))
  db_compta <- db_compta |>
    filter(DATE_DEB %in% db_travail$DATE,
           DATE_DEB %in% as.Date(jours_matiere, origin = "1970-01-01"))
  
  produit <- db_compta %>%
    mutate(PERIODE = debut_periode(DATE_DEB, unite)) %>%
    select(CATEGORIE,PERIODE,TYPE,RUBRIQUE,VALEUR, COMPTE) |> 
    filter(CATEGORIE == "VENTE") |> 
    group_by(PERIODE, RUBRIQUE) |> 
    summarise(VALEUR = sum(VALEUR, na.rm = TRUE)) |> 
    filter(VALEUR != 0) |> ungroup()
  
  ca <- produit |> 
    filter(RUBRIQUE == "Chiffre d'affaires") |> 
    rename(CA = VALEUR) |> select(-RUBRIQUE)
  
  autres_produits <- produit |> 
    filter(RUBRIQUE == "Autres produits d'exploitation") |> 
    rename(AUTRES_PRODUITS = VALEUR) |> select(-RUBRIQUE)
  
  trav <- db_travail %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE),
              HEURES  = sum(HEURES, na.rm = TRUE), .groups = "drop")
  
  # On sépare les matières « métier » (Service / Transfo / Brasserie) des frais
  # généraux (Support) : le Prime Cost au sens de la restauration = matières +
  # personnel, les frais généraux étant suivis à part.
  mat <- db_matiere %>%
    mutate(PERIODE = debut_periode(PERIODE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(FOOD    = sum(COUT_MATIERE[SECTEUR != "Support"], na.rm = TRUE),
              GENERAL = sum(COUT_MATIERE[SECTEUR == "Support"], na.rm = TRUE),
              .groups = "drop")
  
  ca %>%
    full_join(autres_produits, by = "PERIODE") %>%
    full_join(trav, by = "PERIODE") %>%
    full_join(mat,  by = "PERIODE") %>%
    filter(!is.na(CA)) |> 
    arrange(PERIODE) %>%
    mutate(across(c(CA, AUTRES_PRODUITS, TRAVAIL, HEURES, FOOD, GENERAL), ~replace_na(., 0))) %>%
    mutate(MATIERE = FOOD + GENERAL,
           PRIME   = FOOD + TRAVAIL,
           CHARGES = PRIME + GENERAL,
           CA_ADD  = CA + AUTRES_PRODUITS,
           MARGE   = CA_ADD - CHARGES,
           FOOD_PCT    = ratio_pct(FOOD,    CA),
           WORK_PCT    = ratio_pct(TRAVAIL, CA),
           GENERAL_PCT = ratio_pct(GENERAL, CA),
           PRIME_PCT   = ratio_pct(PRIME,   CA),
           MARGE_PCT   = ratio_pct(MARGE,   CA_ADD)) %>%
    filter(CA > 0 | CHARGES > 0)
}

# Détail par secteur sur une fenêtre de dates (une ligne par secteur + Total).
compta_secteurs <- function(db_travail, db_matiere, d1, d2) {
  d1 <- as.Date(d1); d2 <- as.Date(d2)
  
  trav <- db_travail %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(SECTEUR) %>%
    summarise(HEURES  = sum(HEURES, na.rm = TRUE),
              TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")
  
  # Une période native chevauchant les bornes est retenue si elle commence
  # dedans — même règle qu'avant, appliquée au mois comme à la semaine.
  mat <- db_matiere %>%
    filter(PERIODE >= d1, PERIODE <= d2) %>%
    group_by(SECTEUR) %>%
    summarise(ACHATS  = sum(ACHATS, na.rm = TRUE),
              STOCK   = sum(VARIATION_STOCK, na.rm = TRUE),
              MATIERE = sum(COUT_MATIERE, na.rm = TRUE),
              STOCK_CONNU = all(STOCK_CONNU), .groups = "drop")
  
  tibble(SECTEUR = SECTEURS_COMPTA) %>%
    left_join(trav, by = "SECTEUR") %>%
    left_join(mat,  by = "SECTEUR") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)),
           TOTAL = MATIERE + TRAVAIL)
}

# Synthèse d'UNE période : la ligne d'agrégat + le détail par secteur.
compta_apercu <- function(db_compta, db_travail, db_matiere, periode,
                          unite = c("semaine", "mois", "annee")) {
  unite   <- match.arg(unite)
  periode <- as.Date(periode)
  d1 <- periode
  d2 <- fin_periode(periode, unite)
  
  res <- agrege_compta(db_compta, db_travail, db_matiere, unite, d1 = d1, d2 = d2)
  if (nrow(res) == 0)
    res <- tibble(PERIODE = periode, CA = 0, TRAVAIL = 0, HEURES = 0,
                  FOOD = 0, GENERAL = 0, MATIERE = 0, PRIME = 0, CHARGES = 0,
                  MARGE = 0, FOOD_PCT = NA_real_, WORK_PCT = NA_real_,
                  GENERAL_PCT = NA_real_, PRIME_PCT = NA_real_,
                  MARGE_PCT = NA_real_)
  
  print(res)
  
  list(unite    = unite,
       periode  = periode,
       libelle  = label_periode(periode, unite),
       bornes   = c(d1, d2),
       total    = res[1, ],
       secteurs = compta_secteurs(db_travail, db_matiere, d1, d2))
}


##### Graphiques compta #####

# Évolution par période : coûts empilés + CA (ligne) + marge (losange).
# Cliquable : `source` permet de sélectionner une période au clic.
graph_evo_compta <- function(comptes, unite = c("semaine", "mois", "annee"),
                             source = "compta_evo", selection = NULL) {
  unite <- match.arg(unite)
  if (is.null(comptes) || nrow(comptes) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  lbl <- label_periode(comptes$PERIODE, unite)
  # La période sélectionnée est mise en évidence (les autres sont atténuées)
  op <- if (is.null(selection)) rep(1, nrow(comptes))
  else ifelse(comptes$PERIODE == as.Date(selection), 1, 0.45)
  
  plot_ly(comptes, source = source) %>%
    add_bars(x = ~PERIODE, y = ~FOOD, name = "Matières",
             marker = list(color = COUL_MATIERE, opacity = op),
             hovertemplate = ~paste0(lbl, "<br>Matières ",
                                     format_CA(FOOD, -1), "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~TRAVAIL, name = "Personnel",
             marker = list(color = COUL_TRAVAIL, opacity = op),
             hovertemplate = ~paste0(lbl, "<br>Personnel ",
                                     format_CA(TRAVAIL, -1), "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~GENERAL, name = "Frais généraux",
             marker = list(color = "#8d7b68", opacity = op),
             hovertemplate = ~paste0(lbl, "<br>Frais généraux ",
                                     format_CA(GENERAL, -1), "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~CA, name = "CA HTVA",
              line = list(color = "#2e7d32", width = 2.5),
              hovertemplate = ~paste0(lbl, "<br>CA ",
                                      format_CA(CA, -1), "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~CA_ADD, name = "CA + Autres Produits",
              line = list(color = "#2eaa00", width = 2.5),
              hovertemplate = ~paste0(lbl, "<br>CA + Autres Produits ",
                                      format_CA(CA_ADD, -1), "<extra></extra>")) %>%
    add_markers(x = ~PERIODE, y = ~MARGE, name = "Marge",
                marker = list(size = 9, symbol = "diamond",
                              color = ifelse(comptes$MARGE >= 0, COUL_VERT, COUL_ROUGE)),
                hovertemplate = ~paste0(lbl, "<br>Marge ", format_CA(MARGE, -1),
                                        " (", MARGE_PCT, " %)<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "€"),
           legend = list(orientation = "h"), hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Évolution des ratios (food / work / prime cost, en % du CA).
graph_evo_kpi_compta <- function(comptes, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(comptes) || nrow(comptes) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  lbl <- label_periode(comptes$PERIODE, unite)
  ligne <- function(p, col, nom, couleur) {
    p %>% add_lines(x = ~PERIODE, y = comptes[[col]], name = nom,
                    
                    line = list(color = couleur, width = 2),
                    hovertemplate = paste0(lbl, "<br>", nom, " %{y:.1f} %<extra></extra>"))
  }
  
  plot_ly(comptes) %>%
    ligne("FOOD_PCT",    "Food Cost",      COUL_MATIERE) %>%
    ligne("WORK_PCT",    "Work Cost",      COUL_TRAVAIL) %>%
    ligne("GENERAL_PCT", "Frais généraux", "#8d7b68") %>%
    ligne("PRIME_PCT",   "Prime Cost",     COUL_ROUGE) %>%
    ligne("MARGE_PCT",   "Marge",          COUL_VERT) %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = "% du CA", ticksuffix = " %", range = c(-100, 200)),
           legend = list(orientation = "h"), hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Barres horizontales empilées matière/personnel par secteur (+ ligne Prime Cost).
graph_secteurs_compta <- function(ap) {
  sect <- ap$secteurs %>% filter(TOTAL != 0)
  if (nrow(sect) == 0 | sum(sect$TRAVAIL) == 0)
    return(plotly_empty() %>% 
             layout(title = "Aucun coût sur la période",
                    paper_bgcolor = "rgba(0,0,0,0)", 
                    plot_bgcolor = "rgba(0,0,0,0)"))
  
  # Total en haut, puis les secteurs du plus coûteux au moins coûteux
  sect <- sect %>% arrange(TOTAL)
  tot <- tibble(SECTEUR = "Prime Cost",
                MATIERE = sum(sect$MATIERE), TRAVAIL = sum(sect$TRAVAIL),
                TOTAL = sum(sect$TOTAL))
  dat <- bind_rows(sect %>% select(SECTEUR, MATIERE, TRAVAIL, TOTAL), tot) %>%
    mutate(SECTEUR = factor(SECTEUR, levels = SECTEUR),
           PC_MAT = ifelse(TOTAL > 0, round(100 * MATIERE / TOTAL), NA),
           PC_TRA = ifelse(TOTAL > 0, round(100 * TRAVAIL / TOTAL), NA))
  
  etiquette <- function(pc) ifelse(is.na(pc) | abs(pc) < 8, "", paste0(pc, "%"))
  
  plot_ly(dat) %>%
    add_bars(y = ~SECTEUR, x = ~MATIERE, orientation = "h", name = "Matières",
             marker = list(color = COUL_MATIERE),
             text = etiquette(dat$PC_MAT), textposition = "inside",
             insidetextfont = list(color = "#260b01"),
             hovertemplate = ~paste0(SECTEUR, "<br>Matières ",
                                     format_CA(MATIERE, -1), "<extra></extra>")) %>%
    add_bars(y = ~SECTEUR, x = ~TRAVAIL, orientation = "h", name = "Personnel",
             marker = list(color = COUL_TRAVAIL),
             text = etiquette(dat$PC_TRA), textposition = "inside",
             insidetextfont = list(color = "#ffffff"),
             hovertemplate = ~paste0(SECTEUR, "<br>Personnel ",
                                     format_CA(TRAVAIL, -1), "<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = "€"), yaxis = list(title = ""),
           legend = list(orientation = "h"),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Tableau des coûts par secteur (+ ligne Total), style "Coûts par secteur".
table_secteurs_compta <- function(ap) {
  sect <- ap$secteurs
  ca <- ap$total$CA
  
  if (is.na(ca) || ca == 0) return(NULL)
  
  tot <- sect %>%
    summarise(SECTEUR = "Total", HEURES = sum(HEURES), ACHATS = sum(ACHATS),
              STOCK = sum(STOCK), MATIERE = sum(MATIERE),
              TRAVAIL = sum(TRAVAIL), TOTAL = sum(TOTAL))
  
  bind_rows(sect, tot) %>%
    transmute(Secteur    = SECTEUR,
              Heures     = round(HEURES),
              Achats     = format_CA(ACHATS, -1),
              Stock      = format_CA(STOCK, -1),
              Matières   = format_CA(MATIERE, -1),
              Personnel  = format_CA(TRAVAIL, -1),
              Total      = format_CA(TOTAL, -1),
              `% du CA`  = paste0(round(100 * TOTAL / ca, 1), " %"))
}
