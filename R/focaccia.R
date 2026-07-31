# Décompose un PRODUIT_FULL de focaccia en base + options.
# Les libellés viennent de la caisse :
#   "Focaccia du moment + Options focaccias: + SUPPL. Fromage + SUPPL. Viande
#    + Option pikant: !! SPICY HOT !!"
parse_focaccia <- function(pf) {
  tibble(
    BASE = case_when(
      str_detect(pf, regex("brunch", ignore_case = TRUE))         ~ "Brunch",
      str_detect(pf, regex("patates douces", ignore_case = TRUE)) ~ "Patates douces",
      str_detect(pf, regex("du moment", ignore_case = TRUE))      ~ "Du moment",
      TRUE                                                        ~ "Autre"),
    FROMAGE = str_detect(pf, fixed("SUPPL. Fromage")),
    VIANDE  = str_detect(pf, fixed("SUPPL. Viande")),
    SPICY   = str_detect(pf, fixed("SPICY HOT"))
  ) %>%
    mutate(GARNITURE = case_when(FROMAGE & VIANDE ~ "Fromage + Viande",
                                 FROMAGE          ~ "Fromage",
                                 VIANDE           ~ "Viande",
                                 TRUE             ~ "Nature"),
           VARIANTE = paste0(GARNITURE, ifelse(SPICY, " + Spicy", "")))
}

ORDRE_GARNITURES <- c("Nature", "Fromage", "Viande", "Fromage + Viande")

# Lignes de focaccia sur une fenêtre, décomposées en options.
# On écarte les remises et lignes négatives, qui ne sont pas des ventes.
conso_focaccias <- function(db_produits, d1, d2, unite_tva) {
  
  col <- paste0("CA_",unite_tva)
  
  db <- db_produits %>%
    filter(str_detect(tolower(PRODUIT_FULL), "focaccia"),
           !str_detect(tolower(PRODUIT_FULL), "discount|% sur produit"),
           QUANTITE > 0, DATE >= as.Date(d1), DATE <= as.Date(d2))
  if (nrow(db) == 0)
    return(tibble(DATE = as.Date(character()), BASE = character(),
                  FROMAGE = logical(), VIANDE = logical(), SPICY = logical(),
                  GARNITURE = character(), VARIANTE = character(),
                  QUANTITE = numeric(), CA = numeric()))
  bind_cols(db %>% select(DATE, QUANTITE, CA = !!sym(col)),
            parse_focaccia(db$PRODUIT_FULL)) %>%
    mutate(GARNITURE = factor(GARNITURE, levels = ORDRE_GARNITURES))
}

# Synthèse d'une semaine, avec la semaine précédente pour comparaison.
focaccias_semaine <- function(db_produits, semaine, unite_tva = "HTVA") {
  semaine <- as.Date(semaine)
  list(semaine = semaine,
       act  = conso_focaccias(db_produits, semaine, semaine + 6, unite_tva),
       prec = conso_focaccias(db_produits, semaine - 7, semaine - 1, unite_tva))
}

# Nombre de focaccias par jour de la semaine choisie.
focaccias_par_jour <- function(fo, semaine) {
  semaine <- as.Date(semaine)
  jours <- tibble(DATE = seq(semaine, semaine + 6, by = "day")) %>%
    mutate(JOUR = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1))
  fo %>%
    group_by(DATE) %>%
    summarise(QUANTITE = sum(QUANTITE), CA = sum(CA), .groups = "drop") %>%
    right_join(jours, by = "DATE") %>%
    mutate(across(c(QUANTITE, CA), ~replace_na(., 0))) %>%
    arrange(DATE)
}

# Répartition par garniture x spicy.
focaccias_variantes <- function(fo) {
  if (nrow(fo) == 0)
    return(tibble(GARNITURE = factor(character(), levels = ORDRE_GARNITURES),
                  SPICY = logical(), QUANTITE = numeric()))
  fo %>%
    group_by(GARNITURE, SPICY) %>%
    summarise(QUANTITE = sum(QUANTITE), .groups = "drop")
}

# Historique hebdomadaire : volumes et taux d'options.
evo_focaccias <- function(db_produits, n_semaines = 26, fin = NULL, unite_tva = "HTVA") {
  fin <- if (is.null(fin)) max(db_produits$DATE, na.rm = TRUE) else as.Date(fin)
  debut <- floor_date(fin, "week", week_start = 1) - weeks(n_semaines - 1)
  # Les quantités par option sont calculées AVANT le regroupement : dans un
  # summarise(), `QUANTITE = sum(QUANTITE)` écrase la colonne, et un
  # `QUANTITE[FROMAGE]` écrit ensuite indexerait le total (scalaire) au lieu
  # des lignes — ce qui ne produit que des NA.
  conso_focaccias(db_produits, debut, fin, unite_tva) %>%
    mutate(SEMAINE   = floor_date(DATE, "week", week_start = 1),
           Q_FROMAGE = QUANTITE * FROMAGE,
           Q_VIANDE  = QUANTITE * VIANDE,
           Q_SPICY   = QUANTITE * SPICY) %>%
    group_by(SEMAINE) %>%
    summarise(QUANTITE  = sum(QUANTITE, na.rm = TRUE),
              CA        = sum(CA, na.rm = TRUE),
              Q_FROMAGE = sum(Q_FROMAGE, na.rm = TRUE),
              Q_VIANDE  = sum(Q_VIANDE, na.rm = TRUE),
              Q_SPICY   = sum(Q_SPICY, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(PCT_FROMAGE = ratio_pct(Q_FROMAGE, QUANTITE),
           PCT_VIANDE  = ratio_pct(Q_VIANDE,  QUANTITE),
           PCT_SPICY   = ratio_pct(Q_SPICY,   QUANTITE)) %>%
    arrange(SEMAINE)
}

kpi_focaccias_tiles <- function(fs, unite_tva = NULL) {
  
  act <- fs$act; prec <- fs$prec
  q  <- sum(act$QUANTITE);  q_m1  <- sum(prec$QUANTITE)
  ca <- sum(act$CA);        ca_m1 <- sum(prec$CA)
  pct <- function(d, col) if (sum(d$QUANTITE) > 0)
    100 * sum(d$QUANTITE[d[[col]]]) / sum(d$QUANTITE) else NA_real_
  jours_ouverts <- n_distinct(act$DATE)
  
  div(
    class = "kpi-grid",
    tuile_evolution(q, q_m1, "Focaccias vendues", "bread-slice"),
    tuile_evolution(ca, ca_m1, paste("CA",unite_tva,"focaccias"), "euro-sign",
                    function(x) format_CA(x, -1)),
    kpi_tile(if (jours_ouverts > 0) format(round(q / jours_ouverts, 1)) else "—",
             "Par jour d'ouverture", CONSO_BRUN, "gauge-high",
             sous_titre = paste0(jours_ouverts, " jours servis")),
    tuile_evolution(pct(act, "FROMAGE"), pct(prec, "FROMAGE"),
                    "Avec fromage", "cheese", function(x) format_pct(x)),
    tuile_evolution(pct(act, "VIANDE"), pct(prec, "VIANDE"),
                    "Avec viande", "drumstick-bite", function(x) format_pct(x)),
    tuile_evolution(pct(act, "SPICY"), pct(prec, "SPICY"),
                    "Spicy hot", "pepper-hot", function(x) format_pct(x))
  )
}

# Rythme sur la semaine : quantités par jour (+ rappel de S-1 en pointillé).
graph_focaccias_jour <- function(jour_act, jour_prec) {
  if (is.null(jour_act) || nrow(jour_act) == 0)
    return(plotly_empty() %>% layout(title = "Aucune focaccia vendue"))
  
  # `add_lines` retrie les points par x : avec des jours en texte, la trace S-1
  # ressortait dans l'ordre alphabétique (dimanche, jeudi, lundi...). On passe
  # donc un FACTEUR ORDONNÉ, dont le tri suit les niveaux lundi -> dimanche.
  jours <- factor(as.character(jour_act$JOUR), levels = as.character(jour_act$JOUR))
  
  p <- plot_ly() %>%
    add_bars(x = jours, y = jour_act$QUANTITE, name = "Semaine",
             marker = list(color = CONSO_BRUN),
             hovertemplate = paste0(jours, "<br>", jour_act$QUANTITE,
                                    " focaccias<extra></extra>"))
  if (!is.null(jour_prec) && nrow(jour_prec) == nrow(jour_act))
    p <- p %>% add_lines(x = jours, y = jour_prec$QUANTITE, name = "S-1",
                         line = list(color = "#8d7b68", dash = "dot", width = 2),
                         hovertemplate = paste0("S-1 : ", jour_prec$QUANTITE,
                                                "<extra></extra>"))
  p %>% layout(xaxis = list(title = "", categoryorder = "array",
                            categoryarray = levels(jours)),
               yaxis = list(title = "Focaccias"),
               legend = list(orientation = "h"), bargap = 0.35,
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Popularité des variantes : garniture en barres, part spicy empilée.
graph_variantes_focaccias <- function(var) {
  if (is.null(var) || nrow(var) == 0)
    return(plotly_empty() %>% layout(title = "Aucune focaccia vendue"))
  
  dat <- var %>%
    mutate(GARNITURE = factor(as.character(GARNITURE),
                              levels = ORDRE_GARNITURES))
  doux  <- dat %>% filter(!SPICY)
  epice <- dat %>% filter(SPICY)
  
  plot_ly() %>%
    add_bars(x = as.character(doux$GARNITURE), y = doux$QUANTITE,
             name = "Standard", marker = list(color = CONSO_AMBRE),
             hovertemplate = paste0(doux$GARNITURE, "<br>", doux$QUANTITE,
                                    "<extra></extra>")) %>%
    add_bars(x = as.character(epice$GARNITURE), y = epice$QUANTITE,
             name = "Spicy hot", marker = list(color = "#c0392b"),
             hovertemplate = paste0(epice$GARNITURE, " (spicy)<br>",
                                    epice$QUANTITE, "<extra></extra>")) %>%
    layout(barmode = "stack",
           xaxis = list(title = "", categoryorder = "array",
                        categoryarray = ORDRE_GARNITURES),
           yaxis = list(title = "Focaccias"),
           legend = list(orientation = "h"), bargap = 0.35,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Historique : volumes hebdo en barres.
graph_evo_focaccias <- function(evo, semaine = NULL) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  couleurs <- if (is.null(semaine)) CONSO_BRUN
  else ifelse(evo$SEMAINE == as.Date(semaine), CONSO_AMBRE, CONSO_BRUN)
  
  plot_ly(evo) %>%
    add_bars(x = ~SEMAINE, y = ~QUANTITE, name = "Focaccias",
             marker = list(color = couleurs),
             hovertemplate = ~paste0("Sem. ", format(SEMAINE, "%d/%m/%y"), "<br>",
                                     QUANTITE, " focaccias — ",
                                     format_CA(CA, -1), "<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "Focaccias"),
           showlegend = FALSE,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Taux d'options dans le temps (fromage / viande / spicy).
graph_options_focaccias <- function(evo) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  plot_ly(evo) %>%
    add_lines(x = ~SEMAINE, y = ~PCT_FROMAGE, name = "Fromage",
              line = list(color = "#d4ac0d", width = 2),
              hovertemplate = ~paste0("Fromage %{y:.0f} %<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~PCT_VIANDE, name = "Viande",
              line = list(color = "#8d5524", width = 2),
              hovertemplate = ~paste0("Viande %{y:.0f} %<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~PCT_SPICY, name = "Spicy hot",
              line = list(color = "#c0392b", width = 2),
              hovertemplate = ~paste0("Spicy %{y:.0f} %<extra></extra>")) %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = "% des focaccias", ticksuffix = " %",
                        rangemode = "tozero"),
           legend = list(orientation = "h"), hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Détail par variante, avec comparaison S-1.
table_focaccias <- function(fs, unite_tva = NULL) {
  
  col_name <- paste("CA",unite_tva)
  agg <- function(d) d %>% group_by(VARIANTE) %>%
    summarise(Q = sum(QUANTITE), CA = sum(CA), .groups = "drop")
  act <- agg(fs$act)
  prec <- agg(fs$prec) %>% rename(Q_M1 = Q, CA_M1 = CA)
  if (nrow(act) == 0 && nrow(prec) == 0) return(tibble(Variante = character()))
  
  res <- full_join(act, prec, by = "VARIANTE") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    arrange(desc(Q))
  
  # Le total est calculé hors du mutate : `ifelse(sum(Q) > 0, ...)` renverrait
  # une valeur de longueur 1 (la condition est un scalaire), recyclée sur
  # toutes les lignes — toutes les parts afficheraient le même pourcentage.
  total <- sum(res$Q)
  
  res %>%
    mutate(PART = if (total > 0) 100 * Q / total else NA_real_,
           EVO = ifelse(Q_M1 > 0, round(100 * (Q - Q_M1) / Q_M1, 1), NA_real_)) %>%
    transmute(Variante   = VARIANTE,
              Quantité   = Q,
              # une décimale : à l'entier, huit petites parts arrondies
              # chacune vers le haut faisaient une somme à 102 %
              `Part`           = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              !!sym(col_name) := format_CA(CA, -1),
              `Qté S-1`        = Q_M1,
              `Évol.`          = ifelse(is.na(EVO), "—",
                                        paste0(ifelse(EVO >= 0, "+", ""), EVO, " %")))
}


##### Focaccias — aide à la production #####
# Combien produire de chaque préparation pour la semaine à venir ?
#


# Dernières semaines ENTIÈREMENT couvertes par les données. Une semaine
# tronquée par la fin du jeu de données tirerait la moyenne vers le bas.
semaines_completes <- function(dates, n = 3, fin = NULL) {
  dates <- as.Date(dates)
  fin <- if (is.null(fin)) max(dates, na.rm = TRUE) else as.Date(fin)
  sems <- sort(unique(floor_date(dates, "week", week_start = 1)))
  tail(sems[sems + 6 <= fin], n)
}

# Base de la carte production : nombre moyen de focaccias par semaine
# concernées par chaque ingrédient, et portion par défaut.
production_focaccias_base <- function(db_produits, n_semaines = 3, fin = NULL,
                                      marge = 1.1, unite_tva = "HTVA") {
  base <- INGREDIENTS_FOCACCIA %>% mutate(FOCACCIAS = NA_real_, SEMAINES = 0L)
  if (is.null(db_produits) || nrow(db_produits) == 0) return(base)
  
  fin_donnees <- if (is.null(fin)) max(db_produits$DATE, na.rm = TRUE)
  else as.Date(fin)
  if (!is.finite(fin_donnees)) return(base)
  
  fo_all <- conso_focaccias(db_produits, as.Date("1900-01-01"), fin_donnees, unite_tva)
  if (nrow(fo_all) == 0) return(base)
  
  sems <- semaines_completes(fo_all$DATE, n_semaines, fin)
  if (length(sems) == 0) return(base)
  
  fo_sub <- fo_all %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) |> 
    filter(SEMAINE %in% sems)
  
  # Max sur les trois semaines, par ingrédient
  max_toutes <- fo_sub |> group_by(SEMAINE) |> 
    summarise(n = sum(QUANTITE)) |> pull(n) |> max()
  max_fromage <- fo_sub |> filter(FROMAGE) |> group_by(SEMAINE) |> 
    summarise(n = sum(QUANTITE)) |> pull(n) |> max()
  max_viande <- fo_sub |> filter(VIANDE) |> group_by(SEMAINE) |> 
    summarise(n = sum(QUANTITE)) |> pull(n) |> max()
  
  base |> 
    mutate(FOCACCIAS = case_when(
      ASSIETTE == "toutes" ~ round(max_toutes * marge),
      ASSIETTE == "fromage" ~ round(max_fromage * marge),
      ASSIETTE == "viande" ~ round(max_viande * marge),
      TRUE ~ NA_real_),
      SEMAINES  = length(sems)
    )
}

# Quantité en grammes, basculée en kilos quand ça devient lourd à lire.
format_qte_g <- function(x) {
  if (length(x) == 0 || is.na(x)) return("—")
  if (abs(x) >= 1000) paste0(format(round(x / 1000, 2), nsmall = 2), " kg")
  else paste0(round(x), " g")
}