
#### REFONTE — Volet "Pizzwanze" ####
# La Pizzwanze est une soirée pizza qui revient toutes les trois à quatre
# semaines, le mardi soir. Quelques pizzas sont là à chaque fois, le reste
# change au gré des produits de saison.
#
# Détection : il n'existe pas de catégorie dédiée, on part donc du NOM du
# produit — tout ce qui contient « pizza », plus le Khachapuri, une pizza
# géorgienne qui ne porte pas le mot.
#
# Une soirée se reconnaît à sa CARTE : on y propose plusieurs pizzas. Les
# autres jours où l'on voit passer de la pizza n'ont qu'une seule référence
# (part vendue le lendemain, slices du Bal National...). Le critère est donc
# « au moins deux références », et non « c'est un mardi » : les soirées
# exceptionnelles hors mardi sont ainsi capturées elles aussi. Sur
# l'historique disponible, la règle retient exactement les 32 mardis.

# Un produit est une pizza si son nom le dit, Khachapuri compris.
# `as.character` d'abord : la colonne peut arriver en facteur, et un NA nu est
# logique — `replace_na` refuserait alors la valeur de remplacement texte.
est_pizza <- function(x) {
  str_detect(tolower(replace_na(as.character(x), "")), "pizz|khachapuri")
}



# Dates des soirées Pizzwanze, de la plus ancienne à la plus récente.
soirees_pizzwanze <- function(db_produits,
                              min_refs = PIZZWANZE_MIN_REFS,
                              min_pizzas = PIZZWANZE_MIN_PIZZAS) {
  db_produits %>%
    filter(est_pizza(PRODUIT_FULL), QUANTITE > 0) %>%
    filter(!str_detect(PRODUIT_FULL,"Slice")) |> 
    group_by(DATE) %>%
    summarise(PIZZAS = sum(QUANTITE, na.rm = TRUE),
              N_REF  = n_distinct(PRODUIT_FULL), .groups = "drop") %>%
    filter(N_REF >= min_refs, PIZZAS >= min_pizzas) %>%
    arrange(DATE) %>%
    pull(DATE)
}

# Ventes de pizzas d'une ou plusieurs soirées, agrégées par date et produit.
conso_pizzas <- function(db_produits, dates, unite_tva) {
  col <- paste0("CA_",unite_tva)
  dates <- as.Date(dates)
  db_produits %>%
    filter(est_pizza(PRODUIT_FULL), QUANTITE > 0, DATE %in% dates) %>%
    group_by(DATE, PIZZA = PRODUIT_FULL) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA       = sum(!!sym(col), na.rm = TRUE), .groups = "drop")
}

# Statut de chaque pizza sur l'ensemble des soirées : incontournable, régulière
# ou occasionnelle, avec ses dates de première et dernière apparition.
statut_pizzas <- function(db_produits, soirees, unite_tva = "HTVA") {
  n_soirees <- length(soirees)
  if (n_soirees == 0)
    return(tibble(PIZZA = character(), N_SOIREES = integer(),
                  PREMIERE = as.Date(character()), DERNIERE = as.Date(character()),
                  QUANTITE = numeric(), STATUT = character()))
  
  conso_pizzas(db_produits, soirees, unite_tva) %>%
    group_by(PIZZA) %>%
    summarise(N_SOIREES = n_distinct(DATE),
              PREMIERE  = min(DATE),
              DERNIERE  = max(DATE),
              QUANTITE  = sum(QUANTITE, na.rm = TRUE), .groups = "drop") %>%
    mutate(PART_SOIREES = N_SOIREES / n_soirees,
           STATUT = case_when(PART_SOIREES >= 2/3 ~ "Incontournable",
                              PART_SOIREES >= 1/3 ~ "Régulière",
                              TRUE                ~ "Occasionnelle")) %>%
    arrange(desc(N_SOIREES), desc(QUANTITE))
}

STATUTS_PIZZA <- c("Incontournable", "Régulière", "Occasionnelle", "Nouveauté")
PAL_STATUT_PIZZA <- c("Incontournable" = "#732c02", "Régulière" = "#d98236",
                      "Occasionnelle"  = "#d3c0ac", "Nouveauté" = "#5B7B5A")

# Synthèse d'une soirée : ses ventes, celles de la soirée précédente, et le
# statut de chaque pizza. Une pizza dont la première apparition est ce soir-là
# est marquée « Nouveauté ».
pizzwanze_soiree <- function(db_produits, db_ticket, date_soiree, 
                             soirees = NULL, unite_tva = "HTVA") {
  date_soiree <- as.Date(date_soiree)
  if (is.null(soirees)) soirees <- soirees_pizzwanze(db_produits)
  
  precedente <- soirees[soirees < date_soiree]
  precedente <- if (length(precedente) == 0) NA else max(precedente)
  
  pic <- pizzas_par_heure(db_ticket, date_soiree) |> 
    arrange(-QUANTITE) |> filter(row_number() == 1) |> pull(HEURE)
  
  statuts <- statut_pizzas(db_produits, soirees, unite_tva)
  
  act <- conso_pizzas(db_produits, date_soiree, unite_tva) %>%
    left_join(statuts %>% select(PIZZA, N_SOIREES, PREMIERE, STATUT), by = "PIZZA") %>%
    mutate(NOUVEAUTE = !is.na(PREMIERE) & PREMIERE == date_soiree,
           STATUT_SOIR = ifelse(NOUVEAUTE, "Nouveauté", STATUT)) %>%
    arrange(desc(QUANTITE))
  
  prec <- if (is.na(precedente)) act[0, c("PIZZA", "QUANTITE", "CA")]
  else conso_pizzas(db_produits, precedente, unite_tva) %>% select(PIZZA, QUANTITE, CA)
  
  list(date = date_soiree,
       precedente = precedente,
       ecart_jours = if (is.na(precedente)) NA_real_
       else as.numeric(date_soiree - precedente),
       act = act, prec = prec, statuts = statuts, pic = pic)
}

# Historique : une ligne par soirée, avec le nombre de nouveautés et l'écart
# depuis la soirée précédente.
historique_pizzwanze <- function(db_produits, soirees = NULL, 
                                 unite_tva = "HTVA") {
  if (is.null(soirees)) soirees <- soirees_pizzwanze(db_produits)
  if (length(soirees) == 0)
    return(tibble(DATE = as.Date(character()), PIZZAS = numeric(),
                  CA = numeric(), N_REF = integer(), NOUVEAUTES = integer(),
                  ECART = numeric()))
  
  detail <- conso_pizzas(db_produits, soirees, unite_tva)
  premieres <- detail %>% group_by(PIZZA) %>%
    summarise(PREMIERE = min(DATE), .groups = "drop")
  
  detail %>%
    left_join(premieres, by = "PIZZA") %>%
    group_by(DATE) %>%
    summarise(PIZZAS     = sum(QUANTITE, na.rm = TRUE),
              CA         = sum(CA, na.rm = TRUE),
              N_REF      = n_distinct(PIZZA),
              NOUVEAUTES = sum(PREMIERE == DATE), .groups = "drop") %>%
    arrange(DATE) %>%
    mutate(ECART = as.numeric(DATE - lag(DATE)))
}

# Ventes de pizzas heure par heure sur une soirée (source : tickets).
pizzas_par_heure <- function(db_ticket, date_soiree) {
  date_soiree <- as.Date(date_soiree)
  db_ticket %>%
    filter(est_pizza(PRODUIT_FULL), DATE == date_soiree, QUANTITE > 0) %>%
    mutate(HEURE = heure_service(TIMESTAMP)) %>%
    group_by(HEURE) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(HEURE), QUANTITE > 0)
}

##### Tuiles KPI #####

kpi_pizzwanze_tiles <- function(ps, unite_tva = NULL) {
  act <- ps$act; prec <- ps$prec; pic <- ps$pic
  q    <- sum(act$QUANTITE);  q_m1  <- sum(prec$QUANTITE)
  ca   <- sum(act$CA);        ca_m1 <- sum(prec$CA)
  nref <- nrow(act);          nref_m1 <- nrow(prec)
  nouv <- sum(act$NOUVEAUTE, na.rm = TRUE)
  vedette <- if (nrow(act) > 0) act$PIZZA[1] else NULL
  
  div(
    class = "kpi-grid",
    tuile_evolution(q, q_m1, "Pizzas vendues", "pizza-slice",
                    suffixe = "vs soirée précédente"),
    tuile_evolution(ca, ca_m1, paste("CA",unite_tva,"pizzas"), "euro-sign",
                    function(x) format_CA(x, -1),
                    suffixe = "vs soirée précédente"),
    tuile_ecart(nref, nref_m1, "Pizzas à la carte", "list-ul",
                suffixe = "vs soirée précédente"),
    kpi_tile(if (is.na(ps$ecart_jours)) "—" else paste0(round(ps$ecart_jours), " j"),
             "Depuis la précédente", COUL_NEUTRE, "calendar-day",
             sous_titre = if (is.na(ps$precedente)) "première soirée"
             else format(ps$precedente, "%d/%m/%Y")),
    kpi_tile(if (is.null(vedette)) "—" else str_trunc(vedette, 18),
             "Pizza vedette", CONSO_BRUN, "trophy",
             sous_titre = if (is.null(vedette)) NULL
             else paste0(act$QUANTITE[1], " vendues")),
    kpi_tile(as.character(pic), "Pic de consommation", "#8d7b68", "clock",
             sous_titre = "")
  )
}

# Historique : pizzas vendues par soirée, la soirée analysée mise en avant.
graph_evo_pizzwanze <- function(hist, soiree = NULL) {
  if (is.null(hist) || nrow(hist) == 0)
    return(plotly_empty() %>% layout(title = "Aucune soirée détectée"))
  
  couleurs <- if (is.null(soiree)) CONSO_BRUN
  else ifelse(hist$DATE == as.Date(soiree), CONSO_AMBRE, CONSO_BRUN)
  
  plot_ly(hist) %>%
    add_bars(x = ~DATE, y = ~PIZZAS, name = "Pizzas",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(format(DATE, "%a %d/%m/%Y"), "<br>",
                                     PIZZAS, " pizzas — ", N_REF, " références<br>",
                                     format_CA(CA, -1),
                                     "<extra></extra>")) %>%
    # add_markers(x = ~DATE, y = ~PIZZAS, name = "Nouveautés",
    #             marker = list(color = COUL_VERT, size = 8, symbol = "diamond"),
    #             text = ~NOUVEAUTES,
    #             hovertemplate = ~paste0(NOUVEAUTES, " nouveauté(s)<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "Pizzas vendues"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Répartition de la soirée : une barre par pizza, colorée selon son statut.
graph_pizzas_soiree <- function(ps) {
  act <- ps$act
  if (is.null(act) || nrow(act) == 0)
    return(plotly_empty() %>% layout(title = "Aucune pizza ce soir-là"))
  
  dat <- act %>%
    arrange(QUANTITE) %>%
    mutate(PIZZA_LBL = factor(str_trunc(PIZZA, 30), levels = str_trunc(PIZZA, 30)),
           STATUT_SOIR = factor(STATUT_SOIR, levels = STATUTS_PIZZA))
  
  p <- plot_ly()
  for (st in STATUTS_PIZZA) {
    sub <- dat %>% filter(STATUT_SOIR == st)
    if (nrow(sub) == 0) next
    p <- p %>% add_bars(
      data = sub, y = ~PIZZA_LBL, x = ~QUANTITE, orientation = "h", name = st,
      marker = list(color = PAL_STATUT_PIZZA[[st]]),
      hovertemplate = ~paste0(PIZZA, "<br>", QUANTITE, " vendues — ",
                              format_CA(CA, -1), "<br>", st, ", vue ",
                              N_SOIREES, " fois<extra></extra>"))
  }
  p %>% layout(barmode = "stack", xaxis = list(title = "Pizzas vendues"),
               yaxis = list(title = ""), 
               # legend = list(orientation = "h"),
               legend = list(yref = "container", y = 0, yanchor = "bottom"),
               margin = list(l = 10),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Heatmap pizza x soirée : d'un coup d'œil, qui revient et qui ne fait que
# passer. Les pizzas les plus présentes sont en haut.
graph_carte_pizzwanze <- function(db_produits, soirees, n_soirees = NULL,
                                  unite_tva = "HTVA") {
  if (length(soirees) == 0)
    return(plotly_empty() %>% layout(title = "Aucune soirée détectée"))
  
  # n_soirees = NULL -> tout l'historique. C'est le réglage le plus parlant :
  # la carte s'est stabilisée sur les dernières soirées, les allées et venues
  # de pizzas ne se voient qu'en remontant plus loin.
  dernieres <- sort(soirees)
  if (!is.null(n_soirees)) dernieres <- tail(dernieres, n_soirees)
  detail <- conso_pizzas(db_produits, dernieres, unite_tva)
  if (nrow(detail) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  ordre <- detail %>%
    group_by(PIZZA) %>%
    summarise(n = n_distinct(DATE), q = sum(QUANTITE), .groups = "drop") %>%
    arrange(n, q) %>%
    pull(PIZZA)
  
  grille <- detail %>%
    mutate(PIZZA = factor(PIZZA, levels = ordre)) %>%
    complete(PIZZA, DATE = dernieres, fill = list(QUANTITE = 0)) %>%
    arrange(PIZZA, DATE)
  
  mat <- grille %>%
    select(PIZZA, DATE, QUANTITE) %>%
    pivot_wider(names_from = DATE, values_from = QUANTITE, values_fill = 0) %>%
    arrange(PIZZA)
  cols <- as.character(dernieres)
  z <- as.matrix(mat[, cols, drop = FALSE])
  
  plot_ly(x = format(dernieres, "%d/%m/%y"),
          y = str_trunc(as.character(mat$PIZZA), 30), z = z,
          type = "heatmap", colorscale = list(c(0, "#f2efe6"), c(1, CONSO_BRUN)),
          hovertemplate = "%{y}<br>%{x} : %{z:.0f} vendues<extra></extra>") %>%
    layout(xaxis = list(title = "", side = "top"), yaxis = list(title = ""),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Rythme de la soirée : pizzas servies heure par heure.
graph_pizzas_heure <- function(par_heure) {
  if (is.null(par_heure) || nrow(par_heure) == 0)
    return(plotly_empty() %>% layout(title = "Pas de détail horaire"))
  
  plot_ly(par_heure) %>%
    add_bars(x = ~HEURE, y = ~QUANTITE,
             marker = list(color = CONSO_AMBRE),
             hovertemplate = ~paste0(HEURE, "<br>", QUANTITE,
                                     " pizzas<extra></extra>")) %>%
    layout(xaxis = list(title = "Heure de service"),
           yaxis = list(title = "Pizzas"), showlegend = FALSE, bargap = 0.3,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Détail par pizza, avec la part de la soirée et l'écart vs la précédente.
table_pizzwanze <- function(ps, unite_tva = NULL) {
  col_name <- paste("CA",unite_tva)
  act <- ps$act
  if (is.null(act) || nrow(act) == 0) return(tibble(Pizza = character()))
  
  total <- sum(act$QUANTITE)
  act %>%
    left_join(ps$prec %>% rename(Q_M1 = QUANTITE) %>% select(PIZZA, Q_M1),
              by = "PIZZA") %>%
    mutate(Q_M1 = replace_na(Q_M1, 0),
           PART = if (total > 0) 100 * QUANTITE / total else NA_real_,
           EVO  = ifelse(Q_M1 > 0, round(100 * (QUANTITE - Q_M1) / Q_M1, 1),
                         NA_real_)) %>%
    transmute(Pizza            = PIZZA,
              Quantité         = QUANTITE,
              Part             = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              !!sym(col_name) := format_CA(CA, -1),
              # Statut     = STATUT_SOIR,
              # `Soirées`  = N_SOIREES,
              `Qté préc.`      = Q_M1,
              `Évol.`          = ifelse(is.na(EVO), "—",
                                        paste0(ifelse(EVO >= 0, "+", ""), EVO, " %")))
}
