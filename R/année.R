#### REFONTE — Volet "Année" ####
# Suivi annuel « à date » : on ne compare que les jours déjà écoulés, en cumulé.

# Série quotidienne de l'année : CA, objectif, marge (matière hebdo étalée /7).
serie_annuelle <- function(db_kpi, db_obj, db_travail, db_matiere,
                           annee = year(today())) {
  d1 <- as.Date(paste0(annee, "-01-01"))
  d2 <- as.Date(paste0(annee, "-12-31"))
  
  jours <- db_kpi %>%
    filter(DATE >= d1, DATE <= d2) %>%
    select(DATE, ventes) %>%
    arrange(DATE)
  
  obj <- db_obj %>%
    filter(DATE >= d1, DATE <= d2) %>%
    select(DATE, objectif = ventes)
  
  trav <- db_travail %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(DATE) %>%
    summarise(TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")
  
  # Le coût matière est hebdomadaire -> réparti à parts égales sur les 7 jours
  mat <- db_matiere %>%
    group_by(SEMAINE) %>%
    summarise(MATIERE = sum(COUT_MATIERE, na.rm = TRUE), .groups = "drop")
  
  # jours <- jours |> 
  #   filter(DATE %in% trav$DATE) |> 
  #   filter(floor_date(DATE, "week", week_start = 1) %in% mat$SEMAINE)
  
  jours %>%
    left_join(obj,  by = "DATE") %>%
    left_join(trav, by = "DATE") %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    left_join(mat, by = "SEMAINE") %>%
    mutate(across(c(ventes, objectif, TRAVAIL, MATIERE), ~replace_na(., 0)),
           MATIERE = MATIERE / 7,
           MARGE   = ventes - TRAVAIL - MATIERE
           # MARGE   = ifelse(TRAVAIL == 0 | MATIERE == 0,NA, ventes - TRAVAIL - MATIERE
    )
}

# Graphe générique d'écart cumulé « à date » (aire verte au-dessus de 0, rouge en
# dessous) + point et annotation sur la dernière valeur connue.
graph_ecart_cumule <- function(dat, titre_y, libelle) {
  dat <- dat %>% filter(!is.na(ECART))
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  
  dernier <- dat %>% slice_tail(n = 1)
  couleur <- if (dernier$ECART >= 0) COUL_VERT else COUL_ROUGE
  
  plot_ly(dat) %>%
    add_lines(x = ~DATE, y = ~ECART, name = libelle,
              line = list(color = couleur, width = 2.5),
              fill = "tozeroy",
              fillcolor = if (dernier$ECART >= 0) "rgba(91,123,90,0.15)"
              else "rgba(192,57,43,0.15)",
              hovertemplate = ~paste0(LABEL, "<extra></extra>")) %>%
    add_markers(data = dernier, x = ~DATE, y = ~ECART, name = "Dernier jour",
                marker = list(color = couleur, size = 10),
                hovertemplate = ~paste0(LABEL, "<extra></extra>")) %>%
    layout(
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         y0 = 0, y1 = 0,
                         line = list(color = "#260b01", width = 1.5))),
      annotations = list(list(
        x = dernier$DATE, y = dernier$ECART,
        text = paste0("<b>", format_CA(dernier$ECART, -1), "</b>"),
        showarrow = TRUE, arrowhead = 0, ax = -45, ay = -30,
        font = list(color = couleur, size = 13),
        bgcolor = "rgba(255,255,255,0.75)", bordercolor = couleur)),
      xaxis = list(title = ""),
      yaxis = list(title = titre_y),
      showlegend = FALSE,
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Écart cumulé de CA vs objectif, à date.
graph_ecart_objectif <- function(serie) {
  dat <- serie %>%
    filter(DATE < today()) %>%
    arrange(DATE) %>%
    mutate(ECART = cumsum(ventes - objectif),
           LABEL = paste0(format(DATE, "%d/%m/%Y"),
                          "<br>CA : ", format_CA(ventes, -1),
                          "<br>Objectif : ", format_CA(objectif, -1),
                          "<br><b>Écart cumulé : ", format_CA(ECART, -1), "</b>"))
  graph_ecart_cumule(dat, "Écart cumulé vs objectif (€)", "Écart vs objectif")
}

# Écart cumulé vs N-1 (aligné sur le même numéro de semaine et le même jour).
graph_ecart_ym1 <- function(db_kpi, annee = year(today()), var = c("ventes", "marge"),
                            serie = NULL, serie_m1 = NULL) {
  var <- match.arg(var)
  
  prep <- function(d) {
    d %>% mutate(WEEK = week(DATE), WDAY = wday(DATE))
  }
  
  if (var == "ventes") {
    cur <- db_kpi %>% filter(year(DATE) == annee) %>%
      transmute(DATE, VAL = ventes) %>% prep()
    prec <- db_kpi %>% filter(year(DATE) == annee - 1) %>%
      transmute(DATE, VAL_M1 = ventes) %>% prep() %>%
      select(WEEK, WDAY, VAL_M1)
    titre <- "Écart cumulé de CA vs N-1 (€)"
    nom   <- "CA"
  } else {
    cur <- serie %>% transmute(DATE, VAL = MARGE) %>% prep()
    prec <- serie_m1 %>% transmute(DATE, VAL_M1 = MARGE) %>% prep() %>%
      select(WEEK, WDAY, VAL_M1)
    titre <- "Écart cumulé de marge vs N-1 (€)"
    nom   <- "Marge"
  }
  
  dat <- cur %>%
    left_join(prec, by = c("WEEK", "WDAY")) %>%
    arrange(DATE) %>%
    filter(DATE < today()) %>%
    mutate(
      VAL = replace_na(VAL, 0), VAL_M1 = replace_na(VAL_M1, 0),
      ECART = replace_na(cumsum(VAL - VAL_M1), 0),
      LABEL = paste0(format(DATE, "%d/%m/%Y"),
                     "<br>", nom, " : ", format_CA(VAL, -1),
                     "<br>", nom, " N-1 : ", format_CA(VAL_M1, -1),
                     "<br><b>Écart cumulé : ", format_CA(ECART, -1), "</b>"))
  
  graph_ecart_cumule(dat, titre, paste("Écart", nom, "vs N-1"))
}

# Tuiles de synthèse annuelle « à date ».
kpi_annee_tiles <- function(serie, serie_m1, unite_tva = NULL) {
  ecoule <- serie %>% filter(DATE < today())
  ca     <- sum(ecoule$ventes, na.rm = TRUE)
  obj    <- sum(ecoule$objectif, na.rm = TRUE)
  marge  <- sum(ecoule$MARGE, na.rm = TRUE)
  
  # N-1 sur le même nombre de jours d'ouverture écoulés
  n_jours <- nrow(ecoule %>% filter(ventes > 0))
  ecoule_m1 <- serie_m1 %>% filter(ventes > 0) %>% arrange(DATE) %>% head(n_jours)
  ca_m1    <- sum(ecoule_m1$ventes, na.rm = TRUE)
  marge_m1 <- sum(ecoule_m1$MARGE, na.rm = TRUE)
  
  pct <- function(x, y) if (y > 0) round(100 * x / y, 1) else NA_real_
  
  div(
    class = "kpi-grid",
    kpi_tile(format_CA(ca, -1), titre_avec_tva("CA à date", unite_tva),
             "#2e7d32", "euro-sign"),
    kpi_tile(format_CA(obj, -1), "Objectif à date", COUL_TRAVAIL, "bullseye",
             sous_titre = format_pct(pct(ca, obj))),
    kpi_tile(format_CA(ca - obj, -1), "Écart objectif",
             if (ca >= obj) COUL_VERT else COUL_ROUGE, "arrow-right-arrow-left"),
    kpi_tile(format_CA(ca - ca_m1, -1), "Écart CA vs N-1",
             if (ca >= ca_m1) COUL_VERT else COUL_ROUGE, "clock-rotate-left",
             sous_titre = paste0("N-1 : ", format_CA(ca_m1, -1))),
    kpi_tile(format_CA(marge, -1), "Marge à date",
             if (marge >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(pct(marge, ca))),
    kpi_tile(format_CA(marge - marge_m1, -1), "Écart marge vs N-1",
             if (marge >= marge_m1) COUL_VERT else COUL_ROUGE, "chart-line",
             sous_titre = paste0("N-1 : ", format_CA(marge_m1, -1)))
  )
}