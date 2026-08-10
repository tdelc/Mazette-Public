#### REFONTE — Volet "Année" ####
# Suivi annuel « à date » : on ne compare que les jours déjà écoulés, en cumulé.

# Série quotidienne de l'année : CA, objectif, et marge d'exploitation.
#
# La marge vient de DB_COMPTA, seule source qui couvre tout l'historique avec
# TOUTES les charges — matières, rémunérations, frais généraux, amortissements.
# L'ancienne version croisait DB_HEURES (avril 2025 → mars 2026) et les matières
# seules : hors de cette fenêtre le coût du travail valait zéro, et la marge
# était surévaluée de moitié. Comparer deux années devenait sans objet.
#
# La comptabilité étant mensuelle, ses charges sont étalées uniformément sur les
# jours d'ouverture du mois. Le cumul est donc exact à chaque fin de mois ; seul
# le chemin à l'intérieur du mois est lissé. Un mois sans comptabilité laisse la
# marge à NA plutôt que de la surestimer.
serie_annuelle <- function(db_kpi, db_obj, db_compta, annee = year(today())) {
  d1 <- as.Date(paste0(annee, "-01-01"))
  d2 <- as.Date(paste0(annee, "-12-31"))

  jours <- db_kpi %>%
    filter(DATE >= d1, DATE <= d2) %>%
    select(DATE, ventes) %>%
    arrange(DATE)

  obj <- db_obj %>%
    filter(DATE >= d1, DATE <= d2) %>%
    select(DATE, objectif = ventes)

  res <- jours %>%
    left_join(obj, by = "DATE") %>%
    mutate(across(c(ventes, objectif), ~replace_na(., 0)),
           MOIS = floor_date(DATE, "month"))

  # AUTRES = subsides, interventions ONEM... Les omettre sous-estimait la marge
  # de plus de 100 000 EUR par an : ce sont des produits d'exploitation, ils
  # entrent dans le resultat au meme titre que le chiffre d'affaires.
  charges <- if (is.null(db_compta) || !nrow(db_compta)) NULL else
    postes_exploitation(db_compta, d1, d2) %>%
      select(MOIS = PERIODE, AUTRES, MATIERES, REMUNERATION, GENERAUX,
             AMORTISSEMENT, CHARGES)

  if (is.null(charges) || !nrow(charges)) {
    return(res %>% mutate(AUTRES = NA_real_, CHARGES = NA_real_,
                          MARGE = NA_real_) %>% select(-MOIS))
  }

  # Répartition sur les seuls jours d'ouverture : imputer une charge à un jour
  # de fermeture donnerait une marge négative sans activité en face.
  res %>%
    left_join(charges, by = "MOIS") %>%
    group_by(MOIS) %>%
    mutate(N_OUVERTS = sum(ventes > 0),
           across(c(AUTRES, MATIERES, REMUNERATION, GENERAUX, AMORTISSEMENT,
                    CHARGES),
                  ~ if_else(N_OUVERTS > 0 & ventes > 0, . / N_OUVERTS, 0))) %>%
    ungroup() %>%
    mutate(MARGE = if_else(is.na(CHARGES), NA_real_,
                           ventes + AUTRES - CHARGES)) %>%
    select(-MOIS, -N_OUVERTS)
}

# Marge cumulée de l'année, superposée à celle de N-1 alignée sur la date N
graph_marge_cumulee <- function(serie, serie_m1, annee = year(today())) {
  
  cum <- function(d) {
    d <- d %>% filter(DATE < today(),ventes > 0,!is.na(MARGE)) %>% arrange(DATE)
    if (!nrow(d)) return(NULL)
    d %>% mutate(RANG = row_number(), MARGE = replace_na(MARGE,0), CUMUL = cumsum(MARGE))
  }
  a <- cum(serie); b <- cum(serie_m1)
  if (is.null(a))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de comptabilité sur cette année")))

  fin <- slice_tail(a, n = 1)
  couleur <- if (fin$CUMUL >= 0) COUL_VERT else COUL_ROUGE
  
  g <- plot_ly() %>%
    add_trace(x = a$RANG, y = a$CUMUL, type = "scatter", mode = "lines",
              name = as.character(annee), fill = "tozeroy",
              fillcolor = if (fin$CUMUL >= 0) "rgba(91,123,90,0.18)"
                          else "rgba(192,57,43,0.15)",
              line = list(color = if (fin$CUMUL >= 0) COUL_VERT else COUL_ROUGE,
                          width = 3),
              hovertemplate = paste0(format(a$DATE, "%d/%m/%Y"),
                                     "<br>Marge cumulée : ", format_CA(a$CUMUL, -1),
                                     "<extra></extra>"))
  if (!is.null(b)) {
    g <- g %>% add_trace(x = b$RANG, y = b$CUMUL, type = "scatter", mode = "lines",
                         name = as.character(annee - 1),
                         line = list(color = COUL_NEUTRE, width = 2, dash = "dot"),
                         hovertemplate = paste0(format(b$DATE, "%d/%m/%Y"),
                                  "<br>Marge cumulée N-1 : ", format_CA(b$CUMUL, -1),
                                  "<extra></extra>"))
  }
  g %>%
    layout(
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         y0 = 0, y1 = 0,
                         line = list(color = "#260b01", width = 1.5))),
      annotations = list(list(
        x = fin$RANG, y = fin$CUMUL,
        text = paste0("<b>", format_CA(fin$CUMUL, -1), "</b>"),
        showarrow = TRUE, arrowhead = 0, ax = -45, ay = -30,
        font = list(color = couleur, size = 13),
        bgcolor = "rgba(255,255,255,0.75)", bordercolor = couleur)),
      xaxis = list(title = "Jours d'ouverture écoulés"),
           yaxis = list(title = "Marge cumulée (€)", zeroline = TRUE,
                        zerolinecolor = "#8d7b68"),
           legend = list(orientation = "h", y = -0.2),
           margin = list(b = 60),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
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

  # La marge ne se cumule que sur les jours où la comptabilité existe : ajouter
  # un jour sans charges y ferait entrer son CA en marge pure.
  avec_marge <- ecoule %>% filter(!is.na(MARGE), ventes > 0)
  marge      <- sum(avec_marge$MARGE, na.rm = TRUE)
  ca_marge   <- sum(avec_marge$ventes, na.rm = TRUE)

  # N-1 sur le même nombre de jours d'ouverture écoulés. La marge se compare sur
  # le même nombre de jours COUVERTS, sinon on oppose 12 mois à 3.
  n_jours   <- nrow(ecoule %>% filter(ventes > 0))
  ecoule_m1 <- serie_m1 %>% filter(ventes > 0) %>% arrange(DATE) %>% head(n_jours)
  ca_m1     <- sum(ecoule_m1$ventes, na.rm = TRUE)
  marge_m1  <- ecoule_m1 %>% filter(!is.na(MARGE)) %>% arrange(DATE) %>%
    head(nrow(avec_marge)) %>% pull(MARGE) %>% sum(na.rm = TRUE)

  div(
    class = "kpi-grid",
    kpi_tile(format_CA(ca, -1), titre_avec_tva("CA à date", unite_tva),
             "#2e7d32", "euro-sign",
             sous_titre = paste(n_jours, "jours d'ouverture")),
    kpi_tile(format_CA(obj, -1), "Objectif à date", COUL_AMBRE, "bullseye",
             sous_titre = format_pct(ratio_pct(ca, obj))),
    kpi_tile(format_CA(ca - obj, -1), "Écart objectif",
             if (ca >= obj) COUL_VERT else COUL_ROUGE, "arrow-right-arrow-left"),
    kpi_tile(format_CA(ca - ca_m1, -1), "Écart CA vs N-1",
             if (ca >= ca_m1) COUL_VERT else COUL_ROUGE, "clock-rotate-left",
             sous_titre = paste0("N-1 : ", format_CA(ca_m1, -1))),
    if (nrow(avec_marge) == 0)
      kpi_tile("—", "Marge d'exploitation", COUL_NEUTRE, "piggy-bank",
               sous_titre = "pas de comptabilité")
    else
      kpi_tile(format_CA(marge, -1), "Marge d'exploitation à date",
               if (marge >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
               sous_titre = paste0(format_pct(ratio_pct(marge, ca_marge)),
                                   " du CA · ", nrow(avec_marge), " jours couverts")),
    if (nrow(avec_marge) == 0) NULL else
      kpi_tile(format_CA(marge - marge_m1, -1), "Écart marge vs N-1",
               if (marge >= marge_m1) COUL_VERT else COUL_ROUGE, "chart-line",
               sous_titre = paste0("N-1 : ", format_CA(marge_m1, -1)))
  )
}