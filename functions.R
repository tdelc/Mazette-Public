

#### REFONTE — Conventions de couleurs ####
# Palette d'appréciation, partagée par tous les volets de la refonte.
COUL_VERT   <- "#5B7B5A"
COUL_AMBRE  <- "#d98236"
COUL_ROUGE  <- "#c0392b"
COUL_NEUTRE <- "#8d7b68"

# Couleur d'un CA selon l'atteinte de son objectif. Même convention que les
# box de ventes (cf. get_color_from_gradient) :
#   >= 100 %  -> vert    (objectif atteint)
#   >=  90 %  -> ambre   (tout proche)
#   <   90 %  -> rouge   (manqué)
# Sans objectif renseigné (0 ou NA), la barre reste neutre : on ne peut rien
# juger. Vectorisé, donc utilisable directement sur une colonne.
couleur_objectif <- function(reel, objectif, seuil_proche = 0.9) {
  pct <- ifelse(is.na(objectif) | objectif <= 0, NA_real_, reel / objectif)
  case_when(
    is.na(pct)          ~ COUL_NEUTRE,
    pct >= 1            ~ COUL_VERT,
    pct >= seuil_proche ~ COUL_AMBRE,
    TRUE                ~ COUL_ROUGE
  )
}

# Libellé "x % de l'objectif" pour les infobulles.
label_objectif <- function(reel, objectif) {
  ifelse(is.na(objectif) | objectif <= 0, "pas d'objectif",
         paste0(round(100 * reel / objectif), " % de l'objectif"))
}


#### REFONTE — Volet "Maintenant" ####

# Tronque un nom de produit trop long
tronque_nom <- function(x, max = 40) {
  ifelse(nchar(x) > max, paste0(substr(x, 1, max), "…"), x)
}

# Produit "bière" = catégorie contenant BIÈRE (rotation constante -> exclu des comparaisons)
est_biere <- function(category) {
  str_detect(toupper(replace_na(category, "")), "BI[EÈ]RE")
}

# Top produits (CA HTVA) sur une période [date_debut, date_fin]
top_produits_periode <- function(db_produits, date_debut, date_fin, n = 10) {
  db_produits %>%
    filter(DATE >= date_debut, DATE <= date_fin) %>%
    group_by(PRODUCT) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(CA)) %>%
    slice_head(n = n) %>%
    transmute(Produit = tronque_nom(PRODUCT),
              Quantité = Quantite,
              `CA HTVA` = format_CA(CA, -1))
}

# Évolution des produits (hors bières) : semaine en cours vs semaine précédente
evolution_produits_semaine <- function(db_produits, date_debut_semaine, n = 10,
                                       sens = c("hausse", "baisse")) {
  sens <- match.arg(sens)

  agrege <- function(d1, d2) {
    db_produits %>%
      filter(DATE >= d1, DATE <= d2, !est_biere(CATEGORY)) %>%
      group_by(PRODUCT) %>%
      summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE), .groups = "drop")
  }

  sem    <- agrege(date_debut_semaine,     date_debut_semaine + 6)
  sem_m1 <- agrege(date_debut_semaine - 7, date_debut_semaine - 1) %>%
    rename(QUANTITE_m1 = QUANTITE)

  evo <- inner_join(sem, sem_m1, by = "PRODUCT") %>%
    mutate(delta = QUANTITE - QUANTITE_m1)

  evo <- if (sens == "hausse") arrange(evo, desc(delta)) else arrange(evo, delta)

  evo %>%
    slice_head(n = n) %>%
    transmute(Produit = tronque_nom(PRODUCT),
              `Cette sem.` = QUANTITE,
              `Sem. -1` = QUANTITE_m1,
              `Δ` = delta)
}

# Cumul jour à jour du CA réalisé vs objectif sur un mois donné
progression_mois <- function(db_kpi, db_obj, mois = floor_date(today(), "month")) {
  fin <- ceiling_date(mois, "month") - 1
  
  reel <- db_kpi %>% filter(DATE >= mois, DATE <= fin) %>% transmute(DATE, ventes)
  obj  <- db_obj %>% filter(DATE >= mois, DATE <= fin) %>% transmute(DATE, objectif = ventes)
  
  full_join(reel, obj, by = "DATE") %>%
    arrange(DATE) %>%
    mutate(ventes   = replace_na(ventes, 0),
           objectif = replace_na(objectif, 0),
           cum_reel = cumsum(ventes),
           cum_obj  = cumsum(objectif),
           # on n'affiche pas le cumulé réalisé pour les jours pas encore passés
           cum_reel = ifelse(DATE > today(), NA, cum_reel))
}

# Graphe de progression mensuelle : objectif cumulé (pointillé) + réalisé cumulé (aire)
graph_progression_mois <- function(prog, mois = floor_date(today(), "month")) {
  plot_ly(prog) %>%
    add_lines(x = ~DATE, y = ~cum_obj, name = "Objectif cumulé",
              line = list(color = "#260b01", dash = "dot", width = 1.5),
              hovertemplate = ~paste0(format(DATE, "%d/%m"), "<br>Objectif ",
                                      format_CA(cum_obj, -1), "<extra></extra>")) %>%
    add_lines(x = ~DATE, y = ~cum_reel, name = "Réalisé cumulé",
              line = list(color = "#732c02", width = 3),
              fill = "tozeroy", fillcolor = "rgba(115,44,2,0.10)",
              connectgaps = FALSE,
              hovertemplate = ~paste0(format(DATE, "%d/%m"), "<br>Réalisé ",
                                      format_CA(cum_reel, -1), "<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA cumulé (€)"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}


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
liste_produits_periode <- function(db_produits, d1, d2) {
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(Produit = PRODUCT) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(CA))
}

# Évolution hebdomadaire d'un produit
evolution_un_produit <- function(db_produits, produit, d1, d2) {
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) |> 
    mutate(SEMAINE = floor_date(DATE, unit = "week", week_start = 1)) %>%
    group_by(SEMAINE) %>%
    mutate(CA_TOT = sum(CA_HTVA, na.rm = TRUE)) |> 
    group_by(SEMAINE,CATEGORY) %>%
    mutate(CA_CATEGORY = sum(CA_HTVA, na.rm = TRUE)) |> 
    filter(PRODUCT == produit) %>%
    group_by(SEMAINE,CATEGORY) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), 
              PC_ALL = CA / mean(CA_TOT, na.rm = TRUE), 
              PC_CATEGORY = CA / mean(CA_CATEGORY, na.rm = TRUE), 
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
  
  dat$ma <- forecast::ma(dat$ventes,5)
  
  lbl <- if (unite == "semaine") paste0("Sem. du ", format(dat$PERIODE, "%d/%m/%Y"))
  else format(dat$PERIODE, "%B %Y")
  # (pas de couleur par objectif ici : ce graphe trace des lignes, pas des barres)

  plot_ly(dat) %>%
    add_lines(x = ~PERIODE, y = ~ventes, name = "CA réalisé",
              line = list(color = "#d98236"),
             hovertemplate = ~paste0(lbl, "<br>CA ", format_CA(ventes, -1), "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~ma, name = "",
              line = list(color = "#5B7BAA")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  
  # p <- dat %>% 
  #   ggplot() +
  #   aes(x = PERIODE, y = ventes) +
  #   geom_line()+
  #   scale_x_date(breaks = "years")+
  #   labs(x = "", y = "CA (€)")+
  #   geom_smooth(method = "loess",formula = 'y ~ x')
  # 
  # ggplotly(p) %>% 
  #   layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
  #          bargap = 0.3, legend = list(orientation = "h"),
  #          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}



#### REFONTE — Volet "Bières" ####

# Niveau actuel de chaque bière en cours (dernière mesure connue)
niveau_bieres_actuel <- function(max_date = today()) {
  DB_BIERES %>%
    filter(!FL_FINI, DATE <= max_date, DATE >= max_date - 30) %>%
    group_by(ID_BRASSIN, BOISSON) %>%
    arrange(DATE) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    transmute(ID_BRASSIN, BOISSON,
              VOLUME_TOTAL = VOLUME_BRASSIN_AJUST,
              VOLUME_RESTANT = pmax(0, DIFF),
              PCT = ifelse(VOLUME_BRASSIN_AJUST > 0,
                           round(100 * pmax(0, DIFF) / VOLUME_BRASSIN_AJUST), 0)) %>%
    arrange(PCT)
}

# Date de fin prévue par brassin (tôt / estimée / tard), à partir de la table
# de prédiction HoltWinters. Une ligne par ID_BRASSIN.
predictions_par_brassin <- function(db_predict) {
  vide <- tibble(ID_BRASSIN = character(), FIN_TOT = as.Date(character()),
                 FIN_EST = as.Date(character()), FIN_TARD = as.Date(character()))
  if (is.null(db_predict) || nrow(db_predict) == 0) return(vide)

  as_date <- function(x) as.Date(x, origin = "1970-01-01")
  unique(db_predict$ID_BRASSIN) %>%
    map_df(function(id) {
      fin <- predict_fin_brassin(db_predict, id)
      tibble(ID_BRASSIN = id, FIN_TOT = as_date(fin[1]),
             FIN_EST = as_date(fin[2]), FIN_TARD = as_date(fin[3]))
    })
}

# Libellé + couleur de l'échéance d'un fût, selon le nombre de jours restants.
# Sert de code couleur d'urgence sous la jauge.
etiquette_fin_fut <- function(fin_est, aujourdhui = today()) {
  if (is.null(fin_est) || length(fin_est) == 0 || is.na(fin_est))
    return(list(texte = "fin non prévisible", couleur = "#8d7b68"))

  jours <- as.numeric(as.Date(fin_est) - aujourdhui)
  # Une échéance déjà passée alors que le fût est toujours ouvert signale un
  # retard de saisie ou une consommation plus lente que prévu : on le dit,
  # plutôt que d'annoncer « aujourd'hui ».
  quand <- if (jours < 0) "échéance dépassée"
           else if (jours == 0) "aujourd'hui"
           else if (jours == 1) "demain"
           else paste0("dans ", jours, " j")
  couleur <- if (jours <= 3) COUL_ROUGE
             else if (jours <= 7) COUL_AMBRE
             else COUL_VERT
  list(texte = paste0("fin ~ ", format(as.Date(fin_est), "%a %d/%m"),
                      " (", quand, ")"),
       couleur = couleur)
}

# Grille de jauges plotly : une par bière en cours, avec son niveau et — si la
# table de prédiction est fournie — la date de fin prévue juste en dessous du
# nom. On voit ainsi d'un coup d'œil la bière, son niveau et son échéance.
graph_niveaux_bieres <- function(niveaux, db_predict = NULL) {
  n <- nrow(niveaux)
  if (n == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière en cours"))

  if (!is.null(db_predict) && nrow(db_predict) > 0)
    niveaux <- niveaux %>%
      left_join(predictions_par_brassin(db_predict), by = "ID_BRASSIN")
  if (!"FIN_EST" %in% names(niveaux)) niveaux$FIN_EST <- as.Date(NA)

  ncol <- min(n, 5)
  nlig <- ceiling(n / ncol)

  p <- plot_ly()
  for (i in seq_len(n)) {
    lig <- (i - 1) %/% ncol
    col <- (i - 1) %% ncol
    pct <- niveaux$PCT[i]
    couleur <- if (pct < 20) COUL_ROUGE else if (pct < 40) COUL_AMBRE else COUL_VERT
    ech <- etiquette_fin_fut(niveaux$FIN_EST[i])

    titre <- paste0(
      "<b>", niveaux$BOISSON[i], "</b>",
      "<br><span style='font-size:0.72em;color:#888'>",
      round(niveaux$VOLUME_RESTANT[i]), " / ",
      round(niveaux$VOLUME_TOTAL[i]), " L</span>",
      "<br><span style='font-size:0.72em;color:", ech$couleur, "'>",
      ech$texte, "</span>")

    p <- p %>% add_trace(
      type = "indicator", mode = "gauge+number",
      value = pct,
      # Le nombre était plus imposant que le nom de la bière : on le ramène à
      # une taille proche de celle du titre.
      number = list(suffix = " %", font = list(size = 22)),
      title = list(text = titre, font = list(size = 14)),
      gauge = list(axis = list(range = list(0, 100), ticksuffix = "%"),
                   bar = list(color = couleur),
                   bordercolor = "rgba(0,0,0,0.10)"),
      domain = list(row = lig, column = col)
    )
  }
  p %>% layout(grid = list(rows = nlig, columns = ncol, pattern = "independent"),
               # trois lignes de titre : il faut plus d'air en haut
               margin = list(t = 90, b = 10),
               paper_bgcolor = "rgba(0,0,0,0)")
}

# Évolution + prédiction du volume restant (version plotly de graph_evo_brassin)
graph_evo_brassin_plotly <- function(db, max_affichage = today() %m+% months(1)) {
  if (is.null(db) || nrow(db) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière en cours"))

  # On n'affiche la prédiction que jusqu'à un mois après aujourd'hui (au-delà,
  # l'extrapolation HoltWinters n'apporte rien et écrase l'échelle).
  db <- db %>% filter(DATE <= max_affichage)
  if (nrow(db) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière en cours"))

  bieres <- unique(db$BOISSON)
  pal <- setNames(
    grDevices::colorRampPalette(
      c("#732c02", "#d98236", "#5B7B5A", "#2980b9", "#9b59b6"))(length(bieres)),
    bieres)

  plt <- plot_ly()
  for (b in bieres) {
    sub  <- db %>% filter(BOISSON == b) %>% arrange(DATE)
    act  <- sub %>% filter(!FL_PREDICT)
    pred <- sub %>% filter(FL_PREDICT)
    couleur <- pal[[b]]

    plt <- plt %>% add_lines(
      data = act, x = ~DATE, y = ~VOLUME_RESTANT,
      name = b, legendgroup = b, line = list(color = couleur, width = 2.5),
      hovertemplate = ~paste0(BOISSON, "<br>", format(DATE, "%d/%m"), "<br>",
                              round(VOLUME_RESTANT), " L<extra></extra>"))

    if (nrow(pred) > 0) {
      pred2 <- bind_rows(slice_tail(act, n = 1), pred)
      plt <- plt %>% add_lines(
        data = pred2, x = ~DATE, y = ~VOLUME_RESTANT,
        name = b, legendgroup = b, showlegend = FALSE,
        line = list(color = couleur, width = 2, dash = "dot"),
        hovertemplate = ~paste0(BOISSON, " (prév.)<br>", format(DATE, "%d/%m"), "<br>",
                                round(VOLUME_RESTANT), " L<extra></extra>"))
    }
  }
  plt %>% layout(xaxis = list(title = ""),
                 yaxis = list(title = "Volume restant (L)", rangemode = "tozero",range = c(0, 400)),
                 legend = list(orientation = "h"),
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Table des dates de fin de fût prévues (tôt / estimée / tard) par bière
table_predictions_fin <- function(db_predict) {
  if (is.null(db_predict) || nrow(db_predict) == 0)
    return(tibble(Bière = character(), `Volume restant` = character(),
                  `Fin (tôt)` = character(), `Fin estimée` = character(),
                  `Fin (tard)` = character()))

  fmt <- function(x) {
    x <- as.Date(x, origin = "1970-01-01")
    ifelse(is.na(x), "—", format(x, "%a %d/%m/%Y"))
  }

  unique(db_predict$ID_BRASSIN) %>%
    map_df(function(id) {
      fin  <- predict_fin_brassin(db_predict, id)
      info <- db_predict %>%
        filter(ID_BRASSIN == id, !FL_PREDICT) %>%
        slice_tail(n = 1)
      tibble(Bière = info$BOISSON,
             `Volume restant` = paste0(round(info$VOLUME_RESTANT), " L"),
             `Fin (tôt)`  = fmt(fin[1]),
             `Fin estimée` = fmt(fin[2]),
             `Fin (tard)` = fmt(fin[3]),
             ord = suppressWarnings(as.numeric(as.Date(fin[2], origin = "1970-01-01"))))
    }) %>%
    arrange(ord) %>%
    select(-ord)
}


#### REFONTE — Volet "Simulation" ####

# Base de simulation : par produit sur une période -> quantité, CA HTVA, prix moyen HTVA.
# Ordre stable (CATEGORY puis CA décroissant) pour mapper les éditions par n° de ligne.
prepa_simulation <- function(db_produits, d1, d2) {
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(CATEGORY, PRODUCT = PRODUCT_FULL) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    mutate(PRIX_MOYEN = round(CA / QUANTITE, 2)) %>%
    arrange(CATEGORY, desc(CA))
}

# Applique un vecteur de prix simulés (indexé par n° de ligne) à la base.
# Hypothèse : quantité inchangée -> le CA varie au prorata du prix.
calc_simulation <- function(base, prix_simu) {
  if (is.null(prix_simu) || length(prix_simu) != nrow(base))
    prix_simu <- base$PRIX_MOYEN
  base %>%
    mutate(PRIX_SIMU = as.numeric(prix_simu),
           PRIX_SIMU = ifelse(is.na(PRIX_SIMU), PRIX_MOYEN, PRIX_SIMU),
           CA_SIMU = QUANTITE * PRIX_SIMU,
           DELTA = CA_SIMU - CA)
}

# Mise en forme pour affichage DT (table éditable côté serveur)
table_simulation_aff <- function(sim) {
  sim %>%
    transmute(Catégorie = CATEGORY,
              Produit = tronque_nom(PRODUCT),
              Quantité = QUANTITE,
              `Prix moyen` = PRIX_MOYEN,
              `Prix simulé` = round(PRIX_SIMU, 2),
              `CA actuel` = round(CA),
              `CA simulé` = round(CA_SIMU),
              `Δ CA` = round(DELTA))
}


#### REFONTE — Volet "Compta / Gestion" ####
# Sources (fictives pour l'instant, cf. donnees_fictives_compta.R) :
#   DB_COUTS_TRAVAIL : DATE    x SECTEUR -> HEURES, COUT_TRAVAIL
#   DB_COUTS_MATIERE : SEMAINE x SECTEUR -> ACHATS, VARIATION_STOCK, COUT_MATIERE
# 4 secteurs, JAMAIS agrégés entre eux : Service / Transformation alimentaire /
# Brasserie / Support. Indicateurs : Food Cost, Work Cost, Prime Cost, Marge.
#
# NB : les coûts matière sont hebdomadaires. Quand on agrège au mois ou à
# l'année, chaque semaine est rattachée à la période de son LUNDI (règle simple
# et stable ; une semaine à cheval compte donc pour le mois de son lundi).

# Couleurs par secteur (déclinaison de la palette Mazette)
COULEURS_SECTEURS <- c(
  "Service"                    = "#2980b9",
  "Transformation alimentaire" = "#5B7B5A",
  "Brasserie"                  = "#d98236",
  "Support"                    = "#8d7b68"
)

COUL_MATIERE <- "#d3c0ac"   # coût matière / frais généraux
COUL_TRAVAIL <- "#732c02"   # coût du personnel
# COUL_VERT / COUL_AMBRE / COUL_ROUGE : cf. « Conventions de couleurs » en tête
# de fichier, partagées avec les barres de CA vs objectif.

##### Périodes #####

# Début de période d'une date selon la granularité.
debut_periode <- function(d, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
    semaine = floor_date(d, "week", week_start = 1),
    mois    = floor_date(d, "month"),
    annee   = floor_date(d, "year"))
}

# Dernier jour d'une période.
fin_periode <- function(periode, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
    semaine = periode + 6,
    mois    = ceiling_date(periode, "month") - 1,
    annee   = ceiling_date(periode, "year") - 1)
}

# Étiquette lisible d'une période (date = début de période).
label_periode <- function(periode, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
    semaine = paste0("Sem. ", format(periode, "%d/%m/%y")),
    mois    = format(periode, "%B %Y"),
    annee   = format(periode, "%Y"))
}

# Périodes disponibles (avec du CA), de la plus récente à la plus ancienne.
liste_periodes_dispo <- function(db_kpi, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  db_kpi %>%
    filter(ventes > 0) %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    distinct(PERIODE) %>%
    arrange(desc(PERIODE)) %>%
    pull(PERIODE)
}

##### Agrégation compta #####

# Agrège CA / matières / personnel / marge par période, avec les ratios KPI.
#   db_kpi     : sortie de prepa_db (DATE, ventes)
#   db_travail : DB_COUTS_TRAVAIL
#   db_matiere : DB_COUTS_MATIERE
agrege_compta <- function(db_kpi, db_travail, db_matiere,
                          unite = c("semaine", "mois", "annee"),
                          d1 = NULL, d2 = NULL) {
  unite <- match.arg(unite)

  if (!is.null(d1)) {
    d1 <- as.Date(d1)
    db_kpi     <- filter(db_kpi,     DATE    >= d1)
    db_travail <- filter(db_travail, DATE    >= d1)
    db_matiere <- filter(db_matiere, SEMAINE >= d1)
  }
  if (!is.null(d2)) {
    d2 <- as.Date(d2)
    db_kpi     <- filter(db_kpi,     DATE    <= d2)
    db_travail <- filter(db_travail, DATE    <= d2)
    db_matiere <- filter(db_matiere, SEMAINE <= d2)
  }
  
  # Se limiter aux données dispo en db_travail et en db_matiere
  db_kpi <- db_kpi |>
    filter(DATE %in% db_travail$DATE,
           PREMIER_JOUR_SEMAINE %in% db_matiere$SEMAINE)

  ca <- db_kpi %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(CA = sum(ventes, na.rm = TRUE), .groups = "drop")

  trav <- db_travail %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE),
              HEURES  = sum(HEURES, na.rm = TRUE), .groups = "drop")

  # On sépare les matières « métier » (Service / Transfo / Brasserie) des frais
  # généraux (Support) : le Prime Cost au sens de la restauration = matières +
  # personnel, les frais généraux étant suivis à part.
  mat <- db_matiere %>%
    mutate(PERIODE = debut_periode(SEMAINE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(FOOD    = sum(COUT_MATIERE[SECTEUR != "Support"], na.rm = TRUE),
              GENERAL = sum(COUT_MATIERE[SECTEUR == "Support"], na.rm = TRUE),
              .groups = "drop")

  ca %>%
    full_join(trav, by = "PERIODE") %>%
    full_join(mat,  by = "PERIODE") %>%
    filter(!is.na(CA)) |> 
    arrange(PERIODE) %>%
    mutate(across(c(CA, TRAVAIL, HEURES, FOOD, GENERAL), ~replace_na(., 0))) %>%
    mutate(MATIERE = FOOD + GENERAL,
           PRIME   = FOOD + TRAVAIL,
           CHARGES = PRIME + GENERAL,
           MARGE   = CA - CHARGES,
           FOOD_PCT    = ratio_pct(FOOD,    CA),
           WORK_PCT    = ratio_pct(TRAVAIL, CA),
           GENERAL_PCT = ratio_pct(GENERAL, CA),
           PRIME_PCT   = ratio_pct(PRIME,   CA),
           MARGE_PCT   = ratio_pct(MARGE,   CA)) %>%
    filter(CA > 0 | CHARGES > 0)
}

# Ratio en % (NA si dénominateur nul)
ratio_pct <- function(num, den) ifelse(den > 0, round(100 * num / den, 1), NA_real_)

# Détail par secteur sur une fenêtre de dates (une ligne par secteur + Total).
compta_secteurs <- function(db_travail, db_matiere, d1, d2) {
  d1 <- as.Date(d1); d2 <- as.Date(d2)

  trav <- db_travail %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(SECTEUR) %>%
    summarise(HEURES  = sum(HEURES, na.rm = TRUE),
              TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")

  mat <- db_matiere %>%
    filter(SEMAINE >= d1, SEMAINE <= d2) %>%
    group_by(SECTEUR) %>%
    summarise(ACHATS  = sum(ACHATS, na.rm = TRUE),
              STOCK   = sum(VARIATION_STOCK, na.rm = TRUE),
              MATIERE = sum(COUT_MATIERE, na.rm = TRUE), .groups = "drop")

  tibble(SECTEUR = SECTEURS_COMPTA) %>%
    left_join(trav, by = "SECTEUR") %>%
    left_join(mat,  by = "SECTEUR") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)),
           TOTAL = MATIERE + TRAVAIL)
}

# Synthèse d'UNE période : la ligne d'agrégat + le détail par secteur.
compta_apercu <- function(db_kpi, db_travail, db_matiere, periode,
                          unite = c("semaine", "mois", "annee")) {
  unite   <- match.arg(unite)
  periode <- as.Date(periode)
  d1 <- periode
  d2 <- fin_periode(periode, unite)

  res <- agrege_compta(db_kpi, db_travail, db_matiere, unite, d1 = d1, d2 = d2)
  if (nrow(res) == 0)
    res <- tibble(PERIODE = periode, CA = 0, TRAVAIL = 0, HEURES = 0,
                  FOOD = 0, GENERAL = 0, MATIERE = 0, PRIME = 0, CHARGES = 0,
                  MARGE = 0, FOOD_PCT = NA_real_, WORK_PCT = NA_real_,
                  GENERAL_PCT = NA_real_, PRIME_PCT = NA_real_,
                  MARGE_PCT = NA_real_)

  list(unite    = unite,
       periode  = periode,
       libelle  = label_periode(periode, unite),
       bornes   = c(d1, d2),
       total    = res[1, ],
       secteurs = compta_secteurs(db_travail, db_matiere, d1, d2))
}

##### Tuiles KPI #####

# Couleur d'un ratio où PLUS BAS = MIEUX (food/work/prime cost).
couleur_seuil <- function(x, bon, moyen) {
  if (is.na(x)) return("#9e9e9e")
  if (x <= bon) COUL_VERT else if (x <= moyen) COUL_AMBRE else COUL_ROUGE
}

# Couleur d'un ratio où PLUS HAUT = MIEUX (marge).
couleur_seuil_haut <- function(x, bon, moyen) {
  if (is.na(x)) return("#9e9e9e")
  if (x >= bon) COUL_VERT else if (x >= moyen) COUL_AMBRE else COUL_ROUGE
}

format_pct <- function(x, nb = 1) if (is.na(x)) "—" else paste0(round(x, nb), " %")

# Une tuile KPI (grand chiffre + libellé + icône en filigrane)
kpi_tile <- function(valeur, libelle, couleur, icone = NULL, sous_titre = NULL) {
  div(
    class = "kpi-tile", style = paste0("background:", couleur, ";"),
    if (!is.null(icone)) span(class = "kpi-tile-icon", icon(icone)),
    div(class = "kpi-tile-val", valeur),
    div(class = "kpi-tile-lab", libelle),
    if (!is.null(sous_titre)) div(class = "kpi-tile-sub", sous_titre)
  )
}

# Grille des KPI d'une période (sortie de compta_apercu).
kpi_compta_tiles <- function(ap) {
  t <- ap$total
  div(
    class = "kpi-grid",
    kpi_tile(format_CA(t$CA, -1), "CA HTVA", "#2e7d32", "euro-sign"),
    kpi_tile(format_CA(t$MARGE, -1), "Marge",
             if (t$MARGE >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(t$MARGE_PCT)),
    kpi_tile(format_pct(t$FOOD_PCT), "Food Cost / CA",
             couleur_seuil(t$FOOD_PCT, 30, 35), "cart-shopping",
             sous_titre = format_CA(t$FOOD, -1)),
    kpi_tile(format_pct(t$WORK_PCT), "Work Cost / CA",
             couleur_seuil(t$WORK_PCT, 35, 42), "person-running",
             sous_titre = format_CA(t$TRAVAIL, -1)),
    kpi_tile(format_pct(t$PRIME_PCT), "Prime Cost / CA",
             couleur_seuil(t$PRIME_PCT, 65, 72), "scale-balanced",
             sous_titre = format_CA(t$PRIME, -1)),
    kpi_tile(format_pct(t$GENERAL_PCT), "Frais généraux / CA",
             couleur_seuil(t$GENERAL_PCT, 12, 18), "receipt",
             sous_titre = format_CA(t$GENERAL, -1)),
    kpi_tile(format(round(t$HEURES)), "Heures prestées", "#8d7b68", "clock",
             sous_titre = if (t$HEURES > 0)
               paste0(format_CA(t$CA / t$HEURES, -1), " de CA / h") else NULL)
  )
}

# Bandeau de comparaison A vs B (écarts en € et en points de %).
kpi_ecarts_tiles <- function(ap_a, ap_b) {
  a <- ap_a$total; b <- ap_b$total
  ec <- function(x, y) x - y
  pt <- function(x, y) if (is.na(x) || is.na(y)) NA_real_ else x - y
  signe <- function(v, unite = "€") {
    if (is.na(v)) return("—")
    prefixe <- if (v > 0) "+" else ""
    if (unite == "€") paste0(prefixe, format_CA(v, -1))
    else paste0(prefixe, round(v, 1), " pt")
  }
  # Pour les coûts, une hausse est défavorable -> rouge
  coul_bas <- function(v) if (is.na(v)) "#9e9e9e" else if (v <= 0) COUL_VERT else COUL_ROUGE
  coul_haut <- function(v) if (is.na(v)) "#9e9e9e" else if (v >= 0) COUL_VERT else COUL_ROUGE

  div(
    class = "kpi-grid",
    kpi_tile(signe(ec(a$CA, b$CA)), "Écart CA", coul_haut(ec(a$CA, b$CA)), "euro-sign"),
    kpi_tile(signe(ec(a$MARGE, b$MARGE)), "Écart marge",
             coul_haut(ec(a$MARGE, b$MARGE)), "piggy-bank"),
    kpi_tile(signe(pt(a$FOOD_PCT, b$FOOD_PCT), "pt"), "Écart food cost",
             coul_bas(pt(a$FOOD_PCT, b$FOOD_PCT)), "cart-shopping"),
    kpi_tile(signe(pt(a$WORK_PCT, b$WORK_PCT), "pt"), "Écart work cost",
             coul_bas(pt(a$WORK_PCT, b$WORK_PCT)), "person-running"),
    kpi_tile(signe(pt(a$PRIME_PCT, b$PRIME_PCT), "pt"), "Écart prime cost",
             coul_bas(pt(a$PRIME_PCT, b$PRIME_PCT)), "scale-balanced"),
    kpi_tile(signe(pt(a$MARGE_PCT, b$MARGE_PCT), "pt"), "Écart marge %",
             coul_haut(pt(a$MARGE_PCT, b$MARGE_PCT)), "percent")
  )
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


#### REFONTE — Volet "Comparaison" ####

# Tableau de comparaison : une ligne par période, ventes vs objectif ET compta.
comparaison_periodes <- function(db_kpi, db_obj, db_travail, db_matiere,
                                 unite = c("semaine", "mois", "annee"),
                                 periodes = NULL) {
  unite <- match.arg(unite)

  comptes <- agrege_compta(db_kpi, db_travail, db_matiere, unite)

  obj <- db_obj %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(OBJECTIF = sum(ventes, na.rm = TRUE), .groups = "drop")

  res <- comptes %>%
    left_join(obj, by = "PERIODE") %>%
    mutate(OBJECTIF = replace_na(OBJECTIF, 0),
           PCT_OBJ  = ratio_pct(CA, OBJECTIF))

  if (!is.null(periodes))
    res <- res %>% filter(PERIODE %in% as.Date(periodes))

  res %>% arrange(PERIODE)
}

# Barres groupées : CA réalisé / objectif / marge pour chaque période comparée.
graph_comparaison <- function(comp, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(comp) || nrow(comp) == 0)
    return(plotly_empty() %>% layout(title = "Sélectionnez des périodes à comparer"))

  comp <- comp %>% arrange(PERIODE)
  lab  <- factor(label_periode(comp$PERIODE, unite),
                 levels = label_periode(comp$PERIODE, unite))

  # La barre de CA prend la couleur de l'atteinte de son objectif ; l'objectif
  # lui-même reste neutre pour ne pas brouiller la lecture.
  atteinte <- label_objectif(comp$CA, comp$OBJECTIF)

  plot_ly(comp) %>%
    add_bars(x = lab, y = ~CA, name = "CA réalisé",
             marker = list(color = couleur_objectif(comp$CA, comp$OBJECTIF)),
             hovertemplate = ~paste0("CA ", format_CA(CA, -1), "<br>", atteinte,
                                     "<extra></extra>")) %>%
    add_bars(x = lab, y = ~OBJECTIF, name = "Objectif",
             marker = list(color = COUL_NEUTRE),
             hovertemplate = ~paste0("Objectif ", format_CA(OBJECTIF, -1), "<extra></extra>")) %>%
    add_bars(x = lab, y = ~MARGE, name = "Marge",
             marker = list(color = COUL_VERT),
             hovertemplate = ~paste0("Marge ", format_CA(MARGE, -1),
                                     " (", MARGE_PCT, " %)<extra></extra>")) %>%
    layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "€"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Tableau comparatif (ventes vs objectif + compta).
table_comparaison_aff <- function(comp, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(comp) || nrow(comp) == 0)
    return(tibble(Période = character()))
  comp %>%
    arrange(desc(PERIODE)) %>%
    transmute(Période       = label_periode(PERIODE, unite),
              `CA (HTVA)`   = format_CA(CA, -1),
              Objectif      = format_CA(OBJECTIF, -1),
              `% obj.`      = ifelse(is.na(PCT_OBJ), "—", paste0(PCT_OBJ, " %")),
              `Food cost`   = ifelse(is.na(FOOD_PCT), "—", paste0(FOOD_PCT, " %")),
              `Work cost`   = ifelse(is.na(WORK_PCT), "—", paste0(WORK_PCT, " %")),
              `Prime cost`  = ifelse(is.na(PRIME_PCT), "—", paste0(PRIME_PCT, " %")),
              Marge         = format_CA(MARGE, -1),
              `Marge %`     = ifelse(is.na(MARGE_PCT), "—", paste0(MARGE_PCT, " %")))
}


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
      # VAL = replace_na(VAL, 0), VAL_M1 = replace_na(VAL_M1, 0),
           ECART = replace_na(cumsum(VAL - VAL_M1), 0),
           LABEL = paste0(format(DATE, "%d/%m/%Y"),
                          "<br>", nom, " : ", format_CA(VAL, -1),
                          "<br>", nom, " N-1 : ", format_CA(VAL_M1, -1),
                          "<br><b>Écart cumulé : ", format_CA(ECART, -1), "</b>"))

  graph_ecart_cumule(dat, titre, paste("Écart", nom, "vs N-1"))
}

# Tuiles de synthèse annuelle « à date ».
kpi_annee_tiles <- function(serie, serie_m1) {
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
    kpi_tile(format_CA(ca, -1), "CA à date", "#2e7d32", "euro-sign"),
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

# Jours de Pizzwanze : mardi soir où l'on a vendu des pizzas.
jours_pizzwanze <- function(db_produits) {
  db_produits %>%
    filter(str_detect(toupper(PRODUCT), "PIZZ"),
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
ca_par_creneau <- function(db_produits, d1 = NULL, d2 = NULL) {
  piz <- jours_pizzwanze(db_produits)
  db <- db_produits
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
base_travail <- function(db_produits, db_travail, d1, d2) {
  d1 <- as.Date(d1); d2 <- as.Date(d2)
  piz <- jours_pizzwanze(db_produits)
  
  # Ne prendre les jours de db_produits que pour les jours de db_travail connu
  db_produits <- db_produits |> 
    filter(DATE %in% db_travail$DATE)

  ca <- ca_par_creneau(db_produits, d1, d2)

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

##### Tuiles KPI #####

kpi_travail_tiles <- function(ag) {
  ca    <- sum(ag$CA, na.rm = TRUE)
  hs    <- sum(ag$H_SERVICE, na.rm = TRUE)
  ht    <- sum(ag$H_TOTAL, na.rm = TRUE)
  cs    <- sum(ag$COUT_SERVICE, na.rm = TRUE)
  ct    <- sum(ag$COUT_TOTAL, na.rm = TRUE)
  marge <- ca - ct
  cah   <- if (hs > 0) ca / hs else NA_real_

  div(
    class = "kpi-grid",
    kpi_tile(if (is.na(cah)) "—" else format_CA(cah, -1), "CA par heure de service",
             couleur_seuil_haut(cah, 90, 70), "gauge-high",
             sous_titre = paste0(format(round(hs)), " h de service")),
    kpi_tile(format(round(ht)), "Heures totales", "#8d7b68", "clock",
             sous_titre = paste0(round(ratio_pct(hs, ht)), " % en service")),
    kpi_tile(format_CA(cs, -1), "Coût de service", COUL_TRAVAIL, "person-running",
             sous_titre = format_pct(ratio_pct(cs, ca))),
    kpi_tile(format_CA(ct - cs, -1), "Coûts indirects", "#8d7b68", "people-roof",
             sous_titre = format_pct(ratio_pct(ct - cs, ca))),
    kpi_tile(format_pct(ratio_pct(ct, ca)), "Coût du travail / CA",
             couleur_seuil(ratio_pct(ct, ca), 35, 45), "scale-balanced",
             sous_titre = format_CA(ct, -1)),
    kpi_tile(format_CA(marge, -1), "Marge après travail",
             if (marge >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(ratio_pct(marge, ca)))
  )
}

##### Graphiques — suivi dans le temps #####

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

#### REFONTE — Volet "Consommation" (bières & focaccias) ####
# Deux volets de suivi de la consommation, à la maille SEMAINE, toujours
# comparés à la semaine précédente (S-1).
#
# Sources :
#   DB_TICKET   : une ligne par produit vendu, avec TIMESTAMP (donc l'heure),
#                 BOISSON et VOLUME_TOT_L -> analyse horaire et en litres.
#                 Attention : DATE est le JOUR DE SERVICE (une vente à 1h du
#                 matin est rattachée à la soirée de la veille).
#   DB_PRODUITS : une ligne par (jour, produit) avec PRODUCT_FULL complet,
#                 options comprises -> seule source qui porte les suppléments
#                 des focaccias. Peut contenir plusieurs lignes par jour et
#                 produit : toujours agréger.

# Heures de service, dans l'ordre d'une soirée (on ouvre le matin, on ferme
# après minuit) plutôt que dans l'ordre naturel 0..23.
# Palette locale (functions.R ne dépend pas de ui.R)
CONSO_BRUN  <- "#732c02"
CONSO_AMBRE <- "#d98236"

ORDRE_HEURES_SERVICE <- c(6:23, 0:5)

heure_service <- function(ts) {
  factor(hour(ts), levels = ORDRE_HEURES_SERVICE,
         labels = paste0(ORDRE_HEURES_SERVICE, "h"))
}

# Semaines (lundi) disponibles dans une table, de la plus récente à la plus
# ancienne. `complete_only` retire la semaine en cours, forcément partielle.
semaines_dispo <- function(db, col = "DATE", complete_only = TRUE) {
  s <- db %>%
    mutate(SEM = floor_date(.data[[col]], "week", week_start = 1)) %>%
    distinct(SEM) %>%
    filter(!is.na(SEM)) %>%
    arrange(desc(SEM)) %>%
    pull(SEM)
  if (complete_only) s <- s[s < floor_date(today(), "week", week_start = 1)]
  s
}

# Tuile d'évolution : valeur de la semaine + écart en % vs S-1.
# `sens_positif = FALSE` quand une hausse est une mauvaise nouvelle.
tuile_evolution <- function(valeur, reference, libelle, icone,
                            format_val = function(x) format(round(x)),
                            sens_positif = TRUE, suffixe = "vs S-1") {
  evo <- if (is.na(reference) || reference == 0) NA_real_
         else 100 * (valeur - reference) / reference
  couleur <- if (is.na(evo)) "#8d7b68"
             else if ((evo >= 0) == sens_positif) COUL_VERT else COUL_ROUGE
  sous <- if (is.na(evo)) paste("pas de référence", suffixe)
          else paste0(if (evo >= 0) "+" else "", round(evo, 1), " % ", suffixe)
  kpi_tile(format_val(valeur), libelle, couleur, icone, sous_titre = sous)
}

tuile_ecart <- function(valeur, reference, libelle, icone,
                        format_val = function(x) format(round(x)),
                        sens_positif = TRUE, suffixe = "vs S-1") {
  ecart <- if (is.na(reference) || reference == 0) NA_real_
  else valeur - reference
  couleur <- if (is.na(ecart)) "#8d7b68"
  else if ((ecart >= 0) == sens_positif) COUL_VERT else COUL_ROUGE
  sous <- if (is.na(ecart)) paste("pas de référence", suffixe)
  else if (ecart == 0) paste0("identique ",suffixe)
  else paste0(if (ecart >= 0) "+" else "", round(ecart, 0), " ", suffixe)
  kpi_tile(format_val(valeur), libelle, couleur, icone, sous_titre = sous)
}


##### Bières — consommation #####

# Référentiel des vraies bières (catégories BIÈRES / ANCIENNES BIÈRES), pour
# écarter les autres boissons volumétriques (limonade, kéfir, cola, cidre...).
ref_bieres <- function(db_produits) {
  db_produits %>%
    filter(est_biere(CATEGORY), !is.na(BOISSON), BOISSON != "") %>%
    distinct(BOISSON) %>%
    pull(BOISSON)
}

# Lignes de ticket correspondant à des bières, sur une fenêtre de dates.
tickets_bieres <- function(db_ticket, ref, d1, d2) {
  db_ticket %>%
    filter(BOISSON %in% ref, DATE >= as.Date(d1), DATE <= as.Date(d2),
           QUANTITE > 0) %>%
    mutate(LITRES = replace_na(VOLUME_TOT_L, 0),
           HEURE  = heure_service(TIMESTAMP))
}

# Consommation par bière sur une fenêtre : verres, litres, CA.
conso_bieres <- function(db_ticket, ref, d1, d2) {
  tickets_bieres(db_ticket, ref, d1, d2) %>%
    group_by(BOISSON) %>%
    summarise(VERRES = sum(QUANTITE, na.rm = TRUE),
              LITRES = sum(LITRES, na.rm = TRUE),
              CA     = sum(PRIX_TOTAL, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(LITRES))
}

# Consommation d'une semaine, comparée à la semaine précédente.
conso_bieres_comparee <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  act <- conso_bieres(db_ticket, ref, semaine, semaine + 6)
  prec <- conso_bieres(db_ticket, ref, semaine - 7, semaine - 1) %>%
    rename(VERRES_M1 = VERRES, LITRES_M1 = LITRES, CA_M1 = CA)

  full_join(act, prec, by = "BOISSON") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)),
           DELTA_L   = LITRES - LITRES_M1,
           EVO_PCT   = ifelse(LITRES_M1 > 0,
                              round(100 * DELTA_L / LITRES_M1, 1), NA_real_),
           STATUT    = case_when(LITRES_M1 == 0 & LITRES > 0 ~ "Nouveauté",
                                 LITRES == 0 & LITRES_M1 > 0 ~ "Arrêtée",
                                 TRUE ~ "En cours")) %>%
    arrange(desc(LITRES))
}

# Litres par heure de service, semaine courante et S-1.
conso_bieres_horaire <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  par_heure <- function(d1, d2, nom) {
    tickets_bieres(db_ticket, ref, d1, d2) %>%
      group_by(HEURE) %>%
      summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
      mutate(PERIODE = nom)
  }
  bind_rows(par_heure(semaine, semaine + 6, "Semaine"),
            par_heure(semaine - 7, semaine - 1, "S-1")) %>%
    filter(!is.na(HEURE), LITRES > 0)
}

# Litres par jour de semaine et par heure (heatmap).
conso_bieres_jour_heure <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  tickets_bieres(db_ticket, ref, semaine, semaine + 6) %>%
    mutate(JOUR = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1)) %>%
    group_by(JOUR, HEURE) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(HEURE))
}

# Historique hebdomadaire des litres servis.
evo_conso_bieres <- function(db_ticket, ref, n_semaines = 26, fin = NULL) {
  fin <- if (is.null(fin)) max(db_ticket$DATE, na.rm = TRUE) else as.Date(fin)
  debut <- floor_date(fin, "week", week_start = 1) - weeks(n_semaines - 1)
  tickets_bieres(db_ticket, ref, debut, fin) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    group_by(SEMAINE) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE),
              VERRES = sum(QUANTITE, na.rm = TRUE),
              CA     = sum(PRIX_TOTAL, na.rm = TRUE),
              NB_BIERES = n_distinct(BOISSON), .groups = "drop") %>%
    arrange(SEMAINE)
}

# Trajectoire hebdomadaire des principales bières de la semaine choisie :
# permet de voir lesquelles montent, lesquelles s'essoufflent.
evo_top_bieres <- function(db_ticket, ref, semaine, n_top = 5, n_semaines = 12) {
  semaine <- as.Date(semaine)
  top <- conso_bieres(db_ticket, ref, semaine, semaine + 6) %>%
    slice_head(n = n_top) %>%
    pull(BOISSON)
  if (length(top) == 0) return(tibble(SEMAINE = as.Date(character()),
                                      BOISSON = character(), LITRES = numeric()))

  debut <- semaine - weeks(n_semaines - 1)
  tickets_bieres(db_ticket, ref, debut, semaine + 6) %>%
    filter(BOISSON %in% top) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    group_by(SEMAINE, BOISSON) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    complete(SEMAINE, BOISSON, fill = list(LITRES = 0)) %>%
    mutate(BOISSON = factor(BOISSON, levels = top)) %>%
    arrange(BOISSON, SEMAINE)
}

# Répartition des formats servis (33 cl, 50 cl, dégustation...).
formats_bieres <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  tickets_bieres(db_ticket, ref, semaine, semaine + 6) %>%
    filter(!is.na(VOLUME_CL)) %>%
    group_by(FORMAT = paste0(VOLUME_CL, " cl")) %>%
    summarise(VERRES = sum(QUANTITE, na.rm = TRUE),
              LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(VERRES))
}

kpi_bieres_tiles <- function(comp, formats, horaire = NULL) {
  litres  <- sum(comp$LITRES);    litres_m1 <- sum(comp$LITRES_M1)
  verres  <- sum(comp$VERRES);    verres_m1 <- sum(comp$VERRES_M1)
  ca      <- sum(comp$CA);        ca_m1     <- sum(comp$CA_M1)
  nb      <- sum(comp$LITRES > 0); nb_m1    <- sum(comp$LITRES_M1 > 0)
  tanker  <- litres / 500   # un tanker = 500 L

  # Heure de plus forte consommation sur la semaine
  pic <- NULL
  if (!is.null(horaire) && nrow(horaire) > 0) {
    h <- horaire %>% filter(PERIODE == "Semaine") %>% slice_max(LITRES, n = 1,
                                                                with_ties = FALSE)
    if (nrow(h) == 1) pic <- h
  }

  div(
    class = "kpi-grid",
    tuile_evolution(litres, litres_m1, "Litres servis", "beer-mug-empty",
                    function(x) paste0(format(round(x)), " L")),
    tuile_evolution(verres, verres_m1, "Verres servis", "wine-glass"),
    tuile_evolution(ca, ca_m1, "CA bières", "euro-sign",
                    function(x) format_CA(x, -1)),
    tuile_ecart(nb, nb_m1, "Bières différentes", "list-ul"),
    kpi_tile(paste0(round(tanker, 2)), "Équivalent tanker (500 L)", CONSO_BRUN,
             "boxes-stacked", sous_titre = paste0(round(litres / 7), " L / jour")),
    kpi_tile(if (is.null(pic)) "—" else as.character(pic$HEURE),
             "Pic de consommation", "#8d7b68", "clock",
             sous_titre = if (is.null(pic)) NULL
                          else paste0(round(pic$LITRES), " L sur la semaine"))
  )
}

# Top bières par litres, colorées selon l'évolution vs S-1.
graph_top_bieres <- function(comp, n = 12) {
  if (is.null(comp) || nrow(comp) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière servie"))

  dat <- comp %>% filter(LITRES > 0) %>% slice_head(n = n) %>% arrange(LITRES)
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière servie"))
  dat <- dat %>% mutate(BOISSON = factor(BOISSON, levels = BOISSON))

  plot_ly(dat) %>%
    add_bars(y = ~BOISSON, x = ~LITRES_M1, orientation = "h", name = "S-1",
             marker = list(color = "#d3c0ac"),
             hovertemplate = ~paste0(BOISSON, " (S-1)<br>", round(LITRES_M1),
                                     " L<extra></extra>")) %>%
    add_bars(y = ~BOISSON, x = ~LITRES, orientation = "h", name = "Semaine",
             marker = list(color = CONSO_BRUN),
             hovertemplate = ~paste0(BOISSON, "<br>", round(LITRES), " L — ",
                                     VERRES, " verres<extra></extra>")) %>%
    layout(barmode = "group", xaxis = list(title = "Litres"),
           yaxis = list(title = ""), legend = list(orientation = "h"),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Trajectoire des principales bières : une ligne par bière, litres par semaine.
# La semaine analysée est marquée d'un point, pour situer le contexte.
graph_tendance_bieres <- function(evo, semaine = NULL) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière servie"))

  bieres <- levels(droplevels(evo$BOISSON))
  pal <- setNames(
    grDevices::colorRampPalette(
      c(CONSO_BRUN, CONSO_AMBRE, "#5B7B5A", "#2980b9", "#9b59b6"))(length(bieres)),
    bieres)

  p <- plot_ly()
  for (b in bieres) {
    sub <- evo %>% filter(BOISSON == b) %>% arrange(SEMAINE)
    # Le hover lit la COLONNE BOISSON, pas la variable de boucle `b` : dans une
    # formule (~), l'expression est évaluée après la boucle, si bien que toutes
    # les traces afficheraient le nom de la dernière bière.
    p <- p %>% add_lines(
      data = sub, x = ~SEMAINE, y = ~LITRES, name = b, legendgroup = b,
      line = list(color = pal[[b]], width = 2.5),
      hovertemplate = ~paste0(BOISSON, "<br>Sem. ", format(SEMAINE, "%d/%m"),
                              "<br>", round(LITRES), " L<extra></extra>"))
    if (!is.null(semaine)) {
      pt <- sub %>% filter(SEMAINE == as.Date(semaine))
      if (nrow(pt) > 0)
        p <- p %>% add_markers(data = pt, x = ~SEMAINE, y = ~LITRES,
                               name = b, legendgroup = b, showlegend = FALSE,
                               marker = list(color = pal[[b]], size = 9),
                               hoverinfo = "skip")
    }
  }
  p %>% layout(xaxis = list(title = ""),
               yaxis = list(title = "Litres par semaine", rangemode = "tozero"),
               legend = list(orientation = "h"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Heatmap jour x heure des litres servis.
graph_heatmap_bieres <- function(jh) {
  if (is.null(jh) || nrow(jh) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  # pivot_wider ordonne les colonnes par ordre d'APPARITION dans les données
  # (18h, 19h, ... puis 11h). On complète la grille puis on resélectionne les
  # colonnes dans l'ordre des niveaux du facteur (heure de service).
  jh <- jh %>%
    mutate(HEURE = droplevels(HEURE)) %>%
    complete(JOUR, HEURE, fill = list(LITRES = 0))
  heures <- levels(jh$HEURE)

  mat <- jh %>%
    pivot_wider(names_from = HEURE, values_from = LITRES, values_fill = 0) %>%
    arrange(JOUR)
  z <- as.matrix(mat[, heures, drop = FALSE])

  plot_ly(x = heures, y = as.character(mat$JOUR), z = z, type = "heatmap",
          colorscale = list(c(0, "#f2efe6"), c(1, CONSO_BRUN)),
          hovertemplate = "%{y} — %{x}<br>%{z:.0f} L<extra></extra>") %>%
    layout(xaxis = list(title = "", side = "top"),
           yaxis = list(title = "", autorange = "reversed"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Historique hebdomadaire : litres en barres, nombre de bières en ligne.
graph_evo_conso_bieres <- function(evo, semaine = NULL) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  couleurs <- if (is.null(semaine)) CONSO_BRUN
              else ifelse(evo$SEMAINE == as.Date(semaine), CONSO_AMBRE, CONSO_BRUN)

  plot_ly(evo) %>%
    add_bars(x = ~SEMAINE, y = ~LITRES, name = "Litres",
             marker = list(color = couleurs),
             hovertemplate = ~paste0("Sem. ", format(SEMAINE, "%d/%m/%y"), "<br>",
                                     round(LITRES), " L — ", VERRES,
                                     " verres<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~NB_BIERES, name = "Bières à la carte",
              yaxis = "y2", line = list(color = "#5B7B5A", width = 2),
              hovertemplate = ~paste0(NB_BIERES, " bières<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "Litres"),
           yaxis2 = list(title = "Nb de bières", overlaying = "y", side = "right",
                         showgrid = FALSE, rangemode = "tozero"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Formats servis (33 cl, 50 cl, dégustation...).
graph_formats_bieres <- function(formats) {
  if (is.null(formats) || nrow(formats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  dat <- formats %>% arrange(VERRES) %>%
    mutate(FORMAT = factor(FORMAT, levels = FORMAT))
  plot_ly(dat) %>%
    add_bars(y = ~FORMAT, x = ~VERRES, orientation = "h",
             marker = list(color = CONSO_AMBRE),
             hovertemplate = ~paste0(FORMAT, "<br>", VERRES, " verres — ",
                                     round(LITRES), " L<extra></extra>")) %>%
    layout(xaxis = list(title = "Verres servis"), yaxis = list(title = ""),
           showlegend = FALSE, margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

table_conso_bieres <- function(comp) {
  if (is.null(comp) || nrow(comp) == 0) return(tibble(Bière = character()))

  # Totaux calculés hors du transmute : une condition scalaire dans un
  # `ifelse` renverrait une valeur unique, recyclée sur toutes les lignes.
  total    <- sum(comp$LITRES, na.rm = TRUE)
  total_m1 <- sum(comp$LITRES_M1, na.rm = TRUE)

  comp %>%
    mutate(PART    = if (total > 0) 100 * LITRES / total else NA_real_,
           PART_M1 = if (total_m1 > 0) 100 * LITRES_M1 / total_m1 else NA_real_) %>%
    transmute(Bière      = BOISSON,
              Verres     = VERRES,
              Litres     = round(LITRES),
              Part       = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              `CA`       = format_CA(CA, -1),
              `Litres S-1` = round(LITRES_M1),
              `Part S-1` = ifelse(is.na(PART_M1), "—",
                                  paste0(round(PART_M1, 1), " %")),
              `Évol.`    = ifelse(is.na(EVO_PCT), "—",
                                  paste0(ifelse(EVO_PCT >= 0, "+", ""),
                                         EVO_PCT, " %")),
              Statut     = STATUT)
}


##### Focaccias #####

# Décompose un PRODUCT_FULL de focaccia en base + options.
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
conso_focaccias <- function(db_produits, d1, d2) {
  db <- db_produits %>%
    filter(str_detect(tolower(PRODUCT_FULL), "focaccia"),
           !str_detect(tolower(PRODUCT_FULL), "discount|% sur produit"),
           QUANTITE > 0, DATE >= as.Date(d1), DATE <= as.Date(d2))
  if (nrow(db) == 0)
    return(tibble(DATE = as.Date(character()), BASE = character(),
                  FROMAGE = logical(), VIANDE = logical(), SPICY = logical(),
                  GARNITURE = character(), VARIANTE = character(),
                  QUANTITE = numeric(), CA = numeric()))
  bind_cols(db %>% select(DATE, QUANTITE, CA = CA_HTVA),
            parse_focaccia(db$PRODUCT_FULL)) %>%
    mutate(GARNITURE = factor(GARNITURE, levels = ORDRE_GARNITURES))
}

# Synthèse d'une semaine, avec la semaine précédente pour comparaison.
focaccias_semaine <- function(db_produits, semaine) {
  semaine <- as.Date(semaine)
  list(semaine = semaine,
       act  = conso_focaccias(db_produits, semaine, semaine + 6),
       prec = conso_focaccias(db_produits, semaine - 7, semaine - 1))
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
evo_focaccias <- function(db_produits, n_semaines = 26, fin = NULL) {
  fin <- if (is.null(fin)) max(db_produits$DATE, na.rm = TRUE) else as.Date(fin)
  debut <- floor_date(fin, "week", week_start = 1) - weeks(n_semaines - 1)
  # Les quantités par option sont calculées AVANT le regroupement : dans un
  # summarise(), `QUANTITE = sum(QUANTITE)` écrase la colonne, et un
  # `QUANTITE[FROMAGE]` écrit ensuite indexerait le total (scalaire) au lieu
  # des lignes — ce qui ne produit que des NA.
  conso_focaccias(db_produits, debut, fin) %>%
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

kpi_focaccias_tiles <- function(fs) {
  act <- fs$act; prec <- fs$prec
  q  <- sum(act$QUANTITE);  q_m1  <- sum(prec$QUANTITE)
  ca <- sum(act$CA);        ca_m1 <- sum(prec$CA)
  pct <- function(d, col) if (sum(d$QUANTITE) > 0)
    100 * sum(d$QUANTITE[d[[col]]]) / sum(d$QUANTITE) else NA_real_
  jours_ouverts <- n_distinct(act$DATE)

  div(
    class = "kpi-grid",
    tuile_evolution(q, q_m1, "Focaccias vendues", "bread-slice"),
    tuile_evolution(ca, ca_m1, "CA focaccias", "euro-sign",
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
table_focaccias <- function(fs) {
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
              `Part`     = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              `CA`       = format_CA(CA, -1),
              `Qté S-1`  = Q_M1,
              `Évol.`    = ifelse(is.na(EVO), "—",
                                  paste0(ifelse(EVO >= 0, "+", ""), EVO, " %")))
}

##### Focaccias — aide à la production #####
# Combien produire de chaque préparation pour la semaine à venir ?
#
# Assiette de chaque ingrédient (quelles focaccias le consomment) :
#   Crémeux et Légume -> TOUTES les focaccias, ils sont dans la recette de base
#   Fromage           -> celles qui portent le supplément fromage (« full »
#                        compris, puisqu'une focaccia complète en contient)
#   Viande            -> idem avec le supplément viande
#   Autre             -> ligne libre, rien n'est préchargé
#
# NB : la caisse ne connaît pas de « supplément légume » — les seules options
# sont Fromage, Viande et Spicy. Le légume est donc traité comme le crémeux,
# c'est-à-dire présent dans toutes les recettes.

INGREDIENTS_FOCACCIA <- tibble::tribble(
  ~ID, ~NOM,       ~ASSIETTE,  ~PORTION,
  1L,  "Crémeux",  "toutes",   40,
  2L,  "Légume",   "toutes",   60,
  3L,  "Fromage",  "fromage",  30,
  4L,  "Viande",   "viande",   50,
  5L,  "Autre",    NA,         NA
)

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
production_focaccias_base <- function(db_produits, n_semaines = 3, fin = NULL) {
  base <- INGREDIENTS_FOCACCIA %>% mutate(FOCACCIAS = NA_real_, SEMAINES = 0L)
  if (is.null(db_produits) || nrow(db_produits) == 0) return(base)

  fin_donnees <- if (is.null(fin)) max(db_produits$DATE, na.rm = TRUE)
                 else as.Date(fin)
  if (!is.finite(fin_donnees)) return(base)

  fo_all <- conso_focaccias(db_produits, as.Date("1900-01-01"), fin_donnees)
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
      ASSIETTE == "toutes" ~ round(max_toutes * 1.1),
      ASSIETTE == "fromage" ~ round(max_fromage * 1.1),
      ASSIETTE == "viande" ~ round(max_viande * 1.1),
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

# Seuils de détection d'une soirée (documentés pour pouvoir être ajustés).
PIZZWANZE_MIN_REFS   <- 2   # une soirée propose une carte, pas un seul produit
PIZZWANZE_MIN_PIZZAS <- 5   # garde-fou volume, pour écarter les restes

# Dates des soirées Pizzwanze, de la plus ancienne à la plus récente.
soirees_pizzwanze <- function(db_produits,
                              min_refs = PIZZWANZE_MIN_REFS,
                              min_pizzas = PIZZWANZE_MIN_PIZZAS) {
  db_produits %>%
    filter(est_pizza(PRODUCT_FULL), QUANTITE > 0) %>%
    filter(!str_detect(PRODUCT_FULL,"Slice")) |> 
    group_by(DATE) %>%
    summarise(PIZZAS = sum(QUANTITE, na.rm = TRUE),
              N_REF  = n_distinct(PRODUCT_FULL), .groups = "drop") %>%
    filter(N_REF >= min_refs, PIZZAS >= min_pizzas) %>%
    arrange(DATE) %>%
    pull(DATE)
}

# Ventes de pizzas d'une ou plusieurs soirées, agrégées par date et produit.
conso_pizzas <- function(db_produits, dates) {
  dates <- as.Date(dates)
  db_produits %>%
    filter(est_pizza(PRODUCT_FULL), QUANTITE > 0, DATE %in% dates) %>%
    group_by(DATE, PIZZA = PRODUCT_FULL) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA       = sum(CA_HTVA, na.rm = TRUE), .groups = "drop")
}

# Statut de chaque pizza sur l'ensemble des soirées : incontournable, régulière
# ou occasionnelle, avec ses dates de première et dernière apparition.
statut_pizzas <- function(db_produits, soirees) {
  n_soirees <- length(soirees)
  if (n_soirees == 0)
    return(tibble(PIZZA = character(), N_SOIREES = integer(),
                  PREMIERE = as.Date(character()), DERNIERE = as.Date(character()),
                  QUANTITE = numeric(), STATUT = character()))

  conso_pizzas(db_produits, soirees) %>%
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
pizzwanze_soiree <- function(db_produits, db_ticket, date_soiree, soirees = NULL) {
  date_soiree <- as.Date(date_soiree)
  if (is.null(soirees)) soirees <- soirees_pizzwanze(db_produits)

  precedente <- soirees[soirees < date_soiree]
  precedente <- if (length(precedente) == 0) NA else max(precedente)
  
  pic <- pizzas_par_heure(db_ticket, date_soiree) |> 
    arrange(-QUANTITE) |> filter(row_number() == 1) |> pull(HEURE)

  statuts <- statut_pizzas(db_produits, soirees)

  act <- conso_pizzas(db_produits, date_soiree) %>%
    left_join(statuts %>% select(PIZZA, N_SOIREES, PREMIERE, STATUT), by = "PIZZA") %>%
    mutate(NOUVEAUTE = !is.na(PREMIERE) & PREMIERE == date_soiree,
           STATUT_SOIR = ifelse(NOUVEAUTE, "Nouveauté", STATUT)) %>%
    arrange(desc(QUANTITE))

  prec <- if (is.na(precedente)) act[0, c("PIZZA", "QUANTITE", "CA")]
          else conso_pizzas(db_produits, precedente) %>% select(PIZZA, QUANTITE, CA)

  list(date = date_soiree,
       precedente = precedente,
       ecart_jours = if (is.na(precedente)) NA_real_
                     else as.numeric(date_soiree - precedente),
       act = act, prec = prec, statuts = statuts, pic = pic)
}

# Historique : une ligne par soirée, avec le nombre de nouveautés et l'écart
# depuis la soirée précédente.
historique_pizzwanze <- function(db_produits, soirees = NULL) {
  if (is.null(soirees)) soirees <- soirees_pizzwanze(db_produits)
  if (length(soirees) == 0)
    return(tibble(DATE = as.Date(character()), PIZZAS = numeric(),
                  CA = numeric(), N_REF = integer(), NOUVEAUTES = integer(),
                  ECART = numeric()))

  detail <- conso_pizzas(db_produits, soirees)
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
    filter(est_pizza(PRODUCT_FULL), DATE == date_soiree, QUANTITE > 0) %>%
    mutate(HEURE = heure_service(TIMESTAMP)) %>%
    group_by(HEURE) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(HEURE), QUANTITE > 0)
}

##### Tuiles KPI #####

kpi_pizzwanze_tiles <- function(ps) {
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
    tuile_evolution(ca, ca_m1, "CA pizzas", "euro-sign",
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

##### Graphiques #####

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
graph_carte_pizzwanze <- function(db_produits, soirees, n_soirees = NULL) {
  if (length(soirees) == 0)
    return(plotly_empty() %>% layout(title = "Aucune soirée détectée"))

  # n_soirees = NULL -> tout l'historique. C'est le réglage le plus parlant :
  # la carte s'est stabilisée sur les dernières soirées, les allées et venues
  # de pizzas ne se voient qu'en remontant plus loin.
  dernieres <- sort(soirees)
  if (!is.null(n_soirees)) dernieres <- tail(dernieres, n_soirees)
  detail <- conso_pizzas(db_produits, dernieres)
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
table_pizzwanze <- function(ps) {
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
    transmute(Pizza      = PIZZA,
              Quantité   = QUANTITE,
              Part       = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              CA         = format_CA(CA, -1),
              Statut     = STATUT_SOIR,
              `Soirées`  = N_SOIREES,
              `Qté préc.` = Q_M1,
              `Évol.`    = ifelse(is.na(EVO), "—",
                                  paste0(ifelse(EVO >= 0, "+", ""), EVO, " %")))
}

#### Générique ####

# Bandeau d'avertissement, à utiliser dans un renderUI.
# Ne renvoie quelque chose que si `afficher` est vrai ; sinon NULL, donc rien
# ne s'affiche et l'espace n'est pas réservé.
bandeau_alerte <- function(afficher, texte,
                           titre   = "À lire attentivement",
                           couleur = COUL_ROUGE,
                           icone   = "triangle-exclamation") {
  if (!isTRUE(afficher)) return(NULL)
  
  div(
    class = "d-flex align-items-start gap-2", role = "alert",
    style = paste0("background:", couleur, "1a;",
                   "border-left:4px solid ", couleur, ";",
                   "border-radius:0.5rem;padding:0.7rem 0.9rem;",
                   "margin-bottom:0.9rem;"),
    span(style = paste0("color:", couleur, ";font-size:1.15rem;line-height:1.2;"),
         icon(icone)),
    div(
      div(style = paste0("font-weight:700;color:", couleur, ";"), titre),
      div(class = "small", texte)
    )
  )
}

datatable_simple <- function(table){
  datatable(
    table,
    options = list(
      dom = 't', # 't' pour "table" - affiche uniquement le tableau sans contrôles
      paging = FALSE, # Désactive la pagination
      ordering = FALSE, # Désactive le tri
      searching = FALSE # Désactive la recherche
    ),
    rownames= FALSE
  )
}

theme_mazette <- function(){
  theme(
    axis.title.x.top = element_text(margin = margin(b=10)),
    axis.text = element_text(face = "bold",size = 12),
    axis.title = element_text(face = "bold",size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
}

table_format <- function(table){
  table +
    scale_x_date(date_breaks = "1 week", position = "top",
                 labels = function(x) format(x, "%d/%m")) +
    theme_minimal()+
    theme(legend.position = "none",
          axis.title.y = element_blank())+
    theme_mazette()
}

label_specific <- function(jour,nb_jours){
  if (nb_jours == 0)
    paste0(format(jour,"%a %d/%m/%y"))
  else if (nb_jours == 7)
    paste0("Semaine du ",format(jour,"%d/%m/%y"))
  else if (nb_jours == 31)
    paste0("Mois du ",format(jour,"%d/%m/%y"))
  else
    paste0(format(jour,"%a %d/%m/%y")," -> ",format(jour+nb_jours,"%d/%m/%y"))
}

prepa_db <- function(DB,var_tva){
  DB_DATE %>%
    left_join(DB) %>%
    mutate_if(is.numeric,replace_na,0) %>%
    mutate_if(is.character,replace_na,"") %>%
    rename(ventes = var_tva)
}


##### Format #####

format_CA <- function(montant,nb_apres=0) {
  montant_formatte <- format(round(montant,nb_apres), big.mark = ".",
                               decimal.mark = ",", nsmall = max(nb_apres,0))
  montant_formatte <- paste0(montant_formatte, "€")

  montant_formatte[str_trim(montant_formatte) == "0€"] <- ""
  montant_formatte[montant_formatte == "€"] <- ""
  montant_formatte
}

format_x_date <- function(nb_dates){
  case_when(
    nb_dates > 365*5 ~ c("1 year","%Y"),
    nb_dates > 365 ~ c("6 months","%b %Y"),
    nb_dates > 150 ~ c("1 month","%b %Y"),
    TRUE ~ c("7 days","%d %b"))
}



##### Couleurs #####

get_color_from_gradient <- function(actual, goal) {
  percent_achieved <- actual / goal
  if (!is.nan(percent_achieved)){
    if (percent_achieved <= 0.9) { return("#FF0000") }
    else if (percent_achieved <= 1) { return("#FFA500") }
    else { return("#32CD32") }
  }else{ return("#FFFFFF") }
}

pal_col <- tibble(name = character(), col= character(),icon = character()) %>%
  add_row(name = "Boulangerie - Global", col = "#A14466",icon = "bread-slice") %>%
  add_row(name = "Boulangerie - Achats", col = "#D2B48C") %>%
  add_row(name = "Boulangerie - Travail", col = "#8B4513") %>%
  add_row(name = "Cuisine - Global", col = "#589441",icon = "fire-burner") %>%
  add_row(name = "Cuisine - Achats", col = "#9CAF88") %>%
  add_row(name = "Cuisine - Travail", col = "#556B2F") %>%
  add_row(name = "Service - Global", col = "#40E0F0",icon = "mug-saucer") %>%
  add_row(name = "Service - Achats", col = "#ADD8E6") %>%
  add_row(name = "Service - Travail", col = "#40E0D0") %>%
  add_row(name = "Brasserie - Global", col = "#E9BF00",icon = "wheat-awn") %>%
  add_row(name = "Brasserie - Achats", col = "#FFD700") %>%
  add_row(name = "Brasserie - Travail", col = "#FFBF00") %>%
  add_row(name = "Support - Global", col = "#8080E0",icon = "right-left") %>%
  add_row(name = "Support - Achats", col = "#D3D3D3",icon = "receipt") %>%
  add_row(name = "Support - Travail", col = "#696969") %>%
  add_row(name = "Nourriture - Global", col = "#27ae60",icon = "utensils") %>%
  add_row(name = "Nourriture - Achat", col = "#B7B18A",icon = "cart-shopping") %>%
  add_row(name = "Nourriture - Travail", col = "#705821",icon = "person-running") %>%
  add_row(name = "Boisson - Global", col = "#d4ac0d",icon = "beer-mug-empty") %>%
  add_row(name = "Boisson - Achat", col = "#9FCF68",icon = "cart-shopping") %>%
  add_row(name = "Boisson - Travail", col = "#D6D773",icon = "person-running") %>%
  add_row(name = "Global - Achat", col = "#C9C78F",icon = "cart-shopping") %>%
  add_row(name = "Global - Travail", col = "#AF4553",icon = "person-running") %>%
  add_row(name = "Midi - Global", col = "#e67e22") %>%
  add_row(name = "Soir - Global", col = "#9b59b6") %>%
  add_row(name = "Semaine - Global", col = "#2980b9") %>%
  add_row(name = "Week-end - Global", col = "#c0392b") %>%
  add_row(name = "Prime Cost / CA", col = "red",icon = "scale-balanced") %>%
  add_row(name = "Prime Cost", col = "#BAB86C",icon = "credit-card") %>%
  add_row(name = "CA HTVA", col = "green",icon = "euro-sign") %>%
  mutate(SECTEUR = str_extract(name,"^(.*?) - (.*?)$",group = 1),
         STEP = str_extract(name,"^(.*?) - (.*?)$",group = 2),
         CD_SECTEUR = case_when(
           SECTEUR == "Support" ~ "Support",
           SECTEUR %in% c("Boulangerie","Cuisine") ~ "Nourriture",
           SECTEUR %in% c("Brasserie","Service") ~ "Boisson",
           TRUE ~ SECTEUR))


#### Tableaux génériques ####

# Tableau produits
table_produits <- function(DB){

  TEST <- DB %>%
    group_by(DATE,JOUR_SEMAINE,PREMIER_JOUR_SEMAINE) %>%
    summarise(quantity = sum(QUANTITE,na.rm = TRUE),.groups = "drop")

  TOT <- TEST %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(quantity = sum(quantity),.groups = "drop") %>%
    mutate(JOUR_SEMAINE = "total")

  TEST <- TEST %>% add_row(TOT) %>%
    mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels=c(vecteur_jours,"total")))

  plot <- ggplot(TEST) +
    aes(x=PREMIER_JOUR_SEMAINE,y = JOUR_SEMAINE,label = quantity) +
    geom_label(label.padding = unit(0.65, "lines"),label.size = 0, size = 6) +
    geom_hline(yintercept=1.5,size = 1,linetype="dashed", color = "grey") +
    scale_y_discrete(limits = rev(levels(TEST$JOUR_SEMAINE))) +
    xlab(label="Premier jour de la semaine")

  table_format(plot)
}

# Tableau de ventes CA avec objectif

table_ventes <- function(DB_JOURS,DB_OBJECTIFS,date_debut,date_fin){

  TEST <- DB_JOURS %>%
    filter(DATE > date_debut & DATE < date_fin) %>%
    left_join(DB_OBJECTIFS %>%
                rename(objectif = ventes) %>%
                select(DATE,objectif)) %>%
    select(PREMIER_JOUR_SEMAINE,JOUR_SEMAINE,ventes,objectif)

  TOT <- TEST %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(ventes = sum(ventes),objectif = sum(objectif),.groups = "drop") %>%
    mutate(JOUR_SEMAINE = "total")

  TEST <- TEST %>% add_row(TOT) %>%
    mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels=c(vecteur_jours,"total")),
           ratio = ventes / objectif,
           SCORE = case_when(
             ventes == 0 ~ "rien",
             ratio < 0.9 ~ "bas",
             ratio < 1 ~ "moyen",
             TRUE ~ "haut")
    )

  plot <- ggplot(TEST) +
    aes(x=PREMIER_JOUR_SEMAINE,y = JOUR_SEMAINE,label = format_CA(ventes,-1)) +
    # geom_label(aes(fill = ventes >= objectif),
    geom_label(aes(fill = SCORE),
               label.padding = unit(0.65, "lines"),label.size = 0, size = 6) +
    geom_hline(yintercept=1.5,size = 1,linetype="dashed", color = "grey") +
    # scale_fill_manual(values = c(`TRUE` = "green3", `FALSE` = "red3")) +
    scale_fill_manual(values = c("rien" = "white","haut" = "green3",
                                 "moyen" = "yellow3", "bas" = "red3")) +
    scale_y_discrete(limits = rev(levels(TEST$JOUR_SEMAINE))) +
    xlab(label="Premier jour de la semaine")

  table_format(plot)
}

table_produits_mois <- function(db,debut_mois){
  DB <- DB_DATE %>% left_join(db) %>%
    filter(PREMIER_JOUR_MOIS == debut_mois) %>%
    group_by(PRODUCT) %>%
    summarise(CA = sum(CA_HTVA),.groups = "drop") %>% arrange(-CA) %>%
    mutate(CA = format_CA(CA,-1)) %>%
    mutate(PRODUCT = ifelse(nchar(PRODUCT) > 40,
                            paste0(substr(PRODUCT,1,40),"..."),PRODUCT)) %>%
    rename(`CA HTVA` = CA)

  DB
}

# Tableau de récapitulatif de coût

table_cout_secteurs <- function(DB){

  # Somme sur tous les mois
  SYN <- DB %>% group_by(SECTEUR,TYPE_COUT) %>%
    summarise(HEURES = sum(HEURES,na.rm = TRUE),
              COUT = sum(COUT,na.rm = TRUE),.groups = "drop")

  DETAILS <- SYN %>% filter(TYPE_COUT == "Travail") %>%
    mutate(TYPE_COUT = "Heures", COUT = HEURES) %>%
    add_row(SYN) %>% select(SECTEUR,TYPE_COUT,COUT) %>%
    pivot_wider(names_from = TYPE_COUT,values_from = COUT) %>%
    select(SECTEUR,Heures,Achat,Stock,Travail) %>%
    mutate(Total = Achat+Stock+Travail)

  TOTAL <- DETAILS %>%
    summarise(Heures=sum(Heures),Achat=sum(Achat),Stock=sum(Stock),
              Travail=sum(Travail),Total=sum(Total)) %>%
    mutate(SECTEUR = "Total")

  DETAILS %>% add_row(TOTAL) %>%
    mutate(Achat = format_CA(Achat,-1),
         Stock = format_CA(Stock,-1),
         Heures = paste0(round(Heures,0)),
         Travail = format_CA(Travail,-1),
         Total = format_CA(Total,-1))
}




# Tableau d'objectif de ventes

table_objectifs <- function(DB,date_debut,date_fin){

  TEST <- DB %>%
    filter(DATE >= date_debut & DATE <= date_fin) %>%
    rename(objectif = ventes) %>%
    select(PREMIER_JOUR_SEMAINE,objectif,JOUR_SEMAINE)

  TOT <- TEST %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(objectif = ceiling(sum(objectif)/100)*100,.groups = "drop") %>%
    mutate(JOUR_SEMAINE = "total")

  TEST <- TEST %>% add_row(TOT) %>%
    mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels=c(vecteur_jours,"total")))

  plot <- ggplot(TEST) +
    aes(x=PREMIER_JOUR_SEMAINE,y = JOUR_SEMAINE,label = format_CA(objectif,-1)) +
    geom_label(label.padding = unit(0.75, "lines"),label.size = 0, size = 6) +
    geom_hline(yintercept=1.5,size = 1,linetype="dashed", color = "grey") +
    scale_y_discrete(limits = rev(levels(TEST$JOUR_SEMAINE))) +
    xlab(" ") +
    ylab(" ")

  table_format(plot) +
    theme(axis.text.x = element_blank())
}


table_stats_ventes_LT <- function(DB_JOURS,var_group,classement=FALSE){

  if (var_group == "DATE"){
    TEST <- DB_JOURS %>%
      select(DATE,ventes) %>%
      filter(ventes > 0) %>%
      arrange(desc(ventes))
  }

  if (var_group == "MOIS"){
    TEST <- DB_JOURS %>%
      filter(format(DATE,"%m-%Y") != format(today(),"%m-%Y")) %>%
      select(DATE,ventes) %>%
      filter(ventes > 0) %>%
      group_by(MOIS = format(DATE,"%m-%Y")) %>%
      summarise(ventes = sum(ventes,na.rm = TRUE),NB_JOURS = n(),.groups = "drop") %>%
      mutate(ventes_mean = ventes / NB_JOURS) %>%
      arrange(desc(ventes))

    if (classement) TEST <- TEST %>% arrange(desc(ventes_mean))
    TEST <- TEST %>% select(-ventes_mean)

    # Quelques données aberrantes
    TEST <- TEST %>% filter(MOIS != "02-2022")
  }

  if (var_group == "PREMIER_JOUR_SEMAINE"){
    TEST <- DB_JOURS %>%
      filter(PREMIER_JOUR_SEMAINE != DB_JOURS %>%
               filter(DATE == today()) %>%
               pull(PREMIER_JOUR_SEMAINE) %>%
               unique()) %>%
      select(PREMIER_JOUR_SEMAINE,ventes) %>%
      filter(ventes > 0) %>%
      group_by(SEMAINE = paste(PREMIER_JOUR_SEMAINE,"->",
                               PREMIER_JOUR_SEMAINE+6)) %>%
      summarise(ventes = sum(ventes,na.rm = TRUE),NB_JOURS = n(),.groups = "drop") %>%
      mutate(ventes_mean = ventes / NB_JOURS) %>%
      arrange(desc(ventes))

    if (classement) TEST <- TEST %>% arrange(desc(ventes_mean))
    TEST <- TEST %>% select(-ventes_mean)

    # Quelques données aberrantes
    TEST <- TEST %>% filter(SEMAINE != "2023-01-02 -> 2023-01-08")
  }


  statistiques_detail <- tibble(Indicateur = c("Moyenne","Min",
                                               "Q1","Médiane","Q3","Max"),
                                ventes = c(
                                  mean(TEST$ventes),
                                  min(TEST$ventes),
                                  quantile(TEST$ventes,0.25),
                                  median(TEST$ventes),
                                  quantile(TEST$ventes,0.75),
                                  max(TEST$ventes)))

  if (var_group == "DATE"){
    statistiques_detail <- statistiques_detail %>%
      mutate(ventes = format_CA(ventes))

    TEST <- TEST %>% mutate(ventes = format_CA(ventes))
  }else{
    statistiques_detail <- statistiques_detail %>%
      mutate(ventes = format_CA(ventes,-2))

    TEST <- TEST %>% mutate(ventes = format_CA(ventes,-2))
  }

  meilleures <- TEST %>% slice_head(n = 6)
  pires <- TEST %>% slice_tail(n = 6)

  list(statistiques_detail,meilleures,pires)
}


# préparation de Comparaison entre trimestre

table_resume_mois <- function(DB_JOURS,date){

  DB_JOURS <- DB_JOURS %>%
    select(DATE,ANNEE_MOIS,ANNEE_SEMAINE,JOUR_SEMAINE,
           PREMIER_JOUR_MOIS,PREMIER_JOUR_SEMAINE,ventes,
           NB_CLIENTS,NB_TABLES) %>%
    filter(DATE < today() & ventes > 0)

  mois <- month(date)
  annee <- year(date)
  trimestre <- quarters(as.Date(date))

  PREPA_MOIS <- DB_JOURS %>% filter(month(DATE) == mois & year(DATE) == annee)
  PREPA_TRIM <- DB_JOURS %>% filter(quarters(DATE) == trimestre & year(DATE) == annee)

  TRAVAIL <- COUT_TRAVAIL %>%
    group_by(PREMIER_JOUR_MOIS) %>%
    summarise(HEURES = sum(HEURES),
              Travail = sum(Travail),.groups = "drop") %>%
    filter(month(PREMIER_JOUR_MOIS) == mois & year(PREMIER_JOUR_MOIS) == annee)

  LISTE_premier_jour <- DB_JOURS %>%
    filter(month(PREMIER_JOUR_SEMAINE) == mois
           & year(PREMIER_JOUR_SEMAINE) == annee) %>%
    pull(PREMIER_JOUR_SEMAINE) %>% unique()

  liste_premier_jour_today <- DB_JOURS %>% filter(DATE == max(DATE)) %>%
    pull(PREMIER_JOUR_SEMAINE)

  SUB_SEMAINE <- DB_JOURS %>%
    filter(PREMIER_JOUR_SEMAINE %in% LISTE_premier_jour &
             PREMIER_JOUR_SEMAINE != liste_premier_jour_today) %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(ventes = sum(ventes,na.rm = TRUE),.groups = "drop")

  nb_jours <- PREPA_MOIS %>% select(DATE) %>% pull() %>% unique() %>% length()

  CA_mois_sum <- PREPA_MOIS %>% summarise(sum(ventes,na.rm = TRUE)) %>% pull()
  CA_mois_mean <- PREPA_MOIS %>% summarise(mean(ventes,na.rm = TRUE)) %>% pull()
  CA_semaine_max <- max(SUB_SEMAINE$ventes)
  CA_semaine_min <- min(SUB_SEMAINE$ventes)
  CA_jour_max <- PREPA_MOIS %>% summarise(max(ventes)) %>% pull()
  CA_jour_min <- PREPA_MOIS %>% filter(ventes > 0) %>% summarise(min(ventes)) %>% pull()
  CA_trim <- PREPA_TRIM %>% summarise(sum(ventes,na.rm = TRUE)) %>% pull()

  nb_heures <- round(TRAVAIL$HEURES)
  cout_travail <- TRAVAIL$Travail

  CA_heures <- CA_mois_sum/nb_heures
  CA_heures <- ifelse(is.infinite(CA_heures),0,CA_heures)

  if (length(nb_heures) == 0) nb_heures <- ""

  # Répartition des CA
  PC_CA <- DB_DATE %>%
    left_join(DB_KPI_SIMPLE) %>%
    filter(month(DATE) == mois & year(DATE) == annee) %>%
    group_by(PREMIER_JOUR_MOIS) %>%
    summarise(nb_jours = n(),Jour=sum(Jour,na.rm=TRUE),Semaine =sum(Semaine,na.rm=TRUE),
              Boisson=sum(Boisson,na.rm=TRUE),CA_HTVA=sum(CA_HTVA,na.rm=TRUE))

  PC_jours_soir <- round(100*PC_CA$Jour/PC_CA$CA_HTVA,0)
  PC_jours_soir <- paste0(PC_jours_soir,"% / ",100 - PC_jours_soir,"%")

  PC_semaine_we <- round(100*PC_CA$Semaine/PC_CA$CA_HTVA,0)
  PC_semaine_we <- paste0(PC_semaine_we,"% / ",100 - PC_semaine_we,"%")

  PC_boi_nourr <- round(100*PC_CA$Boisson/PC_CA$CA_HTVA,0)
  PC_boi_nourr <- paste0(PC_boi_nourr,"% / ",100 - PC_boi_nourr,"%")

  if (PC_CA$Jour + PC_CA$Semaine + PC_CA$Boisson == 0){
    PC_jours_soir <- ""
    PC_semaine_we <- ""
    PC_boi_nourr <- ""
  }

  SYNTHESE <- tibble(
    `Nombre de jours de travail` = as.character(nb_jours),
    `Nombre d'heures de travail` = as.character(nb_heures),
    `Coût du travail` = format_CA(cout_travail,-1),
    `Total CA` = format_CA(CA_mois_sum,-2),
    `CA Semaine / Week-end` = PC_semaine_we,
    `CA Jour / Soir` = PC_jours_soir,
    `CA Boisson / Nourriture` = PC_boi_nourr,
    `CA par heures de travail` = format_CA(CA_heures,-1),
    `CA par jour` = format_CA(CA_mois_mean,-1),
    `Meilleure Semaine` = format_CA(CA_semaine_max,-2),
    `Pire Semaine` = format_CA(CA_semaine_min,-2),
    `Meilleur Jour` = format_CA(CA_jour_max,-1),
    `Pire Jour` = format_CA(CA_jour_min,-1),
    `Trimestre` = format_CA(CA_trim,-2)
  )

  SYNTHESE %>%
    pivot_longer(cols=1:ncol(SYNTHESE),names_to = "Indicateur",
                 values_to = format(ymd(paste(annee,mois,01)),format="%b %Y"))
}

# Tableau de résumé des mois par catégorie
table_resume_mois_category <- function(DB_PRODUITS,date){

  mois <- month(date)
  annee <- year(date)

  PREPA_MOIS <- DB_PRODUITS %>%
    select(DATE,ANNEE_MOIS,ANNEE_SEMAINE,JOUR_SEMAINE,
           PREMIER_JOUR_MOIS,PREMIER_JOUR_SEMAINE,
           CATEGORY,ventes) %>%
    filter(month(DATE) == mois & year(DATE) == annee & ventes > 0)

  SYNTHESE <- PREPA_MOIS %>%
    group_by(CATEGORY) %>%
    summarise(ventes = sum(ventes)) %>%
    ungroup() %>%
    mutate(ventes = paste0(round(100*ventes / sum(ventes),1),"%"))

  colnames(SYNTHESE) <- c("Catégorie", format(ymd(paste(annee,mois,01)),format="%b %Y"))

  SYNTHESE
}

# Tableau de prédiction de bières
table_evo_brassins <- function(max_date=today(),
                               length_predict = 200,
                               FL_ONLY_FINI = TRUE){

  if (is.null(max_date)) max_date <- today()

  table_evo_brassin_unique <- function(id_brassin){
    table_evo_brassin(id_brassin,length_predict,max_date)
  }

  if (FL_ONLY_FINI){
    vec_brassins_en_cours <- DB_BIERES %>%
      filter(!FL_FINI & DATE >= max_date-20) %>% pull(ID_BRASSIN) %>% unique
  }else{
    vec_brassins_en_cours <- DB_BIERES %>%
      filter(DATE >= max_date-20) %>% pull(ID_BRASSIN) %>% unique
  }

  vec_brassins_en_cours %>%
    map_df(table_evo_brassin_unique) %>%
    mutate(DT_PREDICT = max_date)
}

#### Graphiques génériques ####

# Graph de ventes sur X semaines (histogramme)
graph_hist_ventes <- function(DB_JOURS,DB_OBJECTIFS,date_before){

  TEST <- DB_JOURS %>%
    filter(DATE > date_before & DATE < today()) %>%
    left_join(DB_OBJECTIFS %>%
                rename(objectif = ventes) %>%
                select(DATE,objectif)) %>%
    group_by(ANNEE_SEMAINE) %>%
    summarise(ventes = round(sum(ventes,na.rm = TRUE)),
              objectif = sum(objectif,na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(ratio = ventes / objectif,
           SCORE = case_when(
             ratio < 0.85 ~ "bas",
             ratio < 1 ~ "moyen",
             TRUE ~ "haut"))

  TEST$predict <- c(smooth(pull(TEST[-nrow(TEST),"ventes"])),NA)

  ggplot(TEST) +
    aes(x = ANNEE_SEMAINE) +
    geom_bar(aes(y=ventes,fill = SCORE),width=0.5,stat = "identity") +
    geom_line(aes(y=predict,group = 1),col="grey",size=1,lty=2)+
    scale_fill_manual(values = c("haut" = "green3",
                                 "moyen" = "yellow3", "bas" = "red3")) +
    scale_y_continuous(labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    theme_minimal()+
    theme(legend.position = "none",
          axis.title.y = element_blank()
    ) +
    theme_mazette()+
    scale_x_discrete(position = "top") +
    xlab("Semaine") +
    ylab("Ventes")

}

# Graph de récapitulatif d'une année
graph_evo_annee_complete <- function(DB_JOURS,year,max_CA = 5000){
  db_jours <- DB_JOURS %>%
    filter(year(DATE) == year) %>%
    select(DATE,PREMIER_JOUR_MOIS,ventes) %>%
    mutate(ventes = ifelse(DATE >= today(),NA,ventes)) %>%
    mutate(jour_annee = yday(DATE),
           jour_mois = mday(DATE),
           jour_semaine = wday(DATE),
           jour_mois_id = ((month(DATE)-1)*40) + mday(DATE)) %>%
    group_by(PREMIER_JOUR_MOIS) %>%
    mutate(CA_MOYEN = mean(ventes,na.rm=TRUE),
           mid = mean(jour_mois_id)) %>%
    mutate(CA = pmin(max_CA,ventes)) %>%
    ungroup() %>%
    mutate(MOIS = factor(lubridate::month(PREMIER_JOUR_MOIS,
                                          label = TRUE, abbr = FALSE)))

  df_lines <- db_jours %>%
    group_by(MOIS) %>%
    summarise(
      start_x = min(jour_mois_id) - 5,
      end_x = max(jour_mois_id) + 5,
      y = unique(CA_MOYEN)
    ) %>%
    pivot_longer(
      cols = c(start_x, end_x),
      names_to = "type",
      values_to = "x"
    ) %>%
    mutate(
      x_group = if_else(type == "start_x", x + .1, x - .1),
      x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
      x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
    )

  # First, horizontal lines that are used as scale reference.
  # They are added first to ensure they stay in the background.
  p <- db_jours %>%
    ggplot(aes(jour_mois_id, CA_MOYEN)) +
    geom_hline(
      data = tibble(y = 7:10),
      aes(yintercept = y),
      color = "grey82",
      size = .5
    )

  # Add vertical segments.
  # These represent the deviation of episode's rating from the mean rating of
  # the season they appeared.
  p <- p +
    geom_segment(
      aes(
        xend = jour_mois_id,
        yend = CA,
        color = MOIS,
        color = after_scale(colorspace::lighten(color, .2))
      )
    )

  # Add lines and dots.
  # These represent the mean rating per season.
  # The dots mark each episode's rating, with its size given by the number of votes.
  p <- p +
    geom_line(
      data = df_lines,
      aes(x, y),
      color = "grey40"
    ) +
    geom_line(
      data = df_lines,
      aes(
        x_group,
        y,
        color = MOIS,
        color = after_scale(colorspace::darken(color, .2))
      ),
      size = 2.5
    ) +
    # geom_point(aes(size = total_votes, color = PREMIER_JOUR_MOIS))
    geom_point(aes(color = MOIS))

  p

  p <- p +
    geom_label(
      aes(
        mid,
        10.12, # vertical position of labels
        label = glue::glue("{MOIS}"),
        color = MOIS,
        color = after_scale(colorspace::darken(color, .2))
      ),
      fill = NA,
      # family = "Special Elite",
      fontface = "bold",
      size = 5,
      label.padding = unit(.2, "lines"),
      label.r = unit(.25, "lines"), # radius of the rounder corners.
      label.size = 0.5
    )

  # Scale and labels customization.
  # Override default colors with a much better looking palette.
  p <- p +
    scale_x_continuous(expand = c(.015, .015)) +
    scale_y_continuous(
      expand = c(.03, .03),
      limits = c(0, max_CA),
      breaks = seq(0, max_CA, by = 500),
      labels = dollar_format(suffix = "€", prefix = "",
                             big.mark = ".",decimal.mark = ","),
      sec.axis = dup_axis(name = NULL)
    ) +
    scale_color_manual(
      values = c("#486090", "#D7BFA6", "#6078A8", "#9CCCCC", "#7890A8",
                 "#C7B0C1", "#B5C9C9", "#90A8C0", "#A8A890", "#B8B810",
                 "#AE4590", "#B110E8"),
      guide = FALSE # don't show guide for the color scale.
    ) +
    # scale_size_binned(name = "Votes per Episode", range = c(.3, 3)) +
    labs(
      x = NULL,
      y = "Chiffre d'affaires HTVA",
      caption = ""
    ) +
    theme_light()+
    theme(
      # legend.position = c(.5, .085),
      legend.position = "top",
      panel.grid.major.x = element_blank() ,
      panel.grid.minor.x = element_blank() ,
      # axis.title.x.top = element_text(margin = margin(b=10)),
      axis.text = element_text(face = "bold",size = 12),
      axis.title = element_text(face = "bold",size = 12),
      axis.text.x = element_blank(),
      legend.key.width = unit(2, "lines")
    )

  p
}


# Graph evo ventes longue durée
graph_evo_ventes_LT <- function(DB_JOURS,var_group){

  TEMP <- DB_JOURS %>%
    select(DATE,ANNEE_MOIS,ANNEE_SEMAINE,JOUR_SEMAINE,
           PREMIER_JOUR_MOIS,PREMIER_JOUR_SEMAINE,
           COMMENTAIRE_FULL,ventes) %>%
    mutate(MOIS = ymd(paste(year(DATE),month(DATE),1))) %>%
    arrange(DATE)

  TEST <- TEMP %>%
    filter(DATE < today()) %>%
    rename(level = var_group) %>%
    group_by(level) %>%
    summarise(ventes = round(sum(ventes,na.rm = TRUE)),
              COMMENTAIRE = paste(COMMENTAIRE_FULL,collapse = "")) %>%
    ungroup() %>%
    mutate(FL_COM = COMMENTAIRE != "",
           LABEL = paste0("Ventes : ",format_CA(ventes,0),"\n",
                          "Date : ",level,"\n",
                          COMMENTAIRE))

  if (var_group == "PREMIER_JOUR_SEMAINE")
    var_unit <- "week"
  else if (var_group == "MOIS")
    var_unit <- "month"
  else
    var_unit <- NULL

  if (!is.null(var_unit)){
    last_day <- DB_JOURS[nrow(DB_JOURS),]$DATE
    test_day <- ceiling_date(last_day, unit = var_unit,
                             week_start = 1)-1 == last_day
    if (!test_day)
      TEST <- TEST %>% filter(row_number() != n())
  }

  nb_dates <- length(unique(TEMP$DATE))
  f_date <- format_x_date(nb_dates)

  p <- ggplot(TEST)+
    aes(x=level,y=ventes,text=LABEL,group=1)+
    geom_line()+
    geom_point(data = TEST %>% filter(FL_COM),col = "green") +
    geom_smooth(method = "loess",formula = 'y ~ x') +
    scale_y_continuous(labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    scale_x_date(breaks = f_date[1],date_labels =f_date[2])+
    # scale_x_date(breaks = "3 months",date_labels ="%m-%Y")+
    theme_minimal()+
    theme(legend.position = "none") +
    theme_mazette()+
    xlab("Date") +
    ylab("Chiffre d'affaires")

  ggplotly(p,tooltip = "text")
}


# graph evo ventes par table ou client
graph_evo_ventes_table <- function(DB_JOURS,var_group,var_div,fl_CA,label){

  TEMP <- DB_JOURS %>%
    select(DATE,ANNEE_MOIS,ANNEE_SEMAINE,JOUR_SEMAINE,ventes,
           PREMIER_JOUR_MOIS,PREMIER_JOUR_SEMAINE,COMMENTAIRE_FULL,
           NB_TABLES,NB_CLIENTS) %>%
    filter(DATE <= today()) %>%
    rename(NB = var_div)

  TEST <- TEMP %>%
    mutate(MOIS = ymd(paste(year(DATE),month(DATE),1))) %>%
    arrange(DATE) %>%
    rename(level = var_group) %>%
    group_by(level) %>%
    summarise(ventes = round(sum(ventes,na.rm = TRUE)),
              NB = round(sum(NB,na.rm = TRUE)),
              COMMENTAIRE = paste(COMMENTAIRE_FULL,collapse = "")) %>%
    ungroup() %>%
    mutate(FL_COM = COMMENTAIRE != "",
           ventes = round(ventes/NB,2),
           LABEL = paste0("Date : ",level,"\n",
                          "Ventes : ",format_CA(ventes,0),"\n",
                          "Nombre : ",NB,"\n",
                          COMMENTAIRE)) %>%
    filter(row_number() != n())

  p <- ggplot(TEST) + aes(x=level,y=NB,text=LABEL,group=1)

  if (fl_CA){
    p <- p +
      aes(x=level,y=ventes) +
      scale_y_continuous(labels = dollar_format(
        suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))
  }

  p <- p + geom_line() +
    geom_point(data = TEST %>% filter(FL_COM),col = "green") +
    geom_smooth(method = "loess",formula = 'y ~ x') +
    scale_x_date(breaks = "3 months",date_labels ="%m-%Y")+
    theme_minimal()+
    theme(legend.position = "none") +
    theme_mazette() +
    xlab("Date") +
    ylab(label)

  ggplotly(p,tooltip = "text")
}

graph_evo_productivite <- function(DB_HOREKO,DB_JOURS){

  DB_HOREKO <- DB_HOREKO %>%
    filter(HEURES > 0) %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(HEURES = sum(HEURES,na.rm = TRUE))

  DB_JOURS <- DB_JOURS %>%
    filter(ventes > 0) %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(ventes = sum(ventes,na.rm = TRUE))

  TABLE <- DB_JOURS %>%
    left_join(DB_HOREKO) %>%
    ungroup() %>% filter(row_number() != n()) %>%
    filter(HEURES > 150) %>%
    mutate(CA_HEURES = ventes / HEURES)

  ggplot(TABLE)+
    aes(x=PREMIER_JOUR_SEMAINE,y=CA_HEURES)+
    geom_line()+
    geom_smooth(method = "loess",formula = 'y ~ x') +
    scale_x_date(breaks = "1 month",date_labels ="%m-%Y")+
    scale_y_continuous(labels = dollar_format(suffix = "€", prefix = "",
                         big.mark = ".",decimal.mark = ","))+
    theme_minimal()+
    theme(legend.position = "none") +
    theme_mazette()+
    xlab("Date") +
    ylab("Chiffre d'affaires par heure de travail")

}

graph_evo_heures <- function(DB_HOREKO){

  TEST <- DB_HOREKO %>%
    filter(HEURES > 0) %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(HEURES = sum(HEURES,na.rm = TRUE)) %>%
    ungroup() %>% filter(row_number() != n())

  ggplot(TEST)+
    aes(x=PREMIER_JOUR_SEMAINE,y=HEURES)+
    geom_line()+
    geom_smooth(method = "loess",formula = 'y ~ x') +
    scale_x_date(breaks = "1 month",date_labels ="%m-%Y")+
    theme_minimal()+
    theme(legend.position = "none") +
    theme_mazette() +
    xlab("Date") +
    ylab("Nombre d'heures")
}

graph_evo_heures2 <- function(db_heures){

  db_heures <<- db_heures

  SYN_CA <- db_heures %>%
    filter(!is.na(Compta),!is.na(Horeko)) %>%
    mutate(EcartPct = paste0(round(Compta / Horeko * 100, 0), "%"),
           EcartEuro = Compta - Horeko,
           y_axis = (Compta + Horeko) / 2)

  # y_limits <- c(min(SYN_CA$Budget)*0.8,max(SYN_CA$Budget,2))
  y_min <- min(pmin(SYN_CA$Horeko,SYN_CA$Compta,na.rm=TRUE))
  y_max <- max(pmax(SYN_CA$Horeko,SYN_CA$Compta,na.rm=TRUE))
  y_limits <- c(y_min,y_max)
  y_limits <- c(0,y_max)

  SYN_CA_LONG <- SYN_CA %>% pivot_longer(cols = c("Compta","Horeko"),
                                         names_to = "Type", values_to = "Valeur")

  # Création du graphique
  p <- ggplot(SYN_CA_LONG) +
    geom_line(aes(x = PREMIER_JOUR_MOIS, y = Valeur, color = Type)) +
    geom_point(data = subset(SYN_CA_LONG,Type == "Compta"),
               aes(x = PREMIER_JOUR_MOIS, y = Valeur, color = Type)) +
    geom_segment(data = SYN_CA, aes(x = PREMIER_JOUR_MOIS, xend = PREMIER_JOUR_MOIS,
                                    y = Horeko, yend = Compta),
                 color = "grey", linetype = "dotted", alpha = 1) +
    geom_text(data = SYN_CA, aes(x = PREMIER_JOUR_MOIS, y = y_axis, label = format_CA(EcartEuro)),color = "black") +
    # geom_text(data = subset(SYN_CA_LONG, Mois == max(SYN_CA_LONG$Mois)),
    #           aes(x = Mois, y = Valeur, color = Type, label = Type)) +
    scale_x_date(breaks = "1 month",date_labels ="%b %Y")+
    scale_y_continuous(limits = y_limits,labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    scale_color_manual(values = c("Horeko" = "red", "Compta" = "black")) +
    labs(
      # title = paste0("Label comptable : ",label_compte,
      #                ". (Cumul : ",format_CA(
      #                  sum(SYN_CA$EcartEuro,na.rm = TRUE)),")"),
      x = "Mois",
      y = "Coût du travail",
      color = "") +
    theme_minimal() +
    theme_mazette()

  ggplotly(p)

}

graph_evo_ventes_mois <- function(DB_JOURS,DB_OBJECTIFS,nb_mois){

  db_objectif_mois <- DB_OBJECTIFS %>%
    filter(ANNEE == year(today()),MOIS == month(today())) %>%
    arrange(DATE) %>%
    mutate(cum_objectif = cumsum(ventes),
           DATE_GRAPH = make_date(year(today()), 12, day(DATE)))

  objectifs <- DB_OBJECTIFS %>%
    filter(DATE >= floor_date(today(),"month")-months(nb_mois)
           # & DATE <= today()
           ) %>%
    group_by(ANNEE,MOIS,ANNEE_MOIS) %>%
    summarise(objectif = sum(ventes,na.rm = TRUE),.groups = "drop")

  objectif_actuel <- objectifs %>%
    filter(ANNEE == year(today()),MOIS == month(today())) %>%
    pull(objectif)

  objectif_cours <- DB_OBJECTIFS %>%
    filter(DATE >= floor_date(today(),"month") & DATE <= today()-1) %>%
    summarise(objectif = sum(ventes,na.rm = TRUE)) %>%
    pull(objectif)

  TEST <- DB_JOURS %>%
    filter(DATE >= floor_date(today(),"month")-months(nb_mois)
             & DATE < today()) %>%
    left_join(objectifs) %>%
    group_by(ANNEE_MOIS) %>%
    arrange(DATE) %>%
    mutate(ventes = ifelse(is.na(ventes),0,ventes),
           cum_ventes = cumsum(round(ventes)),
           objectif_actuel = cumsum(objectif_actuel),
           MOIS = month(DATE,label=TRUE,abbr = FALSE),
           # DATE_GRAPH = make_date(year(today()), month(today()), day(DATE))) %>%
           DATE_GRAPH = make_date(year(today()), 12, day(DATE))) %>%
    filter(!is.na(DATE_GRAPH)) %>%
    mutate(OPACITY = ifelse(month(DATE) == month(today()),1,0.9),
           FL_COM = COMMENTAIRE != "",
           LABEL = paste0("Date : ",DATE,"\n",
                          "Ventes du jour : ",format_CA(ventes,0),"\n",
                          "Ventes cumulées : ",format_CA(cum_ventes,0),"\n",
                          "Objectif du mois : ",format_CA(objectif,0),"\n",
                          COMMENTAIRE_FULL))

  ggplot(TEST) +
    geom_line(aes(x=DATE_GRAPH,y=cum_ventes,col=MOIS,
                  text=LABEL,group=MOIS),size=1)+
    geom_line(data=db_objectif_mois,aes(x=DATE_GRAPH,y=cum_objectif),
              col='green3',lty=2)+
    geom_point(data = subset(TEST,DATE == max(DATE)),
              aes(x=DATE_GRAPH,y=cum_ventes),
              size=3.5,color="red3")+
    geom_point(aes(x=DATE_GRAPH,y=cum_ventes,col=MOIS,text=LABEL),
               data = TEST %>% filter(FL_COM),size=3) +
    geom_text(data = subset(TEST,DATE == max(DATE)),
              aes(x=DATE_GRAPH,y=cum_ventes),
              label="Dernier jour",nudge_y = 2500)+
    scale_x_date(breaks = "5 days",date_labels ="%d")+
    scale_y_continuous(breaks = seq(0,objectif_actuel+5000,by=10000),
                       labels = dollar_format(
                         suffix = "€", prefix = "",
                         big.mark = ".",decimal.mark = ","))+
    xlab("Jour du mois") +
    ylab("Ventes cumulées")+
    labs(alpha = "")+
    theme_minimal()+
    theme_mazette()+
    theme(legend.position = "bottom",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) +
    geom_hline(yintercept = objectif_cours,color="green3",lty=2,lwd=0.5,alpha=0.75) +
    annotate("text", x = min(TEST$DATE_GRAPH), y = objectif_cours+1000, label = "Objectif du mois",color="green4") +
    geom_hline(yintercept = objectif_actuel,color="green3",lty=2,lwd=1) +
    annotate("text", x = min(TEST$DATE_GRAPH), y = objectif_actuel+1000, label = "Objectif final du mois",color="green4")
}

# Graph evo ventes longue durée
graph_evo_produits <- function(db,indic,label){

  TEST <- db %>%
    rename(INDIC = indic) %>%
    select(DATE,PREMIER_JOUR_SEMAINE,INDIC) %>%
    mutate(MOIS = ymd(paste(year(DATE),month(DATE),1))) %>%
    arrange(DATE) %>%
    group_by(PREMIER_JOUR_SEMAINE) %>%
    summarise(INDIC = round(sum(INDIC,na.rm=TRUE))) %>%
    ungroup() %>%
    filter(row_number() != n())

  nb_dates <- length(unique(db$DATE))
  f_date <- format_x_date(nb_dates)

  ggplot(TEST)+
    aes(x=PREMIER_JOUR_SEMAINE, y = INDIC)+
    geom_line() +
    geom_smooth(method = "loess",formula = 'y ~ x') +
    scale_x_date(breaks = f_date[1],date_labels =f_date[2])+
    scale_y_continuous(labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    theme_minimal()+
    theme(legend.position = "bottom") +
    theme_mazette()+
    labs(title = "Évolution de cette gamme de produits",
         x = "Date",
         y = label)
}



graph_cout_secteurs <- function(DB,x,fill){

  # Nourriture
  if ("Cuisine" %in% DB$SECTEUR){
    if (fill == "TYPE_COUT")
      vec_col <- c(pull(pal_col[pal_col$name == "Nourriture - Achat","col"]),
                        pull(pal_col[pal_col$name == "Nourriture - Travail","col"]))
    if (fill == "SECTEUR")
      vec_col <- c(pull(pal_col[pal_col$name == "Boulangerie - Global","col"]),
                   pull(pal_col[pal_col$name == "Cuisine - Global","col"]))
  }

  # Boisson
  if ("Service" %in% DB$SECTEUR){
    if (fill == "TYPE_COUT")
      vec_col <- c(pull(pal_col[pal_col$name == "Boisson - Achat","col"]),
                   pull(pal_col[pal_col$name == "Boisson - Travail","col"]))
    if (fill == "SECTEUR")
      vec_col <- c(pull(pal_col[pal_col$name == "Brasserie - Global","col"]),
                   pull(pal_col[pal_col$name == "Service - Global","col"]))
  }

  # Global
  if ("Nourriture" %in% DB$SECTEUR){
    if (fill == "TYPE_COUT")
      vec_col <- c(pull(pal_col[pal_col$name == "Global - Achat","col"]),
                   pull(pal_col[pal_col$name == "Support - Achats","col"]),
                   pull(pal_col[pal_col$name == "Global - Travail","col"]))
    if (fill == "SECTEUR")
      vec_col <- c(pull(pal_col[pal_col$name == "Boisson - Global","col"]),
                   pull(pal_col[pal_col$name == "Nourriture - Global","col"]),
                   pull(pal_col[pal_col$name == "Support - Global","col"]))
  }


  DB_TEMP <- DB %>%
    mutate(TYPE_COUT = ifelse(TYPE_COUT %in% c("Achat","Stock"),
                              "Food Cost","Work Cost")) %>%
    mutate(TYPE_COUT = ifelse("Brasserie" %in% SECTEUR &
                                TYPE_COUT == "Food Cost",
                              "Bev Cost",TYPE_COUT)) %>%
    mutate(TYPE_COUT = ifelse("Nourriture" %in% SECTEUR &
                                TYPE_COUT == "Food Cost",
                              "Food & Bev Cost",TYPE_COUT)) %>%
    mutate(TYPE_COUT = ifelse(SECTEUR == "Support" & TYPE_COUT == "Food & Bev Cost",
                              "General Cost",TYPE_COUT)) %>%
    group_by(SECTEUR,TYPE_COUT) %>%
    summarise(COUT = sum(COUT,na.rm=TRUE),.groups = "drop")

  DB_TEMP %>%
    group_by(!!sym(fill)) %>% summarise(COUT = sum(COUT,na.rm=TRUE)) %>%
    mutate(!!sym(x) := "Prime Cost",CD_COUT="TOTAL") %>%
    add_row(DB_TEMP %>% select(SECTEUR,TYPE_COUT,COUT) %>% mutate(CD_COUT="DETAIL")) %>%
    group_by(!!sym(x),CD_COUT) %>%
    mutate(pc = round(100*COUT / sum(COUT)),
           pc = ifelse(abs(pc) < 5,"",paste0(pc,"%")))%>%
    ungroup() %>%
    ggplot() +
    aes(x=!!sym(x),y=COUT,fill=!!sym(fill))+
    geom_bar(stat="identity",position = position_stack(reverse = TRUE)) +
    facet_grid(CD_COUT~.,scales="free_y",space = "free") +
    geom_text(aes(label=pc),position = position_stack(reverse = TRUE,vjust = 0.5)) +
    scale_y_continuous(labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    scale_fill_manual(values=vec_col) +
    labs(x="",y="") +
    coord_flip() +
    theme_minimal()+
    theme_mazette()+
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          strip.text = element_blank())
}

graph_evo_cout <- function(date_fin,cd_secteur){


  if (cd_secteur == "Nourriture") {
    secteur <- c("Cuisine", "Boulangerie")
    vec_col <- c(pull(pal_col[pal_col$name == "Boulangerie - Achats","col"]),
                 pull(pal_col[pal_col$name == "Boulangerie - Travail","col"]),
                 pull(pal_col[pal_col$name == "Cuisine - Achats","col"]),
                 pull(pal_col[pal_col$name == "Cuisine - Travail","col"]))
  }

  if (cd_secteur == "Boisson") {
    secteur <- c("Service", "Brasserie")
    vec_col <- c(pull(pal_col[pal_col$name == "Brasserie - Achats","col"]),
                 pull(pal_col[pal_col$name == "Brasserie - Travail","col"]),
                 pull(pal_col[pal_col$name == "Service - Achats","col"]),
                 pull(pal_col[pal_col$name == "Service - Travail","col"]))
  }

  db_cout_total <- DB_COUT_TOTAL %>%
    mutate(TYPE_COUT = ifelse(TYPE_COUT %in% c("Achat","Stock"),
                              ifelse(cd_secteur == "Nourriture",
                                     "Food Cost","Bev Cost")
                              ,"Work Cost"))

  if (cd_secteur == "Global"){
    db_cout_total <- DB_COUT_TOTAL %>%
      mutate(SECTEUR = case_when(
        SECTEUR %in% c("Boulangerie","Cuisine") ~ "Nourriture",
        SECTEUR %in% c("Service","Brasserie") ~ "Boisson",
        TRUE ~ "Support"
      )) %>%
      mutate(TYPE_COUT = case_when(
        SECTEUR == "Support" & TYPE_COUT == "Achat" ~ "General Cost",
        TYPE_COUT %in% c("Achat","Stock") ~ "Fodd & Bev Cost",
        TRUE ~ "Work Cost"))

    secteur <- c("Nourriture", "Boisson","Support")
    vec_col <- c(pull(pal_col[pal_col$name == "Boisson - Achat","col"]),
                 pull(pal_col[pal_col$name == "Boisson - Travail","col"]),
                 pull(pal_col[pal_col$name == "Nourriture - Achat","col"]),
                 pull(pal_col[pal_col$name == "Nourriture - Travail","col"]),
                 pull(pal_col[pal_col$name == "Support - Achats","col"]),
                 pull(pal_col[pal_col$name == "Support - Travail","col"]))
  }

  vec_DATE <- DB_DATE %>%
    filter(PREMIER_JOUR_MOIS <= date_fin,
           PREMIER_JOUR_MOIS >= date_fin-years(1),) %>%
    select(PREMIER_JOUR_MOIS) %>% pull %>% unique

  DB_TEMP <- db_cout_total %>%
    group_by(PREMIER_JOUR_MOIS,SECTEUR,TYPE_COUT) %>%
    summarise(COUT = max(0,sum(COUT,na.rm=TRUE)),.groups = "drop")

  DB <- DB_TEMP %>%
    filter(PREMIER_JOUR_MOIS %in% vec_DATE, SECTEUR %in% secteur) %>%
    mutate(group = paste0(SECTEUR,"-",TYPE_COUT)) %>%
    filter(COUT > 0)

  p <- ggplot(DB) +
    aes(x = PREMIER_JOUR_MOIS, y = COUT,
        group= group,fill = group) +
    geom_area(position = "fill") +
    scale_y_continuous(name = "Indicateurs (%)") +
    labs(x = "Date", color = "Indicateurs") +
    theme_minimal() +
    theme_minimal()+
    theme_mazette()+
    theme(legend.position = "bottom",
          legend.title = element_text(size=16),
          legend.text = element_text(size=14))+
    scale_x_date(date_breaks = "1 month",
                 labels = function(x) format(x, "%d/%m")) +
    scale_fill_manual(values = vec_col)

  p
}




graph_evo_kpi <- function(date_fin,cd_secteur,secteur){

  DB_TEMP <- DB_KPI_NOURRITURE_CA
  if (cd_secteur == "Nourriture"){
    vec_col <- c(pull(pal_col[pal_col$name == "Prime Cost / CA","col"]),
                 pull(pal_col[pal_col$name == "Nourriture - Achat","col"]),
                 pull(pal_col[pal_col$name == "Nourriture - Travail","col"]),
                 pull(pal_col[pal_col$name == "Cuisine - Global","col"]),
                 pull(pal_col[pal_col$name == "Boulangerie - Travail","col"]))
  }
  if (cd_secteur == "Boisson"){
    DB_TEMP <- DB_KPI_BOISSON_CA
    vec_col <- c(pull(pal_col[pal_col$name == "Prime Cost / CA","col"]),
                 pull(pal_col[pal_col$name == "Boisson - Achat","col"]),
                 pull(pal_col[pal_col$name == "Boisson - Travail","col"]),
                 pull(pal_col[pal_col$name == "Service - Global","col"]),
                 pull(pal_col[pal_col$name == "Brasserie - Travail","col"]))
  }

  DB_TEMP <- DB_TEMP %>%
    filter(PREMIER_JOUR_MOIS <= date_fin,
           PREMIER_JOUR_MOIS >= date_fin-years(1)) %>%
    pivot_longer(cols = c(COUT_CA, L_CA, MP_CA, CUI_CA,
                          BOU_CA, SER_CA, BRA_CA),
                 names_to = "indicateur", values_to = "valeur")


  if (cd_secteur == "Global"){
    DB_TEMP <- DB_KPI_TOTAL_CA %>%
      filter(PREMIER_JOUR_MOIS <= date_fin,
             PREMIER_JOUR_MOIS >= date_fin-years(1)) %>%
      select(-COUT) %>%
      pivot_longer(cols = c(COUT_CA, BOI_CA,
                            NOU_CA, SUP_CA,
                            L_CA, MP_CA, FG_CA),
                   names_to = "indicateur", values_to = "valeur")

    vec_col <- c(pull(pal_col[pal_col$name == "Prime Cost / CA","col"]),
                 pull(pal_col[pal_col$name == "Boisson - Global","col"]),
                 pull(pal_col[pal_col$name == "Nourriture - Global","col"]),
                 pull(pal_col[pal_col$name == "Support - Global","col"]),
                 pull(pal_col[pal_col$name == "Global - Achat","col"]),
                 pull(pal_col[pal_col$name == "Support - Achats","col"]),
                 pull(pal_col[pal_col$name == "Global - Travail","col"]))
  }

  DB_TEMP <- DB_TEMP %>%
    mutate(KPI = case_when(
      indicateur == "COUT_CA" ~ "Prime Cost / CA",
      indicateur == "L_CA" ~ "Work Cost / CA",
      indicateur == "FG_CA" ~ "General Cost / CA",
      indicateur == "MP_CA" ~ case_when(
        cd_secteur == "Nourriture" ~ "Food Cost / CA",
        cd_secteur == "Boisson" ~ "Bev Cost / CA",
        TRUE ~ "Food & Bev Cost / CA"),
      indicateur == "CUI_CA" ~ "Cuisine / CA",
      indicateur == "BOU_CA" ~ "Boulangerie / CA",
      indicateur == "SER_CA" ~ "Service / CA",
      indicateur == "BRA_CA" ~ "Brasserie / CA",
      indicateur == "BOI_CA" ~ "Boisson / CA",
      indicateur == "NOU_CA" ~ "Nourriture / CA",
      indicateur == "SUP_CA" ~ "Support / CA"),
      KPI = factor(KPI,levels = c("Prime Cost / CA","Boisson / CA",
                                  "Nourriture / CA","Support / CA",
                                  "Food Cost / CA",
                                  "Bev Cost / CA","Food & Bev Cost / CA",
                                  "General Cost / CA","Work Cost / CA",
                                  "Boulangerie / CA","Cuisine / CA",
                                  "Brasserie / CA","Service / CA")),
      ca = paste("CA :",format_CA(CA,-1))
      ) %>%
    group_by(KPI) %>%
    filter(sum(valeur) != 0) %>%
    # mutate(COUT = CA * valeur) %>%
    mutate(COUT = valeur) %>%
    ungroup()

  p <- DB_TEMP %>%
    ggplot()+
    aes(x=PREMIER_JOUR_MOIS,y=COUT,group = KPI,color = KPI, text = ca)+
    geom_point()+
    geom_line()+
    scale_x_date(date_breaks = "1 month",
                 labels = function(x) format(x, "%d/%m")) +
    scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                       breaks= seq(0,1,by=0.2)) +
    scale_color_manual(values = vec_col)+
    theme_minimal() +
    theme(legend.position = "bottom")+
    theme_mazette()

  ggplotly(p)
}

# Comptabilité

graph_evo_ecart_budget <- function(db_obj,db_jours){

  db <- db_jours %>%
    filter(year(DATE) == year(today())) %>%
    arrange(DATE) %>%
    left_join(db_obj %>% select(DATE,ventes) %>%
                rename(ventes_OBJ = ventes)) %>%
    mutate(ventes = ifelse(is.na(ventes),0,ventes),
           DIFF_OBJ_REAL = ifelse(DATE < today(),
                                  cumsum(ventes - ventes_OBJ),
                                  NA),
           LABEL = paste0("Date : ",DATE,"\n",
                          "Ventes : ",format_CA(ventes,0),"\n",
                          "Objectif : ",format_CA(ventes_OBJ,0),"\n",
                          "Ecart : ",format_CA(DIFF_OBJ_REAL,0)))

  p <- db %>%
    ggplot()+
    aes(x=DATE,y=DIFF_OBJ_REAL,col=DIFF_OBJ_REAL,text=LABEL)+
    geom_point()+
    geom_point(data = db %>% filter(ventes>0) %>% tail(1),size=3)+
    scale_color_gradient2(
      low = "red3",mid = "grey",high = "green3",midpoint = 0
    ) +
    geom_text(data =db %>% filter(ventes>0) %>% tail(1),
              aes(x=DATE,y=DIFF_OBJ_REAL,label=LABEL),nudge_x = 40) +
    scale_y_continuous(labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    geom_hline(yintercept = 0,col="grey3",lwd=1)+
    theme_minimal()+
    theme(legend.position = "none") +
    theme_mazette()+
    labs(x="Date",y="Écart avec l'objectif")

  ggplotly(p,tooltip = "text")
}

graph_evo_ecart_ym1 <- function(db_jours){
  db <- db_jours %>%
    filter(year(DATE) == year(today())) %>%
    mutate(WEEK = week(DATE),
           WDAY = wday(DATE)) %>%
    arrange(DATE) %>%
    left_join(db_jours %>%
                filter(year(DATE) == year(today())-1) %>%
                mutate(WEEK = week(DATE),
                       WDAY = wday(DATE)) %>%
                select(WEEK,WDAY,ventes) %>%
                rename(ventes_ym1 = ventes)) %>%
    mutate(ventes = ifelse(is.na(ventes),0,ventes),
           ventes_ym1 = ifelse(is.na(ventes_ym1),0,ventes_ym1),
           DIFF_Y_YM1 = ifelse(DATE < today(),
                                  cumsum(ventes - ventes_ym1),
                                  NA),
           LABEL = paste0("Date : ",DATE,"\n",
                          "Ventes Y : ",format_CA(ventes,0),"\n",
                          "Ventes Y-1: ",format_CA(ventes_ym1,0),"\n",
                          "Ecart : ",format_CA(DIFF_Y_YM1,0)))

  p <- db %>%
    ggplot()+
    aes(x=DATE,y=DIFF_Y_YM1,col=DIFF_Y_YM1,text=LABEL)+
    geom_point()+
    geom_point(data = db %>% filter(ventes>0) %>% tail(1),size=3)+
    scale_color_gradient2(
      low = "red3",mid = "grey",high = "green3",midpoint = 0
    ) +
    geom_text(data =db %>% filter(ventes>0) %>% tail(1),
              aes(x=DATE,y=DIFF_Y_YM1,label=LABEL),nudge_x = 40) +
    scale_y_continuous(labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    geom_hline(yintercept = 0,col="grey3",lwd=1)+
    theme_minimal()+
    theme(legend.position = "none") +
    theme_mazette()+
    labs(x="Date",y="Écart avec l'an passé à date")

  ggplotly(p,tooltip = "text")
}

graph_evo_comptes <- function(code){

  if (is.numeric(code)) code <- as.character(code)

  label <- DB_COMPTA %>% filter(CODE == code,TYPE == "Budget") %>%
    pull(LABEL) %>% unique()

  SYN_CA <- DB_COMPTA %>%
    filter(CODE == code,!is.na(CA_HTVA)) %>%
    group_by(CODE,TYPE,PREMIER_JOUR_MOIS) %>%
    filter(row_number() == 1) %>%
    mutate(LABEL = label) %>%
    pivot_wider(names_from = TYPE,values_from = CA_HTVA, values_fill = 0) %>%
    mutate(Realisé = Comptes,Budgeté = Budget,
          # Realisé = Comptes/Budget,
          #  Budgeté = 1,
           EcartPct = paste0(round(Realisé / Budgeté * 100, 0), "%"),
           EcartEuro = Comptes - Budget,
           y_axis = (Realisé + Budgeté) / 2)

  SYN_CA[SYN_CA$Realisé == 0,"Realisé"] <- NA
  SYN_CA[is.na(SYN_CA$Realisé),"EcartEuro"] <- NA
  SYN_CA[is.na(SYN_CA$Realisé),"EcartPct"] <- NA
  SYN_CA[is.na(SYN_CA$Realisé),"y_axis"] <- NA

  # y_limits <- c(min(SYN_CA$Budgeté)*0.8,max(SYN_CA$Budgeté,2))
  y_min <- min(pmin(SYN_CA$Budgeté,SYN_CA$Realisé,na.rm=TRUE))
  y_max <- max(pmax(SYN_CA$Budgeté,SYN_CA$Realisé,na.rm=TRUE))
  y_limits <- c(y_min,y_max)


  SYN_CA_LONG <- SYN_CA %>% pivot_longer(cols = c("Realisé","Budgeté"),
                                         names_to = "Type", values_to = "Valeur")

  # Création du graphique
  p <- ggplot(SYN_CA_LONG) +
    geom_line(aes(x = PREMIER_JOUR_MOIS, y = Valeur, color = Type)) +
    geom_point(data = subset(SYN_CA_LONG,Type == "Realisé"),
               aes(x = PREMIER_JOUR_MOIS, y = Valeur, color = Type)) +
    geom_segment(data = SYN_CA, aes(x = PREMIER_JOUR_MOIS, xend = PREMIER_JOUR_MOIS,
                                    y = Budgeté, yend = Realisé),
                 color = "grey", linetype = "dotted", alpha = 1) +
    geom_text(data = SYN_CA, aes(x = PREMIER_JOUR_MOIS, y = y_axis, label = format_CA(EcartEuro)),color = "black") +
    # geom_text(data = subset(SYN_CA_LONG, Mois == max(SYN_CA_LONG$Mois)),
    #           aes(x = Mois, y = Valeur, color = Type, label = Type)) +
    scale_x_date(breaks = "1 month",date_labels ="%b")+
    scale_y_continuous(limits = y_limits,labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    scale_color_manual(values = c("Budgeté" = "red", "Realisé" = "black")) +
    labs(
      title = paste0("Code comptable ",code, " : ",label,
                     ". (Cumul : ",format_CA(
        sum(SYN_CA$EcartEuro,na.rm = TRUE)),")"),
      x = "Année 2024",
      y = "Chiffre d'affaires HTVA",
      color = "") +
    theme_minimal() +
    theme_mazette() +
    theme(legend.position = "none")

  ggplotly(p)

}

graph_evo_comptes2 <- function(label_compte){

  SYN_CA <- DB_COMPTA_FULL %>%
    filter(LABEL_COMPTE == label_compte,
           PERIODE == "MOIS",
           year(INDEX_PERIODE) == year(today())) %>%
    mutate(EcartPct = paste0(round(Realise / Budget * 100, 0), "%"),
           EcartEuro = Realise - Budget,
           y_axis = (Realise + Budget) / 2)

  SYN_CA[SYN_CA$Realise == 0,"Realise"] <- NA
  SYN_CA[is.na(SYN_CA$Realise),"EcartEuro"] <- NA
  SYN_CA[is.na(SYN_CA$Realise),"EcartPct"] <- NA
  SYN_CA[is.na(SYN_CA$Realise),"y_axis"] <- NA

  # y_limits <- c(min(SYN_CA$Budget)*0.8,max(SYN_CA$Budget,2))
  y_min <- min(pmin(SYN_CA$Budget,SYN_CA$Realise,na.rm=TRUE))
  y_max <- max(pmax(SYN_CA$Budget,SYN_CA$Realise,na.rm=TRUE))
  y_limits <- c(y_min,y_max)

  SYN_CA_LONG <- SYN_CA %>% pivot_longer(cols = c("Realise","Budget"),
                                         names_to = "Type", values_to = "Valeur")

  # Création du graphique
  p <- ggplot(SYN_CA_LONG) +
    geom_line(aes(x = INDEX_PERIODE, y = Valeur, color = Type)) +
    geom_point(data = subset(SYN_CA_LONG,Type == "Realise"),
               aes(x = INDEX_PERIODE, y = Valeur, color = Type)) +
    geom_segment(data = SYN_CA, aes(x = INDEX_PERIODE, xend = INDEX_PERIODE,
                                    y = Budget, yend = Realise),
                 color = "grey", linetype = "dotted", alpha = 1) +
    geom_text(data = SYN_CA, aes(x = INDEX_PERIODE, y = y_axis, label = format_CA(EcartEuro)),color = "black") +
    # geom_text(data = subset(SYN_CA_LONG, Mois == max(SYN_CA_LONG$Mois)),
    #           aes(x = Mois, y = Valeur, color = Type, label = Type)) +
    scale_x_date(breaks = "1 month",date_labels ="%b")+
    scale_y_continuous(limits = y_limits,labels = dollar_format(
      suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","))+
    scale_color_manual(values = c("Budget" = "red", "Realise" = "black")) +
    labs(
      title = paste0("Label comptable : ",label_compte,
                     ". (Cumul : ",format_CA(
                       sum(SYN_CA$EcartEuro,na.rm = TRUE)),")"),
      x = "Année 2024",
      y = "Chiffre d'affaires HTVA",
      color = "") +
    theme_minimal() +
    theme_mazette() +
    theme(legend.position = "none")

  ggplotly(p)

}

# Compta V2
graph_decomp_comptes <- function(df){

  # Préparer le nom du graphique

  # Préparer les données pour le graphique
  data <- df %>%
    select(LABEL_COMPTE, Realise, Budget, CODE_COMPTE) %>%
    mutate(
      step = factor(LABEL_COMPTE, levels = LABEL_COMPTE),
      is_subtotal = is.na(CODE_COMPTE)
    )

  # Renommer les colonnes pour éviter les conflits avec les fonctions R
  data <- data %>%
    mutate(
      start_realise = NA,
      end_realise = NA,
      start_budget = NA,
      end_budget = NA
    )

  # Variables pour suivre les totaux cumulatifs
  total_realise <- 0
  total_budget <- 0

  # Calculer les positions de début et de fin pour chaque barre
  for (i in 1:nrow(data)) {
    if (data$is_subtotal[i]) {
      # Pour les sous-totaux, la barre commence à 0 et se termine au montant du sous-total
      data$start_realise[i] <- 0
      data$end_realise[i] <- data$Realise[i]
      data$start_budget[i] <- 0
      data$end_budget[i] <- data$Budget[i]
      # Mettre à jour les totaux cumulatifs pour les étapes suivantes
      total_realise <- data$Realise[i]
      total_budget <- data$Budget[i]
    } else {
      # Pour les autres éléments, la barre commence au total cumulatif actuel
      data$start_realise[i] <- total_realise
      total_realise <- total_realise + data$Realise[i]
      data$end_realise[i] <- total_realise

      data$start_budget[i] <- total_budget
      total_budget <- total_budget + data$Budget[i]
      data$end_budget[i] <- total_budget
    }
  }

  # Déterminer le type de chaque barre pour la coloration
  data$type <- ifelse(
    data$is_subtotal,
    ifelse(data$end_realise >= 0, "Sous-total positif", "Sous-total négatif"),
    ifelse(data$Realise >= 0, "Augmentation", "Diminution")
  )

  # Définir les couleurs pour chaque type de barre
  fill_colors <- c(
    "Augmentation" = "forestgreen",
    "Diminution" = "firebrick",
    "Sous-total positif" = "steelblue",
    "Sous-total négatif" = "darkorange",
    "Augmentation (Budget)" = "darkolivegreen3",
    "Diminution (Budget)" = "indianred2",
    "Sous-total positif (Budget)" = "lightsteelblue",
    "Sous-total négatif (Budget)" = "orange"
  )

  # Renommer les colonnes pour éviter les conflits avec les fonctions R
  data <- data %>%
    rename(
      start_R = start_realise,
      end_R = end_realise,
      start_B = start_budget,
      end_B = end_budget
    )

  # Transformer les données en format long
  data_long <- data %>%
    select(step, start_R, end_R, start_B, end_B, type, is_subtotal, Realise, Budget) %>%
    pivot_longer(
      cols = c(start_R, end_R, start_B, end_B),
      names_to = c(".value", "measure"),
      names_pattern = "(start|end)_(R|B)"
    ) %>%
    mutate(
      measure = recode(measure, R = "Réalisé", B = "Budget"),
      fill_type = ifelse(measure == "Réalisé", type, paste0(type, " (Budget)")),
      value = ifelse(measure == "Réalisé", Realise, Budget)
    )

  # Ordre des niveaux pour que le Budget soit en arrière-plan
  data_long$measure <- factor(data_long$measure, levels = c("Budget", "Réalisé"))

  # Créer le graphique en cascade
  ggplot(data_long, aes(x = step, fill = fill_type, group = interaction(step, measure))) +
    # Les barres du Budget en arrière-plan
    geom_rect(
      data = data_long %>% filter(measure == "Budget"),
      aes(
        xmin = as.numeric(step) - 0.3,
        xmax = as.numeric(step) + 0.2,
        ymin = pmin(start, end),
        ymax = pmax(start, end)
      ),
      color = NA, alpha = 0.5
    ) +
    # Les barres du Réalisé en avant-plan
    geom_rect(
      data = data_long %>% filter(measure == "Réalisé"),
      aes(
        xmin = as.numeric(step) - 0.2,
        xmax = as.numeric(step) + 0.3,
        ymin = pmin(start, end),
        ymax = pmax(start, end)
      ),
      color = NA
    ) +
    # Ajouter les montants sur les barres du Réalisé uniquement
    geom_text(
      data = data_long %>% filter(measure == "Réalisé"),
      aes(
        x = as.numeric(step) + 0.05,
        y = (start + end) / 2,
        label = round(value, 2)
      ),
      color = "white",
      size = 3
    ) +
    scale_fill_manual(values = fill_colors) +
    labs(
      title = "Comparaison du Réalisé et du Budget",
      x = "Étapes", y = "Montant", fill = "Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

}

# Affluence

graph_hist_affluence <- function(DB_TICKET){

  TEST <- DB_TICKET %>%
    filter(JOUR_SEMAINE != vecteur_jours[1]) %>%
    mutate(
      TIMESTAMP_GRAPH = make_datetime(
        year = year(today()),
        month = month(today()),
        day = day(today()),
        hour = hour(TIMESTAMP),
        min = minute(TIMESTAMP)
      )) %>%
    mutate(TIMESTAMP_GRAPH =
             if_else(hour(TIMESTAMP_GRAPH) < 5,
                     TIMESTAMP_GRAPH + days(1),
                     TIMESTAMP_GRAPH)) %>%
    select(DATE,TIMESTAMP_GRAPH,ID_TICKET,PRIX_TOTAL,JOUR_SEMAINE)

  TEST <- TEST %>%
    group_by(DATE,TIMESTAMP_GRAPH,ID_TICKET,JOUR_SEMAINE) %>%
    summarise(PRIX_TOTAL = round(sum(PRIX_TOTAL)),.groups = "drop") %>%
    filter(PRIX_TOTAL > 0) %>%
    uncount(PRIX_TOTAL)

  ggplot(TEST) +
    aes(x = TIMESTAMP_GRAPH, y = JOUR_SEMAINE, fill = ..y..) +
    geom_density_ridges_gradient(panel_scaling = FALSE,
                                 scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis(alpha = 0.8) +
    scale_x_datetime(date_breaks = "1 hour", date_labels  = "%H") +
    scale_y_discrete(limits = rev(levels(TEST$JOUR_SEMAINE))) +
    ylab("Jour de semaine")+
    xlab("Heure du ticket")+
    # labs(title = 'Temperatures in Lincoln NE in 2016') +
    theme_ridges() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )
}

# Évolution des brassins

graph_evo_brassin <- function(db){

  # Calculer le nombre de dates dans votre plage
  nb_dates <- length(unique(db$DATE))
  f_date <- format_x_date(nb_dates)

  ggplot(db,aes(x = DATE, y = VOLUME_RESTANT,col = BOISSON,lty=FL_PREDICT)) +
    geom_line() +
    labs(title = "Évolution des bières actuelles avec prédiction",
         x = "Date", y = "Volume Restant") +
    theme_minimal() +
    scale_x_date(date_breaks = f_date[1], date_labels  = f_date[2]) +
    ylim(-1,NA)+
    guides(linetype = FALSE)
}

graph_predict_brassin <- function(db){
  ggplot(db,aes(x = DATE, y = VOLUME_RESTANT)) +
    geom_line() +
    geom_ribbon(aes(ymin = LO_50, ymax = HI_50), fill = "blue", alpha = 0.2) +
    geom_ribbon(aes(ymin = LO_75, ymax = HI_75), fill = "green", alpha = 0.2) +
    geom_ribbon(aes(ymin = LO_95, ymax = HI_95), fill = "red", alpha = 0.2) +
    labs(title = "Volume Restant avec Intervalles de Confiance",
         x = "Date",
         y = "Volume Restant") +
    theme_minimal() +
    scale_x_date(date_breaks = "7 days", date_labels  = "%d/%m") +
    ylim(-1,NA)
}

report_brassin <- function(DB_BRASSINS,DB_BIERES,DB_PRODUITS,id_brassin){

  DB_BRASSINS <- DB_BRASSINS %>%
    select(-any_of("DT_DEB")) %>%
    left_join(DB_BIERES %>% group_by(ID_BRASSIN) %>%
                summarise(DT_DEB = min(DATE),.groups = "drop"))

  info_brassin <- DB_BRASSINS %>% filter(ID_BRASSIN == id_brassin)
  ventes   <- DB_BIERES %>% filter(ID_BRASSIN == id_brassin)

  name_logo <- paste0(info_brassin$`NAME LOGO`,".png")
  path_logo <- NA
  # if (length(name_logo) != 0 & !is.na(name_logo)){
    try({
      path_logo <- paste0("logos/",name_logo)
      id_png <- df_logos %>% filter(name == name_logo) %>% pull(id)
      path_png <- paste0("https://drive.google.com/uc?id=",id_png,"&export=download")
      download.file(path_png,destfile = path_logo,mode = "wb")
      img_magick <- image_read(path_logo) %>% image_scale("200")
      img_grob <- rasterGrob(as.raster(img_magick),interpolate = TRUE)
    },silent = TRUE)
  # }

  # Repérer les autres bières en ventes à ce moment
  context_beers <- DB_BRASSINS %>%
    filter(!BOISSON %in% c("Schieven Architek","Rawette","Suur de BXL"),
           DT_DEB <= info_brassin$DT_FIN,
           DT_FIN >= info_brassin$DT_DEB)

  debut_graph <- info_brassin$DT_BRASSIN-31
  fin_graph <- info_brassin$DT_FIN+31

  context_beers <- context_beers %>%
    mutate(DT_DEB = pmax(DT_DEB,debut_graph),
           DT_FIN = pmin(DT_FIN,fin_graph))

  repartition <- DB_PRODUITS %>%
    filter(ID_BRASSIN == id_brassin) %>%
    group_by(VOLUME_CL) %>%
    summarise(VOLUME = sum(QUANTITE*VOLUME_CL)) %>%
    mutate(
      TYPE = paste0(VOLUME_CL,"cl"),
      VOLUME = VOLUME / sum(VOLUME)
    )

  # Palette de couleurs "Brasserie"
  col_beer_main <- "#f39c12" # Ambrée
  col_beer_dark <- "#d35400" # Sombre
  col_text <- "#2c3e50"
  col_bg <- "#ecf0f1"

  # --- PLOT 1 : TIMELINE DE VIE DU FÛT ---
  p1_timeline <- ggplot() +
    # Les autres bières (en gris)
    geom_segment(data = context_beers,
                 aes(x = DT_DEB, xend = DT_FIN, y = BOISSON, yend = BOISSON),
                 color = "grey80", size = 4) +

    # NOTRE bière (La star)
    # Phase 1: Brassage -> Vente (Production)
    geom_segment(data = info_brassin,
                 aes(x = DT_BRASSIN, xend = DT_DEB, y = NOM_BRASSIN, yend = NOM_BRASSIN),
                 color = col_beer_main, size = 2, linetype = "dotted") +
    # Phase 2: Vente -> Fin (Vie Publique)
    geom_segment(data = info_brassin,
                 aes(x = DT_DEB, xend = DT_FIN, y = NOM_BRASSIN, yend = NOM_BRASSIN),
                 color = col_beer_main, size = 6) +

    # Points clés
    geom_point(data = info_brassin, aes(x = DT_BRASSIN, y = NOM_BRASSIN), color = col_beer_dark, size = 3) +
    geom_text(data = info_brassin, aes(x = DT_BRASSIN, y = NOM_BRASSIN, label = "Brassage"), vjust = 2, size = 3) +

    geom_point(data = info_brassin, aes(x = DT_DEB, y = NOM_BRASSIN), color = col_beer_dark, size = 3) +
    geom_text(data = info_brassin, aes(x = DT_DEB, y = NOM_BRASSIN, label = ""), vjust = 2, size = 3, fontface="bold") +

    geom_text(data = context_beers,
              aes(x = DT_DEB, label = BOISSON,
                  y = reorder(BOISSON, DT_DEB,decreasing=T)),
              hjust = -0.1, vjust = 0, size = 3, color = "grey60") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    labs(title = "1. Chronologie & Contexte", x = "", y = "") +
    theme_minimal(base_size = 16) +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face="bold"))

  # --- PLOT 2 : COURBE DES VENTES (Volume journalier) ---
  p2_sales <- ggplot(ventes, aes(x = DATE, y = VOLUME_JOUR)) +
    # Barres discrètes en fond
    geom_col(fill = col_beer_main, alpha = 0.3, width = 0.8) +
    # Courbe lissée pour la tendance
    geom_smooth(method = "loess", se = FALSE, color = col_beer_dark, size = 1.2, span = 0.2) +
    # Highlight des pics
    geom_point(data = ventes %>% filter(VOLUME_JOUR > 20), color = "red", size = 2) +
    scale_y_continuous(labels = label_number(suffix = " L")) +
    scale_x_date(date_breaks = "1 week", date_labels = "Sem %V\n%d %b %y",minor_breaks = NULL) +
    labs(title = "2. Rythme d'écoulement (Litres/Jour)",x = "", y = "") +
    theme_minimal(base_size = 16) +
    theme(plot.subtitle = element_text(size = 9, color = "grey50"))

  # --- PLOT 3 : KPI & LOGO ---
  # Calcul des stats
  total_vol <- sum(ventes$VOLUME_JOUR)
  total_ca <- sum(ventes$CA_TVAC)
  duree <- as.numeric(max(ventes$DATE) - min(ventes$DATE))
  prix_moyen_L <- total_ca / total_vol

  p3_logo <- ggplot() + theme_void() + labs(title = "3. Fiche d'identité")

  try({
    p3_logo <- p3_logo +
      annotation_custom(img_grob, xmin=-0.15, xmax=Inf, ymin=-Inf, ymax=Inf)
  },silent = T)

  p3_kpi <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "white", color = "white") +
    # Textes Stats
    annotate("text", x = 0.1, y = 0.8, label = info_brassin$NOM_BRASSIN, hjust = 0, size = 6, fontface = "bold") +
    annotate("text", x = 0.1, y = 0.6, label = paste0("Total Vendu: ", round(total_vol, 0), " L / ", info_brassin$VOLUME_BRASSIN, " L"), hjust = 0, size = 5) +
    annotate("text", x = 0.1, y = 0.45, label = paste0("Chiffre d'Affaires HTVA: ", round(total_ca, 0), " €"), hjust = 0, size = 5, color = "darkgreen", fontface="bold") +
    annotate("text", x = 0.1, y = 0.3, label = paste0("Durée de vie: ", duree, " Jours"), hjust = 0, size = 5) +
    annotate("text", x = 0.1, y = 0.15, label = paste0("Rendement: ", round(prix_moyen_L, 1), " €/L"), hjust = 0, size = 5, fontface = "italic", color = "grey50")+
    theme_void(base_size = 16)

  # --- PLOT 4 : DONUT DES FORMATS ---
  p4_donut <- ggplot(repartition, aes(x = 2, y = VOLUME, fill = TYPE)) +
    geom_col(color = "white") +
    scale_fill_brewer(palette = "YlOrBr") +
    geom_text(aes(label = scales::percent(VOLUME,accuracy=1)),
              position = position_stack(vjust = 0.5), size = 5, fontface="bold") +
    labs(title = "4. Formats", fill = "") +
    theme_void(base_size = 16) +
    theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


  layout <- "
AAAAA
BBBBB
CDDDE
"

  final_plot <- p1_timeline + p2_sales + p3_logo + p3_kpi + p4_donut +
    plot_layout(design = layout) + # Hauteur relative des panneaux
    plot_annotation(
      title = paste0("ANALYSE DE BRASSIN : ", info_brassin$NOM_BRASSIN,
                    " ( brassin n°",id_brassin,")"),
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", color = "#2c3e50"),
        plot.subtitle = element_text(size = 14, color = "#7f8c8d"),
        plot.background = element_rect(fill = "#fdfdfd", color = NA)
      )
    )

  final_plot
}

#### Box Ventes ####

box_ventes_jour <- function(db_kpi,db_obj,date_debut,nb_jours,
                            format_date = "%d",titre = "",
                            is_semaine=FALSE,is_midi=TRUE,is_boisson=TRUE,
                            is_objectif=TRUE, width = "14%"){
  plot_kpi <- db_kpi %>%
    left_join(db_obj%>%
                select(-starts_with("CA_")) %>%
                rename(ventes_obj = ventes)) %>%
    filter(DATE >= date_debut,DATE <= date_debut+days(nb_jours)) %>%
    # mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels=vecteur_jours_LOCAL,
    #                              labels = vecteur_jours)) %>%
    mutate(title = paste0(JOUR_SEMAINE," ",format(DATE,format = format_date)))

  if (titre != "") plot_kpi$title <- titre

  plot_kpi <- plot_kpi %>%
    table_kpi(fl_semaine = is_semaine,fl_midi = is_midi,
              fl_boisson = is_boisson,fl_objectif = is_objectif,
              width = width)

  return(
    div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; gap: 1px;",do.call(tagList, plot_kpi))
  )
}

box_ventes_total <- function(db_kpi,db_obj,date_debut,nb_jours,
                            format_date = "%d",titre = "",
                            is_semaine=FALSE,is_midi=TRUE,is_boisson=TRUE,
                            is_objectif=TRUE){
  plot_kpi <- db_kpi %>%
    left_join(db_obj%>%
                select(-starts_with("CA_")) %>%
                rename(ventes_obj = ventes)) %>%
    mutate(ventes_obj = ventes_obj * (ventes>0)) %>%
    filter(DATE >= date_debut,DATE <= date_debut+days(nb_jours)) %>%
    summarise(ventes = sum(ventes,na.rm=TRUE),
              ventes_obj = sum(ventes_obj,na.rm=TRUE),
              Jour = sum(Jour),Soir = sum(Soir),
              Boisson = sum(Boisson),Nourriture = sum(Nourriture),
              Semaine = sum(Semaine),`Week-end` = sum(`Week-end`)) %>%
    mutate(title = titre) %>%
    table_kpi(fl_semaine = is_semaine,fl_midi = is_midi,
              fl_boisson = is_boisson,fl_objectif = is_objectif, width = "100%")

  return(
    div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; gap: 1px;",do.call(tagList, plot_kpi))
  )
}




table_kpi <- function(db,fl_midi=TRUE,fl_boisson=TRUE,
                      fl_semaine=TRUE,fl_objectif=TRUE,width = "14%"){

  list_kpi <- list()
  for (i in 1:nrow(db)){
    ligne <- db[i,]
    title <- ligne$title
    ca <- ligne$ventes
    objectif <- ligne$ventes_obj
    couleur <- get_color_from_gradient(ca,ligne$ventes_obj)
    percent_midi <- round(100 * ligne$Jour / (ligne$Jour+ligne$Soir))
    percent_soir <- 100 - percent_midi
    percent_boisson <- round(100 * ligne$Boisson / (ligne$Boisson+ligne$Nourriture))
    percent_nourriture <- 100 - percent_boisson
    percent_semaine <- round(100 * ligne$Semaine / (ligne$Semaine+ligne$`Week-end`))
    percent_weekend <- 100 - percent_semaine

    if (!fl_midi) {
      percent_midi <- 0
      percent_soir <- 0
    }
    if (!fl_boisson) {
      percent_boisson <- 0
      percent_nourriture <- 0
    }
    if (!fl_semaine) {
      percent_semaine <- 0
      percent_weekend <- 0
    }
    if (!fl_objectif){
      objectif <- NULL
    }

    list_kpi[[i]] <- tagList(caInfoBox(title,ca,percent_midi,
                               percent_soir,percent_boisson,
                               percent_nourriture,percent_semaine,
                               percent_weekend,width,couleur,objectif))
  }
  return(list_kpi)
}


# Fonction pour générer une infoBox avec une info-bulle

generate_bar <- function(percent1, percent2, color1, color2, title) {
  if (percent1 + percent2 > 0) {
    div(
      style = "margin-top: 10px; margin-bottom: 10px;",
      div(
        style = "width: 100%; height: 15px; background-color: #f5f5f5; border-radius: 4px; overflow: hidden; display: flex; position: relative;",
        div(
          style = paste0("flex: ", percent1, "; background-color: ", color1, "; text-align: center; color: white; font-size: 10px; line-height: 15px;"),
          if (percent1 > 0) paste0(percent1, "%") else ""
        ),
        div(
          style = paste0("flex: ", percent2, "; background-color: ", color2, "; text-align: center; color: white; font-size: 10px; line-height: 15px;"),
          if (percent2 > 0) paste0(percent2, "%") else ""
        )
      ),
      p(title, style = "font-size: 12px; margin: 5px 0; color: #666;")
    )
  } else {
    NULL
  }
}

caInfoBox <- function(title, ca, percent_midi, percent_soir, percent_boisson, percent_nourriture, percent_semaine, percent_weekend, width = "300px", ca_color = "#007bff", objectif = NULL) {

  # Contenu des barres (affiché uniquement si le CA est > 0)
  bar_content <- if (ca > 5) {
    tagList(
      generate_bar(
        percent1 = percent_midi, percent2 = percent_soir,
        color1 = "#e67e22", color2 = "#9b59b6",
        title = "Midi / Soir"
      ),
      generate_bar(
        percent1 = percent_boisson, percent2 = percent_nourriture,
        color1 = "#d4ac0d", color2 = "#27ae60",
        title = "Boisson / Nourriture"
      ),
      generate_bar(
        percent1 = percent_semaine, percent2 = percent_weekend,
        color1 = "#2980b9", color2 = "#c0392b",
        title = "Semaine / Week-end"
      )
    )
  } else {
    NULL
  }

  obj_content <- if (!is.null(objectif) && objectif > 0){
    paste0("(objectif : ",format_CA(objectif,-1),")")
  }else{
    " "
  }

  # Structure principale
  div(
    class = "info-box",
    style = paste0("border: 1px solid #dcdcdc; border-radius: 8px; padding: 10px; margin-bottom: 20px; text-align: center; width: ", width, ";"),
    h4(title, style = "margin-bottom: 15px; font-weight: bold;"),
    div(
      style = paste0("font-size: 30px; font-weight: bold; color: ", ca_color, ";"),
      `data-bs-placement` = "top",
      format_CA(ca,-1)
    ),
    div(
      style = paste0("font-size: 12px; color: #666;"),
      `data-bs-placement` = "top",
      obj_content
    ),
    bar_content
  )
}

box_ventes_mois <- function(db_kpi,db_obj,debut_mois,fin_mois,
                            titre = paste(format(DATE,format = "%d/%m"),"->",
                                          format(ceiling_date(DATE, unit = "month")-1,
                                                 format = "%d/%m"))){
  ventes <- db_kpi %>%
    left_join(db_obj %>%
                select(-starts_with("CA_")) %>%
                rename(ventes_obj = ventes)) %>%
    filter(DATE >= debut_mois,DATE <= fin_mois) %>%
    group_by(PREMIER_JOUR_MOIS) %>%
    summarise(Jour=sum(Jour),Semaine =sum(Semaine),Boisson=sum(Boisson),
              Soir=sum(Soir),`Week-end` =sum(`Week-end`),Nourriture=sum(Nourriture),
              nb_jours = n(),CA_HTVA_KEEP=sum(CA_HTVA_KEEP),
              ventes=sum(ventes),ventes_obj=sum(ventes_obj)) %>%
    rename(DATE = PREMIER_JOUR_MOIS) %>%
    mutate(title=titre) %>%
    table_kpi(width="100%")

  div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; gap: 1px;",do.call(tagList, ventes))
}

#### Gauges ####

gauge_calculs <- function(var_tva,periode,lag=0){
  OUT <- DB_OBJECTIFS %>% prepa_db(var_tva) %>%
    select(DATE,OBJECTIF_PCT,obj = ventes) %>%
    left_join(DB_JOURS %>% prepa_db(var_tva) %>% select(DATE,ventes)) %>%
    filter(year(DATE) == year(today()),
           periode(DATE) == periode(today())-lag) %>%
    mutate(SCORE = sum(ventes,na.rm = TRUE) /
             sum(obj*(DATE < today()),na.rm = TRUE),
           SCORE = sum(ventes,na.rm = TRUE) / sum(obj,na.rm = TRUE),
           SCORE = replace_na(SCORE,0),
           OBJ = sum(obj,na.rm = TRUE),
           PCT_OBJ = sum(OBJECTIF_PCT*(ventes == 0),na.rm=TRUE),
           NB_JOURS = sum((DATE >= today())*(OBJECTIF_PCT > 0),na.rm=TRUE),
           ACTU = sum(ventes,na.rm = TRUE),
           DIFF = OBJ - ACTU,
           DIFF_DATE = sum(ventes*(DATE < today()),na.rm = TRUE) -
             sum(obj*(DATE < today()),na.rm = TRUE),
           OBJ_DATE = sum(obj*(DATE < today()),na.rm = TRUE),
           ATTEINT_DATE = sum(ventes*(DATE < today()),na.rm = TRUE),
           ventes_new = ifelse(DATE < today(),NA,DIFF*OBJECTIF_PCT/PCT_OBJ),
           ventes_full = ifelse(DATE < today(),ventes,ventes_new)
    )
  OUT %>% summarise(score = mean(SCORE)*100,
                    atteint = sum(ventes,na.rm = TRUE),
                    reste = sum(ventes_new,na.rm = TRUE),
                    objectif = sum(obj,na.rm = TRUE),
                    ATTEINT_DATE = mean(ATTEINT_DATE,na.rm = TRUE),
                    DIFF_DATE = mean(DIFF_DATE,na.rm = TRUE),
                    OBJ_DATE = mean(OBJ_DATE,na.rm = TRUE),
                    nb_jours = mean(NB_JOURS,na.rm = TRUE))
}

gauge_ventes <- function(db_gauge){
  gauge(db_gauge$score, label = paste0(
    format_CA(db_gauge$atteint,-2),
    "\n sur ",format_CA(db_gauge$objectif,-2)),
        min = 0, max = 100, symbol = "%",
        sectors = gaugeSectors(success = c(100, 1000),
                               warning = c(80, 100),
                               danger = c(0, 80)))
}

gauge_details <- function(db_gauge){
  db_gauge <<- db_gauge
  db_gauge <- db_gauge %>%
    mutate(ratio = (atteint - objectif)/objectif,
           ratio = (ATTEINT_DATE / OBJ_DATE),
           diff = abs(atteint - objectif))

  descriptionBlock(
    number = paste0(round(db_gauge$ratio*100,1),"%"),
    numberColor = ifelse(db_gauge$ratio > 1,"green","red"),
    numberIcon = icon(ifelse(db_gauge$ratio > 1,"caret-up","caret-down")),
    header = format_CA(db_gauge$DIFF_DATE,-1),
    text = ifelse(db_gauge$ratio > 1,"Avance à date","Retard à date"),
    rightBorder = TRUE,
    marginBottom = FALSE
  )
}


#### KPI ####


kpi_cout <- function(db_kpi,db_cout_total){
  # CA
  ca_htva <- db_kpi %>%
    summarise(CA_HTVA=sum(CA_HTVA,na.rm = TRUE)) %>% pull

  DB_TEMP <- db_cout_total %>%
    mutate(TYPE_COUT = ifelse(TYPE_COUT %in% c("Achat","Stock"),
                              "Matières premières","Travail")) %>%
    group_by(SECTEUR,TYPE_COUT) %>%
    summarise(COUT = sum(COUT,na.rm=TRUE))

  # KPI
  mois <- unique(db_kpi$PREMIER_JOUR_MOIS)
  COUT_CA <- sum(DB_TEMP$COUT) / ca_htva
  BOU_CA <- sum(DB_TEMP %>% filter(SECTEUR == "Boulangerie") %>% pull(COUT)) / ca_htva
  CUI_CA <- sum(DB_TEMP %>% filter(SECTEUR == "Cuisine") %>% pull(COUT)) / ca_htva
  SER_CA <- sum(DB_TEMP %>% filter(SECTEUR == "Service") %>% pull(COUT)) / ca_htva
  BRA_CA <- sum(DB_TEMP %>% filter(SECTEUR == "Brasserie") %>% pull(COUT)) / ca_htva
  L_CA <- sum(DB_TEMP %>% filter(TYPE_COUT == "Travail") %>% pull(COUT)) / ca_htva
  MP_CA <- sum(DB_TEMP %>% filter(TYPE_COUT == "Matières premières") %>%
                 pull(COUT)) / ca_htva

  list(PREMIER_JOUR_MOIS = mois,CA = ca_htva,
       COUT_CA = COUT_CA, L_CA = L_CA, MP_CA = MP_CA,CUI_CA = CUI_CA,
       BOU_CA = BOU_CA,SER_CA = SER_CA, BRA_CA = BRA_CA)
}


kpi_cout_total <- function(db_kpi,db_cout_total){
  # CA
  ca_htva <- db_kpi %>% summarise(CA_HTVA=sum(CA_HTVA,na.rm = TRUE)) %>% pull

  DB_TEMP <- db_cout_total %>%
    mutate(SECTEUR = case_when(
      SECTEUR %in% c("Cuisine", "Boulangerie") ~ "Nourriture",
      SECTEUR %in% c("Service", "Brasserie") ~ "Boisson",
      TRUE ~ "Support")) %>%
    mutate(TYPE_COUT = case_when(
      SECTEUR == "Support" & TYPE_COUT == "Achat" ~ "Frais",
      TYPE_COUT %in% c("Achat","Stock") ~ "Mat",
      TRUE ~ "Travail")) %>%
    group_by(SECTEUR,TYPE_COUT) %>%
    summarise(COUT = sum(COUT,na.rm=TRUE))

  # KPI
  mois <- unique(db_kpi$PREMIER_JOUR_MOIS)
  COUT_CA <- sum(DB_TEMP$COUT) / ca_htva
  COUT <- sum(DB_TEMP$COUT)
  BOI_CA <- sum(DB_TEMP %>% filter(SECTEUR == "Boisson") %>% pull(COUT)) / ca_htva
  NOU_CA <- sum(DB_TEMP %>% filter(SECTEUR == "Nourriture") %>% pull(COUT)) / ca_htva
  SUP_CA <- sum(DB_TEMP %>% filter(SECTEUR == "Support") %>% pull(COUT)) / ca_htva
  L_CA <- sum(DB_TEMP %>% filter(TYPE_COUT == "Travail") %>% pull(COUT)) / ca_htva
  MP_CA <- sum(DB_TEMP %>% filter(TYPE_COUT == "Mat") %>% pull(COUT)) / ca_htva
  FG_CA <- sum(DB_TEMP %>% filter(TYPE_COUT == "Frais") %>% pull(COUT)) / ca_htva

  list(PREMIER_JOUR_MOIS = mois,CA = ca_htva,
       COUT_CA = COUT_CA, COUT = COUT, BOI_CA = BOI_CA, NOU_CA = NOU_CA,
       SUP_CA = SUP_CA, L_CA = L_CA,MP_CA = MP_CA,FG_CA = FG_CA)
}


create_kpi_secteur <- function(db_kpi,secteur){
  db_kpi_sub <- db_kpi %>%
    mutate(CA_HTVA = ifelse(CD_SECTEUR != secteur,0,CA_HTVA),
           CA_TVAC = ifelse(CD_SECTEUR != secteur,0,CA_TVAC))

  DB_KPI_JOUR <- db_kpi_sub %>%
    group_by(DATE,CD_PERIODE_JOUR) %>%
    summarise(CA_HTVA = sum(CA_HTVA)) %>%
    pivot_wider(names_from = CD_PERIODE_JOUR,
                values_from = CA_HTVA,values_fill = 0)

  DB_KPI_SEMAINE <- db_kpi_sub %>%
    group_by(DATE,CD_PERIODE_SEMAINE) %>%
    summarise(CA_HTVA = sum(CA_HTVA)) %>%
    pivot_wider(names_from = CD_PERIODE_SEMAINE,
                values_from = CA_HTVA,values_fill = 0)

  DB_KPI_SECTEUR <- db_kpi_sub %>%
    group_by(DATE,CD_SECTEUR) %>%
    summarise(CA_HTVA = sum(CA_HTVA)) %>%
    pivot_wider(names_from = CD_SECTEUR,
                values_from = CA_HTVA,values_fill = 0)

  DB_JOURS %>% select(DATE,CA_HTVA,CA_TVAC) %>%
    left_join(DB_KPI_JOUR) %>%
    left_join(DB_KPI_SEMAINE) %>%
    left_join(DB_KPI_SECTEUR) %>%
    mutate(CA_HTVA = Boisson + Nourriture,
           CA_TVAC = Boisson + Nourriture) %>%
    mutate(CA_HTVA_KEEP = CA_HTVA)
}

valueBox2 <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      # icon(icon, "fa-5x")
                      tagAppendAttributes(icon, class = "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          value
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}

valueBox_perso <- function(value, subtitle, icon, color) {
  div(class = "col-sm-4",
    div(class = "small-box", style = paste0("background-color:", color),
      div(class = "inner",style = "color: white !important;",h3(value),p(subtitle)),
      div(class = "icon-large",tagAppendAttributes(icon, class = "far "))
    )
  )
}

valueBox_nourriture_all <- function(liste_kpi) {

  tagList(
    fluidRow(
      valueBox_perso(
        format_CA(liste_kpi[["CA"]],-1),
        subtitle = "CA HTVA",
        icon = icon(pull(pal_col[pal_col$name == "CA HTVA","icon"])),
        color = pull(pal_col[pal_col$name == "CA HTVA","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["MP_CA"]]),"%"),
        subtitle = "Food Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Nourriture - Achat","icon"])),
        color = pull(pal_col[pal_col$name == "Nourriture - Achat","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["BOU_CA"]]),"%"),
        subtitle = "Boulangerie / CA",
        icon = icon(pull(pal_col[pal_col$name == "Boulangerie - Global","icon"])),
        color = pull(pal_col[pal_col$name == "Boulangerie - Global","col"])
      )
    ),
    fluidRow(
      valueBox_perso(
        paste0(round(100*liste_kpi[["COUT_CA"]]),"%"),
        subtitle = "Prime Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Prime Cost / CA","icon"])),
        color = pull(pal_col[pal_col$name == "Prime Cost / CA","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["L_CA"]]),"%"),
        subtitle = "Work Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Nourriture - Travail","icon"])),
        color = pull(pal_col[pal_col$name == "Nourriture - Travail","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["CUI_CA"]]),"%"),
        subtitle = "Cuisine / CA",
        icon = icon(pull(pal_col[pal_col$name == "Cuisine - Global","icon"])),
        color = pull(pal_col[pal_col$name == "Cuisine - Global","col"])
      )
    )
  )
}


valueBox_boisson_all <- function(liste_kpi) {

  tagList(
    fluidRow(
      valueBox_perso(
        format_CA(liste_kpi[["CA"]],-1),
        subtitle = "CA HTVA",
        icon = icon(pull(pal_col[pal_col$name == "CA HTVA","icon"])),
        color = pull(pal_col[pal_col$name == "CA HTVA","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["MP_CA"]]),"%"),
        subtitle = "Bev Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Boisson - Achat","icon"])),
        color = pull(pal_col[pal_col$name == "Boisson - Achat","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["BRA_CA"]]),"%"),
        subtitle = "Brasserie / CA",
        icon = icon(pull(pal_col[pal_col$name == "Brasserie - Global","icon"])),
        color = pull(pal_col[pal_col$name == "Brasserie - Global","col"])
      )

    ),
    fluidRow(
      valueBox_perso(
        paste0(round(100*liste_kpi[["COUT_CA"]]),"%"),
        subtitle = "Prime Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Prime Cost / CA","icon"])),
        color = pull(pal_col[pal_col$name == "Prime Cost / CA","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["L_CA"]]),"%"),
        subtitle = "Work Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Boisson - Travail","icon"])),
        color = pull(pal_col[pal_col$name == "Boisson - Travail","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["SER_CA"]]),"%"),
        subtitle = "Service / CA",
        icon = icon(pull(pal_col[pal_col$name == "Service - Global","icon"])),
        color = pull(pal_col[pal_col$name == "Service - Global","col"])
      )
    )
  )
}



valueBox_total_all <- function(liste_kpi) {

  tagList(
    fluidRow(
      valueBox_perso(
        format_CA(liste_kpi[["CA"]],-1),
        subtitle = "CA HTVA",
        icon = icon(pull(pal_col[pal_col$name == "CA HTVA","icon"])),
        color = pull(pal_col[pal_col$name == "CA HTVA","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["BOI_CA"]]),"%"),
        subtitle = "Boisson / CA",
        icon = icon(pull(pal_col[pal_col$name == "Boisson - Global","icon"])),
        color =  pull(pal_col[pal_col$name == "Boisson - Global","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["MP_CA"]]),"%"),
        subtitle = "Food & Bev / CA",
        icon = icon(pull(pal_col[pal_col$name == "Global - Achat","icon"])),
        color =  pull(pal_col[pal_col$name == "Global - Achat","col"])
      )
    ),
    fluidRow(
      valueBox_perso(
        format_CA(liste_kpi[["COUT"]],-1),
        subtitle = "Prime Cost",
        icon = icon(pull(pal_col[pal_col$name == "Prime Cost","icon"])),
        color = pull(pal_col[pal_col$name == "Prime Cost","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["NOU_CA"]]),"%"),
        subtitle = "Nourriture / CA",
        icon = icon( pull(pal_col[pal_col$name == "Nourriture - Global","icon"])),
        color =  pull(pal_col[pal_col$name == "Nourriture - Global","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["FG_CA"]]),"%"),
        subtitle = "General Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Support - Achats","icon"])),
        color =  pull(pal_col[pal_col$name == "Support - Achats","col"])
      )
    ),
    fluidRow(
      valueBox_perso(
        paste0(round(100*liste_kpi[["COUT_CA"]]),"%"),
        subtitle = "Prime Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Prime Cost / CA","icon"])),
        color = pull(pal_col[pal_col$name == "Prime Cost / CA","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["SUP_CA"]]),"%"),
        subtitle = "Support / CA",
        icon = icon(pull(pal_col[pal_col$name == "Support - Global","icon"])),
        color =  pull(pal_col[pal_col$name == "Support - Global","col"])
      ),
      valueBox_perso(
        paste0(round(100*liste_kpi[["L_CA"]]),"%"),
        subtitle = "Work Cost / CA",
        icon = icon(pull(pal_col[pal_col$name == "Global - Travail","icon"])),
        color =  pull(pal_col[pal_col$name == "Global - Travail","col"])
      )
    )
  )
}

#### Econométrie ####

predict_fin_brassin <- function(DB_PREDICT,id_brassin){
  if (nrow(DB_PREDICT) == 0) return(c(NA,NA,NA))
  table <- DB_PREDICT %>% filter(ID_BRASSIN == id_brassin)

  zero_LO_50 <- which(table$LO_50 <= 0)[1]
  zero_HI_50 <- which(table$HI_50 <= 0)[1]
  zero_mean <- which(table$VOLUME_RESTANT <= 0)[1]

  if (!is.na(zero_mean)){
    c(table[zero_LO_50,]$DATE,
      table[zero_mean,]$DATE,
      table[zero_HI_50,]$DATE)
  }else{
    c(NA,NA,NA)
  }
}

# Ajout des prédictions
table_evo_brassin <- function(id_brassin,length_predict = 200,
                              max_date=today()){

  serie <- DB_BIERES %>%
    filter(DATE <= max_date) %>%
    filter(ID_BRASSIN == id_brassin)

  if (nrow(serie) == 0) return(NULL)

  serie <- serie %>%
    complete(DATE = seq.Date(min(DATE), max(DATE), by = "1 day")) %>%
    arrange(DATE) %>%
    mutate(across(everything(), ~na.locf(.)),
           VOLUME_RESTANT = VOLUME_BRASSIN_AJUST-VOLUME_TOT)

  boisson <- serie %>% pull(BOISSON) %>% unique()

  actual <- serie %>%
    select(DATE,ID_BRASSIN,BOISSON,VOLUME_RESTANT) %>%
    mutate(FL_PREDICT = FALSE,
           LO_50 = VOLUME_RESTANT,HI_50 = VOLUME_RESTANT,
           LO_75 = VOLUME_RESTANT,HI_75 = VOLUME_RESTANT,
           LO_95 = VOLUME_RESTANT,HI_95 = VOLUME_RESTANT)

  predict <- NULL

  complet <- actual
  try({

    if (nrow(serie) >= 7){

      if (nrow(serie) >= 14)
        fcmodel <- HoltWinters(ts(serie$VOLUME_RESTANT, frequency=7))
      else
        fcmodel <- HoltWinters(ts(serie$VOLUME_RESTANT, frequency=3))
      predict <- forecast(fcmodel, h=length_predict, level=c(50,75,95))
      predict <- tibble(as.data.frame(predict))
      colnames(predict) <- c("VOLUME_RESTANT","LO_50","HI_50",
                             "LO_75","HI_75","LO_95","HI_95")

      predict <- predict %>%
        mutate(DATE = seq(max(serie$DATE)+1, max(serie$DATE)+
                            days(length_predict),by=1),
               ID_BRASSIN = id_brassin,
               BOISSON = boisson,
               FL_PREDICT = TRUE,
               LO_50 = pmax(0,LO_50),HI_50 = pmax(0,HI_50),
               LO_75 = pmax(0,LO_75),HI_75 = pmax(0,HI_75),
               LO_95 = pmax(0,LO_95),HI_95 = pmax(0,HI_95))
    }

    complet <- rbind(actual,predict)
    complet <- complet %>% filter(HI_75 > 0)

  },silent = TRUE)

  complet
}

graph_quali_predict <- function(max_date=today(),nb_jours = 30){

  vec_days <- seq(max_date-nb_jours,max_date,by=1)

  DB_PREDICT_QUALI <- vec_days %>% map_df(table_evo_brassins)

  TEST <- DB_PREDICT_QUALI %>%
    mutate(BIERE = paste0(ID_BRASSIN,"-",BOISSON)) %>%
    filter(VOLUME_RESTANT > 0 & FL_PREDICT) %>%
    arrange(VOLUME_RESTANT) %>%
    group_by(DT_PREDICT,ID_BRASSIN) %>%
    filter(row_number() == 1) %>%
    ungroup()

  ggplot(TEST,aes(x = DT_PREDICT, y = DATE, col=BIERE)) +
    geom_line() +
    labs(title = "Qualité des prédictions",
         x = "Date de la prédiction",
         y = "Date de fin prévu") +
    theme_minimal() +
    scale_x_date(date_breaks = "7 days", date_labels  = "%d/%m") +
    scale_y_date(date_breaks = "7 days", date_labels  = "%d/%m")
}

graph_cluster_bieres <- function(){

  donnees <- DB_BIERES %>%
    filter(VOLUME_BRASSIN > 0 & NB_JOURS_VENTES > 0) %>%
    group_by(ID_BRASSIN) %>%
    arrange(DATE) %>% filter(row_number() == n()) %>% ungroup() %>%
    group_by(FL_FINI,BOISSON,PRICE_33CL) %>%
    summarise(CA_HTVA_TOT = sum(CA_HTVA_TOT),
              NB_JOURS_VENTES = sum(NB_JOURS_VENTES),
              VOLUME_TOT = sum(VOLUME_TOT),
              VOLUME_BRASSIN = sum(VOLUME_BRASSIN_AJUST)) %>%
    ungroup() %>%
    mutate(CA_HTVA_JOUR = CA_HTVA_TOT / NB_JOURS_VENTES,
           VOLUME_JOUR = VOLUME_TOT / NB_JOURS_VENTES,
           PCT = VOLUME_TOT/VOLUME_BRASSIN) %>%
    filter(PCT < 1.5) %>%
    select(BOISSON,VOLUME_JOUR,CA_HTVA_JOUR,PRICE_33CL,PCT,FL_FINI)

  donnees_fini <- donnees %>% filter(FL_FINI)
  donnees_actu <- donnees %>% filter(!FL_FINI)

  donnees_normalisees <- scale(donnees_fini %>% select_if(is.numeric))

  set.seed(123)  # Pour la reproductibilité
  k <- 4  # Nombre de clusters à définir
  clusters <- kmeans(donnees_normalisees, centers=k)

  # Ajouter les résultats de clustering au dataframe
  donnees_fini$cluster <- as.character(clusters$cluster)
  donnees_actu$cluster <- "Brassin en cours"

  donnees <- donnees_fini %>% add_row(donnees_actu)

  ggplot(donnees, aes(x=PRICE_33CL, y=VOLUME_JOUR, label=BOISSON,
                      color=cluster)) +
    geom_point(alpha=0.7) +
    geom_text(nudge_y = 0.5)+
    # scale_y_continuous(labels = scales::percent)+
    labs(title="Clustering des bières",
         y="Volume vendu par jour", x="Prix du 33 cl") +
    theme_mazette()+
    theme_light()
}
