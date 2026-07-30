# Tableau de prédiction de bières
table_evo_brassins <- function(db_bieres,
                               max_date=today(),
                               length_predict = 200,
                               FL_ONLY_FINI = TRUE){
  
  if (is.null(max_date)) max_date <- today()
  
  table_evo_brassin_unique <- function(id_brassin){
    table_evo_brassin(db_bieres,id_brassin,length_predict,max_date)
  }
  
  if (FL_ONLY_FINI){
    vec_brassins_en_cours <- db_bieres %>%
      filter(!BIERE_FINIE & DATE >= max_date-20) %>% pull(ID_BRASSIN) %>% unique
  }else{
    vec_brassins_en_cours <- db_bieres %>%
      filter(DATE >= max_date-20) %>% pull(ID_BRASSIN) %>% unique
  }
  
  vec_brassins_en_cours %>%
    map_df(table_evo_brassin_unique) %>%
    mutate(DT_PREDICT = max_date)
}

# Évolution des brassins

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

# Cartes de niveau des fûts : une par bière en cours, avec sa jauge, son
# volume restant et — si la table de prédiction est fournie — sa date de fin
# prévue. On voit ainsi d'un coup d'œil la bière, son niveau et son échéance.
#
# La jauge est dessinée en CSS (dégradé conique) plutôt qu'en plotly : la
# grille plotly impose un nombre de colonnes fixe au moment du rendu, si bien
# que sur un téléphone les jauges se retrouvaient réduites à quelques dizaines
# de pixels. Ici, c'est la grille CSS qui décide du nombre de colonnes selon
# la largeur réellement disponible.
cartes_niveaux_bieres <- function(niveaux, db_predict = NULL) {
  if (is.null(niveaux) || nrow(niveaux) == 0)
    return(div(class = "text-muted small", "Aucune bière en cours."))
  
  if (!is.null(db_predict) && nrow(db_predict) > 0)
    niveaux <- niveaux %>%
      left_join(predictions_par_brassin(db_predict), by = "ID_BRASSIN")
  if (!"FIN_EST" %in% names(niveaux)) niveaux$FIN_EST <- as.Date(NA)
  
  carte <- function(i) {
    pct <- max(0, min(100, round(niveaux$PCT[i])))
    couleur <- if (pct < 20) COUL_ROUGE else if (pct < 40) COUL_AMBRE else COUL_VERT
    ech <- etiquette_fin_fut(niveaux$FIN_EST[i])
    
    div(
      class = "fut-carte",
      # Les deux variables CSS pilotent le dégradé conique de la jauge
      div(
        class = "fut-jauge",
        style = paste0("--pct:", pct, ";--coul:", couleur, ";"),
        div(class = "fut-jauge-trou",
            span(class = "fut-pct", style = paste0("color:", couleur, ";"),
                 paste0(pct, " %")))
      ),
      div(
        class = "fut-infos",
        div(class = "fut-nom", niveaux$BOISSON[i]),
        div(class = "fut-vol",
            round(niveaux$VOLUME_RESTANT[i]), " / ",
            round(niveaux$VOLUME_TOTAL[i]), " L"),
        div(class = "fut-fin", style = paste0("color:", ech$couleur, ";"),
            ech$texte)
      )
    )
  }
  
  div(class = "fut-grid", lapply(seq_len(nrow(niveaux)), carte))
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
table_evo_brassin <- function(db_bieres,id_brassin,length_predict = 200,
                              max_date=today()){
  
  serie <- db_bieres %>%
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

