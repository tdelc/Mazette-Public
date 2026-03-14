

#### Générique ####

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
      title = paste("ANALYSE DE BRASSIN :", info_brassin$NOM_BRASSIN),
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
                            is_objectif=TRUE){
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
              fl_boisson = is_boisson,fl_objectif = is_objectif)

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
