library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(janitor)
library(googledrive)
library(googlesheets4)
library(readxl)
library(scales)
library(ggridges)
library(ggstats)
library(ggrepel)
library(explore)
library(viridis)
library(plotly)
library(forecast)
library(DT)
library(zoo)
library(parallel)
library(snow)
library(googledrive)
library(grid)
library(magick)
library(patchwork)
library(rhandsontable)

link_json <- Sys.getenv("LINK_JSON")
path_drive <- Sys.getenv("PATH_DRIVE")
id_sheet_mazette <- Sys.getenv("ID_DRIVE_MAZETTE")
path_logos <- Sys.getenv("PATH_LOGOS")

download.file(link_json,destfile = "connect.json")
drive_auth(path = "connect.json")
gs4_auth(path = "connect.json")

df_logos <- googledrive::drive_ls(path_logos)

options(DT.options = list(pageLength = 5, language = list(search = 'Filter:')))

# Configuration des dates
date_before <- today()-lubridate::wday(today(),week_start = 1)-7*7
date_debut_semaine <- today()-lubridate::wday(today(),week_start = 1)+1
date_fin_semaine <- today() + (7 - lubridate::wday(today(), week_start = 1))
date_debut_semaine <- floor_date(today()-2, unit = "week")+1
date_debut_mois <- floor_date(today()-2, unit = "month")+1
date_debut_annee <- floor_date(today()-2, unit = "year")+1

date_debut_semaine_m1 <- date_debut_semaine - 7
date_fin_semaine_m1 <- date_fin_semaine - 7
date_debut_8_semaines <- floor_date(today()-2, unit = "week")-weeks(8)
date_fin_8_semaines <- today()

#### Chargement initial des données ####
# USER$url_active <- session$clientData$url_search
# force_dl <- str_detect(USER$url_active,"force_dl")
force_dl <- F

# Date du jour
# date_jour <- format(now() - hours(8) - minutes(15),format="%Y-%m-%d")
prefix <- "R_env_"
date_jour <- format(now()-days(1),format="%Y-%m-%d")
drive_env_name <- paste0(prefix,date_jour,".RData")

if (!force_dl){
  # Chargement local
  drive_mazette <- try({
    print("Chargement de données locales")
    # env_vec <- list.files("outputs/",pattern = "R_env.*[0-9].RData")
    env_vec <- list.files("outputs/",pattern = drive_env_name)
    env_name <- sort(env_vec)[length(env_vec)]
    load(paste0("outputs/",env_name))
    print(system.time({source("functions.R", local = TRUE)}))
    print(system.time({source("modules.R", local = TRUE)}))
  },silent=TRUE)
}else{
  drive_mazette <- try({ERROR},silent=TRUE)
}

# Chargement via environnement google
if (force_dl | class(drive_mazette)[1] == "try-error"){
  drive_mazette <- try({
    print("Aucune données locales, chargement de l'environnement via google")
    drive_info <- drive_get(path=drive_env_name)
    drive_download(drive_info,
                   path = file.path("outputs",drive_info$name),
                   overwrite = TRUE)
    load(paste0("outputs/",drive_env_name))
    # load(drive_env_name)
    print(system.time({source("functions.R", local = TRUE)}))
    print(system.time({source("modules.R", local = TRUE)}))
  },silent=TRUE)
}

# Chargement via google sheets
if (force_dl | class(drive_mazette)[1] == "try-error"){
  print("Aucune données encore, il faut les charger une par une.")
  print(system.time({source("functions.R", local = TRUE)}))
  print(system.time({source("import.R", local = TRUE)}))
  print(system.time({source("nettoyage_ajout.R", local = TRUE)}))
  print(system.time({source("modules.R", local = TRUE)}))

  # Nouveauté Mai 2025 : prendre la dernière date comme date sauvegarde
  date_jour <- max(DB_JOURS$DATE)
  drive_env_name <- paste0(prefix,date_jour,".RData")

  # save(list = ls(), file = "outputs/env_entier.RData")
  save(list = ls(),
       file = file.path("outputs",drive_env_name),
       compress = "xz",
       compression_level = 9)

  # googledrive::drive_upload("outputs/env_entier.RData",
  googledrive::drive_upload(file.path("outputs",drive_env_name),
                            name = drive_env_name,
                            path = as_id(path_drive),
                            overwrite = TRUE)
}

source("functions.R", local = TRUE)

# Optimisation Bolt : Pré-calcul des jointures lourdes hors du graphe réactif pour améliorer la fluidité
DB_JOURS_PRE <- prepa_db_prejoin(DB_JOURS)
DB_OBJECTIFS_PRE <- prepa_db_prejoin(DB_OBJECTIFS)
DB_PRODUITS_PRE <- prepa_db_prejoin(DB_PRODUITS)
DB_HOREKO_PRE <- prepa_db_prejoin(DB_HOREKO)
DB_KPI_PRE <- prepa_db_prejoin(DB_KPI)
DB_KPI_SIMPLE_PRE <- prepa_db_prejoin(DB_KPI_SIMPLE)

# Optimisation Bolt : Pré-calculer les versions jointes pour éviter les calculs redondants dans le graphe réactif
DB_PRODUITS_JOURS_PRE <- prepa_db_prejoin(DB_PRODUITS_JOURS)
DB_PRODUITS_JOURS_FULL_PRE <- prepa_db_prejoin(DB_PRODUITS_JOURS_FULL)
DB_TICKET_PRE <- prepa_db_prejoin(DB_TICKET)

server <- function(input, output, session) {

  #### Valeurs réactives ####

  local <- reactiveValues()
  USER <- reactiveValues()

  #### Configuration ####

  var_tva <- reactive({if (input$check_tva) "CA_TVAC" else "CA_HTVA"})

  # Optimisation Bolt : Utilisation des bases pré-jointes pour réduire le temps de calcul réactif
  UPD_JOURS <- reactive({prepa_db(DB_JOURS_PRE,var_tva())})
  UPD_OBJECTIFS <- reactive({prepa_db(DB_OBJECTIFS_PRE,var_tva())})
  UPD_PRODUITS <- reactive({prepa_db(DB_PRODUITS_PRE,var_tva())})
  UPD_HOREKO <- reactive({prepa_db(DB_HOREKO_PRE,var_tva())})
  UPD_KPI <- reactive({prepa_db(DB_KPI_PRE,var_tva())})
  UPD_KPI_SIMPLE <- reactive({prepa_db(DB_KPI_SIMPLE_PRE,var_tva())})
  UPD_TICKETS <- reactive({DB_TICKET_PRE})

  # UPD_JOURS <- function() {prepa_db(DB_JOURS,var_tva())}
  # UPD_OBJECTIFS <- function() {prepa_db(DB_OBJECTIFS,var_tva())}
  # UPD_PRODUITS <- function() {prepa_db(DB_PRODUITS,var_tva())}
  # UPD_HOREKO <- function() {prepa_db(DB_HOREKO,var_tva())}
  # UPD_KPI <- function() {prepa_db(DB_KPI,var_tva())}
  # UPD_KPI_SIMPLE <- function() {prepa_db(DB_KPI_SIMPLE,var_tva())}

  # observe({

    # UPD_KPI <- reactive({DB_DATE %>%
    #     left_join(DB_KPI) %>%
    #     mutate_if(is.numeric,replace_na,0) %>%
    #     mutate_if(is.character,replace_na,"") %>%
    #     mutate(ventes = !!sym(var_tva()))})
  # })

  #### Login ####
  observeEvent(input$boutton_log, {

    password <- IMPORT_PASS %>%
      filter(Date_debut <= today(),Date_fin >= today()) %>%
      pull(pass)

    if (input$password %in% password){
      USER$logged <- TRUE
      updateTabItems(session,"tabs","ventes")
    }else{
      output$text_log <- renderText("Erreur dans le mot de passe")
    }
  })

  output$logged <- renderText({
    req(USER$logged)
    if (USER$logged){
      print("log ok")
      return('ok')
    } else {
      print("log not ok")
      return("not_ok")
    }
  })
  outputOptions(output, "logged", suspendWhenHidden = FALSE)

  #### Commentaires ####

  observeEvent(input$add_com, {
    showModal(modalDialog(
      span("Le commentaire sera ajouté à chaque graphique en fonction de la date choisie. L'objectif est d'illustrer les chiffres du jour avec un événement en particulier."),
      textInput("com_text", "Commentaire",
                placeholder = 'Soyez simple et concis'),
      textInput("com_date", "Date concernant ce commentaire",
                placeholder = 'Attention, format obligatoire DD/MM/YYYY'),
      textInput("com_author", "Personne à l'origine du commentaire ",
                placeholder = ''),
      span('Le commentaire sera ajouté lors du prochain chargement du dashboard'),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("com_ok", "Envoyé")
      )
    ))
  })

  observeEvent(input$com_ok, {
    if (!is.null(input$com_text) & !is.null(input$com_date)
        & !is.null(input$com_author)) {
      record <- tibble(TIMESTAMP = today(),
                       DATE = input$com_date,
                       AUTEUR = input$com_author,
                       COMMENTAIRE = input$com_text)
      sheet_append(id_sheet_mazette, record, sheet = "COMMENTAIRES DASHBOARD")
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })

  #### Ventes ####

  output$box_ventes_semaine <- renderUI({
    box_ventes_jour(UPD_KPI_SIMPLE(),UPD_OBJECTIFS(),date_debut_semaine,6)
  })

  output$box_ventes_semaine_total <- renderUI({
    box_ventes_total(UPD_KPI_SIMPLE(),UPD_OBJECTIFS(),
                    date_debut_semaine,6,titre="Total",
                    is_semaine=FALSE)
  })

  output$box_ventes_semaine_m1 <- renderUI({
    1:5 %>% map(~{
      box_ventes_jour(UPD_KPI_SIMPLE(),UPD_OBJECTIFS(),
                        date_debut_semaine - 7*.x,6,
                        is_midi=FALSE,is_boisson=FALSE,is_objectif=FALSE)
    })
  })

  output$box_ventes_semaine_m1_total <- renderUI({
    1:5 %>% map(~{
      box_ventes_total(UPD_KPI_SIMPLE(),UPD_OBJECTIFS(),
                       date_debut_semaine - 7*.x,6,titre="Total",
                       is_semaine=FALSE,is_midi=FALSE,is_boisson=FALSE,
                       is_objectif=FALSE)
    })
  })

  # Choix de la semaine de référence

  observe({
    updateDateInput(session,"ventes_date",
                    value = floor_date(today(), "month") %m-% months(1))
  })

#   DB_DIFF <- DB_KPI_SIMPLE %>%
#     left_join(DB_OBJECTIFS %>% select(-CA_TVAC) %>% rename(CA_HTVA_obj = CA_HTVA)) %>%
#     filter(DATE >= date_debut_semaine) %>%
#     mutate(diff = CA_HTVA-CA_HTVA_obj) %>% select(DATE,diff)
#
#   DB_PRODUITS %>%
#     filter(DATE >= date_debut_semaine) %>%
#     group_by(DATE,PRODUCT_FULL,PRICE) %>%
#     summarise(
#       QUANTITE = sum(QUANTITE),
#       CA_HTVA = sum(CA_HTVA)
#     ) %>%
#     arrange(-CA_HTVA) %>%
#     group_by(DATE) %>%
#     filter(row_number() <= 5) %>%
#     left_join(DB_DIFF) %>%
#     mutate(nb_sup = ifelse(diff > 0,NA,(CA_HTVA/QUANTITE)/diff))
#
# objectifs_CA <- DB_KPI_SIMPLE %>%
#   left_join(DB_OBJECTIFS %>% select(-CA_TVAC) %>% rename(CA_HTVA_obj = CA_HTVA)) %>%
#   filter(DATE <= date_debut_semaine-7,DATE >= date_debut_semaine_m1-7) %>%
#   # filter(DATE >= date_debut_semaine) %>%
#   mutate(diff = CA_HTVA-CA_HTVA_obj) %>% pull(diff) %>% sum
#
# for (k in 1:500){
#   test <- DB_PRODUITS %>%
#     filter(DATE <= date_debut_semaine-7,DATE >= date_debut_semaine_m1-7) %>%
#     group_by(PRODUCT_FULL,PRICE) %>%
#     summarise(
#       QUANTITE = sum(QUANTITE),
#       CA_HTVA = sum(CA_HTVA),.groups = "drop"
#     ) %>%
#     arrange(-CA_HTVA) %>%
#     filter(row_number() <= 8) %>%
#     mutate(
#       k = k,
#       QUANTITE_SIMU = round(QUANTITE*(1+k/100)),
#       CA_HTVA_SIMU = round(CA_HTVA*QUANTITE_SIMU/QUANTITE,1)) %>%
#     mutate(diff = CA_HTVA_SIMU - CA_HTVA)
#
#   if (sum(test$diff) > -objectifs_CA){
#     print(test)
#     break
#   }
# }


  debut_semaine_ventes <- reactive({
    floor_date(input$ventes_date, unit = "week")+1
  })

  output$box_ventes_semaine_mx <- renderUI({
    box_ventes_jour(UPD_KPI_SIMPLE(),UPD_OBJECTIFS(),
                    debut_semaine_ventes(),6,"%d/%m")
  })

  output$box_ventes_semaine_mx_total <- renderUI({
    box_ventes_total(UPD_KPI_SIMPLE(),UPD_OBJECTIFS(),
                     debut_semaine_ventes(),6,titre="Total",
                     is_semaine=FALSE)
  })

  output$box_ventes_semaines <- renderUI({
    plot_kpi <- UPD_KPI_SIMPLE() %>%
      left_join(UPD_OBJECTIFS() %>%
                  select(-starts_with("CA_")) %>%
                  rename(ventes_obj = ventes)) %>%
      filter(DATE >= date_debut_8_semaines,DATE <= date_fin_8_semaines) %>%
      group_by(JOUR_SEMAINE) %>%
      summarise(ventes = mean(ventes,na.rm=TRUE),
                ventes_obj = mean(ventes_obj,na.rm=TRUE),
                Jour = sum(Jour),Soir = sum(Soir),
                Boisson = sum(Boisson),Nourriture = sum(Nourriture),
                Semaine = sum(Semaine),`Week-end` = sum(`Week-end`),
                .groups = "drop") %>%
      # mutate(id_JOUR_SEMAINE = as.numeric) %>%
      # mutate(title = paste0(JOUR_SEMAINE," ",format(DATE,format = format_date))) %>%
      arrange(JOUR_SEMAINE) %>%
      mutate(title = JOUR_SEMAINE) %>%
      table_kpi(fl_semaine = FALSE)

    div(style = "display: flex; flex-wrap: wrap; justify-content: space-between; gap: 1px;",do.call(tagList, plot_kpi))
  })

  observe({
    CA_MOIS <- UPD_JOURS() %>%
      filter(year(DATE) == year(today()),
             month(DATE) == month(today())) %>%
      ungroup() %>%
      summarise(ventes = sum(ventes,na.rm = TRUE)) %>%
      pull()

    OBJECTIF_MOIS <- UPD_OBJECTIFS() %>%
      filter(year(DATE) == year(today()),
             month(DATE) == month(today())) %>%
      ungroup() %>%
      summarise(ventes = sum(ventes,na.rm = TRUE)) %>%
      pull()

    updateProgressBar(session, "month_progress",
                      value = 100*CA_MOIS/OBJECTIF_MOIS,
                      total = max(100,100*CA_MOIS/OBJECTIF_MOIS))
  })

  # Optimisation Bolt : Mettre en cache le graphique d'évolution des ventes car il nécessite des calculs cumulatifs et une conversion Plotly
  output$graph_ventes_mois <- renderPlotly({
    graph_evo_ventes_mois(UPD_JOURS(), UPD_OBJECTIFS(), 4) %>% style(textposition = "right")
  }) %>% bindCache(input$check_tva, date_jour)

  #### Evo Nourriture ####

  prepa_data_nourriture <- reactive({
    date_debut_4_semaines <- floor_date(today()-4, unit = "week")-weeks(4)
    table <- UPD_PRODUITS() %>%
      filter(CATEGORY %in% c("SALÉ","SUCRÉ"),ventes > 0) %>%
      filter(DATE >= date_debut_4_semaines,BOISSON == "") %>%
      group_by(CATEGORY,PRODUCT,PREMIER_JOUR_SEMAINE) %>%
      summarise(ventes = paste0(format_CA(sum(ventes,na.rm=T)),
                                " (",round(sum(QUANTITE),1),")"),
                .groups = "drop") %>%
      arrange(PREMIER_JOUR_SEMAINE) %>%
      pivot_wider(names_from = PREMIER_JOUR_SEMAINE,values_from = ventes) %>%
      arrange(PRODUCT)

    k <- 3:ncol(table)
    colnames(table)[k] <- paste("Semaine du",format(ymd(colnames(table)[k]), "%d/%m"))
    table
  })

  output$nourriture_evo_sucres <- renderDataTable({
    prepa_data_nourriture() %>%
      filter(CATEGORY == "SUCRÉ") %>% select(-CATEGORY) %>%
      datatable_simple()
  })

  output$nourriture_evo_sales <- renderDataTable({
    prepa_data_nourriture() %>%
      filter(CATEGORY == "SALÉ") %>% select(-CATEGORY) %>%
      datatable_simple()
  })


  #### Nourriture ####

  observe({
    updateDateInput(session,"nourriture_date",
                    value = floor_date(today(), "month") %m-% months(3))
  })

  observeEvent(input$nourriture_date,{
    updateDateInput(session,"nourriture_date_comp1",
                    value = input$nourriture_date-months(1))
  })

  observeEvent(input$nourriture_date,{
    updateDateInput(session,"nourriture_date_comp2",
                    value = input$nourriture_date-years(1))
  })

  comparaisonServer(
    id = "nourriture_main",date_reactive = reactive(input$nourriture_date),
    label_suff = "", secteur = "Nourriture", db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  comparaisonServer(
    id = "nourriture_comp1", date_reactive = reactive(input$nourriture_date_comp1),
    label_suff = "_comp1", secteur = "Nourriture", db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  comparaisonServer(
    id = "nourriture_comp2", date_reactive = reactive(input$nourriture_date_comp2),
    label_suff = "_comp2",secteur = "Nourriture",db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  output$graph_nourriture_evo_cout <- renderPlot({
    graph_evo_cout(input$nourriture_date,"Nourriture")
  })

  output$graph_nourriture_evo_kpi <- renderPlotly({
    graph_evo_kpi(input$nourriture_date,"Nourriture")
  })

  #### Evo Boisson ####

  prepa_data_boisson <- reactive({
    date_debut_4_semaines <- floor_date(today()-4, unit = "week")-weeks(4)
    table <- UPD_PRODUITS() %>%
      filter(! CATEGORY %in% c("SALÉ","SUCRÉ")) %>%
      filter(DATE >= date_debut_4_semaines,ventes > 0) %>%
      group_by(CATEGORY,BOISSON,PREMIER_JOUR_SEMAINE) %>%
      summarise(ventes = paste0(format_CA(sum(ventes,na.rm=T)),
                                " (",round(sum(VOLUME_TOT_L),1),"L)"),
                .groups = "drop") %>%
      arrange(PREMIER_JOUR_SEMAINE) %>%
      pivot_wider(names_from = PREMIER_JOUR_SEMAINE,values_from = ventes) %>%
      arrange(BOISSON)

    k <- 3:ncol(table)
    colnames(table)[k] <- paste("Semaine du",format(ymd(colnames(table)[k]), "%d/%m"))
    table
  })

  output$boisson_evo_bieres <- renderDataTable({
    prepa_data_boisson() %>%
      filter(CATEGORY == "BIÈRES") %>% select(-CATEGORY) %>%
      datatable_simple()
  })

  output$boisson_evo_alcools <- renderDataTable({
    prepa_data_boisson() %>%
      filter(CATEGORY == "ALCOOLS & VINS") %>% select(-CATEGORY) %>%
      datatable_simple()
  })

  output$boisson_evo_softs <- renderDataTable({
    prepa_data_boisson() %>%
      filter(CATEGORY == "SOFTS") %>% select(-CATEGORY) %>%
      datatable_simple()
  })

  output$boisson_evo_chaud <- renderDataTable({
    date_debut_4_semaines <- floor_date(today()-4, unit = "week")-weeks(4)
    UPD_PRODUITS() %>%
      filter(! CATEGORY %in% c("SALÉ","SUCRÉ")) %>%
      filter(DATE >= date_debut_4_semaines,ventes > 0) %>%
      group_by(CATEGORY,PRODUCT,PREMIER_JOUR_SEMAINE) %>%
      summarise(ventes = paste0(format_CA(sum(ventes,na.rm=T)),
                                " (",round(sum(QUANTITE),1),")"),
                .groups = "drop") %>%
      arrange(PREMIER_JOUR_SEMAINE) %>%
      pivot_wider(names_from = PREMIER_JOUR_SEMAINE,values_from = ventes) %>%
      arrange(PRODUCT) %>%
      filter(CATEGORY == "BOISSONS CHAUDES") %>% select(-CATEGORY) %>%
      datatable_simple()
  })

  #### Boisson ####

  observe({
    updateDateInput(session,"boisson_date",
                    value = floor_date(today(), "month") %m-% months(3))
  })

  observeEvent(input$boisson_date,{
    updateDateInput(session,"boisson_date_comp1",
                    value = input$boisson_date-months(1))
  })

  observeEvent(input$boisson_date,{
    updateDateInput(session,"boisson_date_comp2",
                    value = input$boisson_date-years(1))
  })

  comparaisonServer(
    id = "boisson_main",date_reactive = reactive(input$boisson_date),
    label_suff = "", secteur = "Boisson", db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  comparaisonServer(
    id = "boisson_comp1", date_reactive = reactive(input$boisson_date_comp1),
    label_suff = "_comp1", secteur = "Boisson", db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  comparaisonServer(
    id = "boisson_comp2", date_reactive = reactive(input$boisson_date_comp2),
    label_suff = "_comp2",secteur = "Boisson",db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  output$graph_boisson_evo_cout <- renderPlot({
    graph_evo_cout(input$boisson_date,"Boisson")
  })

  output$graph_boisson_evo_kpi <- renderPlotly({
    graph_evo_kpi(input$boisson_date,"Boisson")
  })

  #### Comptes complet ####

  observe({
    updateDateInput(session,"comptes_date",
                    value = floor_date(today(), "month") %m-% months(3))
  })

  observeEvent(input$comptes_date,{
    updateDateInput(session,"comptes_date_comp1",
                    value = input$comptes_date-months(1))
  })

  observeEvent(input$comptes_date,{
    updateDateInput(session,"comptes_date_comp2",
                    value = input$comptes_date-years(1))
  })

  comparaisonServer(
    id = "comptes_main",date_reactive = reactive(input$comptes_date),
    label_suff = "", secteur = "Total", db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  comparaisonServer(
    id = "comptes_comp1", date_reactive = reactive(input$comptes_date_comp1),
    label_suff = "_comp1", secteur = "Total", db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  comparaisonServer(
    id = "comptes_comp2", date_reactive = reactive(input$comptes_date_comp2),
    label_suff = "_comp2", secteur = "Total", db_kpi = UPD_KPI_SIMPLE,
    db_obj = UPD_OBJECTIFS, var_tva = var_tva
  )

  output$graph_comptes_evo_cout <- renderPlot({
    graph_evo_cout(input$comptes_date,"Global")
  })

  output$graph_comptes_evo_kpi <- renderPlotly({
    graph_evo_kpi(input$comptes_date,"Global")
  })

  #### Répartition des ventes  ####

  observe({
    updateDateInput(session,"ventes_repartition_date_max",value = today())
    updateDateInput(session,"ventes_repartition_date_min",
                    value = floor_date(today(), "month") %m-% months(3))
  })

  output$table_ventes_repartition_CA <- renderDataTable({

    test_max <- today()
    test_min <- floor_date(today(), "month") %m-% months(3)

    DB_PRODUITS_JOURS_PRE %>%
      filter(DATE > input$ventes_repartition_date_min,
             DATE <= input$ventes_repartition_date_max,
             # CATEGORY %in% c("A GRIGNOTER (MAIN)","A GRIGNOTER (SUB)",
             #                 "DESSERTS","DIKKEBROODJES",
             #                 "LUNCHS & SUGGESTIONS","PETITS DEJ'S & BRUNCHS",
             #                 "PLANCHE","SOUPE")) %>%
              CATEGORY %in% c("SALÉ","SUCRÉ","PETITS DEJ'S & BRUNCHS")) %>%
      group_by(JOUR_SEMAINE,DATE,CD_HEURE) %>%
      summarise(CA_HTVA = sum(CA_HTVA),.groups = "drop") %>%
      group_by(JOUR_SEMAINE,CD_HEURE) %>%
      summarise(CA_HTVA=format_CA(mean(CA_HTVA)),.groups = "drop") %>%
      pivot_wider(names_from = CD_HEURE,values_from = CA_HTVA) %>%
      datatable_simple()
  })

  output$table_ventes_repartition_items <- renderDataTable({

    SYNTHESE <- DB_PRODUITS_JOURS_PRE %>%
      filter(DATE > input$ventes_repartition_date_min,
             DATE <= input$ventes_repartition_date_max,
             # CATEGORY %in% c("A GRIGNOTER","DESSERTS","DIKKEBROODJES",
             #                 "LUNCHS & SUGGESTIONS","PETITS DEJ'S & BRUNCHS")) %>%
             CATEGORY %in% c("SALÉ","SUCRÉ","PETITS DEJ'S & BRUNCHS")) %>%
      group_by(JOUR_SEMAINE,CD_HEURE,CATEGORY) %>%
      summarise(QUANTITE=round(mean(QUANTITE)),.groups = "drop") %>%
      pivot_wider(names_from = c(CD_HEURE,CATEGORY),values_from = QUANTITE)

    # a custom table container
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'JOUR_SEMAINE'),
          th(colspan = 5, 'Midi (<17h)'),
          th(colspan = 5, 'Soir (>=17h)')
        ),
        tr(
          lapply(rep(c(
            # "A GRIGNOTER","DESSERTS","DIKKEBROODJES",
            # "LUNCHS & SUGGESTIONS","PETITS DEJ'S & BRUNCHS"
            "SALÉ","SUCRÉ","PETITS DEJ'S & BRUNCHS"
            ), 2), th)
        )
      )
    ))

    datatable(
      SYNTHESE, container = sketch, rownames = FALSE,
      options = list(
        dom = 't', # 't' pour "table" - affiche uniquement le tableau sans contrôles
        paging = FALSE, # Désactive la pagination
        ordering = FALSE, # Désactive le tri
        searching = FALSE # Désactive la recherche
      )
    )
  })

  db_ventes_items <- reactive({
    DB_PRODUITS_JOURS_PRE %>%
      filter(DATE > input$ventes_repartition_date_min,
             DATE <= input$ventes_repartition_date_max,
             CATEGORY %in% c(
               # "A GRIGNOTER (MAIN)","DESSERTS","DIKKEBROODJES",
               # "LUNCHS & SUGGESTIONS","PETITS DEJ'S & BRUNCHS",
               # "PLANCHE","SOUPE"
               "SALÉ","SUCRÉ","PETITS DEJ'S & BRUNCHS"
               ),
             JOUR_SEMAINE != "lundi") %>%
      mutate(CATEGORY = case_when(
        # CATEGORY == "A GRIGNOTER (MAIN)" ~ "GRIGNOTER",
        # CATEGORY == "LUNCHS & SUGGESTIONS" ~ "LUNCHS",
        CATEGORY == "PETITS DEJ'S & BRUNCHS" ~ "BRUNCHS",
        TRUE ~ CATEGORY
      )) %>%
      group_by(JOUR_SEMAINE,CD_HEURE,CATEGORY) %>%
      summarise(QUANTITE=round(mean(QUANTITE)),.groups = "drop")
  })

  output$table_ventes_items_midi <- renderDataTable({
    db_ventes_items() %>%
      filter(CD_HEURE == "Midi (<17h)",
             !CATEGORY %in% c("PLANCHE")) %>%
      select(-CD_HEURE) %>%
      pivot_wider(names_from = c(CATEGORY),values_from = QUANTITE) %>%
      datatable_simple()
  })

  output$table_ventes_items_soir <- renderDataTable({
    db_ventes_items() %>%
      filter(CD_HEURE == "Soir (>=17h)",
             !CATEGORY %in% c("BRUNCHS","LUNCHS")) %>%
      select(-CD_HEURE) %>%
      pivot_wider(names_from = c(CATEGORY),values_from = QUANTITE) %>%
      datatable_simple()
  })

  output$graph_ventes_items_midi <- renderPlot({
    db_ventes_items() %>%
      filter(CD_HEURE == "Midi (<17h)",
             QUANTITE > 0) %>%
      ggplot() +
      aes(fill=CATEGORY,values = QUANTITE) +
      geom_waffle(color = "white", size = 0.5, n_rows = 6) +
      facet_wrap(~JOUR_SEMAINE, ncol=1,strip.position = "left") +
      scale_fill_manual(values = c("#69b3a2", "#404080", "#FFA07A", "#FFD700", "#FF6347"))
    theme_void() +
      theme(legend.position = "bottom")
  })

  output$graph_ventes_items_soir <- renderPlot({
    db_ventes_items() %>%
      filter(CD_HEURE == "Soir (>=17h)",
             CATEGORY != "PETITS DEJ'S & BRUNCHS",
             QUANTITE > 0) %>%
      ggplot() +
      aes(fill=CATEGORY,values = QUANTITE) +
      geom_waffle(color = "white", size = 0.5, n_rows = 6) +
      facet_wrap(~JOUR_SEMAINE, ncol=1,strip.position = "left") +
      scale_fill_manual(values = c("#404080", "#FFA07A", "#FFD700"))
    theme_void() +
      theme(legend.position = "bottom")

  })

  output$graph_ventes_items <- renderPlot({

    db_ventes_items() %>%
      complete(JOUR_SEMAINE, CATEGORY, CD_HEURE,fill = list(QUANTITE = 0.0001)) %>%
      mutate(QUANTITE = pmax(QUANTITE,0.0001)) %>%
      ggplot() +
      aes(fill=CATEGORY,values = QUANTITE) +
      geom_waffle(color = "white", size = 0.5, n_rows = 6) +
      facet_grid(rows = vars(JOUR_SEMAINE),
                 cols = vars(CD_HEURE),
                 switch="both") +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14)
      )

  })



  #### Boulangerie ####

  output$boulangerie_table_focaccia <- renderTable({
    UPD_PRODUITS() %>%
      filter(DATE > date_before & DATE <= today()) %>%
      filter(str_detect(PRODUCT,"[Ff]ocaccia")) %>%
      mutate(TYPE = case_when(
        str_detect(PRODUCT,"[Vv]iande") & str_detect(PRODUCT,"[Ff]romage") ~ "full",
        str_detect(PRODUCT,"[Vv]iande") ~ "+viande",
        str_detect(PRODUCT,"[Ff]romage") ~ "+fromage",
        TRUE ~ "base")) %>%
      group_by(PREMIER_JOUR_SEMAINE,TYPE) %>%
      summarise(QUANTITE = round(sum(QUANTITE,na.rm=TRUE)),.groups = "drop") %>%
      mutate(QUANTITE = as.character(QUANTITE)) %>%
      pivot_wider(names_from = "PREMIER_JOUR_SEMAINE",values_from = "QUANTITE")
  })

  output$boulangerie_plot_focaccia <- renderPlot({
    UPD_PRODUITS() %>%
      filter(DATE > date_before & DATE <= today()) %>%
      filter(str_detect(PRODUCT,"[Ff]ocaccia")) %>%
      mutate(TYPE = case_when(
        str_detect(PRODUCT,"[Vv]iande") & str_detect(PRODUCT,"[Ff]romage") ~ "full",
        str_detect(PRODUCT,"[Vv]iande") ~ "+viande",
        str_detect(PRODUCT,"[Ff]romage") ~ "+fromage",
        TRUE ~ "base")) %>%
      group_by(PREMIER_JOUR_SEMAINE,TYPE) %>%
      summarise(QUANTITE = round(sum(QUANTITE,na.rm=TRUE)),.groups = "drop") %>%
      ggplot()+
      aes(x=PREMIER_JOUR_SEMAINE,y=QUANTITE,fill=TYPE)+
      geom_bar(position="fill", stat="identity")+
      scale_y_continuous(labels = scales::percent_format())+
      theme_mazette() +
      theme(legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            plot.caption = element_text(size = 12)) +
      labs(title = "Répartition des ventes par semaine")
  })

  output$table_focaccia_previous <- renderPlot({
    table_produits(UPD_PRODUITS() %>%
                     filter(DATE > date_before & DATE <= date_debut_semaine) %>%
                     filter(str_detect(PRODUCT,"[fF]ocaccia")))
  }, bg = "transparent")

  output$table_focaccia_actual <- renderPlot({
    table_produits(UPD_PRODUITS() %>%
                     filter(DATE > date_debut_semaine & DATE <= today()) %>%
                     filter(str_detect(PRODUCT,"[fF]ocaccia")))
  }, bg = "transparent")

  output$table_brunch_previous <- renderPlot({
    table_produits(UPD_PRODUITS() %>%
                     filter(DATE > date_before & DATE <= date_debut_semaine) %>%
                     filter(str_detect(PRODUCT,"Brioche") | str_detect(PRODUCT,"Brunch")) %>%
                     filter(PRICE > 0))
  }, bg = "transparent")

  output$table_brunch_actual <- renderPlot({
    table_produits(UPD_PRODUITS() %>%
                     filter(DATE > date_debut_semaine & DATE <= today()) %>%
                     filter(str_detect(PRODUCT,"Brioche") | str_detect(PRODUCT,"Brunch")) %>%
                     filter(PRICE > 0))
  }, bg = "transparent")

  #### Horaires ####

  # Optimisation Bolt : Mettre en cache le graphique des horaires car le lissage Loess sur de gros volumes de tickets est lourd
  output$graph_horaires_CA <- renderPlot({

    if (input$config_horaires_tempo == "Semaines"){
      date_debut <- date_debut_semaine - 7*input$config_horaires_nb
    } else if (input$config_horaires_tempo == "Mois"){
      date_debut <- date_debut_mois - 30*input$config_horaires_nb
    } else if (input$config_horaires_tempo == "Années") {
      date_debut <- date_debut_annee - 365*input$config_horaires_nb
    } else {
      date_debut <- date_debut_semaine - 7*input$config_horaires_nb
    }

    date_debut_ref <- date_debut_semaine - 7*input$config_horaires_ref
    date_fin_ref <- date_debut_ref+7
    ref_texte <- paste("Semaine du",date_debut_ref,"en pointillé.")

    date_debut <- UPD_TICKETS() %>%
      filter(DATE >= date_debut) %>%
      summarise(DATE = min(DATE)) %>%
      pull(DATE)

    aire_texte <- paste("Données à partir de", date_debut, "pour l'aire.")

    # Optimisation Bolt : Suppression des calculs inutilisés et filtrage par date avant le filtrage textuel (regex)
    TICKETS <- UPD_TICKETS() %>%
      filter(DATE >= date_debut, DATE <= today() - 1)

    if (input$config_horaires_text != "") {
      TICKETS <- TICKETS %>%
        filter(str_detect(
          str_to_lower(PRODUCT),
          str_to_lower(input$config_horaires_text)
        ))
    }

    TEST <- TICKETS %>%
      mutate(HEURE = hour(TIMESTAMP),
             HEURE = if_else(HEURE<8,HEURE+24,HEURE)) %>%
      group_by(DATE, HEURE) %>%
      summarise(PRIX_TOTAL = sum(PRIX_TOTAL),.groups = "drop") %>%
      full_join(DB_DATE %>% filter(DATE >= date_debut,
                                  DATE <= today()-1) %>% select(DATE, JOUR_SEMAINE), by = "DATE") %>%
      complete(DATE, HEURE, fill = list(PRIX_TOTAL = 0)) %>%
      mutate(HEURE = ifelse(is.na(HEURE),0,HEURE)) %>%
      filter(JOUR_SEMAINE != "lundi") %>%
      mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels = vecteur_jours)) %>%
      group_by(JOUR_SEMAINE,HEURE) %>%
      summarise(PRIX_TOTAL = mean(PRIX_TOTAL),.groups = "drop")

    TEST2 <- TICKETS %>%
      filter(DATE >= date_debut_ref & DATE <= date_fin_ref) %>%
      mutate(HEURE = hour(TIMESTAMP),
             HEURE = if_else(HEURE<8,HEURE+24,HEURE)) %>%
      group_by(DATE, HEURE) %>%
      summarise(PRIX_TOTAL = sum(PRIX_TOTAL),.groups = "drop") %>%
      full_join(DB_DATE %>% filter(DATE >= date_debut_ref,
                                   DATE <= date_fin_ref) %>% select(DATE, JOUR_SEMAINE), by = "DATE") %>%
      complete(DATE, HEURE, fill = list(PRIX_TOTAL = 0)) %>%
      mutate(HEURE = ifelse(is.na(HEURE),0,HEURE)) %>%
      filter(JOUR_SEMAINE != "lundi") %>%
      mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels = vecteur_jours)) %>%
      group_by(JOUR_SEMAINE,HEURE) %>%
      summarise(PRIX_TOTAL = mean(PRIX_TOTAL),.groups = "drop")

    p <- ggplot(TEST) +
      aes(x = HEURE,y=PRIX_TOTAL,fill=JOUR_SEMAINE)+
      # geom_area()+
      stat_smooth(geom = 'area', method = 'loess', span = 1/3,alpha = 1/2) +
      stat_smooth(data=TEST2,aes(col=JOUR_SEMAINE),
                  geom = 'line', method = 'loess', span = 1/3, lty=2) +
      scale_fill_hue(drop   = FALSE) +
      scale_colour_hue(drop = FALSE) +
      facet_wrap(~JOUR_SEMAINE,axes = "all_x")+
      scale_x_continuous(breaks = c(10,12,14,16,18,20,22,24))+
      scale_y_continuous(labels = dollar_format(
        suffix = "€", prefix = "", big.mark = ".",decimal.mark = ","),
        limit=c(0,NA),oob=squish)+
      theme_mazette() +
      theme(legend.position = "none",strip.text = element_text(size = 14),
            plot.caption = element_text(size = 12)) +
      labs(caption = paste(aire_texte,ref_texte))

    p
  }) %>% bindCache(
    input$config_horaires_tempo,
    input$config_horaires_nb,
    input$config_horaires_ref,
    input$config_horaires_text,
    date_jour
  )

  #### Produits ####

  prepa_table_category <- reactive({
    table <- UPD_PRODUITS() %>%
      filter(CATEGORY != "",ventes > 0,QUANTITE > 0) %>%
      filter(DATE >= floor_date(today()-2, unit = "week")-weeks(3),
             DATE <= today()-1) %>%
      group_by(PREMIER_JOUR_SEMAINE,CATEGORY)

    if (input$produits_evo)
      table <- table %>% summarise(n = sum(QUANTITE,na.rm = T),.groups = "drop")
    else
      table <- table %>% summarise(n = round(sum(ventes,na.rm = TRUE)),.groups = "drop")

    table <- table %>% pivot_wider(names_from = "PREMIER_JOUR_SEMAINE",values_from = "n")
    k <- 2:ncol(table)
    colnames(table)[k] <- paste("Semaine du",format(ymd(colnames(table)[k]), "%d/%m"))
    table
  })

  output$table_category_evo <- renderDT({
    dt <- prepa_table_category() %>%
      rename(Catégorie = CATEGORY) %>%
      datatable(filter = 'top', rownames= FALSE,
      options = list( pageLength = 15, dom = 't',
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )

    if (input$produits_evo)
      dt
    else
      dt %>% formatCurrency(-1, '€',before = F,digits =0,mark = ".",)
  })

  prepa_table_produits <- reactive({
    table <- UPD_PRODUITS() %>%
      filter(DATE >= floor_date(today()-2, unit = "week")-weeks(3),
             DATE <= today()-1) %>%
      group_by(PREMIER_JOUR_SEMAINE,CATEGORY,PRODUCT_FULL)

    if (input$produits_evo)
      table <- table %>% summarise(n = sum(QUANTITE,na.rm = T),.groups = "drop")
    else
      table <- table %>% summarise(n = round(sum(ventes,na.rm = TRUE)),.groups = "drop")

    table <- table %>% pivot_wider(names_from = "PREMIER_JOUR_SEMAINE",values_from = "n")
    k <- 3:ncol(table)
    colnames(table)[k] <- paste("Semaine du",format(ymd(colnames(table)[k]), "%d/%m"))
    table
  })

  output$table_produits_evo <- renderDT({
    dt <- prepa_table_produits() %>%
      rename(Catégorie = CATEGORY,Produit = PRODUCT_FULL) %>%
      datatable(filter = 'top', rownames= FALSE,
      options = list( pageLength = 15,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )

    if (input$produits_evo)
      dt
    else
      dt %>% formatCurrency(-c(1:2), '€',before = F,digits =0,mark = ".")
  })

  # observe({
  #   mois_dispo <- UPD_PRODUITS() %>%
  #     filter(DATE <= today() & ventes > 0) %>%
  #     arrange(DATE) %>%
  #     mutate(MOIS = format(DATE,"%m/%y")) %>%
  #     pull(MOIS) %>%
  #     unique()
  #
  #   # QUatre derniers mois
  #   debut_periode <- mois_dispo[1]
  #   fin_periode <- mois_dispo[length(mois_dispo)]
  #
  #   updateSliderTextInput(session,"produits_periode",
  #                         choices = mois_dispo,
  #                         selected = c(debut_periode,fin_periode))
  #
  # })

  # observe({
  #   vec_category <- sort(unique(UPD_PRODUITS()$CATEGORY))
  #
  #   updateCheckboxGroupButtons(session,"category_list",status = "custom-class2",
  #                              choices = vec_category,selected = vec_category)
  # })

  # data_produits <- reactive({
  #
  #   debut_date <- my(input$produits_periode[1])
  #   debut_date <- ymd(paste(year(debut_date),month(debut_date),1))
  #
  #   fin_date <- today()
  #   fin_date <- ymd(paste(year(fin_date),month(fin_date),1))+months(1)
  #
  #   TABLE <- UPD_PRODUITS() %>% filter(DATE >= debut_date, DATE <= fin_date)
  #
  #   if (input$produits_text != "")
  #     TABLE <- TABLE %>%
  #     filter(str_detect(str_to_lower(PRODUCT),
  #                       str_to_lower(input$produits_text)) |
  #              str_detect(str_to_lower(CATEGORY),
  #                         str_to_lower(input$produits_text)))
  #
  #   TABLE
  # })

  # observe({
  #   vec_produits <- data_produits() %>%
  #     filter(CATEGORY %in% input$category_list) %>%
  #     select(PRODUCT) %>% pull()
  #
  #   vec_produits <- sort(unique(vec_produits))
  #
  #   if (length(vec_produits) > 100) vec_produits <- "Trop de produits"
  #
  #   updateCheckboxGroupButtons(session,"produits_list",status = "custom-class",
  #                              choices = vec_produits,selected = vec_produits)
  # })

  data_produits_filter <- reactive({

    # TEMP_JOURS <- data_produits()
    # TEMP_JOURS <- TEMP_JOURS %>% filter(CATEGORY %in% input$category_list)
    #
    # if (input$produits_list[1] != "Trop de produits")
    #   TEMP_JOURS <- TEMP_JOURS %>% filter(PRODUCT %in% input$produits_list)
    #
    # TEMP_JOURS

    df <- UPD_PRODUITS()

    s = input$table_category_evo_rows_selected
    if (length(s) > 0) {
      extract_table_category <- prepa_table_category()[s,]
      vec_category <- sort(unique(extract_table_category$CATEGORY))
      df <- df %>% filter(CATEGORY %in% vec_category)
    }

    s = input$table_produits_evo_rows_selected
    if (length(s) > 0) {
      extract_table_produits <- prepa_table_produits()[s,]
      vec_produits <- sort(unique(extract_table_produits$PRODUCT_FULL))
      df <- df %>% filter(PRODUCT_FULL %in% vec_produits)
    }

    df
  })

  output$liste_produits <- renderText({
    vec_produits <- sort(unique(data_produits_filter()$PRODUCT_FULL))
    vec_category <- sort(unique(data_produits_filter()$CATEGORY))
    if (length(vec_produits) > 30)
      paste0("Liste des catégories : ", paste(vec_category,collapse = ", "))
    else
      paste0("Liste des produits : ", paste(vec_produits,collapse = ", "))
  })

  data_produits_jours <- reactive({
    data_produits_filter() %>%
      filter(DATE >= "2023-07-01") %>%
      group_by(DATE,ANNEE_MOIS,ANNEE_SEMAINE,JOUR_SEMAINE,
               PREMIER_JOUR_MOIS,PREMIER_JOUR_SEMAINE,
               COMMENTAIRE_FULL) %>%
      summarise(ventes = sum(ventes,na.rm = TRUE),.groups = "drop")
  })

  output$graph_CA_produits <- renderPlotly({
    ggplotly(graph_evo_ventes_LT(data_produits_jours(),
                                 input$produits_indic),tooltip = "text")})

  output$table_produits_previous <- renderPlot({
    table_produits(data_produits_filter() %>%
                     filter(DATE > date_before & DATE <= date_debut_semaine))
  }, bg = "transparent")

  output$table_produits_actual <- renderPlot({
    table_produits(data_produits_filter() %>%
                     filter(DATE > date_debut_semaine & DATE <= today()))
  }, bg = "transparent")

  db_produits_items <- reactive({

    debut_date <- my(input$produits_periode[1])
    debut_date <- ymd(paste(year(debut_date),month(debut_date),1))

    fin_date <- today()
    fin_date <- ymd(paste(year(fin_date),month(fin_date),1))+months(1)

    DB_PRODUITS_JOURS_FULL_PRE %>%
      filter(CATEGORY %in% input$category_list) %>%
      filter(PRODUCT %in% input$produits_list) %>%
      filter(DATE >= debut_date, DATE <= fin_date) %>%
      group_by(JOUR_SEMAINE,CD_HEURE,PRODUCT) %>%
      summarise(QUANTITE=round(mean(QUANTITE)),.groups = "drop")
  })

  output$graph_produits_items <- renderPlot({

    db_produits_items() %>%
      complete(JOUR_SEMAINE, PRODUCT, CD_HEURE,fill = list(QUANTITE = 0.0001)) %>%
      mutate(QUANTITE = pmax(QUANTITE,0.0001)) %>%
      ggplot() +
      aes(fill=PRODUCT,values = QUANTITE) +
      geom_waffle(color = "white", size = 0.5, n_rows = 6) +
      facet_grid(rows = vars(JOUR_SEMAINE),
                 cols = vars(CD_HEURE),
                 switch="both") +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14)
      )

  })

  #### Historique ####

  # Optimisation Bolt : Mettre en cache l'historique long terme qui utilise un lissage Loess coûteux
  output$vente_LT <- renderPlotly({
    ggplotly(graph_evo_ventes_LT(UPD_JOURS(), input$LT_indic), tooltip = "text")
  }) %>% bindCache(input$check_tva, input$LT_indic, date_jour)

  output$stats_LT <- renderDataTable({
    datatable_simple(table_stats_ventes_LT(UPD_JOURS(),input$LT_indic,input$LT_flag)[[1]])})
  output$meilleurs_LT <- renderDataTable({
    datatable_simple(table_stats_ventes_LT(UPD_JOURS(),input$LT_indic,input$LT_flag)[[2]])})
  output$pires_LT <- renderDataTable({
    datatable_simple(table_stats_ventes_LT(UPD_JOURS(),input$LT_indic,input$LT_flag)[[3]])})


  #### Comparaison ####

  observe({
    updateDateInput(session,"compa_date",value = today()-2)
  })

  observeEvent(input$compa_date,{
    local$compa_jour <- input$compa_date
    local$compa_semaine <- input$compa_date - lubridate::wday(input$compa_date,week_start = 1)+1
    local$compa_mois <- input$compa_date - mday(input$compa_date)+1
  })

  output$table_compa_mois <- renderDataTable({
    mois_actual <- table_resume_mois(UPD_JOURS(),input$compa_date)
    mois_ym1 <- table_resume_mois(UPD_JOURS(),input$compa_date-years(1))
    mois_ym2 <- table_resume_mois(UPD_JOURS(),input$compa_date-years(2))
    datatable_simple(mois_ym2 %>% left_join(mois_ym1) %>% left_join(mois_actual))
  })

  output$table_compa_mois_category <- renderDataTable({
    mois_actual <- table_resume_mois_category(UPD_PRODUITS(),input$compa_date)
    mois_ym1 <- table_resume_mois_category(UPD_PRODUITS(),input$compa_date-months(1))
    mois_ym2 <- table_resume_mois_category(UPD_PRODUITS(),input$compa_date-months(2))
    datatable_simple(mois_ym2 %>% full_join(mois_ym1) %>% full_join(mois_actual))
  })




  #### Heures de travail ####

  observe({
    updateDateInput(session,"heures_date",
                    value = floor_date(today(), "month") %m-% months(1))
  })

  debut_mois_heures <- reactive({
    floor_date(input$heures_date, unit = "month")
  })

  output$heures_detail <- renderTable({
    DB <- DB_HEURES %>%
      left_join(DB_DATE) %>%
      filter(PREMIER_JOUR_MOIS == debut_mois_heures()) %>%
      mutate(PERSONNE = ifelse(PERSONNE == "Pointage",
                               ".Pointage",PERSONNE)) %>%
      group_by(PERSONNE,SECTEUR) %>%
      summarise(`Nombre d'heures` = sum(NB_HEURES_JOUR),
                `Coût du travail` = sum(COUT_JOUR),.groups = "drop") %>%
      filter(`Nombre d'heures` > 0 )

    DB %>%
      add_row(PERSONNE = "Total",SECTEUR = "",
              `Nombre d'heures` = sum(DB$`Nombre d'heures`),
              `Coût du travail` = sum(DB$`Coût du travail`)) %>%
      mutate(`Nombre d'heures` = paste0(round(`Nombre d'heures`)),
             `Coût du travail` = format_CA(`Coût du travail`))
  })

  output$heures_service <- renderTable({
    DB <- DB_HEURES %>%
      left_join(DB_DATE) %>%
      filter(PREMIER_JOUR_MOIS == debut_mois_heures()) %>%
      mutate(Type = ifelse(PERSONNE == "Pointage","Pointée","Listée")) %>%
      group_by(Type,SECTEUR) %>%
      summarise(`Nombre d'heures` = sum(NB_HEURES_JOUR),
                `Coût du travail` = sum(COUT_JOUR),.groups = "drop") %>%
      filter(`Nombre d'heures` > 0 )

    DB %>%
      add_row(Type = "Total",SECTEUR = "",
              `Nombre d'heures` = sum(DB$`Nombre d'heures`),
              `Coût du travail` = sum(DB$`Coût du travail`)) %>%
      mutate(`Nombre d'heures` = paste0(round(`Nombre d'heures`)),
             `Coût du travail` = format_CA(`Coût du travail`))
  })

  output$heures_personne <- renderTable({
    DB <- DB_HEURES %>%
      left_join(DB_DATE) %>%
      filter(PREMIER_JOUR_MOIS == debut_mois_heures()) %>%
      mutate(PERSONNE = ifelse(PERSONNE == "Pointage",
                               ".Pointage",PERSONNE)) %>%
      group_by(PERSONNE) %>%
      summarise(`Nombre d'heures` = sum(NB_HEURES_JOUR),
                `Coût du travail` = sum(COUT_JOUR),.groups = "drop") %>%
      filter(`Nombre d'heures` > 0 )

    DB %>%
      add_row(PERSONNE = "Total",
              `Nombre d'heures` = sum(DB$`Nombre d'heures`),
              `Coût du travail` = sum(DB$`Coût du travail`)) %>%
      mutate(`Nombre d'heures` = paste0(round(`Nombre d'heures`)),
             `Coût du travail` = format_CA(`Coût du travail`))
  })

  output$heures_compta <- renderTable({
    COUT_TRAVAIL_COMPTA <- DB_COMPTES_DETAIL %>%
      mutate(PREMIER_JOUR_MOIS=INDEX_PERIODE) %>%
      filter(PREMIER_JOUR_MOIS == debut_mois_heures()) %>%
      filter(LABEL_COMPTE_SUM == "Ressources humaines") %>%
      mutate(Montant = -Realise) %>%
      select(Compte = CODE_COMPTE,Label = LABEL_COMPTE,Montant)

    COUT_TRAVAIL_COMPTA %>%
      mutate(Compte = paste0(Compte)) %>%
      add_row(Compte = "",Label = "Total",
              Montant = sum(COUT_TRAVAIL_COMPTA$Montant)) %>%
      mutate(
        Label = ifelse(nchar(Label) > 35,
                       paste0(substr(Label,1,35),"..."),Label),
        Montant = format_CA(Montant,2)
      )

  })

  output$evo_heures <- renderPlotly({

    COUT_TRAVAIL_HOREKO <- DB_HEURES %>%
      left_join(DB_DATE) %>%
      filter(PREMIER_JOUR_MOIS >= debut_mois_heures()-years(1),
             PREMIER_JOUR_MOIS <= debut_mois_heures()) %>%
      group_by(PREMIER_JOUR_MOIS) %>%
      summarise(Montant = sum(COUT_JOUR),.groups = "drop") %>%
      mutate(TYPE = "Horeko")


    COUT_TRAVAIL_COMPTA <- DB_COMPTES_DETAIL %>%
      mutate(PREMIER_JOUR_MOIS=INDEX_PERIODE) %>%
      filter(PREMIER_JOUR_MOIS >= debut_mois_heures()-years(1),
             PREMIER_JOUR_MOIS <= debut_mois_heures()) %>%
      filter(LABEL_COMPTE_SUM == "Ressources humaines") %>%
      group_by(PREMIER_JOUR_MOIS) %>%
      summarise(Montant = -sum(Realise,na.rm=TRUE),.groups = "drop") %>%
      mutate(TYPE = "Compta")

    COUT_TRAVAIL <- rbind(COUT_TRAVAIL_HOREKO,COUT_TRAVAIL_COMPTA) %>%
      pivot_wider(names_from = TYPE,values_from = Montant)

    graph_evo_heures2(COUT_TRAVAIL)

    # ggplotly(graph_evo_heures(UPD_HOREKO()))

    # p <- COUT_TRAVAIL %>%
    #   ggplot()+
    #   aes(x=PREMIER_JOUR_MOIS,y=Montant,col = TYPE)+
    #   geom_line()+
    #   scale_x_date(breaks = "1 month",date_labels ="%m-%Y")+
    #   scale_y_continuous(labels = dollar_format(suffix = "€", prefix = "",
    #                                             big.mark = ".",decimal.mark = ","))+
    #   theme_minimal()+
    #   theme_mazette()+
    #   xlab("Date") +
    #   ylab("Coût du travail")
    #
    # ggplotly(p)

  })

  #### Brassin ####

  observe({

    # Ne prendre que les brassins unique
    db_brassins <- DB_BRASSINS %>%
      filter(!is.na(BOISSON)) %>%
      group_by(BOISSON) %>%
      filter(row_number() == 1) %>%
      distinct()

    vec_id_brassins <- db_brassins %>% pull(ID_BRASSIN)
    vec_name_brassins <- db_brassins %>% pull(BOISSON)

    names(vec_id_brassins) <- vec_name_brassins

    updateSelectInput(session,"report_choice",choices = vec_id_brassins)
  })

  output$report <- renderPlot({
    report_brassin(DB_BRASSINS,DB_BIERES,DB_PRODUITS,input$report_choice)
  })

  # Optimisation Bolt : Mettre en cache les prédictions de brassins (Holt-Winters) qui sont gourmandes en CPU
  DB_PREDICT <- reactive({
    table_evo_brassins(max_date = input$predict_date)
  }) %>% bindCache(input$predict_date, date_jour)

  output$table_brassins_fini <- renderDataTable({
    datatable(
      DB_BIERES %>%
        filter(FL_FINI & VOLUME_BRASSIN > 0) %>% arrange(desc(DATE)) %>%
        group_by(ID_BRASSIN) %>% filter(row_number() == 1) %>% ungroup()%>%
        mutate(across(where(is.numeric), ~round(., 2))) %>%
        select(BOISSON,ID_BRASSIN,VOLUME_BRASSIN,VOLUME_BRASSIN_AJUST,
               VOLUME_TOT,VOLUME_PAR_JOUR,DIFF,PCT)

      , options = list(pageLength = 20))
  })

  output$table_brassins_cours <- renderDataTable({
    datatable(

      TEST <- DB_BIERES %>%
        filter(!FL_FINI & VOLUME_BRASSIN > 0) %>% arrange(desc(DATE)) %>%
        group_by(ID_BRASSIN) %>% filter(row_number() == 1) %>% ungroup() %>%
        mutate(across(where(is.numeric), ~round(., 2))) %>%
        select(BOISSON,ID_BRASSIN,VOLUME_BRASSIN,VOLUME_BRASSIN_AJUST,
               VOLUME_TOT,VOLUME_PAR_JOUR,DIFF,PCT)

      , options = list(pageLength = 20))
  })

  data <- reactive({

    # TABLE <- DB_BIERES %>%
    #   filter(DATE <= input$predict_date) %>%
    #   filter(!FL_FINI & VOLUME_BRASSIN > 0) %>% arrange(desc(DATE)) %>%
    #   group_by(ID_BRASSIN) %>% filter(row_number() == 1) %>% ungroup() %>%
    #   mutate(across(where(is.numeric), ~round(., 2)))

    data.frame(DB_BIERES %>%
                 # filter(DATE <= input$predict_date) %>%
                 filter(DATE <= today()) %>%
                 filter(!FL_FINI & VOLUME_BRASSIN > 0) %>%
                 arrange(desc(DATE)) %>%
                 group_by(ID_BRASSIN) %>%
                 filter(row_number() == 1) %>%
                 ungroup() %>%
                 mutate(RESTE_L = VOLUME_BRASSIN_AJUST-VOLUME_TOT,
                        RESTE_PC = 100-round(100*VOLUME_TOT/VOLUME_BRASSIN_AJUST),
                        color = case_when(
                          RESTE_PC > 50 ~ "#00FF00",
                          RESTE_PC > 25 ~ "#FFFF00",
                          TRUE ~ "#FF0000"
                        )) %>%
                 arrange(RESTE_PC))
  })

  observe({

    updateSelectInput(session,"choix_biere",
                      choices = data()$BOISSON)

    updateCheckboxGroupButtons(session,"bieres_predict",status = "info",
                               choices = sort(unique(DB_PREDICT()$BOISSON)),
                               selected = sort(unique(DB_PREDICT()$BOISSON)))

    lapply(1:nrow(data()), function(i) {
      output[[paste0("gauge_", i)]] <- renderGauge({
        gauge(data()[i, "RESTE_PC"],
              label = paste(round(data()[i, "RESTE_L"],1),' L'),
              min = 0,
              max = 100,
              symbol = "%",
              sectors = gaugeSectors(success = c(50, 100),
                                     warning = c(30, 50),
                                     danger = c(0, 30)))
      })

      output[[paste0("label_", i)]] <- renderText({
        data()[i, "BOISSON"]
      })

      prediction <- predict_fin_brassin(DB_PREDICT(),data()[i, "ID_BRASSIN"])
      if (is.na(prediction[1]))
        prediction <- "(Inconnu)"
      else if (is.na(prediction[3]))
        prediction <- paste("Vers le",prediction[2])
      else
        prediction <- paste0("(entre ",prediction[1]," et ",prediction[3],")")

      output[[paste0("predict_", i)]] <- renderText({prediction})
    })

    output$graph_biere_TOT <- renderPlotly({
      ggplotly(graph_evo_brassin(DB_PREDICT() %>%
                                   filter(BOISSON %in% input$bieres_predict,
                                          DATE <= input$max_predict)))
    })

    output$graph_biere <- renderPlotly({
      id_brassin <- unique(data()[data()$BOISSON == input$choix_biere,"ID_BRASSIN"])
      ggplotly(graph_predict_brassin(DB_PREDICT() %>% filter(ID_BRASSIN == id_brassin)))
    })

    observeEvent(input$check_quali,{
      output$graph_quali_predict <- renderPlotly({
        # ggplotly(graph_quali_predict(input$predict_date))
        ggplotly(graph_quali_predict(today()))
      })
    })


    output$table_biere_cours <- renderDataTable({

      TABLE <- DB_BIERES %>%
        # filter(DATE <= input$predict_date) %>%
        filter(DATE <= today()) %>%
        filter(!FL_FINI & VOLUME_BRASSIN > 0) %>% arrange(desc(DATE)) %>%
        group_by(ID_BRASSIN) %>% filter(row_number() == 1) %>% ungroup() %>%
        mutate(across(where(is.numeric), ~round(., 2)))

      TABLE$DATE_FIN_MIN <- NA
      TABLE$DATE_FIN <- NA
      TABLE$DATE_FIN_MAX <- NA

      for (id_brassin in TABLE$ID_BRASSIN){
        predict <- predict_fin_brassin(DB_PREDICT(),id_brassin)
        TABLE[TABLE$ID_BRASSIN == id_brassin,"DATE_FIN_MIN"] <- predict[1]
        TABLE[TABLE$ID_BRASSIN == id_brassin,"DATE_FIN"] <- predict[2]
        TABLE[TABLE$ID_BRASSIN == id_brassin,"DATE_FIN_MAX"] <- predict[3]
      }

      TABLE <- TABLE %>%
        arrange(DATE_FIN) %>%
        select(BOISSON,ID_BRASSIN,VOLUME_BRASSIN,VOLUME_BRASSIN_AJUST,
               VOLUME_TOT,NB_JOURS_VENTES,PCT,VOLUME_PAR_JOUR,
               DATE_FIN_MIN,DATE_FIN,DATE_FIN_MAX) %>%
        mutate(VOLUME_BRASSIN = round(VOLUME_BRASSIN,0),
               VOLUME_BRASSIN_AJUST = round(VOLUME_BRASSIN_AJUST,0),
               VOLUME_TOT = round(VOLUME_TOT,0),
               PCT = paste0(round(PCT*100,0),"%")
        )

      datatable_simple(TABLE)
    })
  })

  output$graph_cluster_bieres <- renderPlotly({
    ggplotly(graph_cluster_bieres())
  })

  #### Budget ####

  output$gauge_month <- renderGauge({
    ventes_month <- gauge_calculs(var_tva(),month)
    output$gauge_month_details <- renderUI({
      gauge_details(ventes_month)})
    gauge_ventes(ventes_month)
  })

  output$gauge_quarter <- renderGauge({
    ventes_quarter <- gauge_calculs(var_tva(),quarter)
    ventes_quarter <<- ventes_quarter
    output$gauge_quarter_details <- renderUI({
      gauge_details(ventes_quarter)})
    gauge_ventes(ventes_quarter)
  })

  output$gauge_year <- renderGauge({
    ventes_year <- gauge_calculs(var_tva(),year)
    ventes_year <<- ventes_year
    output$gauge_year_details <- renderUI({
      gauge_details(ventes_year)})
    gauge_ventes(ventes_year)
  })


  output$graph_evo_ecart_budget <- renderPlotly({
    graph_evo_ecart_budget(UPD_OBJECTIFS(),UPD_JOURS())
  })

  output$graph_evo_ecart_ym1 <- renderPlotly({
    graph_evo_ecart_ym1(UPD_JOURS())
  })

  output$graph_evo_ecart_ym1 <- renderPlotly({
    graph_evo_ecart_ym1(UPD_JOURS())
  })

  observe({
    vec_year <- sort(unique(year(DB_JOURS$DATE)))
    updateRadioButtons(session,"year_panorama",inline = TRUE,
                       choices = vec_year,selected = vec_year[length(vec_year)])
  })

  output$graph_evo_annee_complete <- renderPlot({
    graph_evo_annee_complete(UPD_JOURS(),input$year_panorama)
  })


  #### Comptabilité ####

  observe({
    # temp <- DB_COMPTA %>% filter(TYPE == "Budget",CA_HTVA > 0) %>%
    #   select(CODE,LABEL) %>% distinct() %>%
    #   mutate(CODE_LABEL = paste0(CODE," : ", LABEL))
    temp <- DB_COMPTA_FULL %>%
      select(CODE_COMPTE,LABEL_COMPTE) %>% distinct() %>%
      mutate(CODE_LABEL = ifelse(is.na(CODE_COMPTE),LABEL_COMPTE,
                                 paste0(LABEL_COMPTE, " (",CODE_COMPTE,")")))

    updatePrettyRadioButtons(session,"compta_code",
                             inline = TRUE,
                             choiceNames = temp$CODE_LABEL,
                             choiceValues = temp$LABEL_COMPTE)
  })

  output$graph_evo_comptes <- renderPlotly({
    graph_evo_comptes2(input$compta_code)
  })

  #### Simulation ####

  observe({
    updateDateRangeInput(session,"date_simulation",
                         start = floor_date(today()-2, unit = "week")-weeks(3),
                         end = today()-1)
  })

  prepa_table_simulation <- reactive({
    input$reset_simulation
    # Optimisation Bolt : Utilisation de DB_PRODUITS_PRE déjà joint et remplacement de mutate_if par across
    DB_PRODUITS_PRE %>%
      filter(PRICE > 0) %>%
      filter(DATE >= input$date_simulation[1],
             DATE <= input$date_simulation[2]) %>%
      group_by(CATEGORY,PRODUCT_FULL,PRICE,PRICE_SIMU=PRICE) %>%
      summarise(QUANTITE = sum(QUANTITE,na.rm = T),
                VENTES = round(sum(CA_TVAC,na.rm = TRUE)),
                .groups = "drop")
  })

  observe({
    updateSelectInput(session,"select_category_simulation",
                      choices = unique(prepa_table_simulation()$CATEGORY))
  })

  prix_modifies <- reactiveVal(NULL)

  observeEvent(prepa_table_simulation(), {
    prix_modifies(prepa_table_simulation()$PRICE)
  }, ignoreNULL = TRUE)

  # Quand l'utilisateur édite le tableau
  observeEvent(input$table_simulation, {
    req(input$table_simulation)
    df_input <- hot_to_r(input$table_simulation)
    prix_modifies(df_input$PRICE_SIMU)  # on récupère uniquement la colonne éditable
  })

  observeEvent(input$apply_category_simulation, {
    new_prix <- prepa_table_simulation() %>%
      mutate(PRICE = ifelse(CATEGORY == input$select_category_simulation,
                            PRICE * (1+input$pc_category_simulation/100),PRICE))
    prix_modifies(round(new_prix$PRICE,1))
  })

  # Reset
  observeEvent(input$reset, {
    prix_modifies(prepa_table_simulation()$PRICE)
  })

  # Table affichée : on reconstruit à chaque fois avec les colonnes calculées
  df_affiche <- reactive({
    prepa_table_simulation() %>%
      mutate(
        PRICE_SIMU  = prix_modifies(),
        VENTES_ACTU = round(QUANTITE * PRICE,1),
        VENTES_SIMU = round(QUANTITE * PRICE_SIMU,1),
        DELTA       = round(VENTES_SIMU - VENTES_ACTU,1)
      ) %>%
      select(CATEGORY,PRODUCT_FULL,PRICE,PRICE_SIMU,QUANTITE,VENTES_ACTU,VENTES_SIMU,DELTA)
  })

  output$vb_actuel <- renderUI({
    valueBox_perso(
      format_CA(sum(df_affiche()$VENTES_ACTU,na.rm = T),-1),
      subtitle = "CA TVAC actuel",
      icon = icon("euro"),
      color = "#320032"
    )
  })

  output$vb_simule <- renderUI({
    valueBox_perso(
      format_CA(sum(df_affiche()$VENTES_SIMU,na.rm = T),-1),
      subtitle = "CA TVAC simulé",
      icon = icon("compass"),
      color = "#FFA500"
    )
  })

  output$vb_delta <- renderUI({
    diff <- sum(df_affiche()$DELTA,na.rm = T)
    nb_days <- as.numeric(input$date_simulation[2] - input$date_simulation[1])
    diff_mois <- diff /nb_days*30
    valueBox_perso(
      ifelse(format_CA(diff,-1)=="","0€",format_CA(diff,-1)),
      subtitle = paste(format_CA(diff_mois,-1),"par mois"),
      icon = icon("not-equal"),
      color = ifelse(diff>=0,"#32CD32","#FF0000")
    )
  })

  output$table_simulation <- renderDT({
    # req(prix_modifies())
    # df_affiche() %>%
    prepa_table_simulation() %>%
      datatable(
        filter = "top",
        editable = list(
          target = "cell",
          disable = list(columns = c(0, 1, 2, 3, 5, 6, 7))
        ),
        options = list(pageLength = 25)
      ) %>%
      formatStyle('PRICE_SIMU', backgroundColor = 'yellow')
  })

  # Capture des éditions cellule par cellule
  observeEvent(input$table_simulation_cell_edit, {
    info <- input$table_simulation_cell_edit
    prix <- prix_modifies()
    prix[info$row] <- as.numeric(info$value)
    prix_modifies(prix)
  })

  output$table_simulation_diff <- renderDT({
    req(prix_modifies())
    df <- df_affiche() %>% filter(PRICE != PRICE_SIMU)

    if (nrow(df) > 0){
      df %>% datatable(
        options = list(
          paging = T, # Désactive la pagination
          pageLength = 10,
          ordering = FALSE, # Désactive le tri
          searching = FALSE # Désactive la recherche
        ),
        rownames= FALSE
      )
    }
  })

  #### Vérification ####

  output$table_verif_lignes <- renderUI({

    db_brassins <- DB_BRASSINS %>%
      slice_tail(n = 5) %>%
      select(ID_BRASSIN,NOM_BRASSIN,DT_BRASSIN,DT_CONDI,VOLUME_BRASSIN)

    db_caisse <- DB_CAISSE %>%
      slice_tail(n = 5) %>%
      select(DATE,CATEGORY,DETAIL,MONTANT,METHOD)

    db_compta <- DB_COMPTA %>%
      slice_tail(n = 5) %>%
      select(INDEX_PERIODE,CODE_COMPTE,LABEL_COMPTE,ANNEE,Realise)

    db_horeko <- DB_HOREKO %>%
      filter(!is.na(DATE)) %>%
      slice_tail(n = 5) %>%
      select(DATE,HEURES,HEURES_LISTEES,SALAIRES)

    db_jours <- DB_JOURS %>%
      slice_tail(n = 5) %>%
      select(DATE,CA_HTVA,CA_TVAC,VIREMENT)

    db_objectifs <- DB_OBJECTIFS %>%
      slice_tail(n = 5) %>%
      select(DATE,CA_HTVA)

    db_produits <- DB_PRODUITS %>%
      slice_tail(n = 5) %>%
      select(DATE,CATEGORY,PRODUCT,CA_HTVA)

    tagList(

      # Pour DB_BRASSINS
      h3("Dernières lignes de DB_BRASSINS"),
      datatable_simple(db_brassins),
      h3("Dernières lignes de DB_CAISSE"),
      datatable_simple(db_caisse),
      h3("Dernières lignes de DB_COMPTA"),
      datatable_simple(db_compta),
      h3("Dernières lignes de HOREKO"),
      datatable_simple(db_horeko),
      h3("Dernières lignes de DB_JOURS"),
      datatable_simple(db_jours),
      h3("Dernières lignes de DB_OBJECTIFS"),
      datatable_simple(db_objectifs),
      h3("Dernières lignes de DB_PRODUITS"),
      datatable_simple(db_produits)

     )

  })

}

