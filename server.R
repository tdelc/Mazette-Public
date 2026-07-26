library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(janitor)
library(googledrive)
library(googlesheets4)
library(readxl)
library(scales)
library(plotly)
library(forecast)
library(DT)
library(zoo)
library(patchwork)

link_json <- Sys.getenv("LINK_JSON")
path_drive <- Sys.getenv("PATH_DRIVE")
id_sheet_mazette <- Sys.getenv("ID_DRIVE_MAZETTE")
ID_MAZETTE_2023 <- Sys.getenv("ID_MAZETTE_2023")
ID_MAZETTE_2025 <- Sys.getenv("ID_MAZETTE_2025")
path_logos <- Sys.getenv("PATH_LOGOS")

download.file(link_json, destfile = "connect.json")
drive_auth(path = "connect.json")
gs4_auth(path = "connect.json")

df_logos <- googledrive::drive_ls(path_logos)

options(DT.options = list(pageLength = 5, language = list(search = 'Filter:')))

# Configuration des dates
date_debut_semaine <- floor_date(today() - 2, unit = "week") + 1
date_debut_mois <- floor_date(today() - 2, unit = "month")
nb_jours_mois <- as.numeric(ceiling_date(today() - 2, unit = "month")-date_debut_mois)
date_debut_annee <- floor_date(today() - 2, unit = "year") + 1

date_debut_semaine_m1 <- date_debut_semaine - 7
date_debut_8_semaines <- floor_date(today() - 2, unit = "week") - weeks(8)
date_fin_8_semaines <- today()

#### Chargement initial des données ####

force_dl <- FALSE

prefix <- "R_env_"
date_jour <- format(now() - days(1), format = "%Y-%m-%d")
drive_env_name <- paste0(prefix, date_jour, ".RData")

if (!force_dl) {
  # Chargement local
  drive_mazette <- try({
    print("Chargement de données locales")
    env_vec <- list.files("outputs/", pattern = drive_env_name)
    env_name <- sort(env_vec)[length(env_vec)]
    load(paste0("outputs/", env_name))
    print(system.time({source("functions.R", local = TRUE)}))
  }, silent = TRUE)
} else {
  drive_mazette <- try({ERROR}, silent = TRUE)
}

# Chargement via environnement google
if (force_dl | class(drive_mazette)[1] == "try-error") {
  drive_mazette <- try({
    print("Aucune données locales, chargement de l'environnement via google")
    drive_info <- drive_get(path = drive_env_name)
    drive_download(drive_info,
                   path = file.path("outputs", drive_info$name),
                   overwrite = TRUE)
    load(paste0("outputs/", drive_env_name))
    print(system.time({source("functions.R", local = TRUE)}))
  }, silent = TRUE)
}

# Chargement via google sheets (reconstruction complète)
if (force_dl | class(drive_mazette)[1] == "try-error") {
  print("Aucune données encore, il faut les charger une par une.")
  print(system.time({source("functions.R", local = TRUE)}))
  print(system.time({source("import.R", local = TRUE)}))
  print(system.time({source("nettoyage_ajout.R", local = TRUE)}))

  # Prendre la dernière date comme date de sauvegarde
  date_jour <- max(DB_JOURS$DATE)
  drive_env_name <- paste0(prefix, date_jour, ".RData")
  
  rm(force_dl)

  save(list = ls(),
       file = file.path("outputs", drive_env_name),
       compress = "xz",
       compression_level = 9)

  googledrive::drive_upload(file.path("outputs", drive_env_name),
                            name = drive_env_name,
                            path = as_id(path_drive),
                            overwrite = TRUE)
}

source("functions.R", local = TRUE)
# Générateurs de DB fictives pour l'onglet Compta (+ tutoriel d'intégration)
source("donnees_fictives_compta.R", local = TRUE)

#### Serveur ####

server <- function(input, output, session) {

  USER <- reactiveValues(logged = FALSE)

  #### Données préparées (tout en CA HTVA) ####
  UPD_KPI_SIMPLE <- prepa_db(DB_KPI_SIMPLE, "CA_HTVA")
  UPD_OBJECTIFS  <- prepa_db(DB_OBJECTIFS, "CA_HTVA")

  # DB fictives compta (personnel + matières), calées sur le calendrier réel.
  # 4 secteurs : Service / Transformation alimentaire / Brasserie / Support.
  DATES_COMPTA     <- DB_DATE %>% filter(DATE <= today()) %>% pull(DATE)
  DB_COUTS_TRAVAIL <- generer_couts_travail(DATES_COMPTA)
  DB_COUTS_MATIERE <- generer_couts_matiere(DATES_COMPTA)
  
  DB_COUTS_TRAVAIL <- DB_COUTS_TRAVAIL |> 
    left_join(DB_DATE |> select(DATE,PREMIER_JOUR_SEMAINE,PREMIER_JOUR_MOIS))
  
  DB_COUTS_MATIERE_JOUR <- DB_DATE |> 
    rename(SEMAINE = PREMIER_JOUR_SEMAINE) |> 
    left_join(DB_COUTS_MATIERE) |> 
    group_by(SEMAINE,SECTEUR) |> 
    mutate(COUT_MATIERE    = COUT_MATIERE / n(),
           ACHATS          = ACHATS / n(),
           VARIATION_STOCK = VARIATION_STOCK / n()) |> 
    ungroup() |> 
    select(DATE,SEMAINE,SECTEUR,COUT_MATIERE,ACHATS,VARIATION_STOCK) |> 
    filter(DATE < today())

  # Dernier jour d'ouverture (= "veille")
  date_veille <- DB_KPI_SIMPLE %>%
    filter(CA_HTVA > 0, DATE < today()) %>%
    summarise(d = max(DATE)) %>%
    pull(d)

  #### Login ####
  observeEvent(input$boutton_log, {
    password <- IMPORT_PASS %>%
      filter(Date_debut <= today(), Date_fin >= today()) %>%
      pull(pass)

    if (input$password %in% password) {
      USER$logged <- TRUE
      shinyjs::hide("login_screen")
      shinyjs::show("app_screen")
    } else {
      output$text_log <- renderText("Erreur dans le mot de passe")
    }
  })

  #### Volet "Maintenant" — Indicateurs clés ####
  ca_periode <- function(db, d1, d2) {
    db %>% filter(DATE >= d1, DATE <= d2) %>% summarise(s = sum(ventes, na.rm = TRUE)) %>% pull(s)
  }

  output$vb_ca_veille <- renderText({
    format_CA(ca_periode(UPD_KPI_SIMPLE, date_veille, date_veille), -1)
  })

  output$vb_ca_semaine <- renderText({
    format_CA(ca_periode(UPD_KPI_SIMPLE, date_debut_semaine, today()), -1)
  })

  output$vb_pct_semaine <- renderText({
    reel <- ca_periode(UPD_KPI_SIMPLE, date_debut_semaine, today())
    obj  <- ca_periode(UPD_OBJECTIFS, date_debut_semaine, today())
    if (is.na(obj) || obj == 0) "—" else paste0(round(100 * reel / obj), " %")
  })

  #### Volet "Maintenant" — Veille ####
  output$titre_veille <- renderText({
    paste0("Veille — ", format(date_veille, "%A %d/%m/%Y"))
  })

  output$box_veille <- renderUI({
    box_ventes_jour(UPD_KPI_SIMPLE, UPD_OBJECTIFS, date_veille, 0,
                    format_date = "%d/%m")
  })

  output$top_veille <- renderDT({
    datatable_simple(
      top_produits_periode(DB_PRODUITS_JOURS_FULL, date_veille, date_veille, n = 10)
    )
  })

  #### Volet "Maintenant" — Semaine en cours ####
  output$box_semaine <- renderUI({
    box_ventes_jour(UPD_KPI_SIMPLE, UPD_OBJECTIFS, date_debut_semaine, 6)
  })

  output$box_semaine_total <- renderUI({
    box_ventes_total(UPD_KPI_SIMPLE, UPD_OBJECTIFS, date_debut_semaine, 6,
                     titre = "Total semaine", is_semaine = TRUE)
  })
  
  output$box_semaine_avant <- renderUI({
    1:5 %>% map(~{
      box_ventes_jour(UPD_KPI_SIMPLE,UPD_OBJECTIFS,
                      date_debut_semaine - 7*.x,6,
                      is_midi=FALSE,is_boisson=FALSE,is_objectif=FALSE)
    })
  })
  
  output$box_semaine_total_avant <- renderUI({
    1:5 %>% map(~{
      box_ventes_total(UPD_KPI_SIMPLE,UPD_OBJECTIFS,
                       date_debut_semaine - 7*.x,6,titre="Total",
                       is_semaine=FALSE,is_midi=FALSE,is_boisson=FALSE,
                       is_objectif=FALSE)
    })
  })
 
  
  #### Volet "Maintenant" — Progression du mois ####
  
  # Liste des mois disponibles (du plus récent au plus ancien)
  observe({
    mois_dispo <- UPD_KPI_SIMPLE %>%
      filter(ventes > 0) %>%
      distinct(PREMIER_JOUR_MOIS) %>%
      arrange(desc(PREMIER_JOUR_MOIS)) %>%
      pull(PREMIER_JOUR_MOIS)
    
    choix <- setNames(as.character(mois_dispo), format(mois_dispo, "%B %Y"))
    updateSelectInput(session, "prog_mois", choices = choix,
                      selected = as.character(floor_date(date_veille, "month")))
  })
  
  mois_choisi <- reactive({
    req(input$prog_mois)
    as.Date(input$prog_mois)
  })
  
  output$box_mois_total <- renderUI({
    box_ventes_total(UPD_KPI_SIMPLE, UPD_OBJECTIFS, mois_choisi(), 
                     days_in_month(mois_choisi())-1, 
                     titre = "Total mois", is_semaine = TRUE)
  })
  
  prog_data <- reactive({
    progression_mois(UPD_KPI_SIMPLE, UPD_OBJECTIFS, mois_choisi())
  })
  
  output$prog_graph <- renderPlotly({
    graph_progression_mois(prog_data(), mois_choisi())
  })
  
  output$prog_resume <- renderUI({
    d <- prog_data()
    reel <- suppressWarnings(max(d$cum_reel, na.rm = TRUE))
    if (!is.finite(reel)) reel <- 0
    obj  <- max(d$cum_obj, na.rm = TRUE)
    pct  <- if (obj > 0) round(100 * reel / obj) else NA
    
    badge <- function(label, valeur, bg, fg = "#ffffff") {
      div(style = paste0("background:", bg, ";color:", fg,
                         ";border-radius:0.5rem;padding:0.4rem 0.7rem;min-width:110px;"),
          div(class = "small", label),
          div(style = "font-weight:700;font-size:1.05rem;", valeur))
    }
    
    div(class = "d-flex gap-2 flex-wrap align-items-center",
        badge("Réalisé", format_CA(reel, -1), "#732c02"),
        badge("Objectif", format_CA(obj, -1), "#d98236"),
        badge("Atteint", if (is.na(pct)) "—" else paste0(pct, " %"), "#efe7d8", "#732c02"))
  })

  #### Volet "Maintenant" — Produits de la semaine ####
  output$top_semaine <- renderDT({
    datatable_simple(
      top_produits_periode(DB_PRODUITS_JOURS_FULL,
                           date_debut_semaine, date_debut_semaine + 6, n = 10)
    )
  })

  output$hausse_semaine <- renderDT({
    datatable_simple(
      evolution_produits_semaine(DB_PRODUITS_JOURS_FULL, date_debut_semaine,
                                 sens = "hausse")
    )
  })

  output$baisse_semaine <- renderDT({
    datatable_simple(
      evolution_produits_semaine(DB_PRODUITS_JOURS_FULL, date_debut_semaine,
                                 sens = "baisse")
    )
  })

  #### Volet "Détail" — Par jour ####

  # Période par défaut : 8 dernières semaines jusqu'à la veille
  observe({
    updateDateRangeInput(session, "detail_periode",
                         start = date_veille - weeks(8),
                         end   = date_veille)
  })

  periode_detail <- reactive({
    rng <- input$detail_periode
    if (is.null(rng) || any(is.na(rng))) c(date_veille - weeks(8), date_veille) else rng
  })

  output$detail_jour_graph <- renderPlotly({
    p <- periode_detail()
    graph_ca_jour(UPD_KPI_SIMPLE, UPD_OBJECTIFS, p[1], p[2], source = "detail_jour")
  })

  # Jour sélectionné (clic sur une barre, défaut = veille)
  selected_jour <- reactiveVal(NULL)

  observeEvent(event_data("plotly_click", source = "detail_jour"), {
    ev <- event_data("plotly_click", source = "detail_jour")
    if (!is.null(ev$x)) selected_jour(as.Date(ev$x))
  })

  jour_detail <- reactive({
    j <- selected_jour()
    if (is.null(j)) date_veille else j
  })
  
  semaine_detail <- reactive({
    req(jour_detail())
    jour_detail()-lubridate::wday(jour_detail(),week_start = 1)+1
  })

  output$detail_jour_titre <- renderText({
    paste0("Journée du ", format(jour_detail(), "%A %d/%m/%Y"))
  })

  output$detail_jour_box <- renderUI({
    box_ventes_jour(UPD_KPI_SIMPLE, UPD_OBJECTIFS, jour_detail(), 0,
                    format_date = "%d/%m", width = "100%")
  })

  output$detail_jour_produits <- renderDT({
    datatable_simple(
      top_produits_periode(DB_PRODUITS_JOURS_FULL, jour_detail(), jour_detail(), n = 15)
    )
  })
  
  # Personnel du jour, par secteur
  output$detail_jour_travail <- renderDT({
    datatable_simple(
      DB_COUTS_TRAVAIL %>%
        filter(DATE == jour_detail()) %>%
        arrange(SECTEUR) |> 
        transmute(Secteur = SECTEUR, Heures = round(HEURES),
                  `Taux/h` = format_CA(TAUX_HORAIRE, 2),
                  Personnel = format_CA(COUT_TRAVAIL, -1))
    )
  })
  
  output$detail_jour_travail_semaine <- renderDT({
    print(semaine_detail())
    datatable_simple(
      DB_COUTS_TRAVAIL %>%
        filter(PREMIER_JOUR_SEMAINE == semaine_detail()) %>%
        group_by(SECTEUR) |> 
        summarise(HEURES = sum(HEURES),
                  COUT_TRAVAIL = sum(COUT_TRAVAIL),
                  TAUX_HORAIRE = COUT_TRAVAIL / HEURES) |> 
        transmute(Secteur = SECTEUR, Heures = round(HEURES),
                  `Taux/h` = format_CA(TAUX_HORAIRE, 2),
                  Personnel = format_CA(COUT_TRAVAIL, -1))
    )
  })

  # Matières de la semaine du jour sélectionné, par secteur
  output$detail_jour_cout <- renderDT({
    datatable_simple(
      DB_COUTS_MATIERE %>%
        filter(SEMAINE == floor_date(jour_detail(), unit = "week", week_start = 1)) %>%
        transmute(Secteur = SECTEUR, Achats = format_CA(ACHATS, -1),
                  Stock = format_CA(VARIATION_STOCK, -1),
                  Matières = format_CA(COUT_MATIERE, -1))
    )
  })

  #### Volet "Détail" — Par semaine / Par mois ####
  # Un même bloc sert les deux sous-onglets (suffixes "sem" et "mois").
  registre_detail_periode <- function(sfx, unite, defaut_debut) {
    id <- function(x) paste0("detail_", sfx, "_", x)
    src <- paste0("detail_", sfx)

    observe({
      updateDateRangeInput(session, id("periode"),
                           start = defaut_debut, end = date_veille)
    })

    periode <- reactive({
      rng <- input[[id("periode")]]
      if (is.null(rng) || any(is.na(rng))) c(defaut_debut, date_veille) else rng
    })

    output[[id("graph")]] <- renderPlotly({
      p <- periode()
      graph_ca_periode(UPD_KPI_SIMPLE, UPD_OBJECTIFS, p[1], p[2],
                       unite = unite, source = src)
    })

    # Période sélectionnée au clic (défaut : la dernière période connue)
    choisie <- reactiveVal(NULL)
    observeEvent(event_data("plotly_click", source = src), {
      ev <- event_data("plotly_click", source = src)
      if (!is.null(ev$x)) choisie(debut_periode(as.Date(ev$x), unite))
    })

    periode_sel <- reactive({
      p <- choisie()
      if (is.null(p)) debut_periode(date_veille, unite) else p
    })
    
    cout_travail <- reactive({
      d1 <- periode_sel()
      d2 <- fin_periode(d1, unite)
      i <- interval(d1, d2)
      
      DB_COUTS_TRAVAIL %>%
        filter(DATE %within% i) %>%
        group_by(SECTEUR) |> 
        summarise(HEURES = sum(HEURES),
                  COUT_TRAVAIL = sum(COUT_TRAVAIL),
                  TAUX_HORAIRE = COUT_TRAVAIL / HEURES)
    })
    
    cout_matiere <- reactive({
      d1 <- periode_sel()
      d2 <- fin_periode(d1, unite)
      i <- interval(d1, d2)
      
      DB_COUTS_MATIERE_JOUR %>%
        filter(DATE %within% i) %>%
        group_by(SECTEUR) |> 
        summarise(ACHATS = sum(ACHATS),
                  VARIATION_STOCK = sum(VARIATION_STOCK),
                  COUT_MATIERE = sum(COUT_MATIERE))
    })
    
    ca <- reactive({
      d1 <- periode_sel()
      d2 <- fin_periode(d1, unite)
      i <- interval(d1, d2)
      
      UPD_KPI_SIMPLE |>  filter(DATE %within% i) |> pull(ventes) |> sum()
    })
    
    apercu <- reactive({
      compta_apercu(UPD_KPI_SIMPLE, DB_COUTS_TRAVAIL, DB_COUTS_MATIERE,
                    periode_sel(), unite)
    })
    
    output[[id("kpi")]] <- renderUI({ kpi_compta_tiles(apercu()) })
    
    marge <- reactive({
      cout_matiere() |> 
        left_join(cout_travail()) |> 
        mutate(CA = ca()) |> 
        select(SECTEUR,CA,COUT_TRAVAIL,COUT_MATIERE)
    })

    output[[id("titre")]] <- renderText({
      d1 <- periode_sel()
      d2 <- fin_periode(d1, unite)
      paste0(label_periode(d1, unite), "  (",
             format(d1, "%d/%m"), " → ", format(d2, "%d/%m/%Y"), ")")
    })

    output[[id("repartition")]] <- renderPlotly({
      graph_repartition_periode(UPD_KPI_SIMPLE, UPD_OBJECTIFS,
                                periode_sel(), unite = unite)
    })

    output[[id("box")]] <- renderUI({
      d1 <- periode_sel()
      box_ventes_total(UPD_KPI_SIMPLE, UPD_OBJECTIFS, d1,
                       as.numeric(fin_periode(d1, unite) - d1),
                       titre = label_periode(d1, unite), is_semaine = TRUE)
    })
    
    output[[id("travail")]] <- renderDT({
      req(cout_travail())
      datatable_simple(
        cout_travail() |> 
          transmute(Secteur = SECTEUR, Heures = round(HEURES),
                    `Taux/h` = format_CA(TAUX_HORAIRE, 2),
                    Personnel = format_CA(COUT_TRAVAIL, -1))
      )
    })
    
    output[[id("cout")]] <- renderDT({
      datatable_simple(
        cout_matiere() |> 
          transmute(Secteur = SECTEUR, Achats = format_CA(ACHATS, -1),
                    Stock = format_CA(VARIATION_STOCK, -1),
                    Matières = format_CA(COUT_MATIERE, -1))
      )
    })
    
    output[[id("marge")]] <- renderDT({
      datatable_simple(
        marge() |> 
          transmute(Secteur = SECTEUR, "Chiffre d'affaire" = format_CA(CA, -1),
                    Personnel = format_CA(COUT_TRAVAIL, -1),
                    Matières = format_CA(COUT_MATIERE, -1))
      )
    })

    output[[id("produits")]] <- renderDT({
      d1 <- periode_sel()
      datatable_simple(
        top_produits_periode(DB_PRODUITS_JOURS_FULL, d1,
                             fin_periode(d1, unite), n = 20)
      )
    })
  }

  registre_detail_periode("sem",  "semaine", date_veille - weeks(26))
  registre_detail_periode("mois", "mois",    floor_date(date_veille, "month") %m-% months(12))

  #### Volet "Détail" — Par produit ####
  
  observe({
    updateDateRangeInput(session, "detail_produit_periode",
                         start = date_veille - weeks(8),
                         end   = date_veille)
  })
  
  periode_produit_detail <- reactive({
    rng <- input$detail_produit_periode
    if (is.null(rng) || any(is.na(rng))) c(date_veille - weeks(8), date_veille) else rng
  })

  produits_df <- reactive({
    p <- periode_produit_detail()
    liste_produits_periode(DB_PRODUITS_JOURS_FULL, p[1], p[2])
  })

  output$detail_produit_liste <- renderDT({
    df <- produits_df() %>%
      transmute(Produit = tronque_nom(Produit),
                Quantité = Quantite,
                `CA HTVA` = format_CA(CA, -1))
    datatable(df, selection = "single", rownames = FALSE,
              options = list(pageLength = 12, dom = 'ftp', 
                             language = list(search = "Filtrer :")))
  })

  produit_choisi <- reactive({
    df <- produits_df()
    if (nrow(df) == 0) return(NULL)
    i <- input$detail_produit_liste_rows_selected
    if (is.null(i)) df$Produit[1] else df$Produit[i]
  })

  output$detail_produit_titre <- renderText({
    pr <- produit_choisi()
    if (is.null(pr)) "Aucun produit" else paste0("Évolution — ", pr)
  })

  evo_produit <- reactive({
    pr <- produit_choisi()
    req(pr)
    evolution_un_produit(DB_PRODUITS_JOURS_FULL, pr,
                         min(DB_PRODUITS_JOURS_FULL$DATE), today())
  })
  
  evo_produit_periode <- reactive({
    pr <- produit_choisi()
    req(pr)
    evolution_un_produit(DB_PRODUITS_JOURS_FULL, pr,
                         periode_produit_detail()[1], today())
  })

  output$detail_produit_graph <- renderPlotly({
    graph_evolution_produit(evo_produit(), produit_choisi())
  })

  output$detail_produit_table <- renderDT({
    
    category <- evo_produit_periode() |> pull(CATEGORY) |> unique() |> str_to_title()
    category_column <- paste0("Part dans '",category,"'")
    
    df <- evo_produit_periode() %>%
      transmute(Semaine = format(SEMAINE, "%d/%m/%Y"),
                Quantité = Quantite,
                `CA HTVA` = format_CA(CA, -1),
                `Part dans Total` = paste0(round(PC_ALL*100,0),"%"),
                !!sym(category_column) := paste0(round(PC_CATEGORY*100,0),"%")
                ) %>%
      arrange(desc(Semaine))
    
    datatable(df, selection = "none", rownames = FALSE,
              options = list(pageLength = 12, dom = 'tp'))
  })

  #### Volet "Historique" — CA par semaine / mois ####
  output$hist_graph <- renderPlotly({
    graph_historique(UPD_KPI_SIMPLE, UPD_OBJECTIFS,
                     unite = input$hist_unite, n = input$hist_n)
  })

  output$hist_evo <- renderPlotly({
    graph_historique_tendance(UPD_KPI_SIMPLE, UPD_OBJECTIFS,
                     unite = input$hist_unite, n = input$hist_n)
  })
  

  #### Volet "Bières" ####

  # Évolution + prédictions des fûts en cours (calcul HoltWinters, une seule fois)
  db_predict_bieres <- reactive({
    table_evo_brassins(today())
  })

  output$bieres_niveaux <- renderPlotly({
    graph_niveaux_bieres(niveau_bieres_actuel())
  })

  output$bieres_evo <- renderPlotly({
    graph_evo_brassin_plotly(db_predict_bieres())
  })

  output$bieres_predict_table <- renderDT({
    datatable_simple(table_predictions_fin(db_predict_bieres()))
  })

  # Sélecteur de brassin pour le rapport
  observe({
    brassins <- DB_BRASSINS %>% arrange(desc(DT_BRASSIN))
    choix <- setNames(brassins$ID_BRASSIN, brassins$NOM_BRASSIN)
    updateSelectInput(session, "brassin_choisi", choices = choix)
  })

  output$brassin_report <- renderPlot({
    req(input$brassin_choisi)
    
    report_brassin(DB_BRASSINS, DB_BIERES, DB_PRODUITS, input$brassin_choisi)
  })

  #### Volet "Simulation" ####

  # Période par défaut : 8 dernières semaines
  observe({
    updateDateRangeInput(session, "sim_periode",
                         start = date_veille - weeks(8), end = date_veille)
  })

  sim_periode_val <- reactive({
    rng <- input$sim_periode
    if (is.null(rng) || any(is.na(rng))) c(date_veille - weeks(8), date_veille) else rng
  })

  # Base figée par période (ordre stable) + prix simulés (vecteur par n° de ligne)
  sim_base <- reactive({
    p <- sim_periode_val()
    prepa_simulation(DB_PRODUITS_JOURS_FULL, p[1], p[2])
  })

  sim_prix <- reactiveVal(NULL)

  # (Ré)initialise les prix simulés quand la base change + remplit les catégories
  observeEvent(sim_base(), {
    sim_prix(sim_base()$PRIX_MOYEN)
    updateSelectInput(session, "sim_categorie",
                      choices = sort(unique(sim_base()$CATEGORY)))
  })

  # Appliquer une variation % à toute une catégorie
  observeEvent(input$sim_apply, {
    base <- sim_base()
    cur  <- sim_prix()
    if (is.null(cur) || length(cur) != nrow(base)) cur <- base$PRIX_MOYEN
    idx <- base$CATEGORY == input$sim_categorie
    cur[idx] <- round(cur[idx] * (1 + input$sim_pct / 100), 2)
    sim_prix(cur)
  })

  # Réinitialiser tous les prix
  observeEvent(input$sim_reset, {
    sim_prix(sim_base()$PRIX_MOYEN)
  })

  # Édition directe d'un prix simulé (colonne 4, rownames = FALSE)
  observeEvent(input$sim_table_cell_edit, {
    info <- input$sim_table_cell_edit
    if (!is.null(info$col) && info$col == 4) {
      base <- sim_base()
      cur  <- sim_prix()
      if (is.null(cur) || length(cur) != nrow(base)) cur <- base$PRIX_MOYEN
      val <- suppressWarnings(as.numeric(info$value))
      if (!is.na(val) && info$row >= 1 && info$row <= length(cur)) {
        cur[info$row] <- round(val, 2)
        sim_prix(cur)
      }
    }
  })

  sim_result <- reactive({
    calc_simulation(sim_base(), sim_prix())
  })

  # Tableau éditable rendu une seule fois (par période) ; mis à jour via proxy
  output$sim_table <- renderDT({
    sim <- calc_simulation(sim_base(), isolate(sim_prix()))
    datatable(
      table_simulation_aff(sim),
      rownames = FALSE, selection = "none",
      editable = list(target = "cell",
                      disable = list(columns = c(0, 1, 2, 3, 5, 6, 7))),
      options = list(pageLength = 15, language = list(search = "Filtrer :"))
    ) %>%
      formatStyle("Prix simulé", backgroundColor = "#fff7e6")
  }, server = TRUE)

  sim_proxy <- dataTableProxy("sim_table")
  observe({
    replaceData(sim_proxy, table_simulation_aff(sim_result()),
                resetPaging = FALSE, rownames = FALSE)
  })

  output$sim_table_diff <- renderDT({
    diff <- sim_result() %>% filter(abs(PRIX_SIMU - PRIX_MOYEN) > 0.001)
    datatable_simple(table_simulation_aff(diff))
  })

  output$sim_vb_actuel <- renderText({
    format_CA(sum(sim_result()$CA, na.rm = TRUE), -1)
  })

  output$sim_vb_simule <- renderText({
    format_CA(sum(sim_result()$CA_SIMU, na.rm = TRUE), -1)
  })

  output$sim_vb_delta <- renderText({
    d <- sum(sim_result()$DELTA, na.rm = TRUE)
    a <- sum(sim_result()$CA, na.rm = TRUE)
    pct <- if (a > 0) round(100 * d / a, 1) else NA
    paste0(format_CA(d, -1),
           if (!is.na(pct)) paste0("  (", ifelse(d >= 0, "+", ""), pct, " %)") else "")
  })

  #### Volet "Compta / Gestion" ####
  # Un bloc générique sert les deux sous-onglets (semaine / mois). Chaque volet
  # a un panneau A (période analysée) et un panneau B (période comparée), ce
  # dernier étant affiché/masqué par shinyjs — l'UI reste statique.
  registre_compta_volet <- function(sfx, unite) {
    id  <- function(x) paste0("compta_", sfx, "_", x)
    src <- paste0("compta_evo_", sfx)

    # Liste des périodes proposées dans les deux sélecteurs
    observe({
      dispo <- liste_periodes_dispo(UPD_KPI_SIMPLE, unite)
      req(length(dispo) > 0)
      choix <- setNames(as.character(dispo), label_periode(dispo, unite))
      updateSelectInput(session, id("a"), choices = choix,
                        selected = as.character(dispo[1]))
      updateSelectInput(session, id("b"), choices = choix,
                        selected = as.character(dispo[min(2, length(dispo))]))
    })

    # Agrégat de toutes les périodes (pour les graphiques d'évolution)
    comptes <- reactive({
      agrege_compta(UPD_KPI_SIMPLE, DB_COUTS_TRAVAIL, DB_COUTS_MATIERE, unite)
    })

    periode_a <- reactive({ req(input[[id("a")]]); as.Date(input[[id("a")]]) })
    periode_b <- reactive({ req(input[[id("b")]]); as.Date(input[[id("b")]]) })

    # Clic sur une barre -> devient la période analysée (panneau A)
    observeEvent(event_data("plotly_click", source = src), {
      ev <- event_data("plotly_click", source = src)
      if (!is.null(ev$x))
        updateSelectInput(session, id("a"),
                          selected = as.character(debut_periode(as.Date(ev$x), unite)))
    })

    # Activation du second volet : split de l'écran en deux boxes
    observeEvent(input[[id("cmp")]], {
      cibles <- c(id("cmp_box"), id("panel_b"), id("ecarts_box"))
      if (isTRUE(input[[id("cmp")]])) {
        for (cible in cibles) shinyjs::show(cible)
      } else {
        for (cible in cibles) shinyjs::hide(cible)
      }
    })

    output[[id("evo")]] <- renderPlotly({
      graph_evo_compta(comptes(), unite = unite, source = src,
                       selection = periode_a())
    })

    output[[id("kpi_evo")]] <- renderPlotly({
      graph_evo_kpi_compta(comptes(), unite = unite)
    })

    apercu_a <- reactive({
      compta_apercu(UPD_KPI_SIMPLE, DB_COUTS_TRAVAIL, DB_COUTS_MATIERE,
                    periode_a(), unite)
    })
    apercu_b <- reactive({
      compta_apercu(UPD_KPI_SIMPLE, DB_COUTS_TRAVAIL, DB_COUTS_MATIERE,
                    periode_b(), unite)
    })

    # Contenu d'un panneau (identique pour A et B)
    registre_panneau <- function(cle, ap) {
      output[[id(paste0("titre_", cle))]] <- renderText({
        a <- ap()
        paste0(a$libelle, "  (", format(a$bornes[1], "%d/%m"), " → ",
               format(a$bornes[2], "%d/%m/%Y"), ")")
      })
      output[[id(paste0("kpi_", cle))]] <- renderUI({ kpi_compta_tiles(ap()) })
      output[[id(paste0("secteurs_", cle))]] <- renderPlotly({
        graph_secteurs_compta(ap())
      })
      output[[id(paste0("table_", cle))]] <- renderDT({
        datatable_simple(table_secteurs_compta(ap()))
      })
    }
    registre_panneau("a", apercu_a)
    registre_panneau("b", apercu_b)

    output[[id("ecarts")]] <- renderUI({ kpi_ecarts_tiles(apercu_a(), apercu_b()) })
  }

  registre_compta_volet("sem",  "semaine")
  registre_compta_volet("mois", "mois")

  #### Volet "Comparaison" ####

  # Met à jour la liste des périodes disponibles selon la granularité choisie ;
  # sélectionne par défaut les 2 plus récentes.
  observe({
    req(input$comp_unite)
    dispo <- liste_periodes_dispo(UPD_KPI_SIMPLE, input$comp_unite)
    choix <- setNames(as.character(dispo), label_periode(dispo, input$comp_unite))
    updateSelectizeInput(session, "comp_periodes", choices = choix,
                         selected = as.character(head(dispo, 2)))
  })

  comp_data <- reactive({
    req(input$comp_periodes)
    comparaison_periodes(UPD_KPI_SIMPLE, UPD_OBJECTIFS,
                         DB_COUTS_TRAVAIL, DB_COUTS_MATIERE,
                         unite = input$comp_unite, periodes = input$comp_periodes)
  })

  output$comp_graph <- renderPlotly({
    graph_comparaison(comp_data(), unite = input$comp_unite)
  })

  output$comp_table <- renderDT({
    datatable_simple(table_comparaison_aff(comp_data(), unite = input$comp_unite))
  })

  #### Volet "Année" ####

  observe({
    annees <- UPD_KPI_SIMPLE %>%
      filter(ventes > 0) %>%
      pull(DATE) %>% year() %>% unique() %>% sort(decreasing = TRUE)
    req(length(annees) > 0)
    updateSelectInput(session, "annee_choisie", choices = annees,
                      selected = annees[1])
  })

  annee_val <- reactive({
    if (is.null(input$annee_choisie)) year(today())
    else as.integer(input$annee_choisie)
  })

  serie_annee <- reactive({
    serie_annuelle(UPD_KPI_SIMPLE, UPD_OBJECTIFS,
                   DB_COUTS_TRAVAIL, DB_COUTS_MATIERE, annee_val())
  })

  serie_annee_m1 <- reactive({
    serie_annuelle(UPD_KPI_SIMPLE, UPD_OBJECTIFS,
                   DB_COUTS_TRAVAIL, DB_COUTS_MATIERE, annee_val() - 1)
  })

  output$annee_kpi <- renderUI({
    kpi_annee_tiles(serie_annee(), serie_annee_m1())
  })

  output$annee_ecart_obj <- renderPlotly({
    graph_ecart_objectif(serie_annee())
  })

  output$annee_ecart_ym1 <- renderPlotly({
    graph_ecart_ym1(UPD_KPI_SIMPLE, annee_val(), var = "ventes")
  })

  output$annee_ecart_marge <- renderPlotly({
    graph_ecart_ym1(UPD_KPI_SIMPLE, annee_val(), var = "marge",
                    serie = serie_annee(), serie_m1 = serie_annee_m1())
  })

}
