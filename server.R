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
id_sheet_heures <- Sys.getenv("ID_DRIVE_HEURES")
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

#### Temporaire : le temps du dev ####

prepa_heures <- function(id_drive){
  
  drive_mazette <- drive_download(drive_get(id =id_drive),overwrite = TRUE)
  DB_HEURES <- read_excel(drive_mazette$local_path,sheet = "Rapport",skip = 1)
  DB_HEURES <- DB_HEURES %>% 
    filter(!is.na(Jour),Jour != "Jour",Jour != "Nom") %>% 
    mutate(DATE = as.Date(as.numeric(Jour), origin = "1899-12-30")) %>% 
    mutate(HEURE_DEB = substr(Temps,1,5),
           HEURE_FIN = substr(Temps,9,13)) %>% 
    rename(CONTRAT = `Type de contrat`,
           DEPARTEMENT = `Nom de l’équipe`,
           HEURES_PAUSE = Pauses)
  
  # Nettoyage
  DB_HEURES <- DB_HEURES %>% filter(Temps != "12:08 - 12:06")
  
  DB_HEURES <- DB_HEURES %>%
    mutate(
      debut_h = hour(hm(HEURE_DEB)) + minute(hm(HEURE_DEB))/60,
      fin_h   = hour(hm(HEURE_FIN))   + minute(hm(HEURE_FIN))/60,
      HEURE_MIDI = case_when(
        debut_h == 0 & fin_h == 0 ~ 0,
        debut_h >= 17 ~ 0,
        fin_h <= 17 & fin_h > 7 ~ fin_h - debut_h,
        TRUE ~ 17 - debut_h
        # fin_h > 7 ~ pmin(fin_h, 17) - pmin(debut_h, 17),
        # TRUE ~ 24 - (debut_h + fin_h)
      ),
      HEURE_SOIR = case_when(
        debut_h == 0 & fin_h == 0 ~ 0,
        fin_h > 7 & fin_h <= 17 ~ 0,
        # fin_h > 17 ~ pmax(fin_h, 17) - pmax(debut_h, 17),
        fin_h >= 17 ~ fin_h - 17,
        debut_h >= 17 ~ 24 - debut_h + fin_h,
        # TRUE ~ 7 + fin_h
        TRUE ~ 7 + fin_h
      )
    ) %>%
    mutate(HEURES = as.numeric(`Heures travaillées`)) %>% 
    filter(HEURES > 0) %>% 
    # Correction des heures en imputant les heures travaillées selon la clé
    # Heures midi et heures soir
    mutate(
      PC_MIDI = HEURE_MIDI / (HEURE_MIDI + HEURE_SOIR),
      PC_SOIR = HEURE_SOIR / (HEURE_MIDI + HEURE_SOIR),
      HEURE_MIDI = HEURES * PC_MIDI,
      HEURE_SOIR = HEURES * PC_SOIR
    ) %>% 
    # filter(HEURES - (HEURE_SOIR + HEURE_MIDI) < 0.2) %>%
    select(DATE,CONTRAT,DEPARTEMENT,HEURE_MIDI,HEURE_SOIR,
           HEURE_DEB,HEURE_FIN,debut_h,fin_h,HEURES,HEURES_PAUSE) %>%
    pivot_longer(cols = c(HEURE_MIDI,HEURE_SOIR), names_to = "CD_HEURE",values_to = "NB_HEURES") %>% 
    group_by(DATE,CONTRAT,DEPARTEMENT,CD_HEURE) %>% 
    summarise(NB_HEURES = sum(NB_HEURES)) %>% 
    left_join(DB_DATE %>% select(DATE,JOUR_SEMAINE)) %>% 
    mutate(CD_HEURE = ifelse(CD_HEURE == "HEURE_MIDI","Midi (<17h)","Soir (>=17h)")) %>% 
    mutate(CD_HEURE = ifelse(JOUR_SEMAINE == "mardi","Soir (>=17h)", CD_HEURE)) %>% 
    mutate(CD_HEURE = ifelse(JOUR_SEMAINE == "dimanche","Midi (<17h)", CD_HEURE)) %>% 
    ungroup()
  
  df_cout <- read_excel(drive_mazette$local_path,sheet = "Cout")
  colnames(df_cout) <- c("CONTRAT","COUT_HEURE")
  
  DB_HEURES <- DB_HEURES %>% left_join(df_cout) %>% 
    mutate(COUT = COUT_HEURE * NB_HEURES)
  
  return(DB_HEURES)
}

DB_COUTS_TRAVAIL <- prepa_heures(id_sheet_heures) |> 
  mutate(
    SECTEUR = case_when(
      DEPARTEMENT == "Transfo alimentaire" ~ "Transformation alimentaire",
      DEPARTEMENT == "Fabrik de boissons" ~ "Brasserie",
      DEPARTEMENT == "Support" ~ "Support",
      DEPARTEMENT == "Service" ~ "Service",
      TRUE ~ "Secteur inconnu"
    ),
    CRENEAU = case_when(
      SECTEUR != "Service" ~ "Journée",
      CD_HEURE == "Soir (>=17h)" ~ "Soir",
      CD_HEURE == "Midi (<17h)" ~ "Midi",
      TRUE ~ "Créneau inconnu"
    ),
  ) |> 
  group_by(DATE,SECTEUR,CRENEAU) |> 
  summarise(
    HEURES = sum(NB_HEURES),
    COUT_TRAVAIL = sum(COUT),
    TAUX_HORAIRE = COUT_TRAVAIL / HEURES,
    .groups = "drop"
  )


#### Serveur ####

server <- function(input, output, session) {

  USER <- reactiveValues(logged = FALSE)

  #### Données préparées (tout en CA HTVA) ####
  UPD_KPI_SIMPLE <- prepa_db(DB_KPI_SIMPLE, "CA_HTVA")
  UPD_OBJECTIFS  <- prepa_db(DB_OBJECTIFS, "CA_HTVA")

  # DB fictives compta (personnel + matières), calées sur le calendrier réel.
  # 4 secteurs : Service / Transformation alimentaire / Brasserie / Support.
  DATES_COMPTA <- DB_DATE %>% filter(DATE <= today()) %>% pull(DATE)

  # CA hebdomadaire réel des 12 derniers mois : sert à calibrer les volumes
  # fictifs pour que food/work/prime cost restent des ordres de grandeur
  # plausibles (et suivent le CA s'il évolue).
  CA_HEBDO_REEL <- UPD_KPI_SIMPLE %>%
    filter(ventes > 0, DATE > today() - 364) %>%
    summarise(s = sum(ventes, na.rm = TRUE) / 52) %>%
    pull(s)
  if (!is.finite(CA_HEBDO_REEL) || CA_HEBDO_REEL <= 0)
    CA_HEBDO_REEL <- CA_HEBDO_DEFAUT

  DB_COUTS_TRAVAIL_FICTIF <- generer_couts_travail(DATES_COMPTA, ca_hebdo = CA_HEBDO_REEL)
  
  # Remplacer pour les données connues du coût du travail
  # date_connues <- DB_COUTS_TRAVAIL |> pull(DATE) |> unique()
  # DB_COUTS_TRAVAIL <- DB_COUTS_TRAVAIL_FICTIF |> 
  #   filter(!DATE %in% date_connues) |> 
  #   add_row(DB_COUTS_TRAVAIL)
  
  DB_COUTS_MATIERE <- generer_couts_matiere(DATES_COMPTA, ca_hebdo = CA_HEBDO_REEL)
  
  # DB_COUTS_MATIERE <- DB_COUTS_MATIERE[0,]
  
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
    select(DATE,SEMAINE,SECTEUR,COUT_MATIERE,ACHATS,VARIATION_STOCK, SIMU) |> 
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
    # paste0("Veille — ", format(date_veille, "%A %d/%m/%Y"))
    "Semaine en cours"
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
  
  # Les 5 semaines qui précèdent la semaine en cours, en une seule matrice
  output$recap_semaines <- renderUI({
    tableau_semaines(UPD_KPI_SIMPLE, UPD_OBJECTIFS,
                     date_debut_semaine - 7, n_semaines = 5)
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
        # Même convention que les barres de CA : vert atteint, ambre à partir
        # de 90 %, rouge en dessous.
        badge("Atteint", if (is.na(pct)) "—" else paste0(pct, " %"),
              couleur_objectif(reel, obj)))
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
  
  # Personnel du jour, par secteur (le service est ventilé par créneau dans la
  # base : on ré-agrège ici pour garder une ligne par secteur)
  output$detail_jour_travail <- renderDT({
    datatable_simple(
      DB_COUTS_TRAVAIL %>%
        filter(DATE == jour_detail()) %>%
        group_by(SECTEUR) |>
        summarise(HEURES = sum(HEURES),
                  COUT_TRAVAIL = sum(COUT_TRAVAIL),
                  TAUX_HORAIRE = COUT_TRAVAIL / HEURES, .groups = "drop") |>
        arrange(SECTEUR) |>
        transmute(Secteur = SECTEUR, Heures = round(HEURES),
                  `Taux/h` = format_CA(TAUX_HORAIRE, 2),
                  Personnel = format_CA(COUT_TRAVAIL, -1))
    )
  })
  
  output$detail_jour_travail_semaine <- renderDT({
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
  
  # Détection de données simulées
  output$detail_jour_simu <- renderUI({
    
    if ("SIMU" %in% colnames(DB_COUTS_TRAVAIL)){
      simu_travail <- DB_COUTS_TRAVAIL %>%
        filter(PREMIER_JOUR_SEMAINE == semaine_detail()) |> 
        summarise(SIMU = sum(SIMU, na.rm = T)) |> pull(SIMU)
    }else{
      simu_travail <- 0
    }
    
    if ("SIMU" %in% colnames(DB_COUTS_MATIERE)){
      simu_matiere <- DB_COUTS_MATIERE %>%
        filter(SEMAINE == floor_date(jour_detail(), unit = "week", week_start = 1)) %>%
        summarise(SIMU = sum(SIMU, na.rm = T)) |> pull(SIMU)
    }else{
      simu_matiere <- 0
    }
    
    bandeau_alerte(simu_travail + simu_matiere > 0,
                   "Résulats basés sur données simulées (coût du travail et matières)")
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
    
    # Détection de données simulées
    output[[id("simu")]] <- renderUI({
      
      d1 <- periode_sel()
      d2 <- fin_periode(d1, unite)
      i <- interval(d1, d2)
      s1 <- floor_date(d1, unit = "week", week_start = 1)
      s2 <- floor_date(d2, unit = "week", week_start = 1)
      i_s <- interval(s1, s2)
      
      if ("SIMU" %in% colnames(DB_COUTS_TRAVAIL)){
        simu_travail <- DB_COUTS_TRAVAIL %>%
          filter(DATE %within% i) %>%
          summarise(SIMU = sum(SIMU, na.rm = T)) |> pull(SIMU)
      }else{
        simu_travail <- 0
      }
      
      if ("SIMU" %in% colnames(DB_COUTS_MATIERE_JOUR)){
        simu_matiere <- DB_COUTS_MATIERE %>%
          filter(SEMAINE %within% i_s) %>%
          summarise(SIMU = sum(SIMU, na.rm = T)) |> pull(SIMU)
      }else{
        simu_matiere <- 0
      }
      
      bandeau_alerte(simu_travail + simu_matiere > 0,
                     "Résulats basés sur données simulées (coût du travail et matières)")
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

  output$bieres_niveaux <- renderUI({
    # La prédiction est déjà calculée pour les autres sorties : on la réutilise
    # pour afficher l'échéance sous chaque jauge.
    cartes_niveaux_bieres(niveau_bieres_actuel(), db_predict_bieres())
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
    
    # Agrégat de toutes les périodes (pour les graphiques d'évolution)
    comptes <- reactive({
      agrege_compta(UPD_KPI_SIMPLE, DB_COUTS_TRAVAIL, DB_COUTS_MATIERE, unite)
    })

    # Liste des périodes proposées dans les deux sélecteurs
    observe({
      # dispo <- liste_periodes_dispo(UPD_KPI_SIMPLE, unite)
      dispo <- sort(unique(comptes()$PERIODE),decreasing = TRUE)
      req(length(dispo) > 0)
      choix <- setNames(as.character(dispo), label_periode(dispo, unite))
      updateSelectInput(session, id("a"), choices = choix,
                        selected = as.character(dispo[1]))
      updateSelectInput(session, id("b"), choices = choix,
                        selected = as.character(dispo[min(2, length(dispo))]))
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
  
  output$annee_marge <- renderPlotly({
    serie_m1 <- serie_annee() |> mutate(MARGE = 0)
    graph_ecart_ym1(UPD_KPI_SIMPLE, annee_val(), var = "marge",
                    serie = serie_annee(), serie_m1 = serie_m1)
  })

  output$annee_ecart_marge <- renderPlotly({
    graph_ecart_ym1(UPD_KPI_SIMPLE, annee_val(), var = "marge",
                    serie = serie_annee(), serie_m1 = serie_annee_m1())
  })

  #### Volet "Travail" ####

  # Fenêtre par défaut : 12 mois glissants (une occurrence de chaque jour de
  # semaine, comme dans l'étude de rentabilité)
  debut_travail <- floor_date(date_veille, "month") %m-% months(12)
  observe({
    updateDateRangeInput(session, "trav_periode",
                         start = debut_travail, end = date_veille)
    updateDateRangeInput(session, "cren_periode",
                         start = debut_travail, end = date_veille)
  })

  fenetre_travail <- function(rng) {
    if (is.null(rng) || any(is.na(rng))) c(debut_travail, date_veille) else rng
  }

  # --- Sous-onglet "Suivi" ---
  trav_base <- reactive({
    p <- fenetre_travail(input$trav_periode)
    base_travail(DB_PRODUITS_JOURS_FULL, DB_COUTS_TRAVAIL, p[1], p[2])
  })

  trav_agrege <- reactive({
    agrege_travail(trav_base(), unite = input$trav_unite)
  })

  output$trav_kpi <- renderUI({
    kpi_travail_tiles(trav_agrege())
  })

  output$trav_structure <- renderPlotly({
    graph_structure_travail(trav_agrege(), unite = input$trav_unite)
  })

  output$trav_productivite <- renderPlotly({
    graph_productivite_temps(trav_agrege(), unite = input$trav_unite)
  })

  output$trav_ca_creneaux <- renderPlotly({
    graph_ca_creneaux_temps(
      agrege_creneaux_periode(trav_base(), unite = input$trav_unite),
      unite = input$trav_unite)
  })

  # --- Sous-onglet "Créneaux" ---
  cren_stats <- reactive({
    p <- fenetre_travail(input$cren_periode)
    stats_creneaux(base_travail(DB_PRODUITS_JOURS_FULL, DB_COUTS_TRAVAIL,
                                p[1], p[2]))
  })

  output$cren_heatmap <- renderPlotly({
    graph_heatmap_creneaux(cren_stats(), var = input$cren_indicateur)
  })

  output$cren_nuage <- renderPlotly({
    graph_nuage_creneaux(cren_stats())
  })

  output$cren_classement <- renderPlotly({
    graph_productivite_creneaux(cren_stats())
  })

  output$cren_decomposition <- renderPlotly({
    graph_decomposition_creneaux(cren_stats())
  })

  output$cren_table <- renderDT({
    datatable_simple(table_creneaux(cren_stats()))
  })

  #### Volet "Bières" — consommation ####

  # Référentiel des vraies bières, calculé une seule fois
  REF_BIERES <- ref_bieres(DB_PRODUITS)

  # Semaines proposées (la semaine en cours, partielle, est exclue)
  observe({
    sems <- semaines_dispo(DB_TICKET)
    req(length(sems) > 0)
    updateSelectInput(session, "conso_semaine",
                      choices = setNames(as.character(sems),
                                         paste0("Sem. du ", format(sems, "%d/%m/%Y"))),
                      selected = as.character(sems[1]))
  })

  conso_sem <- reactive({
    req(input$conso_semaine)
    as.Date(input$conso_semaine)
  })

  conso_comp <- reactive({
    conso_bieres_comparee(DB_TICKET, REF_BIERES, conso_sem())
  })

  conso_formats <- reactive({
    formats_bieres(DB_TICKET, REF_BIERES, conso_sem())
  })

  conso_horaire <- reactive({
    conso_bieres_horaire(DB_TICKET, REF_BIERES, conso_sem())
  })

  output$conso_kpi <- renderUI({
    kpi_bieres_tiles(conso_comp(), conso_formats(), conso_horaire())
  })

  output$conso_top <- renderPlotly({
    graph_top_bieres(conso_comp())
  })

  output$conso_tendance <- renderPlotly({
    graph_tendance_bieres(
      evo_top_bieres(DB_TICKET, REF_BIERES, conso_sem(),
                     n_top = 5, n_semaines = 12),
      semaine = conso_sem())
  })

  output$conso_heatmap <- renderPlotly({
    graph_heatmap_bieres(conso_bieres_jour_heure(DB_TICKET, REF_BIERES, conso_sem()))
  })

  output$conso_formats <- renderPlotly({
    graph_formats_bieres(conso_formats())
  })

  output$conso_evo <- renderPlotly({
    graph_evo_conso_bieres(
      evo_conso_bieres(DB_TICKET, REF_BIERES, n_semaines = 26,
                       fin = conso_sem() + 6),
      semaine = conso_sem())
  })

  output$conso_table <- renderDT({
    datatable_simple(table_conso_bieres(conso_comp()))
  })

  #### Volet "Pizzwanze" ####

  # Dates des soirées, calculées une seule fois
  SOIREES_PIZZWANZE <- soirees_pizzwanze(DB_PRODUITS)

  observe({
    req(length(SOIREES_PIZZWANZE) > 0)
    choix <- rev(SOIREES_PIZZWANZE)   # la plus récente en tête
    updateSelectInput(session, "pizz_soiree",
                      choices = setNames(as.character(choix),
                                         format(choix, "%a %d/%m/%Y")),
                      selected = as.character(choix[1]))
  })
  
  output$pizz_titre <- renderText({
    paste0("La carte du ",format(as.Date(input$pizz_soiree), "%A %d/%m/%Y"))
  })

  pizz_data <- reactive({
    req(input$pizz_soiree)
    pizzwanze_soiree(DB_PRODUITS, DB_TICKET, as.Date(input$pizz_soiree), SOIREES_PIZZWANZE)
  })

  pizz_hist <- reactive({
    historique_pizzwanze(DB_PRODUITS, SOIREES_PIZZWANZE)
  })

  output$pizz_kpi <- renderUI({
    kpi_pizzwanze_tiles(pizz_data())
  })

  output$pizz_soiree <- renderPlotly({
    graph_pizzas_soiree(pizz_data())
  })

  output$pizz_heure <- renderPlotly({
    graph_pizzas_heure(pizzas_par_heure(DB_TICKET, as.Date(input$pizz_soiree)))
  })

  output$pizz_carte <- renderPlotly({
    # n <- suppressWarnings(as.integer(input$pizz_profondeur))
    graph_carte_pizzwanze(DB_PRODUITS, SOIREES_PIZZWANZE, n_soirees = 12)
  })

  output$pizz_evo <- renderPlotly({
    graph_evo_pizzwanze(pizz_hist(), soiree = as.Date(input$pizz_soiree))
  })

  output$pizz_table <- renderDT({
    datatable_simple(table_pizzwanze(pizz_data()))
  })

  #### Volet "Focaccias" ####

  observe({
    sems <- semaines_dispo(DB_PRODUITS)
    req(length(sems) > 0)
    updateSelectInput(session, "foca_semaine",
                      choices = setNames(as.character(sems),
                                         paste0("Sem. du ", format(sems, "%d/%m/%Y"))),
                      selected = as.character(sems[1]))
  })

  foca_sem <- reactive({
    req(input$foca_semaine)
    as.Date(input$foca_semaine)
  })

  foca_data <- reactive({
    focaccias_semaine(DB_PRODUITS, foca_sem())
  })

  foca_evo <- reactive({
    evo_focaccias(DB_PRODUITS, n_semaines = 26, fin = foca_sem() + 6)
  })

  output$foca_kpi <- renderUI({
    kpi_focaccias_tiles(foca_data())
  })

  output$foca_jour <- renderPlotly({
    fs <- foca_data()
    graph_focaccias_jour(focaccias_par_jour(fs$act, foca_sem()),
                         focaccias_par_jour(fs$prec, foca_sem() - 7))
  })

  output$foca_variantes <- renderPlotly({
    graph_variantes_focaccias(focaccias_variantes(foca_data()$act))
  })

  output$foca_evo <- renderPlotly({
    graph_evo_focaccias(foca_evo(), semaine = foca_sem())
  })

  output$foca_options <- renderPlotly({
    graph_options_focaccias(foca_evo())
  })

  output$foca_table <- renderDT({
    datatable_simple(table_focaccias(foca_data()))
  })

  #### Volet "Focaccias" — carte Production ####
  # Le préremplissage vient des dernières semaines COMPLÈTES des données, et
  # non de la semaine sélectionnée dans la barre latérale : on prépare la
  # production à venir, pas celle d'une semaine consultée dans l'historique.

  prod_base <- reactive({
    production_focaccias_base(DB_PRODUITS, n_semaines = 3, marge = 1+input$prod_multi/100)
  })

  # (Ré)applique les valeurs par défaut. La ligne libre reste vide.
  appliquer_prefill <- function() {
    b <- prod_base()
    for (i in b$ID) {
      ligne <- b[b$ID == i, ]
      updateNumericInput(session, paste0("prod_foc_", i),
                         value = if (is.na(ligne$FOCACCIAS)) NA
                                 else round(ligne$FOCACCIAS))
      updateNumericInput(session, paste0("prod_por_", i), value = ligne$PORTION)
      updateNumericInput(session, paste0("prod_stk_", i), value = NA)
    }
    updateTextInput(session, "prod_nom_5", value = "")
  }

  observe({ appliquer_prefill() })
  observeEvent(input$prod_reset, { appliquer_prefill() })

  output$prod_source <- renderText({
    b <- prod_base()
    n <- unique(b$SEMAINES)
    
    n_base <- pull(b[b$NOM == "Légume","FOCACCIAS"])
    n_fromage <- pull(b[b$NOM == "Fromage","FOCACCIAS"])
    n_viande <- pull(b[b$NOM == "Viande","FOCACCIAS"])
    
    info_sup <- paste0(n_base," bases, ",n_fromage," fromages, ",
                       n_viande," viandes.")
    
    if (length(n) == 0 || n[1] == 0)
      "Aucune semaine complète disponible : les champs sont vides."
    else paste0("Pré-rempli avec maximum des ", n[1],
                " dernières semaines (+",input$prod_multi,"%) : ",info_sup)
  })

  # Une paire de sorties calculées par ligne : quantité nécessaire, puis
  # quantité à produire une fois le stock déduit.
  for (i in INGREDIENTS_FOCACCIA$ID) {
    local({
      idx <- i
      qte_necessaire <- reactive({
        foc <- input[[paste0("prod_foc_", idx)]]
        por <- input[[paste0("prod_por_", idx)]]
        if (is.null(foc) || is.null(por) || is.na(foc) || is.na(por)) NA_real_
        else foc * por
      })

      output[[paste0("prod_nec_", idx)]] <- renderText({
        format_qte_g(qte_necessaire())
      })

      output[[paste0("prod_faire_", idx)]] <- renderText({
        nec <- qte_necessaire()
        if (is.na(nec)) return("—")
        stk <- input[[paste0("prod_stk_", idx)]]
        stk <- if (is.null(stk) || is.na(stk)) 0 else stk
        reste <- nec - stk
        # Un stock supérieur au besoin n'est pas une production négative :
        # on l'annonce comme un surplus.
        if (reste <= 0) paste0("0 g (surplus ", format_qte_g(-reste), ")")
        else format_qte_g(reste)
      })
    })
  }

}
