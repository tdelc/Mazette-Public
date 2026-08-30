options(DT.options = list(pageLength = 5, language = list(search = 'Filter:')))

#### Chargement initial des données ####

prefix         <- "R_new_env_"
date_jour      <- format(now() - days(1), format = "%Y-%m-%d")
drive_env_name <- paste0(prefix, date_jour, ".RData")

connexion_ou_creation(drive_env_name, prefix, force_dl = FALSE)

#### Serveur ####

server <- function(input, output, session) {

  # logged : connecté ou non. onglets : les clés auxquelles ce mot de passe
  # donne droit (cf. R/acces.R), qui pilotent la barre de navigation.
  USER <- reactiveValues(logged = FALSE, nom = NULL, role = NULL,
                         onglets = character(0))

  #### Données préparées ####
  UPD_JOURS <- reactive({
    req(input$unite_tva)
    prepa_db(DB_JOURS, paste0("CA_",input$unite_tva))
  })  
  UPD_KPI_SIMPLE <- reactive({
    req(input$unite_tva)
    prepa_db(DB_KPI_SIMPLE, paste0("CA_",input$unite_tva))
  })
  UPD_OBJECTIFS  <- reactive({
    req(input$unite_tva)
    prepa_db(DB_OBJECTIFS, paste0("CA_",input$unite_tva))
  })
  
  # Coûts matière : uniquement la comptabilité réelle, au mois. Aucune donnée
  # simulée — une période absente de DB_COMPTA reste vide, elle n'est pas
  # comblée. Le pilotage vaut mieux vide que faux.
  DB_COUTS_MATIERE <- reactive({
    DB_COMPTA %>%
      filter(TYPE == "compte", AGREGE,
             CATEGORIE %in% c("ACHATS", "VARIATION_STOCK"),
             SECTION == "Coût des ventes et prestations") %>%
      group_by(ANNEE, MOIS, SECTEUR, CATEGORIE) %>%
      summarise(VALEUR = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = CATEGORIE, values_from = VALEUR) %>%
      mutate(ACHATS = replace_na(ACHATS, 0),
             VARIATION_STOCK = replace_na(VARIATION_STOCK, 0)) %>%
      normalise_couts_matiere(granularite = "mois")
  })

  DB_COUTS_TRAVAIL <- DB_COUTS_TRAVAIL |> 
    left_join(creer_db_date() |> select(DATE,PREMIER_JOUR_SEMAINE,PREMIER_JOUR_MOIS), by = "DATE")
  
  DB_COUTS_MATIERE_JOUR <- reactive({
    couts_matiere_par_jour(DB_COUTS_MATIERE(), creer_db_date())
  })
  
  # Dernier jour d'ouverture (= "veille")
  date_veille <- DB_KPI_SIMPLE %>%
    filter(CA_HTVA > 0, DATE < today()) %>%
    summarise(d = max(DATE)) %>%
    pull(d)

  #### Login ####
  # Le mot de passe ne dit plus seulement « oui / non » : il désigne un profil,
  # et donc une liste d'onglets (cf. R/acces.R et l'onglet « IMPORT PASS » du
  # Sheet). On construit la barre de navigation à partir de cette liste.
  observeEvent(input$boutton_log, {
    acces <- verifie_acces(DB_PASSWORD, input$password)

    if (is.null(acces)) {
      output$text_log <- renderText("Erreur dans le mot de passe")
      return()
    }

    USER$logged  <- TRUE
    USER$nom     <- acces$NOM
    USER$role    <- acces$ROLE
    USER$onglets <- acces$ONGLETS

    # Les onglets autorisés sont insérés à la suite de l'accueil, dans l'ordre
    # du catalogue. Ceux qui manquent ne sont pas masqués : ils n'ont jamais
    # été envoyés au navigateur, et leurs sorties ne seront jamais calculées.
    precedent <- ONGLET_ACCUEIL
    for (cle in setdiff(acces$ONGLETS, ONGLET_ACCUEIL)) {
      nav_insert("nav", panneau_onglet(cle), target = precedent,
                 position = "after", session = session)
      precedent <- cle
    }

    # Une carte d'accueil qui renverrait vers un onglet interdit n'a pas de
    # sens : seules celles des onglets autorisés sont révélées.
    for (i in seq_len(nrow(CARTES_ACCUEIL)))
      if (CARTES_ACCUEIL$CLE[i] %in% acces$ONGLETS)
        shinyjs::show(paste0("carte_", CARTES_ACCUEIL$CLE[i]))

    shinyjs::hide("login_screen")
    shinyjs::show("app_screen")
  })

  # Qui est connecté, discrètement, à droite de la barre : avec plusieurs mots
  # de passe en circulation, c'est la seule façon de savoir lequel on utilise
  # — et pourquoi tel onglet manque. Le bouton de déconnexion vit ici plutôt
  # que dans la coquille : il n'a de sens qu'une fois connecté, et le req()
  # ci-dessous suffit à le faire apparaître et disparaître avec le badge.
  output$badge_utilisateur <- renderUI({
    req(USER$logged)
    tagList(
      span(class = "badge-utilisateur",
           USER$nom,
           if (!is.na(USER$role) && nzchar(USER$role))
             tags$span(class = "role", paste0(" (", USER$role, ")"))),
      actionLink(
        "deconnexion", class = "lien-deconnexion",
        # title = l'infobulle au survol ; aria-label = le nom annoncé, qui
        # reprend le libellé visible (WCAG 2.5.3) et reste juste quand le mot
        # est masqué sur téléphone.
        title = "Se déconnecter", `aria-label` = "Déconnexion",
        label = tagList(
          icon("right-from-bracket"),
          # Le mot disparaît sous 992 px : sur téléphone la barre est déjà
          # chargée, et l'icône suffit (le title reste pour le survol).
          tags$span(class = "d-none d-lg-inline ms-1", "Déconnexion")))
    )
  })

  # Déconnexion : on recharge la page plutôt que de défaire l'insertion des
  # onglets un à un.
  #
  # Défaire à la main voudrait dire retirer chaque onglet inséré, remasquer les
  # cartes, revider le champ mot de passe — et surtout se souvenir de tout ce
  # que la session a accumulé entre-temps : périodes saisies, prix simulés,
  # lignes sélectionnées dans les tableaux. Un oubli, et la personne suivante
  # hérite de l'état de la précédente. Le rechargement, lui, ne peut rien
  # oublier.
  #
  # Il est peu coûteux : les données sont chargées en tête de server.R, hors de
  # la fonction serveur, donc une fois par processus R et non par session. La
  # page revient sur l'écran de connexion sans retoucher au .RData.
  observeEvent(input$deconnexion, {
    session$reload()
  })

  #### Volet "Accueil" ####

  # Bandeau : CA de la veille, de la semaine et du mois, chacun face à son
  # objectif. Suit le sélecteur HTVA/TVAC.
  output$accueil_kpi <- renderUI({
    kpi_accueil(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), date_veille, input$unite_tva)
  })

  # Une carte par onglet. Celles qui portent des euros suivent la TVA ; les
  # autres (fûts, réservations) sont en volumes et n'en dépendent pas.
  output$acc_maintenant <- renderUI({
    acc_maintenant(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), date_veille)
  })
  output$acc_annee <- renderUI({
    acc_annee(UPD_KPI_SIMPLE(), date_veille)
  })
  output$acc_futs <- renderUI({
    acc_futs(DB_BIERES, tryCatch(db_predict_bieres(), error = function(e) NULL))
  })
  output$acc_bieres <- renderUI({
    acc_bieres(DB_TICKET, DB_PRODUITS, input$unite_tva)
  })
  output$acc_focaccias <- renderUI({
    acc_focaccias(DB_PRODUITS, input$unite_tva)
  })
  output$acc_pizzwanze <- renderUI({
    acc_pizzwanze(DB_PRODUITS, input$unite_tva)
  })
  output$acc_reservations <- renderUI({
    acc_reservations(RESA())
  })
  output$acc_compta <- renderUI({
    acc_compta(if (exists("DB_COMPTA")) DB_COMPTA else NULL)
  })

  # Les boutons « Aller plus loin » basculent sur l'onglet correspondant.
  # Bouton et onglet cible viennent tous deux de CARTES_ACCUEIL (R/acces.R) :
  # ajouter une carte, c'est ajouter une ligne, et il n'y a plus deux listes à
  # tenir d'accord (l'ancienne visait « tab_bieres », qui n'existe pas).
  for (i in seq_len(nrow(CARTES_ACCUEIL))) {
    # local() fige l'indice : sans lui, les huit observeEvent partageraient la
    # dernière valeur de la boucle et renverraient tous vers le même onglet.
    local({
      bouton <- CARTES_ACCUEIL$BOUTON[i]
      cible  <- CARTES_ACCUEIL$CLE[i]
      observeEvent(input[[bouton]], {
        nav_select("nav", cible, session = session)
      }, ignoreInit = TRUE)
    })
  }

  #### Volet "Maintenant" — Indicateurs clés ####
  ca_periode <- function(db, d1, d2) {
    db %>% filter(DATE >= d1, DATE <= d2) %>% summarise(s = sum(ventes, na.rm = TRUE)) %>% pull(s)
  }

  output$vb_ca_veille <- renderText({
    format_CA(ca_periode(UPD_KPI_SIMPLE(), date_veille, date_veille), -1)
  })

  output$vb_ca_semaine <- renderText({
    format_CA(ca_periode(UPD_KPI_SIMPLE(), date_debut_semaine, today()), -1)
  })

  output$vb_pct_semaine <- renderText({
    reel <- ca_periode(UPD_KPI_SIMPLE(), date_debut_semaine, today()-1)
    obj  <- ca_periode(UPD_OBJECTIFS(), date_debut_semaine, today()-1)
    if (is.na(obj) || obj == 0) "—" else paste0(round(100 * reel / obj), " %")
  })

  #### Volet "Maintenant" — Veille ####
  output$titre_veille <- renderText({
    "Semaine en cours"
  })
  
  output$title_vb_veille <- renderUI({
    titre_avec_tva("CA de la veille", input$unite_tva)
  })

  output$box_veille <- renderUI({
    box_ventes_jour(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), date_veille, 0,
                    format_date = "%d/%m", unite_tva = input$unite_tva)
  })

  output$top_veille <- renderDT({
    datatable_simple(
      top_produits_periode(DB_PRODUITS, date_veille, date_veille, n = 10, 
                           unite_tva = input$unite_tva)
    )
  })

  #### Volet "Maintenant" — Semaine en cours ####
  output$box_semaine <- renderUI({
    box_ventes_jour(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), date_debut_semaine, 6,
                    unite_tva = input$unite_tva)
  })
  
  output$title_vb_semaine <- renderUI({
    titre_avec_tva("CA de la semaine", input$unite_tva)
  })

  output$box_semaine_total <- renderUI({
    box_ventes_total(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), date_debut_semaine, 6,
                     titre = "Total semaine", is_semaine = TRUE,
                     unite_tva = input$unite_tva)
  })
  
  # Les 5 semaines qui précèdent la semaine en cours, en une seule matrice
  output$recap_semaines <- renderUI({
    tableau_semaines(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                     date_debut_semaine - 7, n_semaines = 5,
                     unite_tva = input$unite_tva)
  })
 
  
  #### Volet "Maintenant" — Progression du mois ####
  
  # Liste des mois disponibles (du plus récent au plus ancien)
  observe({
    mois_dispo <- UPD_KPI_SIMPLE() %>%
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
    box_ventes_total(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), mois_choisi(), 
                     days_in_month(mois_choisi())-1, 
                     titre = "Total mois", is_semaine = TRUE,
                     unite_tva = input$unite_tva)
  })
  
  prog_data <- reactive({
    progression_mois(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), mois_choisi())
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
        badge("Réalisé", format_CA(reel, -1), COUL_BRUN),
        badge("Objectif", format_CA(obj, -1), COUL_AMBRE),
        # Même convention que les barres de CA : vert atteint, ambre à partir
        # de 90 %, rouge en dessous.
        badge("Atteint", if (is.na(pct)) "—" else paste0(pct, " %"),
              couleur_objectif(reel, obj)))
  })

  #### Volet "Maintenant" — Produits de la semaine ####
  output$top_semaine <- renderDT({
    datatable_simple(
      top_produits_periode(DB_PRODUITS,
                           date_debut_semaine, date_debut_semaine + 6, n = 10, 
                           unite_tva = input$unite_tva)
    )
  })

  output$hausse_semaine <- renderDT({
    datatable_simple(
      evolution_produits_semaine(DB_PRODUITS, date_debut_semaine,
                                 sens = "hausse")
    )
  })

  output$baisse_semaine <- renderDT({
    datatable_simple(
      evolution_produits_semaine(DB_PRODUITS, date_debut_semaine,
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
    graph_ca_jour(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), p[1], p[2], source = "detail_jour")
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
    box_ventes_jour(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), jour_detail(), 0,
                    format_date = "%d/%m", width = "100%",
                    unite_tva = input$unite_tva)
  })

  output$detail_jour_produits <- renderDT({
    datatable_simple(
      top_produits_periode(DB_PRODUITS, jour_detail(), jour_detail(), n = 15, 
                           unite_tva = input$unite_tva)
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
      DB_COUTS_MATIERE() %>%
        couts_matiere_du_jour(jour_detail()) %>%
        transmute(Secteur = SECTEUR,
                  Période = ifelse(GRANULARITE == "mois",
                                   format(PERIODE, "%B %Y"),
                                   paste("Sem.", format(PERIODE, "%d/%m"))),
                  Achats = format_CA(ACHATS, -1),
                  Stock = ifelse(STOCK_CONNU, format_CA(VARIATION_STOCK, -1), "—"),
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
      graph_ca_periode(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), p[1], p[2],
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
    
    bornes <- reactive({
      d1 <- periode_sel()
      list(d1 = d1, d2 = fin_periode(d1, unite))
    })

    ca <- reactive({
      b <- bornes()
      UPD_KPI_SIMPLE() |> filter(DATE >= b$d1, DATE <= b$d2) |>
        pull(ventes) |> sum(na.rm = TRUE)
    })

    # Matieres : ventilees par secteur dans la compta, etalees au jour. Sur un
    # mois entier le total est exact ; sur une semaine c'est un prorata, signale
    # comme tel plutot que presente comme une mesure hebdomadaire.
    cout_matiere <- reactive({
      b <- bornes()
      matieres_par_secteur(DB_COUTS_MATIERE_JOUR(), b$d1, b$d2)
    })

    # Travail : DB_HEURES tant qu'elle couvre la periode, sinon le total de la
    # comptabilite (qui n'a pas de ventilation par secteur).
    cout_travail <- reactive({
      b <- bornes()
      travail_par_secteur(DB_COUTS_TRAVAIL,
                          if (exists("DB_COMPTA")) DB_COMPTA else NULL,
                          b$d1, b$d2)
    })

    apercu <- reactive({
      b <- bornes()
      req(exists("DB_COMPTA"))
      apercu_exploitation(DB_COMPTA, b$d1, b$d2)
    })

    output[[id("kpi")]] <- renderUI({
      a <- apercu()
      if (is.null(a))
        return(div(class = "text-muted small",
                   "Pas de comptabilite sur cette periode. Les indicateurs de ",
                   "gestion sont mensuels : ils apparaissent sur le sous-onglet ",
                   "Par mois."))
      kpi_exploitation(a, "mois")
    })

    marge <- reactive({ marge_par_secteur(cout_matiere(), cout_travail(), ca()) })

    output[[id("titre")]] <- renderText({
      d1 <- periode_sel()
      d2 <- fin_periode(d1, unite)
      paste0(label_periode(d1, unite), "  (",
             format(d1, "%d/%m"), " → ", format(d2, "%d/%m/%Y"), ")")
    })

    output[[id("repartition")]] <- renderPlotly({
      graph_repartition_periode(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                                periode_sel(), unite = unite)
    })

    output[[id("box")]] <- renderUI({
      d1 <- periode_sel()
      box_ventes_total(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(), d1,
                       as.numeric(fin_periode(d1, unite) - d1),
                       titre = label_periode(d1, unite), is_semaine = TRUE,
                       unite_tva = input$unite_tva)
    })
    
    output[[id("travail")]] <- renderDT({
      t <- cout_travail()
      if (is.null(t) || !nrow(t))
        return(datatable_simple(tibble(`Coût du travail` =
          "Aucune donnée d'heures ni de comptabilité sur la période.")))
      if (identical(t$SOURCE[1], "heures"))
        datatable_simple(t |> transmute(
          Secteur = SECTEUR, Heures = round(HEURES),
          `Taux/h` = format_CA(TAUX_HORAIRE, 2),
          Personnel = format_CA(COUT_TRAVAIL, -1)))
      else
        # Hors couverture de DB_HEURES : la compta donne le total, pas la
        # ventilation par secteur ni les heures.
        datatable_simple(t |> transmute(
          Secteur = SECTEUR, Personnel = format_CA(COUT_TRAVAIL, -1),
          Source = "comptabilité"))
    })

    output[[id("cout")]] <- renderDT({
      m <- cout_matiere()
      if (is.null(m) || !nrow(m))
        return(datatable_simple(tibble(`Coût matière` =
          "Aucune comptabilité sur la période.")))
      datatable_simple(m |> transmute(
        Secteur = SECTEUR, Achats = format_CA(ACHATS, -1),
        Stock = ifelse(STOCK_CONNU, format_CA(VARIATION_STOCK, -1), "—"),
        `Matières` = format_CA(COUT_MATIERE, -1)))
    })

    output[[id("prorata")]] <- renderUI({
      m <- cout_matiere()
      bandeau_alerte(!is.null(m) && nrow(m) && isTRUE(m$PRORATA[1]),
        paste("La comptabilité est mensuelle : les coûts affichés ici sont un",
              "prorata du mois sur les jours de la période. Le total du mois est",
              "juste, sa répartition à l'intérieur du mois est une hypothèse."),
        titre = "Coûts au prorata", couleur = COUL_AMBRE,
        icone = "circle-info")
    })

    output[[id("marge")]] <- renderDT({
      m <- marge()
      if (is.null(m) || !nrow(m))
        return(datatable_simple(tibble(Marge = "Aucun coût sur la période.")))
      datatable_simple(m |> transmute(
        Secteur = SECTEUR,
        Personnel = format_CA(COUT_TRAVAIL, -1),
        `Matières` = format_CA(COUT_MATIERE, -1),
        Total = format_CA(TOTAL, -1),
        `Chiffre d'affaires` = format_CA(CA, -1),
        `% du CA` = format_pct(PCT_CA)))
    })

    output[[id("produits")]] <- renderDT({
      d1 <- periode_sel()
      datatable_simple(
        top_produits_periode(DB_PRODUITS, d1, fin_periode(d1, unite), n = 20, 
                             unite_tva = input$unite_tva)
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
    liste_produits_periode(DB_PRODUITS, p[1], p[2], unite_tva = input$unite_tva)
  })

  output$detail_produit_liste <- renderDT({

    col_name <- paste("CA", input$unite_tva)

    df <- produits_df() %>%
      transmute(Produit = tronque_nom(Produit),
                Quantité = Quantite,
                !!sym(col_name) := format_CA(CA, -1))
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
    evolution_un_produit(DB_PRODUITS, pr, min(DB_PRODUITS$DATE), today(), 
                         unite_tva = input$unite_tva)
  })
  
  evo_produit_periode <- reactive({
    pr <- produit_choisi()
    req(pr)
    evolution_un_produit(DB_PRODUITS, pr, periode_produit_detail()[1], today(),
                         unite_tva = input$unite_tva)
  })

  output$detail_produit_graph <- renderPlotly({
    graph_evolution_produit(evo_produit(), produit_choisi())
  })

  output$detail_produit_table <- renderDT({

    col_name <- paste("CA", input$unite_tva)

    category <- evo_produit_periode() |> pull(CATEGORIE) |> unique() |> str_to_title()
    category_column <- paste0("Part dans '",category,"'")
    
    df <- evo_produit_periode() %>%
      transmute(Semaine = format(SEMAINE, "%d/%m/%Y"),
                Quantité = Quantite,
                !!sym(col_name) := format_CA(CA, -1),
                `Part dans Total` = paste0(round(PC_ALL*100,0),"%"),
                !!sym(category_column) := paste0(round(PC_CATEGORIE*100,0),"%")
                ) %>%
      arrange(desc(Semaine))
    
    datatable(df, selection = "none", rownames = FALSE,
              options = list(pageLength = 12, dom = 'tp'))
  })

  #### Volet "Historique" — CA par semaine / mois ####
  output$hist_graph <- renderPlotly({
    graph_historique(UPD_JOURS(), UPD_OBJECTIFS(),
                     unite = input$hist_unite, n = input$hist_n)
  })

  output$hist_evo <- renderPlotly({
    graph_historique_tendance(UPD_JOURS(), UPD_OBJECTIFS(),
                     unite = input$hist_unite, n = input$hist_n)
  })
  

  #### Volet "Fût" ####

  # Évolution + prédictions des fûts en cours (calcul HoltWinters, une seule fois)
  db_predict_bieres <- reactive({
    table_evo_brassins(DB_BIERES,today())
  })

  output$bieres_niveaux <- renderUI({
    # La prédiction est déjà calculée pour les autres sorties : on la réutilise
    # pour afficher l'échéance sous chaque jauge.
    cartes_niveaux_bieres(niveau_bieres_actuel(DB_BIERES), db_predict_bieres())
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
    prepa_simulation(DB_PRODUITS, p[1], p[2])
  })

  sim_prix <- reactiveVal(NULL)

  # (Ré)initialise les prix simulés quand la base change + remplit les catégories
  observeEvent(sim_base(), {
    sim_prix(sim_base()$PRIX_MOYEN)
    updateSelectInput(session, "sim_categorie",
                      choices = sort(unique(sim_base()$CATEGORIE)))
  })

  # Appliquer une variation % à toute une catégorie
  observeEvent(input$sim_apply, {
    base <- sim_base()
    cur  <- sim_prix()
    if (is.null(cur) || length(cur) != nrow(base)) cur <- base$PRIX_MOYEN
    idx <- base$CATEGORIE == input$sim_categorie
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
  #### Volet "Compta / Gestion" — Exploitation ####

  # Tout vient de DB_COMPTA. Aucune donnee simulee : si les chiffres ne sont pas
  # la, le volet reste vide.
  expl_postes <- reactive({
    req(exists("DB_COMPTA"))
    postes_exploitation(DB_COMPTA)
  })

  expl_serie <- reactive({
    p <- agrege_exploitation(expl_postes(), input$expl_unite %||% "mois")
    req(nrow(p) > 0)
    n <- as.integer(input$expl_nb %||% 12)
    tail(p, n)
  })

  observe({
    p <- expl_postes()
    req(nrow(p) > 0)
    dispo <- sort(unique(p$PERIODE), decreasing = TRUE)
    updateSelectInput(session, "expl_periode",
                      choices = setNames(as.character(dispo),
                                         format(dispo, "%B %Y")),
                      selected = as.character(dispo[1]))
  })

  # La cascade porte sur la periode choisie ; les autres vues sur la serie.
  expl_une <- reactive({
    p <- expl_postes()
    req(nrow(p) > 0)
    d <- if (is.null(input$expl_periode)) max(p$PERIODE) else as.Date(input$expl_periode)
    filter(p, PERIODE == d)
  })

  output$expl_kpi      <- renderUI({ kpi_exploitation(expl_une()) })
  output$expl_cascade  <- renderPlotly({ graph_cascade_exploitation(expl_une()) })
  output$expl_structure <- renderPlotly({
    graph_structure_exploitation(expl_serie(), input$expl_unite %||% "mois") })
  output$expl_table <- renderDT({
    datatable_simple(table_exploitation(expl_serie(), input$expl_unite %||% "mois",
                                        en_pct = isTRUE(input$expl_pct)))
  })
  output$expl_controle <- renderUI({
    ctrl <- controle_exploitation(DB_COMPTA, expl_postes())
    n <- sum(abs(ctrl$ECART) > 1, na.rm = TRUE)
    bandeau_alerte(n > 0, paste0(
      n, " periode(s) ou la marge recomposee differe du solde comptable. ",
      "Un compte echappe au classement en postes."))
  })


  #### Volet "Compta / Gestion" — Comptabilité générale ####

  # Plus de reconstruction de plan : les comptes sont classés sur leur numéro
  # (cf. R/plan_comptable.R), structure du PCMN qui ne bouge pas.

  observe({
    req(exists("DB_COMPTA"))
    p <- periodes_compta(DB_COMPTA)
    updateSelectizeInput(session, "cg_periodes",
                         choices = setNames(as.character(p$PERIODE), p$LIBELLE),
                         selected = as.character(head(p$PERIODE, 3)))
  })

  cg_periodes <- reactive({
    req(input$cg_periodes)
    sort(as.Date(input$cg_periodes))
  })

  output$cg_titre <- renderText({
    n <- length(cg_periodes())
    paste0("Compte de résultat — ", n, if (n > 1) " mois comparés" else " mois")
  })

  output$cg_kpi <- renderUI({ kpi_compta_generale(DB_COMPTA, cg_periodes()) })

  output$cg_table <- renderDT({
    tbl <- table_compte_resultat(DB_COMPTA, cg_periodes(),
                                 detail = isTRUE(input$cg_detail),
                                 en_pct = isTRUE(input$cg_pct))
    datatable(tbl, rownames = FALSE, escape = FALSE, selection = 'single',
              options = list(pageLength = 200, dom = "ft", scrollX = TRUE,
                             ordering = FALSE,
                             columnDefs = list(list(className = "dt-right",
                                                    targets = 2:(ncol(tbl) - 1))),
                             language = list(search = "Filtrer :"))) %>%
      formatStyle("Compte", target = "row", fontWeight = styleEqual("", "bold"))
  })

  output$cg_soldes <- renderPlotly({ graph_soldes(DB_COMPTA, cg_periodes()) })

  # Contrôle : un compte que le plan ne sait pas ranger n'entre dans aucun total.
  output$cg_controle <- renderDT({
    nc <- comptes_non_classes(DB_COMPTA)
    if (!nrow(nc))
      return(datatable_simple(tibble(
        Contrôle = "Tous les comptes sont classés par leur numéro.")))
    datatable_simple(nc %>% transmute(
      Compte = COMPTE, Libellé = tronque_nom(LIBELLE, 60),
      `Nb périodes` = PERIODES, Total = format_CA(TOTAL, -1)))
  })

  output$cg_vie <- renderDT({
    datatable_simple(
      vie_des_comptes(DB_COMPTA) %>%
        transmute(Compte = COMPTE, Libellé = tronque_nom(LIBELLE, 50),
                  Poste = POSTE,
                  `1ʳᵉ période` = format(PREMIERE, "%m/%Y"),
                  `Dernière` = format(DERNIERE, "%m/%Y"),
                  `Nb périodes` = PERIODES, Total = format_CA(TOTAL, -1))
    )
  })

  #### Volet "Réservations" ####

  RESA <- reactive({
    if (!exists("DB_RESA")) return(resa_vide())
    prepare_resa(DB_RESA)
  })

  # --- À venir
  output$resa_kpi_prochaines <- renderUI({ kpi_prochaines_resa(RESA()) })

  output$resa_prochaines <- renderDT({
    datatable_simple(table_prochaines_resa(RESA()))
  })

  output$resa_agenda <- renderPlotly({
    a <- agenda_resa(RESA())
    if (!nrow(a))
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Aucune réservation à venir")))
    a <- creer_db_date() |> 
      rename(JOUR = JOUR_SEMAINE) |> 
      left_join(a |> select(-JOUR), by = "DATE") |> 
      mutate(
        SALLE = replace_na(SALLE,0),
        MIDI = replace_na(MIDI,0),
        TERRASSE = replace_na(TERRASSE,0),
        SOIR = replace_na(SOIR,0)
      ) |> 
      filter(DATE >= now(), DATE <= now() + days(21))
    
    test <<- a
    lab <- paste0(substr(as.character(a$JOUR), 1, 3), " ", format(a$DATE, "%d/%m"))
    ordre <- factor(lab, levels = lab)
    # Deux découpages du même total : par lieu (où installer) ou par créneau
    # (quand renforcer le service).
    par_lieu <- !identical(input$resa_agenda_par, "creneau")
    s1 <- if (par_lieu) list(v = a$SALLE, n = "Salle", c = COUL_LIEU[["SALLE"]])
          else          list(v = a$MIDI,  n = "Midi",  c = COUL_AMBRE)
    s2 <- if (par_lieu) list(v = a$TERRASSE, n = "Terrasse", c = COUL_LIEU[["TERRASSE"]])
          else          list(v = a$SOIR,     n = "Soir",     c = "#8d5b8c")
    plot_ly() %>%
      add_bars(x = ordre, y = s1$v, name = s1$n,
               marker = list(color = s1$c),
               hovertemplate = paste0(lab, "<br>", s1$n, " : ", s1$v,
                                      " couverts<extra></extra>")) %>%
      add_bars(x = ordre, y = s2$v, name = s2$n,
               marker = list(color = s2$c),
               hovertemplate = paste0(lab, "<br>", s2$n, " : ", s2$v,
                                      " couverts<extra></extra>")) %>%
      layout(barmode = "stack", xaxis = list(title = "", tickangle = -35),
             yaxis = list(title = "Couverts réservés"),
             legend = list(orientation = "h", y = -0.3), margin = list(b = 90))
  })

  # --- Statistiques
  observe({
    r <- RESA()
    req(nrow(r) > 0)
    updateDateRangeInput(session, "resa_periode",
                         start = max(r$DATE) - days(89), end = max(r$DATE))
  })

  resa_bornes <- reactive({
    r <- RESA()
    req(nrow(r) > 0)
    rng <- input$resa_periode
    if (is.null(rng) || any(is.na(rng))) c(max(r$DATE) - 89, max(r$DATE)) else rng
  })

  output$resa_kpi_stats <- renderUI({
    b <- resa_bornes(); kpi_stats_resa(RESA(), b[1], b[2])
  })
  output$resa_heures <- renderPlotly({
    b <- resa_bornes()
    graph_heures_resa(RESA(), b[1], b[2], input$resa_par %||% "lieu")
  })
  output$resa_jours <- renderPlotly({
    b <- resa_bornes()
    graph_jours_resa(RESA(), b[1], b[2], input$resa_par %||% "lieu")
  })

  # --- Historique
  resa_histo <- reactive({
    historique_resa(RESA(), input$resa_unite %||% "mois")
  })

  output$resa_historique <- renderPlotly({
    graph_historique_resa(resa_histo(), input$resa_unite %||% "mois")
  })

  output$resa_table_histo <- renderDT({
    h <- resa_histo()
    if (!nrow(h)) return(datatable_simple(tibble(Historique = "Aucune donnée.")))
    datatable_simple(
      h %>% arrange(desc(PERIODE)) %>%
        transmute(Période = if (identical(input$resa_unite, "semaine"))
                    paste("Sem.", format(PERIODE, "%d/%m/%Y"))
                  else format(PERIODE, "%B %Y"),
                  Réservations = RESA, Couverts = COUVERTS,
                  Salle = SALLE, Terrasse = TERRASSE,
                  `Part terrasse` = format_pct(PCT_TERRASSE),
                  `Taille moy.` = TAILLE_MOY,
                  `Part du soir` = format_pct(PCT_SOIR)))
  })

  # --- Réservations et CA
  resa_ca <- reactive({ resa_vs_ca(RESA(), UPD_KPI_SIMPLE()) })

  output$resa_kpi_ca   <- renderUI({ kpi_resa_ca(resa_ca()) })
  output$resa_ca_nuage <- renderPlotly({ graph_resa_ca(resa_ca()) })

  output$resa_ca_table <- renderDT({
    d <- resa_ca()
    if (!nrow(d)) return(datatable_simple(tibble(Jours = "Aucune donnée croisée.")))
    datatable_simple(
      d %>% arrange(desc(COUVERTS)) %>% head(25) %>%
        transmute(Date = format(DATE, "%a %d/%m/%Y"),
                  Réservations = RESA, Couverts = COUVERTS,
                  Midi = COUVERTS_MIDI, Soir = COUVERTS_SOIR,
                  CA = format_CA(CA, -1),
                  `CA / couvert` = format_CA(CA_PAR_COUVERT, 0)))
  })

  #### Volet "Comparaison" ####

  # Met à jour la liste des périodes disponibles selon la granularité choisie ;
  # sélectionne par défaut les 2 plus récentes.
  observe({
    req(input$comp_unite)
    dispo <- liste_periodes_dispo(UPD_KPI_SIMPLE(), input$comp_unite)
    choix <- setNames(as.character(dispo), label_periode(dispo, input$comp_unite))
    updateSelectizeInput(session, "comp_periodes", choices = choix,
                         selected = as.character(head(dispo, 2)))
  })

  comp_data <- reactive({
    req(input$comp_periodes)
    comparaison_periodes(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                         db_compta = if (exists("DB_COMPTA")) DB_COMPTA else NULL,
                         unite = input$comp_unite,
                         periodes = input$comp_periodes)
  })

  output$comp_graph <- renderPlotly({
    graph_comparaison(comp_data(), unite = input$comp_unite)
  })

  output$comp_table <- renderDT({
    datatable_simple(table_comparaison_aff(comp_data(), unite = input$comp_unite,
                                           unite_tva = input$unite_tva))
  })

  #### Volet "Année" ####

  observe({
    annees <- UPD_KPI_SIMPLE() %>%
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
    serie_annuelle(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                   if (exists("DB_COMPTA")) DB_COMPTA else NULL, annee_val())
  })

  serie_annee_m1 <- reactive({
    serie_annuelle(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                   if (exists("DB_COMPTA")) DB_COMPTA else NULL, annee_val() - 1)
  })

  output$annee_kpi <- renderUI({
    kpi_annee_tiles(serie_annee(), serie_annee_m1(), input$unite_tva)
  })

  output$annee_ecart_obj <- renderPlotly({
    graph_ecart_objectif(serie_annee())
  })

  output$annee_ecart_ym1 <- renderPlotly({
    graph_ecart_ym1(UPD_KPI_SIMPLE(), annee_val(), var = "ventes")
  })
  
  output$annee_marge <- renderPlotly({
    graph_marge_cumulee(serie_annee(), serie_annee_m1(), annee_val())
  })

  output$annee_ecart_marge <- renderPlotly({
    graph_ecart_ym1(UPD_KPI_SIMPLE(), annee_val(), var = "marge",
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
    base_travail(TICKETS_HEURES, DB_COUTS_TRAVAIL, p[1], p[2])
  })

  trav_agrege <- reactive({
    agrege_travail(trav_base(), unite = input$trav_unite)
  })

  output$trav_kpi <- renderUI({
    kpi_travail_tiles(trav_agrege())
  })

  output$trav_structure <- renderPlotly({
    graph_structure_travail(trav_agrege(), unite = input$trav_unite,
                            source = "trav_structure_graph")
  })

  output$trav_productivite <- renderPlotly({
    graph_productivite_temps(trav_agrege(), unite = input$trav_unite)
  })

  output$trav_ca_creneaux <- renderPlotly({
    graph_ca_creneaux_temps(
      agrege_creneaux_periode(trav_base(), unite = input$trav_unite),
      unite = input$trav_unite)
  })
  
  # Mois / Semaine sélectionnée (clic sur une barre, défaut = veille)
  selected_bar <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "trav_structure_graph"), {
    ev <- event_data("plotly_click", source = "trav_structure_graph")
    if (!is.null(ev$x)) selected_bar(as.Date(ev$x))
  })
  
  periode_trav <- reactive({
    req(selected_bar())
    j <- unique(selected_bar())
    if (input$trav_unite == "semaine") {
      d1 <- floor_date(j, "week")
      d2 <- ceiling_date(j, "week")
    }else{
      d1 <- floor_date(j, "month")
      d2 <- ceiling_date(j, "month")
    }
    return(c(d1,d2))
  })
  
  output$trav_heures_decomp <- renderDT({
    print(periode_trav())
    DB_COUTS_TRAVAIL |> 
      filter(DATE >= periode_trav()[1], DATE <= periode_trav()[2]) |> 
      group_by(SECTEUR,CRENEAU) |> 
      summarise(
        `Heures de travail` = round(sum(HEURES)),
        `Coût du travail (compta)` = format_CA(sum(COUT_TRAVAIL,na.rm = T),-1),
        `Coût du travail (horeko)` = format_CA(sum(COUT_TRAVAIL_HOREKO,na.rm = T),-1),
        `∑ coûts du travail (compta)` = format_CA(mean(COUT_COMPTA,na.rm = T),-1), 
        `∑ coûts du travail (horeko)` = format_CA(mean(COUT_HOREKO,na.rm = T),-1)) |> 
      datatable(rownames = FALSE, escape = FALSE,
                options = list(dom = "t"))
  })
  

  # --- Sous-onglet "Créneaux" ---
  cren_stats <- reactive({
    p <- fenetre_travail(input$cren_periode)
    stats_creneaux(base_travail(TICKETS_HEURES, DB_COUTS_TRAVAIL, p[1], p[2]))
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

  #### Volet "Boisson" — consommation ####
  
  observe({
    updateSelectInput(session,"conso_categorie",
                      choices=c("Bières","Softs","Alcools & Vins"))
  })
    
  REF_BOISSONS <- reactive({
    choix <- toupper(input$conso_categorie)
    DB_PRODUITS %>%
      filter(str_detect(toupper(replace_na(CATEGORIE, "")), choix), 
             !is.na(BOISSON), BOISSON != "") %>%
      distinct(BOISSON) %>%
      pull(BOISSON)
  })

  # Référentiel des vraies bières, calculé une seule fois
  # REF_BIERES <- ref_bieres(DB_PRODUITS)

  # Semaines proposées (la semaine en cours, partielle, est exclue)
  observe({
    sems <- semaines_dispo(DB_JOURS)
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
    conso_boissons_comparee(DB_TICKET, REF_BOISSONS(), conso_sem(), input$unite_tva)
  })

  conso_formats <- reactive({
    formats_boissons(DB_TICKET, REF_BOISSONS(), conso_sem())
  })

  conso_horaire <- reactive({
    conso_boissons_horaire(DB_TICKET, REF_BOISSONS(), conso_sem())
  })

  output$conso_kpi <- renderUI({
    kpi_boissons_tiles(conso_comp(), conso_formats(), conso_horaire(), input$unite_tva, input$conso_categorie)
  })

  output$conso_top <- renderPlotly({
    graph_top_boissons(conso_comp())
  })

  output$conso_tendance <- renderPlotly({
    graph_tendance_boissons(
      evo_top_boissons(DB_TICKET, REF_BOISSONS(), conso_sem(),
                     n_top = 5, n_semaines = 12, input$unite_tva),
      semaine = conso_sem())
  })

  output$conso_heatmap <- renderPlotly({
    graph_heatmap_boissons(conso_boissons_jour_heure(DB_TICKET, REF_BOISSONS(), conso_sem()))
  })

  output$conso_formats <- renderPlotly({
    graph_formats_boissons(conso_formats())
  })

  output$conso_evo <- renderPlotly({
    graph_evo_conso_boissons(
      evo_conso_boissons(DB_TICKET, REF_BOISSONS(), n_semaines = 26,
                       fin = conso_sem() + 6),
      semaine = conso_sem())
  })

  output$conso_table <- renderDT({
    datatable_simple(table_conso_boissons(conso_comp(), input$unite_tva))
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
    pizzwanze_soiree(DB_PRODUITS, DB_TICKET, as.Date(input$pizz_soiree), SOIREES_PIZZWANZE, input$unite_tva)
  })

  pizz_hist <- reactive({
    historique_pizzwanze(DB_PRODUITS, SOIREES_PIZZWANZE, input$unite_tva)
  })

  output$pizz_kpi <- renderUI({
    kpi_pizzwanze_tiles(pizz_data(), input$unite_tva)
  })

  output$pizz_soiree <- renderPlotly({
    graph_pizzas_soiree(pizz_data())
  })

  output$pizz_heure <- renderPlotly({
    graph_pizzas_heure(pizzas_par_heure(DB_TICKET, as.Date(input$pizz_soiree)))
  })

  output$pizz_carte <- renderPlotly({
    # n <- suppressWarnings(as.integer(input$pizz_profondeur))
    graph_carte_pizzwanze(DB_PRODUITS, SOIREES_PIZZWANZE, n_soirees = 12, unite_tva = input$unite_tva)
  })

  output$pizz_evo <- renderPlotly({
    graph_evo_pizzwanze(pizz_hist(), soiree = as.Date(input$pizz_soiree))
  })

  output$pizz_table <- renderDT({
    datatable_simple(table_pizzwanze(pizz_data(), input$unite_tva))
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
    focaccias_semaine(DB_PRODUITS, foca_sem(), input$unite_tva)
  })

  foca_evo <- reactive({
    evo_focaccias(DB_PRODUITS, n_semaines = 26, fin = foca_sem() + 6, input$unite_tva)
  })

  output$foca_kpi <- renderUI({
    kpi_focaccias_tiles(foca_data(), input$unite_tva)
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
    datatable_simple(table_focaccias(foca_data(), input$unite_tva))
  })

  #### Volet "Focaccias" — carte Production ####
  # Le préremplissage vient des dernières semaines COMPLÈTES des données, et
  # non de la semaine sélectionnée dans la barre latérale : on prépare la
  # production à venir, pas celle d'une semaine consultée dans l'historique.

  prod_base <- reactive({
    # Le curseur de marge vit dans l'onglet Focaccias, qui n'est inséré qu'après
    # la connexion : avant, input$prod_multi est NULL, `1 + NULL/100` vaut
    # numeric(0), et le case_when() de production_focaccias_base() refuse de
    # recycler une condition de 5 lignes contre une valeur de longueur 0.
    # L'observateur de préremplissage, lui, tourne dès le premier flush.
    req(input$prod_multi)
    production_focaccias_base(DB_PRODUITS, n_semaines = 3,
                              marge = 1 + input$prod_multi / 100,
                              unite_tva = input$unite_tva)
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
  
  # La navigation des cartes d'accueil est branchée plus haut, à partir de
  # CARTES_ACCUEIL : le second jeu d'observateurs qui vivait ici faisait
  # double emploi.

}
