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

  # Vrai une fois que les onglets autorisés sont VRAIMENT dans la page.
  #
  # Les sélecteurs d'un onglet — périodes, années, semaines, brassins — sont
  # garnis depuis le serveur par des observateurs qui ne dépendent que des
  # données. Tant que l'onglet n'est pas inséré, leur update*Input() vise un
  # champ inexistant : le message est perdu sans erreur, et l'observateur, qui
  # n'a plus aucune raison d'être invalidé, ne rejoue jamais. Le sélecteur
  # reste vide pour de bon. Ils attendent donc ce drapeau.
  ONGLETS_PRETS <- reactiveVal(FALSE)

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

    # Le drapeau ne passe pas à TRUE ici, mais au flush SUIVANT. Côté
    # navigateur, Shiny traite ses messages dans l'ordre où leurs gestionnaires
    # sont enregistrés, et « inputMessages » (3e) passe avant
    # « shiny-insert-tab » (19e) : garnir dans le même flush que l'insertion
    # remplirait des champs pas encore créés. onFlushed() décale d'un cycle,
    # donc les onglets existent quand les valeurs arrivent.
    session$onFlushed(function() ONGLETS_PRETS(TRUE), once = TRUE)

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

  # La grille des cartes : seulement celles dont l'onglet est autorisé. Une
  # carte qui renverrait vers un onglet absent de la barre n'a pas de sens.
  output$accueil_cartes <- renderUI({
    req(USER$logged)
    grille_cartes_accueil(USER$onglets)
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
  output$acc_planning <- renderUI({
    if (is.null(PLANNING_BRUT())) return(corps_vide("Pas encore de planning."))
    acc_planning(PLAN_RESUME())
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
    req(ONGLETS_PRETS())
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

  #### Volet "Détails" — Périodes ####
  #
  # Une seule chaîne pour les trois mailles : la maille est une VARIABLE, pas
  # trois copies du même code. L'ancienne version avait un bloc par maille et
  # une fonction generatrice pour deux d'entre elles ; les trois divergeaient
  # déjà sur des détails.

  det_maille <- reactive(input$det_maille %||% "jour")

  # La fenêtre par défaut suit la maille : 8 semaines de jours, 6 mois de
  # semaines, 2 ans de mois. Elle est semée à chaque changement de maille, et
  # derrière ONGLETS_PRETS() comme tout sélecteur d'onglet (cf. R/acces.R).
  observe({
    req(ONGLETS_PRETS())
    m <- maille_detail(det_maille())
    updateDateRangeInput(session, "det_fenetre",
                         start = date_veille - days(m$FENETRE),
                         end   = date_veille)
  })

  det_fenetre <- reactive({
    rng <- input$det_fenetre
    m <- maille_detail(det_maille())
    if (is.null(rng) || any(is.na(rng)))
      c(date_veille - days(m$FENETRE), date_veille) else rng
  })

  det_serie <- reactive({
    serie_detail(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                 det_fenetre()[1], det_fenetre()[2], det_maille())
  })

  output$det_titre_serie <- renderText({
    m <- maille_detail(det_maille())
    paste0("Vue d'ensemble — ", tolower(m$LIBELLE))
  })

  output$det_serie <- renderPlotly({
    graph_serie_detail(det_serie(), det_maille(), source = "det_serie")
  })

  # La période choisie au clic. Le clic rend `customdata` (la date en texte) et
  # non `x`, qui ne porte qu'un libellé d'axe : as.Date("ven 11/09") lève.
  det_choisie <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click", source = "det_serie"), {
    ev <- event_data("plotly_click", source = "det_serie")
    d <- suppressWarnings(as.Date(ev$customdata %||% NA))
    if (length(d) == 1 && !is.na(d)) det_choisie(d)
  })

  # Changer de maille périme la sélection : un début de semaine n'est pas un
  # début de mois. On la vide plutôt que de la traîner.
  observeEvent(det_maille(), { det_choisie(NULL) })

  det_periode <- reactive({
    s <- det_serie()
    c <- det_choisie()
    # Défaut : la dernière période de la fenêtre, celle qu'on vient regarder.
    if (is.null(c) || is.null(s) || !(c %in% s$PERIODE)) {
      if (is.null(s) || !nrow(s)) debut_maille(date_veille, det_maille())
      else max(s$PERIODE)
    } else c
  })

  det_resume <- reactive({
    resume_detail(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                  if (exists("DB_TICKET")) DB_TICKET else NULL,
                  det_periode(), det_maille())
  })

  output$det_titre_periode <- renderText({
    libelle_maille(det_periode(), det_maille())
  })

  output$det_bandeau <- renderUI({ bandeau_ailleurs(det_maille()) })
  output$det_kpi <- renderUI({ kpi_detail(det_resume()) })

  output$det_titre_compo <- renderText({
    maille_detail(det_maille())$COMPO_TITRE
  })

  output$det_composition <- renderPlotly({
    graph_composition_detail(
      composition_detail(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                         if (exists("DB_TICKET")) DB_TICKET else NULL,
                         det_periode(), det_maille(), input$unite_tva),
      det_maille())
  })

  # Les heures d'une journée n'ont pas d'objectif : la légende de couleur n'y
  # voudrait rien dire, et se lirait comme un jugement absent du graphe.
  output$det_legende_compo <- renderUI({
    if (identical(maille_detail(det_maille())$COMPOSITION, "heure")) NULL
    else legende_objectif()
  })

  output$det_repartition <- renderPlotly({
    graph_repartition_detail(
      repartition_detail(UPD_KPI_SIMPLE(), det_periode(), det_maille()))
  })

  output$det_produits <- renderDT({
    datatable_simple(table_produits_detail(
      produits_detail(DB_PRODUITS, det_periode(), det_maille(),
                      n = 25, unite_tva = input$unite_tva),
      input$unite_tva))
  })

  # --- Les tickets
  det_tickets <- reactive({
    b <- list(d1 = debut_maille(det_periode(), det_maille()),
              d2 = fin_maille(det_periode(), det_maille()))
    tickets_detail(if (exists("DB_TICKET")) DB_TICKET else NULL,
                   b$d1, b$d2, input$unite_tva)
  })

  output$det_tickets_alerte <- renderUI({
    alerte_tickets(if (exists("DB_TICKET")) DB_TICKET else NULL,
                   debut_maille(det_periode(), det_maille()),
                   fin_maille(det_periode(), det_maille()))
  })

  output$det_tickets <- renderDT({
    tk <- det_tickets()
    tbl <- table_tickets(tk, input$unite_tva)
    if ("Info" %in% names(tbl)) return(datatable_simple(tbl))
    datatable(tbl, selection = "single", rownames = FALSE,
              options = list(pageLength = 12, dom = "ftp", scrollX = TRUE,
                             columnDefs = defs_tri(tbl),
                             language = list(search = "Filtrer :")))
  })

  output$det_paniers <- renderPlotly({ graph_paniers(det_tickets()) })
  output$det_resume_paniers <- renderText({ resume_paniers(det_tickets()) })

  det_ticket_choisi <- reactive({
    tk <- det_tickets()
    if (is.null(tk) || !nrow(tk)) return(NULL)
    i <- input$det_tickets_rows_selected
    if (is.null(i)) NULL else tk$ID_TICKET[i]
  })

  output$det_titre_ticket <- renderText({
    id <- det_ticket_choisi()
    if (is.null(id)) "Détail d'un ticket" else paste0("Ticket ", id)
  })

  output$det_ticket_lignes <- renderDT({
    datatable_simple(lignes_ticket(if (exists("DB_TICKET")) DB_TICKET else NULL,
                                   det_ticket_choisi(), input$unite_tva))
  })

  output$det_heures <- renderDT({
    r <- det_resume()
    datatable_simple(table_heures_periode(
      DB_COUTS_TRAVAIL, if (exists("DB_ONSS")) DB_ONSS else NULL,
      det_periode(), det_maille(),
      ca = if (is.null(r)) NA_real_ else r$CA))
  })

  # La note change avec la maille : au mois les heures sont mesurées, ailleurs
  # elles sont pointées. Écrire les deux phrases en dur ferait lire la mauvaise.
  output$det_heures_note <- renderUI({
    if (identical(det_maille(), "mois"))
      div(class = "small text-muted mt-1",
          "Au mois, les heures sont ", tags$b("mesurées"), " : les heures",
          " payées et le coût employeur viennent du fichier de paie, le coût",
          " comptable de la comptabilité. Le pointage Horeko reste affiché à",
          " côté — c'est lui qui ventile les journées, mais il ignore les",
          " heures payées non pointées.",
          tags$br(),
          "Ces trois sources se comparent dans ", tags$b("Travail › Sources"),
          ".")
    else
      div(class = "small text-muted mt-1",
          "Les heures ", tags$b("pointées"), " dans Horeko, pas les heures",
          " recalées sur la paie : à cette maille, la valeur recalée est une",
          " répartition d'un total mensuel, pas une mesure du jour.",
          tags$br(),
          "Les heures payées et leur coût réel ne se connaissent qu'au ",
          tags$b("mois"), " : choisissez cette maille pour les voir.")
  })

  #### Volet "Détail" — Par produit ####
  
  observe({
    req(ONGLETS_PRETS())
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
                !!sym(col_name) := format_CA(CA, -1),
                # Sans sa jumelle, la colonne de CA se trie dans l'ordre du
                # dictionnaire (cf. defs_tri dans R/theme.R).
                !!sym(col_tri(col_name)) := CA)
    datatable(df, selection = "single", rownames = FALSE,
              options = list(pageLength = 12, dom = 'ftp', 
                             columnDefs = defs_tri(df),
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
      # arrange() AVANT le formatage : trié sur « 31/01/2026 », l'ordre est
      # celui du dictionnaire, donc celui des jours du mois.
      arrange(desc(SEMAINE)) %>%
      transmute(Semaine = format(SEMAINE, "%d/%m/%Y"),
                Quantité = Quantite,
                !!sym(col_name) := format_CA(CA, -1),
                `Part dans Total` = paste0(round(PC_ALL*100,0),"%"),
                !!sym(category_column) := paste0(round(PC_CATEGORIE*100,0),"%"),
                !!sym(col_tri("Semaine")) := as.numeric(SEMAINE),
                !!sym(col_tri(col_name)) := CA,
                !!sym(col_tri("Part dans Total")) := PC_ALL,
                !!sym(col_tri(category_column)) := PC_CATEGORIE)

    datatable(df, selection = "none", rownames = FALSE,
              options = list(pageLength = 12, dom = 'tp',
                             columnDefs = defs_tri(df)))
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
    req(ONGLETS_PRETS())
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
    req(ONGLETS_PRETS())
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
  # Le drapeau est dans l'expression déclenchante, pas dans le corps :
  # observeEvent() isole son corps, donc une dépendance posée là ne rejouerait
  # jamais l'observateur au moment où le drapeau bascule — et sim_categorie
  # resterait vide. C'est le seul observeEvent parmi les garnissages.
  observeEvent({ req(ONGLETS_PRETS()); sim_base() }, {
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
  #### Volet "Compta / Gestion" — socle commun ####

  # Ce que TOUS les onglets du volet lisent : la compta brute, les mois
  # écartés, et la compta retenue qui en découle. Posé avant eux, parce que
  # chacun en dépend et qu'aucun ne doit le recalculer de son côté.
  #
  # Tout vient de DB_COMPTA. Aucune donnee simulee : si les chiffres ne sont pas
  # la, le volet reste vide.
  # La compta BRUTE, jamais filtrée. C'est elle qui sert à repérer les mois
  # douteux : la filtrer ici ferait disparaître de la liste le mois qu'on vient
  # d'écarter, donc sa case, donc l'écartement — une boucle.
  compta_brute <- reactive({
    req(exists("DB_COMPTA"))
    DB_COMPTA
  })

  compta_improbable <- reactive({
    periodes_improbables(postes_exploitation(compta_brute()))
  })

  # Le topo, calculé sur la compta BRUTE. Il ne dépend PAS de l'écartement, et
  # c'est volontaire : s'il en dépendait, le mois qu'on vient d'écarter
  # sortirait du tableau, donc de la sélection, donc de l'écartement — et la
  # table se redessinerait à chaque clic en perdant le clic.
  compta_topo <- reactive({ topo_mois(compta_brute(), compta_improbable()) })

  output$topo_table <- renderDT({
    tbl <- table_topo(compta_topo())
    # escape = FALSE pour la pastille d'état. Tout le contenu de ce tableau est
    # fabriqué ici — aucun libellé de compte, donc aucun texte venu d'un
    # export ne traverse sans échappement.
    # Deux familles de colonnes masquées, et il faut penser aux deux : les
    # jumelles « .tri: », que defs_tri() connaît, et le drapeau « .f_ », qui
    # ne sert qu'à colorer la cellule voisine. L'oublier le laisse s'afficher
    # en bout de ligne — un « -1 » que personne ne sait lire.
    drapeaux <- which(startsWith(names(tbl), ".f_")) - 1L
    chiffres <- match(COLONNES_CHIFFREES_TOPO, names(tbl)) - 1L
    defs <- c(defs_tri(tbl),
              list(list(targets = as.list(drapeaux), visible = FALSE,
                        searchable = FALSE, orderable = FALSE),
                   list(targets = as.list(chiffres[!is.na(chiffres)]),
                        className = "dt-right")))
    dt <- datatable(
      tbl, rownames = FALSE, escape = FALSE,
      selection = list(mode = "multiple", target = "row"),
      options = list(dom = "t", paging = FALSE, searching = FALSE,
                     scrollX = TRUE, scrollCollapse = TRUE,
                     order = list(list(0, "desc")),
                     columnDefs = defs))
    # Le nombre de comptes mouvementés : signal d'incomplétude indépendant des
    # montants (cf. topo_mois).
    formatStyle(dt, "Comptes", valueColumns = ".f_Comptes",
                backgroundColor = styleEqual(-1L, "rgba(192,57,43,0.18)"),
                fontWeight = styleEqual(-1L, "600"))
  })

  # SÉLECTIONNER ÉCARTE. Le sens compte : une ligne non sélectionnée est une
  # ligne retenue, donc l'état de départ — aucune sélection — veut dire « rien
  # d'écarté », qui est le bon défaut. Dans l'autre sens, l'écran s'ouvrirait
  # sur une analyse déjà amputée.
  compta_exclus <- reactive({
    s <- input$topo_table_rows_selected
    if (!length(s)) as.Date(character(0)) else compta_topo()$PERIODE[s]
  })

  # Les deux raccourcis. Ils ne décident rien que le clic ne puisse faire : ils
  # évitent douze clics, et c'est tout ce qu'on leur demande.
  observeEvent(input$topo_ecarter_risque, {
    DT::selectRows(DT::dataTableProxy("topo_table"),
                   which(compta_topo()$A_ECARTER))
  })
  observeEvent(input$topo_tout_garder, {
    DT::selectRows(DT::dataTableProxy("topo_table"), integer(0))
  })

  output$topo_resume <- renderUI({
    resume_topo(compta_topo(), compta_exclus())
  })
  # La compta RETENUE : celle que lisent les six onglets.
  compta <- reactive({ ecarte_mois(compta_brute(), compta_exclus()) })

  # La même, mais qui rend NULL au lieu de s'interrompre quand DB_COMPTA
  # manque. Le volet Année sait afficher son CA sans comptabilité — il perd
  # juste la marge — là où les onglets de Compta n'ont rien à montrer sans
  # elle. Le `if` évite d'appeler compta(), donc le req() qu'elle porte.
  compta_si_dispo <- reactive({
    if (exists("DB_COMPTA")) compta() else NULL
  })

  expl_postes <- reactive({ postes_exploitation(compta()) })

  #### Volet "Compta / Gestion" — Contrôle ####

  # Les alertes portent sur TOUT l'historique, pas sur le dernier mois. Un
  # onglet qui demande « puis-je me fier à ces chiffres » doit répondre pour
  # toute la série : une écriture mal classée l'an dernier fausse encore les
  # comparaisons d'aujourd'hui, et personne n'ira la chercher mois par mois.
  ctrl_alertes <- reactive({
    req(exists("DB_COMPTA"))
    alertes_integrite(compta(), postes_exploitation(compta()),
                      compta_improbable())
  })

  output$ctrl_diagnostic  <- renderUI({ rendu_diagnostic(ctrl_alertes()) })
  output$ctrl_diag_resume <- renderText({ resume_diagnostic(ctrl_alertes()) })
  output$ctrl_badge       <- renderUI({ badge_alertes(ctrl_alertes()) })

  # Compta BRUTE, à dessein : ce tableau répond à « quels comptes le plan
  # ignore-t-il ». L'amputer des mois écartés masquerait le compte cherché.
  output$ctrl_comptes <- renderDT({
    nc <- comptes_non_classes(compta_brute())
    if (!nrow(nc))
      return(datatable_simple(tibble(
        Contrôle = "Tous les comptes sont classés par leur numéro.")))
    tbl <- nc %>% transmute(
      Compte = COMPTE, Libellé = tronque_nom(LIBELLE, 60),
      Total = euro_compta(TOTAL),
      ` ` = lien_forage_html("Écritures", forages_texte(COMPTE)))
    datatable(tbl, rownames = FALSE, escape = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE,
                             searching = FALSE))
  })

  # Les contrôles de lecture des écritures. Ils étaient calculés à l'import,
  # journalisés en console, sauvés dans le .RData — et affichés nulle part.
  output$ctrl_import <- renderUI({
    tables <- list(
      if (exists("CONTROLE_ODOO")) CONTROLE_ODOO else NULL,
      if (exists("CONTROLE_COUTURE")) CONTROLE_COUTURE else NULL)
    d <- bind_rows(Filter(Negate(is.null), tables))
    if (is.null(d) || !nrow(d))
      return(div(class = "text-muted small p-2",
                 "Aucun contrôle d'import : la comptabilité ne vient pas",
                 " d'un export Odoo."))
    # Même rendu que les alertes : une liste à lire, alertes en tête — et la
    # destination de chacune se DÉDUIT de son nom plutôt que d'être stockée
    # dans la table. Les contrôles sont calculés à l'import et sauvés dans le
    # .RData : y ajouter une colonne casserait la lecture des caches déjà
    # écrits, pour une information qui se retrouve ici sans effort.
    rendu_diagnostic(d %>% transmute(TITRE = CONTROLE, STATUT = VERDICT,
                                     MESSAGE = DETAIL,
                                     FORAGE = forage_import(CONTROLE, DETAIL)))
  })

  # Le forage : une consigne posée par un lien, un seul observateur pour les
  # appliquer toutes.
  observeEvent(input[[INPUT_FORAGE]], {
    f <- analyse_forage(input[[INPUT_FORAGE]])
    if (is.null(f)) return()
    updateTabsetPanel(session, "compta_tabs", selected = "ecritures")
    if (!is.null(f$bloc))  updateRadioButtons(session, "ecr_bloc", selected = f$bloc)
    updateTextInput(session, "ecr_texte", value = f$texte %||% "")
    if (!is.null(f$du) && !is.null(f$au))
      updateDateRangeInput(session, "ecr_dates",
                           start = as.Date(f$du), end = as.Date(f$au))
    # Les filtres de poste et de journal ne sont jamais posés par un forage :
    # les laisser tels quels ferait arriver sur une sélection vide sans qu'on
    # comprenne pourquoi.
    updateSelectizeInput(session, "ecr_poste", selected = "")
    updateSelectizeInput(session, "ecr_journal", selected = "")
  })

  #### Volet "Compta / Gestion" — Exploitation ####

  # La granularite pilote tout l'onglet : la serie complete est agregee une
  # fois, et la cascade comme le tableau y puisent.
  expl_unite <- reactive(input$expl_unite %||% "mois")

  expl_serie_complete <- reactive({
    p <- agrege_exploitation(expl_postes(), expl_unite())
    req(nrow(p) > 0)
    p
  })

  expl_serie <- reactive({
    tail(expl_serie_complete(), as.integer(input$expl_nb %||% 12))
  })

  observe({
    req(ONGLETS_PRETS())
    p <- expl_serie_complete()
    dispo <- sort(unique(p$PERIODE), decreasing = TRUE)
    # Les libelles suivent la granularite : « T3 2026 » pour un trimestre,
    # « 2026 » pour une annee.
    updateSelectInput(session, "expl_periode",
                      choices = setNames(as.character(dispo),
                                         etiquette_periode(dispo, expl_unite())),
                      selected = as.character(dispo[1]))
  })

  # La cascade porte sur la periode choisie ; les autres vues sur la serie.
  expl_une <- reactive({
    p <- expl_serie_complete()
    choisie <- suppressWarnings(as.Date(input$expl_periode %||% NA))
    # Changer de granularite laisse un instant l'ancienne valeur dans le select
    # — un debut de mois qui n'est pas un debut de trimestre. Plutot que de
    # rendre une cascade vide, on retombe sur la periode la plus recente.
    d <- if (length(choisie) != 1 || is.na(choisie) || !choisie %in% p$PERIODE)
      max(p$PERIODE) else choisie
    filter(p, PERIODE == d)
  })

  output$expl_kpi <- renderUI({ kpi_exploitation(expl_une(), expl_unite()) })
  output$expl_cascade <- renderPlotly({
    graph_cascade_exploitation(expl_une(), expl_unite()) })
  output$expl_structure <- renderPlotly({
    graph_structure_exploitation(expl_serie(), expl_unite()) })
  # La période choisie est-elle close et complète ? Un trimestre à un mois sur
  # trois se lit sinon comme un effondrement.
  output$expl_alerte_periode <- renderUI({
    alerte_periode(etat_periode(expl_une(), expl_unite()))
  })

  output$expl_table <- renderDT({
    tbl <- table_exploitation(expl_serie(), expl_unite())
    # Les colonnes .f_* portent le repérage des écarts (cf. R/exploitation.R) :
    # masquées, elles ne servent qu'à colorer la cellule voisine.
    caches <- which(startsWith(names(tbl), ".f_")) - 1L
    dt <- datatable(
      tbl, rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     searching = FALSE, scrollX = TRUE,
                     columnDefs = list(list(visible = FALSE, targets = caches))))
    for (i in seq_len(nrow(COLONNES_SURVEILLEES))) {
      dt <- formatStyle(
        dt, COLONNES_SURVEILLEES$COLONNE[i],
        valueColumns = COLONNES_SURVEILLEES$DRAPEAU[i],
        backgroundColor = styleEqual(c(-1L, 1L),
                                     c("rgba(192,57,43,0.18)",
                                       "rgba(91,123,90,0.18)")),
        fontWeight = styleEqual(c(-1L, 1L), c("600", "600")))
    }
    dt
  })
  output$expl_vraisemblance <- renderUI({ bandeau_mois_ecartes(compta_exclus()) })


  #### Volet "Compta / Gestion" — Écarts ####

  # Même socle que le volet Exploitation : postes_exploitation() puis
  # agrege_exploitation() à la granularité choisie. Rien n'est recalculé ici,
  # sinon les deux sous-onglets finiraient par afficher deux marges.
  eca_unite <- reactive(input$eca_unite %||% "mois")

  # Deux séries, et la distinction porte tout le rabotage.
  #
  # La série COMPLÈTE sert au sélecteur de période : toutes les périodes
  # doivent rester choisissables, y compris celles qu'un rabotage viderait.
  eca_serie_complete <- reactive({
    p <- agrege_exploitation(expl_postes(), eca_unite())
    req(nrow(p) > 0)
    p
  })

  # Les rangs de mois présents dans la période analysée — le gabarit auquel on
  # ramène les autres périodes (cf. R/analyse.R).
  eca_offsets <- reactive({
    if (!isTRUE(input$eca_raboter %||% TRUE))
      return(seq_len(mois_attendus(eca_unite())) - 1L)
    offsets_periode(expl_postes(), eca_periode(), eca_unite())
  })

  # La série COMPARABLE : chaque période ramenée à ces mêmes rangs. C'est elle
  # qui alimente tout ce qui compare — référence, pont, tuiles, tableau — et
  # aussi la tendance : un graphe où 2026 vaut huit mois et 2025 douze ferait
  # exactement l'erreur que la case à cocher existe pour éviter.
  #
  # La période analysée y est identique à celle de la série complète : tous ses
  # mois sont dans le gabarit, par construction.
  eca_serie_comparable <- reactive({
    p <- serie_rabotee(expl_postes(), eca_unite(), eca_offsets())
    req(nrow(p) > 0)
    p
  })

  # Rabote-t-on réellement quelque chose ? Faux sur une période complète, et
  # c'est ce qui permet de n'annoncer le rabotage que lorsqu'il a lieu.
  eca_rabotage <- reactive({
    etiquette_rabotage(eca_offsets(), eca_unite())
  })

  # Le sélecteur de période. Comme partout, il n'est semé qu'une fois les
  # onglets insérés : écrire dans un input qui n'existe pas encore est perdu
  # en silence, et l'observateur ne rejoue pas (cf. R/acces.R).
  observe({
    req(ONGLETS_PRETS())
    dispo <- sort(unique(eca_serie_complete()$PERIODE), decreasing = TRUE)
    updateSelectInput(session, "eca_periode",
                      choices = setNames(as.character(dispo),
                                         etiquette_periode(dispo, eca_unite())),
                      selected = as.character(dispo[1]))
  })

  eca_periode <- reactive({
    p <- eca_serie_complete()
    choisie <- suppressWarnings(as.Date(input$eca_periode %||% NA))
    # Changer de granularité laisse un instant l'ancienne valeur dans le select
    # — un début de mois qui n'est pas un début de trimestre. On retombe alors
    # sur la période la plus récente plutôt que de rendre un volet vide.
    if (length(choisie) != 1 || is.na(choisie) || !choisie %in% p$PERIODE)
      max(p$PERIODE) else choisie
  })

  eca_actuel <- reactive({
    filter(eca_serie_comparable(), PERIODE == eca_periode())
  })

  eca_reference <- reactive({
    reference_analyse(eca_serie_comparable(), eca_periode(),
                      mode = input$eca_ref %||% "precedente",
                      unite = eca_unite())
  })

  # Le libellé dit le rabotage quand il a lieu : « 2025 » et « 2025 (janv.–août) »
  # ne désignent pas les mêmes chiffres, et l'en-tête du tableau de comparaison
  # comme le titre du graphe s'appuient dessus.
  eca_lib_ref <- reactive({
    r <- eca_reference()
    if (is.null(r)) return("aucune référence")
    rab <- eca_rabotage()
    if (is.null(rab)) r$libelle else paste0(r$libelle, " (", rab, ")")
  })

  # Les périodes dont la référence est faite : une seule pour les modes
  # « précédente » et « an dernier », plusieurs pour la médiane. Le forage par
  # compte en a besoin — il ne peut pas travailler sur une ligne médiane, qui
  # n'est la comptabilité d'aucune période réelle.
  eca_ref_periodes <- reactive({
    r <- eca_reference()
    if (is.null(r)) return(as.Date(character()))
    if (identical(r$mode, "habituelle")) {
      serie <- eca_serie_comparable() %>% filter(PERIODE < eca_periode())
      return(tail(sort(serie$PERIODE), 6))
    }
    r$ligne$PERIODE
  })

  output$eca_vraisemblance <- renderUI({ bandeau_mois_ecartes(compta_exclus()) })

  output$eca_alerte <- renderUI({
    rab <- eca_rabotage()
    tagList(
      # Sur une période partielle, l'alerte standard dit « les totaux ne sont
      # pas comparables ». Quand le rabotage est actif, ils le redeviennent :
      # on remplace donc l'avertissement par ce qui a été fait, sans quoi on
      # lirait une mise en garde contre un problème déjà résolu.
      if (is.null(rab)) alerte_periode(etat_periode(eca_actuel(), eca_unite()))
      else bandeau_alerte(
        TRUE,
        paste0(etiquette_periode(eca_periode(), eca_unite()), " ne porte que ",
               length(eca_offsets()), " mois. Toutes les périodes comparées ",
               "sont donc ramenées aux mêmes mois (", rab, ") : les écarts ",
               "affichés sont de vrais écarts, pas des différences de durée. ",
               "Décochez « raboter la référence » pour retrouver les périodes ",
               "entières."),
        titre = "Périodes ramenées à la même taille", couleur = COUL_AMBRE,
        icone = "scissors"),
      if (is.null(eca_reference()))
        bandeau_alerte(
          TRUE,
          paste0("Aucune période de référence n'existe pour ce mode de ",
                 "comparaison. Le volet affiche alors les niveaux, sans écart. ",
                 "Choisissez « période précédente » ou reculez la période ",
                 "analysée."),
          titre = "Pas de référence", couleur = COUL_NEUTRE,
          icone = "circle-info"))
  })

  output$eca_kpi <- renderUI({
    r <- eca_reference()
    kpi_analyse(eca_actuel(), if (is.null(r)) NULL else r$ligne,
                eca_lib_ref(), eca_unite())
  })

  eca_pont <- reactive({
    r <- eca_reference()
    if (is.null(r)) return(NULL)
    pont_marge(eca_actuel(), r$ligne)
  })

  # Le libellé de la période analysée sert au graphe, au tableau de
  # décomposition ET aux phrases de lecture : un seul reactive, pour que les
  # trois nomment la même période de la même façon.
  eca_lib_periode <- reactive(etiquette_periode(eca_periode(), eca_unite()))

  output$eca_pont <- renderPlotly({
    graph_pont_marge(eca_pont(), eca_lib_periode(), eca_lib_ref(), eca_unite(),
                     sous_titre = if (is.null(eca_rabotage())) NULL
                                  else "périodes ramenées aux mêmes mois")
  })

  output$eca_pont_explication <- renderUI({
    explication_pont(eca_pont(), eca_lib_periode(), eca_lib_ref())
  })

  output$eca_pont_table <- renderDT({
    datatable_simple(table_pont_marge(eca_pont(), eca_lib_periode(),
                                      eca_lib_ref()))
  })

  eca_contrib <- reactive({
    req(exists("DB_COMPTA"))
    # Le même gabarit que la série : sans lui, les contributions ne sommeraient
    # plus à l'écart affiché juste au-dessus.
    contributions_comptes(compta(), eca_periode(), eca_ref_periodes(),
                          eca_unite(), eca_offsets())
  })

  output$eca_contrib <- renderPlotly({ graph_contributions(eca_contrib()) })
  output$eca_contrib_table <- renderDT({
    datatable_simple(table_contributions(eca_contrib()))
  })

  output$eca_table <- renderDT({
    r <- eca_reference()
    tbl <- table_comparaison(eca_actuel(), if (is.null(r)) NULL else r$ligne,
                             etiquette_periode(eca_periode(), eca_unite()),
                             eca_lib_ref())
    if (!".sens" %in% names(tbl)) return(datatable_simple(tbl))
    # .sens colore l'écart sans être affichée : même mécanique que les
    # colonnes .f_* du tableau d'exploitation.
    cache <- which(names(tbl) == ".sens") - 1L
    datatable(
      tbl, rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     searching = FALSE, scrollX = TRUE,
                     columnDefs = list(list(visible = FALSE, targets = cache)))) %>%
      formatStyle("Écart", valueColumns = ".sens",
                  color = styleEqual(c(-1L, 1L), c("#c0392b", "#5B7B5A")),
                  fontWeight = styleEqual(c(-1L, 1L), c("600", "600")))
  })


  #### Volet "Compta / Gestion" — Évolution ####

  # Pas de sélecteur de période : une tendance se lit jusqu'à aujourd'hui.
  # Le gabarit de rabotage est donc celui de la DERNIÈRE période — sans quoi,
  # au grain annuel, une année en cours se comparerait à des années entières
  # et la courbe plongerait pour une raison de calendrier.
  evo_unite <- reactive(input$evo_unite %||% "mois")

  evo_offsets <- reactive({
    if (!isTRUE(input$evo_raboter %||% TRUE))
      return(seq_len(mois_attendus(evo_unite())) - 1L)
    p <- agrege_exploitation(expl_postes(), evo_unite())
    req(nrow(p) > 0)
    offsets_periode(expl_postes(), max(p$PERIODE), evo_unite())
  })

  evo_serie <- reactive({
    p <- serie_rabotee(expl_postes(), evo_unite(), evo_offsets())
    req(nrow(p) > 0)
    tail(p, as.integer(input$evo_nb %||% 18))
  })

  output$evo_vraisemblance <- renderUI({ bandeau_mois_ecartes(compta_exclus()) })

  output$evo_tendance <- renderPlotly({
    graph_tendance(serie_indicateur(evo_serie(), input$evo_indic),
                   input$evo_indic, evo_unite())
  })

  # La saisonnalité travaille toujours au grain MENSUEL, quelle que soit la
  # granularité choisie : superposer des années agrégées à l'année ne
  # donnerait qu'un point par courbe.
  output$evo_saison <- renderPlotly({
    graph_saisonnalite(saisonnalite(expl_postes(), input$evo_indic),
                       input$evo_indic)
  })

  #### Volet "Compta / Gestion" — Comptabilité générale ####

  # Plus de reconstruction de plan : les comptes sont classés sur leur numéro
  # (cf. R/plan_comptable.R), structure du PCMN qui ne bouge pas.

  # La granularite pilote l'onglet entier : les comptes sont consolides au
  # niveau choisi AVANT tout regroupement (cf. consolide_periode()).
  cg_unite <- reactive(input$cg_unite %||% "mois")

  cg_dispo <- reactive({
    req(exists("DB_COMPTA"))
    periodes_compta(compta(), cg_unite())
  })

  observe({
    req(ONGLETS_PRETS())
    p <- cg_dispo()
    req(nrow(p) > 0)
    updateSelectizeInput(session, "cg_periodes",
                         choices = setNames(as.character(p$PERIODE), p$LIBELLE),
                         selected = as.character(head(p$PERIODE, 3)))
  })

  cg_periodes <- reactive({
    req(input$cg_periodes)
    # Changer de granularite laisse un instant des valeurs perimees dans le
    # selecteur : on ne garde que celles qui existent au niveau courant.
    valides <- intersect(as.Date(input$cg_periodes), cg_dispo()$PERIODE)
    req(length(valides) > 0)
    sort(as.Date(valides, origin = "1970-01-01"))
  })

  output$cg_titre <- renderText({
    n <- length(cg_periodes())
    mot <- switch(cg_unite(), mois = "mois", trimestre = "trimestre",
                  annee = "année")
    pluriel <- if (n > 1 && mot != "mois") paste0(mot, "s") else mot
    paste0("Compte de résultat — ", n, " ", pluriel,
           if (n > 1) " comparés" else "")
  })

  output$cg_vraisemblance <- renderUI({ bandeau_mois_ecartes(compta_exclus()) })

  output$cg_kpi <- renderUI({
    kpi_compta_generale(compta(), cg_periodes(), cg_unite()) })

  cg_table_data <- reactive({
    table_compte_resultat(compta(), cg_periodes(),
                          detail = isTRUE(input$cg_detail),
                          en_pct = isTRUE(input$cg_pct),
                          unite = cg_unite())
  })

  output$cg_table <- renderDT({
    tbl <- cg_table_data()
    datatable(tbl, rownames = FALSE, escape = FALSE, selection = "none",
              options = list(pageLength = 200, dom = "ft", scrollX = TRUE,
                             ordering = FALSE,
                             columnDefs = list(list(className = "dt-right",
                                                    targets = 2:(ncol(tbl) - 1))),
                             language = list(search = "Filtrer :"))) %>%
      formatStyle("Compte", target = "row", fontWeight = styleEqual("", "bold"))
  })

  output$cg_soldes <- renderPlotly({
    graph_soldes(compta(), cg_periodes(), cg_unite()) })

  # Compta BRUTE : « quand ce compte a-t-il vécu » perdrait son sens sur une
  # série amputée des mois écartés.
  output$cg_vie <- renderDT({
    datatable_simple(
      vie_des_comptes(compta_brute()) %>%
        transmute(Compte = COMPTE, Libellé = tronque_nom(LIBELLE, 50),
                  Poste = POSTE,
                  `1ʳᵉ période` = format(PREMIERE, "%m/%Y"),
                  `Dernière` = format(DERNIERE, "%m/%Y"),
                  `Nb périodes` = PERIODES, Total = euro_compta(TOTAL))
    )
  })

  #### Volet "Compta / Gestion" — Écritures ####

  # DB_ECRITURES est une table OPTIONNELLE (cf. TABLES_OPTIONNELLES dans
  # R/connect.R) : elle n'arrive qu'avec un import Odoo, et tout part donc de
  # ce reactive, qui vaut NULL en son absence. Le volet le dit alors et se
  # tait, au lieu de rendre des graphes vides qu'on prendrait pour une panne.
  ecritures <- reactive({
    if (!exists("DB_ECRITURES")) return(NULL)
    enrichit_ecritures(DB_ECRITURES)
  })

  output$ecr_absent <- renderUI({
    if (!is.null(ecritures())) return(NULL)
    bandeau_alerte(
      TRUE,
      tagList("Les écritures au grain ligne n'arrivent qu'avec l'import Odoo.",
              " Relance un import une fois l'export déposé : le volet se",
              " remplira tout seul, et les trois onglets précédents",
              " continuent de fonctionner sans lui."),
      titre = "Pas encore d'écritures", couleur = COUL_NEUTRE,
      icone = "circle-info")
  })

  # Les filtres sont fabriqués APRÈS les données : leurs choix sont ceux qui
  # existent vraiment. Une liste de journaux écrite en dur se périmerait au
  # premier journal créé dans Odoo, et sans rien dire.
  output$ecr_filtres <- renderUI({
    e <- ecritures()
    if (is.null(e)) return(NULL)
    jx <- sort(unique(na.omit(e$JOURNAL)))
    px <- sort(unique(na.omit(e$GESTION)))
    tagList(
      radioButtons("ecr_bloc", "Périmètre", BLOCS_ECRITURES,
                   selected = "Tout"),
      dateRangeInput("ecr_dates", "Période",
                     start = min(e$DATE), end = max(e$DATE),
                     min = min(e$DATE), max = max(e$DATE),
                     format = "dd/mm/yyyy", language = "fr", separator = " au "),
      selectizeInput("ecr_poste", "Poste", choices = px, multiple = TRUE,
                     options = list(placeholder = "tous")),
      selectizeInput("ecr_journal", "Journal", choices = jx, multiple = TRUE,
                     options = list(placeholder = "tous")),
      textInput("ecr_texte", "Recherche",
                placeholder = "libellé, partenaire, pièce, compte"))
  })

  # Le filtre vit dans un renderUI : il faut qu'il ait été RENDU avant qu'un
  # forage écrive dedans, sinon la consigne se perd en silence. Même famille
  # de piège que les zones d'alerte — et outputOptions() exige que la sortie
  # existe DÉJÀ, d'où cet appel ici et non à côté de l'observateur.
  outputOptions(output, "ecr_filtres", suspendWhenHidden = FALSE)

  # Un seul filtre pour tout le volet : KPI, graphes et tableau. Deux filtres
  # indépendants donneraient un total et une liste qui ne parlent pas de la
  # même chose.
  ecr_filtre <- reactive({
    e <- ecritures()
    req(!is.null(e))
    d <- input$ecr_dates
    filtre_ecritures(
      e,
      du = if (length(d) == 2) d[1] else NULL,
      au = if (length(d) == 2) d[2] else NULL,
      bloc = input$ecr_bloc %||% "Résultat",
      journaux = input$ecr_journal, postes = input$ecr_poste,
      texte = input$ecr_texte)
  })

  output$ecr_kpi <- renderUI({ kpi_ecritures(ecr_filtre()) })

  output$ecr_recap <- renderUI({
    chips_postes(recap_postes(ecr_filtre()), input$ecr_poste)
  })

  # Huit observateurs, créés UNE FOIS au démarrage sur la liste fermée des
  # postes de gestion. Les pastilles se redessinent à chaque filtre ; leurs
  # identifiants, eux, ne bougent pas — sans quoi il faudrait créer des
  # observateurs à la volée, qui s'empilent silencieusement à chaque redessin.
  for (i in seq_along(postes_gestion())) local({
    k <- i
    observeEvent(input[[id_chip_poste(k)]], {
      ps <- postes_gestion()[k]
      cur <- input$ecr_poste
      nouv <- if (ps %in% cur) setdiff(cur, ps) else c(cur, ps)
      # "" et non character(0) : updateSelectizeInput() ignore un vecteur vide
      # et laisserait la pastille cochée après le clic qui la décoche.
      updateSelectizeInput(session, "ecr_poste",
                           selected = if (length(nouv)) nouv else "")
    }, ignoreInit = TRUE)
  })

  # Tous les journaux, pas seulement ceux de la sélection : sans ça, la
  # couleur d'un journal changerait dès qu'un filtre en retire un autre, et
  # une couleur qui bouge n'apprend plus rien.
  ecr_journaux <- reactive({
    e <- ecritures()
    if (is.null(e)) character(0) else sort(unique(e$JOURNAL))
  })

  output$ecr_table <- renderDT({
    tbl <- table_ecritures(ecr_filtre(), journaux = ecr_journaux())
    chiffres <- match(COLONNES_CHIFFREES_ECR, names(tbl)) - 1L
    datatable(
      tbl, rownames = FALSE, selection = "none", escape = FALSE,
      # Hauteur calée sur la fenêtre : le volet sert à TOUT voir d'un coup,
      # pas à feuilleter par vingt-cinq.
      options = list(
        # Pas de « f » : la recherche de DataTables est désactivée au profit
        # du champ de la barre latérale, qui pilote AUSSI les totaux.
        dom = "lrtip", pageLength = 100,
        lengthMenu = list(c(50, 100, 250, 1000), c("50", "100", "250", "1000")),
        language = LANGUE_DT,
        scrollX = TRUE, scrollY = "calc(100vh - 330px)", scrollCollapse = TRUE,
        order = list(list(0, "desc")),
        columnDefs = c(defs_tri(tbl),
                       list(list(targets = as.list(chiffres[!is.na(chiffres)]),
                                 className = "dt-right")))))
  })

  output$ecr_export <- downloadHandler(
    filename = function() nom_export_ecritures(),
    content = function(file) {
      # write.csv2 : séparateur « ; » et virgule décimale, pour qu'un double
      # clic sous un Excel belge ouvre des colonnes et non une seule.
      write.csv2(ecr_filtre(), file, row.names = FALSE, fileEncoding = "UTF-8")
    })

  #### Volet "Travail" — Sources ####
  # DB_ONSS est une table OPTIONNELLE (cf. TABLES_OPTIONNELLES dans
  # R/connect.R) : tout part donc de ce reactive, qui vaut NULL en son absence,
  # et chaque sortie sait traiter le NULL. Le volet se dessine même sans elle —
  # il compare alors Horeko et la comptabilité, ce qui reste utile.
  SRC_ONSS <- reactive({
    o <- if (exists("DB_ONSS")) DB_ONSS else NULL
    if (onss_valide(o)) o else NULL
  })

  src_unite <- reactive(input$src_unite %||% "mois")

  # Les sources au grain MENSUEL d'abord, puis consolidées : la paie est
  # mensuelle par nature, agréger avant de comparer serait perdre le détail
  # qu'on est venu voir.
  src_mensuel <- reactive({
    req(exists("DB_COUTS_TRAVAIL"))
    sources_mensuelles(DB_COUTS_TRAVAIL, SRC_ONSS())
  })

  src_serie <- reactive({
    m <- src_mensuel()
    req(!is.null(m), nrow(m) > 0)
    d <- m %>%
      mutate(PERIODE = switch(src_unite(),
                              mois      = PERIODE,
                              trimestre = floor_date(PERIODE, "quarter"),
                              annee     = floor_date(PERIODE, "year"))) %>%
      group_by(PERIODE, SOURCE, LIBELLE) %>%
      summarise(HEURES = if (all(is.na(HEURES))) NA_real_
                         else sum(HEURES, na.rm = TRUE),
                COUT = sum(COUT, na.rm = TRUE), .groups = "drop")
    # Le curseur coupe les périodes les plus anciennes, pas les sources.
    gardees <- tail(sort(unique(d$PERIODE)), as.integer(input$src_nb %||% 24))
    d %>% filter(PERIODE %in% gardees) %>% arrange(PERIODE, SOURCE)
  })

  src_secteurs <- reactive({
    req(exists("DB_COUTS_TRAVAIL"))
    bornes <- range(src_serie()$PERIODE)
    sources_par_secteur(DB_COUTS_TRAVAIL, SRC_ONSS()) %>%
      filter(PERIODE >= bornes[1])
  })

  src_non_rattache <- reactive({
    req(exists("DB_COUTS_TRAVAIL"))
    onss_non_rattache(DB_COUTS_TRAVAIL, SRC_ONSS())
  })

  output$src_alerte_onss <- renderUI({
    if (is.null(SRC_ONSS()))
      return(bandeau_alerte(
        TRUE,
        paste0("Le fichier de paie n'est pas encore importé : la clé ",
               "PATH_HEURES_ONSS manque dans l'onglet PATHS, ou le classeur ",
               "est illisible. Ce volet compare alors Horeko et la ",
               "comptabilité, et le coût du travail reste l'estimation ",
               "Horeko partout ailleurs."),
        titre = "Pas encore de paie", couleur = COUL_NEUTRE,
        icone = "circle-info"))
    # Le contrôle de lecture est calculé à l'import : s'il n'a pas été
    # conservé dans le .RData, on ne l'invente pas.
    alerte_onss(if (exists("CONTROLE_ONSS")) CONTROLE_ONSS else NULL)
  })

  output$src_alerte_manque <- renderUI({
    alerte_non_rattache(src_non_rattache())
  })

  output$src_kpi <- renderUI({ kpi_sources(src_serie()) })
  output$src_heures <- renderPlotly({
    graph_sources_heures(src_serie(), src_unite()) })
  output$src_cout <- renderPlotly({
    graph_sources_cout(src_serie(), src_unite()) })
  output$src_ecart <- renderPlotly({
    graph_ecart_sources(src_serie(), src_unite()) })
  output$src_secteurs <- renderPlotly({
    graph_sources_secteurs(src_secteurs()) })

  # Le tableau reste MENSUEL quelle que soit la granularité choisie : la
  # demande était « lister, par mois, les heures et le coût selon chaque
  # source ». Les graphiques au-dessus servent la vue d'ensemble.
  output$src_table <- renderDT({
    datatable_simple(table_sources(src_mensuel()))
  })

  output$src_non_rattache <- renderDT({
    datatable_simple(table_non_rattache(src_non_rattache()))
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
      filter(DATE >= floor_date(now(),unit="day"), DATE <= now() + days(21))
    
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
    req(ONGLETS_PRETS())
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
    req(ONGLETS_PRETS())
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
    req(ONGLETS_PRETS())
    annees <- UPD_KPI_SIMPLE() %>%
      filter(ventes > 0) %>%
      pull(DATE) %>% year() %>% unique() %>% sort(decreasing = TRUE)
    req(length(annees) > 0)
    updateSelectInput(session, "annee_choisie", choices = annees,
                      selected = annees[1])
  })

  annee_val <- reactive({
    # Entre l'insertion de l'onglet et son garnissage, le select existe mais
    # est vide : input$annee_choisie vaut "" et non NULL, et as.integer("")
    # vaut NA — que serie_annuelle() finit par passer à as.Date(), d'où un
    # « format standard non ambigu » très loin de sa cause.
    a <- suppressWarnings(as.integer(input$annee_choisie))
    if (length(a) != 1 || is.na(a)) year(today()) else a
  })

  # compta_si_dispo() et non DB_COMPTA : un mois écarté dans l'onglet Contrôle
  # doit sortir du cumul annuel aussi. Sinon l'écartement ne vaudrait que pour
  # le volet où on le décide, et deux écrans donneraient deux marges.
  serie_annee <- reactive({
    serie_annuelle(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                   compta_si_dispo(), annee_val())
  })

  serie_annee_m1 <- reactive({
    serie_annuelle(UPD_KPI_SIMPLE(), UPD_OBJECTIFS(),
                   compta_si_dispo(), annee_val() - 1)
  })

  # Deux messages, dans cet ordre : ce qui reste à vérifier, puis ce qui a
  # déjà été retiré. Les deux ne s'affichent que pour l'année regardée.
  output$annee_compta_alerte <- renderUI({
    if (!exists("DB_COMPTA")) return(NULL)
    a <- annee_val()
    ex <- compta_exclus()
    tagList(
      bandeau_mois_a_risque(compta_topo(), ex, a),
      bandeau_mois_ecartes(ex[year(ex) == a]))
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
  
  #### Volet "Planning" ####
  # DB_HEURES_PLANNING est une table OPTIONNELLE (cf. TABLES_OPTIONNELLES dans
  # R/connect.R) : tant que l'import ne la produit pas, le volet doit le dire
  # plutôt que planter. Tout part donc de ce reactive, qui vaut NULL en son
  # absence, et chaque sortie en dessous sait traiter le NULL.
  PLANNING_BRUT <- reactive({
    p <- if (exists("DB_HEURES_PLANNING")) DB_HEURES_PLANNING else NULL
    if (planning_valide(p)) p else NULL
  })

  PLAN_JOUR <- reactive({
    p <- PLANNING_BRUT()
    if (is.null(p)) return(NULL)
    # L'horizon coupe la projection, pas l'historique : les médianes et la
    # productivité de référence ont besoin de tout le passé disponible.
    horizon <- date_veille + days(req(input$plan_horizon))
    planning_jour(p %>% filter(as.Date(DATE) <= horizon),
                  UPD_KPI_SIMPLE(), date_veille)
  })

  PLAN_HABITUEL <- reactive({
    heures_habituelles(PLAN_JOUR(), DB_COUTS_TRAVAIL,
                       n_semaines = req(input$plan_ref_semaines))
  })

  # Le détail des heures hors service, pour le survol seulement : il ne passe
  # par aucun calcul, d'où ce reactive séparé plutôt qu'une colonne de plus
  # dans la projection.
  PLAN_FIXE <- reactive({
    detail_heures_fixes(PLANNING_BRUT())
  })

  # Le CA habituel de chaque jour de semaine, sur les mêmes semaines de
  # référence que les heures : c'est lui qu'on confronte à l'objectif.
  PLAN_CA_HABITUEL <- reactive({
    ca_habituel(UPD_KPI_SIMPLE(), date_veille,
                n_semaines = req(input$plan_ref_semaines))
  })

  PLAN_PROJECTION <- reactive({
    projection_planning(PLAN_JOUR(), UPD_OBJECTIFS(), PLAN_HABITUEL(),
                        PLAN_CA_HABITUEL(), RESA())
  })

  PLAN_RESUME <- reactive({
    resume_projection(PLAN_PROJECTION())
  })

  # Deux messages distincts : pas de table du tout, ou une table sans passé à
  # laquelle se comparer. Les confondre enverrait chercher le mauvais problème.
  output$plan_alerte <- renderUI({
    if (is.null(PLANNING_BRUT()))
      return(bandeau_alerte(
        TRUE,
        paste0("La table DB_HEURES_PLANNING n'est pas encore produite par ",
               "l'import. Ce volet s'allumera dès qu'elle le sera : une ligne ",
               "par jour et par service, avec les colonnes DATE, SERVICE et ",
               "HEURES."),
        titre = "Pas encore de planning", couleur = COUL_NEUTRE,
        icone = "circle-info"))

    hab <- PLAN_HABITUEL()
    bandeau_alerte(
      identical(hab$source, "heures réelles"),
      paste0("Le planning n'a pas encore assez de semaines écoulées pour ",
             "servir de repère à lui-même. Les heures habituelles viennent ",
             "donc des heures RÉELLEMENT travaillées (",
             hab$libelle, ") — on ne travaille jamais exactement ce qu'on a ",
             "planifié, l'ordre de grandeur reste bon mais la comparaison ",
             "n'est pas exacte."),
      titre = "Repère de repli", couleur = COUL_AMBRE, icone = "circle-info")
  })

  output$plan_kpi <- renderUI({
    req(PLANNING_BRUT())
    kpi_planning_tiles(PLAN_RESUME(), PLAN_HABITUEL(), PLAN_CA_HABITUEL())
  })

  output$plan_heures <- renderPlotly({
    req(PLANNING_BRUT())
    graph_planning_heures(PLAN_PROJECTION(), PLAN_HABITUEL(), PLAN_FIXE())
  })

  output$plan_rentabilite <- renderPlotly({
    req(PLANNING_BRUT())
    graph_planning_rentabilite(PLAN_PROJECTION())
  })

  output$plan_table <- renderDT({
    req(PLANNING_BRUT())
    datatable_simple(table_planning_avenir(PLAN_PROJECTION()))
  })

  #### Volet "Travail" ####
  # DB_COUTS_TRAVAIL arrive déjà recalé sur la paie (cf. import.R et
  # R/sources_travail.R) : HEURES et COUT_TRAVAIL sont les valeurs retenues,
  # Horeko n'ayant servi qu'à les répartir sur les jours et les créneaux. Le
  # total comptable sert de point de contrôle, jamais de correcteur.

  trav_unite <- reactive(input$trav_unite %||% "mois")

  # Toutes les périodes couvertes par les heures, à la granularité choisie.
  trav_dispo <- reactive({
    d <- debut_periode_travail(DB_COUTS_TRAVAIL$DATE, trav_unite())
    sort(unique(d), decreasing = TRUE)
  })

  observe({
    req(ONGLETS_PRETS())
    p <- trav_dispo()
    req(length(p) > 0)
    updateSelectizeInput(session, "trav_periodes",
                         choices = setNames(as.character(p),
                                            etiquette_periode(p, trav_unite())),
                         selected = as.character(head(p, 6)))
  })

  # Bornes de la fenêtre : du début de la plus ancienne période retenue à la
  # fin de la plus récente.
  trav_bornes <- reactive({
    req(input$trav_periodes)
    p <- intersect(as.Date(input$trav_periodes), trav_dispo())
    req(length(p) > 0)
    p <- as.Date(p, origin = "1970-01-01")
    fin <- switch(trav_unite(),
                  mois      = ceiling_date(max(p), "month") - 1,
                  trimestre = ceiling_date(max(p), "quarter") - 1,
                  annee     = ceiling_date(max(p), "year") - 1)
    list(d1 = min(p), d2 = fin, periodes = p)
  })

  trav_base <- reactive({
    b <- trav_bornes()
    base_travail(TICKETS_HEURES, DB_COUTS_TRAVAIL, b$d1, b$d2)
  })

  trav_fixe <- reactive({
    b <- trav_bornes()
    heures_fixes(DB_COUTS_TRAVAIL, b$d1, b$d2)
  })

  # Total mensuel des rémunérations, tel que la comptabilité le publie.
  trav_compta <- reactive({
    if (!"COUT_COMPTA" %in% names(DB_COUTS_TRAVAIL)) return(NULL)
    DB_COUTS_TRAVAIL %>%
      distinct(ANNEE, MOIS, COUT_COMPTA) %>%
      filter(!is.na(COUT_COMPTA)) %>%
      transmute(MOIS_DEBUT = as.Date(sprintf("%04d-%02d-01", ANNEE, MOIS)),
                COUT_COMPTA)
  })

  trav_agrege <- reactive({
    ag <- agrege_travail(trav_base(), trav_fixe(), trav_compta(), trav_unite())
    # On ne garde que les périodes explicitement retenues : les bornes peuvent
    # en couvrir d'autres si la sélection a des trous.
    filter(ag, PERIODE %in% trav_bornes()$periodes)
  })

  output$trav_kpi <- renderUI({ kpi_travail(trav_agrege()) })

  output$trav_productivite <- renderPlotly({
    graph_productivite_temps(trav_agrege(), trav_unite())
  })

  # Période détaillée : celle qu'on a cliquée, sinon la plus récente retenue.
  trav_periode_detail <- reactive({
    ev <- event_data("plotly_click", source = "trav_productivite_graph")
    ag <- trav_agrege()
    req(nrow(ag) > 0)

    # Le graphe a un axe de LIBELLÉS, pas de dates : ev$x vaut « T3 2026 », et
    # as.Date() lève dessus la même erreur « format standard non ambigu » que
    # sur un select vide. On lit donc customdata, où chaque point porte sa
    # période au format ISO (cf. graph_productivite_temps).
    d <- if (is.null(ev) || is.null(ev$customdata)) NULL
         else suppressWarnings(as.Date(as.character(ev$customdata)[1]))

    # Un clic survit au changement de granularité : la période cliquée peut ne
    # plus exister dans la série courante. On retombe alors sur la plus
    # récente, plutôt que de vider le tableau.
    if (is.null(d) || length(d) != 1 || is.na(d) || !d %in% ag$PERIODE)
      d <- max(ag$PERIODE)
    fin <- switch(trav_unite(),
                  mois      = ceiling_date(d, "month") - 1,
                  trimestre = ceiling_date(d, "quarter") - 1,
                  annee     = ceiling_date(d, "year") - 1)
    list(d1 = d, d2 = fin,
         ca = ag$CA[ag$PERIODE == d][1])
  })

  output$trav_decomp_titre <- renderText({
    paste0("Décomposition des heures — ",
           etiquette_periode(trav_periode_detail()$d1, trav_unite()))
  })

  output$trav_heures_decomp <- renderDT({
    p <- trav_periode_detail()
    # DB_ONSS sert au total de la paie, qui inclut le « Non ventilé » et les
    # secteurs sans pointage — invisibles dans la somme des lignes.
    datatable_simple(table_decomposition_travail(
      DB_COUTS_TRAVAIL, p$d1, p$d2, ca_periode = p$ca,
      db_onss = if (exists("DB_ONSS")) DB_ONSS else NULL))
  })

  # --- Créneaux types, sur la même fenêtre
  cren_stats <- reactive({ stats_creneaux(trav_base()) })

  output$cren_heatmap <- renderPlotly({
    graph_heatmap_creneaux(cren_stats(),
                           var = input$cren_indicateur %||% "CA_PAR_HEURE")
  })
  output$cren_nuage <- renderPlotly({ graph_nuage_creneaux(cren_stats()) })
  output$cren_classement <- renderPlotly({ graph_productivite_creneaux(cren_stats()) })
  output$cren_table <- renderDT({ datatable_simple(table_creneaux(cren_stats())) })

  #### Volet "Boisson" — consommation ####
  
  observe({
    req(ONGLETS_PRETS())
    updateSelectInput(session,"conso_categorie",
                      choices=c("Bières","Softs","Alcools & Vins", "Boissons chaudes"))
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
    req(ONGLETS_PRETS())
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
    req(ONGLETS_PRETS())
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
    req(ONGLETS_PRETS())
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
                              marge = 1 + input$prod_multi / 100)
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

  # EN DERNIER, parce que toutes les sorties doivent exister : sans ceci, les
  # bandeaux d'alerte ne s'affichent JAMAIS. Masqués tant qu'ils sont vides,
  # Shiny les suspend ; suspendus, ils ne se calculent pas ; jamais calculés,
  # ils restent vides. Cf. reveille_zones_alerte() dans R/helpers.R.
  reveille_zones_alerte(output)
}
