library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)

#### Palette Mazette (d'après mazette.brussels) ####

MZ_BRUN  <- "#732c02"
MZ_AMBRE <- "#d98236"
MZ_CREME <- "#f2efe6"
MZ_NOIR  <- "#260b01"
MZ_BEIGE <- "#d3c0ac"

#### Thème ####

theme_mazette_ui <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = MZ_NOIR,
  primary = MZ_BRUN,
  secondary = MZ_AMBRE,
  base_font = font_google("Inter"),
  heading_font = font_google("Bricolage Grotesque"),
  "border-radius" = "0.7rem",
  "body-bg" = MZ_CREME
)

#### Constructeurs d'interface ####
# (les constructeurs sont définis d'abord ; `ui` est assemblé tout en bas)

# Ajoute un numéro de version à une feuille de style, tiré de sa date de
# modification. Sans cela le navigateur ressert indéfiniment sa copie en
# cache et les retouches de style passent inaperçues.
feuille_versionnee <- function(fichier, dossier = "www") {
  v <- suppressWarnings(as.integer(file.mtime(file.path(dossier, fichier))))
  if (is.na(v)) fichier else paste0(fichier, "?v=", v)
}

# En-tête « application installable ». Deux choses distinctes s'y jouent :
#
#   - le manifest, qu'Android lit pour fabriquer le raccourci. Sans lui (ou avec
#     des icônes de moins de 192 px) Chrome dessine une tuile avec la première
#     lettre du nom d'hôte — d'où le « P » de Posit ;
#   - apple-touch-icon, qu'iOS utilise à la place : Safari ignore le manifest.
#
# Tous les chemins sont RELATIFS. Posit Connect sert l'app sous /content/<id>/ :
# un "/icone-192.png" pointerait à la racine du serveur, et un start_url absolu
# sortirait du scope, ce qui fait échouer l'installation sans message d'erreur.
entete_application <- function() {
  tags$head(
    tags$link(rel = "manifest", href = feuille_versionnee("manifest.webmanifest")),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192",
              href = feuille_versionnee("icone-192.png")),
    tags$link(rel = "apple-touch-icon", href = feuille_versionnee("apple-touch-icon.png")),
    tags$meta(name = "theme-color", content = "#732c02"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    # "default" et non "black-translucent" : ce dernier fait passer le contenu
    # SOUS la barre d'état de l'iPhone, ce qui masque le haut de la navbar tant
    # qu'on n'a pas ajouté de padding env(safe-area-inset-top) en CSS.
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
    tags$meta(name = "apple-mobile-web-app-title", content = "Mazette"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  )
}

ui_login <- function() {
  div(
    class = "login-wrap",
    card(
      class = "login-card",
      card_header(span(class = "brand-title", 
                       img(src = "mazette.png", height = 30, width = 30),
                       
                       "MAZETTE")),
      card_body(
        p("Tableau de bord", class = "text-muted mb-3"),
        div(textOutput("text_log"), style = "color:#c0392b; min-height: 1.2em;"),
        passwordInput("password", "Mot de passe", ""),
        actionButton("boutton_log", "Entrer", class = "btn-primary w-100")
      )
    )
  )
}

# Petite légende des couleurs des barres
chip_legende <- function(couleur, libelle) {
  div(
    class = "legende-row",
    span(class = "legende-pastille", style = paste0("background:", couleur, ";")),
    span(libelle)
  )
}

# Légende de la convention "CA vs objectif", à poser sous les graphes en barres.
# Reprend les couleurs de couleur_objectif() dans functions.R.
legende_objectif <- function() {
  div(
    class = "d-flex gap-3 flex-wrap text-muted mt-1",
    chip_legende(COUL_VERT, "Objectif atteint"),
    chip_legende(COUL_AMBRE, "À partir de 90 %"),
    chip_legende(COUL_ROUGE, "En dessous de 90 %")
  )
}

ui_app <- function() {
  navset_bar(
    title = span(class = "brand-title", 
                 img(src = "mazette-blanc.png", height = 30, width = 30)),
    id = "nav",
    fillable = FALSE,
    # Un seul onglet part avec la page : l'accueil. Les autres sont insérés
    # après la connexion, selon les droits du mot de passe saisi (server.R).
    # Ce n'est pas qu'un masquage : le HTML d'un onglet interdit ne quitte
    # jamais le serveur, et ses sorties ne sont jamais calculées.
    panneau_onglet(ONGLET_ACCUEIL),
    nav_spacer(),
    nav_item(uiOutput("badge_utilisateur", inline = TRUE)),
    nav_item(
      radioGroupButtons("unite_tva", label = NULL,
                        choices = c("HTVA", "TVAC"), selected = "HTVA",
                        size = "sm", status = "outline-secondary")
    )
  )
}

# Onglet "Accueil" : cards simples avec une info importante de chaque onglet
ui_accueil <- function() {
  # La grille est rendue côté serveur : elle ne contient que les cartes des
  # onglets auxquels le mot de passe donne droit (grille_cartes_accueil(),
  # dans R/acces.R). Construire les huit puis masquer les interdites laissait
  # un trou par carte masquée — layout_columns() enveloppe chaque enfant dans
  # une cellule de grille, qui reste réservée même vide.
  tagList(
    uiOutput("accueil_kpi"),
    uiOutput("accueil_cartes")
  )
}

# Onglet "Planning" : les heures qu'on s'apprête à poser, face au CA qu'elles
# devront produire. Seul volet tourné vers l'avant — d'où un onglet à part
# plutôt qu'un ajout à « Maintenant », qui raconte ce qui s'est passé.
#
# Deux graphiques, une seule grammaire : la barre est ce qui est prévu, le
# trait est la référence. Le premier ne parle qu'en heures, le second qu'en
# euros. Aucun ne mélange les deux.
ui_planning <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Planning",
      width = 300,
      sliderInput("plan_horizon", "Horizon (jours à venir)",
                  min = 7, max = 42, value = 21, step = 7, ticks = FALSE),
      sliderInput("plan_ref_semaines", "Semaines de référence",
                  min = 2, max = 16, value = 8, step = 1, ticks = FALSE),
      hr(),
      div(class = "small text-muted",
          tags$b("Heures habituelles"), " : la médiane des heures posées le",
          " même jour de semaine, sur les semaines de référence. Un samedi se",
          " compare à un samedi.", tags$br(), tags$br(),
          tags$b("CA attendu"), " = heures planifiées × le CA par heure",
          " habituel. C'est ce que ces heures rapportent d'ordinaire.",
          tags$br(), tags$br(),
          tags$b("Les couverts réservés"), " sont donnés en survol : ils",
          " expliquent souvent pourquoi on met plus de monde, sans entrer",
          " dans le calcul.", tags$br(), tags$br(),
          tags$b("Sans coût du travail"), ", ce volet dit si un planning est",
          " inhabituel et si l'objectif est couvert — pas s'il est rentable",
          " en euros de marge.")
    ),
    uiOutput("plan_alerte"),
    uiOutput("plan_kpi"),
    card(
      full_screen = TRUE,
      card_header("Combien d'heures, comparé à d'habitude ?"),
      plotlyOutput("plan_heures", height = "330px"),
      div(class = "small text-muted",
          "Barre : les heures planifiées. Trait noir : ce qu'on met",
          " habituellement ce jour-là. En ambre au-delà de +10 %.",
          " Survolez pour les couverts déjà réservés.")
    ),
    card(
      full_screen = TRUE,
      card_header("Est-ce que ces heures couvrent l'objectif ?"),
      plotlyOutput("plan_rentabilite", height = "330px"),
      div(class = "small text-muted",
          "Barre : le CA que ces heures rapportent au rythme habituel.",
          " Trait noir : l'objectif du jour. Verte, la barre passe l'objectif ;",
          " rouge, elle reste dessous.")
    ),
    card(
      full_screen = TRUE, height = "440px",
      card_header("Détail jour par jour"),
      DTOutput("plan_table")
    )
  )
}

# Onglet "Travail" : productivité et coût du travail, dans le temps puis
# créneau par créneau (midi / soir / Pizzwanze).
ui_travail <- function() {
  navset_card_tab(
    id = "travail_tabs",
    nav_panel(
      title = "Suivi",
      icon = icon("chart-line"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Période",
          width = 290,
          dateRangeInput("trav_periode", "Fenêtre analysée",
                         start = NULL, end = NULL,
                         separator = " → ", language = "fr",
                         weekstart = 1, format = "dd/mm/yyyy"),
          radioButtons("trav_unite", "Granularité",
                       c("Par semaine" = "semaine", "Par mois" = "mois"),
                       selected = "mois"),
          hr(),
          div(class = "small text-muted",
              tags$b("Heures de service"), " : directement liées à l'ouverture",
              " d'un créneau.", tags$br(), tags$br(),
              tags$b("Coûts indirects"), " : transfo, brasserie et support,",
              " mutualisés sur la semaine puis répartis entre créneaux au",
              " prorata du CA.", tags$br(), tags$br(),
              tags$b("Marge après travail"), " = CA HTVA − coût de service −",
              " coûts indirects. Reste à couvrir matières, loyer et énergie.",
              tags$br(), tags$br(),
              tags$b("Remarque"), " Les coûts indirects sont d'abord agrégés par 
              semaine. L'analyse par mois de ces coûts peut donc légèrement 
              différer de la somme par semaine.")
        ),
        uiOutput("trav_kpi"),
        card(
          full_screen = TRUE,
          card_header("Décomposition du CA : marge et coûts du travail"),
          plotlyOutput("trav_structure", height = "340px")
        ),
        card(
          full_screen = TRUE, height = "350px",
          card_header("Décomposition des heures de travail"),
          DTOutput("trav_heures_decomp"),
          div(class = "small text-muted",
              "Cliquez sur un mois sur le graphique pour avoir la 
              décomposition des heures.")
        ),
        card(
          full_screen = TRUE,
          card_header("Productivité : heures de service et CA par heure"),
          plotlyOutput("trav_productivite", height = "340px"),
          div(class = "small text-muted",
              "En pointillé : la productivité moyenne de la fenêtre.")
        ),
        card(
          full_screen = TRUE,
          card_header("CA par créneau (midi / soir / Pizzwanze)"),
          plotlyOutput("trav_ca_creneaux", height = "320px")
        )
      )
    ),
    nav_panel(
      title = "Créneaux",
      icon = icon("table-cells"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Analyse par créneau",
          width = 290,
          dateRangeInput("cren_periode", "Fenêtre analysée",
                         start = NULL, end = NULL,
                         separator = " → ", language = "fr",
                         weekstart = 1, format = "dd/mm/yyyy"),
          selectInput("cren_indicateur", "Indicateur de la heatmap",
                      c("CA moyen par ouverture" = "CA_moyen",
                        "CA par heure de service" = "CA_PAR_HEURE",
                        "Coût du travail / CA"    = "RATIO_TOTAL",
                        "Marge par ouverture"     = "MARGE_moyenne"),
                      selected = "CA_PAR_HEURE"),
          hr(),
          div(class = "small text-muted",
              "Chaque créneau est ramené à une ", tags$b("ouverture type"),
              " pour comparer les jours à armes égales.", tags$br(), tags$br(),
              "Un mardi soir avec vente de pizza est compté comme ",
              tags$b("Pizzwanze"), ".")
        ),
        layout_columns(
          col_widths = c(5, 7),
          card(full_screen = TRUE,
               card_header("Vue jour × créneau"),
               plotlyOutput("cren_heatmap", height = "380px")),
          card(full_screen = TRUE,
               card_header("Productivité : CA vs heures de service"),
               plotlyOutput("cren_nuage", height = "380px"),
               div(class = "small text-muted",
                   "Plus un créneau est haut à gauche, plus il est efficace. ",
                   "La taille des points reflète le CA total."))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(full_screen = TRUE,
               card_header("Classement par productivité horaire"),
               plotlyOutput("cren_classement", height = "400px")),
          card(full_screen = TRUE,
               card_header("Décomposition du CA moyen par créneau"),
               plotlyOutput("cren_decomposition", height = "400px"))
        ),
        card(
          full_screen = TRUE,
          card_header("Détail par créneau"),
          DTOutput("cren_table")
        )
      )
    )
  )
}

# Onglet "Réservations" : ce qui arrive, ce qui s'est passé, et ce que la
# réservation apporte au chiffre d'affaires.
ui_reservations <- function() {
  navset_card_tab(
    id = "resa_tabs",
    nav_panel(
      title = "À venir",
      icon = icon("clock"),
      uiOutput("resa_kpi_prochaines"),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(7, 5)),
        card(
          full_screen = TRUE,
          card_header("Couverts réservés par jour"),
          radioButtons("resa_agenda_par", NULL,
                       c("Salle / terrasse" = "lieu", "Midi / soir" = "creneau"),
                       selected = "lieu", inline = TRUE),
          plotlyOutput("resa_agenda", height = "320px")
        ),
        card(
          full_screen = TRUE,
          card_header("Prochaines réservations"),
          DTOutput("resa_prochaines")
        )
      )
    ),
    nav_panel(
      title = "Statistiques",
      icon = icon("chart-simple"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Période analysée", width = 290,
          dateRangeInput("resa_periode", NULL, start = NULL, end = NULL,
                         separator = " → ", language = "fr",
                         weekstart = 1, format = "dd/mm/yyyy"),
          radioButtons("resa_par", "Répartir par",
                       c("Salle / terrasse" = "lieu", "Midi / soir" = "creneau",
                         "Taille de groupe" = "taille"), selected = "lieu"),
          hr(),
          div(class = "small text-muted",
              "On mesure des",
              " réservations ", tags$b("enregistrées"), ", pas de présences",
              " constatées. Aucun taux de no-show n'est calculable.")
        ),
        uiOutput("resa_kpi_stats"),
        card(
          full_screen = TRUE,
          card_header("Heures d'arrivée"),
          plotlyOutput("resa_heures", height = "320px")
        ),
        card(
          full_screen = TRUE,
          card_header("Jours de semaine et tailles de groupe"),
          plotlyOutput("resa_jours", height = "340px")
        )
      )
    ),
    nav_panel(
      title = "Historique",
      icon = icon("clock-rotate-left"),
      layout_columns(
        fill = FALSE, col_widths = breakpoints(sm = 12, md = c(4, 8)),
        radioButtons("resa_unite", "Granularité",
                     c("Par semaine" = "semaine", "Par mois" = "mois"),
                     selected = "mois", inline = TRUE),
        div()
      ),
      card(
        full_screen = TRUE,
        card_header("Couverts réservés par lieu, et taille moyenne des groupes"),
        plotlyOutput("resa_historique", height = "360px")
      ),
      card(
        full_screen = TRUE,
        card_header("Détail par période"),
        DTOutput("resa_table_histo")
      )
    ),
    nav_panel(
      title = "Réservations et CA",
      icon = icon("link"),
      uiOutput("resa_kpi_ca"),
      card(
        full_screen = TRUE,
        card_header("CA du jour selon les couverts réservés"),
        plotlyOutput("resa_ca_nuage", height = "400px"),
        div(class = "small text-muted",
            "Chaque point est un jour d'ouverture. La réservation ne couvre",
            " qu'une part de la clientèle : la pente mesure ce qu'un couvert",
            " réservé apporte au CA du jour, pas le remplissage de la salle.",
            tags$br(),
            "La corrélation n'est pas la causalité — un samedi soir attire à la",
            " fois plus de réservations et plus de passage.")
      ),
      card(
        full_screen = TRUE,
        card_header("Jours les mieux réservés"),
        DTOutput("resa_ca_table")
      )
    )
  )
}

ui_comparaison <- function() {
  tagList(
    card(min_height = "300px",
      card_header("Périodes à comparer"),
      layout_columns(
        fill = FALSE, col_widths = breakpoints(sm = 12, md = c(4, 8)),
        radioButtons("comp_unite", "Comparer par",
                     c("Semaine" = "semaine", "Mois" = "mois", "Année" = "annee"),
                     selected = "mois", inline = TRUE),
        selectizeInput("comp_periodes", "Périodes", choices = NULL,
                       multiple = TRUE, width = "100%",
                       options = list(plugins = list("remove_button"),
                                      placeholder = "Choisir des périodes…"))
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Chiffre d'affaires par période"),
      plotlyOutput("comp_graph", height = "400px"),
      legende_objectif(),
      div(class = "small text-muted mt-1",
          "La marge d'exploitation n'apparaît",
          " que là où la comptabilité du mois est disponible.")
    ),
    card(
      full_screen = TRUE,
      card_header("Tableau comparatif"),
      DTOutput("comp_table")
    )
  )
}

ui_compta <- function() {
  navset_card_tab(
    id = "compta_tabs",
    nav_panel(
      title = "Exploitation",
      icon = icon("chart-simple"),
      ui_exploitation()
    ),
    nav_panel(
      title = "Comptabilité générale",
      icon = icon("book"),
      ui_compta_generale()
    )
  )
}

# Du chiffre d'affaires à la marge d'exploitation, mois par mois.
# Tout provient de DB_COMPTA : pas de données simulées, donc un mois sans
# comptabilité reste vide plutôt que d'être comblé.
ui_exploitation <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Période", width = 300,
      selectInput("expl_periode", "Mois analysé", choices = NULL),
      radioButtons("expl_unite", "Granularité de la série",
                   c("Par mois" = "mois", "Par trimestre" = "trimestre",
                     "Par année" = "annee"), selected = "mois"),
      sliderInput("expl_nb", "Périodes affichées", min = 4, max = 36,
                  value = 12, step = 1, ticks = FALSE),
      checkboxInput("expl_pct", "Tableau en % du CA", FALSE),
      hr(),
      div(class = "small text-muted",
          tags$b("Marge d'exploitation"), " = produits − matières − rémunérations",
          " − frais généraux − amortissements.", tags$br(), tags$br(),
          "Les charges financières et exceptionnelles sont hors de ce champ :",
          " elles interviennent après, dans l'onglet Comptabilité générale.",
          tags$br(), tags$br(),
          "Les ratios sont rapportés au chiffre d'affaires seul.")
    ),
    uiOutput("expl_controle"),
    uiOutput("expl_kpi"),
    card(
      full_screen = TRUE,
      card_header("Du chiffre d'affaires à la marge"),
      plotlyOutput("expl_cascade", height = "380px")
    ),
    card(
      full_screen = TRUE,
      card_header("Structure des charges dans le temps"),
      plotlyOutput("expl_structure", height = "360px")
    ),
    card(
      full_screen = TRUE,
      card_header("Détail par période"),
      DTOutput("expl_table")
    )
  )
}

# Compte de résultat détaillé, mois par mois, plusieurs mois côte à côte.
# La hiérarchie du plan comptable est reconstruite depuis l'indentation de
# TX_DESCRIPTION (cf. R/plan_comptable.R).
ui_compta_generale <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Périodes", width = 310,
      selectizeInput("cg_periodes", "Mois à comparer", choices = NULL,
                     multiple = TRUE,
                     options = list(plugins = list("remove_button"),
                                    placeholder = "Choisir un ou plusieurs mois")),
      checkboxInput("cg_detail", "Dérouler les comptes", FALSE),
      checkboxInput("cg_pct", "En % du chiffre d'affaires", FALSE),
      hr(),
      div(class = "small text-muted",
          "Les comptes sont classés sur leur ", tags$b("numéro"), " : 70 ventes,",
          " 60 achats (609 variations de stock), 61 services et biens, 62",
          " rémunérations, 63 amortissements, 64 autres charges, 65/75",
          " financier.", tags$br(), tags$br(),
          tags$b("Soldes"), " : calculés en cumulant les postes qui les",
          " précèdent, selon leur définition comptable.")
    ),
    uiOutput("cg_kpi"),
    card(
      full_screen = TRUE,
      card_header(textOutput("cg_titre", inline = TRUE)),
      DTOutput("cg_table")
    ),
    navset_card_tab(
      nav_panel(
        title = "Soldes",
        icon = icon("chart-column"),
        plotlyOutput("cg_soldes", height = "330px")
      ),
      nav_panel(
        title = "Contrôle des totaux",
        icon = icon("scale-balanced"),
        div(class = "small text-muted mb-2",
            "Comptes que le plan ne sait pas ranger d'après leur numéro. Ils",
            " n'entrent dans aucun total : la liste doit rester vide."),
        DTOutput("cg_controle")
      ),
      nav_panel(
        title = "Vie des comptes",
        icon = icon("clock-rotate-left"),
        div(class = "small text-muted mb-2",
            "Première et dernière période où chaque compte porte un montant.",
            " Utile pour ne pas lire une baisse là où il n'y a qu'un changement",
            " de plan comptable."),
        DTOutput("cg_vie")
      )
    )
  )
}

# Un volet compta (semaine ou mois). `sfx` préfixe tous les identifiants pour
# que les deux sous-onglets soient indépendants.
# NB : tout est construit statiquement ; l'activation du panneau de comparaison
# se fait par shinyjs::show/hide (pas de renderUI, qui casserait les
# dépendances CSS bslib).
ui_compta_volet <- function(sfx) {
  id <- function(x) paste0("compta_", sfx, "_", x)

  panneau <- function(cle) {
    div(
      class = "compta-panel", id = id(paste0("panel_", cle)),
      div(class = "compta-panel-titre",
          textOutput(id(paste0("titre_", cle)), inline = TRUE)),
      uiOutput(id(paste0("kpi_", cle))),
      card(
        full_screen = TRUE,
        card_header("Coûts par secteur"),
        plotlyOutput(id(paste0("secteurs_", cle)), height = "260px"),
        DTOutput(id(paste0("table_", cle)))
      )
    )
  }

  layout_sidebar(
    sidebar = sidebar(
      title = "Période",
      width = 290,
      selectInput(id("a"), "Période analysée", choices = NULL),
      hr(),
      checkboxInput(id("cmp"), "Comparer avec une autre période", value = FALSE),
      shinyjs::hidden(
        div(id = id("cmp_box"),
            selectInput(id("b"), "Période comparée", choices = NULL))
      ),
      hr(),
      div(class = "small text-muted",
          tags$b("Food Cost"), " = matières / CA", tags$br(),
          tags$b("Work Cost"), " = personnel / CA", tags$br(),
          tags$b("Prime Cost"), " = (matières + personnel) / CA", tags$br(),
          tags$b("Marge"), " = CA − prime cost", tags$br(), tags$br(),
          "Coûts des matières issus de la comptabilité (DB_COMPTA).")
    ),
    card(
      full_screen = TRUE,
      card_header("Évolution — cliquez sur une barre pour analyser la période"),
      plotlyOutput(id("evo"), height = "330px")
    ),
    div(class = "compta-split", panneau("a"), shinyjs::hidden(panneau("b"))),
    shinyjs::hidden(
      div(id = id("ecarts_box"),
          card(
            card_header("Écarts — période analysée moins période comparée"),
            uiOutput(id("ecarts"))
          ))
    ),
    card(
      full_screen = TRUE,
      card_header("Évolution des indicateurs (% du CA)"),
      plotlyOutput(id("kpi_evo"), height = "320px")
    )
  )
}

ui_annee <- function() {
  tagList(
    card(
      card_header("Suivi de l'année en cours (à date)"),
      layout_columns(
        fill = FALSE,
        col_widths = c(3, 9),
        selectInput("annee_choisie", "Année", choices = NULL),
        div(class = "small text-muted align-self-end pb-2",
            "Cumuls arrêtés à hier : on ne compare que les jours déjà écoulés.")
      ),
      uiOutput("annee_kpi")
    ),
    card(
      full_screen = TRUE,
      card_header("Écart cumulé de CA vs objectif"),
      plotlyOutput("annee_ecart_obj", height = "340px")
    ),
    card(
      full_screen = TRUE,
      card_header("Écart cumulé de CA vs N-1 (même semaine, même jour)"),
      plotlyOutput("annee_ecart_ym1", height = "340px")
    ),
    card(
      full_screen = TRUE,
      card_header("Marge d'exploitation cumulée"),
      plotlyOutput("annee_marge", height = "340px"),
      div(class = "small text-muted",
          "Marge = CA + autres produits − matières − rémunérations − frais ",
          "généraux − amortissements, telle que la donne la comptabilité.",
          tags$br(),
          "Elle est mensuelle : ses charges sont réparties uniformément sur les ",
          "jours d'ouverture du mois. Le cumul est donc exact à chaque fin de ",
          "mois, seul le chemin à l'intérieur du mois est lissé.")
    ),
    card(
      full_screen = TRUE,
      card_header("Écart cumulé de marge vs N-1"),
      plotlyOutput("annee_ecart_marge", height = "340px"),
      div(class = "small text-muted",
          "L'écart est disponible que pour les dates où les deux marges sont ",
          "disponibles.")
    )
  )
}

ui_simulation <- function() {
  tagList(
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      value_box("CA actuel", textOutput("sim_vb_actuel"),
                showcase = icon("euro-sign"), theme = "primary"),
      value_box("CA simulé", textOutput("sim_vb_simule"),
                showcase = icon("wand-magic-sparkles"),
                theme = value_box_theme(bg = MZ_AMBRE, fg = "#ffffff")),
      value_box("Écart", textOutput("sim_vb_delta"),
                showcase = icon("arrow-right-arrow-left"),
                theme = value_box_theme(bg = "#efe7d8", fg = MZ_BRUN))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Paramètres"),
        layout_columns(
          fill = FALSE,
          col_widths = c(6, 6),
          dateRangeInput("sim_periode", "Période de référence",
                         start = NULL, end = NULL,
                         separator = " → ", language = "fr",
                         weekstart = 1, format = "dd/mm/yyyy"),
          selectInput("sim_categorie", "Catégorie à ajuster", choices = NULL),
          numericInput("sim_pct", "Variation (%)", value = 0,
                       min = -100, max = 100, step = 1),
          
            div(class = "d-flex gap-2 align-items-end",
                actionButton("sim_apply", "Appliquer", class = "btn-primary"),
                actionButton("sim_reset", "↺ Réinitialiser"))
        ),
        div(class = "small text-muted",
            "Hypothèse : quantités inchangées. Modifiez aussi un prix directement",
            " dans la colonne « Prix simulé » du tableau. Tous les prix sont TVAC.")
      ),
      card(
        card_header("Produits dont le prix a changé"),
        DTOutput("sim_table_diff")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Tous les produits (prix simulé éditable)"),
      DTOutput("sim_table")
    )
  )
}

# Onglet "Fûts" : suivi des fûts en cours (niveaux, prédiction de fin,
# rapport de brassin). La consommation de bières est dans l'onglet "Bières".
ui_futs <- function() {
  tagList(
    card(
      card_header("Niveau des bières en cours"),
      # Cartes HTML plutôt qu'une grille plotly : la hauteur suit le nombre de
      # fûts et les colonnes s'adaptent à la largeur de l'écran.
      uiOutput("bieres_niveaux")
    ),
    card(
      full_screen = TRUE,
      card_header("Évolution & prédiction des fûts"),
      plotlyOutput("bieres_evo", height = "500px")
    ),
    card(
      card_header("Fin de fût prévue"),
      DTOutput("bieres_predict_table")
    ),
    card(
      full_screen = TRUE,
      card_header("Rapport de brassin"),
      selectInput("brassin_choisi", "Choisir un brassin", choices = NULL,
                  width = "340px"),
      plotOutput("brassin_report", height = "680px")
    )
  )
}

# Onglet "Boisson" : consommation, à la semaine, comparée à S-1.
ui_conso_boissons <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Semaine",
      width = 280,
      selectInput("conso_semaine", "Semaine analysée", choices = NULL),
      selectInput("conso_categorie", "Catégorie", choices = NULL),
      hr(),
      div(class = "small text-muted",
          "Tout est comparé à la ", tags$b("semaine précédente"), ".",
          tags$br(), tags$br(),
          "Les volumes viennent des tickets (format du verre × quantité). ",
          "Certaines différences peuvent advenir par rapport aux résultats",
          "globaux.", tags$br(), tags$br(),
          "Les heures suivent le ", tags$b("jour de service"), " : une pinte ",
          "servie à 1h du matin compte pour la soirée de la veille.")
    ),
    uiOutput("conso_kpi"),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Top boisson — litres servis vs S-1"),
           plotlyOutput("conso_top", height = "420px")),
      card(full_screen = TRUE,
           card_header("Tendance des principales boissons"),
           plotlyOutput("conso_tendance", height = "420px"),
           div(class = "small text-muted",
               "Les 5 boissons les plus servies cette semaine, suivies sur ",
               "12 semaines."))
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(full_screen = TRUE,
           card_header("Quand boit-on ? (litres par jour et par heure)"),
           plotlyOutput("conso_heatmap", height = "360px")),
      card(full_screen = TRUE,
           card_header("Formats servis"),
           plotlyOutput("conso_formats", height = "360px"))
    ),
    card(
      full_screen = TRUE,
      card_header("Historique hebdomadaire"),
      plotlyOutput("conso_evo", height = "320px")
      # div(class = "small text-muted",
      #     "La semaine sélectionnée est mise en avant.")
    ),
    card(
      full_screen = TRUE,
      card_header("Détail par boisson"),
      DTOutput("conso_table")
    )
  )
}

# Onglet "Pizzwanze" : suivi des soirées pizza, soirée par soirée.
ui_pizzwanze <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Soirée",
      width = 280,
      selectInput("pizz_soiree", "Soirée analysée", choices = NULL),
      # selectInput("pizz_profondeur", "Historique de la carte",
      #             c("Toutes les soirées" = "0", "12 dernières" = "12",
      #               "24 dernières" = "24"),
      #             selected = "0"),
      hr(),
      div(class = "small text-muted",
          # "Tout est comparé à la ", tags$b("soirée précédente"), ".",
          # tags$br(), tags$br(),
          "Les pizzas sont repérées par leur ", tags$b("nom"),
          " (aucune catégorie dédiée), Khachapuri compris.", tags$br(), tags$br(),
          "Une soirée pizza se reconnaît à sa carte : au moins deux pizzas ",
          "différentes. Les jours où l'on écoule une seule référence ",
          "(parts du lendemain, slices d'événement) ne sont pas comptés.")
    ),
    uiOutput("pizz_kpi"),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header(textOutput("pizz_titre")),
           plotlyOutput("pizz_soiree", height = "380px")),
           # div(class = "small text-muted",
           #     "Couleur selon la présence historique de la pizza.")),
      card(full_screen = TRUE,
           card_header("Ventes par heure"),
           plotlyOutput("pizz_heure", height = "380px"))
    ),
    card(
      full_screen = TRUE,
      card_header("Par type de pizza"),
      plotlyOutput("pizz_carte", height = "420px")
      # div(class = "small text-muted",
      #     "Les pizzas les plus fidèles sont en haut, les passagères en bas.")
    ),
    card(
      full_screen = TRUE,
      card_header("Historique des soirées"),
      plotlyOutput("pizz_evo", height = "330px")
      # div(class = "small text-muted",
      #     "Les losanges verts indiquent le nombre de nouveautés de la soirée.")
    ),
    card(
      full_screen = TRUE,
      card_header("Détail par pizza"),
      DTOutput("pizz_table")
    )
  )
}

# Carte "Production" : combien préparer de chaque ingrédient pour la semaine.
# Les champs sont préremplis par le serveur (moyenne des dernières semaines
# complètes) et restent entièrement modifiables ; la dernière ligne est libre.
ui_production_focaccias <- function() {
  # Un vrai tableau plutôt qu'une grille CSS : l'alignement des colonnes est
  # garanti par le navigateur, sans dépendre de www/style.css (que le
  # navigateur garde volontiers en cache). Les classes utilisées viennent de
  # Bootstrap, livré par bslib, donc toujours à jour.
  champ <- function(id, pas) {
    numericInput(id, NULL, value = NA, min = 0, step = pas, width = "100%")
  }

  ligne <- function(i, nom, libre = FALSE) {
    tags$tr(
      tags$td(
        if (libre)
          textInput(paste0("prod_nom_", i), NULL, value = "",
                    placeholder = "Autre ingrédient", width = "100%")
        else tags$span(class = "fw-semibold", nom)
      ),
      tags$td(champ(paste0("prod_foc_", i), 1)),
      tags$td(champ(paste0("prod_por_", i), 5)),
      tags$td(class = "text-end fw-bold",
              textOutput(paste0("prod_nec_", i), inline = TRUE)),
      tags$td(champ(paste0("prod_stk_", i), 10)),
      tags$td(class = "text-end fw-bold",
              textOutput(paste0("prod_faire_", i), inline = TRUE))
    )
  }

  entete <- function(...) tags$th(scope = "col", ...)

  card(
    full_screen = TRUE,
    card_header("Production"),
    # Feuille de style locale : embarquée dans la page, donc jamais servie
    # depuis le cache, contrairement à un fichier externe.
    tags$style(HTML(paste0(
      ".prod-table th{font-size:.78rem;font-weight:700;vertical-align:bottom;",
      "color:", MZ_BRUN, ";}",
      ".prod-table td{vertical-align:middle;}",
      ".prod-table .shiny-input-container{margin-bottom:0!important;}",
      ".prod-table input.form-control{padding:.25rem .5rem;font-size:.88rem;}"
    ))),
    div(
      class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-2",
      div(class = "small text-muted", textOutput("prod_source", inline = TRUE)),
      div(
        class = "d-flex align-items-center gap-2 prod-barre",
        tags$label("Marge", `for` = "prod_multi", class = "small text-muted mb-0"),
        numericInputIcon("prod_multi", label = NULL, value = 10, step = 5,
                         icon = list(NULL, "%"), size = "sm", width = "120px"),
        actionButton("prod_reset", "↺ Réinitialiser", class = "btn-sm")
      )
    ),
    div(
      class = "table-responsive",
      tags$table(
        class = "table table-sm align-middle mb-0 prod-table",
        tags$colgroup(
          tags$col(style = "width:20%"), tags$col(style = "width:15%"),
          tags$col(style = "width:13%"), tags$col(style = "width:17%"),
          tags$col(style = "width:15%"), tags$col(style = "width:20%")
        ),
        tags$thead(
          tags$tr(
            entete("Ingrédient"),
            entete("Focaccias concernées"),
            entete("Portion (g)"),
            entete(class = "text-end", "Quantité nécessaire"),
            entete("Stock actuel (g)"),
            entete(class = "text-end", "Quantité à produire")
          )
        ),
        tags$tbody(
          ligne(1, "Crémeux"),
          ligne(2, "Légume"),
          ligne(3, "Fromage"),
          ligne(4, "Viande"),
          ligne(5, NULL, libre = TRUE)
        )
      )
    ),
    div(class = "small text-muted mt-2",
        tags$b("Crémeux"), " et ", tags$b("légume"),
        " sont comptés sur toutes les focaccias ; ",
        tags$b("fromage"), " et ", tags$b("viande"),
        " sur celles qui portent le supplément correspondant, « full » compris.")
  )
}

# Onglet "Focaccias" : suivi du produit phare et de ses options.
ui_focaccias <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Semaine",
      width = 280,
      selectInput("foca_semaine", "Semaine analysée", choices = NULL),
      hr(),
      div(class = "small text-muted",
          "Chaque focaccia est décomposée en une ", tags$b("base"),
          " (du moment ou brunch) et ses ", tags$b("options"),
          " : fromage, viande, et spicy hot.",
          tags$br(), tags$br(),
          "Les remises et lignes négatives sont exclues.")
    ),
    uiOutput("foca_kpi"),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Ventes de la semaine"),
           plotlyOutput("foca_jour", height = "360px"),
           div(class = "small text-muted",
               "En pointillé : la même semaine, une semaine plus tôt.")),
      card(full_screen = TRUE,
           card_header("Options les plus commandées"),
           plotlyOutput("foca_variantes", height = "360px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("Historique hebdomadaire"),
           plotlyOutput("foca_evo", height = "330px")),
      card(full_screen = TRUE,
           card_header("Taux d'options dans le temps"),
           plotlyOutput("foca_options", height = "330px"))
    ),
    card(
      full_screen = TRUE,
      card_header("Détail par option"),
      DTOutput("foca_table")
    ),
    ui_production_focaccias()
  )
}

ui_historique <- function() {
  tagList(
    card(
      card_header("Historique du chiffre d'affaires"),
      radioButtons("hist_unite", "Granularité",
                   c("Par semaine" = "semaine", "Par mois" = "mois"),
                   selected = "semaine", inline = TRUE),
      plotlyOutput("hist_graph", height = "330px"),
      legende_objectif(),
      plotlyOutput("hist_evo", height = "410px")
    )
  )
}



ui_detail <- function() {
  navset_card_tab(
    selected = "sem",
    nav_panel(
      value = 'jour',
      title = "Par jour",
      icon = icon("calendar-day"),
      layout_columns(
        fill = FALSE,
        col_widths = c(8, 4),
        dateRangeInput("detail_periode", "Période",
                       start = NULL, end = NULL,
                       separator = " → ", language = "fr",
                       weekstart = 1, format = "dd/mm/yyyy"),
        div(class = "small text-muted align-self-end pb-2",
            "Cliquez sur une barre pour détailler la journée.")
      ),
      plotlyOutput("detail_jour_graph", height = "300px"),
      legende_objectif(),
      hr(),
      card_header(textOutput("detail_jour_titre", inline = TRUE)),
      layout_columns(
        col_widths = c(2, 5, 5),
        uiOutput("detail_jour_box"),
        div(
          h6("Coût du jour", class = "section-sub"),
          DTOutput("detail_jour_travail"),
          h6("Coût de la semaine", class = "section-sub"),
          DTOutput("detail_jour_travail_semaine"),
          DTOutput("detail_jour_cout")
        ),
        div(
          h6("Produits du jour", class = "section-sub"),
          DTOutput("detail_jour_produits")
        )
      )
    ),
    nav_panel(
      value = 'sem',
      title = "Par semaine",
      icon = icon("calendar-week"),
      ui_detail_periode("sem")
    ),
    nav_panel(
      value = 'mois',
      title = "Par mois",
      icon = icon("calendar-days"),
      ui_detail_periode("mois")
    ),
    nav_panel(
      value = 'produit',
      title = "Par produit",
      icon = icon("box"),
      layout_columns(
        col_widths = c(5, 7),
        div(
          dateRangeInput("detail_produit_periode", "Période",
                         start = NULL, end = NULL,
                         separator = " → ", language = "fr",
                         weekstart = 1, format = "dd/mm/yyyy"),
          h6("Produits (sélectionnez une ligne)", class = "section-sub"),
          DTOutput("detail_produit_liste")
        ),
        div(
          card_header(textOutput("detail_produit_titre", inline = TRUE)),
          plotlyOutput("detail_produit_graph", height = "280px"),
          div(class = "mt-3", DTOutput("detail_produit_table"))
        )
      )
    )
  )
}

# Drill-down "Par semaine" / "Par mois" de l'onglet Détail : même principe que
# "Par jour" — on clique une barre pour détailler la période.
# Périmètre : ce volet reste centré sur les VENTES (répartition, produits).
# Le résultat financier de la période (coûts, marge, KPI) est dans l'onglet
# Compta, qui offre la même granularité semaine / mois.
ui_detail_periode <- function(sfx) {
  id <- function(x) paste0("detail_", sfx, "_", x)

  tagList(
    layout_columns(
      fill = FALSE,
      col_widths = c(8, 4),
      dateRangeInput(id("periode"), "Période",
                     start = NULL, end = NULL,
                     separator = " → ", language = "fr",
                     weekstart = 1, format = "dd/mm/yyyy"),
      div(class = "small text-muted align-self-end pb-2",
          "Cliquez sur une barre pour détailler la période.")
    ),
    plotlyOutput(id("graph"), height = "300px"),
    legende_objectif(),
    hr(),
    card_header(textOutput(id("titre"), inline = TRUE)),
    layout_columns(
      col_widths = c(7, 5),
      div(
        h6("Répartition du CA sur la période", class = "section-sub"),
        plotlyOutput(id("repartition"), height = "260px"),
        layout_columns(
          col_widths = c(4, 8),
          uiOutput(id("box")),
          div(
            h6("Coût de la période", class = "section-sub"),
            uiOutput(id("prorata")),
            DTOutput(id("travail")),
            DTOutput(id("cout")),
            h6("Coûts et marge par secteur", class = "section-sub mt-2"),
            DTOutput(id("marge"))
          )
        ),
        uiOutput(id("kpi"))
      ),
      div(
        h6("Top produits de la période", class = "section-sub"),
        DTOutput(id("produits"))
      )
    )
  )
}

ui_maintenant <- function() {
  tagList(
    # --- Indicateurs clés ---
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      # uiOutput(inline = TRUE) et non textOutput : le titre porte désormais une
      # pastille d'unité, donc du HTML. bslib conserve la classe
      # value-box-title et le showcase (vérifié sur le rendu).
      value_box(uiOutput("title_vb_veille", inline = TRUE), textOutput("vb_ca_veille"),
                showcase = icon("calendar-day"), theme = "primary"),
      value_box(uiOutput("title_vb_semaine", inline = TRUE), textOutput("vb_ca_semaine"),
                showcase = icon("calendar-week"),
                theme = value_box_theme(bg = MZ_AMBRE, fg = "#ffffff")),
      value_box("Objectif de la semaine", textOutput("vb_pct_semaine"),
                showcase = icon("bullseye"),
                theme = value_box_theme(bg = "#efe7d8", fg = MZ_BRUN))
    ),
    # --- Veille ---
    # Sur petit écran, le total passe sous la semaine plutôt que d'être
    # comprimé dans une colonne de 2/12.
    card(
      full_screen = TRUE,
      card_header(textOutput("titre_veille", inline = TRUE)),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(10, 2)),
        uiOutput("box_semaine"),
        uiOutput("box_semaine_total")
      )
    ),
    # --- Semaines précédentes ---
    # Vue condensée : le CA de chaque jour, coloré selon son objectif. Le
    # détail complet est en infobulle, ce qui évite d'empiler cinq grilles.
    card(
      full_screen = TRUE,
      card_header("5 dernières semaines"),
      div(class = "table-responsive", uiOutput("recap_semaines")),
      legende_objectif()
    ),
    # --- Semaine en cours ---
    # card(
    #   card_header("Cette semaine"),
    #   
    #   div(class = "mt-2", style = "max-width: 230px;", uiOutput("box_semaine_total"))
    # ),
    card(
      card_header("Progression du mois vs objectif"),
      layout_columns(
        fill = FALSE,
        col_widths = c(4, 8),
        selectInput("prog_mois", "Mois", choices = NULL),
        uiOutput("prog_resume")
      ),
      layout_columns(
        fill = FALSE,
        col_widths = breakpoints(sm = 12, lg = c(10, 2)),
        plotlyOutput("prog_graph", height = "330px"),
        uiOutput("box_mois_total")
      )
    ),
    # --- Produits de la semaine ---
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Top produits de la semaine"), DTOutput("top_semaine")),
      card(card_header(span(icon("arrow-trend-up"), " En hausse vs S-1")), DTOutput("hausse_semaine")),
      card(card_header(span(icon("arrow-trend-down"), " En baisse vs S-1")), DTOutput("baisse_semaine"))
    )
  )
}

#### Coquille ####
# UI statique : navbar + grilles construites dès le chargement (toutes les
# dépendances bslib sont livrées au démarrage). Le login est un simple overlay
# masqué/affiché via shinyjs depuis le serveur — fini le swap renderUI, qui
# empêchait les dépendances (grille, navbar) d'arriver côté client.
#
# Seul l'accueil est livré avec la page. Les autres onglets sont insérés un à
# un après la connexion (nav_insert), ce qui laisse intacte la logique
# ci-dessus : la coquille bslib, elle, est bien construite au chargement.

# ui.R et server.R sont évalués par Shiny dans deux environnements frères :
# les constructeurs ui_*() définis ici sont invisibles depuis server.R, qui
# doit pourtant les appeler pour insérer un onglet. On les publie donc dans le
# registre prévu à cet effet (R/acces.R).
enregistre_constructeurs_onglets(environment())

ui <- page_fluid(
  # `title` alimente le <title> du document : c'est le libellé de l'onglet du
  # navigateur et le nom proposé par défaut à la mise en favori. À ne pas
  # confondre avec le `title` de navset_bar(), qui est la marque affichée dans
  # la barre de navigation.
  title = "Mazette",
  lang  = "fr",
  useShinyjs(),
  add_busy_spinner(spin = "fading-circle"),
  theme = theme_mazette_ui,
  entete_application(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css",
                      href = feuille_versionnee("style.css"))),
  div(id = "login_screen", ui_login()),
  shinyjs::hidden(div(id = "app_screen", ui_app()))
)
