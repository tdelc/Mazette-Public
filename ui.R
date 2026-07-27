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

ui_login <- function() {
  div(
    class = "login-wrap",
    card(
      class = "login-card",
      card_header(span(class = "brand-title", "MAZETTE")),
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
    chip_legende("#5B7B5A", "Objectif atteint"),
    chip_legende("#d98236", "À partir de 90 %"),
    chip_legende("#c0392b", "En dessous de 90 %")
  )
}

legende_couleurs <- function() {
  tagList(
    div(class = "legende-titre", "Midi / Soir"),
    chip_legende("#e67e22", "Midi (< 17h)"),
    chip_legende("#9b59b6", "Soir (≥ 17h)"),
    div(class = "legende-titre", "Boisson / Nourriture"),
    chip_legende("#d4ac0d", "Boisson"),
    chip_legende("#27ae60", "Nourriture"),
    div(class = "legende-titre", "Semaine / Week-end"),
    chip_legende("#2980b9", "Semaine"),
    chip_legende("#c0392b", "Week-end")
  )
}

ui_app <- function() {
  navset_bar(
    title = span(class = "brand-title", "MAZETTE"),
    id = "nav",
    fillable = FALSE,
    nav_panel(
      title = "Maintenant",
      icon = icon("gauge-high"),
      ui_maintenant()
    ),
    nav_panel(
      title = "Détail",
      icon = icon("magnifying-glass-chart"),
      ui_detail()
    ),
    nav_panel(
      title = "Historique",
      icon = icon("chart-line"),
      ui_historique()
    ),
    nav_panel(
      title = "Année",
      icon = icon("calendar-check"),
      ui_annee()
    ),
    nav_panel(
      title = "Fûts",
      icon = icon("boxes-stacked"),
      ui_futs()
    ),
    nav_panel(
      title = "Bières",
      icon = icon("beer-mug-empty"),
      ui_conso_bieres()
    ),
    nav_panel(
      title = "Focaccias",
      icon = icon("bread-slice"),
      ui_focaccias()
    ),
    nav_panel(
      title = "Pizzwanze",
      icon = icon("pizza-slice"),
      ui_pizzwanze()
    ),
    nav_panel(
      title = "Simulation",
      icon = icon("sliders"),
      ui_simulation()
    ),
    nav_panel(
      title = "Compta",
      icon = icon("calculator"),
      ui_compta()
    ),
    nav_panel(
      title = "Travail",
      icon = icon("person-running"),
      ui_travail()
    ),
    nav_panel(
      title = "Comparaison",
      icon = icon("code-compare"),
      ui_comparaison()
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
              " coûts indirects. Reste à couvrir matières, loyer et énergie.")
        ),
        uiOutput("trav_kpi"),
        card(
          full_screen = TRUE,
          card_header("Décomposition du CA : marge et coûts du travail"),
          plotlyOutput("trav_structure", height = "340px")
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

ui_comparaison <- function() {
  tagList(
    card(
      full_screen = TRUE,
      card_header("Comparer des périodes"),
      layout_columns(
        fill = FALSE,
        col_widths = c(4, 8),
        radioButtons("comp_unite", "Comparer par",
                     c("Semaine" = "semaine", "Mois" = "mois", "Année" = "annee"),
                     selected = "mois", inline = TRUE),
        selectizeInput("comp_periodes", "Périodes à comparer",
                       choices = NULL, multiple = TRUE, width = "100%",
                       options = list(placeholder = "Choisir des périodes…"))
      ),
      plotlyOutput("comp_graph", height = "380px"),
      div(class = "small text-muted",
          "Ventes réalisées vs objectif, et profit (compta fictive), ",
          "pour chaque période sélectionnée.")
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
      title = "Par semaine",
      icon = icon("calendar-week"),
      ui_compta_volet("sem")
    ),
    nav_panel(
      title = "Par mois",
      icon = icon("calendar-days"),
      ui_compta_volet("mois")
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
          "Coûts des matières fictifs (cf. donnees_fictives_compta.R).")
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
      card_header("Marge cumulée"),
      plotlyOutput("annee_marge", height = "340px"),
      div(class = "small text-muted",
          "Marge quotidienne = CA − personnel du jour − matières de la semaine ",
          "réparties sur 7 jours.",tags$br(),
          "La marge est disponible que pour les dates où les coûts sont ",
          "disponibles.")
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
    card(
      card_header("Paramètres"),
      layout_columns(
        fill = FALSE,
        col_widths = c(4, 3, 2, 3),
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
          " dans la colonne « Prix simulé » du tableau.")
    ),
    card(
      card_header("Produits dont le prix a changé"),
      DTOutput("sim_table_diff")
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
      plotlyOutput("bieres_niveaux", height = "500px")
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

# Onglet "Bières" : consommation, à la semaine, comparée à S-1.
ui_conso_bieres <- function() {
  layout_sidebar(
    sidebar = sidebar(
      title = "Semaine",
      width = 280,
      selectInput("conso_semaine", "Semaine analysée", choices = NULL),
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
           card_header("Top bières — litres servis vs S-1"),
           plotlyOutput("conso_top", height = "420px")),
      card(full_screen = TRUE,
           card_header("Tendance des principales bières"),
           plotlyOutput("conso_tendance", height = "420px"),
           div(class = "small text-muted",
               "Les 5 bières les plus servies cette semaine, suivies sur ",
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
      card_header("Détail par bière"),
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
           card_header("La carte du soir"),
           plotlyOutput("pizz_soiree", height = "380px")),
           # div(class = "small text-muted",
           #     "Couleur selon la présence historique de la pizza.")),
      card(full_screen = TRUE,
           card_header("Rythme de la soirée"),
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
      div(class = "small text-muted",
          textOutput("prod_source", inline = TRUE)),
      actionButton("prod_reset", "↺ Réinitialiser", class = "btn-sm")
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
           card_header("Rythme de la semaine"),
           plotlyOutput("foca_jour", height = "360px"),
           div(class = "small text-muted",
               "En pointillé : la même semaine, une semaine plus tôt.")),
      card(full_screen = TRUE,
           card_header("Variantes les plus commandées"),
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
      card_header("Détail par variante"),
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
    nav_panel(
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
          DTOutput("detail_jour_cout"),
          uiOutput("detail_jour_simu")
        ),
        div(
          h6("Produits du jour", class = "section-sub"),
          DTOutput("detail_jour_produits")
        )
      )
    ),
    nav_panel(
      title = "Par semaine",
      icon = icon("calendar-week"),
      ui_detail_periode("sem")
    ),
    nav_panel(
      title = "Par mois",
      icon = icon("calendar-days"),
      ui_detail_periode("mois")
    ),
    nav_panel(
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
            DTOutput(id("travail")),
            DTOutput(id("cout")),
            uiOutput(id("simu"))
            # ,h6("Marge de la période", class = "section-sub")
            # ,DTOutput(id("marge"))
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
      value_box("CA de la veille", textOutput("vb_ca_veille"),
                showcase = icon("calendar-day"), theme = "primary"),
      value_box("CA de la semaine", textOutput("vb_ca_semaine"),
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

ui <- page_fluid(
  useShinyjs(),
  theme = theme_mazette_ui,
  tags$head(tags$link(rel = "stylesheet", type = "text/css",
                      href = feuille_versionnee("style.css"))),
  div(id = "login_screen", ui_login()),
  shinyjs::hidden(div(id = "app_screen", ui_app()))
)
