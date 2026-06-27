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
      title = "Bières",
      icon = icon("beer-mug-empty"),
      ui_bieres()
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
      title = "Comparaison",
      icon = icon("code-compare"),
      ui_comparaison()
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
  tagList(
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      value_box("CA (HTVA)", textOutput("compta_vb_ca"),
                showcase = icon("cash-register"), theme = "primary"),
      value_box("Charges", textOutput("compta_vb_charges"),
                showcase = icon("file-invoice-dollar"),
                theme = value_box_theme(bg = MZ_AMBRE, fg = "#ffffff")),
      value_box("Profit", textOutput("compta_vb_profit"),
                showcase = icon("piggy-bank"),
                theme = value_box_theme(bg = "#5B7B5A", fg = "#ffffff")),
      value_box("Marge moyenne", textOutput("compta_vb_marge"),
                showcase = icon("percent"),
                theme = value_box_theme(bg = "#efe7d8", fg = MZ_BRUN))
    ),
    card(
      full_screen = TRUE,
      card_header("Compte de résultat simplifié"),
      layout_columns(
        fill = FALSE,
        col_widths = c(5, 7),
        dateRangeInput("compta_periode", "Période",
                       start = NULL, end = NULL,
                       separator = " → ", language = "fr",
                       weekstart = 1, format = "dd/mm/yyyy"),
        radioButtons("compta_unite", "Granularité",
                     c("Par semaine" = "semaine", "Par mois" = "mois",
                       "Par année" = "annee"),
                     selected = "mois", inline = TRUE)
      ),
      plotlyOutput("compta_graph", height = "360px"),
      div(class = "small text-muted",
          "Profit = CA HTVA − main d'œuvre − matières premières. ",
          "Coûts horaires et variations de stock fictifs ",
          "(cf. donnees_fictives_compta.R).")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Détail par période"),
           DTOutput("compta_table")),
      card(card_header("Répartition des charges par secteur"),
           plotlyOutput("compta_secteurs", height = "360px"))
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

ui_bieres <- function() {
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

ui_historique <- function() {
  tagList(
    card(
      card_header("Historique du chiffre d'affaires"),
      layout_columns(
        fill = FALSE,
        col_widths = c(5, 7),
        radioButtons("hist_unite", "Granularité",
                     c("Par semaine" = "semaine", "Par mois" = "mois"),
                     selected = "semaine", inline = TRUE)
      ),
      plotlyOutput("hist_graph", height = "330px"),
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
      hr(),
      card_header(textOutput("detail_jour_titre", inline = TRUE)),
      layout_columns(
        col_widths = c(2, 5, 5),
        uiOutput("detail_jour_box"),
        div(
          h6("Coût de la semaine", class = "section-sub"),
          DTOutput("detail_jour_travail"),
          DTOutput("detail_jour_cout")
        ),
        div(
          h6("Produits du jour", class = "section-sub"),
          DTOutput("detail_jour_produits")
        )
      )
    ),
    nav_panel(
      title = "Par produit",
      icon = icon("box"),
      layout_columns(
        col_widths = c(5, 7),
        div(
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
    card(
      full_screen = TRUE,
      card_header(textOutput("titre_veille", inline = TRUE)),
      layout_columns(
        col_widths = c(10, 2),
        # uiOutput("box_veille"),
        uiOutput("box_semaine"),
        uiOutput("box_semaine_total")
        # div(
        #   h6("Top produits du jour", class = "section-sub"),
        #   DTOutput("top_veille")
        # )
      )
    ),
    # --- Derniers jours ---
    card(
      full_screen = TRUE,
      card_header("5 dernières semaines"),
      layout_columns(
        col_widths = c(10, 2),
        uiOutput("box_semaine_avant"),
        uiOutput("box_semaine_total_avant")
      ),
      
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
        col_widths = c(10, 2),
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
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  div(id = "login_screen", ui_login()),
  shinyjs::hidden(div(id = "app_screen", ui_app()))
)
