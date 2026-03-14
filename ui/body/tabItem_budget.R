tabItem_budget <- tabItem(
  tabName = "budget",
  fluidRow(
    box(width = 12, title = "Pourcentage du chiffres d'affaires atteint",
        status = "primary", solidHeader = TRUE,
      fluidRow(
        column(width=2,h4("Ce mois"),gaugeOutput("gauge_month",height = "150px")),
        column(width=2,h4("Ce trimestre"),gaugeOutput("gauge_quarter",height = "150px")),
        column(width=2,h4("Cette année"),gaugeOutput("gauge_year",height = "150px"))
      ),
      fluidRow(
        column(width=2,uiOutput("gauge_month_details")),
        column(width=2,uiOutput("gauge_quarter_details")),
        column(width=2,uiOutput("gauge_year_details"))
      )
    )
  ),
  fluidRow(width = 12,
           box(width = 12, title = "Comparaison entre le budget et le réalisé",
               status = "primary", solidHeader = TRUE,
               plotlyOutput("graph_evo_ecart_budget",height = "600px")
           )
  ),
  fluidRow(width = 12,
           box(width = 12, title = "Comparaison entre le réalisé de l'année et de l'année précédente (même jour et numéro de semaine)",status = "primary", solidHeader = TRUE,
               plotlyOutput("graph_evo_ecart_ym1",height = "600px")
           )
  ),
  fluidRow(width = 12,
           box(width = 12, title = "Panorama de l'année",status = "primary", solidHeader = TRUE,
               radioGroupButtons("year_panorama",choices = "..."),
               plotOutput("graph_evo_annee_complete",height = "600px")
           )
  )
)
