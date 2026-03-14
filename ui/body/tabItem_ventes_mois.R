tabItem_ventes_mois <- tabItem(
  tabName = "ventes_mois",
  fluidRow(
    box(width = 12, title = "Ventes des mois passés",
        status = "primary", solidHeader = TRUE,
        column(width = 12, uiOutput("box_ventes_7_mois"))
    )
  ),
  fluidRow(
    box(width = 12, title = "Progression de l'objectif du mois", 
        status = "primary", solidHeader = TRUE,
        shinyWidgets::progressBar(id = "month_progress", value = 0, 
                                  status = "success", size = "lg", 
                                  striped = TRUE,display_pct  = TRUE))
    
  ),
  fluidRow(
    box(width = 12, title = "Évolution des ventes par mois",
        status = "primary", solidHeader = TRUE,
        plotlyOutput("graph_ventes_mois",height = 600)
    )
  )
)