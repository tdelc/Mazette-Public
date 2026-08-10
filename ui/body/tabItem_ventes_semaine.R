tabItem_ventes_semaine <- tabItem(
  tabName = "ventes_semaine",
  fluidRow(
    box(width = 12, title = "Ventes des 7 semaines passées",
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        column(width = 12, uiOutput("box_ventes_7_semaines"))
    )
  ),
  fluidRow(
    box(width = 12, title = "Évolution des ventes par semaine",
        status = "primary", solidHeader = TRUE,
        plotlyOutput("graph_ventes"))
  )
)