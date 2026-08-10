tabItem_evo_nourriture <- tabItem(
  tabName = "evo_nourriture",
  fluidRow(
    box(width = 12,title = "Ventes du salé",
        status = "primary",solidHeader = TRUE,
        dataTableOutput("nourriture_evo_sales")
    )
  ),
  fluidRow(
    box(width = 12,title = "Ventes du sucré",
        status = "primary",solidHeader = TRUE,
        dataTableOutput("nourriture_evo_sucres")
    )
  )
)
