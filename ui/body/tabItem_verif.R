tabItem_verif <- tabItem(
  tabName = "verif",
  fluidRow(
    box(width = 12, title = "Dernières lignes",
        status = "primary", solidHeader = TRUE,
        uiOutput("table_verif_lignes")
    )
  )
)