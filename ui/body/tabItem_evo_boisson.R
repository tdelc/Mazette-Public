tabItem_evo_boisson <- tabItem(
  tabName = "evo_boisson",
  fluidRow(
    box(width = 12,title = "Ventes des bières",
        status = "primary",solidHeader = TRUE,
        dataTableOutput("boisson_evo_bieres")
    )
  ),
  fluidRow(
    box(width = 12,title = "Ventes des softs",
        status = "primary",solidHeader = TRUE,
        dataTableOutput("boisson_evo_softs")
    )
  ),
  fluidRow(
    box(width = 12,title = "Ventes des alcools",
        status = "primary",solidHeader = TRUE,
        dataTableOutput("boisson_evo_alcools")
    )
  ),
  fluidRow(
    box(width = 12,title = "Ventes des boissons chaudes",
        status = "primary",solidHeader = TRUE,
        dataTableOutput("boisson_evo_chaud")
    )
  )
)
