tabItem_simulation <- tabItem(tabName = "simulation",
        fluidRow(
          box(width = 12, title = "Simulation des variations de prix",
              status = "primary", solidHeader = TRUE,
              actionButton("reset_simulation", "↺ Reset tous les prix"),
              dateRangeInput("date_simulation", "Choix de la période d'analyse :",
                             start = "2026-01-01",
                             end   = "2026-03-01"),
              fluidRow(
                uiOutput("vb_actuel"),
                uiOutput("vb_simule"),
                uiOutput("vb_delta")
              ),
              fluidRow(
                column(width = 3,selectInput("select_category_simulation","Catégorie à modifier",NULL)),
                column(width = 1,numericInput("pc_category_simulation","% de variation",0,-100,100,1)),
                column(width = 2,actionButton("apply_category_simulation","Appliquer la variation"))
              ),
              br(),
              h3("Produits dont le prix a été modifié"),
              DTOutput("table_simulation_diff"),
              br(),
              h3("Tous les produits"),
              DTOutput("table_simulation")
          )
        )
)
