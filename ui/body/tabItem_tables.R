tabItem_tables <- tabItem(tabName = "tables",
                          fluidRow(
                            box(width = 12, title = "Évolution du nombre de tables",
                                status = "primary", solidHeader = TRUE,
                                plotlyOutput("tables_LT",height = 600)
                            )
                          ),
                          fluidRow(
                            box(width = 12, title = "Évolution des ventes par table",
                                status = "primary", solidHeader = TRUE,
                                plotlyOutput("CA_tables_LT",height = 600)
                            )
                          )
)