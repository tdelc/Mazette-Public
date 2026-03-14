tabItem_compta <- tabItem(tabName = "compta",
                          fluidRow(width = 12,
                                   column(width = 6,
                                          box(width = 12, title = "Choix du code comptable",
                                              status = "primary", solidHeader = TRUE,
                                              prettyRadioButtons("compta_code","Choix du code",
                                                           choices = "70",status = "info",
                                                           inline = TRUE)
                                          )
                                   )
                          ),
                          fluidRow(
                            box(width = 12, title = "Évolution du compte",
                                status = "primary", solidHeader = TRUE,
                                plotlyOutput("graph_evo_comptes",height = "600px")
                            )
                          )
                          # ,
                          # fluidRow(
                          #   box(width = 12, title = "Prédiction des ventes",
                          #       status = "primary", solidHeader = TRUE,
                          #       plotOutput("predict_event_ca",height = "1000px")
                          #   )
                          # )
)