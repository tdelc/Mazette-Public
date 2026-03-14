tabItem_events <- tabItem(tabName = "events",
                          fluidRow(width = 12,
                                   column(width = 6,
                                          box(width = 12, title = "Ventes moyennes selon les événements",
                                              status = "primary", solidHeader = TRUE,
                                              dataTableOutput("table_events_category")
                                          )
                                   ),
                                   column(width = 6,
                                          box(width = 12, title = "Cinq meilleurs événements",
                                              status = "primary", solidHeader = TRUE,
                                              dataTableOutput("table_events_max")
                                          ),
                                          box(width = 12, title = "Cinq derniers événements",
                                              status = "primary", solidHeader = TRUE,
                                              # dataTableOutput("table_events_jour")
                                              dataTableOutput("table_events_last")
                                          )
                                   )
                          ),
                          fluidRow(
                            box(width = 12, title = "Évolutions des ventes selon les événements",
                                status = "primary", solidHeader = TRUE,
                                plotlyOutput("graph_evo_events",height = "600px")
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