tabItem_ventes <- tabItem(tabName = "ventes",
                          fluidRow(
                            box(width = 8, title = "Ventes des semaines précédentes",
                                status = "primary", solidHeader = TRUE,
                                plotOutput("table_ventes_previous")),
                            
                            box(width = 2, title = "Cette semaine",
                                status = "primary", solidHeader = TRUE,
                                plotOutput("table_ventes_actual")),
                            
                            box(width = 2, title = "Objectifs cette semaine",
                                status = "primary", solidHeader = TRUE,
                                plotOutput("table_objectifs")
                            )
                          ),
                          
                          fluidRow(
                            box(width = 12, title = "Évolution des ventes par semaine",
                                status = "primary", solidHeader = TRUE,
                                plotlyOutput("graph_ventes"))
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