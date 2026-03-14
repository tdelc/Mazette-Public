tabItem_heures <- tabItem(tabName = "heures",
                          fluidRow(
                            box(width = 4,title = "Sélection du mois",
                                status = "primary",solidHeader = TRUE,
                                dateInput("heures_date","Choix de la date de référence",
                                          value = today(),format = "MM yyyy",
                                          language = "fr", startview = "year")
                            )
                          ),
                          fluidRow(
                            box(width = 4, title = "Heures au détail",
                                status = "primary", solidHeader = TRUE,
                                tableOutput("heures_detail")
                            ),
                            box(width = 4, title = "Heures par secteur et par personne",
                                status = "primary", solidHeader = TRUE,
                                tableOutput("heures_service"),
                                tableOutput("heures_personne")
                            ),
                            box(width = 4, title = "Coût du travail selon la compta",
                                status = "primary", solidHeader = TRUE,
                                tableOutput("heures_compta"),
                                tableOutput("heures_compta_detail")
                            )
                          ),
                          fluidRow(
                            box(width = 12, title = "Évolution du coût du travail par mois",
                                status = "primary", solidHeader = TRUE,
                                plotlyOutput("evo_heures",height = 600)
                            )
                          )
)