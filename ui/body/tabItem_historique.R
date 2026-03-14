tabItem_historique <- tabItem(tabName = "historique",
                              fluidRow(
                                box(width = 2, title = "Personalisation",
                                    status = "primary", solidHeader = TRUE,
                                    radioGroupButtons(
                                      inputId = "LT_indic",
                                      label = "Temporalité ?", 
                                      choices = c("Jour" = "DATE",
                                                  "Semaine" = "PREMIER_JOUR_SEMAINE",
                                                  "Mois" = "MOIS"),
                                      selected = "MOIS",
                                      status = "primary",
                                      individual = TRUE
                                    ),
                                    materialSwitch(
                                      inputId = "LT_flag", value = FALSE,
                                      status = "primary",
                                      label = "Classement selon nombre de jours de ventes ?"
                                    )
                                ),
                                box(width = 2, title = "Statistiques",
                                    status = "primary", solidHeader = TRUE,
                                    dataTableOutput("stats_LT")
                                ),
                                box(width = 4, title = "Meilleurs",
                                    status = "primary", solidHeader = TRUE,
                                    dataTableOutput("meilleurs_LT")
                                ),
                                box(width = 4, title = "Pires",
                                    status = "primary", solidHeader = TRUE,
                                    dataTableOutput("pires_LT")
                                )
                                
                              ),
                              fluidRow(
                                box(width = 12, title = "Évolution des ventes",
                                    status = "primary", solidHeader = TRUE,
                                    plotlyOutput("vente_LT",height = 600)
                                )
                              )
)