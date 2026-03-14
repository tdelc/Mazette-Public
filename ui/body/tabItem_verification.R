tabItem_verification <- tabItem(tabName = "verification",
                                fluidRow(
                                  box(width = 12, title = "Personalisation",
                                      status = "primary", solidHeader = TRUE,
                                      radioGroupButtons(
                                        inputId = "verification_db",
                                        label = "CHoisissez les bases de données à analyser",
                                        choices = c("DB_CAISSE","DB_BIERES",
                                                    "DB_BRASSINS","DB_DATE",
                                                    "DB_EVENTS","DB_HOREKO",
                                                    "DB_JOURS","DB_OBJECTIFS",
                                                    "DB_PRODUITS","DB_TICKET"),
                                        status = "primary",
                                        justified = TRUE,
                                        checkIcon = list(
                                          yes = icon("ok", lib = "glyphicon"),
                                          no = icon("remove", lib = "glyphicon")
                                          )
                                      ),
                                      radioGroupButtons(
                                        inputId = "verification_db_vars",
                                        label = "CHoisissez la variable de classification",
                                        choices = c("var"),
                                        status = "primary",
                                        justified = TRUE,
                                        checkIcon = list(
                                          yes = icon("ok", lib = "glyphicon"),
                                          no = icon("remove", lib = "glyphicon")
                                        )
                                      )
                                  )
                                ),
                                fluidRow(
                                  box(width = 12, title = "Analyse de chaque variable",
                                      status = "primary", solidHeader = TRUE,
                                      uiOutput("verification_info",fill=TRUE)
                                  )
                                ),
                               fluidRow(
                                 box(width = 12, title = "Base de données complète",
                                     status = "primary", solidHeader = TRUE,
                                     dataTableOutput("verification_table_full")
                                     )
                               )
)