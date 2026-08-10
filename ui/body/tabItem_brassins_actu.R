tabItem_brassins_actu <- tabItem(tabName = "brassins_actu",
                                 # fluidRow(
                                 #   box(width = 3, title = "Jour de prédiction",
                                 #       status = "primary", solidHeader = TRUE,
                                 #       dateInput("predict_date","Choix du jour",
                                 #                 value = today(),language = "fr"))
                                 # ),
                                 fluidRow(
                                   box(width = 12, title = "Bières en vente et prédiction de fin",
                                       status = "primary", solidHeader = TRUE,
                                       do.call(splitLayout, c(lapply(1:5, function(i) {
                                         column(width=12,
                                                h4(span(
                                                  textOutput(paste0("label_", i)),
                                                  textOutput(paste0("predict_", i)))),
                                                gaugeOutput(paste0("gauge_", i))
                                         )
                                       }), cellWidths = "20%")),
                                       do.call(splitLayout, c(lapply(6:10, function(i) {
                                         column(width=12,
                                                h4(span(
                                                  textOutput(paste0("label_", i)),
                                                  textOutput(paste0("predict_", i)))),
                                                gaugeOutput(paste0("gauge_", i))
                                         )
                                       }), cellWidths = "20%"))
                                   )
                                 ),
                                 fluidRow(
                                   box(width = 12, title = "Prédiction de fin des bières",
                                       status = "primary", solidHeader = TRUE,
                                       dataTableOutput("table_biere_cours")
                                   )),
                                 fluidRow(
                                   box(width = 12, title = "Évolutions des bières actuelles",
                                       status = "primary", solidHeader = TRUE,
                                       sliderInput("max_predict","Date de la prédiction",today(),
                                                   min=today(),max=today()+100),
                                       checkboxGroupButtons(
                                         inputId = "bieres_predict",
                                         label = "Bières à afficher", 
                                         choices = "Chargement...",
                                         status = "info"
                                       ),
                                       column(width = 12,
                                              plotlyOutput(paste0("graph_biere_TOT"),height = "600px")
                                       )
                                   )
                                 ),
                                 fluidRow(
                                   box(width = 12, title = "Analyse de qualité des prédictions",
                                       status = "primary", solidHeader = TRUE,
                                       actionButton("check_quali","Lancement du calcul (long...)"),
                                       column(width = 12,
                                              plotlyOutput("graph_quali_predict",height = "600px")
                                       )
                                   )
                                 )
)