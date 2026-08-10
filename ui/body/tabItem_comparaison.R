tabItem_comparaison <- tabItem(tabName = "comparaison",
                               fluidRow(
                                 box(width = 3, title = "Mois de référence",
                                     status = "primary", solidHeader = TRUE,
                                     dateInput("compa_date","Choix de la date de référence",
                                               value = today(),format = "MM yyyy",
                                               language = "fr", startview = "year"))
                               ),
                               fluidRow(
                                 box(width = 12, title = "Comparaison des mois de ventes",
                                     status = "primary", solidHeader = TRUE,
                                     dataTableOutput("table_compa_mois")
                                 )
                               )
                               # ,
                               # fluidRow(
                               #   box(width = 12, title = "Comparaison des trois derniers mois par catégorie",
                               #       status = "primary", solidHeader = TRUE,
                               #       dataTableOutput("table_compa_mois_category")
                               #   )
                               # )
)