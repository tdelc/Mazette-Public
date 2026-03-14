tabItem_clients <- tabItem(tabName = "clients",
                           fluidRow(
                             box(width = 12, title = "Évolution du nombre de clients",
                                 status = "primary", solidHeader = TRUE,
                                 plotlyOutput("clients_LT",height = 600)
                             )
                           ),
                           fluidRow(
                             box(width = 12, title = "Évolution des ventes par client",
                                 status = "primary", solidHeader = TRUE,
                                 plotlyOutput("CA_clients_LT",height = 600)
                             )
                           )
)