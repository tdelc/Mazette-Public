tabItem_vacances <- tabItem(tabName = "vacances",
                                fluidRow(
                                  valueBoxOutput("vb_vacances_ca_hors", width = 3),
                                  valueBoxOutput("vb_vacances_ca_fr", width = 3),
                                  valueBoxOutput("vb_vacances_ca_nl", width = 3),
                                  valueBoxOutput("vb_vacances_ca_communes", width = 3)
                                ),
                                fluidRow(
                                  box(width = 12, title = "Comparaison des ventes moyennes par jour selon les congés",
                                      status = "primary", solidHeader = TRUE,
                                      plotlyOutput("graph_vacances_comparaison")
                                  )
                                ),
                                fluidRow(
                                  box(width = 12, title = "Détail des ventes par période de vacances",
                                      status = "primary", solidHeader = TRUE,
                                      dataTableOutput("table_vacances_detail")
                                  )
                                )
)
