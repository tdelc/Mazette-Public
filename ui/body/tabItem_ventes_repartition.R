tabItem_ventes_repartition <- tabItem(tabName = "ventes_repartition",
                                      fluidRow(
                                        box(width = 12, title = "Période à analyser",
                                            status = "primary", solidHeader = TRUE,
                                            column(width = 6,
                                                   dateInput("ventes_repartition_date_min","Date minimale")),
                                            column(width = 6,
                                                   dateInput("ventes_repartition_date_max","Date maximale")
                                            )
                                        )
                                      ),
                          fluidRow(
                            box(width = 12, title = "Ventes (TVAC) selon le moment de la journée",
                                status = "primary", solidHeader = TRUE,
                                dataTableOutput("table_ventes_repartition_CA"))
                          ),
                          fluidRow(
                            box(width = 6, title = "Nombre d'items à midi",
                                status = "primary", solidHeader = TRUE,
                                dataTableOutput("table_ventes_items_midi")
                                # ,plotOutput("graph_ventes_items_midi",height = "800px")
                                ),
                            box(width = 6, title = "Nombre d'items au soir",
                                status = "primary", solidHeader = TRUE,
                                dataTableOutput("table_ventes_items_soir")
                                # ,plotOutput("graph_ventes_items_soir",height = "800px")
                                )
                          ),
                          fluidRow(
                            box(width = 12,title = "Nombre d'items sur la période de la journée",
                                status = "primary", solidHeader = TRUE,
                                plotOutput("graph_ventes_items",height = "800px")
                                )
                          )
)