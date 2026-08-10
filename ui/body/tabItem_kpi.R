tabItem_kpis <- tabItem(tabName = "kpis",
                          fluidRow(
                            box(width = 12, title = "Ventes et objectifs",
                                status = "primary", solidHeader = TRUE,
                                fluidRow(
                                  column(width=2,h4("Semaine précédente"),gaugeOutput("kpis_week_m1",height = "150px")),
                                  column(width=2,h4("Mois précédent"),gaugeOutput("kpis_month_m1",height = "150px")),
                                  column(width=2,h4("Trimestre précédent"),gaugeOutput("kpis_quarter_m1",height = "150px"))
                                ),
                                fluidRow(
                                column(width=2,h4("Cette semaine"),gaugeOutput("kpis_week",height = "150px")),
                                column(width=2,h4("Ce mois"),gaugeOutput("kpis_month",height = "150px")),
                                column(width=2,h4("Ce trimestre"),gaugeOutput("kpis_quarter",height = "150px")),
                                column(width=2,h4("Cette année"),gaugeOutput("kpis_year",height = "150px"))
                                ),
                                fluidRow(
                                  column(width=2,tableOutput("kpis_details_week")),
                                  column(width=2,tableOutput("kpis_details_month")),
                                  column(width=2,tableOutput("kpis_details_quarter")),
                                  column(width=2,tableOutput("kpis_details_year"))
                                )
                            )
                          ),
                        fluidRow(
                          box(width = 12, title = "Produit avec la plus forte différence",
                              status = "primary", solidHeader = TRUE,
                            dataTableOutput("table_forte_difference")
                          )
                        )
                        
)
