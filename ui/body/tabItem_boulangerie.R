tabItem_boulangerie <- tabItem(tabName = "boulangerie",
                               fluidRow(
                                 box(width = 10, title = "Ventes de focaccia des semaines précédentes",
                                     status = "primary", solidHeader = TRUE,
                                     plotOutput("table_focaccia_previous")),
                                 box(width = 2, title = "Cette semaine",
                                     status = "primary", solidHeader = TRUE,
                                     plotOutput("table_focaccia_actual"))
                               ),
                               fluidRow(
                                 box(width = 12, title = "Ventes de focaccia par type",
                                     status = "primary", solidHeader = TRUE,
                                     column(width = 6,plotOutput("boulangerie_plot_focaccia")),
                                     column(width = 6,tableOutput("boulangerie_table_focaccia"))
                                 )
                               ),
                               # fluidRow(
                               #   box(width = 8, title = "Ventes de dikkebroodjes des semaines précédentes",
                               #       status = "primary", solidHeader = TRUE,
                               #       plotOutput("table_dik_previous")),
                               #   box(width = 2, title = "Cette semaine",
                               #       status = "primary", solidHeader = TRUE,
                               #       plotOutput("table_dik_actual"))
                               # ),
                               fluidRow(
                                 box(width = 8, title = "Ventes de brunch et brioches des semaines précédentes",
                                     status = "primary", solidHeader = TRUE,
                                     plotOutput("table_brunch_previous")),
                                 box(width = 2, title = "Cette semaine",
                                     status = "primary", solidHeader = TRUE,
                                     plotOutput("table_brunch_actual"))
                               )
                               
)