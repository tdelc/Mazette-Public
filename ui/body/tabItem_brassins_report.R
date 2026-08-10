tabItem_brassins_report <- tabItem(tabName = "brassins_report",
                                 fluidRow(
                                   box(width = 12, title = "Analyse d'un brassin",
                                       status = "primary", solidHeader = TRUE,
                                       selectInput("report_choice","Choix du brassins",NULL),
                                       plotOutput("report",width = "80%",height = "800px")
                                   )
                                 )
)
