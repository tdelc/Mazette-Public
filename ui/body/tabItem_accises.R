tabItem_accises <- tabItem(tabName = "accises",
                           fluidRow(
                             box(width = 12, title = "Listing des brassins finis",
                                 status = "primary", solidHeader = TRUE,
                                 dataTableOutput("table_brassins_fini",height = 600)
                             )
                           )
)