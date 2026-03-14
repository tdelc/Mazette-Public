tabItem_brassins_fini <- tabItem(tabName = "brassins_fini",
                                 fluidRow(
                                   box(width = 12, title = "Catégorisation des bières",
                                       status = "primary", solidHeader = TRUE,
                                       h5("Les bières terminées sont classées en 4 catégories selon le nombre de litres vendus par jour et le pourcentage du fut terminé. Les bières actuelles sont en violet et peuvent du coup être comparés à celles terminées."),
                                       column(width = 12,
                                              plotlyOutput("graph_cluster_bieres",height = "800px")
                                       )
                                   )
                                 )
)