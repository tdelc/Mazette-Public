tabItem_db <- tabItem(tabName = "db",
                      fluidRow(
                        column(width = 6,
                               box(title = "Table DB_JOURS", width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   p("Une ligne par jour d'ouverture de Mazette. Reprends toutes les ventes et TVA, à la fois à la caisse mais aussi avec l'ajout des factures. Avec le détail de moyen de paiement"),
                                   downloadButton("dl_DB_JOURS","Télécharger DB JOURS"))
                               
                        ),
                        column(width = 6,
                               box(title = "Table DB BIERES", width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   p("Une ligne par brassin * jour de ventes. Permet de savoir, pour une date données, le volume vendue et CA de cette bière."),
                                   downloadButton("dl_DB_BIERES","Télécharger DB BIERES"))
                               
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               box(title = "Table DB TICKET", width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   p("Une ligne par produits * client * jour de vente. Peu d'usage actuellement, permettra d'étudier les tables et les clients."),
                                   downloadButton("dl_DB_TICKET","Télécharger DB TICKET"))
                               
                        ),
                        column(width = 6,
                               box(title = "Table DB PRODUITS", width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   p("Une ligne par produits * jour de vente. Permet d'obtenir toutes les chiffres de ventes et de quantité pour chaque produit vendu."),
                                   downloadButton("dl_DB_PRODUITS","Télécharger DB PRODUITS"))
                               
                        )
                      ),
                      fluidRow(
                        column(width = 6,
                               box(title = "Table DB HOREKO", width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   p("Une ligne par jour d'ouverture. Reprends toutes les données Horeko concernant les heures de travail"),
                                   downloadButton("dl_DB_HOREKO","Télécharger DB HOREKO"))
                               
                        )
                      )
)