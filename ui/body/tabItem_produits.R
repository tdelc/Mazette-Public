tabItem_produits <- tabItem(tabName = "produits",
        fluidRow(
          box(width = 12, title = "Évolution des ventes des dernières semaines",
              status = "primary", solidHeader = TRUE,
              switchInput(
                inputId = "produits_evo",
                value = TRUE,
                onStatus = "success", 
                offStatus = "danger",
                label = "Unités",
                onLabel = "Nombre",
                offLabel = "CA",
                size = "small",
                width = "auto"
              ),
              h3("Catégorie par semaine"),
              DTOutput("table_category_evo"),
              h3("Produit par semaine"),
              DTOutput("table_produits_evo")
          )
        ),
        # fluidRow(
        #   box(width = 3, title = "Paramètre du graphique",
        #       status = "primary", solidHeader = TRUE,
        #       radioGroupButtons(
        #                 inputId = "produits_indic",
        #                 label = "Temporalité ?",
        #                 choices = c("Jour" = "DATE",
        #                             "Semaine" = "PREMIER_JOUR_SEMAINE",
        #                             "Mois" = "MOIS"),
        #                 selected = "PREMIER_JOUR_SEMAINE",
        #                 status = "primary",
        #                 individual = TRUE
        #               )
        #   ),
        #   box(width = 9, title = "Liste des produits",
        #       status = "primary", solidHeader = TRUE,
        #       textOutput("liste_produits"))
        # ),
        # fluidRow(
        #   box(width = 12, title = "Choix du produit et de la période",
        #       status = "primary", solidHeader = TRUE,
        #       sliderTextInput(
        #         inputId = "produits_periode",
        #         label = "Choisissez une période :", 
        #         choices = month.abb,
        #         selected = month.abb
        #       ),
        #       radioGroupButtons(
        #         inputId = "produits_indic",
        #         label = "Temporalité ?", 
        #         choices = c("Jour" = "DATE",
        #                     "Semaine" = "PREMIER_JOUR_SEMAINE",
        #                     "Mois" = "MOIS"),
        #         selected = "PREMIER_JOUR_SEMAINE",
        #         status = "primary",
        #         individual = TRUE
        #       ),
        #       textInput("produits_text","Tapez le produit souhaité"),
        #       checkboxGroupButtons(
        #         inputId = "category_list",
        #         label = "Catégories listées", 
        #         choices = "Chargement...",
        #         status = "danger"
        #       ),
        #       checkboxGroupButtons(
        #         inputId = "produits_list",
        #         label = "Produits listés", 
        #         choices = "Chargement...",
        #         status = "info"
        #       )
        #   )
        # ),
        fluidRow(
          box(width = 12, title = "Évolution des ventes", 
              status = "primary", solidHeader = TRUE,
              fluidRow(
                column(width = 3, 
                   radioGroupButtons(
                      inputId = "produits_indic",
                      label = "Temporalité ?",
                      choices = c("Jour" = "DATE",
                                  "Semaine" = "PREMIER_JOUR_SEMAINE",
                                  "Mois" = "MOIS"),
                      selected = "PREMIER_JOUR_SEMAINE",
                      status = "primary",
                      individual = TRUE
                    )
                ),
                column(width = 9, textOutput("liste_produits"))
              ),
              fluidRow(plotlyOutput("graph_CA_produits",height = 800)))
        ),
        fluidRow(
          box(width = 8, title = "Unités venduées des semaines précédentes",
              status = "primary", solidHeader = TRUE,
              plotOutput("table_produits_previous")),
          box(width = 2, title = "Cette semaine",
              status = "primary", solidHeader = TRUE,
              plotOutput("table_produits_actual"))
        )
)