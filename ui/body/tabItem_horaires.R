tabItem_horaires <- tabItem(tabName = "horaires",
        fluidRow(
          box(width = 12, title = "Configuration",
              status = "primary", solidHeader = TRUE,
              column(width = 1,
              numericInput("config_horaires_nb",min=2,max=12,
                           label = NULL,value = 8)),
              column(width = 1,h4("dernières")),
              column(width = 2,
                     radioGroupButtons(
                       inputId = "config_horaires_tempo",
                       label = NULL, 
                       choices = c("Semaines","Mois","Années"),
                       selected = "Semaines",status = "primary",
                       individual = TRUE)
              ),
              column(width = 3,
                     textInput("config_horaires_text","Se limiter à un produit")
              ),
              column(width = 3,
                     sliderInput("config_horaires_ref",min=0,max=8,value = 0,
                                 label = "Semaine en pointillé")
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "Chiffre d'affaires par heure",
              status = "primary", solidHeader = TRUE,
              plotOutput("graph_horaires_CA",height = 800)
          )
        )
)
