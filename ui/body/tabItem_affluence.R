tabItem_affluence <- tabItem(tabName = "affluence",
        fluidRow(
          box(width = 12, title = "Choix de la période",
              status = "primary", solidHeader = TRUE,
              sliderTextInput(
                inputId = "affluence_periode",
                label = "Choisissez une période :", 
                choices = month.abb,
                selected = month.abb
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "Affluence par jour d'ouverture", 
              status = "primary", solidHeader = TRUE,
              plotOutput("graph_affluence",height = 800))
        )
)