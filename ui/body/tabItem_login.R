tabItem_login <- tabItem(tabName = "login",
                         fluidRow(
                           box(width = 4, title = "Connexion",
                               status = "primary", solidHeader = TRUE,
                               div(h5(textOutput("info_log")), style = "color:red"),
                               br(),
                               passwordInput("password", "Mot de passe", ""),
                               actionButton("boutton_log", "Go!"),
                               textOutput("text_log")
                           )
                         )
)