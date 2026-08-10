tabItem_chatGPT <- tabItem(tabName = "chat_gpt",
                          fluidRow(width = 12,
                                    box(width = 12, title = "Dialogue avec Chat GPT",
                                        status = "primary", solidHeader = TRUE,
                                        
                                        column(width = 4,
                                               h4("Pose ta question ici"),
                                               textAreaInput("chat_question",label = NULL,height = "100px"),
                                               actionButton("chat_send","Send")
                                               ),
                                        
                                        column(width = 4,
                                               h4("Le code proposé"),
                                               htmlOutput("chat_code")
                                               # codeOutput("chat_code")
                                        ),
                                        
                                        column(width = 4,
                                               h4("Explication du code"),
                                               htmlOutput("chat_reponse")
                                        )
                                    )
                          ),
                          fluidRow(width = 12,
                                   box(width = 12, title = "Écriture du code",
                                       status = "primary", solidHeader = TRUE,
                                       
                                       column(width = 6,
                                              h4("Ecris ton code R ici"),
                                              textAreaInput("console_input",label = NULL,height = "300px")
                                       ),
                                       column(width = 6,
                                              h4("Réponse de R"),
                                              verbatimTextOutput("console_output_verbatim")
                                       )
                                   )
                          ),
                          fluidRow(width = 12,
                                   box(width = 12, title = "Tableau ou Graphique",
                                       status = "primary", solidHeader = TRUE,
                                       dataTableOutput("console_output_table"),
                                       plotOutput("console_output_plot")
                                   )
                          )
)