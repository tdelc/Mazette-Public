tabItem_ventes <- tabItem(
  tabName = "ventes",
  fluidRow(
    box(width = 12, title = "Ventes de cette semaine",
        status = "primary", solidHeader = TRUE,
        fluidRow(
          column(width = 10, uiOutput("box_ventes_semaine")),
          column(width = 2, uiOutput("box_ventes_semaine_total"))
        )
      )
  ),
  fluidRow(
    box(width = 12, title = "Ventes des 5 semaines passées",
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        column(width = 10, uiOutput("box_ventes_semaine_m1")),
        column(width = 2, uiOutput("box_ventes_semaine_m1_total"))
    )
  ),
  fluidRow(
    box(width = 12, title = "Ventes moyenne des 8 semaines passées",
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        column(width = 10, uiOutput("box_ventes_semaines"))
    )
  ),
  fluidRow(
    box(width = 12, title = "Comparaison avec une semaine au choix",
        status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        dateInput("ventes_date","Choix de la semaine de référence",
                  value = today(),format = "dd MM yyyy",
                  language = "fr", startview = "year"),
        br(),
        column(width = 10, uiOutput("box_ventes_semaine_mx")),
        column(width = 2, uiOutput("box_ventes_semaine_mx_total"))
    )
  ),
  fluidRow(
    box(width = 12, title = "Évolution des ventes par mois",
        status = "primary", solidHeader = TRUE,
        plotlyOutput("graph_ventes_mois",height = 600)
    )
  )
)