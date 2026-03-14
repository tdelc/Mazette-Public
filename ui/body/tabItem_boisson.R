tabItem_boisson <- tabItem(
  tabName = "boisson",
  # Sélecteur du mois
  fluidRow(
    box(width = 4,title = "Sélection du mois",
        status = "primary",solidHeader = TRUE,
        dateInput("boisson_date","Choix de la date de référence",
                  value = today(),format = "MM yyyy",
                  language = "fr", startview = "year")
    ),
    box(width = 4,title = "Première Comparaison",
        status = "primary",solidHeader = TRUE,
        dateInput("boisson_date_comp1",
                  "Choix de la date de référence",
                  value = today(),format = "MM yyyy",
                  language = "fr", startview = "year")
    ),
    box(
      width = 4,title = "Deuxième Comparaison",
      status = "primary",solidHeader = TRUE,
      dateInput("boisson_date_comp2",
                "Choix de la date de référence",
                value = today(),format = "MM yyyy",
                language = "fr", startview = "year")
    )
  ),
  fluidRow(
    class="no-gutter-row",
    column(width = 4,comparaisonUI("boisson_main", "","Boisson")),
    column(width = 4,comparaisonUI("boisson_comp1", "_comp1","Boisson")),
    column(width = 4,comparaisonUI("boisson_comp2", "_comp2","Boisson"))
  ),
  fluidRow(
    box(width = 12,title = "Évolution temporelle des coûts",
        status = "primary",solidHeader = TRUE,
        plotOutput("graph_boisson_evo_cout", height = "600px")
    )
  ),
  fluidRow(
    box(
      width = 12,title = "Évolution temporelle des KPI",
      status = "primary",solidHeader = TRUE,
      plotlyOutput("graph_boisson_evo_kpi", height = "600px")
    )
  )
)
