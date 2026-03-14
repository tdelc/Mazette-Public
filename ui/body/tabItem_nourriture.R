tabItem_nourriture <- tabItem(
  tabName = "nourriture",
  # Sélecteur du mois
  fluidRow(
    box(width = 4,title = "Sélection du mois",
        status = "primary",solidHeader = TRUE,
        dateInput("nourriture_date","Choix de la date de référence",
                value = today(),format = "MM yyyy",
                language = "fr", startview = "year")
    ),
    box(width = 4,title = "Première Comparaison",
      status = "primary",solidHeader = TRUE,
      dateInput("nourriture_date_comp1",
                "Choix de la date de référence",
                value = today(),format = "MM yyyy",
                language = "fr", startview = "year")
    ),
    box(
      width = 4,title = "Deuxième Comparaison",
      status = "primary",solidHeader = TRUE,
      dateInput("nourriture_date_comp2",
                "Choix de la date de référence",
                value = today(),format = "MM yyyy",
                language = "fr", startview = "year")
    )
  ),
  fluidRow(
    class="no-gutter-row",
    column(width = 4,comparaisonUI("nourriture_main", "","Nourriture")),
    column(width = 4,comparaisonUI("nourriture_comp1", "_comp1","Nourriture")),
    column(width = 4,comparaisonUI("nourriture_comp2", "_comp2","Nourriture"))
  ),
  fluidRow(
    box(width = 12,title = "Évolution temporelle des coûts",
      status = "primary",solidHeader = TRUE,
      plotOutput("graph_nourriture_evo_cout", height = "600px")
    )
  ),
  fluidRow(
    box(
      width = 12,title = "Évolution temporelle des KPI",
      status = "primary",solidHeader = TRUE,
      plotlyOutput("graph_nourriture_evo_kpi", height = "600px")
    )
  )
)
