tabItem_comptes <- tabItem(
  tabName = "comptes",
  tags$head(
    tags$style(HTML("
      .no-gutter-row { margin-left: 0px; margin-right: 0px; }
      .no-gutter-row [class*='col-sm-12'] {padding-left: 0px; padding-right: 0px;}
      .no-gutter-row [class*='col-sm-4'] {padding-left: 15px; padding-right: 15px;}
    "))
  ),
  # Sélecteur du mois
  fluidRow(
    box(width = 4,title = "Sélection du mois",
        status = "primary",solidHeader = TRUE,
        dateInput("comptes_date","Choix de la date de référence",
                  value = today(),format = "MM yyyy",
                  language = "fr", startview = "year")
    ),
    box(width = 4,title = "Première Comparaison",
        status = "primary",solidHeader = TRUE,
        dateInput("comptes_date_comp1",
                  "Choix de la date de référence",
                  value = today(),format = "MM yyyy",
                  language = "fr", startview = "year")
    ),
    box(
      width = 4,title = "Deuxième Comparaison",
      status = "primary",solidHeader = TRUE,
      dateInput("comptes_date_comp2",
                "Choix de la date de référence",
                value = today(),format = "MM yyyy",
                language = "fr", startview = "year")
    )
  ),
  fluidRow(
    class="no-gutter-row",
    column(width = 4,comparaisonUI("comptes_main", "","Total")),
    column(width = 4,comparaisonUI("comptes_comp1", "_comp1","Total")),
    column(width = 4,comparaisonUI("comptes_comp2", "_comp2","Total"))
  ),
  fluidRow(
    box(width = 12,title = "Évolution temporelle des coûts",
        status = "primary",solidHeader = TRUE,
        plotOutput("graph_comptes_evo_cout", height = "600px")
    )
  ),
  fluidRow(
    box(
      width = 12,title = "Évolution temporelle des KPI",
      status = "primary",solidHeader = TRUE,
      plotlyOutput("graph_comptes_evo_kpi", height = "600px")
    )
  )
)
