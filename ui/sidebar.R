sidebar <- function(){
  dashboardSidebar(
    useShinyjs(),

    conditionalPanel(
      condition = "output.logged != 'ok'",
      sidebarMenu(
        id = "tab_login",
        menuItem("Login", tabName = "login", icon = icon("lock"))
      )
    ),

    conditionalPanel(
      condition = "output.logged == 'ok'",
      sidebarMenu(
        id = "tabs",
          # menuItem("KPIs", tabName = "kpis", icon = icon("dashboard")),
          menuItem("Ventes",startExpanded = TRUE,
                   menuSubItem("Ventes actuelles", tabName = "ventes", icon = icon("dashboard")),
                   menuSubItem("Ventes horaires", tabName = "horaires", icon = icon("clock")),
                   menuSubItem("Historique", tabName = "historique", icon = icon("clock-rotate-left")),
                   menuSubItem("Comparaison", tabName = "comparaison", icon = icon("code-compare")),
                   menuSubItem("Analyse annuelle", tabName = "budget", icon = icon("euro-sign"))
          ),
          menuItem("Évolution Produits", tabName = "produits", icon = icon("tags")),
          menuItem("Simulation Prix", tabName = "simulation", icon = icon("flask")),
          menuItem("Heures de travail", tabName = "heures", icon = icon("clock")),

          menuItem("Nourriture",startExpanded = TRUE,
                   menuSubItem("Ventes Actuelles", tabName = "boulangerie", icon = icon("bread-slice")),
                   menuSubItem("Évolution de la nourriture", tabName = "evo_nourriture", icon = icon("cookie-bite")),
                   menuSubItem("Midi/Soir (food)", tabName = "ventes_repartition", icon = icon("down-left-and-up-right-to-center")),
                   menuSubItem("CA / Cout / KPI", tabName = "nourriture", icon = icon("utensils"))
          ),
          menuItem("Boisson",startExpanded = TRUE,
                   menuSubItem("Bières actuelles", tabName = "brassins_actu", icon = icon("beer")),
                   menuSubItem("Rapport d'un brassin", tabName = "brassins_report", icon = icon("beer")),
                   menuSubItem("Évolution des boissons", tabName = "evo_boisson", icon = icon("champagne-glasses")),
                   menuSubItem("CA / Cout / KPI", tabName = "boisson", icon = icon("wine-glass"))
          ),
          menuItem("Global",startExpanded = TRUE,
                   menuSubItem("CA / Cout / KPI", tabName = "comptes", icon = icon("calculator")),
                   menuSubItem("Compta", tabName = "compta", icon = icon("list"))
          ),
        menuItem("Vérification", tabName = "verif", icon = icon("rocket-launch"))
      )
    )
  )
}
