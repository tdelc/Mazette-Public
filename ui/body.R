
# source("ui/body/tabItem_produits.R")
print(system.time({source("modules.R", local = TRUE)}))
sapply(list.files("ui/body",full.names = TRUE),source)

body <- function(){
  dashboardBody(
    # add_busy_spinner(spin = "double-bounce",position ="full-page"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    add_busy_spinner(spin = "fading-circle"),
    tabItems(

      tabItem_login,

      tabItem_produits,
      tabItem_simulation,
      tabItem_horaires,
      tabItem_boulangerie,
      tabItem_ventes,
      tabItem_ventes_repartition,
      tabItem_comparaison,
      tabItem_historique,
      tabItem_budget,

      tabItem_heures,
      tabItem_nourriture,
      tabItem_evo_boisson,
      tabItem_evo_nourriture,
      tabItem_boisson,
      tabItem_comptes,

      tabItem_brassins_actu,
      tabItem_brassins_fini,
      tabItem_brassins_report,
      tabItem_accises,

      tabItem_compta,
      tabItem_verif
    )
  )
}
