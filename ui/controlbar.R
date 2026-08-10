controlbar <- function(){
  dashboardControlbar(
  skin = "dark",
  controlbarMenu(
    id = "menu",
    controlbarItem("Indicateur de ventes",
                   radioGroupButtons(
                     inputId = "global_tva",
                     label = "",
                     choices = c("HTVA" = "CA_HTVA",
                                 "TVAC" = "CA_TVAC"),
                     size = 'xs',
                     individual = TRUE,
                     status = "primary"
                   )
    )
  )
)
}