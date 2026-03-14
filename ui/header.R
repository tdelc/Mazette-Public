header <- function(){
  dashboardHeader(title = "Mazette",
                  leftUi = tagList(
                    
                    switchInput(
                      inputId = "check_tva",
                      onStatus = "success", 
                      offStatus = "danger",
                      label = "Ventes",
                      onLabel = "TVAC",
                      offLabel = "HTVA",
                      size = "small",
                      width = "auto"
                    ),
                    actionBttn("add_com","Ajouter un commentaire",size="sm")
                  )
  )
}