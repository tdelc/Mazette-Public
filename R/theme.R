##### Format #####

theme_mazette <- function(){
  theme(
    axis.title.x.top = element_text(margin = margin(b=10)),
    axis.text = element_text(face = "bold",size = 12),
    axis.title = element_text(face = "bold",size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
}

format_CA <- function(montant,nb_apres=0) {
  montant_formatte <- format(round(montant,nb_apres), big.mark = ".",
                             decimal.mark = ",", nsmall = max(nb_apres,0))
  montant_formatte <- paste0(montant_formatte, "€")
  
  montant_formatte[str_trim(montant_formatte) == "0€"] <- ""
  montant_formatte[montant_formatte == "€"] <- ""
  montant_formatte
}

datatable_simple <- function(table){
  datatable(
    table,
    options = list(
      dom = 't', # 't' pour "table" - affiche uniquement le tableau sans contrôles
      paging = FALSE, # Désactive la pagination
      ordering = FALSE, # Désactive le tri
      searching = FALSE # Désactive la recherche
    ),
    rownames= FALSE
  )
}