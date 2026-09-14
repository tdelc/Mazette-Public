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
  # scientific = FALSE, sinon format() bascule en notation scientifique dès que
  # celle-ci est plus COURTE que la notation décimale : un chiffre d'affaires de
  # 100 000 € s'affichait « 1e+05€ ». Le piège ne se déclenche que sur les
  # nombres ronds, et seulement quand l'appel est scalaire — sur un vecteur, la
  # largeur commune est décidée par le plus long élément et masque le problème.
  montant_formatte <- format(round(montant,nb_apres), big.mark = ".",
                             decimal.mark = ",", nsmall = max(nb_apres,0),
                             scientific = FALSE)
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

change_cursor_plotly <- function(p){
  p |> htmlwidgets::onRender(
    paste0(
      "function(el, x) {",
      "  function setCursor() {",
      "    var dragLayer = el.getElementsByClassName('nsewdrag')[0];",
      "    if (dragLayer) {",
      "      dragLayer.style.cursor = 'default';",
      "      el.on('plotly_hover', function(data) {",
      "        dragLayer.style.cursor = 'pointer';",
      "      });",
      "      el.on('plotly_unhover', function(data) {",
      "        dragLayer.style.cursor = 'default';",
      "      });",
      "      return true;",
      "    }",
      "    return false;",
      "  }",
      "  if (!setCursor()) {",
      "    var observer = new MutationObserver(function(mutations) {",
      "      if (setCursor()) { observer.disconnect(); }",
      "    });",
      "    observer.observe(el, { childList: true, subtree: true });",
      "  }",
      "}"
    )
  )
}