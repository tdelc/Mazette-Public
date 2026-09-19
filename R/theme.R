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
  # Une valeur inconnue s'affichait « NA€ », ce qui se lit comme un montant.
  # Le tiret est la convention du reste du tableau de bord (cf. format_pct) et
  # se distingue du vide, qui dit zéro.
  montant_formatte[is.na(montant)] <- "—"
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

##### Le tri des tableaux #####

# DT trie sur ce qu'il AFFICHE.
#
# Une colonne de montants formatés se trie donc dans l'ordre du dictionnaire :
# « 90€ » y passe devant « 1.234€ », et le plus gros ticket d'une période n'est
# pas celui que le tri désigne. Même piège pour une date écrite « 31/01/2026 »,
# qui se classe par son jour, et pour un pourcentage écrit « 9 % ».
#
# Le remède : on joint à la table des colonnes de tri CACHÉES, nommées
# « .tri:<colonne visible> » et portant la valeur brute. defs_tri() fabrique les
# instructions DataTables qui masquent ces jumelles et ordonnent chaque colonne
# visible sur la sienne.
PREFIXE_TRI <- ".tri:"

col_tri <- function(nom) paste0(PREFIXE_TRI, nom)

defs_tri <- function(table) {
  noms <- names(table)
  cachees <- which(startsWith(noms, PREFIXE_TRI))
  if (!length(cachees)) return(NULL)
  visibles <- match(substring(noms[cachees], nchar(PREFIXE_TRI) + 1L), noms)

  # as.list() : une cible unique doit tout de même être sérialisée en tableau
  # JSON, sinon DataTables reçoit un scalaire là où il attend une liste.
  defs <- list(list(targets = as.list(cachees - 1L), visible = FALSE,
                    searchable = FALSE, orderable = FALSE))
  # Une jumelle dont la colonne visible a disparu (renommée ailleurs) est
  # simplement cachée : viser un indice absent ferait tomber le tableau entier.
  appariees <- which(!is.na(visibles))
  c(defs, lapply(appariees, function(i)
    list(targets = visibles[i] - 1L, orderData = cachees[i] - 1L)))
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