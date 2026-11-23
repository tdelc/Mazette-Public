# Les formats d'affichage, et les conventions qui les tiennent.
#
# Montants, pourcentages, pastilles, et les « jumelles » de tri des tableaux.
# Deux précisions coexistent à dessein : la dizaine d'euros pour les ordres de
# grandeur, le centime pour la comptabilité (cf. DECIMALES_COMPTA).

format_CA <- function(montant,nb_apres=0) {
  # scientific = FALSE : sans lui, 100 000 € s'affiche « 1e+05€ ».
  montant_formatte <- format(round(montant,nb_apres), big.mark = ".",
                             decimal.mark = ",", nsmall = max(nb_apres,0),
                             scientific = FALSE)
  montant_formatte <- paste0(montant_formatte, "€")

  # Zéro ne s'affiche pas (convention des tableaux). Le test porte sur le
  # nombre arrondi, pour valoir quelle que soit la précision.
  nul <- !is.na(montant) & round(montant, nb_apres) == 0
  montant_formatte[nul] <- ""
  montant_formatte[montant_formatte == "€"] <- ""
  # Inconnu : un tiret, qui se distingue du vide (zéro).
  montant_formatte[is.na(montant)] <- "—"
  montant_formatte
}

# Les libellés de DataTables en français, écrits ici plutôt que chargés d'un
# CDN : le tableau doit s'afficher même hors ligne.
LANGUE_DT <- list(
  lengthMenu  = "Afficher _MENU_ lignes",
  info        = "_START_ à _END_ sur _TOTAL_",
  infoEmpty   = "aucune ligne",
  infoFiltered = "(filtré sur _MAX_)",
  zeroRecords = "Aucune ligne ne correspond",
  paginate    = list(previous = "Précédent", `next` = "Suivant"))

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

# La précision de la comptabilité : deux décimales.
#
# Le reste du dashboard arrondit à la dizaine d'euros (format_CA(x, -1)) ; un
# compte de résultat ou un grand livre se vérifie au centime. euro_compta()
# affiche aussi les zéros : dans un tableau qu'on refait à la main, « 0 € »
# est une information.
DECIMALES_COMPTA <- 2

euro_compta <- function(x) format_CA(x, DECIMALES_COMPTA)

# La même chose pour un total : un total nul s'affiche, un blanc y
# ressemblerait à un calcul qui n'a pas abouti.
euro_compta_total <- function(x)
  ifelse(is.na(x), "—",
         ifelse(round(x, DECIMALES_COMPTA) == 0, "0,00€", euro_compta(x)))

# Une petite étiquette colorée, à poser dans une cellule de tableau. Le texte
# dit ce que dit la couleur : le tableau doit se lire en noir et blanc.
pastille <- function(texte, couleur, classe = "badge-etat") {
  ifelse(is.na(texte) | !nzchar(texte), "",
         sprintf(paste0("<span class=\"%s\" style=\"background:%s1f;",
                        "color:%s;border-color:%s66\">%s</span>"),
                 classe, couleur, couleur, couleur, texte))
}

euro_exact <- function(x) {
  ifelse(is.na(x), "—",
  ifelse(abs(x) < 0.5, "0 €", trimws(format_CA(x, -1))))
}

euro_signe <- function(x) {
  ifelse(is.na(x), "—",
  ifelse(abs(x) < 0.5, "0 €",
         paste0(ifelse(x > 0, "+", ""), trimws(format_CA(x, -1)))))
}

##### Le tri des tableaux #####

# DT trie sur ce qu'il affiche : « 90€ » passe devant « 1.234€ », et
# « 31/01/2026 » se classe par son jour.
#
# On joint donc à la table des colonnes cachées « .tri:<colonne> » qui portent
# la valeur brute ; defs_tri() masque ces jumelles et fait trier chaque colonne
# visible sur la sienne.
PREFIXE_TRI <- ".tri:"

col_tri <- function(nom) paste0(PREFIXE_TRI, nom)

defs_tri <- function(table) {
  noms <- names(table)
  cachees <- which(startsWith(noms, PREFIXE_TRI))
  if (!length(cachees)) return(NULL)
  visibles <- match(substring(noms[cachees], nchar(PREFIXE_TRI) + 1L), noms)

  # as.list() : une cible unique doit partir en tableau JSON.
  #
  # Pas de `orderable = FALSE` sur les jumelles : DataTables écarte les colonnes
  # non triables en aplatissant une consigne de tri, et le tableau ne se
  # trierait plus du tout.
  defs <- list(list(targets = as.list(cachees - 1L), visible = FALSE,
                    searchable = FALSE))
  # Une jumelle sans colonne visible est seulement cachée : viser un indice
  # absent ferait tomber le tableau.
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