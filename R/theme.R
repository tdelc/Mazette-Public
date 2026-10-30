# Les formats d'affichage, et les conventions qui les tiennent.
#
# Montants, pourcentages, pastilles, et les « jumelles » de tri des tableaux.
# Deux précisions coexistent à dessein : la dizaine d'euros pour les ordres de
# grandeur, le centime pour la comptabilité (cf. DECIMALES_COMPTA).

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

  # Zéro ne s'affiche pas : c'est la convention des tableaux du dashboard.
  # Le test porte sur le nombre ARRONDI et non sur la chaîne « 0€ » : avec des
  # décimales, la chaîne vaut « 0,00€ » et la comparaison textuelle ratait
  # tous les zéros. Sur le nombre, la règle est la même quelle que soit la
  # précision demandée — et reste celle d'avant pour tous les appels existants.
  nul <- !is.na(montant) & round(montant, nb_apres) == 0
  montant_formatte[nul] <- ""
  montant_formatte[montant_formatte == "€"] <- ""
  # Une valeur inconnue s'affichait « NA€ », ce qui se lit comme un montant.
  # Le tiret est la convention du reste du tableau de bord (cf. format_pct) et
  # se distingue du vide, qui dit zéro.
  montant_formatte[is.na(montant)] <- "—"
  montant_formatte
}

# Les libellés de DataTables, en français et SANS requête réseau.
#
# L'usage est de charger un fichier de langue depuis un CDN. Le dashboard
# tourne derrière un login et doit s'afficher même quand ce CDN est
# injoignable : on écrit donc les six chaînes dont on se sert, et on s'arrête
# là. « Show 25 entries » au milieu d'un écran en français est le genre de
# détail qui fait douter du reste.
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

# format_CA() blanchit les montants nuls et aligne les largeurs sur le plus
# long élément du vecteur. Les deux comportements sont utiles ailleurs et
# nuisibles ici : dans un tableau dont l'objet est de refaire l'arithmétique,
# une cellule vide se lit « donnée manquante » et non « zéro », et une cellule
# rembourrée d'espaces ne se compare plus à ce qu'on attend. Un poste dont le
# poids n'a pas bougé DOIT montrer 0 €, c'est même le résultat le plus parlant
# du pont.
# La précision de la comptabilité : deux décimales, jamais d'arrondi.
#
# Le reste du dashboard affiche ses montants à la DIZAINE d'euros près
# (format_CA(x, -1)) — à l'échelle d'un CA mensuel, le centime est du bruit.
# Mais sur un compte de résultat et sur un grand livre, c'est l'inverse : on
# vient y vérifier qu'un total retombe sur ses pieds, et « 2.450€ » qui vaut
# en réalité 2.453,27 € fait douter de tout l'écran. Deux écrans, deux
# précisions, et la raison tient en une phrase : on ne lit pas un ordre de
# grandeur et une pièce comptable de la même façon.
DECIMALES_COMPTA <- 2

euro_compta <- function(x) format_CA(x, DECIMALES_COMPTA)

# La même chose, mais pour un TOTAL. format_CA() blanchit les zéros, ce qui
# est la bonne règle dans une colonne de tableau — une case vide s'y lit « il
# ne s'est rien passé ». Sur un total, c'est l'inverse : « 0,00 € au débit »
# est une information, un blanc ressemble à un calcul qui n'a pas abouti.
euro_compta_total <- function(x)
  ifelse(is.na(x), "—",
         ifelse(round(x, DECIMALES_COMPTA) == 0, "0,00€", euro_compta(x)))

# Une petite étiquette colorée, à poser dans une cellule de tableau.
#
# La couleur porte le sens, et le texte le dit quand même : un tableau se lit
# aussi en noir et blanc, et se photocopie.
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
  #
  # PAS de `orderable = FALSE` sur les jumelles, et c'est contre-intuitif :
  # une colonne cachée n'a pas d'en-tête à cliquer, l'interdire semblait donc
  # gratuit — et c'était le défaut. Quand DataTables aplatit une consigne de
  # tri (_fnSortFlatten), il écarte les colonnes source non triables ; une
  # jumelle marquée non triable fait donc tomber la consigne entière, et le
  # tableau ne se trie PLUS DU TOUT. Le clic enregistre bien l'ordre, l'en-tête
  # affiche bien sa flèche, et rien ne bouge : le défaut le plus silencieux de
  # tout ce fichier. Repéré au navigateur, jamais par un test serveur.
  defs <- list(list(targets = as.list(cachees - 1L), visible = FALSE,
                    searchable = FALSE))
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