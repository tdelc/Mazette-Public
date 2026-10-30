# Les tuiles de synthèse, partagées par tous les volets.
#
# Une tuile porte un chiffre, un intitulé, une couleur et parfois un
# sous-titre. La couleur dit le verdict — c'est ici que vit le contrôle de
# gestion des ratios (SEUILS_RATIOS est dans R/exploitation.R, à côté des
# tuiles d'exploitation qui l'affichent).

# Les tuiles du volet Travail vivent désormais dans R/travail.R
# (kpi_travail), avec le reste du volet.

##### Tuiles KPI #####

# Couleur d'un ratio où PLUS BAS = MIEUX (food/work/prime cost).
couleur_seuil <- function(x, bon, moyen) {
  if (is.na(x)) return("#9e9e9e")
  if (x <= bon) COUL_VERT else if (x <= moyen) COUL_AMBRE else COUL_ROUGE
}

# Couleur d'un ratio où PLUS HAUT = MIEUX (marge).
couleur_seuil_haut <- function(x, bon, moyen) {
  if (is.na(x)) return("#9e9e9e")
  if (x >= bon) COUL_VERT else if (x >= moyen) COUL_AMBRE else COUL_ROUGE
}

# Vectorisé : `if` prendrait la première valeur sur un vecteur, et lève une
# erreur depuis R 4.2. Les appels scalaires sont inchangés.
format_pct <- function(x, nb = 1) ifelse(is.na(x), "—", paste0(round(x, nb), " %"))

# Une tuile KPI (grand chiffre + libellé + icône en filigrane)
kpi_tile <- function(valeur, libelle, couleur, icone = NULL, sous_titre = NULL) {
  div(
    class = "kpi-tile", style = paste0("background:", couleur, ";"),
    if (!is.null(icone)) span(class = "kpi-tile-icon", icon(icone)),
    div(class = "kpi-tile-val", valeur),
    div(class = "kpi-tile-lab", libelle),
    if (!is.null(sous_titre)) div(class = "kpi-tile-sub", sous_titre)
  )
}

