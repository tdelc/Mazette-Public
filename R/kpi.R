##### Tuiles KPI #####

kpi_travail_tiles <- function(ag) {
  ca    <- sum(ag$CA, na.rm = TRUE)
  hs    <- sum(ag$H_SERVICE, na.rm = TRUE)
  ht    <- sum(ag$H_TOTAL, na.rm = TRUE)
  cs    <- sum(ag$COUT_SERVICE, na.rm = TRUE)
  ct    <- sum(ag$COUT_TOTAL, na.rm = TRUE)
  marge <- ca - ct
  cah   <- if (hs > 0) ca / hs else NA_real_
  
  div(
    class = "kpi-grid",
    kpi_tile(if (is.na(cah)) "—" else format_CA(cah, -1), "CA par heure de service",
             couleur_seuil_haut(cah, 90, 70), "gauge-high",
             sous_titre = paste0(format(round(hs)), " h de service")),
    kpi_tile(format(round(ht)), "Heures totales", "#8d7b68", "clock",
             sous_titre = paste0(round(ratio_pct(hs, ht)), " % en service")),
    kpi_tile(format_CA(cs, -1), "Coût de service", COUL_TRAVAIL, "person-running",
             sous_titre = format_pct(ratio_pct(cs, ca))),
    kpi_tile(format_CA(ct - cs, -1), "Coûts indirects", "#8d7b68", "people-roof",
             sous_titre = format_pct(ratio_pct(ct - cs, ca))),
    kpi_tile(format_pct(ratio_pct(ct, ca)), "Coût du travail / CA",
             couleur_seuil(ratio_pct(ct, ca), 35, 45), "scale-balanced",
             sous_titre = format_CA(ct, -1)),
    kpi_tile(format_CA(marge, -1), "Marge après travail",
             if (marge >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(ratio_pct(marge, ca)))
  )
}

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

format_pct <- function(x, nb = 1) if (is.na(x)) "—" else paste0(round(x, nb), " %")

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

# Grille des KPI d'une période (sortie de compta_apercu).
kpi_compta_tiles <- function(ap, unite_tva = 'HTVA') {
  t <- ap$total
  div(
    class = "kpi-grid",
    kpi_tile(format_CA(t$CA, -1), paste("CA",unite_tva), "#2e7d32", "euro-sign"),
    kpi_tile(format_CA(t$MARGE, -1), "Marge",
             if (t$MARGE >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(t$MARGE_PCT)),
    kpi_tile(format_pct(t$FOOD_PCT), "Food Cost / CA",
             couleur_seuil(t$FOOD_PCT, 30, 35), "cart-shopping",
             sous_titre = format_CA(t$FOOD, -1)),
    kpi_tile(format_pct(t$WORK_PCT), "Work Cost / CA",
             couleur_seuil(t$WORK_PCT, 35, 42), "person-running",
             sous_titre = format_CA(t$TRAVAIL, -1)),
    kpi_tile(format_pct(t$PRIME_PCT), "Prime Cost / CA",
             couleur_seuil(t$PRIME_PCT, 65, 72), "scale-balanced",
             sous_titre = format_CA(t$PRIME, -1)),
    kpi_tile(format_pct(t$GENERAL_PCT), "Frais généraux / CA",
             couleur_seuil(t$GENERAL_PCT, 12, 18), "receipt",
             sous_titre = format_CA(t$GENERAL, -1)),
    kpi_tile(format(round(t$HEURES)), "Heures prestées", "#8d7b68", "clock",
             sous_titre = if (t$HEURES > 0)
               paste0(format_CA(t$CA / t$HEURES, -1), " de CA / h") else NULL)
  )
}

# Bandeau de comparaison A vs B (écarts en € et en points de %).
kpi_ecarts_tiles <- function(ap_a, ap_b) {
  a <- ap_a$total; b <- ap_b$total
  ec <- function(x, y) x - y
  pt <- function(x, y) if (is.na(x) || is.na(y)) NA_real_ else x - y
  signe <- function(v, unite = "€") {
    if (is.na(v)) return("—")
    prefixe <- if (v > 0) "+" else ""
    if (unite == "€") paste0(prefixe, format_CA(v, -1))
    else paste0(prefixe, round(v, 1), " pt")
  }
  # Pour les coûts, une hausse est défavorable -> rouge
  coul_bas <- function(v) if (is.na(v)) "#9e9e9e" else if (v <= 0) COUL_VERT else COUL_ROUGE
  coul_haut <- function(v) if (is.na(v)) "#9e9e9e" else if (v >= 0) COUL_VERT else COUL_ROUGE
  
  div(
    class = "kpi-grid",
    kpi_tile(signe(ec(a$CA, b$CA)), "Écart CA", coul_haut(ec(a$CA, b$CA)), "euro-sign"),
    kpi_tile(signe(ec(a$MARGE, b$MARGE)), "Écart marge",
             coul_haut(ec(a$MARGE, b$MARGE)), "piggy-bank"),
    kpi_tile(signe(pt(a$FOOD_PCT, b$FOOD_PCT), "pt"), "Écart food cost",
             coul_bas(pt(a$FOOD_PCT, b$FOOD_PCT)), "cart-shopping"),
    kpi_tile(signe(pt(a$WORK_PCT, b$WORK_PCT), "pt"), "Écart work cost",
             coul_bas(pt(a$WORK_PCT, b$WORK_PCT)), "person-running"),
    kpi_tile(signe(pt(a$PRIME_PCT, b$PRIME_PCT), "pt"), "Écart prime cost",
             coul_bas(pt(a$PRIME_PCT, b$PRIME_PCT)), "scale-balanced"),
    kpi_tile(signe(pt(a$MARGE_PCT, b$MARGE_PCT), "pt"), "Écart marge %",
             coul_haut(pt(a$MARGE_PCT, b$MARGE_PCT)), "percent")
  )
}
