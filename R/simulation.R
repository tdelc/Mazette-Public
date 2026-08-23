# Base de simulation : par produit sur une période -> quantité, CA HTVA, prix moyen HTVA.
# Ordre stable (CATEGORIE puis CA décroissant) pour mapper les éditions par n° de ligne.
prepa_simulation <- function(db_produits, d1, d2) {
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(CATEGORIE, PRODUIT = PRODUIT_FULL) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_TVAC, na.rm = TRUE), .groups = "drop") %>%
    mutate(PRIX_MOYEN = CA / QUANTITE) %>%
    arrange(CATEGORIE, desc(CA))
}

# Applique un vecteur de prix simulés (indexé par n° de ligne) à la base.
# Hypothèse : quantité inchangée -> le CA varie au prorata du prix.
calc_simulation <- function(base, prix_simu) {
  if (is.null(prix_simu) || length(prix_simu) != nrow(base))
    prix_simu <- base$PRIX_MOYEN
  base %>%
    mutate(PRIX_SIMU = as.numeric(prix_simu),
           PRIX_SIMU = ifelse(is.na(PRIX_SIMU), PRIX_MOYEN, PRIX_SIMU),
           CA_SIMU = QUANTITE * PRIX_SIMU,
           DELTA = CA_SIMU - CA)
}

# Mise en forme pour affichage DT (table éditable côté serveur)
table_simulation_aff <- function(sim) {
  sim %>%
    transmute(Catégorie = CATEGORIE,
              Produit = tronque_nom(PRODUIT),
              Quantité = QUANTITE,
              `Prix moyen` = round(PRIX_MOYEN, 2),
              `Prix simulé` = round(PRIX_SIMU, 2),
              `CA HTVA actuel` = round(CA),
              `CA HTVA simulé` = round(CA_SIMU),
              `Δ CA` = round(DELTA))
}