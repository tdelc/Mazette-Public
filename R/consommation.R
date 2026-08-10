#### REFONTE — Volet "Consommation" (bières & focaccias) ####
# Deux volets de suivi de la consommation, à la maille SEMAINE, toujours
# comparés à la semaine précédente (S-1).
#
# Sources :
#   DB_TICKET   : une ligne par produit vendu, avec TIMESTAMP (donc l'heure),
#                 BOISSON et VOLUME_TOT_L -> analyse horaire et en litres.
#                 Attention : DATE est le JOUR DE SERVICE (une vente à 1h du
#                 matin est rattachée à la soirée de la veille).
#   DB_PRODUITS : une ligne par (jour, produit) avec PRODUIT_FULL complet,
#                 options comprises -> seule source qui porte les suppléments
#                 des focaccias. Peut contenir plusieurs lignes par jour et
#                 produit : toujours agréger.



heure_service <- function(ts) {
  factor(hour(ts), levels = ORDRE_HEURES_SERVICE,
         labels = paste0(ORDRE_HEURES_SERVICE, "h"))
}

# Semaines (lundi) disponibles dans une table, de la plus récente à la plus
# ancienne. `complete_only` retire la semaine en cours, forcément partielle.
semaines_dispo <- function(db, col = "DATE", complete_only = TRUE) {
  s <- db %>%
    mutate(SEM = floor_date(.data[[col]], "week", week_start = 1)) %>%
    distinct(SEM) %>%
    filter(!is.na(SEM)) %>%
    arrange(desc(SEM)) %>%
    pull(SEM)
  if (complete_only) s <- s[s < floor_date(today(), "week", week_start = 1)]
  s
}

# Tuile d'évolution : valeur de la semaine + écart en % vs S-1.
# `sens_positif = FALSE` quand une hausse est une mauvaise nouvelle.
tuile_evolution <- function(valeur, reference, libelle, icone,
                            format_val = function(x) format(round(x)),
                            sens_positif = TRUE, suffixe = "vs S-1") {
  evo <- if (is.na(reference) || reference == 0) NA_real_
  else 100 * (valeur - reference) / reference
  couleur <- if (is.na(evo)) "#8d7b68"
  else if ((evo >= 0) == sens_positif) COUL_VERT else COUL_ROUGE
  sous <- if (is.na(evo)) paste("pas de référence", suffixe)
  else paste0(if (evo >= 0) "+" else "", round(evo, 1), " % ", suffixe)
  kpi_tile(format_val(valeur), libelle, couleur, icone, sous_titre = sous)
}

tuile_ecart <- function(valeur, reference, libelle, icone,
                        format_val = function(x) format(round(x)),
                        sens_positif = TRUE, suffixe = "vs S-1") {
  ecart <- if (is.na(reference) || reference == 0) NA_real_
  else valeur - reference
  couleur <- if (is.na(ecart)) "#8d7b68"
  else if ((ecart >= 0) == sens_positif) COUL_VERT else COUL_ROUGE
  sous <- if (is.na(ecart)) paste("pas de référence", suffixe)
  else if (ecart == 0) paste0("identique ",suffixe)
  else paste0(if (ecart >= 0) "+" else "", round(ecart, 0), " ", suffixe)
  kpi_tile(format_val(valeur), libelle, couleur, icone, sous_titre = sous)
}
