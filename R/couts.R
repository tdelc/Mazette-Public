# Coûts matière : table canonique et normalisation des sources.
#
#   PERIODE         premier jour de la période (1er du mois, ou lundi)
#   GRANULARITE     "mois" ou "semaine"
#   SECTEUR         Service / Transformation alimentaire / Brasserie / Support
#   ACHATS          factures de la période
#   VARIATION_STOCK cf. convention de signe ci-dessous
#   COUT_MATIERE    ACHATS + signe * VARIATION_STOCK
#   STOCK_CONNU     FALSE si l'inventaire manque : le coût vaut les seuls achats
#   SIMU            TRUE pour des données fictives
#
# Un mois n'est jamais converti en semaines : une vue hebdomadaire ne
# s'appuie que sur des lignes hebdomadaires.
#
# Convention de signe : variation de stock = stock initial − stock final
# (positive = on a puisé dans le stock). Pour un export dans la convention
# inverse, passer cette constante à -1.
SIGNE_VARIATION_STOCK <- 1

# Premier jour de la période contenant `d`. Vectorisées toutes les deux :
# GRANULARITE varie ligne à ligne.
debut_granularite <- function(d, granularite) {
  d <- as.Date(d)
  # rep_len : `granularite` est un scalaire ou une colonne.
  mois <- rep_len(granularite == "mois", length(d))
  if_else(mois, floor_date(d, "month"), floor_date(d, "week", week_start = 1))
}

# Dernier jour de la période commençant à `p`.
fin_granularite <- function(p, granularite) {
  p <- as.Date(p)
  mois <- rep_len(granularite == "mois", length(p))
  if_else(mois, ceiling_date(p, "month") - 1, p + 6)
}

#' Normalise une source de coûts matière vers la table canonique.
#'
#' Accepte ANNEE + MOIS, PERIODE, ou SEMAINE.
#'
#' @param df table source
#' @param granularite "mois" ou "semaine"
#' @param simu TRUE si les données sont fictives
normalise_couts_matiere <- function(df, granularite = c("mois", "semaine"),
                                    simu = FALSE) {
  granularite <- match.arg(granularite)

  if (!nrow(df))
    return(tibble(PERIODE = as.Date(character()), GRANULARITE = character(),
                  SECTEUR = character(), ACHATS = numeric(),
                  VARIATION_STOCK = numeric(), COUT_MATIERE = numeric(),
                  STOCK_CONNU = logical(), SIMU = logical()))

  df <- ungroup(df)

  periode <- if (all(c("ANNEE", "MOIS") %in% names(df))) {
    as.Date(sprintf("%04d-%02d-01", as.integer(df$ANNEE), as.integer(df$MOIS)))
  } else if ("PERIODE" %in% names(df)) {
    debut_granularite(df$PERIODE, granularite)
  } else if ("SEMAINE" %in% names(df)) {
    debut_granularite(df$SEMAINE, granularite)
  } else {
    stop("normalise_couts_matiere() attend ANNEE+MOIS, PERIODE ou SEMAINE ; ",
         "colonnes reçues : ", paste(names(df), collapse = ", "))
  }

  manque <- setdiff(c("SECTEUR", "ACHATS"), names(df))
  if (length(manque))
    stop("colonnes manquantes dans la source de coûts matière : ",
         paste(manque, collapse = ", "))

  if (!"VARIATION_STOCK" %in% names(df)) df$VARIATION_STOCK <- NA_real_

  res <- tibble(
    PERIODE     = periode,
    GRANULARITE = granularite,
    SECTEUR     = as.character(df$SECTEUR),
    ACHATS      = as.numeric(df$ACHATS),
    VARIATION_STOCK = as.numeric(df$VARIATION_STOCK)
  ) %>%
    mutate(
      # Inventaire absent : les achats seuls.
      STOCK_CONNU     = !is.na(VARIATION_STOCK),
      VARIATION_STOCK = replace_na(VARIATION_STOCK, 0),
      ACHATS          = replace_na(ACHATS, 0),
      COUT_MATIERE    = ACHATS + SIGNE_VARIATION_STOCK * VARIATION_STOCK,
      SIMU            = simu
    )

  # Une ligne par période × secteur.
  res %>%
    group_by(PERIODE, GRANULARITE, SECTEUR) %>%
    summarise(ACHATS          = sum(ACHATS),
              VARIATION_STOCK = sum(VARIATION_STOCK),
              COUT_MATIERE    = sum(COUT_MATIERE),
              STOCK_CONNU     = any(STOCK_CONNU),
              SIMU            = any(SIMU),
              .groups = "drop") %>%
    arrange(PERIODE, SECTEUR)
}

#' Étale les coûts d'une période sur ses jours d'ouverture (répartition
#' uniforme, pour le détail par jour ; jamais utilisée dans les KPI).
couts_matiere_par_jour <- function(db, db_date, jusqu_a = today()) {
  if (!nrow(db)) return(tibble())
  db %>%
    mutate(FIN = fin_granularite(PERIODE, GRANULARITE)) %>%
    rowwise() %>%
    mutate(DATE = list(seq(PERIODE, FIN, by = "day"))) %>%
    ungroup() %>%
    tidyr::unnest(DATE) %>%
    semi_join(db_date, by = "DATE") %>%
    group_by(PERIODE, GRANULARITE, SECTEUR) %>%
    mutate(across(c(ACHATS, VARIATION_STOCK, COUT_MATIERE), ~ . / n())) %>%
    ungroup() %>%
    filter(DATE < jusqu_a) %>%
    select(DATE, PERIODE, GRANULARITE, SECTEUR,
           COUT_MATIERE, ACHATS, VARIATION_STOCK, STOCK_CONNU, SIMU) %>%
    arrange(DATE, SECTEUR)
}
