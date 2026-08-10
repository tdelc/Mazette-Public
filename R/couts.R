# Coûts matière : table canonique et normalisation des sources.
#
# ---------------------------------------------------------------------------
# Grain
# ---------------------------------------------------------------------------
# L'historique comptable est MENSUEL. Le suivi courant pourra devenir
# hebdomadaire. La table porte donc sa granularité au lieu de la supposer :
#
#   PERIODE         premier jour de la période (1er du mois, ou lundi)
#   GRANULARITE     "mois" ou "semaine"
#   SECTEUR         Service / Transformation alimentaire / Brasserie / Support
#   ACHATS          factures de la période
#   VARIATION_STOCK cf. convention de signe ci-dessous
#   COUT_MATIERE    consommation réelle = ACHATS + signe * VARIATION_STOCK
#   STOCK_CONNU     FALSE si l'inventaire manque : le coût vaut alors les seuls
#                   achats, ce qui le rend juste sur un an mais bruité au mois
#   SIMU            TRUE pour les données fictives
#
# On ne convertit JAMAIS un mois en semaines : répartir un montant mensuel sur
# quatre semaines fabriquerait des food costs hebdomadaires qui n'existent pas.
# Une vue hebdomadaire ne s'appuie que sur des lignes hebdomadaires.
#
# ---------------------------------------------------------------------------
# Convention de signe
# ---------------------------------------------------------------------------
# Le plan comptable français définit la variation de stock (compte 603) comme
# « stock initial − stock final » : positive = on a puisé dans le stock = charge
# supplémentaire. D'où COUT_MATIERE = ACHATS + VARIATION_STOCK.
#
# Certains exports retiennent la convention inverse (stock final − stock
# initial). Si c'est le cas du tien, passe cette constante à -1 : c'est le seul
# endroit à changer. `diagnostic_signe_stock()` plus bas tranche sur pièces.
SIGNE_VARIATION_STOCK <- 1

GRANULARITES_COUTS <- c("mois", "semaine")

# Premier jour de la période contenant `d`, selon la granularité.
# Vectorisées toutes les deux : elles sont appelées dans des mutate() où
# GRANULARITE varie ligne à ligne, un `if` y prendrait la première valeur.
debut_granularite <- function(d, granularite) {
  d <- as.Date(d)
  # rep_len : `granularite` est tantôt un scalaire (normalisation d'une source
  # entière), tantôt une colonne (table mixte). if_else() refuse une condition
  # plus courte que ses branches, il faut donc la recycler nous-mêmes.
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
#' Accepte trois formes d'entrée, au choix :
#'   - ANNEE + MOIS  (l'historique comptable)
#'   - PERIODE       (déjà un premier jour de période)
#'   - SEMAINE       (l'ancien nom, pour les données fictives)
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
      # Un inventaire absent n'est pas une variation nulle : on le dit, et on
      # retombe sur les achats seuls plutôt que d'inventer un stock.
      STOCK_CONNU     = !is.na(VARIATION_STOCK),
      VARIATION_STOCK = replace_na(VARIATION_STOCK, 0),
      ACHATS          = replace_na(ACHATS, 0),
      COUT_MATIERE    = ACHATS + SIGNE_VARIATION_STOCK * VARIATION_STOCK,
      SIMU            = simu
    )

  # Une seule ligne par période x secteur : un export comptable en contient
  # parfois plusieurs (ventilation par compte), qu'on additionne.
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

#' Assemble plusieurs sources en une table canonique.
#'
#' L'ordre compte : en cas de recouvrement sur une même période x secteur x
#' granularité, la PREMIÈRE source l'emporte. Placer donc le réel avant le
#' fictif.
empile_couts_matiere <- function(...) {
  sources <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
  if (!length(sources)) return(normalise_couts_matiere(tibble()))
  bind_rows(sources) %>%
    group_by(PERIODE, GRANULARITE, SECTEUR) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(PERIODE, SECTEUR) |> 
    mutate(simu = replace_na(FALSE))
}

#' Lignes utilisables pour une granularité d'analyse donnée.
#'
#' Règle : une vue hebdomadaire n'accepte que des lignes hebdomadaires. Une vue
#' mensuelle ou annuelle accepte les deux, mais si un mois dispose de lignes
#' hebdomadaires, elles priment et la ligne mensuelle est écartée — sans quoi
#' on compterait deux fois le même mois.
couts_matiere_effectifs <- function(db, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (!nrow(db)) return(db)

  if (unite == "semaine") return(filter(db, GRANULARITE == "semaine"))

  hebdo <- filter(db, GRANULARITE == "semaine")
  mensuel <- filter(db, GRANULARITE == "mois")

  if (nrow(hebdo)) {
    mois_couverts <- hebdo %>%
      mutate(MOIS = floor_date(PERIODE, "month")) %>%
      distinct(MOIS, SECTEUR)
    mensuel <- mensuel %>%
      mutate(MOIS = floor_date(PERIODE, "month")) %>%
      anti_join(mois_couverts, by = c("MOIS", "SECTEUR")) %>%
      select(-MOIS)
  }
  bind_rows(hebdo, mensuel) %>% arrange(PERIODE, SECTEUR)
}

#' Lignes de coûts dont la période contient `jour`.
#'
#' Remplace les `filter(SEMAINE == floor_date(jour, "week"))` : la période
#' englobante est un mois ou une semaine selon la ligne, on ne peut donc plus
#' la calculer depuis le jour, il faut la chercher.
couts_matiere_du_jour <- function(db, jour) {
  jour <- as.Date(jour)
  if (!nrow(db)) return(db)
  db %>% filter(PERIODE <= jour, fin_granularite(PERIODE, GRANULARITE) >= jour)
}

#' Étale les coûts d'une période sur ses jours d'ouverture.
#'
#' Sert au volet « détail par jour », qui a besoin d'un montant journalier.
#' C'est une répartition uniforme, donc une approximation assumée : elle ne
#' remonte jamais dans les KPI, qui travaillent sur la période native.
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

#' Fenêtre réellement analysable par la compta.
#'
#' `agrege_compta()` croise CA, travail et matières : la marge n'a de sens que
#' là où les trois existent. Avec un historique matière long et un historique
#' travail court, l'analyse se limite au second — ce que cette fonction rend
#' visible plutôt que de laisser un tableau vide sans explication.
couverture_compta <- function(db_kpi, db_travail, db_matiere) {
  plage <- function(x, lab) {
    if (!length(x) || all(is.na(x)))
      tibble(source = lab, debut = as.Date(NA), fin = as.Date(NA))
    else tibble(source = lab, debut = min(x, na.rm = TRUE), fin = max(x, na.rm = TRUE))
  }
  fins <- if (nrow(db_matiere))
    fin_granularite(db_matiere$PERIODE, db_matiere$GRANULARITE) else as.Date(NA)

  res <- bind_rows(
    plage(db_kpi$DATE[db_kpi$ventes > 0], "CA"),
    plage(db_travail$DATE, "Travail"),
    plage(c(db_matiere$PERIODE, fins), "Matières")
  )
  bind_rows(res, tibble(source = "→ analysable",
                        debut = suppressWarnings(max(res$debut, na.rm = TRUE)),
                        fin   = suppressWarnings(min(res$fin,   na.rm = TRUE))))
}

#' Départage les deux conventions de signe sur pièces.
#'
#' Calcule le food cost des secteurs métier sous chacune des deux conventions et
#' le compare au CA réel. Un food cost de restauration se situe entre 25 et 35 %
#' du CA HTVA : la convention qui sort de cette fourchette est la mauvaise.
diagnostic_signe_stock <- function(db_couts, db_kpi) {
  ca <- db_kpi %>%
    mutate(MOIS = floor_date(DATE, "month")) %>%
    group_by(MOIS) %>%
    summarise(CA = sum(ventes, na.rm = TRUE), .groups = "drop") %>%
    filter(CA > 0)

  db_couts %>%
    filter(SECTEUR != "Support") %>%
    mutate(MOIS = floor_date(PERIODE, "month")) %>%
    group_by(MOIS) %>%
    summarise(ACHATS = sum(ACHATS, na.rm = TRUE),
              STOCK  = sum(VARIATION_STOCK, na.rm = TRUE), .groups = "drop") %>%
    inner_join(ca, by = "MOIS") %>%
    summarise(
      mois          = n(),
      `food +` = paste0(round(100 * sum(ACHATS + STOCK) / sum(CA), 1), " %"),
      `food -` = paste0(round(100 * sum(ACHATS - STOCK) / sum(CA), 1), " %"),
      `achats seuls` = paste0(round(100 * sum(ACHATS) / sum(CA), 1), " %")
    )
}
