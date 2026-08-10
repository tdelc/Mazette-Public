# Plan comptable : classement des comptes par leur NUMÉRO.
#
# ---------------------------------------------------------------------------
# Pourquoi le numéro et rien d'autre
# ---------------------------------------------------------------------------
# Le PCMN attribue une signification fixe aux deux premiers chiffres : 70 = ventes,
# 62 = rémunérations, 63 = amortissements… Cette structure ne bouge pas, alors que
# tout le reste bouge :
#
#   - la mise en page du rapport change (31 nombres de lignes différents sur les
#     52 mois de DB_COMPTA) ;
#   - les SECTION / RUBRIQUE de l'export dérivent : « Précompte immobilier »
#     (640400) est tantôt sous « Services et Biens », tantôt sous « Autres
#     charges d'exploitation » ; « Charges d'exploitation non récurrentes »
#     (664000) tantôt sous « Services et Biens », tantôt sous « Charges non
#     récurrentes » ;
#   - des comptes apparaissent et disparaissent d'un exercice à l'autre.
#
# On classe donc sur le numéro, et on ne déduit plus rien des libellés ni des
# totaux. Un compte inconnu du tableau ci-dessous tombe dans « Non classé » et se
# voit, plutôt que de disparaître d'un total.

#' Correspondance préfixe de compte -> poste de gestion.
#'
#' `PREFIXE` est testé du plus long au plus court : « 609 » l'emporte sur « 60 ».
#' `SENS` vaut +1 pour un produit, -1 pour une charge.
#' `ORDRE` fixe la place dans le compte de résultat.
PLAN_PCMN <- tibble::tribble(
  ~PREFIXE, ~POSTE,                          ~SECTION,                    ~SENS, ~ORDRE,
  "70",     "Chiffre d'affaires",            "Produits d'exploitation",       1,     10,
  "71",     "En-cours de fabrication",       "Produits d'exploitation",       1,     11,
  "72",     "Production immobilisée",        "Produits d'exploitation",       1,     12,
  "74",     "Autres produits d'exploitation","Produits d'exploitation",       1,     13,
  "609",    "Variation de stocks",           "Coût des ventes",              -1,     21,
  "60",     "Achats",                        "Coût des ventes",              -1,     20,
  "61",     "Services et biens divers",      "Charges d'exploitation",       -1,     30,
  "62",     "Rémunérations",                 "Charges d'exploitation",       -1,     31,
  "63",     "Amortissements",                "Charges d'exploitation",       -1,     32,
  "64",     "Autres charges d'exploitation", "Charges d'exploitation",       -1,     33,
  "66",     "Charges non récurrentes",       "Charges d'exploitation",       -1,     34,
  "76",     "Produits non récurrents",       "Produits d'exploitation",       1,     14,
  "75",     "Produits financiers",           "Résultat financier",            1,     40,
  "65",     "Charges financières",           "Résultat financier",           -1,     41,
  "67",     "Impôts",                        "Impôts",                       -1,     50,
  "77",     "Régularisations d'impôts",      "Impôts",                        1,     51
)

#' Soldes intermédiaires : chacun cumule les postes qui le précèdent.
#'
#' Déclaratif plutôt que déduit : c'est la définition comptable, elle ne dépend
#' d'aucun fichier.
SOLDES_PCMN <- tibble::tribble(
  ~SOLDE,                        ~JUSQU_A,
  "Marge brute d'exploitation",       21,
  "Résultat d'exploitation",          34,
  "Résultat avant impôts",            41,
  "Résultat de l'exercice",           51
)

#' Ajoute POSTE / SECTION / SENS à des lignes de compte, d'après leur numéro.
#'
#' @param db DB_COMPTA, ou toute table portant COMPTE et VALEUR.
#' @param comptes_seuls TRUE pour écarter les lignes de rubrique et de total de
#'   l'export, qui feraient double emploi avec leurs comptes.
classe_comptes <- function(db, comptes_seuls = TRUE) {
  if (is.null(db) || !nrow(db)) return(db)

  if (comptes_seuls) {
    if ("TYPE" %in% names(db)) db <- filter(db, TYPE == "compte")
    if ("AGREGE" %in% names(db)) db <- filter(db, AGREGE)
    db <- filter(db, !is.na(COMPTE))
  }
  if (!nrow(db)) return(db)

  # Test du plus long préfixe au plus court : 609 avant 60.
  ref <- PLAN_PCMN %>% arrange(desc(nchar(PREFIXE)))
  idx <- vapply(as.character(db$COMPTE), function(cp) {
    m <- which(startsWith(cp, ref$PREFIXE))
    if (length(m)) m[1] else NA_integer_
  }, integer(1), USE.NAMES = FALSE)

  db %>%
    mutate(POSTE      = ifelse(is.na(idx), "Non classé",      ref$POSTE[idx]),
           SECTION_G  = ifelse(is.na(idx), "Non classé",      ref$SECTION[idx]),
           SENS_G     = ifelse(is.na(idx), NA_real_,          ref$SENS[idx]),
           ORDRE_G    = ifelse(is.na(idx), 99L,               ref$ORDRE[idx]),
           PERIODE    = as.Date(sprintf("%04d-%02d-01",
                                        as.integer(ANNEE), as.integer(MOIS))))
}

#' Comptes que le plan ne sait pas classer.
#'
#' À regarder après chaque nouvel import : un compte non classé n'entre dans
#' aucun total, et c'est visible ici plutôt que sous forme d'un écart inexpliqué.
comptes_non_classes <- function(db) {
  classe_comptes(db) %>%
    filter(POSTE == "Non classé") %>%
    group_by(COMPTE, LIBELLE) %>%
    summarise(PERIODES = n_distinct(PERIODE), TOTAL = sum(VALEUR, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(abs(TOTAL)))
}

#' Compte de résultat par poste, une ligne par période x poste.
resultat_par_poste <- function(db, periodes = NULL) {
  d <- classe_comptes(db)
  if (is.null(d) || !nrow(d)) return(tibble())
  if (!is.null(periodes)) d <- filter(d, PERIODE %in% as.Date(periodes))
  d %>%
    group_by(PERIODE, ORDRE_G, SECTION_G, POSTE, SENS_G) %>%
    summarise(VALEUR = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
    arrange(PERIODE, ORDRE_G)
}

#' Détail d'un poste : ses comptes, pour le déroulé fin.
detail_poste <- function(db, poste, periodes = NULL) {
  d <- classe_comptes(db) %>% filter(POSTE == poste)
  if (!is.null(periodes)) d <- filter(d, PERIODE %in% as.Date(periodes))
  d %>%
    group_by(COMPTE, LIBELLE) %>%
    summarise(VALEUR = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
    filter(VALEUR != 0) %>%
    arrange(desc(abs(VALEUR)))
}

#' Compte de résultat complet : postes + soldes intermédiaires, par période.
compte_resultat <- function(db, periodes = NULL) {
  p <- resultat_par_poste(db, periodes)
  if (!nrow(p)) return(tibble())

  bind_rows(lapply(split(p, p$PERIODE), function(x) {
    x <- arrange(x, ORDRE_G)
    soldes <- lapply(seq_len(nrow(SOLDES_PCMN)), function(i) {
      jusqu <- SOLDES_PCMN$JUSQU_A[i]
      part <- filter(x, ORDRE_G <= jusqu)
      if (!nrow(part)) return(NULL)
      tibble(PERIODE = x$PERIODE[1], ORDRE_G = jusqu + 0.5,
             SECTION_G = "Solde", POSTE = SOLDES_PCMN$SOLDE[i],
             SENS_G = NA_real_,
             VALEUR = sum(part$SENS_G * part$VALEUR, na.rm = TRUE))
    })
    bind_rows(x, bind_rows(soldes)) %>% arrange(ORDRE_G)
  })) %>%
    mutate(TYPE_LIGNE = if_else(SECTION_G == "Solde", "solde", "poste"))
}

#' Périodes disponibles, de la plus récente à la plus ancienne.
periodes_compta <- function(db) {
  db %>%
    distinct(ANNEE, MOIS) %>%
    filter(!is.na(ANNEE), !is.na(MOIS)) %>%
    mutate(PERIODE = as.Date(sprintf("%04d-%02d-01",
                                     as.integer(ANNEE), as.integer(MOIS))),
           LIBELLE = format(PERIODE, "%B %Y")) %>%
    arrange(desc(PERIODE))
}

#' Comptes apparus ou disparus au fil du temps.
vie_des_comptes <- function(db) {
  classe_comptes(db) %>%
    filter(VALEUR != 0, !is.na(VALEUR)) %>%
    group_by(COMPTE, LIBELLE, POSTE) %>%
    summarise(PREMIERE = min(PERIODE), DERNIERE = max(PERIODE),
              PERIODES = n_distinct(PERIODE),
              TOTAL = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
    arrange(PREMIERE, COMPTE)
}

##### Rendus #####

#' Compte de résultat mis en forme, une colonne par période.
#'
#' @param detail TRUE pour dérouler les comptes sous chaque poste.
table_compte_resultat <- function(db, periodes, detail = FALSE, en_pct = FALSE) {
  cr <- compte_resultat(db, periodes)
  if (!nrow(cr)) return(tibble(Libellé = character()))

  lignes <- cr %>%
    transmute(ORDRE_G, PERIODE, TYPE_LIGNE, COMPTE = "",
              Libellé = if_else(TYPE_LIGNE == "solde", POSTE, paste0("▸ ", POSTE)),
              VALEUR = if_else(TYPE_LIGNE == "solde", VALEUR, SENS_G * VALEUR))

  if (detail) {
    d <- classe_comptes(db) %>%
      filter(PERIODE %in% as.Date(periodes)) %>%
      group_by(PERIODE, ORDRE_G, POSTE, COMPTE, LIBELLE, SENS_G) %>%
      summarise(VALEUR = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
      transmute(ORDRE_G = ORDRE_G + 0.1, PERIODE, TYPE_LIGNE = "compte",
                COMPTE, Libellé = paste0("     ", LIBELLE),
                VALEUR = SENS_G * VALEUR)
    lignes <- bind_rows(lignes, d)
  }

  base_ca <- cr %>% filter(POSTE == "Chiffre d'affaires") %>%
    select(PERIODE, CA = VALEUR)

  if (en_pct)
    lignes <- lignes %>% left_join(base_ca, by = "PERIODE") %>%
      mutate(VALEUR = if_else(!is.na(CA) & CA > 0, 100 * VALEUR / CA, NA_real_))

  fmt <- if (en_pct) function(x) format_pct(x) else function(x) format_CA(x, -1)

  lignes %>%
    mutate(P = format(PERIODE, "%Y-%m")) %>%
    group_by(ORDRE_G, Libellé, COMPTE, TYPE_LIGNE) %>%
    summarise(across(everything(), ~NULL), .groups = "drop") %>%
    left_join(
      lignes %>% mutate(P = format(PERIODE, "%Y-%m"), V = fmt(VALEUR)) %>%
        select(ORDRE_G, Libellé, COMPTE, P, V) %>%
        pivot_wider(names_from = P, values_from = V),
      by = c("ORDRE_G", "Libellé", "COMPTE")) %>%
    arrange(ORDRE_G) %>%
    select(-ORDRE_G, -TYPE_LIGNE) %>%
    rename(Compte = COMPTE)
}

#' Évolution des soldes intermédiaires sur les périodes retenues.
graph_soldes <- function(db, periodes) {
  cr <- compte_resultat(db, periodes)
  d <- cr %>% filter(TYPE_LIGNE == "solde")
  if (!nrow(d)) return(plotly_empty(type = "scatter", mode = "markers") %>%
                         layout(title = list(text = "Aucun solde à afficher")))
  lab <- format(d$PERIODE, "%b %Y")
  d$LAB <- factor(lab, levels = unique(lab[order(d$PERIODE)]))

  plot_ly(d, x = ~LAB, y = ~VALEUR, color = ~POSTE, type = "bar",
          colors = c(COUL_BRUN, COUL_AMBRE, COUL_VERT, COUL_NEUTRE),
          hovertemplate = ~paste0(POSTE, "<br>", format_CA(VALEUR, -1),
                                  "<extra></extra>")) %>%
    layout(barmode = "group", xaxis = list(title = ""),
           yaxis = list(title = "€", zeroline = TRUE, zerolinecolor = "#8d7b68"),
           legend = list(orientation = "h", y = -0.2), margin = list(b = 70))
}

#' Tuiles de synthèse pour la période la plus récente sélectionnée.
kpi_compta_generale <- function(db, periodes) {
  cr <- compte_resultat(db, periodes)
  if (!nrow(cr)) return(div(class = "text-muted small", "Aucune période."))
  derniere <- max(cr$PERIODE)
  d <- filter(cr, PERIODE == derniere)
  val <- function(nom) {
    x <- d$VALEUR[d$POSTE == nom]
    if (!length(x)) NA_real_ else x[1]
  }
  ca <- val("Chiffre d'affaires")
  tuile <- function(v, lib, icone) {
    kpi_tile(format_CA(v, -1), lib,
             if (is.na(v)) COUL_NEUTRE else if (v >= 0) COUL_VERT else COUL_ROUGE,
             icone,
             sous_titre = if (!is.na(ca) && ca > 0)
               paste0(format_pct(ratio_pct(v, ca)), " du CA") else NULL)
  }
  div(class = "kpi-grid",
      kpi_tile(format_CA(ca, -1), paste("CA —", format(derniere, "%B %Y")),
               COUL_BRUN, "euro-sign"),
      tuile(val("Marge brute d'exploitation"), "Marge brute", "layer-group"),
      tuile(val("Résultat d'exploitation"), "Résultat d'exploitation", "chart-line"),
      tuile(val("Résultat avant impôts"), "Résultat avant impôts", "scale-balanced"))
}
