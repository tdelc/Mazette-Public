# =============================================================================
#  DONNÉES FICTIVES — COMPTA / RAPPORTS DE GESTION
# =============================================================================
#
#  OBJET
#  -----
#  Tant qu'on ne dispose pas de sources fiables pour les COÛTS et les STOCKS,
#  on simule deux bases plausibles, calées sur le calendrier réel, afin de
#  prototyper l'onglet « Compta ». Les indicateurs calculés sont ceux de la
#  restauration :
#
#       Food Cost   = coût des matières        / CA HTVA
#       Work Cost   = coût du personnel        / CA HTVA
#       Prime Cost  = (matières + personnel)   / CA HTVA
#       Marge       = CA HTVA − matières − personnel
#
#  QUATRE SECTEURS, jamais agrégés entre eux. Chacun porte SES PROPRES coûts
#  de personnel et de matière :
#
#     | Secteur                    | Personnel          | Matière / achats         |
#     |----------------------------|--------------------|--------------------------|
#     | Service                    | staff au bar       | boissons non alcoolisées |
#     | Transformation alimentaire | staff des recettes | matières premières food  |
#     | Brasserie                  | staff des brassins | matières premières bière |
#     | Support                    | staff de gestion   | frais généraux           |
#
#  -----------------------------------------------------------------------------
#  TUTORIEL — COMMENT CONSTRUIRE CES DEUX BASES (vraie version)
#  -----------------------------------------------------------------------------
#
#  1) DB_COUTS_TRAVAIL — personnel, PAR JOUR et PAR SECTEUR
#     ----------------------------------------------------------------------
#     Granularité : une ligne par (DATE, SECTEUR).
#     Colonnes :
#        DATE          (Date)   jour concerné
#        SECTEUR       (chr)    un des 4 secteurs ci-dessus
#        HEURES        (num)    heures prestées ce jour-là dans le secteur
#        TAUX_HORAIRE  (num)    coût horaire EMPLOYEUR moyen (brut + charges), €/h
#        COUT_TRAVAIL  (num)    = HEURES * TAUX_HORAIRE  (€)
#
#     Source réelle : export de l'outil de paie / pointeuse (badgeuse, planning
#     type Combo/Skello, fiches de prestation). Étapes :
#        a. exporter les heures prestées par employé et par jour ;
#        b. rattacher chaque employé à un secteur (table de correspondance
#           employé -> secteur ; un employé polyvalent se répartit au prorata) ;
#        c. agréger les heures par (DATE, SECTEUR) ;
#        d. multiplier par le coût horaire chargé (par employé puis sommer, ou
#           taux moyen du secteur) pour obtenir COUT_TRAVAIL.
#
#  2) DB_COUTS_MATIERE — matières & frais généraux, PAR SEMAINE et PAR SECTEUR
#     ----------------------------------------------------------------------
#     Hypothèse de travail : la gestion fine des stocks est faite AILLEURS.
#     Ici on raisonne à la semaine :
#
#         CONSO = ACHATS + (STOCK_DEBUT − STOCK_FIN)
#                          \_____ VARIATION_STOCK _____/
#
#     Granularité : une ligne par (SEMAINE, SECTEUR).
#     Colonnes :
#        SEMAINE          (Date)  premier jour (lundi) de la semaine
#        SECTEUR          (chr)   un des 4 secteurs
#        ACHATS           (num)   factures fournisseurs de la semaine (€)
#        STOCK_DEBUT      (num)   stock valorisé en début de semaine (€)
#        STOCK_FIN        (num)   stock valorisé en fin de semaine (€)
#        VARIATION_STOCK  (num)   = STOCK_DEBUT − STOCK_FIN (>0 = déstockage)
#        COUT_MATIERE     (num)   = ACHATS + VARIATION_STOCK  (consommation)
#
#     Source réelle : factures fournisseurs (ventilées par secteur) + inventaire
#     hebdomadaire valorisé (comptage du dimanche soir, ou module de stock).
#     Cas particulier du Support : pas de stock, COUT_MATIERE = frais généraux
#     de la semaine (loyer au prorata, énergie, assurances, comptable, etc.).
#
#  3) BRANCHER LE VRAI À LA PLACE DU FICTIF
#     ----------------------------------------------------------------------
#     Produire deux data.frames respectant EXACTEMENT les colonnes ci-dessus
#     (depuis import.R / nettoyage_ajout.R), puis remplacer dans server.R les
#     appels `generer_couts_travail()` / `generer_couts_matiere()` par les
#     vraies tables. Tous les helpers d'analyse (agrege_compta, kpi_compta,
#     compta_secteurs, ...) restent inchangés.
#
# =============================================================================

# Les 4 secteurs, dans l'ordre d'affichage. Jamais agrégés entre eux.
SECTEURS_COMPTA <- c("Service", "Transformation alimentaire",
                     "Brasserie", "Support")

# Libellé du poste "matière" propre à chaque secteur (pour l'affichage)
LIBELLE_MATIERE <- c(
  "Service"                    = "Boissons non alcoolisées",
  "Transformation alimentaire" = "Matières premières",
  "Brasserie"                  = "Matières premières brassin",
  "Support"                    = "Frais généraux"
)

# --- Générateur 1 : coût du personnel, par jour et par secteur ---------------
generer_couts_travail <- function(dates, seed = 42) {
  set.seed(seed)
  tidyr::expand_grid(DATE = as.Date(dates), SECTEUR = SECTEURS_COMPTA) %>%
    dplyr::mutate(
      .wday    = lubridate::wday(DATE, week_start = 1),  # 1 = lundi ... 7 = dimanche
      .weekend = .wday >= 5,                             # ven./sam./dim. = plus de monde
      # Volumes calibrés pour donner des ratios réalistes (work cost ~33 % du CA)
      HEURES = dplyr::case_when(
        # Service : staff au bar, très sensible à l'affluence
        SECTEUR == "Service" ~
          round(ifelse(.weekend, runif(dplyr::n(), 24, 32), runif(dplyr::n(), 14, 22))),
        # Cuisine : prépa des recettes, un peu plus lissée
        SECTEUR == "Transformation alimentaire" ~
          round(ifelse(.weekend, runif(dplyr::n(), 13, 19), runif(dplyr::n(), 8, 13))),
        # Brasserie : brassins ponctuels, surtout en début de semaine
        SECTEUR == "Brasserie" ~
          round(ifelse(.wday <= 4, runif(dplyr::n(), 3, 7), runif(dplyr::n(), 0, 3))),
        # Support : gestion, du lundi au vendredi
        SECTEUR == "Support" ~
          round(ifelse(.wday <= 5, runif(dplyr::n(), 2, 5), 0))
      ),
      TAUX_HORAIRE = dplyr::case_when(
        SECTEUR == "Service"                    ~ round(runif(dplyr::n(), 15, 18), 2),
        SECTEUR == "Transformation alimentaire" ~ round(runif(dplyr::n(), 16, 19), 2),
        SECTEUR == "Brasserie"                  ~ round(runif(dplyr::n(), 17, 21), 2),
        SECTEUR == "Support"                    ~ round(runif(dplyr::n(), 20, 25), 2)
      ),
      COUT_TRAVAIL = round(HEURES * TAUX_HORAIRE, 2)
    ) %>%
    dplyr::select(DATE, SECTEUR, HEURES, TAUX_HORAIRE, COUT_TRAVAIL)
}

# --- Générateur 2 : coût matière / frais généraux, par semaine et par secteur -
generer_couts_matiere <- function(dates, seed = 7) {
  set.seed(seed)
  semaines <- sort(unique(lubridate::floor_date(as.Date(dates), "week", week_start = 1)))
  tidyr::expand_grid(SEMAINE = semaines, SECTEUR = SECTEURS_COMPTA) %>%
    dplyr::mutate(
      # Calibrage : food cost ~25 % du CA, frais généraux ~10 %
      ACHATS = dplyr::case_when(
        SECTEUR == "Service"                    ~ round(runif(dplyr::n(), 600, 1200)),
        SECTEUR == "Transformation alimentaire" ~ round(runif(dplyr::n(), 1700, 2500)),
        SECTEUR == "Brasserie"                  ~ round(runif(dplyr::n(), 350, 850)),
        SECTEUR == "Support"                    ~ round(runif(dplyr::n(), 1200, 1800))
      ),
      STOCK_DEBUT = dplyr::case_when(
        SECTEUR == "Service"                    ~ round(runif(dplyr::n(), 1800, 2600)),
        SECTEUR == "Transformation alimentaire" ~ round(runif(dplyr::n(), 2600, 3800)),
        SECTEUR == "Brasserie"                  ~ round(runif(dplyr::n(), 2000, 3200)),
        SECTEUR == "Support"                    ~ 0   # pas de stock sur le support
      ),
      # Variation de stock sur la semaine (>0 = on a puisé dans le stock)
      VARIATION_STOCK = dplyr::case_when(
        SECTEUR == "Support" ~ 0,
        TRUE ~ round(runif(dplyr::n(), -350, 500))
      ),
      STOCK_FIN    = STOCK_DEBUT - VARIATION_STOCK,
      # Consommation réelle de la période = achats + déstockage
      COUT_MATIERE = ACHATS + VARIATION_STOCK
    ) %>%
    dplyr::select(SEMAINE, SECTEUR, ACHATS, STOCK_DEBUT, STOCK_FIN,
                  VARIATION_STOCK, COUT_MATIERE)
}
