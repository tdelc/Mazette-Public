# =============================================================================
#  DONNÉES FICTIVES — COMPTA / RAPPORTS DE GESTION  (ébauche Phase 6)
# =============================================================================
#
#  OBJET
#  -----
#  Tant qu'on ne dispose pas de sources fiables pour les COÛTS et les STOCKS,
#  on simule deux bases plausibles, calées sur le calendrier réel, afin de
#  prototyper l'onglet « Compta ». Le calcul du profit y est :
#
#       PROFIT = CA (HTVA)  −  coût de la main d'œuvre  −  consommation matières
#
#  Ce fichier contient (1) les deux générateurs de DB fictives et
#  (2) un TUTORIEL expliquant comment, demain, brancher les vraies données.
#
#  -----------------------------------------------------------------------------
#  TUTORIEL — COMMENT CONSTRUIRE CES DEUX BASES (vraie version)
#  -----------------------------------------------------------------------------
#
#  1) DB_COUTS_HORAIRES — coûts de main d'œuvre, PAR JOUR et PAR SECTEUR
#     ----------------------------------------------------------------------
#     Granularité : une ligne par (DATE, SECTEUR).
#     Trois secteurs métier : « Service », « Transformation alimentaire »,
#     « Administration ».
#     Colonnes :
#        DATE          (Date)     jour concerné
#        SECTEUR       (chr)      service / transfo. alim. / administration
#        HEURES        (num)      heures prestées ce jour-là dans le secteur
#        TAUX_HORAIRE  (num)      coût horaire EMPLOYEUR moyen (brut + charges), €/h
#        COUT          (num)      = HEURES * TAUX_HORAIRE  (€)
#
#     Source réelle : export de l'outil de paie / pointeuse (ex. badgeuse,
#     planning Combo/Skello, fiches de prestation). Étapes :
#        a. exporter les heures prestées par employé et par jour ;
#        b. rattacher chaque employé à un secteur (table de correspondance) ;
#        c. agréger les heures par (DATE, SECTEUR) ;
#        d. multiplier par le coût horaire chargé du secteur (ou par employé,
#           puis sommer) pour obtenir COUT.
#     => On obtient exactement le schéma ci-dessus.
#
#  2) DB_CONSO_MP — consommation de matières premières, PAR SEMAINE et PAR SECTEUR
#     ----------------------------------------------------------------------
#     Hypothèse de travail : la gestion fine des stocks est faite AILLEURS.
#     Ici on ne suit QUE la VARIATION DE STOCK sur la semaine, que l'on assimile
#     à la consommation de la semaine (réappros déjà retraités en amont).
#     Granularité : une ligne par (SEMAINE, SECTEUR).
#     Deux secteurs de stock : « Nourriture », « Boisson ».
#     Colonnes :
#        SEMAINE          (Date)  premier jour (lundi) de la semaine
#        SECTEUR          (chr)   nourriture / boisson
#        STOCK_DEBUT      (num)   valeur du stock en début de semaine (€)
#        STOCK_FIN        (num)   valeur du stock en fin de semaine (€)
#        VARIATION_STOCK  (num)   = STOCK_DEBUT − STOCK_FIN  (€ consommés)
#        CONSO            (num)   = VARIATION_STOCK  (alias lisible)
#
#     Source réelle : inventaire hebdomadaire valorisé (feuille de comptage du
#     dimanche soir, ou module de stock). Étapes :
#        a. valoriser le stock en début et en fin de semaine ;
#        b. CONSO = stock_debut + achats_semaine − stock_fin  (formule complète) ;
#           dans cette ébauche on néglige les achats (gérés ailleurs) :
#           CONSO ≈ stock_debut − stock_fin = VARIATION_STOCK ;
#        c. répartir entre « Nourriture » et « Boisson » selon les familles
#           d'articles de l'inventaire.
#
#  3) BRANCHER LE VRAI À LA PLACE DU FICTIF
#     ----------------------------------------------------------------------
#     Il suffira de produire deux data.frames respectant EXACTEMENT les colonnes
#     ci-dessus (depuis import.R / nettoyage_ajout.R) et de remplacer, dans
#     server.R, les appels `generer_couts_horaires()` / `generer_conso_mp()` par
#     les vraies tables. Les helpers d'analyse (agrege_compta, graph_compta,
#     table_compta_aff, graph_compta_secteurs dans functions.R) restent inchangés.
#
# =============================================================================

# --- Générateur 1 : coûts horaires par jour et par secteur -------------------
generer_couts_horaires <- function(dates, seed = 42) {
  set.seed(seed)
  secteurs <- c("Service", "Transformation alimentaire", "Administration")
  tidyr::expand_grid(DATE = as.Date(dates), SECTEUR = secteurs) %>%
    dplyr::mutate(
      .wday    = lubridate::wday(DATE, week_start = 1),   # 1 = lundi ... 7 = dimanche
      .weekend = .wday >= 5,                              # ven., sam., dim. = plus de monde
      HEURES = dplyr::case_when(
        SECTEUR == "Service" ~
          round(ifelse(.weekend, runif(dplyr::n(), 30, 42), runif(dplyr::n(), 16, 28))),
        SECTEUR == "Transformation alimentaire" ~
          round(ifelse(.weekend, runif(dplyr::n(), 16, 24), runif(dplyr::n(), 8, 16))),
        SECTEUR == "Administration" ~
          round(ifelse(.wday <= 5, runif(dplyr::n(), 2, 6), 0))   # admin du lundi au vendredi
      ),
      TAUX_HORAIRE = dplyr::case_when(
        SECTEUR == "Service"                    ~ round(runif(dplyr::n(), 15, 18), 2),
        SECTEUR == "Transformation alimentaire" ~ round(runif(dplyr::n(), 16, 19), 2),
        SECTEUR == "Administration"             ~ round(runif(dplyr::n(), 20, 25), 2)
      ),
      COUT = round(HEURES * TAUX_HORAIRE, 2)
    ) %>%
    dplyr::select(DATE, SECTEUR, HEURES, TAUX_HORAIRE, COUT)
}

# --- Générateur 2 : consommation matières (hebdo) par secteur ----------------
generer_conso_mp <- function(dates, seed = 7) {
  set.seed(seed)
  semaines <- sort(unique(lubridate::floor_date(as.Date(dates), "week", week_start = 1)))
  secteurs <- c("Nourriture", "Boisson")
  tidyr::expand_grid(SEMAINE = semaines, SECTEUR = secteurs) %>%
    dplyr::mutate(
      STOCK_DEBUT = dplyr::case_when(
        SECTEUR == "Nourriture" ~ round(runif(dplyr::n(), 3000, 4500)),
        SECTEUR == "Boisson"    ~ round(runif(dplyr::n(), 2500, 4000))
      ),
      # Variation de stock sur la semaine = ce qui a été consommé
      VARIATION_STOCK = dplyr::case_when(
        SECTEUR == "Nourriture" ~ round(runif(dplyr::n(), 1500, 2600)),
        SECTEUR == "Boisson"    ~ round(runif(dplyr::n(), 1200, 2100))
      ),
      STOCK_FIN = STOCK_DEBUT - VARIATION_STOCK,
      CONSO     = VARIATION_STOCK
    ) %>%
    dplyr::select(SEMAINE, SECTEUR, STOCK_DEBUT, STOCK_FIN, VARIATION_STOCK, CONSO)
}
