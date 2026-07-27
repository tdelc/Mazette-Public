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
#  1) DB_COUTS_TRAVAIL — personnel, PAR JOUR, CRÉNEAU et SECTEUR
#     ----------------------------------------------------------------------
#     Granularité : une ligne par (DATE, CRENEAU, SECTEUR).
#     Colonnes :
#        DATE          (Date)   jour concerné
#        CRENEAU       (chr)    "Midi" / "Soir" pour le Service, "Journée" sinon
#        SECTEUR       (chr)    un des 4 secteurs ci-dessus
#        HEURES        (num)    heures prestées
#        TAUX_HORAIRE  (num)    coût horaire EMPLOYEUR moyen (brut + charges), €/h
#        COUT_TRAVAIL  (num)    = HEURES * TAUX_HORAIRE  (€)
#
#     Pourquoi un créneau ? Les heures de SERVICE sont directement liées à
#     l'ouverture d'un créneau : on peut donc les imputer à « Midi » ou
#     « Soir » et mesurer la productivité (CA par heure de service) créneau par
#     créneau. Les autres secteurs sont mutualisés sur la journée
#     (CRENEAU = "Journée") ; les helpers d'analyse les réimputent aux créneaux
#     au prorata du CA, comme dans l'étude de rentabilité.
#
#     Source réelle : export de l'outil de paie / pointeuse (badgeuse, planning
#     type Combo/Skello, fiches de prestation). Étapes :
#        a. exporter les heures prestées par employé et par jour ;
#        b. rattacher chaque employé à un secteur (table de correspondance
#           employé -> secteur ; un employé polyvalent se répartit au prorata) ;
#           l'ancienne DB_HEURES sépare Cuisine et Boulangerie : les deux se
#           regroupent en « Transformation alimentaire » ;
#        c. pour le service, découper les prestations à 17h (bornes du créneau)
#           et agréger par (DATE, CRENEAU) ; pour les autres secteurs, agréger
#           par (DATE) avec CRENEAU = "Journée" ;
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

# Ratios CIBLES (en part du CA HTVA) servant à calibrer les volumes fictifs.
# Les générateurs tirent des volumes au hasard puis les remettent à l'échelle
# pour atteindre ces cibles : les indicateurs affichés restent donc plausibles
# quel que soit le niveau réel du CA (et suivent le CA s'il évolue).
CIBLES_COMPTA <- c(work = 0.33, food = 0.26, general = 0.10)

# CA HTVA hebdomadaire de référence, utilisé si l'appelant n'en fournit pas.
CA_HEBDO_DEFAUT <- 10700

# Remet un vecteur de volumes à l'échelle pour atteindre un coût total cible.
# `plancher` garde au moins 1 unité là où il y avait de l'activité, pour ne pas
# faire disparaître un créneau entier à cause des arrondis.
recale_volumes <- function(heures, taux, cout_cible, plancher = TRUE) {
  cout_actuel <- sum(heures * taux, na.rm = TRUE)
  if (cout_actuel <= 0 || cout_cible <= 0) return(heures)
  h <- round(heures * cout_cible / cout_actuel)
  if (plancher) h <- ifelse(heures > 0 & h < 1, 1, h)
  pmax(0, h)
}

# Nombre de semaines couvertes par un vecteur de dates
nb_semaines <- function(dates) {
  max(1, dplyr::n_distinct(lubridate::floor_date(as.Date(dates), "week",
                                                 week_start = 1)))
}

# Libellé du poste "matière" propre à chaque secteur (pour l'affichage)
LIBELLE_MATIERE <- c(
  "Service"                    = "Boissons non alcoolisées",
  "Transformation alimentaire" = "Matières premières",
  "Brasserie"                  = "Matières premières brassin",
  "Support"                    = "Frais généraux"
)

# Profil d'ouverture : heures de SERVICE par jour de semaine et par créneau.
# min/max des heures tirées au sort. Reflète les horaires de Mazette :
# lundi fermé, mardi en soirée seulement, dimanche en midi seulement.
# Le dimanche est volontairement sur-staffé et le vendredi soir plutôt tendu,
# pour que la productivité horaire varie réellement d'un créneau à l'autre.
PROFIL_SERVICE <- tibble::tribble(
  ~.wday, ~CRENEAU, ~MIN, ~MAX,
  1L, "Midi",  0,  0,     # lundi : fermé
  1L, "Soir",  0,  0,
  2L, "Midi",  0,  0,     # mardi : ouverture à 17h
  2L, "Soir", 12, 17,
  3L, "Midi",  7, 11,     # mercredi
  3L, "Soir", 12, 17,
  4L, "Midi",  7, 11,     # jeudi
  4L, "Soir", 12, 17,
  5L, "Midi",  8, 12,     # vendredi
  5L, "Soir", 15, 20,
  6L, "Midi", 12, 17,     # samedi
  6L, "Soir", 16, 22,
  7L, "Midi", 24, 31,     # dimanche : ferme à 18h
  7L, "Soir",  0,  0
)

# --- Générateur 1 : coût du personnel, par jour, créneau et secteur ----------
# Les heures de SERVICE sont ventilées par créneau (Midi / Soir) : ce sont les
# heures directement liées à l'ouverture d'un créneau. Les autres secteurs sont
# mutualisés sur la journée (CRENEAU = "Journée") et seront réimputés aux
# créneaux au prorata du CA par les helpers d'analyse.
generer_couts_travail <- function(dates, ca_hebdo = CA_HEBDO_DEFAUT, seed = 42) {
  set.seed(seed)

  taux <- function(secteur, n) {
    switch(secteur,
      "Service"                    = round(runif(n, 15, 18), 2),
      "Transformation alimentaire" = round(runif(n, 16, 19), 2),
      "Brasserie"                  = round(runif(n, 17, 21), 2),
      "Support"                    = round(runif(n, 20, 25), 2))
  }

  # Heures de service, par créneau
  service <- tidyr::expand_grid(DATE = as.Date(dates),
                                CRENEAU = c("Midi", "Soir")) %>%
    dplyr::mutate(.wday = lubridate::wday(DATE, week_start = 1)) %>%
    dplyr::left_join(PROFIL_SERVICE, by = c(".wday", "CRENEAU")) %>%
    dplyr::mutate(SECTEUR = "Service",
                  HEURES  = round(runif(dplyr::n(), MIN, MAX))) %>%
    dplyr::filter(HEURES > 0)

  # Heures des autres secteurs, au niveau de la journée
  autres <- tidyr::expand_grid(
      DATE = as.Date(dates),
      SECTEUR = setdiff(SECTEURS_COMPTA, "Service")) %>%
    dplyr::mutate(
      .wday    = lubridate::wday(DATE, week_start = 1),
      .weekend = .wday >= 5,
      CRENEAU  = "Journée",
      HEURES = dplyr::case_when(
        # Transformation alimentaire : prépa des recettes, assez lissée
        SECTEUR == "Transformation alimentaire" ~
          round(ifelse(.weekend, runif(dplyr::n(), 13, 19), runif(dplyr::n(), 8, 13))),
        # Brasserie : brassins ponctuels, surtout en début de semaine
        SECTEUR == "Brasserie" ~
          round(ifelse(.wday <= 4, runif(dplyr::n(), 3, 7), runif(dplyr::n(), 0, 3))),
        # Support : gestion, du lundi au vendredi
        SECTEUR == "Support" ~
          round(ifelse(.wday <= 5, runif(dplyr::n(), 2, 5), 0))
      ))

  dplyr::bind_rows(service, autres) %>%
    dplyr::group_by(SECTEUR) %>%
    dplyr::mutate(TAUX_HORAIRE = taux(dplyr::first(SECTEUR), dplyr::n())) %>%
    dplyr::ungroup() %>%
    # Recalage sur le CA de référence : le coût du travail doit peser la part
    # cible du CA (les proportions entre secteurs sont conservées).
    dplyr::mutate(
      HEURES = recale_volumes(
        HEURES, TAUX_HORAIRE,
        cout_cible = ca_hebdo * nb_semaines(dates) * CIBLES_COMPTA[["work"]]),
      COUT_TRAVAIL = round(HEURES * TAUX_HORAIRE, 2),
      SIMU = TRUE) %>%
    dplyr::filter(HEURES > 0) %>%
    dplyr::arrange(DATE, SECTEUR, CRENEAU) %>%
    dplyr::select(DATE, CRENEAU, SECTEUR, HEURES, TAUX_HORAIRE, COUT_TRAVAIL, SIMU)
}

# --- Générateur 2 : coût matière / frais généraux, par semaine et par secteur -
generer_couts_matiere <- function(dates, ca_hebdo = CA_HEBDO_DEFAUT, seed = 7) {
  set.seed(seed)
  semaines <- sort(unique(lubridate::floor_date(as.Date(dates), "week", week_start = 1)))
  tidyr::expand_grid(SEMAINE = semaines, SECTEUR = SECTEURS_COMPTA) %>%
    dplyr::mutate(
      # Volumes bruts, remis à l'échelle plus bas sur les ratios cibles
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
    # Recalage sur le CA de référence : les matières « métier » visent le food
    # cost cible, les frais généraux du Support leur propre cible.
    dplyr::mutate(.grp = ifelse(SECTEUR == "Support", "general", "food")) %>%
    dplyr::group_by(.grp) %>%
    dplyr::mutate(
      .f = (ca_hebdo * length(semaines) * CIBLES_COMPTA[[dplyr::first(.grp)]]) /
        sum(COUT_MATIERE),
      dplyr::across(c(ACHATS, STOCK_DEBUT, VARIATION_STOCK), ~round(. * .f))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(STOCK_FIN    = STOCK_DEBUT - VARIATION_STOCK,
                  COUT_MATIERE = ACHATS + VARIATION_STOCK,
                  SIMU = TRUE) %>%
    dplyr::select(SEMAINE, SECTEUR, ACHATS, STOCK_DEBUT, STOCK_FIN,
                  VARIATION_STOCK, COUT_MATIERE, SIMU)
}
