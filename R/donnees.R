# Stockage normalisé des tickets, et reconstruction au chargement.
#
# Le fichier .RData ne contient qu'une forme réduite de DB_TICKET :
#
#   DB_TICKET    : DATE, DATE_TS, HEURE, ID_PRODUIT, QUANTITE, PRIX_TOTAL
#   REF_PRODUITS : ID_PRODUIT -> PRODUIT, PRODUIT_FULL, BOISSON, CATEGORIE,
#                  TAUX_TVA, VOLUME_CL
#
# Ces six colonnes sont entièrement déterminées par ID_PRODUIT (843 valeurs
# distinctes) : les répéter sur 245 000 lignes coûtait 1,2 Mo. Le référentiel
# tient en 843 lignes, et une jointure au démarrage rend la table d'origine.
#
# Pourquoi DATE *et* DATE_TS : sur 3,2 % des lignes le TIMESTAMP ne tombe pas le
# même jour que DATE — les tickets d'après minuit sont rattachés au service de la
# veille, et quelques centaines de lignes portent un horodatage antérieur de
# plusieurs mois (bons cadeaux honorés plus tard). Or CD_PERIODE_SEMAINE se
# calcule sur le jour du TIMESTAMP : le déduire de DATE donnerait 2 136 lignes
# fausses. On garde donc les deux dates, qui coûtent 3 Ko chacune, au lieu du
# TIMESTAMP en texte qui en coûtait 124.

# Colonnes conservées dans le .RData (le reste se recalcule).
TICKET_COLONNES <- c("DATE", "DATE_TS", "HEURE", "ID_PRODUIT",
                     "QUANTITE", "PRIX_TOTAL")

# Réduit un DB_TICKET complet à sa forme stockable, et en extrait le référentiel.
# Renvoie les deux tables ; c'est l'inverse exact de hydrate_donnees().
normalise_tickets <- function(db_ticket) {
  list(
    DB_TICKET = db_ticket %>%
      mutate(DATE_TS = as_date(ymd_hms(TIMESTAMP, quiet = TRUE)),
             HEURE   = as.integer(hour(ymd_hms(TIMESTAMP, quiet = TRUE)))) %>%
      select(all_of(TICKET_COLONNES)),
    REF_PRODUITS = db_ticket %>%
      distinct(ID_PRODUIT, PRODUIT, PRODUIT_FULL, BOISSON, CATEGORIE,
               TAUX_TVA, VOLUME_CL)
  )
}

# Reconstruit DB_TICKET dans sa forme complète, puis TICKETS_HEURES qui s'en
# déduit. Appelée après chaque chargement ET à la fin de l'import : une seule
# définition de la forme complète, donc aucun risque que les deux divergent.
# Fonction pure : elle renvoie les deux tables, l'appelant les assigne.
hydrate_donnees <- function(db_ticket, ref_produits) {
  complet <- db_ticket %>%
    left_join(ref_produits, by = "ID_PRODUIT") %>%
    mutate(
      # heure_service() et hour() n'ont besoin que de l'heure : on recompose un
      # POSIXct à la minute près nulle, ce qui suffit à tous les appelants.
      TIMESTAMP          = as.POSIXct(DATE_TS) + hours(HEURE),
      CD_HEURE           = if_else(HEURE < 17, "Midi (<17h)", "Soir (>=17h)"),
      CD_SECTEUR         = if_else(TAUX_TVA == 0.12, "Nourriture", "Boisson"),
      CD_PERIODE_JOUR    = if_else(HEURE %in% 8:16, "Jour", "Soir"),
      CD_PERIODE_SEMAINE = if_else(
        wday(DATE_TS, week_start = 1) %in% c(6, 7)
        | (wday(DATE_TS, week_start = 1) == 5 & CD_PERIODE_JOUR == "Soir"),
        "Week-end", "Semaine"),
      VOLUME_TOT_L       = QUANTITE * VOLUME_CL / 100,
      CA_TVAC = PRIX_TOTAL,
      CA_HTVA = CA_TVAC / (1 + TAUX_TVA)
    )

  list(DB_TICKET = complet, TICKETS_HEURES = tickets_heures(complet))
}

# Applique l'hydratation dans un environnement donné : utilisé après chaque
# chargement de .RData, où les tables arrivent sous leur forme réduite.
hydrate_dans <- function(envir) {
  ticket <- get0("DB_TICKET", envir = envir, inherits = FALSE)
  if (is.null(ticket)) return(invisible(FALSE))
  # Un .RData d'avant la normalisation contient déjà la forme complète.
  if ("TIMESTAMP" %in% names(ticket)) return(invisible(FALSE))

  h <- hydrate_donnees(ticket, get("REF_PRODUITS", envir = envir))
  assign("DB_TICKET",      h$DB_TICKET,      envir = envir)
  assign("TICKETS_HEURES", h$TICKETS_HEURES, envir = envir)
  invisible(TRUE)
}

# Ventes agrégées par jour x créneau x produit. Recalculée au démarrage plutôt
# que stockée : 70 000 lignes de plus dans le fichier pour ~150 ms de calcul.
tickets_heures <- function(db_ticket) {
  db_ticket %>%
    filter(PRIX_TOTAL > 0) %>%
    group_by(DATE, CD_HEURE, CD_SECTEUR,
             CD_PERIODE_JOUR, CD_PERIODE_SEMAINE,
             PRODUIT_FULL, PRODUIT, CATEGORIE, TAUX_TVA) %>%
    summarise(CA_TVAC = sum(PRIX_TOTAL), QUANTITE = sum(QUANTITE),
              .groups = "drop") %>%
    mutate(CA_HTVA = CA_TVAC / (1 + TAUX_TVA))
}
