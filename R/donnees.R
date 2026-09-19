# Stockage normalisé des tickets, et reconstruction au chargement.
#
# Le fichier .RData ne contient qu'une forme réduite de DB_TICKET :
#
#   DB_TICKET    : DATE, DATE_TS, HEURE, ID_TICKET, ID_REF, QUANTITE, PRIX_TOTAL
#   REF_PRODUITS : ID_REF -> ID_PRODUIT, PRODUIT, PRODUIT_FULL, BOISSON,
#                  CATEGORIE, TAUX_TVA, VOLUME_CL
#
# Ces six colonnes sont entièrement déterminées par la ligne du référentiel :
# les répéter sur 245 000 lignes coûtait 1,2 Mo, alors que le référentiel tient
# en quelques milliers de lignes et qu'une jointure au démarrage rend la table
# d'origine.
#
# Pourquoi DATE *et* DATE_TS : sur 3,2 % des lignes le TIMESTAMP ne tombe pas le
# même jour que DATE — les tickets d'après minuit sont rattachés au service de la
# veille, et quelques centaines de lignes portent un horodatage antérieur de
# plusieurs mois (bons cadeaux honorés plus tard). Or CD_PERIODE_SEMAINE se
# calcule sur le jour du TIMESTAMP : le déduire de DATE donnerait 2 136 lignes
# fausses. On garde donc les deux dates, qui coûtent 3 Ko chacune, au lieu du
# TIMESTAMP en texte qui en coûtait 124.

# Pourquoi ID_TICKET y est revenu
#
# Il avait été écarté avec le reste, et c'était cohérent : rien ne s'en servait.
# Le volet Détails en a désormais besoin — sans lui, deux lignes de la même
# minute sont indiscernables d'un même ticket, et « nombre de tickets » comme
# « panier moyen » ne se calculent pas du tout.
#
# Le coût a été mesuré plutôt que supposé : sur 245 000 lignes, la colonne
# ajoute 60 Ko au .RData compressé, soit 6 % de sa taille. Les identifiants
# croissent avec le temps, donc xz les réduit très bien. À ce prix-là, la
# question ne se pose pas.
#
# NB_CLIENTS coûterait 68 Ko de plus et donnerait le panier PAR PERSONNE. Il
# est resté dehors faute d'usage : le jour où un écran en aura besoin, il se
# rajoutera de la même façon.

# Pourquoi la clé du référentiel n'est PAS l'ID_PRODUIT
#
# La caisse laisse renommer une ligne : le même ID_PRODUIT y apparaît sous
# « Latte », « Latte deca », « Latte chaud »... Le référentiel portait alors
# plusieurs lignes pour cet ID, et la jointure de reconstruction, faite sur le
# seul ID_PRODUIT, rendait CHAQUE ligne de caisse autant de fois qu'il existait
# de libellés — un latte vendu une fois ressortait en six exemplaires, cinq
# d'entre eux sans TVA ni catégorie. Quantités et CA TVAC s'en trouvaient
# gonflés ; le CA HTVA, lui, survivait par accident, les lignes fantômes étant
# à NA et écartées par na.rm.
#
# La clé est donc un identifiant de LIGNE DU RÉFÉRENTIEL (ID_REF), une par
# combinaison d'attributs observée. La jointure redevient un à un, et
# hydrate_donnees() est de nouveau l'inverse exact de normalise_tickets().
# ID_REF remplace ID_PRODUIT dans la table stockée au lieu de s'y ajouter :
# l'ID du produit se relit dans le référentiel.

# Attributs déterminés par le produit vendu, et non par la vente elle-même.
REF_COLONNES <- c("ID_PRODUIT", "PRODUIT", "PRODUIT_FULL", "BOISSON",
                  "CATEGORIE", "TAUX_TVA", "VOLUME_CL")

# Colonnes conservées dans le .RData (le reste se recalcule).
TICKET_COLONNES <- c("DATE", "DATE_TS", "HEURE", "ID_TICKET", "ID_REF",
                     "QUANTITE", "PRIX_TOTAL")

# Réduit un DB_TICKET complet à sa forme stockable, et en extrait le référentiel.
# Renvoie les deux tables ; c'est l'inverse exact de hydrate_donnees().
normalise_tickets <- function(db_ticket) {
  cols <- intersect(REF_COLONNES, names(db_ticket))

  # La clé de regroupement est la combinaison complète des attributs, collée
  # avec un séparateur qu'aucun nom de produit ne contient. paste() rend "NA"
  # pour un NA, et c'est le comportement voulu : deux lignes également
  # inconnues appartiennent bien à la même entrée du référentiel.
  cle <- do.call(paste, c(unname(as.list(db_ticket[cols])), sep = "\u001f"))
  premiere <- !duplicated(cle)

  list(
    DB_TICKET = db_ticket %>%
      mutate(DATE_TS = as_date(ymd_hms(TIMESTAMP, quiet = TRUE)),
             HEURE   = as.integer(hour(ymd_hms(TIMESTAMP, quiet = TRUE))),
             ID_REF  = match(cle, cle[premiere])) %>%
      # any_of et non all_of : une source d'où ID_TICKET serait absent doit
      # produire un cache amputé plutôt que faire tomber tout l'import.
      select(any_of(TICKET_COLONNES)),
    REF_PRODUITS = db_ticket[premiere, cols, drop = FALSE] %>%
      as_tibble() %>%
      mutate(ID_REF = seq_len(n()), .before = 1)
  )
}

# Rattache chaque ligne de caisse à sa ligne de référentiel.
#
# Le cas normal est la jointure sur ID_REF, un à un par construction. Un .RData
# enregistré avant ID_REF ne porte que l'ID_PRODUIT : on ne peut alors joindre
# que sur lui, et il faut d'abord ramener le référentiel à une ligne par
# produit — sans quoi on reproduit exactement la duplication décrite plus haut.
# On retient la ligne la mieux renseignée : celle qui porte un taux de TVA.
joint_referentiel <- function(db_ticket, ref_produits) {
  if ("ID_REF" %in% names(db_ticket) && "ID_REF" %in% names(ref_produits))
    return(left_join(db_ticket, ref_produits, by = "ID_REF"))

  ref1 <- ref_produits %>%
    select(-any_of("ID_REF")) %>%
    arrange(ID_PRODUIT, is.na(TAUX_TVA), is.na(CATEGORIE)) %>%
    group_by(ID_PRODUIT) %>%
    slice(1) %>%
    ungroup()
  left_join(db_ticket, ref1, by = "ID_PRODUIT")
}

# Reconstruit DB_TICKET dans sa forme complète, puis TICKETS_HEURES qui s'en
# déduit. Appelée après chaque chargement ET à la fin de l'import : une seule
# définition de la forme complète, donc aucun risque que les deux divergent.
# Fonction pure : elle renvoie les deux tables, l'appelant les assigne.
hydrate_donnees <- function(db_ticket, ref_produits) {
  complet <- db_ticket %>%
    joint_referentiel(ref_produits) %>%
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
