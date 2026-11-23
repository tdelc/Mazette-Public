# Stockage normalisé des tickets, et reconstruction au chargement.
#
# Le cache ne contient qu'une forme réduite de DB_TICKET :
#
#   DB_TICKET    : DATE, DATE_TS, HEURE, ID_TICKET, ID_REF, QUANTITE, PRIX_TOTAL
#   REF_PRODUITS : ID_REF -> ID_PRODUIT, PRODUIT, PRODUIT_FULL, BOISSON,
#                  CATEGORIE, TAUX_TVA, VOLUME_CL
#
# Les attributs du produit vivent une fois dans le référentiel, et une
# jointure au démarrage rend la table complète.
#
# DATE et DATE_TS diffèrent sur quelques pour cent des lignes : DATE est le
# jour de service (un ticket après minuit appartient à la veille), DATE_TS le
# jour de l'horodatage, sur lequel se calcule CD_PERIODE_SEMAINE.
#
# La clé du référentiel est ID_REF, une par combinaison d'attributs observée,
# et non ID_PRODUIT : la caisse laisse renommer une ligne, un même ID_PRODUIT
# porte plusieurs libellés, et une jointure sur lui dupliquerait les ventes.


# Attributs déterminés par le produit vendu, et non par la vente elle-même.
REF_COLONNES <- c("ID_PRODUIT", "PRODUIT", "PRODUIT_FULL", "BOISSON",
                  "CATEGORIE", "TAUX_TVA", "VOLUME_CL")

# Taux de TVA de la nourriture : 12 % sur place, 6 % à emporter.
TAUX_NOURRITURE <- c(0.06, 0.12)

# Colonnes conservées dans le .RData (le reste se recalcule).
TICKET_COLONNES <- c("DATE", "DATE_TS", "HEURE", "ID_TICKET", "ID_REF",
                     "QUANTITE", "PRIX_TOTAL")

# Réduit un DB_TICKET complet à sa forme stockable, et en extrait le référentiel.
# Renvoie les deux tables ; c'est l'inverse exact de hydrate_donnees().
normalise_tickets <- function(db_ticket) {
  cols <- intersect(REF_COLONNES, names(db_ticket))

  # Clé = combinaison complète des attributs. paste() rend "NA" pour un NA :
  # deux lignes également inconnues partagent la même entrée.
  cle <- do.call(paste, c(unname(as.list(db_ticket[cols])), sep = "\u001f"))
  premiere <- !duplicated(cle)

  list(
    DB_TICKET = db_ticket %>%
      mutate(DATE_TS = as_date(ymd_hms(TIMESTAMP, quiet = TRUE)),
             HEURE   = as.integer(hour(ymd_hms(TIMESTAMP, quiet = TRUE))),
             ID_REF  = match(cle, cle[premiere])) %>%
      # any_of : une source sans ID_TICKET donne un cache amputé, pas une erreur.
      select(any_of(TICKET_COLONNES)),
    REF_PRODUITS = db_ticket[premiere, cols, drop = FALSE] %>%
      as_tibble() %>%
      mutate(ID_REF = seq_len(n()), .before = 1)
  )
}

# Rattache chaque ligne de caisse à sa ligne de référentiel : sur ID_REF
# (un à un), ou sur ID_PRODUIT pour un cache ancien, après avoir ramené le
# référentiel à une ligne par produit (celle qui porte un taux de TVA).
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

# Reconstruit DB_TICKET dans sa forme complète, puis TICKETS_HEURES. Appelée
# après chaque chargement et à la fin de l'import. Fonction pure.
hydrate_donnees <- function(db_ticket, ref_produits) {
  complet <- db_ticket %>%
    joint_referentiel(ref_produits) %>%
    mutate(
      # Un POSIXct à l'heure près suffit aux appelants.
      TIMESTAMP          = as.POSIXct(DATE_TS) + hours(HEURE),
      CD_HEURE           = if_else(HEURE < 17, "Midi (<17h)", "Soir (>=17h)"),
      CD_SECTEUR         = if_else(TAUX_TVA %in% TAUX_NOURRITURE,
                                   "Nourriture", "Boisson"),
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

# Applique l'hydratation dans un environnement donné, après un load().
hydrate_dans <- function(envir) {
  ticket <- get0("DB_TICKET", envir = envir, inherits = FALSE)
  if (is.null(ticket)) return(invisible(FALSE))
  # Un cache ancien contient déjà la forme complète.
  if ("TIMESTAMP" %in% names(ticket)) return(invisible(FALSE))

  h <- hydrate_donnees(ticket, get("REF_PRODUITS", envir = envir))
  assign("DB_TICKET",      h$DB_TICKET,      envir = envir)
  assign("TICKETS_HEURES", h$TICKETS_HEURES, envir = envir)
  invisible(TRUE)
}

# Ventes agrégées par jour × créneau × produit, recalculées au démarrage.
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

# ##### La nomenclature des produits #####


# Le libellé canonique d'un produit : le plus fréquent (« Latte » plutôt que
# « Latte AVOINE »), l'ordre alphabétique départageant les égalités.
libelle_canonique <- function(db_ticket) {
  db_ticket %>%
    count(ID_PRODUIT, PRODUIT) %>%
    arrange(ID_PRODUIT, desc(n), PRODUIT) %>%
    group_by(ID_PRODUIT) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(ID_PRODUIT, PRODUIT)
}

# Le taux de TVA d'un produit, cherché sur tous ses libellés : l'export
# produits est indexé par libellé. En cas de désaccord, le plus vendu tranche.
tva_par_produit <- function(db_ticket, produits_ref) {
  db_ticket %>%
    count(ID_PRODUIT, PRODUIT_FULL = PRODUIT) %>%
    inner_join(produits_ref, by = "PRODUIT_FULL") %>%
    filter(!is.na(TAUX_TVA)) %>%
    arrange(ID_PRODUIT, desc(n)) %>%
    group_by(ID_PRODUIT) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(ID_PRODUIT, TAUX_TVA, CATEGORIE)
}

# Les remises de la caisse, et leur taux de TVA.
#
# Une remise sort de la caisse comme une ligne négative nommée d'après le
# produit remisé (« 20% sur produit : Espresso »), libellé absent de l'export
# produits. Elle prend le taux du produit remisé, cherché dans l'export puis
# parmi les libellés de caisse ; à défaut, 21 %.
#
# Pas de CATÉGORIE : une remise n'est pas un produit vendu, et compterait
# sinon dans les comptages par catégorie. Le taux suffit à la ranger du bon
# côté des secteurs.
MOTIF_REMISE <- "^\\s*[0-9]+(?:[.,][0-9]+)?\\s*%\\s*sur\\s+produit\\s*:\\s*"
TAUX_REMISE_DEFAUT <- 0.21

# Le produit visé par une remise, ou NA si le libellé n'est pas une remise.
produit_remise <- function(libelle) {
  est <- grepl(MOTIF_REMISE, libelle, ignore.case = TRUE, perl = TRUE)
  cible <- trimws(sub(MOTIF_REMISE, "", libelle, ignore.case = TRUE, perl = TRUE))
  ifelse(est & nzchar(cible), cible, NA_character_)
}

# Le taux des remises encore sans taux, sous la forme de tva_par_produit().
# Libellés comparés sans casse ni espaces en trop.
tva_remises <- function(db_ticket, produits_ref, tva_par_id,
                        defaut = TAUX_REMISE_DEFAUT) {
  vide <- tibble(ID_PRODUIT = db_ticket$ID_PRODUIT[0], TAUX_TVA = numeric(),
                 CATEGORIE = character())
  cle <- function(x) tolower(trimws(x))

  restants <- db_ticket %>%
    anti_join(tva_par_id, by = "ID_PRODUIT") %>%
    count(ID_PRODUIT, PRODUIT) %>%
    mutate(CIBLE = cle(produit_remise(PRODUIT))) %>%
    filter(!is.na(CIBLE))
  if (!nrow(restants)) return(vide)

  # Un taux par libellé : l'export produits d'abord, la caisse ensuite.
  par_libelle <- bind_rows(
    produits_ref %>% filter(!is.na(TAUX_TVA)) %>%
      transmute(CIBLE = cle(PRODUIT_FULL), TAUX_TVA, RANG = 1L),
    db_ticket %>% distinct(ID_PRODUIT, PRODUIT) %>%
      inner_join(tva_par_id %>% select(ID_PRODUIT, TAUX_TVA), by = "ID_PRODUIT") %>%
      transmute(CIBLE = cle(PRODUIT), TAUX_TVA, RANG = 2L)) %>%
    arrange(CIBLE, RANG) %>%
    distinct(CIBLE, .keep_all = TRUE) %>%
    select(CIBLE, TAUX_TVA)

  restants %>%
    left_join(par_libelle, by = "CIBLE") %>%
    mutate(TAUX_TVA = coalesce(TAUX_TVA, defaut)) %>%
    # Le plus vendu des libellés d'un ID_PRODUIT tranche.
    arrange(ID_PRODUIT, desc(n)) %>%
    group_by(ID_PRODUIT) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(ID_PRODUIT, TAUX_TVA, CATEGORIE = NA_character_)
}

# Ce qui reste sans taux, avec ce que ça pèse : hors de tout total HTVA. Le
# remède est en amont, dans l'export produits.
produits_sans_tva <- function(db_ticket, tva_par_id) {
  db_ticket %>%
    anti_join(tva_par_id, by = "ID_PRODUIT") %>%
    group_by(ID_PRODUIT, PRODUIT) %>%
    summarise(LIGNES = n(), QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA_TVAC = sum(PRIX_TOTAL, na.rm = TRUE), .groups = "drop") %>%
    # Par poids, signe ignoré : les remises sont négatives.
    arrange(desc(abs(CA_TVAC)))
}
