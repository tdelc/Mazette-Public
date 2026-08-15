# Onglet "Accueil" : une information par onglet, lisible en trois secondes.
#
# Règle de conception : chaque carte porte UN chiffre qu'on lit de loin, une
# phrase qui dit ce qu'il signifie, et rien d'autre. Le détail est à un clic.
# Une carte sans donnée le dit — elle ne montre ni zéro ni tiret muet.

#' Corps d'une carte d'accueil : un grand chiffre, une phrase, un détail.
#'
#' @param valeur le chiffre, déjà formaté
#' @param couleur couleur du chiffre ; NULL pour la couleur du texte courant
#' @param phrase ce que le chiffre veut dire
#' @param detail ligne secondaire, discrète
corps_accueil <- function(valeur, phrase, couleur = NULL, detail = NULL) {
  div(
    class = "acc-corps",
    div(class = "acc-valeur",
        style = if (!is.null(couleur)) paste0("color:", couleur, ";"),
        valeur),
    div(class = "acc-phrase", phrase),
    if (!is.null(detail)) div(class = "acc-detail", detail)
  )
}

#' Carte vide, quand la donnée manque. Dire pourquoi vaut mieux qu'un zéro.
corps_vide <- function(raison = "Pas encore de données.") {
  div(class = "acc-corps", div(class = "acc-phrase text-muted", raison))
}

##### Bandeau de KPI : le chiffre d'affaires, à trois échelles #####

#' Veille, semaine en cours, mois en cours — chacun face à son objectif.
#'
#' Les trois cumuls s'arrêtent à hier : comparer une journée en cours à un
#' objectif plein ferait paraître tous les matins catastrophiques.
kpi_accueil <- function(db_kpi, db_obj, date_veille, unite_tva = NULL) {
  if (is.null(db_kpi) || !nrow(db_kpi)) return(corps_vide())

  somme <- function(db, d1, d2) {
    v <- db %>% filter(DATE >= d1, DATE <= d2) %>%
      summarise(s = sum(ventes, na.rm = TRUE)) %>% pull(s)
    if (length(v) == 0 || is.na(v)) 0 else v
  }
  bloc <- function(d1, d2, libelle, icone, sous = NULL) {
    ca  <- somme(db_kpi, d1, d2)
    obj <- somme(db_obj, d1, d2)
    kpi_tile(format_CA(ca, -1), titre_avec_tva(libelle, unite_tva),
             couleur_objectif(ca, obj), icone,
             sous_titre = if (obj > 0)
               paste0(label_objectif(ca, obj), " · ", format_CA(obj, -1))
             else sous)
  }

  d_sem   <- floor_date(date_veille, "week", week_start = 1)
  d_mois  <- floor_date(date_veille, "month")
  d_annee <- floor_date(date_veille, "year")

  div(class = "kpi-grid",
      bloc(date_veille, date_veille,
           paste0("CA du ", format(date_veille, "%A %d/%m")), "calendar-day"),
      bloc(d_sem, date_veille, "CA de la semaine", "calendar-week",
           sous = paste("depuis le", format(d_sem, "%d/%m"))),
      bloc(d_mois, date_veille, "CA du mois", "calendar-days",
           sous = format(d_mois, "%B %Y")),
      bloc(d_annee, date_veille, "CA de l'année", "calendar-check",
           sous = format(d_annee, "%Y"))
  )
}

##### Une carte par onglet #####

#' Maintenant : où en est la semaine par rapport à son objectif.
acc_maintenant <- function(db_kpi, db_obj, date_veille) {
  d1 <- floor_date(date_veille, "week", week_start = 1)
  sem <- db_kpi %>% filter(DATE >= d1, DATE <= date_veille)
  if (!nrow(sem)) return(corps_vide())
  ca  <- sum(sem$ventes, na.rm = TRUE)
  obj <- db_obj %>% filter(DATE >= d1, DATE <= date_veille) %>%
    summarise(s = sum(ventes, na.rm = TRUE)) %>% pull(s)
  meilleur <- sem %>% filter(ventes > 0) %>% slice_max(ventes, n = 1)

  corps_accueil(
    if (obj > 0) paste0(round(100 * ca / obj), " %") else format_CA(ca, -1),
    if (obj > 0) paste0("de l'objectif de la semaine, soit ",
                        format_CA(ca, -1), " sur ", format_CA(obj, -1))
    else "de chiffre d'affaires cette semaine",
    couleur_objectif(ca, obj),
    if (nrow(meilleur))
      paste0("Meilleur jour : ", format(meilleur$DATE[1], "%A %d/%m"), " avec ",
             format_CA(meilleur$ventes[1], -1)) else NULL)
}

#' Année : le cumul à date, comparé au même nombre de jours l'an dernier.
acc_annee <- function(db_kpi, date_veille) {
  an <- year(date_veille)
  cur <- db_kpi %>% filter(year(DATE) == an, DATE <= date_veille, ventes > 0)
  if (!nrow(cur)) return(corps_vide())
  ca <- sum(cur$ventes, na.rm = TRUE)

  # Même nombre de jours d'ouverture l'an dernier : comparer au calendrier
  # opposerait des saisons décalées d'un jour de fermeture près.
  m1 <- db_kpi %>% filter(year(DATE) == an - 1, ventes > 0) %>%
    arrange(DATE) %>% head(nrow(cur))
  ca_m1 <- sum(m1$ventes, na.rm = TRUE)
  ecart <- ca - ca_m1

  corps_accueil(
    format_CA(ca, -1),
    paste0("de CA depuis le 1ᵉʳ janvier, sur ", nrow(cur), " jours d'ouverture"),
    COUL_BRUN,
    if (nrow(m1) >= 10)
      span(style = paste0("color:", if (ecart >= 0) COUL_VERT else COUL_ROUGE, ";"),
           paste0(if (ecart >= 0) "+" else "", format_CA(ecart, -1),
                  " vs ", an - 1, " (", format_pct(ratio_pct(ecart, ca_m1)), ")"))
    else paste("Pas assez de recul sur", an - 1, "pour comparer"))
}

#' Fûts : la bière la plus proche de la fin.
acc_futs <- function(db_bieres, db_predict = NULL) {
  niv <- niveau_bieres_actuel(db_bieres)
  if (is.null(niv) || !nrow(niv)) return(corps_vide("Aucune bière en cours."))

  if (!is.null(db_predict) && nrow(db_predict)) {
    pred <- tryCatch(predictions_par_brassin(db_predict), error = function(e) NULL)
    if (!is.null(pred) && nrow(pred))
      niv <- left_join(niv, pred, by = "ID_BRASSIN")
  }
  if (!"FIN_EST" %in% names(niv)) niv$FIN_EST <- as.Date(NA)

  bas <- niv %>% arrange(FIN_EST) %>% slice_head(n = 1)
  # PCT est déjà exprimé en points de pourcentage (0-100), pas en fraction.
  pct <- max(0, min(100, round(bas$PCT[1])))
  autres <- sum(niv$PCT < 20, na.rm = TRUE)

  # etiquette_fin_fut() renvoie une liste (texte + couleur), pas une chaîne :
  # la coller telle quelle produisait un vecteur de longueur 2.
  fin <- etiquette_fin_fut(bas$FIN_EST[1])$texte

  corps_accueil(
    paste0(pct, " %"),
    paste0("restants sur ", bas$BOISSON[1], ", la prochaine bière terminée"),
    if (pct < 20) COUL_ROUGE else if (pct < 40) COUL_AMBRE else COUL_VERT,
    paste0(fin, " · ", nrow(niv), " bières en tirage",
           if (autres > 1) paste0(", dont ", autres, " sous 20 %") else ""))
}

#' Bières : la plus servie de la dernière semaine complète.
acc_bieres <- function(db_ticket, db_produits, unite_tva = "HTVA") {
  sem <- suppressWarnings(max(semaines_dispo(db_produits), na.rm = TRUE))
  if (!is.finite(sem)) return(corps_vide())
  ref <- ref_bieres(db_produits)
  c1 <- conso_bieres(db_ticket, ref, sem, sem + 6, unite_tva)
  if (is.null(c1) || !nrow(c1)) return(corps_vide("Aucune bière servie."))

  top <- c1 %>% slice_max(LITRES, n = 1)
  tot <- sum(c1$LITRES, na.rm = TRUE)

  corps_accueil(
    paste0(round(top$LITRES[1]), " L"),
    paste0("de ", top$BOISSON[1], " sur la semaine du ", format(sem, "%d/%m")),
    COUL_AMBRE,
    paste0(round(tot), " L au total sur la semaine"," · ", nrow(c1), " références"))
}

#' Focaccias : combien vendues la dernière semaine complète, et laquelle domine.
acc_focaccias <- function(db_produits, unite_tva = "HTVA") {
  sem <- suppressWarnings(max(semaines_dispo(db_produits), na.rm = TRUE))
  if (!is.finite(sem)) return(corps_vide())
  f <- conso_focaccias(db_produits, sem, sem + 6, unite_tva)
  if (is.null(f) || !nrow(f)) return(corps_vide("Aucune focaccia vendue."))

  n <- sum(f$QUANTITE, na.rm = TRUE)
  top <- f %>% group_by(VARIANTE) %>%
    summarise(Q = sum(QUANTITE, na.rm = TRUE), .groups = "drop") %>%
    slice_max(Q, n = 1)
  
  n_variantes <- f %>% 
    filter(!str_detect(VARIANTE,"Spicy")) |> 
    group_by(VARIANTE) %>%
    summarise(Q = sum(QUANTITE, na.rm = TRUE), .groups = "drop") |> 
    arrange(-Q) |> 
    mutate(text = paste(VARIANTE,":",Q)) |> 
    pull(text) |> 
    paste(collapse = ", ")
  
  corps_accueil(
    round(n),
    paste0("focaccias vendues sur la semaine du ", format(sem, "%d/%m")),
    COUL_BRUN,
    n_variantes
  )
}

#' Pizzwanze : la dernière soirée, et la prochaine attendue.
acc_pizzwanze <- function(db_produits, db_ticket = NULL, unite_tva = "HTVA") {
  soirees <- tryCatch(soirees_pizzwanze(db_produits), error = function(e) NULL)
  if (is.null(soirees) || !length(soirees))
    return(corps_vide("Aucune soirée Pizzwanze détectée."))

  derniere <- max(soirees)
  db_pizzwanze <- db_produits %>%
    filter(DATE == derniere, est_pizza(PRODUIT)) %>%
    summarise(
      q  = sum(QUANTITE, na.rm = TRUE),
      ca = sum(!!sym(paste0("CA_",unite_tva)), na.rm = TRUE)
    )
  
  n  <- db_pizzwanze |> pull(q)
  ca <- db_pizzwanze |> pull(ca)

  # Les soirées reviennent toutes les trois semaines : on projette le prochain
  # mardi à trois semaines, sans prétendre que c'est confirmé.
  ecart <- if (length(soirees) > 3)
    round(median(as.numeric(diff(sort(tail(soirees, 6)))))) else 21
  prochaine <- derniere + ecart

  corps_accueil(
    round(n),
    paste0("pizzas servies le ", format(derniere, "%A %d/%m")),
    COUL_BRUN,
    paste0(format_CA(ca), " ",unite_tva, " générés")
  )
}

#' Réservations : ce qui est attendu dans les sept jours.
acc_reservations <- function(resa) {
  if (is.null(resa) || !nrow(resa))
    return(corps_vide("Pas de réservations enregistrées."))
  p7 <- prochaines_resa(resa, jours = 7)
  if (!nrow(p7)) return(corps_vide("Aucune réservation dans les 7 jours."))

  couverts <- sum(p7$NB_PERS, na.rm = TRUE)
  auj <- resa %>% filter(A_VENIR, DATE == today())
  gros <- p7 %>% filter(NB_PERS >= SEUILS_GROUPE[3])

  corps_accueil(
    couverts,
    "couverts réservés dans les 7 prochains jours",
    COUL_VERT,
    paste0(sum(auj$NB_PERS, na.rm = TRUE), " aujourd'hui · ", nrow(p7),
           " réservations",
           if (nrow(gros)) paste0(" · ", nrow(gros), " groupe(s) de ",
                                  SEUILS_GROUPE[3], "+") else ""))
}

#' Compta : la marge d'exploitation du dernier mois clôturé.
acc_compta <- function(db_compta) {
  if (is.null(db_compta) || !nrow(db_compta))
    return(corps_vide("Pas de comptabilité disponible."))
  p <- postes_exploitation(db_compta)
  if (!nrow(p)) return(corps_vide("Pas de comptabilité disponible."))

  d <- tail(p, 1)
  prec <- if (nrow(p) > 1) p[nrow(p) - 1, ] else NULL

  corps_accueil(
    format_CA(d$MARGE, -1),
    paste0("de marge d'exploitation en ", format(d$PERIODE, "%B %Y")),
    if (is.na(d$MARGE)) COUL_NEUTRE else if (d$MARGE >= 0) COUL_VERT else COUL_ROUGE,
    paste0(format_pct(d$PCT_MARGE), " du CA · prime cost ",
           format_pct(d$PCT_PRIME),
           if (!is.null(prec) && !is.na(prec$MARGE))
             paste0(" · mois précédent ", format_CA(prec$MARGE, -1)) else ""))
}
