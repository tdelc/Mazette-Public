# Volet « Projection » : compléter un mois dont la comptabilité n'est pas
# encore entière (des rémunérations pas encore encodées, typiquement).
#
# Trois règles :
#
#   1. RIEN NE SORT D'ICI. La projection ne touche ni DB_COMPTA ni aucun autre
#      onglet : un chiffre modélisé serait indiscernable d'un réel.
#   2. CHAQUE CHIFFRE DIT D'OÙ IL VIENT : méthode et base de calcul affichées.
#   3. ON MODÉLISE CE QU'ON MESURE AILLEURS :
#
#        CA             les ventes de caisse du mois (DB_KPI_SIMPLE)
#        Matières       le ratio médian matières / CA des mois de référence
#        Rémunérations  la paie, à défaut le pointage, à défaut le planning,
#                       recalés sur la comptabilité (cf. PROJ_SOURCES_REMU)
#        Frais généraux la médiane des mois de référence
#
#      Autres produits, amortissements et financier sont repris tels quels.
#
# La médiane partout : elle ignore le mois exceptionnel que toute série de
# référence contient.

#### Ce qu'on sait projeter ####

# Une ligne par poste projetable : CLE pilote les cases à cocher, BLOC la
# colonne remplacée. L'ordre compte : les matières se calculent sur le CA
# retenu, donc après lui.
PROJ_POSTES <- tibble::tribble(
  ~CLE,           ~BLOC,          ~LIBELLE,             ~SOURCE,
  "ca",           "CA",           "Chiffre d'affaires", "les ventes de caisse",
  "matieres",     "MATIERES",     "Matières premières", "le ratio au CA",
  "remuneration", "REMUNERATION", "Rémunérations",      "les heures planifiées",
  "generaux",     "GENERAUX",     "Frais généraux",     "les mois précédents")

# Combien de mois la trajectoire montre (graphe, tableau, sélecteur).
PROJ_MOIS_GRAPHE <- 14

# Nombre de mois de référence par défaut.
PROJ_DEFAUT_REF <- 6

# En dessous, on projette quand même, mais l'écran le signale.
PROJ_MIN_REF <- 3

# Seuil au-delà duquel un poste est réputé présent dans un mois.
PROJ_SEUIL_PRESENT <- 1

#### Les sources extérieures à la comptabilité ####

# Les jours d'ouverture habituels, par jour de semaine : ouvert plus d'une
# fois sur deux sur la fenêtre. Sert à savoir ce qu'il reste à un mois en
# cours.
jours_ouverts_habituels <- function(journalier, avant, semaines = 12) {
  f <- journalier %>%
    filter(DATE < as.Date(avant),
           DATE >= as.Date(avant) - 7 * semaines)
  if (!nrow(f)) return(1:7)
  f %>% group_by(J = wday(DATE)) %>%
    summarise(PART = mean(V > 0), .groups = "drop") %>%
    filter(PART >= 0.5) %>% pull(J)
}

# Ventes de caisse par mois, HTVA (on compare à des comptes 70).
#
# Le mois en cours est porté à son mois complet, au prorata des jours
# d'ouverture (ATTENDUS / ECOULES / FACTEUR) : sinon un CA partiel ferait face
# à des charges de mois entier. Un mois clos n'est jamais extrapolé.
ventes_par_mois <- function(db_kpi, colonne = "CA_HTVA",
                            aujourd_hui = today()) {
  if (is.null(db_kpi) || !is.data.frame(db_kpi) || !nrow(db_kpi) ||
      !all(c("DATE", colonne) %in% names(db_kpi)))
    return(NULL)
  j <- db_kpi %>%
    transmute(DATE = as.Date(DATE),
              V = suppressWarnings(as.numeric(.data[[colonne]]))) %>%
    filter(!is.na(DATE), !is.na(V))
  if (!nrow(j)) return(NULL)

  aujourd_hui <- as.Date(aujourd_hui)
  en_cours <- floor_date(aujourd_hui, "month")
  ouverts  <- jours_ouverts_habituels(j, en_cours)
  # Arrêté à hier : la journée en cours n'est pas finie.
  j <- j %>% filter(DATE < aujourd_hui)
  if (!nrow(j)) return(NULL)

  j %>%
    mutate(PERIODE = floor_date(DATE, "month")) %>%
    group_by(PERIODE) %>%
    summarise(VENTES = sum(V, na.rm = TRUE), JOURS = sum(V > 0),
              .groups = "drop") %>%
    mutate(
      EN_COURS = PERIODE == en_cours,
      ATTENDUS = vapply(PERIODE, function(m)
        sum(wday(seq(m, fin_periode(m, "mois"), by = "day")) %in% ouverts),
        numeric(1)),
      ECOULES  = ifelse(EN_COURS,
        vapply(PERIODE, function(m) {
          if (aujourd_hui <= m) return(0)
          d <- seq(m, min(fin_periode(m, "mois"), aujourd_hui - 1), by = "day")
          sum(wday(d) %in% ouverts)
        }, numeric(1)),
        ATTENDUS),
      FACTEUR = ifelse(!EN_COURS, 1,
                       ifelse(ECOULES > 0, ATTENDUS / ECOULES, NA_real_))) %>%
    arrange(PERIODE)
}

# Jusqu'où une source au grain jour couvre chaque mois.
couverture_mois <- function(dates) {
  d <- as.Date(dates)
  d <- d[!is.na(d)]
  if (!length(d)) return(NULL)
  tibble(PERIODE = floor_date(d, "month"), DATE = d) %>%
    group_by(PERIODE) %>%
    summarise(DERNIER = max(DATE), .groups = "drop") %>%
    mutate(FIN = fin_periode(PERIODE, "mois"))
}

# Heures planifiées par mois, tous services (le compte 62 les paie tous).
# DERNIER dit jusqu'où le planning du mois va : un mois incomplet est
# signalé, pas redressé.
heures_par_mois <- function(db_planning) {
  if (!planning_valide(db_planning)) return(NULL)
  d <- db_planning %>%
    transmute(DATE = as.Date(DATE),
              PERIODE = floor_date(as.Date(DATE), "month"), HEURES) %>%
    filter(!is.na(PERIODE))
  if (!nrow(d)) return(NULL)
  d %>%
    group_by(PERIODE) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE), .groups = "drop") %>%
    filter(HEURES > 0) %>%
    left_join(couverture_mois(d$DATE), by = "PERIODE") %>%
    arrange(PERIODE)
}

#### Les sources de la rémunération, de la meilleure à la dernière ####

# Les sources des rémunérations, de la meilleure à la dernière :
#
#   paie (DB_ONSS)            le coût employeur mesuré ;
#   pointage valorisé         chaque heure au taux de son contrat : le mix
#                             contractuel du mois est pesé ;
#   pointage en heures        le volume seul, quand la valorisation manque ;
#   planning                  une prévision, seule source d'un mois en cours.
#
# Sur un mois clos, les mesures passent avant le planning. Sur un mois en
# cours, les mesures (partielles) sont écartées. Chaque source est recalée par
# son propre rapport médian au compte 62. controle_sources_remuneration()
# mesure l'écart de chacune ; le choix, lui, n'est pas automatisé.
PROJ_SOURCES_REMU <- tibble::tribble(
  ~CLE,          ~LIBELLE,                    ~UNITE,  ~QUOI,                ~RAPPORT,          ~MOIS_EN_COURS,
  "paie",        "Coût de la paie",           "euro",  "de coût employeur",  "compta/paie",     FALSE,
  "horeko_cout", "Pointage au taux du contrat", "euro", "de pointage valorisé", "compta/pointage", FALSE,
  "horeko",      "Heures pointées",           "heure", "h pointées",         "",                FALSE,
  "planning",    "Heures planifiées",         "heure", "h planifiées",       "",                TRUE)

#' Les sources, mises à la même forme : PERIODE, VALEUR, et la couverture du
#' mois (s'appuie sur sources_mensuelles() de R/sources_travail.R).
sources_remuneration <- function(db_couts = NULL, db_onss = NULL,
                                 db_planning = NULL) {
  src <- list(paie = NULL, horeko_cout = NULL, horeko = NULL, planning = NULL)

  utilisable <- !is.null(db_couts) && is.data.frame(db_couts) &&
    nrow(db_couts) > 0 && "DATE" %in% names(db_couts) &&
    any(c("HEURES", "HEURES_HOREKO") %in% names(db_couts))
  if (utilisable) {
    m <- sources_mensuelles(db_couts, db_onss)
    cv <- couverture_mois(db_couts$DATE)

    # Le pointage valorisé est sommé sans na.rm : un contrat sans taux rend le
    # mois NA, et la source suivante répond (plutôt que de compter ces heures
    # pour zéro euro).
    col_cout <- if ("COUT_HOREKO_LIGNE" %in% names(db_couts))
      "COUT_HOREKO_LIGNE" else "COUT_TRAVAIL"
    if (col_cout %in% names(db_couts)) {
      c_h <- db_couts %>%
        transmute(PERIODE = floor_date(as.Date(DATE), "month"),
                  V = .data[[col_cout]]) %>%
        filter(!is.na(PERIODE)) %>%
        group_by(PERIODE) %>%
        summarise(VALEUR = sum(V), .groups = "drop") %>%
        filter(!is.na(VALEUR), VALEUR > 0)
      if (nrow(c_h)) src$horeko_cout <- c_h %>% left_join(cv, by = "PERIODE")
    }

    if (!is.null(m)) {
      h <- m %>% filter(SOURCE == "horeko", !is.na(HEURES), HEURES > 0) %>%
        transmute(PERIODE, VALEUR = HEURES)
      if (nrow(h)) src$horeko <- h %>% left_join(cv, by = "PERIODE")
      p <- m %>% filter(SOURCE == "paie", !is.na(COUT), COUT > 0) %>%
        transmute(PERIODE, VALEUR = COUT)
      if (nrow(p)) src$paie <- p %>%
        mutate(DERNIER = fin_periode(PERIODE, "mois"),
               FIN = fin_periode(PERIODE, "mois"))
    }
  }

  pl <- heures_par_mois(db_planning)
  if (!is.null(pl) && nrow(pl))
    src$planning <- pl %>% transmute(PERIODE, VALEUR = HEURES, DERNIER, FIN)
  src
}

#### Les mois de référence ####

#' Les mois de référence d'UN poste : antérieurs au mois cible, ni écartés
#' ni projetés, et où le poste est présent (sinon les mois troués tireraient
#' la médiane vers zéro).
mois_reference <- function(postes, cible, bloc = NULL, n = PROJ_DEFAUT_REF,
                           exclus = NULL, a_projeter = NULL) {
  if (is.null(postes) || !nrow(postes)) return(postes)
  d <- postes %>% filter(PERIODE < as.Date(cible))
  if (length(exclus))     d <- d %>% filter(!PERIODE %in% as.Date(exclus))
  if (length(a_projeter)) d <- d %>% filter(!PERIODE %in% as.Date(a_projeter))
  if (!is.null(bloc) && bloc %in% names(d))
    d <- d %>% filter(!is.na(.data[[bloc]]),
                      abs(.data[[bloc]]) > PROJ_SEUIL_PRESENT)
  d %>% arrange(desc(PERIODE)) %>% head(n) %>% arrange(PERIODE)
}

#### Les quatre estimateurs ####

# Chaque estimateur rend une valeur, sa méthode, sa base de calcul en clair
# et le nombre de mois de référence.
proj_valeur <- function(valeur, methode, detail, n_ref = 0L) {
  list(VALEUR = as.numeric(valeur), METHODE = methode,
       DETAIL = detail, N_REF = as.integer(n_ref))
}

# Un taux au centime.
proj_taux <- function(x) paste0(euro_compta(x), "/h")

# Un rapport sans unité (le calibrage caisse -> compta), à trois décimales.
proj_facteur <- function(x) sub(".", ",", format(round(x, 3), nsmall = 3),
                                fixed = TRUE)

#' CA : les ventes de caisse du mois, recalées sur la comptabilité par le
#' rapport médian compta / caisse des mois de référence.
projette_ca <- function(cible, ventes, ref) {
  cible <- as.Date(cible)
  ligne <- if (is.null(ventes)) NULL else ventes[ventes$PERIODE == cible, ]
  v <- if (is.null(ligne) || !nrow(ligne)) numeric(0) else ligne$VENTES[1]

  # Mois à peine commencé : on reconduit la médiane, et la méthode le dit.
  if (!length(v) || is.na(v) || v <= 0 ||
      (isTRUE(ligne$EN_COURS[1]) && is.na(ligne$FACTEUR[1]))) {
    if (!nrow(ref))
      return(proj_valeur(NA_real_, "Ventes de caisse",
                         "Aucune vente de caisse sur ce mois."))
    return(proj_valeur(median(ref$CA), "Médiane des mois précédents",
      paste0("Médiane de ", nrow(ref), " mois (mois à peine commencé : ",
             "aucun jour d'ouverture écoulé)"), nrow(ref)))
  }

  cal <- ref %>% inner_join(ventes, by = "PERIODE") %>%
    filter(VENTES > 0, CA > 0)
  k <- if (nrow(cal) >= PROJ_MIN_REF) median(cal$CA / cal$VENTES) else NA_real_
  f <- if (is.null(ligne$FACTEUR[1]) || is.na(ligne$FACTEUR[1])) 1
       else ligne$FACTEUR[1]
  rythme <- if (f == 1) "" else
    paste0(" sur ", round(ligne$ECOULES[1]), " jours d'ouverture, portés à ",
           round(ligne$ATTENDUS[1]), " jours")

  if (is.na(k))
    return(proj_valeur(v * f, "Ventes de caisse",
      paste0(euro_exact(v), " encaissés", rythme,
             ", sans recalage : moins de ", PROJ_MIN_REF,
             " mois de référence.")))
  proj_valeur(v * f * k,
    if (f == 1) "Ventes de caisse" else "Ventes de caisse, mois extrapolé",
    paste0(euro_exact(v), " encaissés", rythme, " x ", proj_facteur(k),
           " (rapport médian compta/caisse sur ", nrow(cal), " mois)"),
    nrow(cal))
}

#' Matières : le ratio médian au CA, appliqué au CA retenu ; le ratio est la
#' grandeur la plus stable.
projette_matieres <- function(ca, ref) {
  r <- ref %>% filter(CA > PROJ_SEUIL_PRESENT) %>% mutate(R = MATIERES / CA)
  if (!nrow(r) || is.na(ca))
    return(proj_valeur(NA_real_, "Ratio médian au CA",
                       "Pas de mois de référence avec un CA."))
  m <- median(r$R)
  proj_valeur(ca * m, "Ratio médian au CA",
    paste0(format_pct(100 * m), " du CA, médiane de ", nrow(r), " mois, ",
           "appliquée à ", euro_exact(ca)),
    nrow(r))
}

#' Rémunérations : la première source de PROJ_SOURCES_REMU qui répond,
#' recalée sur le compte 62 ; dernier recours, la médiane des montants.
projette_remuneration <- function(cible, sources, ref, aujourd_hui = today()) {
  cible <- as.Date(cible)
  clos  <- fin_periode(cible, "mois") < as.Date(aujourd_hui)
  if (is.null(sources) || !is.list(sources)) sources <- list()
  raisons <- character()

  for (i in seq_len(nrow(PROJ_SOURCES_REMU))) {
    s   <- PROJ_SOURCES_REMU[i, ]
    tbl <- sources[[s$CLE]]
    if (is.null(tbl) || !nrow(tbl)) next
    if (!clos && !s$MOIS_EN_COURS) {
      raisons <- c(raisons, paste0(tolower(s$LIBELLE),
                                   " : le mois n'est pas fini"))
      next
    }
    ligne <- tbl[tbl$PERIODE == cible, ]
    if (!nrow(ligne) || is.na(ligne$VALEUR[1]) || ligne$VALEUR[1] <= 0) next
    v <- ligne$VALEUR[1]

    cal <- ref %>% inner_join(tbl, by = "PERIODE") %>%
      filter(VALEUR > 0, REMUNERATION > PROJ_SEUIL_PRESENT)
    if (nrow(cal) < PROJ_MIN_REF) {
      raisons <- c(raisons, paste0(tolower(s$LIBELLE), " : ", nrow(cal),
                                   " mois de référence seulement"))
      next
    }
    r <- median(cal$REMUNERATION / cal$VALEUR)

    # Une source qui s'arrête avant la fin du mois : on le dit.
    tronque <- !is.null(ligne$DERNIER) && !is.na(ligne$DERNIER[1]) &&
      ligne$DERNIER[1] < ligne$FIN[1]
    base <- if (s$UNITE == "euro")
      paste0(euro_exact(v), " ", s$QUOI, " x ", proj_facteur(r),
             " (rapport médian ", s$RAPPORT, " sur ", nrow(cal), " mois)")
    else
      paste0(round(v), " ", s$QUOI, " x ", proj_taux(r),
             " (coût horaire médian sur ", nrow(cal), " mois)")
    return(proj_valeur(v * r, s$LIBELLE,
      paste0(base, if (tronque) paste0(" — source arrêtée au ",
                                       format(ligne$DERNIER[1], "%d/%m")) else ""),
      nrow(cal)))
  }

  if (!nrow(ref))
    return(proj_valeur(NA_real_, "Médiane des mois précédents",
                       "Aucun mois de référence avec des rémunérations."))
  if (!length(raisons)) raisons <- "aucune source d'heures ou de paie"
  proj_valeur(median(ref$REMUNERATION), "Médiane des mois précédents",
    paste0("Médiane de ", nrow(ref), " mois (", paste(raisons, collapse = " ; "),
           ")"), nrow(ref))
}

#' Frais généraux : la médiane des mois de référence. On reconduit, on ne
#' prétend pas prévoir.
projette_generaux <- function(ref) {
  if (!nrow(ref))
    return(proj_valeur(NA_real_, "Médiane des mois précédents",
                       "Aucun mois de référence."))
  proj_valeur(median(ref$GENERAUX), "Médiane des mois précédents",
    paste0("Médiane de ", nrow(ref), " mois"), nrow(ref))
}

#### L'orchestration ####

#' Projette UN mois, poste par poste.
#'
#' @param postes sortie de postes_exploitation() sur la compta brute.
#' @param cible premier jour du mois à projeter.
#' @param cles les postes à projeter (clés de PROJ_POSTES).
#' @param ventes sortie de ventes_par_mois(), ou NULL.
#' @param sources sortie de sources_remuneration(), ou NULL.
#' @param ajustements vecteur nommé par clé, en % : l'hypothèse de
#'   l'utilisateur, appliquée après le modèle.
#' @return une ligne par poste projeté.
projette_mois <- function(postes, cible, cles, ventes = NULL, sources = NULL,
                          n_ref = PROJ_DEFAUT_REF, exclus = NULL,
                          a_projeter = NULL, ajustements = NULL) {
  vide <- tibble(PERIODE = as.Date(character()), CLE = character(),
                 BLOC = character(), LIBELLE = character(), REEL = numeric(),
                 MODELE = numeric(), AJUST = numeric(), RETENU = numeric(),
                 METHODE = character(), DETAIL = character(),
                 N_REF = integer())
  cles <- intersect(PROJ_POSTES$CLE, cles)   # l'ordre de PROJ_POSTES, toujours
  if (is.null(postes) || !length(cles)) return(vide)
  cible <- as.Date(cible)

  reel <- postes %>% filter(PERIODE == cible)
  lire <- function(bloc) if (nrow(reel)) as.numeric(reel[[bloc]][1]) else 0
  aj <- function(cle) {
    a <- if (is.null(ajustements)) NULL else ajustements[[cle]]
    if (is.null(a) || is.na(a)) 0 else as.numeric(a)
  }

  # Le CA retenu (réel ou projeté) sert de base aux matières.
  ca_retenu <- lire("CA")
  lignes <- list()

  for (cle in cles) {
    bloc <- PROJ_POSTES$BLOC[PROJ_POSTES$CLE == cle]
    ref  <- mois_reference(postes, cible, bloc, n_ref, exclus, a_projeter)
    r <- switch(cle,
      ca           = projette_ca(cible, ventes, ref),
      matieres     = projette_matieres(ca_retenu, ref),
      remuneration = projette_remuneration(cible, sources, ref),
      generaux     = projette_generaux(ref))

    a <- aj(cle)
    retenu <- r$VALEUR * (1 + a / 100)
    if (cle == "ca" && !is.na(retenu)) ca_retenu <- retenu

    lignes[[length(lignes) + 1L]] <- tibble(
      PERIODE = cible, CLE = cle, BLOC = bloc,
      LIBELLE = PROJ_POSTES$LIBELLE[PROJ_POSTES$CLE == cle],
      REEL = lire(bloc), MODELE = r$VALEUR, AJUST = a, RETENU = retenu,
      METHODE = r$METHODE, DETAIL = r$DETAIL, N_REF = r$N_REF)
  }
  bind_rows(lignes)
}

#' La même chose sur plusieurs mois. Les références de chaque mois excluent
#' tous les mois projetés : le modèle ne se nourrit pas de lui-même.
projette_mois_multiples <- function(postes, mois, cles, ventes = NULL,
                                    sources = NULL, n_ref = PROJ_DEFAUT_REF,
                                    exclus = NULL, ajustements = NULL) {
  mois <- sort(as.Date(mois))
  if (!length(mois)) return(projette_mois(postes, Sys.Date(), character()))
  bind_rows(lapply(mois, function(m)
    projette_mois(postes, m, cles, ventes, sources, n_ref, exclus,
                  a_projeter = mois, ajustements = ajustements)))
}

#### Du poste projeté au compte d'exploitation ####

# Les colonnes dérivées de postes_exploitation(), recalculées après
# substitution. Un test garde les deux formules identiques.
recalcule_gestion <- function(p) {
  p %>% mutate(
    PRODUITS      = CA + AUTRES,
    CHARGES       = MATIERES + REMUNERATION + GENERAUX + AMORTISSEMENT,
    MARGE         = PRODUITS - CHARGES,
    MARGE_AA      = PRODUITS - MATIERES - REMUNERATION - GENERAUX + FINANCIER,
    MARGE_PA      = MARGE_AA - AMORTISSEMENT,
    PCT_MATIERES  = ratio_pct(MATIERES, CA),
    PCT_TRAVAIL   = ratio_pct(REMUNERATION, CA),
    PCT_PRIME     = ratio_pct(MATIERES + REMUNERATION, CA),
    PCT_GENERAUX  = ratio_pct(GENERAUX, CA),
    PCT_MARGE     = ratio_pct(MARGE, CA),
    PCT_MARGE_AA  = ratio_pct(MARGE_AA, CA),
    PCT_MARGE_PA  = ratio_pct(MARGE_PA, CA))
}

#' Le compte d'exploitation projection appliquée. Un mois absent de la
#' comptabilité est créé à zéro puis rempli.
applique_projection <- function(postes, proj) {
  if (is.null(postes)) return(NULL)
  if (is.null(proj) || !nrow(proj))
    return(postes %>% mutate(PROJETE = FALSE))

  mois <- sort(unique(proj$PERIODE))
  manquants <- setdiff(mois, postes$PERIODE)
  if (length(manquants)) {
    neuf <- postes[0, , drop = FALSE]
    neuf <- bind_rows(neuf, tibble(PERIODE = as.Date(manquants, origin = "1970-01-01")))
    num <- setdiff(names(postes), "PERIODE")
    for (col in num) neuf[[col]] <- 0
    postes <- bind_rows(postes, neuf) %>% arrange(PERIODE)
  }

  for (i in seq_len(nrow(proj))) {
    if (is.na(proj$RETENU[i])) next
    j <- which(postes$PERIODE == proj$PERIODE[i])
    if (length(j)) postes[[proj$BLOC[i]]][j] <- proj$RETENU[i]
  }
  recalcule_gestion(postes) %>% mutate(PROJETE = PERIODE %in% mois)
}

#### Ce qui manque, et qu'on propose de combler ####

#' Les postes absents d'un mois alors que les mois voisins en portent : une
#' écriture pas encore passée. Ce signal préselectionne les cases de l'écran.
#'
#' @return une ligne par mois x poste manquant.
postes_manquants <- function(postes, depuis = NULL) {
  vide <- tibble(PERIODE = as.Date(character()), CLE = character(),
                 LIBELLE = character(), MEDIANE = numeric())
  if (is.null(postes) || !nrow(postes)) return(vide)
  d <- if (is.null(depuis)) postes else
    postes %>% filter(PERIODE >= as.Date(depuis))
  if (!nrow(d)) return(vide)

  res <- lapply(seq_len(nrow(PROJ_POSTES)), function(i) {
    bloc <- PROJ_POSTES$BLOC[i]
    v <- postes[[bloc]]
    # Médiane prise sur les mois où le poste est présent.
    presents <- v[abs(v) > PROJ_SEUIL_PRESENT]
    if (length(presents) < PROJ_MIN_REF) return(NULL)
    med <- median(presents)
    tibble(PERIODE = d$PERIODE[abs(d[[bloc]]) <= PROJ_SEUIL_PRESENT],
           CLE = PROJ_POSTES$CLE[i], LIBELLE = PROJ_POSTES$LIBELLE[i],
           MEDIANE = med)
  })
  res <- bind_rows(res)
  if (!nrow(res)) vide else res %>% arrange(desc(PERIODE), CLE)
}

#### Le contrôle du modèle ####

#' Le modèle rejoué sur les derniers mois complets, chacun comme s'il était
#' inconnu, et confronté au réel : l'écart dit ce que vaut un chiffre projeté.
controle_modele <- function(postes, cles, ventes = NULL, sources = NULL,
                            n_ref = PROJ_DEFAUT_REF, exclus = NULL,
                            n_mois = 6) {
  vide <- tibble(PERIODE = as.Date(character()), CLE = character(),
                 LIBELLE = character(), REEL = numeric(), MODELE = numeric(),
                 ECART = numeric(), ECART_PCT = numeric())
  cles <- intersect(PROJ_POSTES$CLE, cles)
  if (is.null(postes) || !nrow(postes) || !length(cles)) return(vide)

  if (is.null(exclus)) exclus <- as.Date(character())
  candidats <- postes %>% filter(!PERIODE %in% as.Date(exclus))
  candidats <- candidats %>% arrange(desc(PERIODE)) %>%
    head(n_mois + 1) %>% pull(PERIODE)
  # Le mois le plus ancien n'a pas de passé.
  candidats <- sort(candidats)
  if (length(candidats) < 2) return(vide)
  candidats <- candidats[-1]

  res <- lapply(candidats, function(m) {
    p <- projette_mois(postes, m, cles, ventes, sources, n_ref, exclus,
                       a_projeter = m)
    # Seuls les postes présents ce mois-là sont jugés.
    p %>% filter(abs(REEL) > PROJ_SEUIL_PRESENT, !is.na(MODELE)) %>%
      transmute(PERIODE, CLE, LIBELLE, REEL, MODELE,
                ECART = MODELE - REEL, ECART_PCT = ratio_pct(MODELE - REEL, abs(REEL)))
  })
  bind_rows(res)
}

#' L'écart de chaque source de rémunération prise seule, rejouée sur les
#' mois connus contre le compte 62 réel. Un mois où la source n'a pas répondu
#' ne compte pas.
controle_sources_remuneration <- function(postes, sources,
                                          n_ref = PROJ_DEFAUT_REF,
                                          exclus = NULL, n_mois = 6,
                                          aujourd_hui = today()) {
  vide <- tibble(CLE = character(), LIBELLE = character(), N = integer(),
                 ECART_MEDIAN = numeric())
  if (is.null(postes) || !nrow(postes) || is.null(sources) ||
      !is.list(sources)) return(vide)
  if (is.null(exclus)) exclus <- as.Date(character())

  mois <- postes %>%
    filter(!PERIODE %in% as.Date(exclus),
           abs(REMUNERATION) > PROJ_SEUIL_PRESENT) %>%
    arrange(desc(PERIODE)) %>% head(n_mois) %>% pull(PERIODE) %>% sort()
  if (!length(mois)) return(vide)

  res <- lapply(seq_len(nrow(PROJ_SOURCES_REMU)), function(i) {
    s <- PROJ_SOURCES_REMU[i, ]
    if (is.null(sources[[s$CLE]]) || !nrow(sources[[s$CLE]])) return(NULL)
    ecarts <- vapply(mois, function(m) {
      ref <- mois_reference(postes, m, "REMUNERATION", n_ref, exclus, m)
      r <- projette_remuneration(m, sources[s$CLE], ref, aujourd_hui)
      # La source n'a pas répondu : c'est le repli qui a parlé.
      if (!identical(r$METHODE, s$LIBELLE) || is.na(r$VALEUR))
        return(NA_real_)
      reel <- postes$REMUNERATION[postes$PERIODE == m][1]
      if (!length(reel) || is.na(reel) || reel == 0) return(NA_real_)
      100 * abs(r$VALEUR - reel) / abs(reel)
    }, numeric(1))
    if (all(is.na(ecarts))) return(NULL)
    tibble(CLE = s$CLE, LIBELLE = s$LIBELLE, N = sum(!is.na(ecarts)),
           ECART_MEDIAN = median(ecarts, na.rm = TRUE))
  })
  bind_rows(res)
}

#' L'écart médian absolu, par poste : la fiabilité en une ligne.
fiabilite_modele <- function(ctrl) {
  if (is.null(ctrl) || !nrow(ctrl))
    return(tibble(CLE = character(), LIBELLE = character(),
                  N = integer(), ECART_MEDIAN = numeric()))
  ctrl %>%
    filter(!is.na(ECART_PCT)) %>%
    group_by(CLE, LIBELLE) %>%
    summarise(N = n(), ECART_MEDIAN = median(abs(ECART_PCT)), .groups = "drop") %>%
    arrange(match(CLE, PROJ_POSTES$CLE))
}

#### Rendus ####

# Le signe d'un chiffre modélisé : un caractère, lisible en noir et blanc.
PROJ_MARQUE <- "≈ "

proj_marque_cellule <- function(valeur, periode, bloc, proj) {
  txt <- euro_exact(valeur)
  if (is.null(proj) || !nrow(proj)) return(txt)
  cle <- paste(periode, bloc)
  modelise <- cle %in% paste(proj$PERIODE[!is.na(proj$RETENU)],
                             proj$BLOC[!is.na(proj$RETENU)])
  ifelse(modelise, paste0(PROJ_MARQUE, txt), txt)
}

#' Les tuiles des mois choisis, complétés. « ≈ » seulement sur ce qui est
#' projeté.
kpi_projection <- function(p, mois, proj = NULL) {
  sel <- p %>% filter(PERIODE %in% as.Date(mois))
  if (!nrow(sel))
    return(div(class = "text-muted small p-2",
               "Choisissez un mois à projeter."))
  modelise <- function(bloc) {
    if (is.null(proj) || !nrow(proj)) return(FALSE)
    any(proj$BLOC == bloc & proj$PERIODE %in% sel$PERIODE & !is.na(proj$RETENU))
  }
  tuile <- function(bloc, libelle, couleur, icone) {
    v <- sum(sel[[bloc]], na.rm = TRUE)
    kpi_tile(paste0(if (modelise(bloc)) PROJ_MARQUE else "", euro_exact(v)),
             libelle, couleur, icone,
             sous_titre = format_pct(ratio_pct(v, sum(sel$CA, na.rm = TRUE))))
  }
  ca    <- sum(sel$CA, na.rm = TRUE)
  marge <- sum(sel$MARGE_AA, na.rm = TRUE)
  div(
    class = "kpi-grid",
    kpi_tile(paste0(if (modelise("CA")) PROJ_MARQUE else "", euro_exact(ca)),
             "Chiffre d'affaires", COUL_BRUN, "euro-sign",
             sous_titre = paste0(nrow(sel), " mois complété",
                                 if (nrow(sel) > 1) "s" else "")),
    tuile("MATIERES", "Matières", COUL_MATIERE, "carrot"),
    tuile("REMUNERATION", "Rémunérations", COUL_TRAVAIL, "user-group"),
    tuile("GENERAUX", "Frais généraux", COUL_NEUTRE, "file-invoice"),
    kpi_tile(paste0(PROJ_MARQUE, euro_exact(marge)), "Marge avant amort.",
             if (is.na(marge) || marge >= 0) COUL_VERT else COUL_ROUGE,
             "piggy-bank",
             sous_titre = paste(format_pct(ratio_pct(marge, ca)), "du CA")))
}

#' Le travail du modèle, ligne par ligne.
table_projection_detail <- function(proj) {
  if (is.null(proj) || !nrow(proj))
    return(tibble(Projection = "Aucun poste à projeter."))
  proj %>%
    transmute(
      Mois = etiquette_periode(PERIODE, "mois"),
      `.tri:Mois` = as.integer(PERIODE),
      Poste = LIBELLE,
      Réel = euro_exact(REEL),
      `.tri:Réel` = REEL,
      Modèle = euro_exact(MODELE),
      `.tri:Modèle` = MODELE,
      Hypothèse = ifelse(AJUST == 0, "",
                         paste0(ifelse(AJUST > 0, "+", ""), round(AJUST), " %")),
      Retenu = euro_exact(RETENU),
      `.tri:Retenu` = RETENU,
      Méthode = METHODE,
      `Base de calcul` = DETAIL,
      # Le nombre de mois derrière la médiane.
      Réf = ifelse(N_REF == 0, "—", as.character(N_REF)),
      `.tri:Réf` = N_REF)
}

#' Le compte d'exploitation mois par mois, projection comprise.
table_projection_resultat <- function(p, proj, n = PROJ_MOIS_GRAPHE) {
  if (is.null(p) || !nrow(p)) return(tibble(Résultat = "Aucune comptabilité."))
  d <- p %>% arrange(desc(PERIODE)) %>% head(n)
  cell <- function(col) proj_marque_cellule(d[[col]], d$PERIODE, col, proj)
  tibble(
    Mois = etiquette_periode(d$PERIODE, "mois"),
    `.tri:Mois` = as.integer(d$PERIODE),
    État = pastille(ifelse(d$PROJETE, "Projeté", "Réel"),
                    ifelse(d$PROJETE, COUL_AMBRE, COUL_VERT)),
    `Chiffre d'affaires` = cell("CA"),
    `.tri:Chiffre d'affaires` = d$CA,
    Matières = cell("MATIERES"),
    `.tri:Matières` = d$MATIERES,
    Rémunérations = cell("REMUNERATION"),
    `.tri:Rémunérations` = d$REMUNERATION,
    `Frais généraux` = cell("GENERAUX"),
    `.tri:Frais généraux` = d$GENERAUX,
    `Marge av. amort.` = euro_exact(d$MARGE_AA),
    `.tri:Marge av. amort.` = d$MARGE_AA,
    `% du CA` = format_pct(d$PCT_MARGE_AA),
    `.tri:% du CA` = d$PCT_MARGE_AA)
}

#' La trajectoire : la marge mois par mois, le projeté hachuré (la couleur
#' dit déjà le signe).
graph_projection <- function(p, n = PROJ_MOIS_GRAPHE, source = "proj_traj") {
  if (is.null(p) || !nrow(p))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune comptabilité")))
  d <- p %>% arrange(PERIODE) %>% tail(n)
  # Ordre imposé : plotly trierait les catégories alphabétiquement.
  axe <- etiquette_periode(d$PERIODE, "mois")
  coul <- ifelse(d$MARGE_AA >= 0, COUL_VERT, COUL_ROUGE)
  reels <- d %>% filter(!PROJETE)
  repere <- if (nrow(reels) >= 3) median(reels$MARGE_AA) else NA_real_

  g <- plot_ly(source = source) %>%
    add_trace(
      x = axe, y = d$MARGE_AA,
      type = "bar", name = "Marge avant amort.",
      marker = list(
        color = coul,
        pattern = list(shape = ifelse(d$PROJETE, "/", ""),
                       fgcolor = "#ffffff", bgcolor = coul,
                       size = 7, solidity = 0.35),
        line = list(color = coul, width = 1)),
      hovertemplate = paste0(
        axe, ifelse(d$PROJETE, " (projeté)", ""),
        "<br>Marge avant amort. : ", euro_exact(d$MARGE_AA),
        "<br>CA : ", euro_exact(d$CA), "<extra></extra>"))

  if (!is.na(repere))
    g <- g %>% add_trace(
      x = axe, y = rep(repere, nrow(d)), type = "scatter", mode = "lines",
      name = "Médiane des mois réels",
      line = list(color = COUL_NEUTRE, width = 1.5, dash = "dot"),
      hovertemplate = paste0("Médiane des mois réels : ",
                             euro_exact(repere), "<extra></extra>"))

  # event_register : sans lui, le clic n'arrive pas à Shiny.
  g <- event_register(g, "plotly_click")

  g %>% layout(
    xaxis = list(title = "", type = "category",
                 categoryorder = "array", categoryarray = axe),
    yaxis = list(title = "Marge avant amortissements (€)", zeroline = TRUE,
                 zerolinecolor = "#8d7b68"),
    legend = list(orientation = "h", y = -0.2),
    margin = list(b = 60),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Écart formaté à part : euro_signe() blanchit les petits montants.
proj_ecart <- function(x)
  ifelse(is.na(x), "—",
  ifelse(abs(x) < 5, "0 €",
         paste0(ifelse(x > 0, "+", "-"), trimws(format_CA(abs(x), -1)))))

#' Le contrôle du modèle, mois à mois.
table_controle_modele <- function(ctrl) {
  if (is.null(ctrl) || !nrow(ctrl))
    return(tibble(Contrôle = paste("Pas assez de mois complets pour éprouver",
                                   "le modèle.")))
  ctrl %>%
    arrange(desc(PERIODE), match(CLE, PROJ_POSTES$CLE)) %>%
    transmute(
      Mois = etiquette_periode(PERIODE, "mois"),
      `.tri:Mois` = as.integer(PERIODE),
      Poste = LIBELLE,
      Réel = euro_exact(REEL), `.tri:Réel` = REEL,
      Modèle = euro_exact(MODELE), `.tri:Modèle` = MODELE,
      Écart = proj_ecart(ECART), `.tri:Écart` = ECART,
      `Écart %` = ifelse(is.na(ECART_PCT), "—",
                         paste0(ifelse(ECART_PCT > 0, "+", ""),
                                round(ECART_PCT, 1), " %")),
      `.tri:Écart %` = ECART_PCT)
}

#' La fiabilité en une phrase par poste ; seuils larges (un ordre de
#' grandeur, pas la comptabilité).
resume_fiabilite <- function(fiab) {
  if (is.null(fiab) || !nrow(fiab))
    return(div(class = "text-muted small",
               "Le modèle n'a pas encore assez de mois complets pour se",
               " mesurer."))
  div(
    div(class = "small text-muted mb-1", "Par poste :"),
    div(class = "d-flex flex-wrap gap-2",
        lapply(seq_len(nrow(fiab)), function(i) {
          e <- fiab$ECART_MEDIAN[i]
          coul <- couleur_seuil(e, 5, 15)
          div(class = "small",
              style = paste0("border-left:3px solid ", coul, ";",
                             "padding:0.15rem 0.55rem;"),
              tags$b(fiab$LIBELLE[i]), tags$br(),
              span(style = paste0("color:", coul, ";font-weight:600"),
                   paste0("± ", format_pct(e))),
              span(class = "text-muted",
                   paste0(" sur ", fiab$N[i], " mois")))
        })))
}

#' Les sources de rémunération côte à côte, dans l'ordre de l'échelle, avec
#' leur écart mesuré.
resume_sources_remu <- function(f) {
  if (is.null(f) || !nrow(f)) return(NULL)
  div(
    class = "mb-2",
    div(class = "small text-muted mb-1",
        "Rémunérations : ce que vaut chaque source, mesuré sur les mois",
        " connus. Celle du haut est celle qui sert."),
    div(class = "d-flex flex-wrap gap-2",
        lapply(seq_len(nrow(f)), function(i) {
          coul <- couleur_seuil(f$ECART_MEDIAN[i], 5, 15)
          div(class = "small",
              style = paste0("border-left:3px solid ", coul, ";",
                             "padding:0.15rem 0.55rem;"),
              tags$b(f$LIBELLE[i]),
              if (i == 1) span(class = "text-muted", " (retenue)"),
              tags$br(),
              span(style = paste0("color:", coul, ";font-weight:600"),
                   paste0("± ", format_pct(f$ECART_MEDIAN[i]))),
              span(class = "text-muted", paste0(" sur ", f$N[i], " mois")))
        })))
}

#' Ce qui empêche le modèle de bien travailler, s'il y a lieu.
bandeau_projection <- function(proj, source_mesuree = TRUE) {
  if (is.null(proj) || !nrow(proj)) return(NULL)
  maigres <- proj %>% filter(!is.na(RETENU), N_REF > 0, N_REF < PROJ_MIN_REF)
  sans <- proj %>% filter(is.na(RETENU))
  msg <- c(
    if (!source_mesuree && "remuneration" %in% proj$CLE)
      paste0("Ni paie, ni pointage, ni planning : les rémunérations sont",
             " reconduites à la médiane, ce qui ignore l'activité du mois."),
    if (nrow(maigres))
      paste0(nrow(maigres), " projection", if (nrow(maigres) > 1) "s" else "",
             " repose", if (nrow(maigres) > 1) "nt" else "",
             " sur moins de ", PROJ_MIN_REF, " mois de référence."),
    if (nrow(sans))
      paste0(nrow(sans), " poste", if (nrow(sans) > 1) "s" else "",
             " n'a pas pu être projeté, faute de référence ou de source."))
  if (!length(msg)) return(NULL)
  bandeau_alerte(TRUE, paste(msg, collapse = " "),
                 titre = "Ce que le modèle ne sait pas faire",
                 couleur = COUL_AMBRE, icone = "circle-info")
}
