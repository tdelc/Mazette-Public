# Comptabilité Odoo : des écritures brutes à DB_COMPTA. Le grain écriture est
# le seul stable ; tout le reste (compte de résultat, coûts par secteur) s'en
# déduit.
#
#   lit_ecritures_fichier()  lecture d'un export (cf. R/odoo_api.R pour l'API)
#   normalise_ecritures()    typage, code de compte, journal
#   compta_mensuelle()       agrégation -> DB_COMPTA
#   controle_ecritures()     les garde-fous
#   coud_compta()            la jonction avec l'ancienne source
#
# Cf. docs/maintenance.md.

#### Lecture ####

# Colonnes de l'export « Écritures comptables » (libellés de la vue liste),
# nommées pour qu'une colonne manquante se signale par son nom.
# Onglet préféré ; à défaut, le premier.
SHEET_ODOO <- "ECRITURES"

ODOO_COLONNES <- c(DATE       = "Date",
                   PIECE      = "Numéro",
                   COMPTE     = "Compte",
                   PARTENAIRE = "Partenaire",
                   TEXTE      = "Libellé",
                   DEBIT      = "Débit",
                   CREDIT     = "Crédit")

# Lecture brute d'un export de la vue liste, XLSX de préférence (déjà typé) ;
# un CSV au format belge est accepté et retypé par normalise_ecritures().
lit_ecritures_fichier <- function(chemin, feuille = SHEET_ODOO) {
  if (is.null(chemin) || !length(chemin) || !file.exists(chemin)) return(NULL)
  ext <- tolower(tools::file_ext(chemin))

  if (ext %in% c("xlsx", "xlsm", "xls")) {
    onglets <- readxl::excel_sheets(chemin)
    cible <- if (!is.null(feuille) && feuille %in% onglets) feuille else onglets[1]
    return(readxl::read_excel(chemin, sheet = cible, guess_max = 100000))
  }

  # Séparateur tranché sur la première ligne : virgule ou point-virgule.
  entete <- readLines(chemin, n = 1, warn = FALSE, encoding = "UTF-8")
  sep <- c("\t", ";", ",")[which.max(c(lengths(regmatches(entete, gregexpr("\t", entete))),
                                       lengths(regmatches(entete, gregexpr(";",  entete))),
                                       lengths(regmatches(entete, gregexpr(",",  entete)))))]
  readr::read_delim(chemin, delim = sep, col_types = readr::cols(.default = "c"),
                    locale = readr::locale(encoding = "UTF-8"),
                    na = character(), progress = FALSE)
}

#### Normalisation ####

# Un montant, qu'il arrive en nombre (XLSX) ou en texte belge (CSV).
montant_odoo <- function(x) {
  if (is.numeric(x)) return(replace_na(as.numeric(x), 0))
  replace_na(nombre_europeen(x), 0)
}

# Une ligne par écriture, typée, avec son code de compte (« 623700
# Titres-repas » -> 623700) et son journal (préfixe de « MZACH/2026/09/0003 »).
normalise_ecritures <- function(brut, colonnes = ODOO_COLONNES) {
  if (is.null(brut) || !nrow(brut)) return(NULL)

  manque <- setdiff(unname(colonnes), names(brut))
  if (length(manque))
    stop("colonnes absentes de l'export Odoo : ", paste(manque, collapse = ", "),
         "\ncolonnes reçues : ", paste(names(brut), collapse = ", "))

  col <- function(cle) brut[[colonnes[[cle]]]]

  d <- tibble(
    DATE    = parse_date_souple(col("DATE")),
    PIECE   = as.character(col("PIECE")),
    BRUT    = trimws(as.character(col("COMPTE"))),
    TEXTE   = as.character(col("TEXTE")),
    # Le partenaire : « on a payé ça à qui ? ».
    PARTENAIRE = as.character(col("PARTENAIRE")),
    DEBIT   = montant_odoo(col("DEBIT")),
    CREDIT  = montant_odoo(col("CREDIT"))) %>%
    mutate(
      JOURNAL = str_extract(PIECE, "^[^/]+"),
      COMPTE  = str_extract(BRUT, "^[0-9]+"),
      LIBELLE = trimws(str_remove(BRUT, "^[0-9]+")),
      CLASSE  = substr(COMPTE, 1, 1),
      ANNEE   = year(DATE),
      MOIS    = month(DATE)) %>%
    select(-BRUT)

  # Une ligne sans compte ni date est un reste de mise en page :
  # controle_ecritures() en donne le nombre.
  d %>% filter(!is.na(COMPTE), !is.na(DATE))
}

#### Le secteur des comptes d'achat ####

# Correspondance compte -> secteur, pour les seuls comptes d'achat (60 et
# 609), les seuls dont DB_COUTS_MATIERE a besoin. On lit le numéro, jamais le
# libellé ; le préfixe le plus long l'emporte.
SECTEURS_ACHATS <- tibble::tribble(
  ~PREFIXE,  ~SECTEUR,
  "600100",  "Brasserie",                   # matières premières - fabrique de boisson
  "609010",  "Brasserie",                   # variation de stocks - idem
  "600200",  "Transformation alimentaire",
  "609020",  "Transformation alimentaire",
  "604100",  "Service",                     # marchandises - bar (= service)
  "609410",  "Service",
  "601000",  "Support",                     # fournitures : des frais, pas des matières
  "606000",  "Support"                      # frais accessoires sur achats
  # 604000 « Achats de marchandises » est volontairement absent : on ne sait
  # pas encore le ventiler, il tombe donc dans « Non ventilé ».
  # controle_ecritures() en donne le poids chaque mois.
)

# Préfixes dont les comptes portent un secteur : achats et variations de stocks.
PREFIXES_MATIERE <- c("60")

# Le secteur d'un compte, préfixe le plus long d'abord. NA hors du périmètre
# matière (« la question ne se pose pas ») ; SECTEUR_NON_VENTILE quand elle se
# pose sans réponse.
secteur_par_compte <- function(comptes, table = SECTEURS_ACHATS) {
  comptes <- as.character(comptes)
  ref <- table %>% arrange(desc(nchar(PREFIXE)))

  vapply(comptes, function(cp) {
    if (is.na(cp) || !any(startsWith(cp, PREFIXES_MATIERE))) return(NA_character_)
    m <- which(startsWith(cp, ref$PREFIXE))
    if (length(m)) ref$SECTEUR[m[1]] else SECTEUR_NON_VENTILE
  }, character(1), USE.NAMES = FALSE)
}

#### Agrégation : DB_COMPTA ####

# POSTE -> CATEGORIE, pour les deux consommateurs qui lisent encore le
# vocabulaire de l'ancien export (DB_COUTS_MATIERE dans server.R,
# DB_HEURES_COMPTA dans import.R). Déduit du plan, donc toujours d'accord avec
# lui.
CATEGORIES_LEGACY <- c("Achats"               = "ACHATS",
                       "Variation de stocks"  = "VARIATION_STOCK",
                       "Rémunérations"        = "REMUNERATION")

SECTION_COUT_VENTES <- "Coût des ventes et prestations"

# Les écritures agrégées au mois, dans la forme qu'attend le dashboard.
#
# 1. Seules les classes 6 et 7 : le bilan noierait comptes_non_classes().
# 2. VALEUR = SENS_G × (CREDIT − DEBIT) : une magnitude positive pour une
#    charge comme pour un produit ; une note de crédit sort négative.
compta_mensuelle <- function(ecritures, secteurs = SECTEURS_ACHATS) {
  if (is.null(ecritures) || !nrow(ecritures)) return(NULL)

  resultat <- ecritures %>% filter(CLASSE %in% c("6", "7"))
  if (!nrow(resultat)) return(NULL)

  # classe_comptes() : le classement PCMN vit à un seul endroit.
  classe <- resultat %>%
    mutate(TYPE = "compte", AGREGE = TRUE, VALEUR = NA_real_) %>%
    classe_comptes()

  classe %>%
    mutate(VALEUR  = SENS_G * (CREDIT - DEBIT),
           SECTEUR = secteur_par_compte(COMPTE, secteurs)) %>%
    group_by(ANNEE, MOIS, COMPTE, LIBELLE, POSTE, SECTION_G, SECTEUR) %>%
    summarise(VALEUR = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
    mutate(TYPE      = "compte",
           AGREGE    = TRUE,
           CATEGORIE = unname(CATEGORIES_LEGACY[POSTE]),
           SECTION   = if_else(SECTION_G == "Coût des ventes",
                               SECTION_COUT_VENTES, SECTION_G)) %>%
    select(ANNEE, MOIS, COMPTE, LIBELLE, VALEUR, TYPE, AGREGE,
           SECTEUR, CATEGORIE, SECTION) %>%
    arrange(ANNEE, MOIS, COMPTE)
}

#### Contrôles ####

# Les garde-fous de l'import, en une table et jamais un stop() : on voit
# toutes les anomalies d'un coup. VERDICT vaut "ok", "attention" ou "alerte".

# Au-delà de combien de jours sans écriture la source est réputée muette.
FRAICHEUR_ODOO_JOURS <- 14

controle_ecritures <- function(ecritures, compta = NULL, reprise = NULL,
                               n_brut = NULL, aujourd_hui = today()) {
  if (is.null(ecritures) || !nrow(ecritures))
    return(tibble(CONTROLE = "Lecture", VERDICT = "alerte",
                  DETAIL = "aucune écriture lue", VALEUR = 0))

  # `sinon` : tous les défauts ne sont pas des alertes.
  ligne <- function(nom, ok, detail, valeur = NA_real_, sinon = "alerte")
    tibble(CONTROLE = nom, VERDICT = if (isTRUE(ok)) "ok" else sinon,
           DETAIL = detail, VALEUR = valeur)

  aujourd_hui <- as.Date(aujourd_hui)
  res <- list()

  # 0. Ce que la lecture a jeté : si le format de date change, ce sont les
  #    lignes récentes qui tombent.
  if (!is.null(n_brut) && !is.na(n_brut)) {
    perdues <- max(0, as.integer(n_brut) - nrow(ecritures))
    res[[length(res) + 1]] <- ligne(
      "Lignes lues", perdues == 0,
      if (perdues == 0)
        paste0(nrow(ecritures), " lignes lues, aucune écartée.")
      else paste0(perdues, " ligne(s) sur ", n_brut, " écartée(s) faute de",
                  " date ou de compte : si elles sont récentes, c'est un",
                  " format de date qui a changé, pas de la mise en page."),
      perdues)
  }

  # 0 bis. Jusqu'où la source est à jour, sur les écritures passées seulement
  # (Odoo porte des écritures datées de l'avenir).
  passees <- ecritures$DATE[ecritures$DATE <= aujourd_hui]
  if (length(passees)) {
    retard <- as.numeric(aujourd_hui - max(passees))
    res[[length(res) + 1]] <- ligne(
      "Période couverte", retard <= FRAICHEUR_ODOO_JOURS,
      paste0("du ", format(min(ecritures$DATE), "%d/%m/%Y"), " au ",
             format(max(passees), "%d/%m/%Y"), ", soit ",
             if (retard <= 1) "hier ou aujourd'hui"
             else paste0("il y a ", round(retard), " jours"),
             if (retard > FRAICHEUR_ODOO_JOURS)
               " — la source n'a plus rien livré depuis." else "."),
      retard, sinon = "attention")
  }

  # 0 ter. Les écritures à venir : dans le grand livre, mais retirées de tous
  # les calculs par compta_a_date().
  a_venir <- ecritures$DATE > ceiling_date(aujourd_hui, "month") - 1
  res[[length(res) + 1]] <- ligne(
    "Écritures à venir", TRUE,
    if (!any(a_venir)) "aucune écriture datée après le mois en cours."
    else paste0(sum(a_venir), " ligne(s) datée(s) après le mois en cours,",
                " jusqu'au ", format(max(ecritures$DATE), "%m/%Y"),
                " : elles n'entrent dans aucun total, seul le grand livre",
                " les montre."),
    sum(a_venir))

  # 1. L'export est-il complet ? Un export tronqué déséquilibre le total.
  ecart <- sum(ecritures$DEBIT) - sum(ecritures$CREDIT)
  res[[length(res) + 1]] <- ligne(
    "Débit = crédit", abs(ecart) < 0.01,
    paste0("écart global de ", round(ecart, 2), " €"), ecart)

  # 2. Pièce par pièce : attrape une sélection partielle que le total masque.
  desequ <- ecritures %>%
    group_by(PIECE) %>%
    summarise(E = sum(DEBIT) - sum(CREDIT), .groups = "drop") %>%
    filter(abs(E) > 0.01)
  res[[length(res) + 1]] <- ligne(
    "Pièces équilibrées", nrow(desequ) == 0,
    paste0(nrow(desequ), " pièce(s) déséquilibrée(s) sur ",
           n_distinct(ecritures$PIECE)), nrow(desequ))

  # 3. Le plan couvre-t-il tout le résultat ? Un compte 6 ou 7 inconnu du plan
  #    n'entre dans aucun total, et sa disparition ne se voit nulle part.
  inconnus <- ecritures %>%
    filter(CLASSE %in% c("6", "7")) %>%
    mutate(TYPE = "compte", AGREGE = TRUE, VALEUR = 0) %>%
    classe_comptes() %>%
    filter(POSTE == "Non classé") %>%
    distinct(COMPTE, LIBELLE)
  res[[length(res) + 1]] <- ligne(
    "Comptes 6 et 7 classés", nrow(inconnus) == 0,
    if (nrow(inconnus))
      paste0(paste(inconnus$COMPTE, collapse = ", "),
             " — hors du plan, donc dans aucun total.")
    else "tous classés par PLAN_PCMN", nrow(inconnus))

  # 4. Ce que pèse le secteur « Non ventilé ». C'est ce chiffre, et lui seul,
  #    qui dira s'il faut un sous-compte dans Odoo pour 604000.
  if (!is.null(compta) && nrow(compta)) {
    sans <- compta[compta$SECTEUR %in% SECTEUR_NON_VENTILE, , drop = FALSE]
    nv <- sum(sans$VALEUR, na.rm = TRUE)
    # Les comptes sont nommés : c'est là qu'il faut chercher.
    cpts <- sort(unique(sans$COMPTE))
    res[[length(res) + 1]] <- ligne(
      "Matières non ventilées", nv == 0,
      if (nv == 0) "chaque achat est rattaché à un secteur."
      else paste0(round(nv), " € d'achats sans secteur, sur le compte ",
                  paste(cpts, collapse = ", "), " : ils comptent dans la",
                  " marge mais pas dans les coûts matière par secteur."),
      nv)
  }

  # 5. La pièce de reprise ne doit toucher que le bilan. Une ligne de classe 6
  #    ou 7 à cette date porte le résultat de l'exercice PRÉCÉDENT.
  if (!is.null(reprise)) {
    d <- as.Date(reprise)
    n <- sum(ecritures$DATE == d & ecritures$CLASSE %in% c("6", "7"))
    res[[length(res) + 1]] <- ligne(
      "Pièce de reprise", n == 0,
      if (n == 0)
        paste0("la reprise du ", format(d, "%d/%m/%Y"),
               " ne touche que des comptes de bilan, comme attendu.")
      else paste0("la reprise du ", format(d, "%d/%m/%Y"), " porte ", n,
                  " ligne(s) de classe 6 ou 7 : c'est le résultat de",
                  " l'exercice précédent, repris en bloc. La couture les",
                  " écarte (le mois vient de l'ancienne source), mais elles",
                  " restent visibles dans l'onglet Écritures."),
      n)
  }

  bind_rows(res)
}

#### La jonction avec l'ancienne comptabilité ####

# Premier mois couvert par Odoo ; l'historique d'avant ne vit que dans
# l'ancien DB_COMPTA.rds.
BASCULE_ODOO <- as.Date("2026-07-01")

# Date de la pièce de reprise (soldes de l'ancien comptable) : elle ne doit
# toucher que le bilan.
REPRISE_ODOO <- BASCULE_ODOO - 1

periode_compta <- function(db) {
  as.Date(sprintf("%04d-%02d-01", as.integer(db$ANNEE), as.integer(db$MOIS)))
}

# L'ancien jusqu'à la bascule, Odoo à partir d'elle.
coud_compta <- function(ancien, odoo, bascule = BASCULE_ODOO) {
  bascule <- as.Date(bascule)
  avant <- if (is.null(ancien) || !nrow(ancien)) NULL
           else ancien %>% filter(periode_compta(.) < bascule)
  apres <- if (is.null(odoo) || !nrow(odoo)) NULL
           else odoo %>% filter(periode_compta(.) >= bascule)
  res <- bind_rows(avant, apres)
  if (!nrow(res)) NULL else res %>% arrange(ANNEE, MOIS, COMPTE)
}

# Les deux contrôles de la couture. coud_compta() partitionne par date : un
# mois ne peut être compté deux fois. Des mois d'Odoo antérieurs à la bascule
# sont donc ignorés (le plus souvent, la seule pièce de reprise) : ambre.
controle_couture <- function(ancien, odoo, bascule = BASCULE_ODOO) {
  bascule <- as.Date(bascule)
  mois <- function(db) if (is.null(db) || !nrow(db)) as.Date(character())
                       else sort(unique(periode_compta(db)))

  m_anc <- mois(ancien); m_odo <- mois(odoo)
  # Les mois d'Odoo que la couture laisse de côté : antérieurs à la bascule.
  ecartes <- m_odo[m_odo < bascule]

  attendus <- if (!length(m_anc) || !length(m_odo)) as.Date(character())
              else seq(min(m_anc), max(m_odo), by = "month")
  presents <- union(as.character(m_anc), as.character(m_odo))
  trous <- setdiff(as.character(attendus), presents)

  tibble(
    CONTROLE = c("Mois d'Odoo écartés", "Continuité mensuelle"),
    VERDICT  = c(if (!length(ecartes)) "ok" else "attention",
                 if (!length(trous)) "ok" else "alerte"),
    DETAIL   = c(
      if (!length(ecartes))
        paste0("Odoo ne contient rien avant la bascule du ",
               format(bascule, "%d/%m/%Y"), ".")
      else paste0(length(ecartes), " mois d'Odoo sont antérieurs à la bascule",
                  " du ", format(bascule, "%d/%m/%Y"), " (",
                  paste(format(ecartes, "%m/%Y"), collapse = ", "),
                  ") : leurs lignes sont ignorées, ces mois viennent de",
                  " l'ancienne source. C'est le cas normal si Odoo n'y porte",
                  " que la pièce de reprise."),
      if (length(trous)) paste("mois manquants :", paste(trous, collapse = ", "))
      else "série mensuelle continue"))
}
