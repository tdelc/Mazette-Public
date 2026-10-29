# Comptabilité Odoo : des écritures brutes à DB_COMPTA.
#
# ---------------------------------------------------------------------------
# Pourquoi les écritures, et pas le compte de résultat
# ---------------------------------------------------------------------------
# Parce que le grain fin est la seule chose stable. La mise en page d'un
# rapport, elle, dérive : cf. les 31 nombres de lignes différents sur 52 mois
# d'exports du comptable, documentés dans R/plan_comptable.R. Une ligne
# d'écriture a les mêmes dix colonnes depuis toujours, et tout le reste s'en
# déduit — compte de résultat, balance, coûts par secteur. L'inverse est faux.
#
# ---------------------------------------------------------------------------
# Le découpage, et ce qu'il prépare
# ---------------------------------------------------------------------------
#   lit_ecritures_fichier()  lecture brute          <- SEULE à changer avec l'API
#   normalise_ecritures()    typage, code de compte, journal
#   compta_mensuelle()       agrégation -> DB_COMPTA
#   controle_ecritures()     les garde-fous
#   coud_compta()            la jonction avec l'ancienne source
#
# Le jour où l'API arrive, seule la première est remplacée : le reste, et ses
# tests, ne bougent pas. Cf. docs/odoo.md.

#### Lecture ####

# Colonnes de l'export « Écritures comptables », libellés de la vue liste.
#
# Nommées ici plutôt que devinées : un export dont une colonne manque doit se
# plaindre en nommant la colonne, pas rendre un tableau amputé.
# Onglet préféré quand le fichier en porte plusieurs. Absent, on prend le
# premier : un export brut d'Odoo n'a qu'une feuille, un classeur tenu à la
# main en a souvent plusieurs.
SHEET_ODOO <- "ECRITURES"

ODOO_COLONNES <- c(DATE       = "Date",
                   PIECE      = "Numéro",
                   COMPTE     = "Compte",
                   PARTENAIRE = "Partenaire",
                   TEXTE      = "Libellé",
                   DEBIT      = "Débit",
                   CREDIT     = "Crédit")

# Lecture brute d'un export de la vue liste.
#
# XLSX de préférence : readxl rend des nombres et des dates déjà typés, là où un
# CSV hérite du format belge (« 1.234,56 », « 30/06/2026 »). Les deux chemins
# sont acceptés quand même — normalise_ecritures() retype ce qui arrive en
# texte — mais le XLSX évite une conversion, donc une occasion de se tromper.
lit_ecritures_fichier <- function(chemin, feuille = SHEET_ODOO) {
  if (is.null(chemin) || !length(chemin) || !file.exists(chemin)) return(NULL)
  ext <- tolower(tools::file_ext(chemin))

  if (ext %in% c("xlsx", "xlsm", "xls")) {
    onglets <- readxl::excel_sheets(chemin)
    cible <- if (!is.null(feuille) && feuille %in% onglets) feuille else onglets[1]
    return(readxl::read_excel(chemin, sheet = cible, guess_max = 100000))
  }

  # Odoo exporte en CSV avec la virgule, mais un passage par un tableur belge
  # le rend en point-virgule. On tranche sur la première ligne plutôt que de
  # supposer : un mauvais séparateur donne UNE colonne, et une colonne
  # manquante se diagnostique mal.
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

# Une ligne par écriture, typée, avec son code de compte et son journal.
#
# Deux extractions, et aucune des deux n'est cosmétique :
#   - « 623700 Titres-repas » : la vue liste concatène le code et le libellé.
#     Le code est ce qui classe (R/plan_comptable.R), le libellé n'est là que
#     pour être lu.
#   - « MZACH/2026/09/0003 » : le journal est le préfixe. Il sert à repérer les
#     pièces de reprise et de clôture.
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
    # Le partenaire était exigé à l'entrée et jeté à la sortie. Il ne sert à
    # aucun calcul — mais c'est lui qui répond à « on a payé ça à qui ? »,
    # c'est-à-dire à la moitié des questions qu'on pose à un grand livre.
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

  # Une ligne sans compte ni date n'est pas une écriture : c'est un reste de
  # mise en page. On l'écarte ici, et controle_ecritures() en donne le nombre —
  # la jeter en silence masquerait un export mal découpé.
  d %>% filter(!is.na(COMPTE), !is.na(DATE))
}

#### Le secteur des comptes d'achat ####

# Correspondance compte -> secteur, pour les SEULS comptes d'achat.
#
# DB_COMPTA$SECTEUR n'a qu'un consommateur, DB_COUTS_MATIERE (server.R), qui ne
# regarde que les préfixes 60 et 609. Ni les ventes, ni les rémunérations, ni
# les frais généraux, ni le bilan n'ont besoin d'un secteur.
#
# Le plan comptable de Mazette porte déjà la ventilation dans sa numérotation,
# et l'achat va toujours avec sa variation de stock. On lit donc le NUMÉRO, et
# jamais le libellé : normalise_secteur() connaît « Fabrik de boissons » et
# « Fabrique de boissons », le compte en écrit une troisième au singulier, et
# courir après les orthographes est une course perdue d'avance.
#
# Comme PLAN_PCMN, le préfixe le plus long l'emporte : la table peut donc se
# raffiner sans se réécrire.
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
  # 604000 « Achats de marchandises » est volontairement ABSENT : c'est un
  # compte de marchandises, donc de la matière, mais on ignore encore comment
  # le ventiler. Le laisser tomber dans « Non ventilé » dit cette ignorance ;
  # le ranger en Support affirmerait qu'il s'agit de frais généraux, ce qui
  # serait faux. controle_ecritures() en donne le poids chaque mois, et c'est
  # ce poids qui décidera s'il faut un sous-compte dans Odoo.
)

# Préfixes dont les comptes portent un secteur : achats et variations de stocks.
PREFIXES_MATIERE <- c("60")

# Le secteur d'un compte, préfixe le plus long d'abord.
#
# Rend NA pour un compte hors du périmètre matière (une vente, un salaire) :
# NA veut dire « cette question ne se pose pas », quand SECTEUR_NON_VENTILE
# veut dire « elle se pose et on n'a pas la réponse ». Les deux ne se lisent
# pas pareil et ne se corrigent pas pareil.
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

# Correspondance POSTE -> CATEGORIE, pour les deux consommateurs qui lisent
# encore le vocabulaire de l'ancien export du comptable.
#
# Ce n'est pas une invention : CATEGORIE et SECTION sont ici DÉDUITES du plan,
# donc toujours d'accord avec lui. C'est une couche de compatibilité, à retirer
# le jour où DB_COUTS_MATIERE (server.R) et DB_HEURES_COMPTA (import.R) passeront
# par classe_comptes() comme le reste du dashboard.
CATEGORIES_LEGACY <- c("Achats"               = "ACHATS",
                       "Variation de stocks"  = "VARIATION_STOCK",
                       "Rémunérations"        = "REMUNERATION")

SECTION_COUT_VENTES <- "Coût des ventes et prestations"

# Les écritures agrégées au mois, dans la forme qu'attend le dashboard.
#
# Deux décisions portent tout le reste.
#
# 1. ON NE GARDE QUE LES CLASSES 6 ET 7. Sur un extrait réel, 47 lignes sur 51
#    sont du bilan. Les laisser entrer noierait comptes_non_classes() — le
#    contrôle censé signaler un compte de RÉSULTAT oublié — sous 92 % de comptes
#    que le plan n'a aucune raison de connaître.
#
# 2. VALEUR = SENS_G * (CREDIT - DEBIT). Le dashboard attend une MAGNITUDE
#    POSITIVE, le signe vivant dans le numéro de compte (cf. R/exploitation.R).
#    Une charge a un solde débiteur : la formule rend DEBIT - CREDIT, positif.
#    Un produit a un solde créditeur : elle rend CREDIT - DEBIT, positif aussi.
#    Une note de crédit sort négative, et c'est juste — elle diminue la charge.
#    Se tromper de sens ici inverse tout le volet Compta sans lever d'erreur.
compta_mensuelle <- function(ecritures, secteurs = SECTEURS_ACHATS) {
  if (is.null(ecritures) || !nrow(ecritures)) return(NULL)

  resultat <- ecritures %>% filter(CLASSE %in% c("6", "7"))
  if (!nrow(resultat)) return(NULL)

  # classe_comptes() plutôt qu'un second test de préfixe : le classement PCMN
  # doit vivre à un seul endroit, sinon les deux divergeront.
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

# Les garde-fous de l'import, en une table.
#
# Une table et jamais un stop() : un import qui s'arrête ne dit rien de ce qui
# allait bien, et on perd l'occasion de voir les quatre autres anomalies.
# VERDICT vaut "ok" ou "alerte" ; c'est la colonne qu'on lit en diagonale.
controle_ecritures <- function(ecritures, compta = NULL, reprise = NULL) {
  if (is.null(ecritures) || !nrow(ecritures))
    return(tibble(CONTROLE = "Lecture", VERDICT = "alerte",
                  DETAIL = "aucune écriture lue", VALEUR = 0))

  ligne <- function(nom, ok, detail, valeur = NA_real_)
    tibble(CONTROLE = nom, VERDICT = if (isTRUE(ok)) "ok" else "alerte",
           DETAIL = detail, VALEUR = valeur)

  res <- list()

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
    if (nrow(inconnus)) paste(inconnus$COMPTE, collapse = ", ")
    else "tous classés par PLAN_PCMN", nrow(inconnus))

  # 4. Ce que pèse le secteur « Non ventilé ». C'est ce chiffre, et lui seul,
  #    qui dira s'il faut un sous-compte dans Odoo pour 604000.
  if (!is.null(compta) && nrow(compta)) {
    nv <- sum(compta$VALEUR[compta$SECTEUR %in% SECTEUR_NON_VENTILE], na.rm = TRUE)
    res[[length(res) + 1]] <- ligne(
      "Matières non ventilées", nv == 0,
      paste0(round(nv), " € sans secteur"), nv)
  }

  # 5. La pièce de reprise ne doit toucher que le bilan. Une ligne de classe 6
  #    ou 7 à cette date ferait doublon avec l'ancienne source.
  if (!is.null(reprise)) {
    d <- as.Date(reprise)
    n <- sum(ecritures$DATE == d & ecritures$CLASSE %in% c("6", "7"))
    res[[length(res) + 1]] <- ligne(
      "Reprise sans résultat", n == 0,
      paste0(n, " ligne(s) de classe 6/7 au ", format(d, "%d/%m/%Y")), n)
  }

  bind_rows(res)
}

#### La jonction avec l'ancienne comptabilité ####

# Premier mois qu'Odoo couvre. Odoo ne contient rien avant : l'historique
# 2022 -> juin 2026 ne vit que dans l'ancien DB_COMPTA.rds.
#
# Une constante nommée, et pas une date recopiée dans trois filter() : le jour
# où la bascule se déplace, elle se déplace à un seul endroit.
BASCULE_ODOO <- as.Date("2026-07-01")

# Date de la pièce de reprise : le solde de l'ancien comptable, repris à
# l'ouverture d'Odoo. Elle ne doit toucher que des comptes de bilan — une
# ligne de classe 6 ou 7 à cette date ferait doublon avec l'ancienne source.
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

# Les trois contrôles de la couture. Chacun attrape une faute que les deux
# autres laissent passer : un recouvrement double un mois, un trou se lit comme
# une chute d'activité, et une reprise mal datée fait les deux à la fois.
controle_couture <- function(ancien, odoo, bascule = BASCULE_ODOO) {
  bascule <- as.Date(bascule)
  mois <- function(db) if (is.null(db) || !nrow(db)) as.Date(character())
                       else sort(unique(periode_compta(db)))

  m_anc <- mois(ancien); m_odo <- mois(odoo)
  commun <- intersect(as.character(m_anc), as.character(m_odo))

  attendus <- if (!length(m_anc) || !length(m_odo)) as.Date(character())
              else seq(min(m_anc), max(m_odo), by = "month")
  presents <- union(as.character(m_anc), as.character(m_odo))
  trous <- setdiff(as.character(attendus), presents)

  tibble(
    CONTROLE = c("Pas de recouvrement", "Pas de trou", "Bascule respectée"),
    VERDICT  = c(if (!length(commun)) "ok" else "alerte",
                 if (!length(trous)) "ok" else "alerte",
                 if (!length(m_odo) || min(m_odo) >= bascule) "ok" else "alerte"),
    DETAIL   = c(
      if (length(commun)) paste("mois des deux côtés :", paste(commun, collapse = ", "))
      else "aucun mois présent dans les deux sources",
      if (length(trous)) paste("mois manquants :", paste(trous, collapse = ", "))
      else "série mensuelle continue",
      if (!length(m_odo)) "aucun mois Odoo"
      else paste("premier mois Odoo :", min(m_odo))))
}
