# Tables enregistrées dans le cache : leur absence fait échouer l'import.
TABLES_DASHBOARD <- c(
  "DB_JOURS",            # CA par jour, base des objectifs
  "DB_KPI_SIMPLE",       # CA jour x (midi/soir, semaine/week-end, boisson/nourriture)
  "DB_OBJECTIFS",        # objectifs journaliers calculés
  "DB_PRODUITS",         # ventes par jour x produit (+ brassin rattaché)
  "DB_TICKET",           # grain ticket, forme réduite (cf. R/donnees.R)
  "REF_PRODUITS",        # ID_REF -> produit, nom, catégorie, TVA, volume
  "DB_BIERES",           # suivi des fûts : volumes cumulés par brassin
  "DB_BRASSINS",         # référentiel des brassins
  "DB_COMPTA",           # Comptabilité générale
  "DB_COUTS_TRAVAIL",    # heures et coûts par jour x secteur x créneau
  "DB_RESA",             # réservations : début, fin, nombre de personnes
  "DB_PASSWORD"          # mots de passe valides par période
)

# Tables enregistrées si elles existent : une nouvelle source entre par ici
# sans bloquer l'import tant qu'elle manque.
TABLES_OPTIONNELLES <- c(
  "DB_HEURES_PLANNING",  # heures planifiées par jour x service (cf. R/planning.R)
  "DB_ONSS",             # heures et coût de paie par mois x secteur (cf. R/sources_travail.R)
  "CONTROLE_ONSS",       # contrôle de lecture du fichier de paie
  "CONTROLE_ODOO",       # contrôle de lecture des écritures Odoo
  # Copie du classeur de décisions (cf. R/validation.R), pour ne pas rappeler
  # Google à chaque démarrage.
  "DB_VALIDATION_COMPTA", # décisions de validation des mois (cf. R/validation.R)

  # Le grand livre, au grain ligne ; DB_COMPTA n'en garde que l'agrégat
  # mensuel par compte.
  "DB_ECRITURES"         # grand livre, une ligne par écriture
)


#### Quel cache, et d'où ####

# Un cache porte dans son nom la date du dernier jour de DB_JOURS : la
# fraîcheur des chiffres, pas l'heure de l'import.
PREFIXE_RDS <- "R_new_env_"

nom_rds <- function(date, prefix = PREFIXE_RDS)
  paste0(prefix, format(as.Date(date), "%Y-%m-%d"), ".RData")

#' La date portée par des noms de cache ; NA pour un nom qui n'en porte pas.
date_du_rds <- function(noms, prefix = PREFIXE_RDS) {
  motif <- paste0("^", prefix, "([0-9]{4}-[0-9]{2}-[0-9]{2})\\.RData$")
  d <- rep(as.Date(NA), length(noms))
  pris <- grepl(motif, noms)
  if (any(pris)) d[pris] <- as.Date(sub(motif, "\\1", noms[pris]))
  d
}

#' Le plus récent des caches, quelle que soit sa date ; NULL s'il n'y en a
#' aucun. Le tri porte sur la date lue dans le nom.
dernier_rds <- function(noms, prefix = PREFIXE_RDS) {
  d <- date_du_rds(noms, prefix)
  if (!any(!is.na(d))) return(NULL)
  i <- which.max(d)
  list(nom = noms[[i]], date = d[[i]])
}

#' Qui gagne, du cache local et de celui de Drive ? Drive seulement s'il est
#' strictement plus récent ; Drive injoignable, le local sert quel que soit
#' son âge.
source_du_cache <- function(date_locale, date_drive) {
  if (is.na(date_locale) && is.na(date_drive)) return("import")
  if (is.na(date_drive))  return("local")
  if (is.na(date_locale)) return("drive")
  if (date_locale >= date_drive) "local" else "drive"
}

#### Le tuyau Drive ####

# Le compte de service. connect.json est éphémère sur l'hébergeur : il est
# retéléchargé au premier besoin.
auth_drive <- function() {
  if (!file.exists("connect.json"))
    download.file(Sys.getenv("LINK_JSON"), destfile = "connect.json", quiet = TRUE)
  googledrive::drive_auth(path = "connect.json")
  invisible(TRUE)
}

liste_drive <- function() {
  auth_drive()
  googledrive::drive_ls(googledrive::as_id(Sys.getenv("PATH_DRIVE")))
}

#' Le cache le plus récent du dossier Drive, avec son identifiant. `lister`
#' est remplaçable pour les tests.
dernier_rds_drive <- function(prefix = PREFIXE_RDS, lister = liste_drive) {
  fichiers <- lister()
  if (is.null(fichiers) || !nrow(fichiers)) return(NULL)
  d <- dernier_rds(fichiers$name, prefix)
  if (is.null(d)) return(NULL)
  d$id <- fichiers$id[[match(d$nom, fichiers$name)]]
  d
}

telecharge_rds <- function(info, dossier = "outputs") {
  chemin <- file.path(dossier, info$nom)
  googledrive::drive_download(googledrive::as_id(info$id), path = chemin,
                              overwrite = TRUE)
  chemin
}

#### Ce qu'il reste à faire après un load() ####

#' Ce que tout chargement doit faire, d'où qu'il vienne : reconstruire
#' DB_TICKET et normaliser les droits.
finalise_donnees <- function(envir) {
  if (hydrate_dans(envir))
    cli::cli_alert_success("DB_TICKET reconstruit ({nrow(get('DB_TICKET', envir = envir))} lignes)")

  # Un cache ancien n'a que DATE_DEBUT, DATE_FIN et PASS (cf. R/acces.R).
  if (exists("DB_PASSWORD", envir = envir, inherits = FALSE))
    assign("DB_PASSWORD",
           normalise_password(get("DB_PASSWORD", envir = envir)),
           envir = envir)
  invisible(TRUE)
}

#### Les trois chemins ####

#' Refaire tout l'import, puis l'enregistrer en local et sur Drive. Le seul
#' endroit qui écrit un cache : l'import planifié et le bouton du dashboard
#' passent tous deux par ici.
refait_import <- function(prefix = PREFIXE_RDS, dossier = "outputs",
                          envir = parent.frame()) {
  cli::cli_h1("Mise à jour des données")
  cli::cli_h2("Importer les DB")
  print(system.time({source("import.R", local = envir)}))

  date_jour <- max(get("DB_JOURS", envir = envir)$DATE)
  nom       <- nom_rds(date_jour, prefix)

  cli::cli_h2("Sauvegarder le rds en local")
  # On échoue tôt et clairement plutôt que de sauver un fichier incomplet.
  absents <- setdiff(TABLES_DASHBOARD, ls(envir))
  if (length(absents))
    stop("L'import n'a pas produit ces tables attendues par le dashboard : ",
         paste(absents, collapse = ", "))

  presentes <- c(TABLES_DASHBOARD, intersect(TABLES_OPTIONNELLES, ls(envir)))
  chemin <- file.path(dossier, nom)
  save(list = presentes, envir = envir, file = chemin,
       compress = "xz", compression_level = 9)
  cli::cli_alert_info("{length(presentes)} tables, {round(file.size(chemin)/1024^2, 2)} Mo")

  cli::cli_h2("Sauvegarder le rds sur drive")
  googledrive::drive_upload(chemin, name = nom,
                            path = googledrive::as_id(Sys.getenv("PATH_DRIVE")),
                            overwrite = TRUE)

  finalise_donnees(envir)
  list(fichier = nom, date = as.Date(date_jour), source = "import",
       tables = presentes)
}

#' Charger les données du dashboard : le cache local s'il est au moins aussi
#' récent que celui de Drive, sinon celui de Drive, et l'import complet
#' seulement s'il n'existe aucun cache.
connexion_ou_creation <- function(prefix = PREFIXE_RDS, force_import = FALSE,
                                  dossier = "outputs", envir = parent.frame()) {
  if (force_import) return(invisible(refait_import(prefix, dossier, envir)))

  local <- dernier_rds(list.files(dossier), prefix)

  distant <- tryCatch(dernier_rds_drive(prefix), error = function(e) {
    cli::cli_alert_warning("Drive injoignable ({conditionMessage(e)}) : on s'en tient au cache local.")
    NULL
  })

  choix <- source_du_cache(
    if (is.null(local))   as.Date(NA) else local$date,
    if (is.null(distant)) as.Date(NA) else distant$date)

  if (choix == "import") {
    cli::cli_alert_warning("Aucun cache, ni en local ni sur Drive : import complet.")
    return(invisible(refait_import(prefix, dossier, envir)))
  }

  info <- if (choix == "local") local else distant
  charge <- tryCatch({
    cli::cli_h1("Chargement du cache {choix} ({info$nom})")
    if (choix == "drive") load(telecharge_rds(distant, dossier), envir = envir)
    else                  load(file.path(dossier, info$nom), envir = envir)
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Chargement impossible : {conditionMessage(e)}")
    FALSE
  })

  # Un cache illisible : on refait l'import, une fois.
  if (!charge) return(invisible(refait_import(prefix, dossier, envir)))

  finalise_donnees(envir)
  invisible(list(fichier = info$nom, date = info$date, source = choix))
}

#' Reprendre le dernier cache de Drive, sans refaire l'import (bouton
#' « Reprendre le dernier import »).
recharge_donnees <- function(prefix = PREFIXE_RDS, dossier = "outputs",
                             envir = parent.frame()) {
  distant <- dernier_rds_drive(prefix)
  if (is.null(distant)) stop("Aucun cache sur Drive.")
  load(telecharge_rds(distant, dossier), envir = envir)
  finalise_donnees(envir)
  list(fichier = distant$nom, date = distant$date, source = "drive")
}
