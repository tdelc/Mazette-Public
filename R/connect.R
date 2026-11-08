# Tables conservés dans le rds
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

# Tables enregistrées si elles existent, mais dont l'absence n'empêche pas
# l'import d'aboutir. C'est le sas par lequel une nouvelle source entre dans le
# dashboard : le volet qui la consomme sait se taire tant qu'elle manque, et on
# n'a pas à livrer le code et l'import dans le même mouvement.
TABLES_OPTIONNELLES <- c(
  "DB_HEURES_PLANNING",  # heures planifiées par jour x service (cf. R/planning.R)
  "DB_ONSS",             # heures et coût de paie par mois x secteur (cf. R/sources_travail.R)
  # Une ligne, mais elle doit voyager : sans elle, le volet Sources ne peut pas
  # dire qu'une colonne de secteur a changé de nom dans le fichier de paie —
  # et ce contrôle-là ne se refait pas depuis le .RData, seulement à l'import.
  "CONTROLE_ONSS",       # contrôle de lecture du fichier de paie
  # Même raison : les contrôles de l'import Odoo (équilibre débit/crédit,
  # comptes hors plan, matières non ventilées) ne se refont pas depuis le
  # .RData, et c'est pourtant là qu'on voudra les relire.
  "CONTROLE_ODOO",       # contrôle de lecture des écritures Odoo
  # Le journal des décisions prises sur les mois douteux. Il vit dans un
  # classeur, et c'est lui qui fait foi ; cette copie n'est là que pour que le
  # dashboard s'ouvre sans rappeler Google à chaque démarrage.
  "DB_VALIDATION_COMPTA", # décisions de validation des mois (cf. R/validation.R)

  # Les écritures au grain LIGNE, telles que lues. DB_COMPTA n'en garde que
  # l'agrégat mensuel par compte : ce qui suffit à tous les calculs, et à rien
  # d'autre. Ni la pièce, ni le partenaire, ni la date exacte n'y survivent —
  # or c'est exactement ce qu'on cherche quand un poste surprend.
  "DB_ECRITURES"         # grand livre, une ligne par écriture
)


#### Quel cache, et d'où ####

# Un cache porte sa date dans son nom : R_new_env_2026-11-07.RData. C'est la
# date du DERNIER JOUR de DB_JOURS, posée par l'import qui l'a produit — donc
# la fraîcheur réelle des chiffres, et non l'heure à laquelle on a cliqué.
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

#' Le plus récent des caches, QUELLE QUE SOIT sa date. NULL s'il n'y en a aucun.
#'
#' C'est le changement de doctrine : on ne réclame plus le cache de la veille
#' pour le refuser s'il manque, on prend le plus frais de ceux qui existent.
#' Refaire tout l'import parce que le nom attendu n'était pas là, c'était
#' faire attendre dix minutes la première personne connectée du matin.
#'
#' Le tri porte sur la date LUE dans le nom, pas sur le nom : un fichier
#' étranger au dossier (archive, essai renommé) ne peut pas passer devant.
dernier_rds <- function(noms, prefix = PREFIXE_RDS) {
  d <- date_du_rds(noms, prefix)
  if (!any(!is.na(d))) return(NULL)
  i <- which.max(d)
  list(nom = noms[[i]], date = d[[i]])
}

#' Qui gagne, du cache local et de celui de Drive ?
#'
#' Égalité au local : même date, même contenu, autant s'épargner le
#' téléchargement. Drive ne l'emporte que s'il est STRICTEMENT plus frais —
#' c'est le cas tous les matins, puisque c'est là que l'import de la nuit
#' dépose son fichier.
#'
#' Drive injoignable (date NA) : le local sert, quelle que soit sa date. Un
#' dashboard qui montre les chiffres d'avant-hier en le disant vaut mieux
#' qu'un dashboard qui ne s'ouvre pas.
source_du_cache <- function(date_locale, date_drive) {
  if (is.na(date_locale) && is.na(date_drive)) return("import")
  if (is.na(date_drive))  return("local")
  if (is.na(date_locale)) return("drive")
  if (date_locale >= date_drive) "local" else "drive"
}

#### Le tuyau Drive ####

# Le compte de service, une fois par processus. connect.json est éphémère sur
# l'hébergeur : il est retéléchargé au premier besoin de chaque instance.
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

#' Le cache le plus frais du dossier Drive, avec son identifiant.
#'
#' `lister` est un paramètre pour que la suite de tests puisse le doubler :
#' tout ce qui suit est du choix de fichier, et ça se vérifie sans réseau.
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

#' Les deux reconstructions que tout chargement doit faire, d'où qu'il vienne.
#'
#' Elles sont ici, et pas recopiées dans chaque chemin, parce qu'un chemin qui
#' les oublierait ne planterait pas : il rendrait un DB_TICKET réduit et des
#' mots de passe sans droits, c'est-à-dire un dashboard presque juste.
finalise_donnees <- function(envir) {
  if (hydrate_dans(envir))
    cli::cli_alert_success("DB_TICKET reconstruit ({nrow(get('DB_TICKET', envir = envir))} lignes)")

  # Un .RData enregistré avant la gestion des droits n'a que DATE_DEBUT,
  # DATE_FIN et PASS (cf. R/acces.R).
  if (exists("DB_PASSWORD", envir = envir, inherits = FALSE))
    assign("DB_PASSWORD",
           normalise_password(get("DB_PASSWORD", envir = envir)),
           envir = envir)
  invisible(TRUE)
}

#### Les trois chemins ####

#' Refaire tout l'import, puis l'enregistrer en local et sur Drive.
#'
#' Le SEUL endroit du dépôt qui écrit un cache. Le script de la nuit
#' (import_quotidien.R) passe par ici, le bouton « refaire l'import » aussi :
#' il n'y a donc qu'un format de fichier, qu'un contrat de tables, et qu'un
#' endroit à corriger.
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

#' Charger les données du dashboard, sans jamais faire attendre pour rien.
#'
#' L'ordre : le cache local s'il est au moins aussi frais que celui de Drive,
#' sinon celui de Drive, et l'import complet seulement s'il n'existe AUCUN
#' cache nulle part. Ce dernier cas n'arrive qu'au tout premier démarrage —
#' ou le jour où le dossier Drive a été vidé.
#'
#' Ce que ça change pour la personne qui ouvre le dashboard le matin :
#' quelques secondes de téléchargement au lieu de quelques minutes d'import.
#' Et ce que ça lui doit en échange : voir la date de ce qu'elle regarde, et
#' pouvoir demander mieux (cf. R/fraicheur.R et le bouton « mise à jour »).
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

  # Un cache illisible — tronqué, écrit à moitié — ne doit pas bloquer le
  # dashboard : on refait l'import, une fois, plutôt que de rendre la main
  # sur un environnement vide.
  if (!charge) return(invisible(refait_import(prefix, dossier, envir)))

  finalise_donnees(envir)
  invisible(list(fichier = info$nom, date = info$date, source = choix))
}

#' Reprendre le dernier cache de Drive, sans refaire l'import.
#'
#' C'est le geste du bouton « reprendre le dernier import » : quelques
#' secondes, parce que le travail a déjà été fait cette nuit par le script
#' planifié et que le dashboard, lui, ne le sait pas encore.
recharge_donnees <- function(prefix = PREFIXE_RDS, dossier = "outputs",
                             envir = parent.frame()) {
  distant <- dernier_rds_drive(prefix)
  if (is.null(distant)) stop("Aucun cache sur Drive.")
  load(telecharge_rds(distant, dossier), envir = envir)
  finalise_donnees(envir)
  list(fichier = distant$nom, date = distant$date, source = "drive")
}
