# Tables conservés dans le rds
TABLES_DASHBOARD <- c(
  "DB_JOURS",            # CA par jour, base des objectifs
  "DB_KPI_SIMPLE",       # CA jour x (midi/soir, semaine/week-end, boisson/nourriture)
  "DB_OBJECTIFS",        # objectifs journaliers calculés
  "DB_PRODUITS",         # ventes par jour x produit (+ brassin rattaché)
  "DB_TICKET",           # grain ticket, forme réduite (cf. R/donnees.R)
  "REF_PRODUITS",        # ID_PRODUIT -> nom, catégorie, TVA, volume
  "DB_BIERES",           # suivi des fûts : volumes cumulés par brassin
  "DB_BRASSINS",         # référentiel des brassins
  "DB_COMPTA",           # Comptabilité générale
  "DB_COUTS_TRAVAIL",    # heures et coûts par jour x secteur x créneau
  "DB_RESA",             # réservations : début, fin, nombre de personnes
  "DB_PASSWORD"          # mots de passe valides par période
)

connexion_ou_creation <- function(drive_env_name, prefix, force_dl = FALSE,
                                  envir = parent.frame()){
  if (!force_dl) {
    # Chargement local
    drive_mazette <- try({
      cli::cli_h1("Chargement du rds local")
      env_vec <- list.files("outputs/", pattern = drive_env_name)
      env_name <- sort(env_vec)[length(env_vec)]
      load(paste0("outputs/", env_name), envir = envir)
    }, silent = TRUE)
  } else {
    drive_mazette <- try({ERROR}, silent = TRUE)
  }
  
  # force_dl <- TRUE
  
  # Chargement via environnement google
  if (force_dl | class(drive_mazette)[1] == "try-error") {
    drive_mazette <- try({
      cli::cli_h1("Chargement du rds à partir de drive")
      link_json <- Sys.getenv("LINK_JSON")
      download.file(link_json, destfile = "connect.json")
      drive_auth(path = "connect.json")
      
      drive_info <- drive_get(path = drive_env_name)
      drive_download(drive_info,
                     path = file.path("outputs", drive_info$name),
                     overwrite = TRUE)
      load(paste0("outputs/", drive_env_name), envir = envir)
    }, silent = TRUE)
  }
  
  # force_dl <- TRUE
  
  # Chargement via google sheets (reconstruction complète)
  if (force_dl | class(drive_mazette)[1] == "try-error") {
    cli::cli_h1("Mise à jour des données")
    cli::cli_h2("Importer les DB")
    print(system.time({source("import.R", local = envir)}))
    
    # Prendre la dernière date comme date de sauvegarde
    date_jour      <- max(get("DB_JOURS", envir = envir)$DATE)
    drive_env_name <- paste0(prefix, date_jour, ".RData")
    
    cli::cli_h2("Sauvegarder le rds en local")
    # On échoue tôt et clairement plutôt que de sauver un fichier incomplet.
    absents <- setdiff(TABLES_DASHBOARD, ls(envir))
    if (length(absents))
      stop("L'import n'a pas produit ces tables attendues par le dashboard : ",
           paste(absents, collapse = ", "))

    save(list = TABLES_DASHBOARD, envir = envir,
         file = file.path("outputs", drive_env_name),
         compress = "xz", compression_level = 9)
    cli::cli_alert_info("{length(TABLES_DASHBOARD)} tables, {round(file.size(file.path('outputs', drive_env_name))/1024^2, 2)} Mo")
    
    cli::cli_h2("Sauvegarder le rds sur drive")
    googledrive::drive_upload(file.path("outputs", drive_env_name),
                              name = drive_env_name,
                              path = as_id(Sys.getenv("PATH_DRIVE")),
                              overwrite = TRUE)
  }
  # Quel que soit le chemin suivi, DB_TICKET est ici sous sa forme réduite : on
  # reconstruit la forme complète et TICKETS_HEURES.
  if (hydrate_dans(envir))
    cli::cli_alert_success("DB_TICKET reconstruit ({nrow(get('DB_TICKET', envir = envir))} lignes)")

  # Idem pour les colonnes d'accès : un .RData enregistré avant la gestion des
  # droits n'a que DATE_DEBUT, DATE_FIN et PASS (cf. R/acces.R).
  if (exists("DB_PASSWORD", envir = envir, inherits = FALSE))
    assign("DB_PASSWORD",
           normalise_password(get("DB_PASSWORD", envir = envir)),
           envir = envir)

  invisible(drive_env_name)
}