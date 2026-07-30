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
    cli::cli_h2("Remplir les DB SQL")
    print(system.time({source("sql.R", local = envir)}))
    cli::cli_h2("Importer les DB SQL")
    print(system.time({source("import_sql.R", local = envir)}))
    
    # Prendre la dernière date comme date de sauvegarde
    date_jour      <- max(get("DB_JOURS", envir = envir)$DATE)
    drive_env_name <- paste0(prefix, date_jour, ".RData")
    
    cli::cli_h2("Sauvegarder le rds en local")
    save(list = ls(envir), envir = envir,
         file = file.path("outputs", drive_env_name),
         compress = "xz", compression_level = 9)
    
    cli::cli_h2("Sauvegarder le rds sur drive")
    googledrive::drive_upload(file.path("outputs", drive_env_name),
                              name = drive_env_name,
                              path = as_id(Sys.getenv("PATH_DRIVE")),
                              overwrite = TRUE)
  }
  invisible(drive_env_name)
}