library(tidyverse)
library(janitor)
library(googledrive)
library(googlesheets4)
library(readxl)


link_json <- Sys.getenv("LINK_JSON")
path_drive <- Sys.getenv("PATH_DRIVE")
id_sheet_mazette <- Sys.getenv("ID_DRIVE_MAZETTE")
ID_MAZETTE_2023 <- Sys.getenv("ID_MAZETTE_2023")
ID_MAZETTE_2025 <- Sys.getenv("ID_MAZETTE_2025")
path_logos <- Sys.getenv("PATH_LOGOS")

download.file(link_json,destfile = "connect.json")
drive_auth(path = "connect.json")
gs4_auth(path = "connect.json")

# Configuration des dates
date_before <- today()-lubridate::wday(today(),week_start = 1)-7*7
date_debut_semaine <- today()-lubridate::wday(today(),week_start = 1)+1
date_fin_semaine <- today() + (7 - lubridate::wday(today(), week_start = 1))
date_debut_semaine <- floor_date(today()-2, unit = "week")+1
date_debut_mois <- floor_date(today()-2, unit = "month")+1
date_debut_annee <- floor_date(today()-2, unit = "year")+1

date_debut_semaine_m1 <- date_debut_semaine - 7
date_fin_semaine_m1 <- date_fin_semaine - 7
date_debut_8_semaines <- floor_date(today()-2, unit = "week")-weeks(8)
date_fin_8_semaines <- today()

print(system.time({source("functions.R", local = TRUE)}))
print(system.time({source("import.R", local = TRUE)}))
print(system.time({source("nettoyage_ajout.R", local = TRUE)}))
print(system.time({source("modules.R", local = TRUE)}))

prefix <- "R_env_"
date_jour <- max(DB_JOURS$DATE)
drive_env_name <- paste0(prefix,date_jour,".RData")

# save(list = ls(), file = "outputs/env_entier.RData")
save(list = ls(),
     file = file.path("outputs",drive_env_name),
     compress = "xz",
     compression_level = 9)

# googledrive::drive_upload("outputs/env_entier.RData",
googledrive::drive_upload(file.path("outputs",drive_env_name),
                          name = drive_env_name,
                          path = as_id(path_drive),
                          overwrite = TRUE)
