
drive_mazette <- drive_download(drive_get(id =id_sheet_mazette),overwrite = TRUE)
path_mazette <- drive_mazette$local_path

vec_sheets <- c("DB JOURS","IMPORT BRASSINS",
                    "NETTOYAGE ET AJOUTS HOREKO",
                    "IMPORT HEURES MANUELLES",
                    "IMPORT OLD DATA","COMMENTAIRES DASHBOARD",
                    "IMPORT LIGHTSPEED","IMPORT TICKET","IMPORT CAISSE",
                    "IMPORT OBJECTIFS","IMPORT OBJECTIFS 2025",
                    "IMPORT OBJECTIFS 2026",
                    "IMPORT COMPTES 2","IMPORT COMPTES 3","IMPORT PASS")

read_mazette <- function(sheet_name) read_excel(path_mazette,sheet = sheet_name)

DB_sheets <- sapply(vec_sheets, read_mazette)

IMPORT_DB_JOURS <- DB_sheets$`DB JOURS`
IMPORT_BRASSINS <- DB_sheets$`IMPORT BRASSINS`
IMPORT_HOREKO <- DB_sheets$`NETTOYAGE ET AJOUTS HOREKO`
IMPORT_HEURES_MANUELLES <- DB_sheets$`IMPORT HEURES MANUELLES`
IMPORT_OLD <- DB_sheets$`IMPORT OLD DATA`
COMMENTAIRES <- DB_sheets$`COMMENTAIRES DASHBOARD`

IMPORT_LIGHTSPEED <- DB_sheets$`IMPORT LIGHTSPEED`
IMPORT_TICKET <- DB_sheets$`IMPORT TICKET`
IMPORT_CAISSE <- DB_sheets$`IMPORT CAISSE`
IMPORT_OBJECTIFS <- DB_sheets$`IMPORT OBJECTIFS`
IMPORT_OBJECTIFS_2025 <- DB_sheets$`IMPORT OBJECTIFS 2025`
IMPORT_OBJECTIFS_2026 <- DB_sheets$`IMPORT OBJECTIFS 2026`
IMPORT_COMPTES <- DB_sheets$`IMPORT COMPTES 2`
IMPORT_COMPTES_DETAIL <- DB_sheets$`IMPORT COMPTES 3`
IMPORT_PASS <- DB_sheets$`IMPORT PASS`

# EDIT 2026 : ADD OLD

# load_old_mazette <- function(id,name){
#   drive_info <- drive_get(id=id)
#   name_drive <- drive_info$name
#   drive <- drive_download(drive_info,overwrite = TRUE)
#   path <- drive$local_path
# 
#   vec_sheets <- c("IMPORT LIGHTSPEED","IMPORT TICKET","IMPORT CAISSE","DB JOURS")
# 
#   read_mazette <- function(sheet_name) read_excel(path,sheet = sheet_name)
# 
#   DB_sheets <- sapply(vec_sheets, read_mazette)
# 
#   IMPORT_DB_JOURS_OLD <- DB_sheets$`DB JOURS`
#   IMPORT_LIGHTSPEED_OLD <- DB_sheets$`IMPORT LIGHTSPEED`
#   IMPORT_TICKET_OLD <- DB_sheets$`IMPORT TICKET`
#   IMPORT_CAISSE_OLD <- DB_sheets$`IMPORT CAISSE`
# 
#   path_sauv <- paste0("outputs/",name_drive,".RData")
# 
#   # drive_env_name <- "R_env_2023_2025.RData"
#   save(list = c("IMPORT_DB_JOURS_OLD","IMPORT_LIGHTSPEED_OLD",
#                 "IMPORT_TICKET_OLD","IMPORT_CAISSE_OLD"),
#        file = path_sauv,
#        compress = "xz",
#        compression_level = 9)
# }
# 
# load_old_mazette("1eGu42eV5OYdFe9n4OYltuE179E90HRdDysXxr4dhS6g")
# load_old_mazette("13dX8O_iBd3xVpdPjR3gG84Yu4KyUPWpbvePdAhq6kRc")

drive_mazette_2023 <- try(load("outputs/IMPORT 2023-2024.RData"),silent=TRUE)

if (class(drive_mazette_2023)[1] == "try-error"){
  try({
    drive_download(drive_get(id=ID_MAZETTE_2023),overwrite = TRUE)
    load("IMPORT 2023-2024.RData")
  },silent=TRUE)
}

try({
  IMPORT_DB_JOURS <- rbind(IMPORT_DB_JOURS_OLD,IMPORT_DB_JOURS)
  IMPORT_LIGHTSPEED <- rbind(IMPORT_LIGHTSPEED_OLD,IMPORT_LIGHTSPEED)
  IMPORT_TICKET <- rbind(IMPORT_TICKET_OLD,IMPORT_TICKET)
  IMPORT_CAISSE <- rbind(IMPORT_CAISSE_OLD,IMPORT_CAISSE)
},silent=TRUE)

drive_mazette_2025 <- try(load("outputs/IMPORT 2025.RData"),silent=TRUE)

# Chargement via environnement google

if (class(drive_mazette_2025)[1] == "try-error"){
  try({
    drive_download(drive_get(id=ID_MAZETTE_2025),overwrite = TRUE)
    load("IMPORT 2025.RData")
  },silent=TRUE)
}

try({
  IMPORT_DB_JOURS <- rbind(IMPORT_DB_JOURS_OLD,IMPORT_DB_JOURS)
  IMPORT_LIGHTSPEED <- rbind(IMPORT_LIGHTSPEED_OLD,IMPORT_LIGHTSPEED)
  IMPORT_TICKET <- rbind(IMPORT_TICKET_OLD,IMPORT_TICKET)
  IMPORT_CAISSE <- rbind(IMPORT_CAISSE_OLD,IMPORT_CAISSE)
},silent=TRUE)

IMPORT_BIERES_CORRESPONDANCE <- read_excel(path_mazette,sheet = "CORRESPONDANCE BIERES",range = cell_cols("A:D"))

