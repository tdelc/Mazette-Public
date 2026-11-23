#### Authentification Google ####

download.file(Sys.getenv("LINK_JSON"), destfile = "connect.json")
drive_auth(path = "connect.json")

#### Récupération des liens drive ####

drive_paths <- drive_download(drive_get(id =Sys.getenv("ID_DRIVE_PATHS")), overwrite = TRUE)
paths <- drive_paths$local_path

PATHS <- read_excel(paths,sheet = "PATHS")

get_path <- function(PATH){
  PATHS %>% filter(Path == PATH) %>% pull(Link)
}

#### Import de toute la DB actuelle ####

cli::cli_h2("Import de la DB actuelle")

cli::cli_h3("Drive Mazette")

drive_mazette <- drive_download(drive_get(id =get_path("ID_DRIVE_MAZETTE")),
                                overwrite = TRUE)
path_mazette <- drive_mazette$local_path

IMPORT_BIERES_CORRESPONDANCE <- read_excel(path_mazette,sheet = "CORRESPONDANCE BIERES",range = cell_cols("A:D"))

# L'onglet des mots de passe.
SHEET_PASS <- "IMPORT PASS NEW"

# Un onglet d'objectifs par année ; « IMPORT OBJECTIFS » tout court est 2024.
SHEETS_OBJECTIFS <- c("IMPORT OBJECTIFS", "IMPORT OBJECTIFS 2022",
                      "IMPORT OBJECTIFS 2023", "IMPORT OBJECTIFS 2025",
                      "IMPORT OBJECTIFS 2026")

vec_sheets <- c("DB JOURS","IMPORT OLD DATA","IMPORT BRASSINS",
                "IMPORT LIGHTSPEED","IMPORT TICKET","IMPORT CAISSE",
                SHEETS_OBJECTIFS, SHEET_PASS)

read_mazette <- function(sheet_name) suppressWarnings(
  read_excel(path_mazette, sheet = sheet_name, .name_repair = "unique_quiet"))

DB_sheets <- sapply(vec_sheets, read_mazette)

IMPORT_DB_JOURS      <- DB_sheets$`DB JOURS`
IMPORT_DB_OLD        <- DB_sheets$`IMPORT OLD DATA`
IMPORT_BRASSINS      <- DB_sheets$`IMPORT BRASSINS`
IMPORT_LIGHTSPEED    <- DB_sheets$`IMPORT LIGHTSPEED`
IMPORT_TICKET        <- DB_sheets$`IMPORT TICKET`
IMPORT_CAISSE        <- DB_sheets$`IMPORT CAISSE`
# [[ ]] et non $ : `$` sur une liste apparie par préfixe, en silence.
IMPORT_PASS          <- DB_sheets[[SHEET_PASS]]

# Heures pointées (Horeko)

cli::cli_h3("Drive Heures")

drive_heures <- drive_download(drive_get(id =get_path("ID_DRIVE_HEURES")),
                                overwrite = TRUE)

sheets <- excel_sheets(path = drive_heures$local_path)
sheets_heures <- sheets[str_detect(sheets,"[0-9]+")]

IMPORT_HEURES <- sheets_heures |> map_df(~{
  read_excel(drive_heures$local_path,sheet = .x,skip = 1)
})
IMPORT_COUT <- read_excel(drive_heures$local_path,sheet = "Cout")


cli::cli_h3("Drive Compta")

drive_download(drive_get(id =get_path("ID_DB_COMPTA")),overwrite = TRUE)
DB_COMPTA <- readRDS("DB_COMPTA.rds")

#### Compta Odoo ####

# Source optionnelle : sans configuration, la compta reste celle de
# DB_COMPTA.rds. Deux façons de la renseigner, par priorité :
#
# 1. l'API : les variables ODOO_URL, ODOO_DB, ODOO_LOGIN, ODOO_CLE_API
#    (cf. R/odoo_api.R ; vérifier avec `Rscript essai_odoo_api.R`) ;
# 2. un fichier : la ligne ID_ODOO_ECRITURES de l'onglet PATHS, vers l'export
#    « Écritures comptables » (classeur Google ou .xlsx).
#
# Odoo commence à la reprise du 30/06/2026 : il prend la suite de DB_COMPTA
# (cf. coud_compta() dans R/odoo.R).
ID_ODOO  <- get_path("ID_ODOO_ECRITURES")
IDS_ODOO <- identifiants_odoo()

# Les deux sources rendent le même tableau brut, puis suivent la même chaîne.
A_LIRE_ODOO <- !is.null(IDS_ODOO) ||
  (length(ID_ODOO) == 1 && !is.na(ID_ODOO) && nzchar(ID_ODOO))

if (A_LIRE_ODOO) {
  cli::cli_h3(if (!is.null(IDS_ODOO)) "API Odoo" else "Drive Odoo")

  # Un échec de lecture n'emporte pas l'import : DB_COMPTA reste tel quel.
  COMPTA_ODOO <- tryCatch({
    # Le brut sert à compter les lignes écartées par la normalisation.
    BRUT_ODOO <- if (!is.null(IDS_ODOO)) {
      cli::cli_alert_info("Odoo : lecture par l'API ({IDS_ODOO$url})")
      lit_ecritures_odoo(IDS_ODOO)
    } else {
      # Variables Odoo incomplètes : on prévient que le fichier prend le relais.
      if (length(manque_odoo()) && length(manque_odoo()) < length(ODOO_ENV))
        cli::cli_alert_warning(paste(
          "Odoo API : il manque {.envvar {ODOO_ENV[manque_odoo()]}} —",
          "lecture par fichier, donc aussi fraîche que le dernier export"))
      drive_odoo <- drive_download(drive_get(id = ID_ODOO), overwrite = TRUE)
      lit_ecritures_fichier(drive_odoo$local_path)
    }
    ECRITURES_ODOO <- normalise_ecritures(BRUT_ODOO)
    cli::cli_alert_info("Odoo : {nrow(ECRITURES_ODOO)} écritures lues")

    # Flèche simple dans le tryCatch() : un `<<-` sauterait l'environnement de
    # l'import (R/connect.R), et la table manquerait au cache.
    DB_ECRITURES <- ECRITURES_ODOO

    cp <- compta_mensuelle(ECRITURES_ODOO)

    # Contrôles journalisés dès l'import (même remarque sur la flèche).
    CONTROLE_ODOO <- controle_ecritures(ECRITURES_ODOO, cp,
                                        reprise = REPRISE_ODOO,
                                        n_brut = nrow(BRUT_ODOO))
    for (i in seq_len(nrow(CONTROLE_ODOO))) {
      msg <- paste0("Odoo — ", CONTROLE_ODOO$CONTROLE[i], " : ", CONTROLE_ODOO$DETAIL[i])
      if (CONTROLE_ODOO$VERDICT[i] == "ok") cli::cli_alert_success(msg)
      else                                  cli::cli_alert_warning(msg)
    }
    cp
  }, error = function(e) {
    cli::cli_alert_danger("Odoo : lecture impossible — {conditionMessage(e)}")
    cli::cli_alert_warning("Odoo : la comptabilité reste celle de DB_COMPTA.rds")
    NULL
  })

  # Sans rien à coudre, coud_compta() tronquerait DB_COMPTA à la bascule.
  if (!is.null(COMPTA_ODOO) && nrow(COMPTA_ODOO)) {
    CONTROLE_COUTURE <- controle_couture(DB_COMPTA, COMPTA_ODOO)
    for (i in seq_len(nrow(CONTROLE_COUTURE))) {
      msg <- paste0("Couture — ", CONTROLE_COUTURE$CONTROLE[i], " : ",
                    CONTROLE_COUTURE$DETAIL[i])
      if (CONTROLE_COUTURE$VERDICT[i] == "ok") cli::cli_alert_success(msg)
      else                                     cli::cli_alert_warning(msg)
    }

    n_avant <- nrow(DB_COMPTA)
    DB_COMPTA <- coud_compta(DB_COMPTA, COMPTA_ODOO)
    cli::cli_alert_success(
      "Compta cousue : {n_avant} lignes + {nrow(COMPTA_ODOO)} d'Odoo = {nrow(DB_COMPTA)}")
  }
} else {
  cli::cli_alert_warning(paste(
    "Ni les variables {.envvar ODOO_*} ni ID_ODOO_ECRITURES dans les PATHS :",
    "la compta reste celle de DB_COMPTA.rds"))
  if (length(manque_odoo()) && length(manque_odoo()) < length(ODOO_ENV))
    cli::cli_alert_warning(
      "Odoo API : il manque {.envvar {ODOO_ENV[manque_odoo()]}}")
}

# Les décisions prises sur les mois douteux (cf. R/validation.R) : un
# journal relu tel quel ; son absence ne coûte rien.
cli::cli_h3("Validations de mois")
DB_VALIDATION_COMPTA <- lit_validations()

# Réservations

cli::cli_h3("Drive Réservations")

drive_resa <- drive_download(drive_get(id =get_path("ID_DRIVE_RESA")),
                                overwrite = TRUE)
path_resa <- drive_resa$local_path

IMPORT_HIST <- read_excel(path_resa,sheet = "RESA")

IMPORT_RESA_SALLE <- NULL
IMPORT_RESA_EXT <- NULL
try({IMPORT_RESA_SALLE <- calendar::ic_read(get_path("ICS_SALLE"))},silent=T)
try({IMPORT_RESA_EXT <- calendar::ic_read(get_path("ICS_TERRASSE"))},silent=T)

# Heures planifiées

IMPORT_PLANNING <- calendar::ic_read(get_path("ICS_HEURES"))

DB_HEURES_PLANNING <- IMPORT_PLANNING |> 
  filter(SUMMARY == "", substr(UID,1,5) == "shift", is.na(STATUS)) |> 
  transmute(
    INFOS = str_split(DESCRIPTION,"\\\\n"), 
    INDIVIDU = str_remove(unlist(lapply(INFOS,function(x)x[1])),"Medewerker: "),
    SERVICE = str_remove(unlist(lapply(INFOS,function(x)x[3])),"Team: "),
    SERVICE = str_remove(SERVICE,"\\n "),
    SERVICE = str_remove(SERVICE,"Team: "),
    DT_START = ymd_hms(`DTSTART;TZID=Europe/Brussels`),
    DT_END   = ymd_hms(`DTEND;TZID=Europe/Brussels`),
    DATE = as.Date(DT_START),
    HEURES   = as.numeric(difftime(DT_END, DT_START, units = "hours"))
  )

DB_HEURES_PLANNING <- DB_HEURES_PLANNING |> 
  group_by(DATE, SERVICE) |> 
  summarise(HEURES = sum(HEURES)) |> 
  ungroup()

# Le calendrier de shifts ne garde que l'avenir : on l'archive dans un
# Google Sheet à chaque import (cf. R/planning.R).
SS_PLANNING <- get_path("PATH_HEURES_PLANNING")

if (length(SS_PLANNING) == 1 && !is.na(SS_PLANNING) && nzchar(SS_PLANNING)) {
  # googlesheets4 reprend le jeton du compte de service de googledrive.
  if (requireNamespace("googlesheets4", quietly = TRUE))
    try(googlesheets4::gs4_auth(token = googledrive::drive_token()),
        silent = TRUE)

  # Un incident d'archivage ne fait jamais tomber l'import.
  archive <- try(historise_planning(DB_HEURES_PLANNING, SS_PLANNING),
                 silent = TRUE)
  if (inherits(archive, "try-error"))
    cli::cli_alert_warning("Historisation du planning impossible : {as.character(archive)}")
  else if (!is.null(archive))
    DB_HEURES_PLANNING <- archive
} else {
  cli::cli_alert_warning(
    "PATH_HEURES_PLANNING absent des PATHS : le planning reste sans historique")
}

# Fichier de paie (ONSS) : coût employeur et heures travaillées, par secteur
# et par mois (cf. R/sources_travail.R).
SS_ONSS <- get_path("PATH_HEURES_ONSS")

if (length(SS_ONSS) == 1 && !is.na(SS_ONSS) && nzchar(SS_ONSS)) {
  if (requireNamespace("googlesheets4", quietly = TRUE))
    try(googlesheets4::gs4_auth(token = googledrive::drive_token()),
        silent = TRUE)

  IMPORT_ONSS <- lit_onss(SS_ONSS)

  # Contrôle de lecture journalisé dès l'import : une colonne renommée se voit
  # ici avant de se lire comme une baisse d'heures.
  CONTROLE_ONSS <- controle_onss(IMPORT_ONSS)
  if (!is.null(CONTROLE_ONSS)) {
    if (CONTROLE_ONSS$COLONNES_VUES < CONTROLE_ONSS$COLONNES_ATTENDUES)
      cli::cli_alert_danger(
        "Paie : {CONTROLE_ONSS$COLONNES_VUES}/{CONTROLE_ONSS$COLONNES_ATTENDUES} colonnes de secteur trouvees")
    if (abs(CONTROLE_ONSS$ECART) > 1)
      cli::cli_alert_warning(
        "Paie : {round(CONTROLE_ONSS$ECART)} h d'ecart entre secteurs et heures reelles")
  }

  DB_ONSS <- construit_onss(IMPORT_ONSS)
  if (onss_valide(DB_ONSS))
    cli::cli_alert_success(
      "Paie : {nrow(DB_ONSS)} lignes, {length(unique(paste(DB_ONSS$ANNEE, DB_ONSS$MOIS)))} mois")
  else
    cli::cli_alert_warning("Paie : aucune ligne exploitable")
} else {
  cli::cli_alert_warning(
    "PATH_HEURES_ONSS absent des PATHS : le coût du travail reste estimé par Horeko")
}

# Archives de caisse, empilées sous les données du jour : chacune est lue dans
# outputs/ si elle y est, sinon téléchargée depuis Drive (clé des PATHS).
ARCHIVES_CAISSE <- c("IMPORT 2023-2024.RData" = "ID_MAZETTE_2023",
                     "IMPORT 2025.RData"      = "ID_MAZETTE_2025")

for (fichier in names(ARCHIVES_CAISSE)) {
  lu <- try(load(file.path("outputs", fichier)), silent = TRUE)
  if (inherits(lu, "try-error"))
    try({
      drive_download(drive_get(id = get_path(ARCHIVES_CAISSE[[fichier]])),
                     overwrite = TRUE)
      load(fichier)
    }, silent = TRUE)

  try({
    if (!"BRUSSELS" %in% colnames(IMPORT_DB_JOURS_OLD))
      IMPORT_DB_JOURS_OLD$BRUSSELS = 0
    # Dates alignées avant d'empiler : rbind() ferait du texte d'une Date et
    # d'une chaîne.
    IMPORT_DB_JOURS_OLD$DATEVALUE <- parse_date_souple(IMPORT_DB_JOURS_OLD$DATEVALUE)
    IMPORT_DB_JOURS$DATEVALUE     <- parse_date_souple(IMPORT_DB_JOURS$DATEVALUE)
    IMPORT_DB_JOURS   <- rbind(IMPORT_DB_JOURS_OLD, IMPORT_DB_JOURS)
    IMPORT_LIGHTSPEED <- rbind(IMPORT_LIGHTSPEED_OLD, IMPORT_LIGHTSPEED)
    IMPORT_TICKET     <- rbind(IMPORT_TICKET_OLD, IMPORT_TICKET)
    IMPORT_CAISSE     <- rbind(IMPORT_CAISSE_OLD, IMPORT_CAISSE)
  }, silent = TRUE)
}

#### Premiers traitements des données sources ####

##### IMPORT DB JOURS ####

cli::cli_h3("Import de la DB JOURS")

# On parse avant de filtrer : une cellule illisible est écartée et comptée.
DB_JOURS <- IMPORT_DB_JOURS  |>
  mutate(DATE = parse_date_souple(DATEVALUE)) |>
  filter(!is.na(DATE)) |>
  transmute(
    DATE,
    CARTES,
    CASH,
    GIFTCARD,
    VIREMENT,
    CA_HTVA  = `CA HTVA`,
    CA_TVAC  = `CA TVAC`
  )

# Les lignes perdues se comptent.
controle_dates_jours <- function(brut, net) {
  perdues <- sum(!is.na(brut$DATEVALUE)) - nrow(net)
  if (perdues > 0)
    cli::cli_alert_warning(
      "DB JOURS : {perdues} ligne{?s} datée{?s} mais illisible{?s}")
  else
    cli::cli_alert_success("DB JOURS : {nrow(net)} jours, toutes dates lues")
}
controle_dates_jours(IMPORT_DB_JOURS, DB_JOURS)

##### IMPORT DB OLD ####

cli::cli_h3("Import de la DB OLD")

DB_OLD <- IMPORT_DB_OLD %>%
  rename(DATE = DATEVALUE,
         CA_TVAC = `CA TVAC`,
         CA_CAISSE_0 = `CA TVA 0%`,
         CA_CAISSE_6 = `CA TVA 6%`,
         CA_CAISSE_12 = `CA TVA 12%`,
         CA_CAISSE_21 = `CA TVA 21%`,
         NB_TABLES = Tables,
         NB_CLIENTS = Couverts) %>%
  mutate(DATE = ymd(DATE),
         TVA_CAISSE_6 = 0.06*CA_CAISSE_6/1.06,
         TVA_CAISSE_12 = 0.12*CA_CAISSE_12/1.12,
         TVA_CAISSE_21 = 0.21*CA_CAISSE_21/1.21,
         CA_HTVA = CA_TVAC - (TVA_CAISSE_6+TVA_CAISSE_12+TVA_CAISSE_21)
  ) |> 
  select(DATE,CA_TVAC,CA_HTVA)


##### IMPORT BRASSINS ####

cli::cli_h3("Import de la DB BRASSINS")

DB_BRASSINS <- IMPORT_BRASSINS  |>
  filter(!is.na(`Numéro de brassin`) & !is.na(`Nom commercial`)) %>%
  select(`Numéro de brassin`,`Nom commercial`,`Date de brassage`,
         `Date de conditionnement`, `Volume ST (hL)`,`Volume Fûts (hL)`,
         `Date de fin de bière`,`Date de déclaration`,`Bière finie?`) %>%
  rename(
    ID_BRASSIN         = `Numéro de brassin`,
    NOM_BRASSIN        = `Nom commercial`,
    DT_BRASSIN         = `Date de brassage`,
    DT_CONDI           = `Date de conditionnement`,
    VOLUME_BRASSIN     = `Volume ST (hL)`,
    VOLUME_BRASSIN_ADD = `Volume Fûts (hL)`,
    DT_FIN             = `Date de fin de bière`,
    DT_DECLA           = `Date de déclaration`,
    BIERE_FINIE        = `Bière finie?`
  ) |> 
  mutate(
    ID_BRASSIN         = gsub("\\.0","",ID_BRASSIN),
    BIERE_FINIE        = as.numeric(BIERE_FINIE),
    DT_BRASSIN         = parse_date_souple(DT_BRASSIN),
    DT_CONDI           = parse_date_souple(DT_CONDI),
    DT_FIN             = parse_date_souple(DT_FIN),
    DT_DECLA           = parse_date_souple(DT_DECLA),
    DT_FIN             = if_else(is.na(DT_FIN),DT_DECLA,DT_FIN),
    DT_FIN             = if_else(is.na(DT_FIN),today(),DT_FIN),
    VOLUME_BRASSIN     = VOLUME_BRASSIN * 100,
    VOLUME_BRASSIN_ADD = VOLUME_BRASSIN_ADD * 100,
    VOLUME_BRASSIN     = VOLUME_BRASSIN + VOLUME_BRASSIN_ADD
  ) |> 
  select(-VOLUME_BRASSIN_ADD)

##### IMPORT PRODUITS ####

cli::cli_h3("Import de la DB PRODUITS")

DB_PRODUITS <- IMPORT_LIGHTSPEED  |>
  filter(!is.na(PRODUCT)) |> 
  transmute(
    DATE = parse_date_souple(DATE),
    CATEGORIE = toupper(trimws(CATEGORY)),
    PRODUIT = PRODUCT,
    PRIX = PRICE,
    QUANTITE = `#`,
    CA_TVAC = TOTAL,
    CA_HTVA = PROFIT,
    TAUX_TVA = `TVA RATE`
  ) %>%
  mutate(TAUX_TVA = as.numeric(gsub("%", "", gsub(",",".",TAUX_TVA)))/ 100,
         TAUX_TVA = ifelse(TAUX_TVA < 0.01,TAUX_TVA*100,TAUX_TVA),
         TAUX_TVA = replace_na(TAUX_TVA,0)) |> 
  mutate(
    QUANTITE = case_when(
      substr(PRODUIT,1,4) == "Dikk" & PRIX < 6 ~ QUANTITE/4,
      substr(PRODUIT,1,4) == "Dikk" & PRIX < 10 ~ QUANTITE/3,
      TRUE ~ QUANTITE)
  ) %>%
  from_product_to_boisson() |> 
  select(DATE,CATEGORIE,PRODUIT_FULL,PRODUIT,BOISSON,VOLUME_CL,
         PRIX,QUANTITE,CA_TVAC,CA_HTVA,TAUX_TVA)

##### IMPORT TICKET ####

cli::cli_h3("Import de la DB TICKETS")

DB_TICKET <- IMPORT_TICKET %>%
  mutate(
    TIMESTAMP = str_remove(TIMESTAMP," \\+[0-9]{4}"),
    TIMESTAMP = dmy_hm(TIMESTAMP),
    MIDI         = as.numeric(hour(TIMESTAMP) %in% c(8:16)),
    WEEK_END     = as.numeric(wday(TIMESTAMP, week_start = 1) %in% c(6,7)
                              | (wday(TIMESTAMP,week_start = 1) == 5 & !MIDI))
  ) |> 
  transmute(
    DATE          = parse_date_souple(DATE),
    TIMESTAMP     = as.character(TIMESTAMP),
    TIMESTAMP     = ifelse(nchar(TIMESTAMP) == 19,TIMESTAMP,
                           paste(substr(TIMESTAMP,1,10),"00:00:00")),
    ID_TICKET     = as.numeric(ID_TICKET), 
    ID_PRODUIT    = as.numeric(ID),
    PRODUIT       = PRODUCT,
    NB_CLIENTS    = as.numeric(NB_CUSTOMERS),
    PRIX_UNITE,
    QUANTITE,
    PRIX_TOTAL,
    MIDI,
    WEEK_END
  )

##### IMPORT OBJECTIFS ####

cli::cli_h3("Import de la DB OBJECTIFS")

DB_OBJECTIFS <- bind_rows(lapply(DB_sheets[SHEETS_OBJECTIFS],
                                 transmute_objectifs)) |> 
  select(ANNEE,MOIS,DATE_DEBUT,DATE_FIN,CA_TVAC,CA_HTVA,
         CA_HTVA_NOURRITURE_6,CA_HTVA_NOURRITURE_12,CA_HTVA_BOISSON_21) |> 
  mutate(DATE_DEBUT = ymd(as.character(DATE_DEBUT)),
         DATE_FIN = ymd(as.character(DATE_FIN)))

##### IMPORT NOMENCLATURE BIERES ####

cli::cli_h3("Import de la DB NOMENCLATURE BIERE")

NOMEN_BIERES <- IMPORT_BIERES_CORRESPONDANCE %>%
  filter(!is.na(`Nom commercial`)) |> 
  transmute(
    NOM_BRASSIN = `Nom commercial`,
    BOISSON ,
    LOGO    = `NAME LOGO`
  )

##### IMPORT_PASS #####

cli::cli_h3("Import de la DB PASSWORD")

# Colonnes de l'onglet des accès :
#
#   Date_debut | Date_fin | pass | nom | role | onglets | actif
#
# Les quatre dernières sont facultatives (cf. colonne_optionnelle() dans
# R/acces.R) ; absentes, chaque mot de passe a tous les droits.
#
#   nom     à qui appartient ce mot de passe
#   role    un profil de PROFILS (R/acces.R) ; inconnu ou vide = l'onglet de
#           départ seulement
#   onglets onglets en plus du rôle : "compta, travail", ou "*" pour tout
#   actif   "non" coupe l'accès sans effacer la ligne
DB_PASSWORD <- IMPORT_PASS |>
  transmute(
    DATE_DEBUT    = parse_date_souple(Date_debut),
    DATE_FIN      = parse_date_souple(Date_fin),
    PASS          = trimws(as.character(pass)),
    NOM           = colonne_optionnelle(IMPORT_PASS, "nom",     NA_character_),
    ROLE          = colonne_optionnelle(IMPORT_PASS, "role",    "admin"),
    ONGLETS_LISTE = colonne_optionnelle(IMPORT_PASS, "onglets", NA_character_),
    ACTIF         = oui_non(colonne_optionnelle(IMPORT_PASS, "actif", "oui"))
  ) |>
  # Une ligne sans mot de passe ou sans période n'ouvre rien.
  filter(!is.na(PASS), nzchar(PASS), !is.na(DATE_DEBUT), !is.na(DATE_FIN))

##### IMPORT_HEURES #####

cli::cli_h3("Import de la DB HEURES")

DB_HEURES <- IMPORT_HEURES %>% 
  filter(!is.na(Jour),Jour != "Jour",Jour != "Nom") %>% 
  mutate(DATE = as.Date(as.numeric(Jour), origin = "1899-12-30")) %>% 
  mutate(HEURE_DEB = substr(Temps,1,5),
         HEURE_FIN = substr(Temps,9,13)) %>% 
  rename(CONTRAT = `Type de contrat`,
         DEPARTEMENT = `Nom de l’équipe`,
         HEURES_PAUSE = Pauses) |> 
  filter(Temps != "12:08 - 12:06") |> 
  mutate(
    debut_h = hour(hm(HEURE_DEB)) + minute(hm(HEURE_DEB))/60,
    fin_h   = hour(hm(HEURE_FIN))   + minute(hm(HEURE_FIN))/60,
    HEURE_MIDI = case_when(
      debut_h == 0 & fin_h == 0 ~ 0,
      debut_h >= 17 ~ 0,
      fin_h <= 17 & fin_h > 7 ~ fin_h - debut_h,
      TRUE ~ 17 - debut_h
    ),
    HEURE_SOIR = case_when(
      debut_h == 0 & fin_h == 0 ~ 0,
      fin_h > 7 & fin_h <= 17 ~ 0,
      fin_h >= 17 ~ fin_h - 17,
      debut_h >= 17 ~ 24 - debut_h + fin_h,
      TRUE ~ 7 + fin_h
    )
  ) %>%
  mutate(HEURES = as.numeric(`Heures travaillées`)) %>% 
  filter(HEURES > 0) %>% 
  mutate(
    PC_MIDI = HEURE_MIDI / (HEURE_MIDI + HEURE_SOIR),
    PC_SOIR = HEURE_SOIR / (HEURE_MIDI + HEURE_SOIR),
    HEURE_MIDI = HEURES * PC_MIDI,
    HEURE_SOIR = HEURES * PC_SOIR
  ) %>% 
  select(DATE,CONTRAT,DEPARTEMENT,HEURE_MIDI,HEURE_SOIR,
         HEURE_DEB,HEURE_FIN,debut_h,fin_h,HEURES,HEURES_PAUSE) %>%
  pivot_longer(cols = c(HEURE_MIDI,HEURE_SOIR), names_to = "CD_HEURE",values_to = "NB_HEURES") %>% 
  group_by(DATE,CONTRAT,DEPARTEMENT,CD_HEURE) %>% 
  summarise(NB_HEURES = sum(NB_HEURES)) |> 
  ungroup()

colnames(IMPORT_COUT) <- c("CONTRAT","COUT_HEURE")

DB_HEURES <- DB_HEURES %>% left_join(IMPORT_COUT) %>% 
  mutate(COUT = COUT_HEURE * NB_HEURES)

# Un contrat sans taux rend NA son coût et celui de tout le groupe du jour :
# on le signale (cf. contrats_sans_taux()).
sans_taux <- contrats_sans_taux(DB_HEURES)
if (is.null(sans_taux)) {
  cli::cli_alert_success(
    "Horeko : les {length(unique(DB_HEURES$CONTRAT))} contrats pointés ont un taux")
} else {
  detail <- paste0(sans_taux$CONTRAT, " (", round(sans_taux$HEURES), " h)",
                   collapse = ", ")
  cli::cli_alert_warning(
    "Horeko : {nrow(sans_taux)} contrat{?s} sans taux horaire - {detail}")
}

##### IMPORT RESA ####

cli::cli_h3("Import de la DB RESA")

DB_RESA_HIST <- IMPORT_HIST |> 
  transmute(
    HEURE_DEB = `Heure début`,
    HEURE_FIN = `Heure fin`,
    NB_PERS = `Nombre de personnes`,
    LOCATION = ifelse(str_detect(`...9`, "en salle"),"SALLE","TERRASSE"),
    A_VENIR = FALSE
  )

if (is.null(IMPORT_RESA_SALLE)){
  DB_RESA_SALLE <- NULL
}else{
  DB_RESA_SALLE <- IMPORT_RESA_SALLE |> 
    transmute(
      NB_PERS = as.numeric(str_extract(SUMMARY, "([0-9]+) pax.*",group = 1)),
      HEURE_DEB = ymd_hms(`DTSTART;TZID=Europe/Brussels`),
      HEURE_FIN = ymd_hms(`DTEND;TZID=Europe/Brussels`),
      LOCATION = "SALLE",
      A_VENIR = TRUE
    )
}

if (is.null(IMPORT_RESA_EXT)){
  DB_RESA_EXT <- NULL
}else{

  DB_RESA_EXT <- IMPORT_RESA_EXT |> 
    transmute(
      NB_PERS = as.numeric(str_extract(SUMMARY, "([0-9]+) pax.*",group = 1)),
      HEURE_DEB = ymd_hms(`DTSTART;TZID=Europe/Brussels`),
      HEURE_FIN = ymd_hms(`DTEND;TZID=Europe/Brussels`),
      LOCATION = "TERRASSE",
      A_VENIR = TRUE
    )
}

DB_RESA <- rbind(DB_RESA_HIST,DB_RESA_SALLE,DB_RESA_EXT) |> distinct()


#### Création des tables intermédiaires ####

cli::cli_h2("Création des tables intermédiaires")

##### Nomenclature Produits ######

cli::cli_h3("Nomenclature Produits")

# Les produits sont identifiés par leur ID ; le libellé n'est qu'un nom sur
# le ticket.

NOMEN_PRODUITS <- libelle_canonique(DB_TICKET) |> 
  mutate(PRODUIT = case_when(
    PRODUIT == "Cola maison" ~ "Cola maison 33cL",
    PRODUIT == "Dik effiloché de porc crémeux de carottes 1/2" ~ "Dik effiloché de porc crémeux de carottes",
    PRODUIT == "Sombre Despote" ~ "Sombre Despote 33cL",
    TRUE ~ PRODUIT)) %>%
  from_product_to_boisson()

# Le taux de TVA et la catégorie, depuis l'export produits.

PRODUITS_REF <- DB_PRODUITS %>%
  filter(PRIX > 0) %>%
  mutate(TAUX_TVA = if_else(
    PRODUIT_FULL == "Pain à emporter",0.06,TAUX_TVA)) %>%
  select(PRODUIT_FULL,TAUX_TVA,CATEGORIE) %>%
  distinct() %>%
  group_by(PRODUIT_FULL) %>% arrange(-TAUX_TVA) %>%
  filter(row_number() == 1) %>% ungroup()

vec_sale <- PRODUITS_REF %>% filter(CATEGORIE == "SALÉ") %>%
  pull(PRODUIT_FULL) %>% unique()

vec_sucre <- PRODUITS_REF %>% filter(CATEGORIE == "SUCRÉ") %>%
  pull(PRODUIT_FULL) %>% unique()

PRODUITS_REF[PRODUITS_REF$PRODUIT_FULL %in% vec_sale,"CATEGORIE"] <- "SALÉ"
PRODUITS_REF[PRODUITS_REF$PRODUIT_FULL %in% vec_sucre,"CATEGORIE"] <- "SUCRÉ"

# Le taux de TVA, cherché sur tous les libellés du produit (cf. R/donnees.R).
TVA_PAR_ID <- tva_par_produit(DB_TICKET, PRODUITS_REF)

# Les remises (« 20% sur produit : Espresso ») prennent le taux du produit
# remisé, ou 21 % s'il est introuvable (cf. R/donnees.R).
TVA_REMISES <- tva_remises(DB_TICKET, PRODUITS_REF, TVA_PAR_ID)
if (nrow(TVA_REMISES))
  cli::cli_alert_info("{nrow(TVA_REMISES)} remise{?s} rattachée{?s} au taux du produit remisé.")
TVA_PAR_ID <- bind_rows(TVA_PAR_ID, TVA_REMISES)

# Les produits sans taux sont gardés : leur libellé, leur volume et leur
# famille restent utiles, et leur TVAC est de l'argent encaissé. Seul leur
# HTVA est inconnu.
NOMEN_PRODUITS <- NOMEN_PRODUITS |> 
  left_join(TVA_PAR_ID, by = "ID_PRODUIT")

# Contrôle : ce qui reste sans taux de TVA (cf. R/donnees.R).
CONTROLE_TVA <- produits_sans_tva(DB_TICKET, TVA_PAR_ID)

if (nrow(CONTROLE_TVA)) {
  part <- 100 * sum(CONTROLE_TVA$CA_TVAC) / sum(DB_TICKET$PRIX_TOTAL, na.rm = TRUE)
  cli::cli_alert_warning(
    "{nrow(CONTROLE_TVA)} produit{?s} sans taux de TVA : {round(part, 2)} % du CA TVAC.")
  for (i in seq_len(min(10, nrow(CONTROLE_TVA))))
    cli::cli_alert_info("  {CONTROLE_TVA$PRODUIT[i]} — {round(CONTROLE_TVA$CA_TVAC[i])} € TVAC")
} else {
  cli::cli_alert_success("Tous les produits vendus ont un taux de TVA.")
}

#### Création des tables finales ####
cli::cli_h2("Création des tables finales")

##### DB_JOURS ####
cli::cli_h3("Table DB_JOURS")

# Avec l'historique (DB OLD).

DB_JOURS <- DB_JOURS %>%
  add_row(DB_OLD) %>%
  arrange(DATE)

##### DB_DATE ####
cli::cli_h3("Table DB_DATE")

# Toutes les dates, du premier jour de DB_JOURS à aujourd'hui.

DB_DATE <- creer_db_date(min(DB_JOURS$DATE))

##### DB_TICKETS_HEURES ####
cli::cli_h3("Table DB_TICKETS_HEURES")

# La forme réduite (celle qui est stockée), puis son hydratation par
# hydrate_donnees(), la même qu'au chargement du cache (cf. R/donnees.R).


# Une ligne par produit dans la nomenclature, sinon la jointure dupliquerait
# des ventes.
if (anyDuplicated(NOMEN_PRODUITS$ID_PRODUIT))
  cli::cli_abort("NOMEN_PRODUITS porte plusieurs lignes pour un même ID_PRODUIT.")

# Jointure sur ID_PRODUIT seul : les attributs viennent du produit, le
# libellé de la ligne reste celui de la caisse.
normalise <- normalise_tickets(
  DB_TICKET |> 
    rename(PRODUIT_FULL = PRODUIT) |> 
    left_join(NOMEN_PRODUITS |> select(-PRODUIT_FULL), by = "ID_PRODUIT") |> 
    # Un produit hors nomenclature garde son libellé de caisse.
    mutate(PRODUIT = coalesce(PRODUIT, PRODUIT_FULL)))
DB_TICKET    <- normalise$DB_TICKET
REF_PRODUITS <- normalise$REF_PRODUITS
rm(normalise)

# DB_TICKET reste sous forme réduite (c'est elle qu'on sauvegarde) ; la suite
# n'a besoin que de DB_TICKETS_HEURES.
hydrate <- hydrate_donnees(DB_TICKET, REF_PRODUITS)

# L'hydratation doit rendre exactement la table réduite, ligne pour ligne.
if (nrow(hydrate$DB_TICKET) != nrow(DB_TICKET))
  cli::cli_abort(paste0("La reconstruction des tickets duplique des lignes : ",
                        nrow(DB_TICKET), " avant, ",
                        nrow(hydrate$DB_TICKET), " après."))

DB_TICKETS_HEURES <- hydrate$TICKETS_HEURES
rm(hydrate)

##### DB_KPI ####
cli::cli_h3("Table DB_KPI")

DB_KPI <- DB_TICKETS_HEURES %>%
  filter(!is.na(CD_SECTEUR)) %>%
  group_by(DATE,CD_PERIODE_JOUR,CD_PERIODE_SEMAINE,CD_SECTEUR) %>%
  summarise(CA_HTVA = sum(CA_HTVA),CA_TVAC = sum(CA_TVAC),.groups = "drop") %>%
  filter(!is.na(DATE))

# Le CA de DB_KPI diffère de celui de DB_JOURS (les factures n'y sont pas) :
# DB_KPI_SIMPLE repart donc de DB_JOURS.


DB_KPI_JOUR <- DB_KPI %>%
  group_by(DATE,CD_PERIODE_JOUR) %>%
  summarise(CA_HTVA = sum(CA_HTVA),.groups = "drop") %>%
  pivot_wider(names_from = CD_PERIODE_JOUR,
              values_from = CA_HTVA,values_fill = 0)

DB_KPI_SEMAINE <- DB_KPI %>%
  group_by(DATE,CD_PERIODE_SEMAINE) %>%
  summarise(CA_HTVA = sum(CA_HTVA),.groups = "drop") %>%
  pivot_wider(names_from = CD_PERIODE_SEMAINE,
              values_from = CA_HTVA,values_fill = 0)

DB_KPI_SECTEUR <- DB_KPI %>%
  group_by(DATE,CD_SECTEUR) %>%
  summarise(CA_HTVA = sum(CA_HTVA),.groups = "drop") %>%
  pivot_wider(names_from = CD_SECTEUR,
              values_from = CA_HTVA,values_fill = 0)

DB_KPI_SIMPLE <- DB_JOURS %>% 
  select(DATE,CA_HTVA,CA_TVAC) %>%
  left_join(DB_KPI_JOUR, by = "DATE") %>%
  left_join(DB_KPI_SEMAINE, by = "DATE") %>%
  left_join(DB_KPI_SECTEUR, by = "DATE") %>%
  mutate(CA_HTVA_KEEP = CA_HTVA)

# Règle de trois : chaque répartition retombe sur le CA de DB_JOURS.

# Toutes sur CA_HTVA : cf. recaler_repartitions() (R/donnees.R).

DB_KPI_SIMPLE <- recaler_repartitions(DB_KPI_SIMPLE)

##### DB_OBJECTIFS #####
cli::cli_h3("Table DB_OBJECTIFS")

# Objectifs mensuels répartis en objectifs journaliers.

DB_OBJECTIFS <- bind_rows(
  lapply(1:nrow(DB_OBJECTIFS), function(i) {
    with(DB_OBJECTIFS[i, ],
         calculer_objectifs_journaliers(ANNEE, MOIS, CA_HTVA, CA_TVAC))
  })
)

##### DB_BRASSINS ####
cli::cli_h3("Table DB_BRASSINS")

DB_BRASSINS <- DB_BRASSINS %>% 
  left_join(NOMEN_BIERES, by = "NOM_BRASSIN")

# Une ligne par date × brassin ; en cas de doublon, le brassin le plus récent.
DB_BRASSINS_EXP <- DB_BRASSINS %>%
  filter(!is.na(DT_CONDI)) %>%
  mutate(DT_FIN = if_else(DT_FIN < DT_CONDI,DT_CONDI+1,DT_FIN)) %>%
  mutate(DATE = map2(DT_CONDI, DT_FIN, seq, by = "day")) %>%
  unnest(DATE) %>%
  select(ID_BRASSIN,BOISSON, DATE) %>%
  arrange(BOISSON,DATE,desc(ID_BRASSIN)) %>%
  group_by(BOISSON,DATE) %>%
  mutate(NB = n()) %>%
  group_by(BOISSON,DATE,NB) %>%
  mutate(ID_DOUBLON = ifelse(NB == 1,1,row_number())) %>%
  ungroup() %>%
  filter(ID_DOUBLON == 1) %>%
  select(-NB,-ID_DOUBLON)

##### DB_PRODUITS ####
cli::cli_h3("Table DB_PRODUITS")

# Rattache chaque vente de bière à son brassin.
DB_PRODUITS <- DB_PRODUITS %>%
  left_join(DB_BRASSINS_EXP, by = c("DATE","BOISSON")) %>%
  left_join(DB_BRASSINS, by = c("BOISSON","ID_BRASSIN")) %>%
  mutate(VOLUME_BRASSIN_AJUST = VOLUME_BRASSIN*0.75)

PRIX_BIERES <- DB_PRODUITS %>%
  filter(CATEGORIE ==  "BIÈRES" & !is.na(ID_BRASSIN) & VOLUME_CL == 33) %>%
  select(PRIX_33CL = PRIX,BOISSON) %>%
  distinct() %>%
  group_by(BOISSON) %>%
  arrange(-PRIX_33CL) %>%
  filter(row_number() == 1) |> 
  ungroup()

##### DB_BIERES ####
cli::cli_h3("Table DB_BIERES")

# Statistiques de ventes par brassin.
DB_BIERES <- DB_PRODUITS %>%
  filter(CATEGORIE ==  "BIÈRES" & !is.na(ID_BRASSIN)) %>%
  left_join(PRIX_BIERES, by = "BOISSON") %>%
  right_join(DB_DATE, by = "DATE") %>%
  mutate(VOLUME_TOT_L = QUANTITE*VOLUME_CL/100) |> 
  group_by(CATEGORIE,BOISSON,ID_BRASSIN,BIERE_FINIE,DATE,
           VOLUME_BRASSIN,VOLUME_BRASSIN_AJUST,PRIX_33CL) %>%
  summarise(QUANTITE = sum(QUANTITE),
            CA_HTVA = sum(CA_HTVA),
            CA_TVAC = sum(CA_TVAC),
            VOLUME_JOUR = sum(VOLUME_TOT_L),.groups = "drop") %>%
  arrange(ID_BRASSIN,BOISSON,DATE) %>%
  group_by(BOISSON,ID_BRASSIN) %>%
  mutate(VOLUME_TOT = cumsum(VOLUME_JOUR),
         CA_HTVA_TOT = cumsum(CA_HTVA),
         CA_TVAC_TOT = cumsum(CA_TVAC),
         NB_JOURS_VENTES = sum(CA_HTVA > 0)) %>%
  ungroup() %>%
  mutate(DIFF = VOLUME_BRASSIN_AJUST - VOLUME_TOT,
         PCT = VOLUME_TOT/VOLUME_BRASSIN_AJUST,
         VOLUME_PAR_JOUR = VOLUME_TOT / NB_JOURS_VENTES)

# Dernier jour de vente de chaque bière en cours
vec_brassin_bug <- DB_BIERES %>%
  filter(!BIERE_FINIE) %>%
  arrange(DATE) %>%
  group_by(ID_BRASSIN) %>% filter(row_number() == n()) %>% ungroup() %>%
  filter(today()-DATE > 10) %>% pull(ID_BRASSIN)

DB_BIERES[DB_BIERES$ID_BRASSIN %in% vec_brassin_bug,"BIERE_FINIE"] <- TRUE
DB_BRASSINS[DB_BRASSINS$ID_BRASSIN %in% vec_brassin_bug,"BIERE_FINIE"] <- TRUE

##### DB_COUTS_TRAVAIL ####
cli::cli_h3("Table DB_COUTS_TRAVAIL")

DB_COUTS_TRAVAIL <- DB_HEURES %>% 
  left_join(DB_DATE %>% select(DATE,JOUR_SEMAINE)) %>% 
  mutate(CD_HEURE = ifelse(CD_HEURE == "HEURE_MIDI","Midi (<17h)","Soir (>=17h)")) %>% 
  mutate(CD_HEURE = ifelse(JOUR_SEMAINE == "mardi","Soir (>=17h)", CD_HEURE)) %>% 
  mutate(CD_HEURE = ifelse(JOUR_SEMAINE == "dimanche","Midi (<17h)", CD_HEURE)) %>% 
  ungroup() |> 
  mutate(
    # Correspondance des départements dans R/planning.R (normalise_secteur()).
    SECTEUR = normalise_secteur(DEPARTEMENT),
    CRENEAU = case_when(
      SECTEUR != "Service" ~ "Journée",
      CD_HEURE == "Soir (>=17h)" ~ "Soir",
      CD_HEURE == "Midi (<17h)" ~ "Midi",
      TRUE ~ "Créneau inconnu"
    ),
  ) |> 
  group_by(DATE,SECTEUR,CRENEAU) |> 
  summarise(
    HEURES = sum(NB_HEURES),
    COUT_TRAVAIL = sum(COUT),
    TAUX_HORAIRE = COUT_TRAVAIL / HEURES,
    .groups = "drop"
  )

# Total mensuel des rémunérations selon la comptabilité, rattaché pour la
# CONFRONTATION seulement : l'écart entre comptabilité et Horeko (pécules,
# provisions, personnel non pointé) ne suit pas les heures, on ne s'en sert
# donc pas pour corriger les coûts par secteur.
DB_HEURES_COMPTA <- DB_COMPTA |> 
  filter(TYPE == "compte", CATEGORIE == "REMUNERATION") |> 
  group_by(ANNEE,MOIS) |> 
  summarise(COUT_COMPTA = sum(VALEUR)) |> ungroup()

DB_COUTS_TRAVAIL <- DB_COUTS_TRAVAIL |> 
  mutate(ANNEE = year(DATE), MOIS = month(DATE)) |> 
  left_join(DB_HEURES_COMPTA) |> 
  group_by(ANNEE,MOIS) |> 
  mutate(COUT_HOREKO = sum(COUT_TRAVAIL,na.rm=TRUE)) |> 
  ungroup()

# Recalage sur la paie, si elle est disponible (cf. R/sources_travail.R).
# COUT_HOREKO est calculé avant : c'est le total Horeko d'origine, que le volet
# Sources confronte à la paie.
DB_COUTS_TRAVAIL <- recale_couts_travail(
  DB_COUTS_TRAVAIL, if (exists("DB_ONSS")) DB_ONSS else NULL)

if ("SOURCE_HEURES" %in% names(DB_COUTS_TRAVAIL)) {
  n_paie <- sum(DB_COUTS_TRAVAIL$SOURCE_HEURES == "Paie")
  cli::cli_alert_info(
    "Coût du travail : {n_paie}/{nrow(DB_COUTS_TRAVAIL)} lignes recalées sur la paie")
}

