#### Paramètre de la journée

date_actuelle        <- Sys.Date()
premier_jour_semaine <- date_actuelle - as.POSIXlt(date_actuelle)$wday + 1
jours_semaine        <- seq(from = premier_jour_semaine, by = "days", length.out = 7)
vecteur_jours_LOCAL  <- weekdays(jours_semaine)

#### Authentification Google ####

link_json <- Sys.getenv("LINK_JSON")
path_drive <- Sys.getenv("PATH_DRIVE")

download.file(link_json, destfile = "connect.json")
drive_auth(path = "connect.json")

#### Import de toute la DB actuelle ####

cli::cli_h2("Import de la DB actuelle")

drive_mazette <- drive_download(drive_get(id =Sys.getenv("ID_DRIVE_MAZETTE")),
                                overwrite = TRUE)
path_mazette <- drive_mazette$local_path

IMPORT_BIERES_CORRESPONDANCE <- read_excel(path_mazette,sheet = "CORRESPONDANCE BIERES",range = cell_cols("A:D"))

vec_sheets <- c("DB JOURS","IMPORT OLD DATA","IMPORT BRASSINS",
                "IMPORT LIGHTSPEED","IMPORT TICKET","IMPORT CAISSE",
                "IMPORT OBJECTIFS","IMPORT OBJECTIFS 2025",
                "IMPORT OBJECTIFS 2026","IMPORT PASS")

read_mazette <- function(sheet_name) suppressWarnings(
  read_excel(path_mazette, sheet = sheet_name, .name_repair = "unique_quiet"))

DB_sheets <- sapply(vec_sheets, read_mazette)

IMPORT_DB_JOURS      <- DB_sheets$`DB JOURS`
IMPORT_DB_OLD        <- DB_sheets$`IMPORT OLD DATA`
IMPORT_BRASSINS      <- DB_sheets$`IMPORT BRASSINS`
IMPORT_LIGHTSPEED    <- DB_sheets$`IMPORT LIGHTSPEED`
IMPORT_TICKET        <- DB_sheets$`IMPORT TICKET`
IMPORT_CAISSE        <- DB_sheets$`IMPORT CAISSE`
IMPORT_OBJECTIF_2024 <- DB_sheets$`IMPORT OBJECTIFS`
IMPORT_OBJECTIF_2025 <- DB_sheets$`IMPORT OBJECTIFS 2025`
IMPORT_OBJECTIF_2026 <- DB_sheets$`IMPORT OBJECTIFS 2026`
IMPORT_PASS          <- DB_sheets$`IMPORT PASS`

# Import des heures (issu du rapport pour l'AG)

drive_heures <- drive_download(drive_get(id =Sys.getenv("ID_DRIVE_HEURES")),
                                overwrite = TRUE)
IMPORT_HEURES <- read_excel(drive_heures$local_path,sheet = "Rapport",skip = 1)
IMPORT_COUT <- read_excel(drive_heures$local_path,sheet = "Cout")

# Old Mazette 

# drive_download(drive_get(id=Sys.getenv("ID_MAZETTE_2023")),overwrite = TRUE)
# load("IMPORT 2023-2024.RData")
# 
# IMPORT_DB_JOURS <- rbind(IMPORT_DB_JOURS_OLD,IMPORT_DB_JOURS)
# IMPORT_LIGHTSPEED <- rbind(IMPORT_LIGHTSPEED_OLD,IMPORT_LIGHTSPEED)
# IMPORT_TICKET <- rbind(IMPORT_TICKET_OLD,IMPORT_TICKET)
# IMPORT_CAISSE <- rbind(IMPORT_CAISSE_OLD,IMPORT_CAISSE)
# 
# drive_download(drive_get(id=Sys.getenv("ID_MAZETTE_2025")),overwrite = TRUE)
# load("IMPORT 2025.RData")
# 
# IMPORT_DB_JOURS <- rbind(IMPORT_DB_JOURS_OLD,IMPORT_DB_JOURS)
# IMPORT_LIGHTSPEED <- rbind(IMPORT_LIGHTSPEED_OLD,IMPORT_LIGHTSPEED)
# IMPORT_TICKET <- rbind(IMPORT_TICKET_OLD,IMPORT_TICKET)
# IMPORT_CAISSE <- rbind(IMPORT_CAISSE_OLD,IMPORT_CAISSE)

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

#### Premiers traitements des données sources ####

##### IMPORT DB JOURS ####

cli::cli_h3("Import de la DB JOURS")

DB_JOURS <- IMPORT_DB_JOURS  |>
  filter(!is.na(DATEVALUE)) |> 
  transmute(
    DATE     = ymd(format(DATEVALUE, "%Y-%m-%d")),
    CARTES,
    CASH,
    GIFTCARD,
    VIREMENT,
    CA_HTVA  = `CA HTVA`,
    CA_TVAC  = `CA TVAC`
  )

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
    DT_BRASSIN         = correct_date(DT_BRASSIN),
    DT_CONDI           = correct_date(DT_CONDI),
    DT_FIN             = correct_date(DT_FIN),
    DT_DECLA           = correct_date(DT_DECLA),
    DT_FIN             = if_else(is.na(DT_FIN),DT_DECLA,DT_FIN),
    DT_FIN             = if_else(is.na(DT_FIN),ymd(today()),DT_FIN),
    DT_BRASSIN         = ymd(format(DT_BRASSIN, "%Y-%m-%d")),
    DT_CONDI           = ymd(format(DT_CONDI, "%Y-%m-%d")),
    DT_FIN             = ymd(format(DT_FIN, "%Y-%m-%d")),
    DT_DECLA           = ymd(format(DT_DECLA, "%Y-%m-%d")),
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
    DATE = ymd(format(DATE, "%Y-%m-%d")),
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
    DATE          = ymd(format(DATE, "%Y-%m-%d")),
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

DB_OBJECTIFS <- transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS`) |> 
  add_row(transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS 2025`)) %>%
  add_row(transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS 2026`)) |> 
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

DB_PASSWORD <- IMPORT_PASS |> 
  transmute(
    DATE_DEBUT = ymd(format(Date_debut, "%Y-%m-%d")),
    DATE_FIN = ymd(format(Date_fin, "%Y-%m-%d")),
    PASS = pass)

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
  summarise(NB_HEURES = sum(NB_HEURES))


colnames(IMPORT_COUT) <- c("CONTRAT","COUT_HEURE")

DB_HEURES <- DB_HEURES %>% left_join(IMPORT_COUT) %>% 
  mutate(COUT = COUT_HEURE * NB_HEURES)


#### Création des tables intermédiaires ####

cli::cli_h3(" Création des tables intermédiaires")

##### Nomenclature Produits ######

cli::cli_h3("Nomenclature Produits")

# Correction faite ici : les product sont identifié par leur ID, et le PRODUCT
# n'est qu'un nom sur le ticket. Ne stockons que l'ID, et créons une DB correspondance

NOMEN_PRODUITS <- DB_TICKET |> 
  count(ID_PRODUIT, PRODUIT) |>
  arrange(PRODUIT,-n) |> 
  group_by(ID_PRODUIT) |> filter(row_number() == 1) |> ungroup() |> 
  select(ID_PRODUIT,PRODUIT) |> 
  mutate(PRODUIT = case_when(
    PRODUIT == "Cola maison" ~ "Cola maison 33cL",
    PRODUIT == "Dik effiloché de porc crémeux de carottes 1/2" ~ "Dik effiloché de porc crémeux de carottes",
    PRODUIT == "Sombre Despote" ~ "Sombre Despote 33cL",
    TRUE ~ PRODUIT)) %>%
  from_product_to_boisson()

# Pour récupérer le taux de tva

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

NOMEN_PRODUITS <- NOMEN_PRODUITS |> 
  left_join(PRODUITS_REF, by = "PRODUIT_FULL") |> 
  filter(!is.na(TAUX_TVA))

#### Création des tables finales ####
cli::cli_h2("Création des tables finales")

##### DB_JOURS ####
cli::cli_h3("Table DB_JOURS")

# Ajout simplement la DB OLD

DB_JOURS <- DB_JOURS %>%
  add_row(DB_OLD) %>%
  arrange(DATE)

##### DB_DATE ####
cli::cli_h3("Table DB_DATE")

# Faire une DB date pour avoir toutes les dates, de min DB_JOURS à today()

DB_DATE <- creer_db_date(min(DB_JOURS$DATE))

##### DB_TICKETS_HEURES ####
cli::cli_h3("Table DB_TICKETS_HEURES")

# D'abord, pour chaque produit et chaque date d'ouverture de Mazette, le CA et le nombre vendu

# On construit la forme réduite (celle qui sera stockée), puis on l'hydrate.
# Passer par hydrate_donnees() garantit que la reconstruction complète est
# définie à un seul endroit — cf. R/donnees.R — donc identique après un import
# et après un simple chargement du .RData.
normalise <- normalise_tickets(
  DB_TICKET |> 
    rename(PRODUIT_FULL = PRODUIT) |> 
    left_join(NOMEN_PRODUITS, by = c("ID_PRODUIT","PRODUIT_FULL")))
DB_TICKET    <- normalise$DB_TICKET
REF_PRODUITS <- normalise$REF_PRODUITS
rm(normalise)

# DB_TICKET reste sous sa forme réduite — c'est elle qu'on sauvegarde. Seul
# DB_TICKETS_HEURES est nécessaire à la suite de ce fichier ; le DB_TICKET complet
# sera reconstruit au chargement par hydrate_dans().
DB_TICKETS_HEURES <- hydrate_donnees(DB_TICKET, REF_PRODUITS)$TICKETS_HEURES

# Synthèse par catégorie
DB_CATEGORIES_JOURS <- DB_TICKETS_HEURES %>%
  group_by(DATE,CD_HEURE,CATEGORIE) %>%
  summarise(CA_HTVA = sum(CA_HTVA),QUANTITE = sum(QUANTITE),.groups = "drop") |> 
  complete(DATE, CD_HEURE, CATEGORIE,
           fill = list(CA_HTVA = 0, QUANTITE = 0))

##### DB_KPI ####
cli::cli_h3("Table DB_KPI")

DB_KPI <- DB_TICKETS_HEURES %>%
  filter(!is.na(CD_SECTEUR)) %>%
  group_by(DATE,CD_PERIODE_JOUR,CD_PERIODE_SEMAINE,CD_SECTEUR) %>%
  summarise(CA_HTVA = sum(CA_HTVA),CA_TVAC = sum(CA_TVAC),.groups = "drop") %>%
  filter(!is.na(DATE))

#### Attention, ici problème, le CA_HTVA n'est pas identique à DB_JOURS
# C'est parce qu'on a les factures en plus dans le CA de DB_JOURS

# Normalement, c'est bon, car la DB_KPI_SIMPLE repart de DB_JOURS

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

# Correction ici en règle de trois, afin que chaque paire corresponde au CA de DB_JOURS

DB_KPI_SIMPLE <- DB_KPI_SIMPLE %>%
  mutate(ratio = CA_HTVA/(Boisson + Nourriture),
         Boisson = Boisson*ratio,Nourriture = Nourriture*ratio) %>%
  mutate(ratio = CA_HTVA/(`Week-end` + Semaine),
         `Week-end` = `Week-end`*ratio,Semaine = Semaine*ratio) %>%
  mutate(ratio = CA_TVAC/(Jour + Soir),
         Jour = Jour*ratio,Soir = Soir*ratio)

##### DB_OBJECTIFS #####
cli::cli_h3("Table DB_OBJECTIFS")

# Transformer la DB objectifs en journalier

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

# Une ligne par date*brassin, afin de déterminer quel brassin pour quel jour
# S'il y a des doublons, je prends le nouveau brassin
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

# ADD BRASSINS
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

# Ajout des stats sur les ventes
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
         # NB_JOURS_VENTES = n()) %>%
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
    SECTEUR = case_when(
      DEPARTEMENT == "Transfo alimentaire" ~ "Transformation alimentaire",
      DEPARTEMENT == "Fabrik de boissons" ~ "Brasserie",
      DEPARTEMENT == "Support" ~ "Support",
      DEPARTEMENT == "Service" ~ "Service",
      TRUE ~ "Secteur inconnu"
    ),
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
