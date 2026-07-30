library(googledrive)
library(readxl)
library(tidyverse)

#### Création des DB SQlite #### 

##### JOURS #####

sql_req("
CREATE TABLE IF NOT EXISTS jours (
  date     TEXT    NOT NULL,             -- ISO 'AAAA-MM-JJ'
  cartes   REAL    NOT NULL DEFAULT 0,
  cash     REAL    NOT NULL DEFAULT 0,
  giftcard REAL    NOT NULL DEFAULT 0,
  virement REAL    NOT NULL DEFAULT 0,
  ca_htva  REAL    NOT NULL DEFAULT 0,
  ca_tvac  REAL    NOT NULL DEFAULT 0,
  PRIMARY KEY (date)
);
")

##### PRODUITS #####

sql_req("
CREATE TABLE IF NOT EXISTS produits (
  date         TEXT    NOT NULL,
  categorie    TEXT    NOT NULL DEFAULT '',
  produit_full TEXT    NOT NULL DEFAULT '',
  produit      TEXT    NOT NULL DEFAULT '',
  boisson      TEXT    DEFAULT '',
  volume_cl    INTEGER DEFAULT 0,
  prix      REAL    NOT NULL DEFAULT 0,
  quantite  REAL    NOT NULL DEFAULT 0,
  ca_tvac   REAL    NOT NULL DEFAULT 0,
  ca_htva   REAL    NOT NULL DEFAULT 0,
  tva_rate  REAL    NOT NULL DEFAULT 0,
  PRIMARY KEY (date,categorie,produit_full)
);

CREATE INDEX IF NOT EXISTS idx_produits ON produits(date);")

##### BRASSIN #####

sql_req("
CREATE TABLE IF NOT EXISTS brassins (
  id_brassin         TEXT    NOT NULL,
  nom_brassin        TEXT    NOT NULL DEFAULT '',
  date_brassin       TEXT    ,
  date_condi         TEXT    ,
  volume_brassin     REAL    NOT NULL DEFAULT 0,
  date_fin           TEXT    ,
  date_declaration   TEXT    ,
  biere_finie        INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (id_brassin)
);")


##### CAISSE #####

sql_req("
CREATE TABLE IF NOT EXISTS caisse (
  date     TEXT    NOT NULL,             -- ISO 'AAAA-MM-JJ'
  method   TEXT    NOT NULL,             -- CASH, CARTES, CH...
  detail   TEXT    NOT NULL DEFAULT '',  -- AMEX, BANCONTACT...
  category TEXT,                         -- Cash, Chèque repas... (NULL si carte)
  n        INTEGER NOT NULL DEFAULT 0,
  montant  REAL    NOT NULL DEFAULT 0,
  tips     REAL    NOT NULL DEFAULT 0,
  total    REAL    NOT NULL DEFAULT 0,
  PRIMARY KEY (date, method, detail)
);

CREATE INDEX IF NOT EXISTS idx_caisse_date ON caisse(date);
")

##### TICKETS #####

sql_req("
CREATE TABLE IF NOT EXISTS tickets (
  date            TEXT    NOT NULL,
  timestamp       TEXT    NOT NULL DEFAULT '',
  timestamp_mod   TEXT    NOT NULL DEFAULT '',
  id_ticket       TEXT    NOT NULL DEFAULT '',
  id_produit      REAL    NOT NULL DEFAULT 0,
  nb_clients      REAL    NOT NULL DEFAULT 0,
  prix_unite      REAL    NOT NULL DEFAULT 0,
  quantite        REAL    NOT NULL DEFAULT 0,
  prix_total      REAL    NOT NULL DEFAULT 0,
  midi            INTEGER DEFAULT 0,
  week_end        INTEGER DEFAULT 0,
  PRIMARY KEY (date,id_ticket,id_produit)
);
CREATE INDEX IF NOT EXISTS idx_tickets ON tickets(date);")


##### NOMEN_PRODUITS #####

sql_req("
CREATE TABLE IF NOT EXISTS nomen_produits (
  id_produit     REAL    NOT NULL,
  produit        TEXT    NOT NULL,
  produit_full   TEXT    NOT NULL,
  volume_cl      INTEGER DEFAULT 0,
  boisson        TEXT    DEFAULT '',
  taux_tva       REAL    DEFAULT 0,
  categorie      TEXT    DEFAULT '',
  PRIMARY KEY (id_produit)
);")

##### OBJECTIFS #####

sql_req("
CREATE TABLE IF NOT EXISTS objectifs (
  annee      INTEGER NOT NULL,
  mois       INTEGER NOT NULL,
  date_debut TEXT    NOT NULL DEFAULT '',
  date_fin   TEXT    NOT NULL DEFAULT '',
  ca_tvac    REAL    NOT NULL DEFAULT 0,
  ca_htva    REAL    NOT NULL DEFAULT 0,
  ca_htva_6  REAL    NOT NULL DEFAULT 0,
  ca_htva_12 REAL    NOT NULL DEFAULT 0,
  ca_htva_21 REAL    NOT NULL DEFAULT 0,
  PRIMARY KEY (annee,mois)
);

CREATE INDEX IF NOT EXISTS idx_objectifs ON objectifs(annee);")

##### MOT_DE_PASSE #####

sql_req("
CREATE TABLE IF NOT EXISTS password (
  date_debut   TEXT    NOT NULL DEFAULT '',
  date_fin     TEXT    NOT NULL DEFAULT '',
  pass         TEXT    NOT NULL DEFAULT ''
);")


##### NOMEN_BIERES #####

sql_req("
CREATE TABLE IF NOT EXISTS nomen_bieres (
  nom_brassin    TEXT    NOT NULL,
  nom_biere      TEXT    DEFAULT '',
  nom_logo       TEXT    DEFAULT '',
  PRIMARY KEY (nom_brassin)
);")

#### Authentification Google ####

link_json <- Sys.getenv("LINK_JSON")
path_drive <- Sys.getenv("PATH_DRIVE")

download.file(link_json, destfile = "connect.json")
drive_auth(path = "connect.json")

#### Import de toute la DB actuelle ####

drive_mazette <- drive_download(drive_get(id =Sys.getenv("ID_DRIVE_MAZETTE")),
                                overwrite = TRUE)
path_mazette <- drive_mazette$local_path

IMPORT_BIERES_CORRESPONDANCE <- read_excel(path_mazette,sheet = "CORRESPONDANCE BIERES",range = cell_cols("A:D"))

vec_sheets <- c("DB JOURS","IMPORT BRASSINS",
                "IMPORT LIGHTSPEED","IMPORT TICKET","IMPORT CAISSE",
                "IMPORT OBJECTIFS","IMPORT OBJECTIFS 2025",
                "IMPORT OBJECTIFS 2026","IMPORT PASS")

read_mazette <- function(sheet_name) suppressWarnings(
  read_excel(path_mazette, sheet = sheet_name, .name_repair = "unique_quiet"))

DB_sheets <- sapply(vec_sheets, read_mazette)

IMPORT_DB_JOURS      <- DB_sheets$`DB JOURS`
IMPORT_BRASSINS      <- DB_sheets$`IMPORT BRASSINS`
IMPORT_LIGHTSPEED    <- DB_sheets$`IMPORT LIGHTSPEED`
IMPORT_TICKET        <- DB_sheets$`IMPORT TICKET`
IMPORT_CAISSE        <- DB_sheets$`IMPORT CAISSE`
IMPORT_OBJECTIF_2024 <- DB_sheets$`IMPORT OBJECTIFS`
IMPORT_OBJECTIF_2025 <- DB_sheets$`IMPORT OBJECTIFS 2025`
IMPORT_OBJECTIF_2026 <- DB_sheets$`IMPORT OBJECTIFS 2026`
IMPORT_PASS          <- DB_sheets$`IMPORT PASS`

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

#### Traitement et transfert des DB ####

##### IMPORT CAISSE ####

cli::cli_h3("Import de la DB CAISSE")

caisse <- IMPORT_CAISSE %>%
  transmute(
    DATE     = format(DATE, "%Y-%m-%d"),
    method   = METHOD,
    detail   = replace_na(DETAIL, ""),
    category = CATEGORY,
    n        = as.integer(N),
    montant  = MONTANT, tips = TIPS, total = TOTAL
  )

# Pour éviter de charger tous les tickets
caisse_date_actu <- pull(sql_select("SELECT DISTINCT date FROM caisse"))
caisse <- caisse |> filter(!DATE %in% caisse_date_actu)

if (nrow(caisse) > 0){
  paquets <- df_to_paquets(caisse)
  vec_sql <- vapply(paquets, function(p) paste0(
    "INSERT OR REPLACE INTO caisse ",
    "(date, method, detail, category, n, montant, tips, total) VALUES\n",
    paste(p, collapse = ",\n"), ";"), character(1))
  
  map(vec_sql, sql_req)
}else{
  cli::cli_alert_success("caisse déjà à jour")
}

##### IMPORT DB JOURS ####

cli::cli_h3("Import de la DB JOURS")

jours <- IMPORT_DB_JOURS  |>
  filter(!is.na(DATEVALUE)) |> 
  transmute(
    DATE     = format(DATEVALUE, "%Y-%m-%d"),
    cartes   = CARTES,
    cash     = CASH,
    giftcard = GIFTCARD,
    virement = VIREMENT,
    ca_htva  = `CA HTVA`,
    ca_tvac  = `CA TVAC`
  )

# Pour éviter de charger tous les tickets
jours_date_actu <- pull(sql_select("SELECT DISTINCT date FROM jours"))
jours <- jours |> filter(!DATE %in% jours_date_actu)

if (nrow(jours) > 0){
  paquets <- df_to_paquets(jours)
  
  vec_sql <- vapply(paquets, function(p) paste0(
    "INSERT OR REPLACE INTO jours ",
    "(date, cartes, cash, giftcard, virement, ca_htva, ca_tvac) VALUES\n",
    paste(p, collapse = ",\n"), ";"), character(1))
  
  r <- map(vec_sql, sql_req)
}else{
  cli::cli_alert_success("jours déjà à jour")
}

##### IMPORT IMPORT BRASSINS ####

cli::cli_h3("Import de la DB BRASSINS")

brassins <- IMPORT_BRASSINS  |>
  filter(!is.na(`Numéro de brassin`) & !is.na(`Nom commercial`)) %>%
  select(`Numéro de brassin`,`Nom commercial`,`Date de brassage`,
         `Date de conditionnement`, `Volume ST (hL)`,`Volume Fûts (hL)`,
         `Date de fin de bière`,`Date de déclaration`,`Bière finie?`) %>%
  rename(
    ID_BRASSIN = `Numéro de brassin`,
    NOM_BRASSIN = `Nom commercial`,
    DT_BRASSIN = `Date de brassage`,
    DT_CONDI = `Date de conditionnement`,
    VOLUME_BRASSIN = `Volume ST (hL)`,
    VOLUME_BRASSIN_ADD = `Volume Fûts (hL)`,
    DT_FIN = `Date de fin de bière`,
    DT_DECLA = `Date de déclaration`,
    FL_FINI = `Bière finie?`
  ) |> 
  mutate(
    ID_BRASSIN = gsub("\\.0","",ID_BRASSIN),
    FL_FINI    = as.numeric(FL_FINI),
    DT_BRASSIN = correct_date(DT_BRASSIN),
    DT_CONDI = correct_date(DT_CONDI),
    DT_FIN = correct_date(DT_FIN),
    DT_DECLA = correct_date(DT_DECLA),
    DT_FIN = if_else(is.na(DT_FIN),DT_DECLA,DT_FIN),
    DT_FIN = if_else(is.na(DT_FIN),ymd(today()),DT_FIN),
    DT_BRASSIN = format(DT_BRASSIN, "%Y-%m-%d"),
    DT_CONDI   = format(DT_CONDI, "%Y-%m-%d"),
    DT_FIN     = format(DT_FIN, "%Y-%m-%d"),
    DT_DECLA   = format(DT_DECLA, "%Y-%m-%d"),
    VOLUME_BRASSIN = VOLUME_BRASSIN * 100,
    VOLUME_BRASSIN_ADD = VOLUME_BRASSIN_ADD * 100,
    VOLUME_BRASSIN = VOLUME_BRASSIN + VOLUME_BRASSIN_ADD
  ) |> 
  select(-VOLUME_BRASSIN_ADD)

paquets <- df_to_paquets(brassins)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO brassins ",
  "(id_brassin, nom_brassin, date_brassin, date_condi, volume_brassin, 
  date_fin, date_declaration, biere_finie) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)

##### IMPORT PRODUITS ####

cli::cli_h3("Import de la DB PRODUITS")

produits <- IMPORT_LIGHTSPEED  |>
  filter(!is.na(PRODUCT)) |> 
  transmute(
    DATE = format(DATE, "%Y-%m-%d"),
    CATEGORY = toupper(trimws(CATEGORY)),
    PRODUCT,
    PRICE,
    QUANTITE = `#`,
    CA_TVAC = TOTAL,
    CA_HTVA = PROFIT,
    TVA_RATE = `TVA RATE`
  ) %>%
  mutate(TVA_RATE = as.numeric(gsub("%", "", gsub(",",".",TVA_RATE)))/ 100,
         TVA_RATE = ifelse(TVA_RATE < 0.01,TVA_RATE*100,TVA_RATE),
         TVA_RATE = replace_na(TVA_RATE,0)) |> 
  mutate(
    QUANTITE = case_when(
      substr(PRODUCT,1,4) == "Dikk" & PRICE < 6 ~ QUANTITE/4,
      substr(PRODUCT,1,4) == "Dikk" & PRICE < 10 ~ QUANTITE/3,
      TRUE ~ QUANTITE)
  ) %>%
  from_product_to_boisson() |> 
  select(DATE,CATEGORY,PRODUCT_FULL,PRODUCT,BOISSON,VOLUME_CL,
         PRICE,QUANTITE,CA_TVAC,CA_HTVA,TVA_RATE)

# Check qualité
stopifnot(unique(produits$TVA_RATE) %in% c(0,0.06,0.12,0.21))

# Pour éviter de charger tous les tickets
produits_date_actu <- pull(sql_select("SELECT DISTINCT date FROM produits"))
new_produits <- produits |> filter(!DATE %in% produits_date_actu)

if (nrow(new_produits) > 0){
  paquets <- df_to_paquets(new_produits)
  
  vec_sql <- vapply(paquets, function(p) paste0(
    "INSERT OR REPLACE INTO produits ",
    "(date, categorie, produit_full, produit, boisson, volume_cl, prix, 
  quantite, ca_tvac, ca_htva, tva_rate) VALUES\n",
    paste(p, collapse = ",\n"), ";"), character(1))
  
  r <- map(vec_sql, sql_req)  
}else{
  cli::cli_alert_success("produits déjà à jour")
}

##### IMPORT IMPORT TICKET ####

cli::cli_h3("Import de la DB TICKETS")

tickets <- IMPORT_TICKET %>%
  mutate(
    TIMESTAMP = str_remove(TIMESTAMP," \\+[0-9]{4}"),
    TIMESTAMP = dmy_hm(TIMESTAMP),
    MIDI         = as.numeric(hour(TIMESTAMP) %in% c(8:16)),
    WEEK_END     = as.numeric(wday(TIMESTAMP, week_start = 1) %in% c(6,7)
                              | (wday(TIMESTAMP,week_start = 1) == 5 & !MIDI))
  ) |> 
  transmute(
    DATE          = format(DATE, "%Y-%m-%d"),
    TIMESTAMP     = as.character(TIMESTAMP),
    TIMESTAMP     = ifelse(nchar(TIMESTAMP) == 19,TIMESTAMP,
                           paste(substr(TIMESTAMP,1,10),"00:00:00")),
    ID_TICKET     = as.numeric(ID_TICKET), 
    ID_PRODUCT    = as.numeric(ID),
    PRODUCT,
    NB_CLIENTS    = as.numeric(NB_CUSTOMERS),
    PRIX_UNITE,
    QUANTITE,
    PRIX_TOTAL,
    MIDI,
    WEEK_END
  )

# Pour éviter de charger tous les tickets
tickets_date_actu <- pull(sql_select("SELECT DISTINCT date FROM tickets"))
new_tickets <- tickets |> filter(!DATE %in% tickets_date_actu)

if (nrow(new_tickets) > 0){
  # Attention, ici, pour économiser, on retire le nom du produit
  paquets <- df_to_paquets(new_tickets |> select(-PRODUCT))
  
  vec_sql <- vapply(paquets, function(p) paste0(
    "INSERT OR REPLACE INTO tickets ",
    "(date, timestamp, id_ticket, id_produit, 
    nb_clients, prix_unite, quantite, prix_total, midi,
    week_end) VALUES\n",
    paste(p, collapse = ",\n"), ";"), character(1))
  
  r <- map(vec_sql, sql_req)
}else{
  cli::cli_alert_success("tickets déjà à jour")
}

##### Nomenclature Produits ######

cli::cli_h3("Import de la DB NOMENCLATURE PRODUITS")

# Correction faite ici : les product sont identifié par leur ID, et le PRODUCT
# n'est qu'un nom sur le ticket. Ne stockons que l'ID, et créons une DB correspondance

nomen_produits <- tickets |> 
  count(ID_PRODUCT, PRODUCT) |>
  arrange(PRODUCT,-n) |> 
  group_by(ID_PRODUCT) |> filter(row_number() == 1) |> ungroup() |> 
  select(ID_PRODUCT,PRODUCT) |> 
  mutate(PRODUCT = case_when(
    PRODUCT == "Cola maison" ~ "Cola maison 33cL",
    PRODUCT == "Dik effiloché de porc crémeux de carottes 1/2" ~ "Dik effiloché de porc crémeux de carottes",
    PRODUCT == "Sombre Despote" ~ "Sombre Despote 33cL",
    TRUE ~ PRODUCT)) %>%
  from_product_to_boisson()

# Pour récupérer le taux de tva

produits_ref <- produits %>%
  filter(PRICE > 0) %>%
  mutate(TVA_RATE = if_else(
    PRODUCT_FULL == "Pain à emporter",0.06,TVA_RATE)) %>%
  select(PRODUCT_FULL,TVA_RATE,CATEGORY) %>%
  distinct() %>%
  group_by(PRODUCT_FULL) %>% arrange(-TVA_RATE) %>%
  filter(row_number() == 1) %>% ungroup()

vec_sale <- produits_ref %>% filter(CATEGORY == "SALÉ") %>%
  pull(PRODUCT_FULL) %>% unique()

vec_sucre <- produits_ref %>% filter(CATEGORY == "SUCRÉ") %>%
  pull(PRODUCT_FULL) %>% unique()

produits_ref[produits_ref$PRODUCT_FULL %in% vec_sale,"CATEGORY"] <- "SALÉ"
produits_ref[produits_ref$PRODUCT_FULL %in% vec_sucre,"CATEGORY"] <- "SUCRÉ"

nomen_produits <- nomen_produits |> 
  left_join(produits_ref) |> 
  filter(!is.na(TVA_RATE))

paquets <- df_to_paquets(nomen_produits)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO nomen_produits ",
  "(id_produit, produit, produit_full, volume_cl, boisson, 
  taux_tva, categorie) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)

##### IMPORT IMPORT OBJECTIFS ####

cli::cli_h3("Import de la DB OBJECTIFS")

objectifs <- transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS`) |> 
  add_row(transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS 2025`)) %>%
  add_row(transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS 2026`)) |> 
  select(ANNEE,MOIS,DATE_DEBUT,DATE_FIN,CA_TVAC,CA_HTVA,
         CA_HTVA_NOURRITURE_6,CA_HTVA_NOURRITURE_12,CA_HTVA_BOISSON_21) |> 
  mutate(DATE_DEBUT = as.character(DATE_DEBUT),
         DATE_FIN = as.character(DATE_FIN))

paquets <- df_to_paquets(objectifs)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO objectifs ",
  "(annee, mois, date_debut, date_fin, ca_tvac, ca_htva,
  ca_htva_6, ca_htva_12, ca_htva_21) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)

##### IMPORT IMPORT BIERES CORRESPONDANCE ####

cli::cli_h3("Import de la DB NOMENCLATURE BIERE")

nomen_bieres <- IMPORT_BIERES_CORRESPONDANCE %>%
  filter(!is.na(`Nom commercial`)) |> 
  transmute(
    BRASSIN = `Nom commercial`,
    BOISSON ,
    LOGO    = `NAME LOGO`
    )

paquets <- df_to_paquets(nomen_bieres)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO nomen_bieres ",
  "(nom_brassin, nom_biere, nom_logo) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)


##### IMPORT_PASS #####

cli::cli_h3("Import de la DB PASSWORD")

password <- IMPORT_PASS |> 
  mutate(Date_debut = format(Date_debut, "%Y-%m-%d"),
         Date_fin = format(Date_fin, "%Y-%m-%d"))

paquets <- df_to_paquets(password)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO password ",
  "(date_debut, date_fin, pass) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)

#### Tables intermédiaires #####

##### DB_PRODUITS_JOURS

view_sql <- "
CREATE VIEW IF NOT EXISTS tickets_heures AS
SELECT 
    DATE,
    midi,
    week_end,
    id_produit,
    SUM(prix_total) AS ca_tvac,
    SUM(quantite) AS quantite
FROM tickets
WHERE PRIX_TOTAL > 0
GROUP BY 
    date,
    midi,
    week_end,
    id_produit;
"

r <- sql_req(view_sql)

##### DB_BRASSINS

view_sql <- "
CREATE VIEW IF NOT EXISTS db_brassins AS SELECT * from import_brassins b
left join import_bieres_correspondance c on b.nom_brassin = c.nom_brassin;
"

r <- sql_req(view_sql)

cli::cli_progress_done()

