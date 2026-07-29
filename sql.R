library(googledrive)
library(googlesheets4)
library(readxl)
library(tidyverse)

#### Fonctions pour sql ####

sql_val <- function(x) {
  if (is.character(x)) ifelse(is.na(x), "NULL",
                              paste0("'", gsub("'", "''", x), "'"))
  else ifelse(is.na(x), "NULL", as.character(x))
}

sql_req <- function(sql, 
                    account_id = Sys.getenv("CF_ACCOUNT_ID"),
                    db_id = Sys.getenv("CF_D1_ID"),
                    api_token = Sys.getenv("CF_API_TOKEN")) {
  r <- httr::POST(
    sprintf("https://api.cloudflare.com/client/v4/accounts/%s/d1/database/%s/query",
            account_id, db_id),
    httr::add_headers(Authorization = paste("Bearer", api_token)),
    body = list(sql = sql), encode = "json")
  
  res <- httr::content(r)
  if (!isTRUE(res$success))
    stop("D1 a refusé la requête : ",
         paste(vapply(res$errors, `[[`, "", "message"), collapse = " | "))
  invisible(res)
}

df_to_paquets <- function(db, taille = 200){
  lignes <- paste0("(", db %>% 
                     mutate(across(everything(), sql_val)) %>%
                     apply(1, paste, collapse = ", "), ")")
  
  # On insère par paquets : un INSERT géant dépasserait les limites de D1.
  split(lignes, ceiling(seq_along(lignes) / 200))
}

correct_date <- function(x){
  if (class(x)[1] == "character") 
    janitor::excel_numeric_to_date(as.numeric(x))
  else
    x
}

#### Authentification Google ####

link_json <- Sys.getenv("LINK_JSON")
path_drive <- Sys.getenv("PATH_DRIVE")

download.file(link_json, destfile = "connect.json")
drive_auth(path = "connect.json")
gs4_auth(path = "connect.json")

#### Import de toutes la DB actuelle ####

drive_mazette <- drive_download(drive_get(id =Sys.getenv("ID_DRIVE_MAZETTE")),
                                overwrite = TRUE)
path_mazette <- drive_mazette$local_path

IMPORT_BIERES_CORRESPONDANCE <- read_excel(path_mazette,sheet = "CORRESPONDANCE BIERES",range = cell_cols("A:D"))

vec_sheets <- c("DB JOURS","IMPORT BRASSINS",
                "IMPORT LIGHTSPEED","IMPORT TICKET","IMPORT CAISSE",
                "IMPORT OBJECTIFS","IMPORT OBJECTIFS 2025",
                "IMPORT OBJECTIFS 2026","IMPORT PASS")

read_mazette <- function(sheet_name) read_excel(path_mazette,sheet = sheet_name)

DB_sheets <- sapply(vec_sheets, read_mazette)

IMPORT_DB_JOURS <- DB_sheets$`DB JOURS`
IMPORT_BRASSINS <- DB_sheets$`IMPORT BRASSINS`
IMPORT_LIGHTSPEED <- DB_sheets$`IMPORT LIGHTSPEED`
IMPORT_TICKET <- DB_sheets$`IMPORT TICKET`
IMPORT_CAISSE <- DB_sheets$`IMPORT CAISSE`
IMPORT_OBJECTIF_2024 <- DB_sheets$`IMPORT OBJECTIFS`
IMPORT_OBJECTIF_2025 <- DB_sheets$`IMPORT OBJECTIFS 2025`
IMPORT_OBJECTIF_2026 <- DB_sheets$`IMPORT OBJECTIFS 2026`
IMPORT_PASS <- DB_sheets$`IMPORT PASS`

# Old Mazette 

drive_download(drive_get(id=Sys.getenv("ID_MAZETTE_2023")),overwrite = TRUE)
load("IMPORT 2023-2024.RData")

IMPORT_DB_JOURS <- rbind(IMPORT_DB_JOURS_OLD,IMPORT_DB_JOURS)
IMPORT_LIGHTSPEED <- rbind(IMPORT_LIGHTSPEED_OLD,IMPORT_LIGHTSPEED)
IMPORT_TICKET <- rbind(IMPORT_TICKET_OLD,IMPORT_TICKET)
IMPORT_CAISSE <- rbind(IMPORT_CAISSE_OLD,IMPORT_CAISSE)

drive_download(drive_get(id=Sys.getenv("ID_MAZETTE_2025")),overwrite = TRUE)
load("IMPORT 2025.RData")

IMPORT_DB_JOURS <- rbind(IMPORT_DB_JOURS_OLD,IMPORT_DB_JOURS)
IMPORT_LIGHTSPEED <- rbind(IMPORT_LIGHTSPEED_OLD,IMPORT_LIGHTSPEED)
IMPORT_TICKET <- rbind(IMPORT_TICKET_OLD,IMPORT_TICKET)
IMPORT_CAISSE <- rbind(IMPORT_CAISSE_OLD,IMPORT_CAISSE)

#### Table par table ####

##### IMPORT CAISSE ####

# Création de la DB en sql

create_sql <- "
CREATE TABLE IF NOT EXISTS import_caisse (
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

CREATE INDEX IF NOT EXISTS idx_caisse_date ON import_caisse(date);
"

r <- sql_req(create_sql)

# Préparation de la DB pour import en sql

caisse <- IMPORT_CAISSE %>%
  transmute(
    date     = format(DATE, "%Y-%m-%d"),
    method   = METHOD,
    detail   = replace_na(DETAIL, ""),
    category = CATEGORY,
    n        = as.integer(N),
    montant  = MONTANT, tips = TIPS, total = TOTAL
  )

paquets <- df_to_paquets(caisse)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO import_caisse ",
  "(date, method, detail, category, n, montant, tips, total) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

map(vec_sql, sql_req)

##### IMPORT DB JOURS ####

# Création de la DB en sql

create_sql <- "
CREATE TABLE IF NOT EXISTS db_jours (
  date     TEXT    NOT NULL,             -- ISO 'AAAA-MM-JJ'
  cartes   REAL    NOT NULL DEFAULT 0,
  cash     REAL    NOT NULL DEFAULT 0,
  giftcard REAL    NOT NULL DEFAULT 0,
  virement REAL    NOT NULL DEFAULT 0,
  ca_htva  REAL    NOT NULL DEFAULT 0,
  ca_tvac  REAL    NOT NULL DEFAULT 0,
  PRIMARY KEY (date)
);

CREATE INDEX IF NOT EXISTS idx_db_jours ON db_jours(date);
"

r <- sql_req(create_sql)

# Préparation de la DB pour import en sql

db_jours <- IMPORT_DB_JOURS  |>
  filter(!is.na(DATEVALUE)) |> 
  transmute(
    date     = format(DATEVALUE, "%Y-%m-%d"),
    cartes   = CARTES,
    cash     = CASH,
    giftcard = GIFTCARD,
    virement = VIREMENT,
    ca_htva  = `CA HTVA`,
    ca_tvac  = `CA TVAC`
  )

paquets <- df_to_paquets(db_jours)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO db_jours ",
  "(date, cartes, cash, giftcard, virement, ca_htva, ca_tvac) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)


##### IMPORT IMPORT BRASSINS ####

# Création de la DB en sql

create_sql <- "
CREATE TABLE IF NOT EXISTS import_brassins (
  id_brassin         TEXT    NOT NULL,
  nom_brassin        TEXT    NOT NULL DEFAULT '',
  date_brassin       TEXT    ,
  date_condi         TEXT    ,
  volume_brassin     REAL    NOT NULL DEFAULT 0,
  date_fin           TEXT    ,
  date_declaration   TEXT    ,
  biere_finie        INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (id_brassin)
);

CREATE INDEX IF NOT EXISTS idx_import_brassins ON import_brassins(id_brassin);
"

r <- sql_req(create_sql)

# Préparation de la DB pour import en sql

import_brassins <- IMPORT_BRASSINS  |>
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

paquets <- df_to_paquets(import_brassins)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO import_brassins ",
  "(id_brassin, nom_brassin, date_brassin, date_condi, volume_brassin, 
  date_fin, date_declaration, biere_finie) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)


##### IMPORT IMPORT TICKET ####

# Création de la DB en sql

create_sql <- "
CREATE TABLE IF NOT EXISTS import_ticket (
  date           TEXT    NOT NULL,
  timestamp      TEXT    NOT NULL DEFAULT '',
  timestamp_mod  TEXT    NOT NULL DEFAULT '',
  id_ticket      TEXT    NOT NULL DEFAULT '',
  id_produit     REAL    NOT NULL DEFAULT 0,
  nb_clients     REAL    NOT NULL DEFAULT 0,
  prix_unite     REAL    NOT NULL DEFAULT 0,
  quantite       REAL    NOT NULL DEFAULT 0,
  prix_total     REAL    NOT NULL DEFAULT 0,
  PRIMARY KEY (date,id_ticket,id_produit)
);

CREATE INDEX IF NOT EXISTS idx_import_ticket ON import_ticket(date);
"

r <- sql_req(create_sql)

# Préparation de la DB pour import en sql

import_ticket <- IMPORT_TICKET %>%
  transmute(
    DATE          = format(DATE, "%Y-%m-%d"),
    TIMESTAMP     = as.character(dmy_hm(str_remove(TIMESTAMP," \\+[0-9]{4}"))),
    TIMESTAMP_MOD = as.character(dmy_hm(str_remove(TIMESTAMP_MOD," \\+[0-9]{4}"))),
    ID_TICKET     = as.numeric(ID_TICKET), 
    ID_PRODUIT    = as.numeric(ID),
    PRODUIT       = PRODUCT,
    NB_CLIENTS    = as.numeric(NB_CUSTOMERS),
    PRIX_UNITE,
    QUANTITE,
    PRIX_TOTAL
  )

# Correction faite ici : les product sont identifié par leur ID, et le PRODUCT
# n'est qu'un nom sur le ticket. Ne stockons que l'ID, et créons une DB correspondance

corresp_produit <- import_ticket |> 
  count(ID_PRODUIT, PRODUIT) |>
  arrange(PRODUIT,-n) |> 
  group_by(ID_PRODUIT) |> filter(row_number() == 1) |> ungroup() |> 
  select(ID_PRODUIT,PRODUIT)

create_sql <- "
CREATE TABLE IF NOT EXISTS corresp_produit (
  id_produit     REAL    NOT NULL,
  produit        TEXT    NOT NULL,
  PRIMARY KEY (id_produit)
);
"

r <- sql_req(create_sql)

paquets <- df_to_paquets(corresp_produit)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO corresp_produit ",
  "(id_produit, produit) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)


  
import_ticket <- import_ticket |> select(-PRODUIT)

paquets <- df_to_paquets(import_ticket)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO import_ticket ",
  "(date, timestamp, timestamp_mod, id_ticket, id_produit, 
  nb_clients, prix_unite, quantite, prix_total) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)

##### IMPORT IMPORT LIGHTSPEED ####

# Création de la DB en sql

create_sql <- "
CREATE TABLE IF NOT EXISTS import_lightspeed (
  date      TEXT    NOT NULL,
  categorie TEXT    NOT NULL DEFAULT '',
  produit   TEXT    NOT NULL DEFAULT '',
  prix      REAL    NOT NULL DEFAULT 0,
  quantite  REAL    NOT NULL DEFAULT 0,
  ca_tvac   REAL    NOT NULL DEFAULT 0,
  ca_htva   REAL    NOT NULL DEFAULT 0,
  tva_rate  REAL    NOT NULL DEFAULT 0,
  PRIMARY KEY (date,categorie,produit)
);

CREATE INDEX IF NOT EXISTS idx_import_lightspeed ON import_lightspeed(date);
"

r <- sql_req(create_sql)

# Préparation de la DB pour import en sql

import_lightspeed <- IMPORT_LIGHTSPEED  |>
  filter(!is.na(PRODUCT)) |> 
  transmute(
    DATE = format(DATE, "%Y-%m-%d"),
    CATEGORY = toupper(trimws(CATEGORY)),
    PRODUIT = PRODUCT,
    PRIX = PRICE,
    QUANTITE = `#`,
    CA_TVAC = TOTAL,
    CA_HTVA = PROFIT,
    TVA_RATE = `TVA RATE`
  ) %>%
  mutate(TVA_RATE = as.numeric(gsub("%", "", gsub(",",".",TVA_RATE)))/ 100,
         TVA_RATE = ifelse(TVA_RATE < 0.01,TVA_RATE*100,TVA_RATE),
         TVA_RATE = replace_na(TVA_RATE,0))

# Check qualité
stopifnot(unique(import_lightspeed$TVA_RATE) %in% c(0,0.06,0.12,0.21))

paquets <- df_to_paquets(import_lightspeed)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO import_lightspeed ",
  "(date, categorie, produit, prix, quantite, ca_tvac, ca_htva, tva_rate) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)

##### IMPORT IMPORT OBJECTIFS ####

# Création de la DB en sql

create_sql <- "
CREATE TABLE IF NOT EXISTS import_objectifs (
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

CREATE INDEX IF NOT EXISTS idx_import_objectifs ON import_objectifs(annee);
"

r <- sql_req(create_sql)

# Préparation de la DB pour import en sql

# Ici, il faut d'abord nettoyer la DB en entrée


transmute_objectifs <- function(import){
  for (i_col in 1:ncol(import)){
    if (all(is.na(import[,i_col]))){
      db <- import[,1:(i_col-1)]
      break
    }
  }
  for (i_row in 1:nrow(db)){
    if (all(is.na(db[i_row,]))){
      db <- db[1:(i_row-1),]
      break
    }
  }
  
  debut_mois <- floor_date(ymd(as.Date(as.numeric(colnames(db)[3:14]), origin=as.Date("1900-01-01"))),"month")
  fin_mois <- ceiling_date(debut_mois, "month")-1
  
  db <- tibble(data.frame(t(db))) %>%
    filter(row_number() != 1)
  
  colnames(db) <- db[1,]
  db <- db[-1,]
  
  db <- db %>%
    rename(
      CA_HTVA = `Chiffres d'affaires`,
      CA_HTVA_NOURRITURE_6 = `Ventes nourriture à emporter 6%`,
      CA_HTVA_NOURRITURE_12 = `Ventes nourriture sur place 12%`,
      CA_HTVA_BOISSON_21 = `Ventes boissons sur place 21%`
      # CA_HTVA_BOISSON_EXPORT_21 = `Ventes boissons à emporter 21%`,
      # CA_HTVA_TRAITEUR_EXPORT = `Ventes traiteur à emporter`,
      # CA_HTVA_TRAITEUR = `Ventes traiteur sur place`,
      # CA_HTVA_ATELIER = `Ateliers`
    ) %>%
    select(starts_with("CA_HTVA")) %>%
    mutate_all(as.numeric)
  
  db[is.na(db)] <- 0
  
  db$DATE_DEBUT <- debut_mois
  db$DATE_FIN <- fin_mois
  
  db <- db %>%
    # mutate(CA_TVAC = 1.21 * (CA_HTVA_BOISSON_21 + CA_HTVA_BOISSON_EXPORT_21) +
    mutate(CA_TVAC = 1.21 * (CA_HTVA_BOISSON_21) +
             1.12 * (CA_HTVA_NOURRITURE_12) +
             1.06 * CA_HTVA_NOURRITURE_6,
           MOIS = month(DATE_DEBUT),
           ANNEE = year(DATE_DEBUT))
  db
}

import_objectifs <- transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS`) |> 
  add_row(transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS 2025`)) %>%
  add_row(transmute_objectifs(DB_sheets$`IMPORT OBJECTIFS 2026`)) |> 
  select(ANNEE,MOIS,DATE_DEBUT,DATE_FIN,CA_TVAC,CA_HTVA,
         CA_HTVA_NOURRITURE_6,CA_HTVA_NOURRITURE_12,CA_HTVA_BOISSON_21) |> 
  mutate(DATE_DEBUT = as.character(DATE_DEBUT),
         DATE_FIN = as.character(DATE_FIN))

paquets <- df_to_paquets(import_objectifs)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO import_objectifs ",
  "(annee, mois, date_debut, date_fin, ca_tvac, ca_htva,
  ca_htva_6, ca_htva_12, ca_htva_21) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)

##### IMPORT IMPORT BIERES CORRESPONDANCE ####

# Création de la DB en sql

create_sql <- "
CREATE TABLE IF NOT EXISTS import_bieres_correspondance (
  nom_brassin    TEXT    NOT NULL,
  nom_biere      TEXT    DEFAULT '',
  nom_logo       TEXT    DEFAULT '',
  PRIMARY KEY (nom_brassin)
);

CREATE INDEX IF NOT EXISTS idx_import_bieres_correspondance ON 
import_bieres_correspondance(nom_brassin);
"

r <- sql_req(create_sql)

# Préparation de la DB pour import en sql

import_bieres_correspondance <- IMPORT_BIERES_CORRESPONDANCE %>%
  filter(!is.na(`Nom commercial`)) |> 
  transmute(
    BRASSIN = `Nom commercial`,
    BOISSON ,
    LOGO    = `NAME LOGO`
    )

paquets <- df_to_paquets(import_bieres_correspondance)

vec_sql <- vapply(paquets, function(p) paste0(
  "INSERT OR REPLACE INTO import_bieres_correspondance ",
  "(nom_brassin, nom_biere, nom_logo) VALUES\n",
  paste(p, collapse = ",\n"), ";"), character(1))

r <- map(vec_sql, sql_req)
