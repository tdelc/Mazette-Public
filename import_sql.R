#### library

library(tidyverse)

#### Fonctions

source("R/sql.R")

#### Import de toutes les DB de sqlite

d1_select_format <- function(sql){
  db <- sql_select(sql)
  colnames(db) <- toupper(colnames(db))
  if ("DATE" %in% colnames(db)) db$DATE <- ymd(db$DATE)
  return(db)
}

DB_JOURS       <- d1_select_format("SELECT * FROM jours")
NOMEN_PRODUITS <- d1_select_format("SELECT * FROM nomen_produits")
NOMEN_BIERES   <- d1_select_format("SELECT * FROM nomen_bieres")
BRASSINS       <- d1_select_format("SELECT * FROM brassins")
# CAISSE         <- d1_select_format("SELECT * FROM caisse")
PRODUITS       <- d1_select_format("SELECT * FROM produits")
OBJECTIFS      <- d1_select_format("SELECT * FROM objectifs")
IM_TICKET      <- d1_select_format("SELECT * FROM tickets")
IMPORT_PASS    <- d1_select_format("SELECT * FROM password")

OBJECTIFS$DATE_DEBUT      <- ymd(OBJECTIFS$DATE_DEBUT)
OBJECTIFS$DATE_FIN        <- ymd(OBJECTIFS$DATE_FIN)
OBJECTIFS$DATE_FIN        <- ymd(OBJECTIFS$DATE_FIN)
BRASSINS$DATE_BRASSIN     <- ymd(BRASSINS$DATE_BRASSIN)
BRASSINS$DATE_CONDI       <- ymd(BRASSINS$DATE_CONDI)
BRASSINS$DATE_FIN         <- ymd(BRASSINS$DATE_FIN)
BRASSINS$DATE_DECLARATION <- ymd(BRASSINS$DATE_DECLARATION)

#### Paramètre de la journée

date_actuelle        <- Sys.Date()
premier_jour_semaine <- date_actuelle - as.POSIXlt(date_actuelle)$wday + 1
jours_semaine        <- seq(from = premier_jour_semaine, by = "days", length.out = 7)
vecteur_jours_LOCAL  <- weekdays(jours_semaine)

#### DB_DATE ####

# Faire une DB date pour avoir toutes les dates, de 2023 à today()

DB_DATE <- creer_db_date()

#### DB_PRODUITS_JOURS ####

# D'abord, pour chaque produit et chaque date d'ouverture de Mazette, le CA et le nombre vendu

DB_TICKET <- IM_TICKET |> 
  left_join(NOMEN_PRODUITS) |> 
  mutate(
    CD_HEURE = ifelse(hour(TIMESTAMP) < 17,"Midi (<17h)","Soir (>=17h)"),
    CD_SECTEUR = ifelse(TAUX_TVA == 0.12,"Nourriture","Boisson"),
    CD_PERIODE_JOUR = if_else(hour(TIMESTAMP) %in% c(8:16),"Jour","Soir"),
    CD_PERIODE_SEMAINE = if_else(
      wday(TIMESTAMP,week_start = 1) %in% c(6,7)
      | (wday(TIMESTAMP,week_start = 1) == 5 &
           CD_PERIODE_JOUR == "Soir"),"Week-end","Semaine"
    ),
    VOLUME_TOT_L = QUANTITE*VOLUME_CL/100
  )

TICKETS_HEURES <- DB_TICKET %>%
  filter(PRIX_TOTAL > 0) %>%
  group_by(DATE,CD_HEURE,CD_SECTEUR,
           CD_PERIODE_JOUR,CD_PERIODE_SEMAINE,
           PRODUIT_FULL,PRODUIT,CATEGORIE,TAUX_TVA) %>%
  summarise(CA_TVAC = sum(PRIX_TOTAL),QUANTITE = sum(QUANTITE),.groups = "drop") |> 
  mutate(CA_HTVA = CA_TVAC / (1+TAUX_TVA))

# Synthèse par catégorie
DB_CATEGORIES_JOURS <- TICKETS_HEURES %>%
  group_by(DATE,CD_HEURE,CATEGORIE) %>%
  summarise(CA_HTVA = sum(CA_HTVA),QUANTITE = sum(QUANTITE),.groups = "drop") |> 
  complete(DATE, CD_HEURE, CATEGORIE,
           fill = list(CA_HTVA = 0, QUANTITE = 0))

#### DB_KPI ####

DB_KPI <- TICKETS_HEURES %>%
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
  left_join(DB_KPI_JOUR) %>%
  left_join(DB_KPI_SEMAINE) %>%
  left_join(DB_KPI_SECTEUR) %>%
  mutate(CA_HTVA_KEEP = CA_HTVA)

# Correction ici en règle de trois, afin que chaque paire corresponde au CA de DB_JOURS

DB_KPI_SIMPLE <- DB_KPI_SIMPLE %>%
  mutate(ratio = CA_HTVA/(Boisson + Nourriture),
         Boisson = Boisson*ratio,Nourriture = Nourriture*ratio) %>%
  mutate(ratio = CA_HTVA/(`Week-end` + Semaine),
         `Week-end` = `Week-end`*ratio,Semaine = Semaine*ratio) %>%
  mutate(ratio = CA_TVAC/(Jour + Soir),
         Jour = Jour*ratio,Soir = Soir*ratio)

#### Import Objectifs #####

DB_OBJECTIFS <- bind_rows(
  lapply(1:nrow(OBJECTIFS), function(i) {
    with(OBJECTIFS[i, ],
         calculer_objectifs_journaliers(ANNEE, MOIS, CA_HTVA, CA_TVAC))
  })
)

#### Import Brassins ####

DB_BRASSINS <- BRASSINS %>% 
  mutate(
    DT_BRASSIN = DATE_BRASSIN,
    DT_CONDI = DATE_CONDI,
    DT_FIN = DATE_FIN,
    DT_DECLA = DATE_DECLARATION
  ) |> 
  left_join(NOMEN_BIERES) |> 
  rename(BOISSON = NOM_BIERE)

# Une ligne par date*brassin, afin de déterminer quel brassin pour quel jour
# S'il y a des doublons, je prends le nouveau brassin
DB_BRASSINS_EXP <- DB_BRASSINS %>%
  filter(!is.na(DATE_CONDI)) %>%
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

# ADD BRASSINS
DB_PRODUITS <- PRODUITS %>%
  left_join(DB_BRASSINS_EXP) %>%
  left_join(DB_BRASSINS) %>%
  mutate(VOLUME_BRASSIN_AJUST = VOLUME_BRASSIN*0.75)

PRIX_BIERES <- DB_PRODUITS %>%
  filter(CATEGORIE ==  "BIÈRES" & !is.na(ID_BRASSIN) & VOLUME_CL == 33) %>%
  select(PRIX_33CL = PRIX,BOISSON) %>%
  distinct() %>%
  group_by(BOISSON) %>%
  arrange(-PRIX_33CL) %>%
  filter(row_number() == 1) |> 
  ungroup()

# Ajout des stats sur les ventes
DB_BIERES <- DB_PRODUITS %>%
  filter(CATEGORIE ==  "BIÈRES" & !is.na(ID_BRASSIN)) %>%
  left_join(PRIX_BIERES) %>%
  right_join(DB_DATE) %>%
  mutate(VOLUME_TOT_L = QUANTITE*VOLUME_CL/100) |> 
  group_by(CATEGORIE,BOISSON,ID_BRASSIN,BIERE_FINIE,DATE,
           VOLUME_BRASSIN,VOLUME_BRASSIN_AJUST,PRIX_33CL) %>%
  summarise(CA_HTVA = sum(CA_HTVA),
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



#### DB_HEURES ####

prepa_heures <- function(id_drive){
  
  drive_mazette <- drive_download(drive_get(id =id_drive),overwrite = TRUE)
  DB_HEURES <- read_excel(drive_mazette$local_path,sheet = "Rapport",skip = 1)
  DB_HEURES <- DB_HEURES %>% 
    filter(!is.na(Jour),Jour != "Jour",Jour != "Nom") %>% 
    mutate(DATE = as.Date(as.numeric(Jour), origin = "1899-12-30")) %>% 
    mutate(HEURE_DEB = substr(Temps,1,5),
           HEURE_FIN = substr(Temps,9,13)) %>% 
    rename(CONTRAT = `Type de contrat`,
           DEPARTEMENT = `Nom de l’équipe`,
           HEURES_PAUSE = Pauses)
  
  # Nettoyage
  DB_HEURES <- DB_HEURES %>% filter(Temps != "12:08 - 12:06")
  
  DB_HEURES <- DB_HEURES %>%
    mutate(
      debut_h = hour(hm(HEURE_DEB)) + minute(hm(HEURE_DEB))/60,
      fin_h   = hour(hm(HEURE_FIN))   + minute(hm(HEURE_FIN))/60,
      HEURE_MIDI = case_when(
        debut_h == 0 & fin_h == 0 ~ 0,
        debut_h >= 17 ~ 0,
        fin_h <= 17 & fin_h > 7 ~ fin_h - debut_h,
        TRUE ~ 17 - debut_h
        # fin_h > 7 ~ pmin(fin_h, 17) - pmin(debut_h, 17),
        # TRUE ~ 24 - (debut_h + fin_h)
      ),
      HEURE_SOIR = case_when(
        debut_h == 0 & fin_h == 0 ~ 0,
        fin_h > 7 & fin_h <= 17 ~ 0,
        # fin_h > 17 ~ pmax(fin_h, 17) - pmax(debut_h, 17),
        fin_h >= 17 ~ fin_h - 17,
        debut_h >= 17 ~ 24 - debut_h + fin_h,
        # TRUE ~ 7 + fin_h
        TRUE ~ 7 + fin_h
      )
    ) %>%
    mutate(HEURES = as.numeric(`Heures travaillées`)) %>% 
    filter(HEURES > 0) %>% 
    # Correction des heures en imputant les heures travaillées selon la clé
    # Heures midi et heures soir
    mutate(
      PC_MIDI = HEURE_MIDI / (HEURE_MIDI + HEURE_SOIR),
      PC_SOIR = HEURE_SOIR / (HEURE_MIDI + HEURE_SOIR),
      HEURE_MIDI = HEURES * PC_MIDI,
      HEURE_SOIR = HEURES * PC_SOIR
    ) %>% 
    # filter(HEURES - (HEURE_SOIR + HEURE_MIDI) < 0.2) %>%
    select(DATE,CONTRAT,DEPARTEMENT,HEURE_MIDI,HEURE_SOIR,
           HEURE_DEB,HEURE_FIN,debut_h,fin_h,HEURES,HEURES_PAUSE) %>%
    pivot_longer(cols = c(HEURE_MIDI,HEURE_SOIR), names_to = "CD_HEURE",values_to = "NB_HEURES") %>% 
    group_by(DATE,CONTRAT,DEPARTEMENT,CD_HEURE) %>% 
    summarise(NB_HEURES = sum(NB_HEURES)) %>% 
    left_join(DB_DATE %>% select(DATE,JOUR_SEMAINE)) %>% 
    mutate(CD_HEURE = ifelse(CD_HEURE == "HEURE_MIDI","Midi (<17h)","Soir (>=17h)")) %>% 
    mutate(CD_HEURE = ifelse(JOUR_SEMAINE == "mardi","Soir (>=17h)", CD_HEURE)) %>% 
    mutate(CD_HEURE = ifelse(JOUR_SEMAINE == "dimanche","Midi (<17h)", CD_HEURE)) %>% 
    ungroup()
  
  df_cout <- read_excel(drive_mazette$local_path,sheet = "Cout")
  colnames(df_cout) <- c("CONTRAT","COUT_HEURE")
  
  DB_HEURES <- DB_HEURES %>% left_join(df_cout) %>% 
    mutate(COUT = COUT_HEURE * NB_HEURES)
  
  return(DB_HEURES)
}

DB_COUTS_TRAVAIL <- prepa_heures(Sys.getenv("ID_DRIVE_HEURES")) |> 
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

# TICKETS_HEURES est conservé : c'est la seule table au grain
# DATE x CD_HEURE x PRODUIT, dont l'onglet Travail a besoin pour distinguer
# les créneaux midi / soir (la table SQL `produits` est agrégée à la journée).
rm(DB_DATE, vec_brassin_bug, PRIX_BIERES, DB_BRASSINS_EXP, BRASSINS,
   PRODUITS, DB_KPI, IM_TICKET, OBJECTIFS)
