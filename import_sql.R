#### library

library(tidyverse)

#### Fonctions

source("fonctions_import_sql.R")

#### Import de toutes les DB de sqlite

DB_JOURS                     <- d1_select("SELECT * FROM db_jours")
IMPORT_BIERES_CORRESPONDANCE <- d1_select("SELECT * FROM import_bieres_correspondance")
IMPORT_BRASSINS              <- d1_select("SELECT * FROM import_brassins")
IMPORT_CAISSE                <- d1_select("SELECT * FROM import_caisse")
IMPORT_LIGHTSPEED            <- d1_select("SELECT * FROM import_lightspeed")
IMPORT_OBJECTIFS             <- d1_select("SELECT * FROM import_objectifs")
IMPORT_TICKET                <- d1_select("SELECT * FROM import_ticket")
CORRESP_PRODUIT              <- d1_select("SELECT * FROM corresp_produit")

colnames(DB_JOURS)                     <- toupper(colnames(DB_JOURS))
colnames(IMPORT_BIERES_CORRESPONDANCE) <- toupper(colnames(IMPORT_BIERES_CORRESPONDANCE))
colnames(IMPORT_BRASSINS)              <- toupper(colnames(IMPORT_BRASSINS))
colnames(IMPORT_TICKET)                <- toupper(colnames(IMPORT_TICKET))
colnames(IMPORT_CAISSE)                <- toupper(colnames(IMPORT_CAISSE))
colnames(IMPORT_LIGHTSPEED)            <- toupper(colnames(IMPORT_LIGHTSPEED))
colnames(IMPORT_OBJECTIFS)             <- toupper(colnames(IMPORT_OBJECTIFS))
colnames(CORRESP_PRODUIT)              <- toupper(colnames(CORRESP_PRODUIT))

DB_JOURS$DATE                    <- ymd(DB_JOURS$DATE)
IMPORT_CAISSE$DATE               <- ymd(IMPORT_CAISSE$DATE)
IMPORT_LIGHTSPEED$DATE           <- ymd(IMPORT_LIGHTSPEED$DATE)
IMPORT_TICKET$DATE               <- ymd(IMPORT_TICKET$DATE)
IMPORT_OBJECTIFS$DATE_DEBUT      <- ymd(IMPORT_OBJECTIFS$DATE_DEBUT)
IMPORT_OBJECTIFS$DATE_FIN        <- ymd(IMPORT_OBJECTIFS$DATE_FIN)
IMPORT_OBJECTIFS$DATE_FIN        <- ymd(IMPORT_OBJECTIFS$DATE_FIN)
IMPORT_BRASSINS$DATE_BRASSIN     <- ymd(IMPORT_BRASSINS$DATE_BRASSIN)
IMPORT_BRASSINS$DATE_CONDI       <- ymd(IMPORT_BRASSINS$DATE_CONDI)
IMPORT_BRASSINS$DATE_FIN         <- ymd(IMPORT_BRASSINS$DATE_FIN)
IMPORT_BRASSINS$DATE_DECLARATION <- ymd(IMPORT_BRASSINS$DATE_DECLARATION)

#### Paramètre de la journée

date_actuelle <- Sys.Date()
premier_jour_semaine <- date_actuelle - as.POSIXlt(date_actuelle)$wday + 1
jours_semaine <- seq(from = premier_jour_semaine, by = "days", length.out = 7)
vecteur_jours_LOCAL <- weekdays(jours_semaine)

#### DB_DATE ####

# Faire une DB date pour avoir toutes les dates, de 2023 à today()

DB_DATE <- tibble(
  DATE = list(seq.Date(min(DB_JOURS$DATE), 
                       ceiling_date(today(), "year")-1,by= "1 day"))) |> 
  unnest(cols = c(DATE)) |> 
  mutate(
    JOUR_SEMAINE = lubridate::wday(DATE,week_start = 1),
    JOUR_SEMAINE = factor(vecteur_jours[JOUR_SEMAINE],
                          levels = vecteur_jours),
    ANNEE_MOIS = paste0(year(DATE),"-",month(DATE)),
    ANNEE_SEMAINE = paste0(year(DATE),"-",isoweek(DATE)),
    ANNEE_TRIM = paste0(year(DATE),"-",quarters(DATE)),
    PREMIER_JOUR_SEMAINE = DATE-lubridate::wday(DATE,week_start = 1)+1,
    PREMIER_JOUR_MOIS = DATE-mday(DATE)+1
  )

#### DB_TICKET ####

DB_TICKET <- IMPORT_TICKET
colnames(DB_TICKET) <- toupper(colnames(IMPORT_TICKET))
colnames(CORRESP_PRODUIT) <- toupper(colnames(CORRESP_PRODUIT))

DB_TICKET <- DB_TICKET %>%
  mutate(
    TIMESTAMP = ifelse(nchar(TIMESTAMP) == 19,TIMESTAMP,paste(substr(TIMESTAMP,1,10),"00:00:00")),
    TIMESTAMP = ymd_hms(TIMESTAMP),
    CD_PERIODE_JOUR = if_else(hour(TIMESTAMP) %in% c(8:16),"Jour","Soir"),
    CD_PERIODE_SEMAINE = if_else(
      wday(TIMESTAMP,week_start = 1) %in% c(6,7)
      | (wday(TIMESTAMP,week_start = 1) == 5 &
           CD_PERIODE_JOUR == "Soir"),"Week-end","Semaine"
    )
  ) %>%
  left_join(CORRESP_PRODUIT) |> 
  mutate(PRODUIT = case_when(
    PRODUIT == "Cola maison" ~ "Cola maison 33cL",
    PRODUIT == "Dik effiloché de porc crémeux de carottes 1/2" ~ "Dik effiloché de porc crémeux de carottes",
    PRODUIT == "Sombre Despote" ~ "Sombre Despote 33cL",
    TRUE ~ PRODUIT)) %>%
  rename(PRODUCT = PRODUIT) |> 
  mutate(PRODUCT = ifelse(PRODUCT == "Cola maison","Cola maison 33cL",PRODUCT)) %>%
  from_product_to_boisson()


#### DB_PRODUITS ####

DB_PRODUITS <- IMPORT_LIGHTSPEED
colnames(DB_PRODUITS) <- toupper(colnames(DB_PRODUITS))

DB_PRODUITS <- DB_PRODUITS %>%
  rename(PRODUCT = PRODUIT) |> 
  mutate(
    QUANTITE = case_when(
    substr(PRODUCT,1,4) == "Dikk" & PRIX < 6 ~ QUANTITE/4,
    substr(PRODUCT,1,4) == "Dikk" & PRIX < 10 ~ QUANTITE/3,
    TRUE ~ QUANTITE)
  ) %>%
  from_product_to_boisson()

vec_sale <- DB_PRODUITS %>% filter(CATEGORIE == "SALÉ") %>%
  pull(PRODUCT_FULL) %>% unique()

vec_sucre <- DB_PRODUITS %>% filter(CATEGORIE == "SUCRÉ") %>%
  pull(PRODUCT_FULL) %>% unique()

DB_PRODUITS[DB_PRODUITS$PRODUCT_FULL %in% vec_sale,"CATEGORIE"] <- "SALÉ"
DB_PRODUITS[DB_PRODUITS$PRODUCT_FULL %in% vec_sucre,"CATEGORIE"] <- "SUCRÉ"

DB_PRODUITS_REF <- DB_PRODUITS %>%
  filter(PRIX > 0) %>%
  mutate(TVA_RATE = if_else(
    PRODUCT_FULL == "Pain à emporter",0.06,TVA_RATE)) %>%
  select(PRODUCT_FULL,TVA_RATE) %>%
  distinct() %>%
  add_row(PRODUCT_FULL = "TVUG 50 cl",TVA_RATE = 0.21) %>%
  add_row(PRODUCT_FULL = "TVUG 33cl",TVA_RATE = 0.21) %>%
  add_row(PRODUCT_FULL = "Pizza",TVA_RATE = 0.12) %>%
  add_row(PRODUCT_FULL = "Dikke",TVA_RATE = 0.12) %>%
  add_row(PRODUCT_FULL = "Verrine",TVA_RATE = 0.12) %>%
  add_row(PRODUCT_FULL = "Thé",TVA_RATE = 0.21) %>%
  group_by(PRODUCT_FULL) %>% arrange(-TVA_RATE) %>%
  filter(row_number() == 1) %>% ungroup()

DB_TICKET_PRODUITS <- DB_TICKET %>%
  mutate(PRODUCT_FULL = case_when(
    str_detect(PRODUCT_FULL,"Capuccino") ~ "Capuccino",
    str_detect(PRODUCT_FULL,"Café") ~ "Café",
    str_detect(PRODUCT_FULL,"Espresso") ~ "Espresso",
    str_detect(PRODUCT_FULL,"Pizza") ~ "Pizza",
    str_detect(PRODUCT_FULL,"Verrine") ~ "Verrine",
    str_detect(PRODUCT_FULL,"Dikke") ~ "Dikke",
    str_detect(PRODUCT_FULL,"Thé") ~ "Thé",
    TRUE ~ PRODUCT_FULL
  )) %>%
  full_join(DB_PRODUITS_REF) %>%
  mutate(NEW_TVA_RATE = as.numeric(
    str_extract(PRODUCT_FULL,".*\\(Tax: ([0-9]+)%\\).*",group=1)),
    NEW_TVA_RATE = if_else(NEW_TVA_RATE %in% c(0,6,12,21),
                           NEW_TVA_RATE/100,NA_real_),
    TVA_RATE = if_else(is.na(TVA_RATE),NEW_TVA_RATE,TVA_RATE)) %>%
  mutate(CD_SECTEUR = if_else(TVA_RATE == 0.21,"Boisson","Nourriture"),
         CA_TVAC = PRIX_TOTAL, CA_HTVA = CA_TVAC/(1+TVA_RATE))



#### DB_PRODUITS_JOURS ####

# D'abord, pour chaque produit et chaque date d'ouverture de Mazette, le CA et le nombre vendu
DB_PRODUITS_JOURS <- DB_TICKET %>%
  filter(PRIX_TOTAL > 0) %>%
  mutate(CD_HEURE = ifelse(hour(TIMESTAMP) < 17,
                           "Midi (<17h)","Soir (>=17h)")) %>%
  group_by(DATE,CD_HEURE,PRODUCT_FULL,PRODUCT) %>%
  summarise(CA_TVAC = sum(PRIX_TOTAL),QUANTITE = sum(QUANTITE),.groups = "drop")

# On vient ajouter le taux de TVA 
# Trouver le taux de TVA (supposé), en évitant les doublons
DB_TVA <- DB_PRODUITS %>%
  filter(PRIX > 0,TVA_RATE != 0.06,
         !CATEGORIE %in% c("UNKNOWN (REMOVED)","BOUFFE À EMPORTER")) %>%
  count(CATEGORIE,PRODUCT_FULL,PRODUCT,TVA_RATE) |> 
  group_by(PRODUCT_FULL,PRODUCT) |> mutate(nd = n()) |> 
  arrange(-n) |> filter(row_number() == 1) |> ungroup() |> 
  select(CATEGORIE,PRODUCT_FULL,PRODUCT,TVA_RATE)

DB_PRODUITS_JOURS <- DB_PRODUITS_JOURS %>% 
  left_join(DB_TVA) |> 
  filter(!is.na(TVA_RATE)) %>%
  mutate(CA_HTVA = CA_TVAC / (1+TVA_RATE),
         SECTEUR = ifelse(TVA_RATE == 0.12,"Nourriture","Boisson"))

# Synthèse par catégorie
DB_CATEGORIES_JOURS <- DB_PRODUITS_JOURS %>%
  group_by(DATE,CD_HEURE,CATEGORIE) %>%
  summarise(CA_HTVA = sum(CA_HTVA),QUANTITE = sum(QUANTITE),.groups = "drop") |> 
  complete(DATE, CD_HEURE, CATEGORIE,
           fill = list(CA_HTVA = 0, QUANTITE = 0))



#### DB_KPI ####

DB_KPI <- DB_TICKET_PRODUITS %>%
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

colnames(DB_JOURS) <- toupper(colnames(DB_JOURS))

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

INFO_OBJECTIFS <- tibble(JOUR_SEMAINE = 1:7,
                         OBJECTIF_PCT = c(0,0.065,0.1,0.125,0.21,0.29,0.21))

colnames(IMPORT_OBJECTIFS) <- toupper(colnames(IMPORT_OBJECTIFS))

DB_OBJECTIFS <- bind_rows(
  lapply(1:nrow(IMPORT_OBJECTIFS), function(i) {
    with(IMPORT_OBJECTIFS[i, ],
         calculer_objectifs_journaliers(ANNEE, MOIS, CA_HTVA, CA_TVAC))
  })
)

#### Import Brassins ####

colnames(IMPORT_BRASSINS) <- toupper(colnames(IMPORT_BRASSINS))
colnames(IMPORT_BIERES_CORRESPONDANCE) <- toupper(colnames(IMPORT_BIERES_CORRESPONDANCE))

DB_BRASSINS <- IMPORT_BRASSINS %>% 
  mutate(
    DT_BRASSIN = DATE_BRASSIN,
    DT_CONDI = DATE_CONDI,
    DT_FIN = DATE_FIN,
    DT_DECLA = DATE_DECLARATION
  ) |> 
  left_join(IMPORT_BIERES_CORRESPONDANCE) |> 
  rename(BOISSON = NOM_BIERE)

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

# ADD BRASSINS
DB_PRODUITS <- DB_PRODUITS %>%
  left_join(DB_BRASSINS_EXP) %>%
  left_join(DB_BRASSINS)

# Ajuster le vrai volume de brassin selon les bières finies
DB_PRODUITS <- DB_PRODUITS %>%
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

DB_COUTS_TRAVAIL <- prepa_heures(id_sheet_heures) |> 
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

rm(IMPORT_BIERES_CORRESPONDANCE, IMPORT_BRASSINS, IMPORT_CAISSE,
   IMPORT_LIGHTSPEED, IMPORT_OBJECTIFS, IMPORT_TICKET, DB_TICKET_PRODUITS)


