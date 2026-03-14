
#### Paramètres de dates ####

vecteur_jours <- c("lundi","mardi","mercredi",
                   "jeudi","vendredi","samedi",
                   "dimanche")

vecteur_jours_LOCAL <- c("Monday","Tuesday","Wednesday",
                         "Thursday","Friday","Saturday","Sunday")

date_actuelle <- Sys.Date()
premier_jour_semaine <- date_actuelle - as.POSIXlt(date_actuelle)$wday + 1
jours_semaine <- seq(from = premier_jour_semaine, by = "days", length.out = 7)
vecteur_jours_LOCAL <- weekdays(jours_semaine)

#### Fonctions de traitement ####

from_product_to_boisson <- function(DB){
  DB %>%
    mutate(PRODUCT_VIDE = str_remove(PRODUCT," [0-9]+ *c[lL]"),
           PRODUCT_VIDE = str_remove(PRODUCT_VIDE," verre"),
           PRODUCT_VIDE = str_remove(PRODUCT_VIDE," 1L"),
           VOLUME_CL = case_when(
             PRODUCT %in% c("Pépin blanc verre",
                            "Pépin rouge verre",
                            "Hurluberlu rouge verre") ~ 12.5,
             PRODUCT %in% c("Cidre Rhuys","Kefir") ~ 25,
             PRODUCT %in% c("Rhum Brussels") ~ 3,
             str_detect(PRODUCT,"1L") ~ 100,
             TRUE ~ as.numeric(str_extract(PRODUCT," ([0-9]+) *c*[lL]",group= 1))
           ),
           BOISSON = case_when(
             is.na(VOLUME_CL) ~ "",
             TRUE ~ PRODUCT_VIDE
           ),
           VOLUME_TOT_L = QUANTITE*VOLUME_CL/100
    ) %>%
    rename(PRODUCT_FULL = PRODUCT,
           PRODUCT = PRODUCT_VIDE)
}

# Fonction pour calculer les objectifs journaliers
calculer_objectifs_journaliers <- function(annee, mois, ca_htva, ca_tvac) {
  # Nombre de jours dans le mois
  jours_dans_mois <- seq(ymd(paste(annee, mois, 1)),by = "day",
                         length.out = days_in_month(ymd(paste(annee, mois, 1))))

  # Calculer la semaine du mois pour chaque jour
  semaine_du_mois <- week(jours_dans_mois) - week(min(jours_dans_mois)) + 1

  # Préparer les données pour le calcul
  jours_data <- data.frame(
    DATE = jours_dans_mois,
    ANNEE = year(jours_dans_mois),
    MOIS = month(jours_dans_mois),
    SEMAINE_DU_MOIS = semaine_du_mois,
    JOUR_SEMAINE = lubridate::wday(jours_dans_mois,week_start = 1)
  )

  # Joindre avec les objectifs journaliers en %
  jours_data <- left_join(jours_data, INFO_OBJECTIFS, by = c("JOUR_SEMAINE" = "JOUR_SEMAINE")) %>%
    mutate(OBJECTIF_PCT = OBJECTIF_PCT/sum(OBJECTIF_PCT)) %>%
    select(-JOUR_SEMAINE)

  # Calculer l'objectif journalier
  jours_data <- jours_data %>%
    mutate(CA_HTVA = round(ca_htva * OBJECTIF_PCT),
           CA_TVAC = round(ca_tvac * OBJECTIF_PCT))

  return(jours_data)
}

#### DB_DATE ####

DB_DATE <- tibble(
  DATE = list(seq.Date(min(ymd(IMPORT_OLD$DATEVALUE)),
                       ceiling_date(today(), "year")-1,by= "1 day"))) %>%
  unnest(cols = c(DATE)) %>%
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

##### AJOUT COMMENTAIRES ####

DB_DATE <- DB_DATE %>%
  left_join(COMMENTAIRES %>% mutate(DATE = dmy(DATE))) %>%
  rename(AUTEUR_COMMENTAIRE = AUTEUR,
         TIMESTAMP_COMMENTAIRE = TIMESTAMP) %>%
  mutate(COMMENTAIRE_FULL = ifelse(is.na(COMMENTAIRE),"",
                                   paste0(COMMENTAIRE," (",
                                          AUTEUR_COMMENTAIRE,", ",
                                          TIMESTAMP_COMMENTAIRE,")\n")))


#### DB_TICKET ####

DB_TICKET <- IMPORT_TICKET %>%
  rename(NB_CLIENTS = NB_CUSTOMERS) %>%
  mutate(DATE = ymd(DATE),
         TIMESTAMP = dmy_hm(str_remove(TIMESTAMP," \\+[0-9]{4}")),
         TIMESTAMP_MOD = dmy_hm(str_remove(TIMESTAMP_MOD," \\+[0-9]{4}")),
         ID_TICKET = as.numeric(ID_TICKET),
         NB_CLIENTS = as.numeric(NB_CLIENTS),
         CD_PERIODE_JOUR = if_else(hour(TIMESTAMP) %in% c(8:16),"Jour","Soir"),
         CD_PERIODE_SEMAINE = if_else(
           wday(TIMESTAMP,week_start = 1) %in% c(6,7)
           | (wday(TIMESTAMP,week_start = 1) == 5 &
                CD_PERIODE_JOUR == "Soir"),"Week-end","Semaine"
          )
  ) %>%
  mutate(PRODUCT = case_when(
    PRODUCT == "Cola maison" ~ "Cola maison 33cL",
    PRODUCT == "Dik effiloché de porc crémeux de carottes 1/2" ~ "Dik effiloché de porc crémeux de carottes",
    PRODUCT == "Sombre Despote" ~ "Sombre Despote 33cL",
    TRUE ~ PRODUCT)) %>%
  mutate(PRODUCT = ifelse(PRODUCT == "Cola maison","Cola maison 33cL",PRODUCT)) %>%
  from_product_to_boisson()



#### DB_OLD ####

DB_OLD <- IMPORT_OLD %>%
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
  )

#### DB_JOURS ####

DB_JOURS <- IMPORT_DB_JOURS %>%
  filter(!is.na(DATEVALUE)) %>%
  rename(
    DATE = DATEVALUE,
    CA_CAISSE_21 = `CA CAISSE 21%`,
    CA_CAISSE_12 = `CA CAISSE 12%`,
    CA_CAISSE_6 = `CA CAISSE 6%`,
    CA_CAISSE_0 = `CA CAISSE 0%`,
    CA_CAISSE_TVAC = `CA CAISSE TVAC`,
    CA_CAISSE_HTVA = `CA CAISSE HTVA`,
    TVA_CAISSE_21 = `TVA CAISSE 21%`,
    TVA_CAISSE_12 = `TVA CAISSE 12%`,
    TVA_CAISSE_6 = `TVA CAISSE 6%`,
    TVA_CAISSE = `TVA CAISSE`,
    CA_HTVA = `CA HTVA`,
    CA_TVAC = `CA TVAC`,
  ) %>%
  mutate(DATE = ymd(DATE)) %>%
  select(-NUM_JOUR,-NUM_SEMAINE,-JOUR_SEMAINE,-MOIS) %>%
  left_join(DB_TICKET %>%
              select(DATE,ID_TICKET,NB_CLIENTS) %>% distinct() %>%
              group_by(DATE) %>%
              summarise(NB_CLIENTS = sum(NB_CLIENTS,na.rm=TRUE),
                        NB_TABLES = n(),.groups = "drop"))

DB_JOURS <- DB_JOURS %>%
  add_row(DB_OLD) %>%
  arrange(DATE)


#### DB_CAISSE ####

DB_CAISSE <- IMPORT_CAISSE %>%
  mutate(DATE = ymd(DATE))


#### DB_PRODUITS ####

DB_PRODUITS <- IMPORT_LIGHTSPEED %>%
  filter(!is.na(PRODUCT)) %>%
  select(-...2,-`TOTAL REVENUE`,-DISCOUNT,-`COST TVA EXCL`) %>%
  rename(
    QUANTITE = `#`,
    CA_TVAC = TOTAL,
    CA_HTVA = PROFIT,
    TVA_RATE = `TVA RATE`
  ) %>%
  mutate(
    CATEGORY = toupper(trimws(CATEGORY)),
    TVA_RATE = as.numeric(gsub("%", "", gsub(",",".",TVA_RATE)))/ 100,
    TVA_RATE = ifelse(TVA_RATE < 0.01,TVA_RATE*100,TVA_RATE),
    DATE = ymd(DATE)
  )%>%
  mutate(QUANTITE = case_when(
    substr(PRODUCT,1,4) == "Dikk" & PRICE < 6 ~ QUANTITE/4,
    substr(PRODUCT,1,4) == "Dikk" & PRICE < 10 ~ QUANTITE/3,
    TRUE ~ QUANTITE)) %>%
  from_product_to_boisson()

# Correction des catégories
# DB_PRODUITS[DB_PRODUITS$CATEGORY == "ALCOOL & VIN","CATEGORY"] <- "ALCOOLS & VINS"
# DB_PRODUITS[DB_PRODUITS$CATEGORY == "ANCIENNES BIÈRES","CATEGORY"] <- "BIÈRES"
#
# petit_dej <- DB_PRODUITS %>%
#   filter(CATEGORY == "PETITS DEJ'S & BRUNCHS",!is.na(PRODUCT)) %>%
#   pull(PRODUCT) %>% unique()
#
# DB_PRODUITS[DB_PRODUITS$PRODUCT %in% petit_dej,"CATEGORY"] <- "PETITS DEJ'S & BRUNCHS"
# DB_PRODUITS[DB_PRODUITS$CATEGORY == "LUNCHS & BRUNCHS","CATEGORY"] <- "LUNCHS & SUGGESTIONS"
#
# DB_PRODUITS[DB_PRODUITS$CATEGORY == "A GRIGNOTER" &
#               (str_detect(DB_PRODUITS$PRODUCT_FULL,"[Ss]oupe") |
#                  str_detect(DB_PRODUITS$PRODUCT_FULL,"[Pp]otage") |
#                  str_detect(DB_PRODUITS$PRODUCT_FULL,"[Gg]aspacho") |
#                  str_detect(DB_PRODUITS$PRODUCT_FULL,"[Vv]elouté"))
#             ,"CATEGORY"] <- "SOUPE"
#
# DB_PRODUITS[DB_PRODUITS$CATEGORY == "A GRIGNOTER" &
#               (str_detect(DB_PRODUITS$PRODUCT_FULL,"^[Pp]lanche"))
#             ,"CATEGORY"] <- "PLANCHE"
#
# DB_PRODUITS[DB_PRODUITS$CATEGORY == "A GRIGNOTER" &
#               (str_detect(DB_PRODUITS$PRODUCT_FULL,"[Hh]oumous") |
#                  str_detect(DB_PRODUITS$PRODUCT_FULL,"[Pp]ottekeis") |
#                  str_detect(DB_PRODUITS$PRODUCT_FULL,"[Ll]evain flamme") |
#                  str_detect(DB_PRODUITS$PRODUCT_FULL,"[Ff]ougasse"))
#             ,"CATEGORY"] <- "A GRIGNOTER (MAIN)"
#
# DB_PRODUITS[DB_PRODUITS$CATEGORY == "A GRIGNOTER","CATEGORY"] <- "A GRIGNOTER (SUB)"

table(DB_PRODUITS$CATEGORY)
# DB_PRODUITS %>% count(CATEGORY)
# DB_PRODUITS %>% filter(CATEGORY == "UNKNOWN (REMOVED)") %>% count(PRODUCT)

# Nouveau Mazette 4.0

vec_sale <- DB_PRODUITS %>% filter(CATEGORY == "SALÉ") %>%
  pull(PRODUCT_FULL) %>% unique()

vec_sucre <- DB_PRODUITS %>% filter(CATEGORY == "SUCRÉ") %>%
  pull(PRODUCT_FULL) %>% unique()

DB_PRODUITS[DB_PRODUITS$PRODUCT_FULL %in% vec_sale,"CATEGORY"] <- "SALÉ"
DB_PRODUITS[DB_PRODUITS$PRODUCT_FULL %in% vec_sucre,"CATEGORY"] <- "SUCRÉ"

##### Concordance entre DB_PRODUITS et DB_TICKET ####

TEST <- DB_TICKET %>% filter(PRIX_TOTAL > 0) %>%
  group_by(PRODUCT_FULL,QUANTITE) %>%
  summarise(QUANTITE = sum(QUANTITE),.groups = "drop") %>%
  mutate(PRODUCT_FULL_TICKET = PRODUCT_FULL) %>%
  full_join(DB_PRODUITS %>% filter(PRICE > 0) %>%
              select(PRODUCT_FULL,TVA_RATE) %>% distinct())

DB_PRODUITS_REF <- DB_PRODUITS %>%
  filter(PRICE > 0) %>%
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

DB_TICKET_PRODUITS %>%
  filter(is.na(TVA_RATE)) %>%
  select(DATE,PRODUCT_FULL,PRIX_UNITE,PRIX_TOTAL,TVA_RATE,NEW_TVA_RATE) %>%
  group_by(year(DATE),month(DATE)) %>%
  summarise(sum(PRIX_TOTAL),.groups = "drop")

# Corrections encore à faire ici

#### DB_PRODUITS_JOURS ####

# D'abord, pour chaque produit et chaque date d'ouverture de Mazette, le CA et le nombre vendu
DB_PRODUITS_JOURS <- DB_TICKET %>%
  filter(PRIX_TOTAL > 0) %>%
  mutate(CD_HEURE = ifelse(hour(TIMESTAMP) < 17,
                           "Midi (<17h)","Soir (>=17h)")) %>%
  group_by(DATE,CD_HEURE,PRODUCT_FULL,PRODUCT) %>%
  summarise(CA_TVAC = sum(PRIX_TOTAL),QUANTITE = sum(QUANTITE),.groups = "drop")

# On vient ajouter le taux de TVA (supposé)
DB_PRODUITS_JOURS <- DB_PRODUITS_JOURS %>%
  left_join(DB_PRODUITS %>%
              filter(PRICE > 0,TVA_RATE != 0.06,
                     !CATEGORY %in% c("UNKNOWN (REMOVED)","BOUFFE À EMPORTER")) %>%
              select(CATEGORY,PRODUCT_FULL,PRODUCT,TVA_RATE) %>% distinct())

# DB_PRODUITS_JOURS %>% filter(is.na(TVA_RATE)) %>%
#   count(PRODUCT_FULL) %>% print(n=50)
# Essentiellement du à emporter, ou quelques trucs qu'on peut ignorer

DB_PRODUITS_JOURS <- DB_PRODUITS_JOURS %>%
  filter(!is.na(TVA_RATE)) %>%
  mutate(CA_HTVA = CA_TVAC / (1+TVA_RATE))

DB_PRODUITS_JOURS_FULL <- DB_PRODUITS_JOURS %>%
  mutate(SECTEUR = ifelse(TVA_RATE == 0.12,"Nourriture","Boisson"))

# Synthèse par catégorie
DB_PRODUITS_JOURS <- DB_PRODUITS_JOURS %>%
  group_by(DATE,CD_HEURE,CATEGORY) %>%
  summarise(CA_HTVA = sum(CA_HTVA),QUANTITE = sum(QUANTITE),.groups = "drop")

DB_PRODUITS_JOURS <- DB_PRODUITS_JOURS %>%
  complete(DATE, CD_HEURE, CATEGORY,
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

DB_KPI_SIMPLE <- DB_JOURS %>% select(DATE,CA_HTVA,CA_TVAC) %>%
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

# DB_DATE %>%
#   left_join(DB_KPI) %>% filter(!is.na(CD_PERIODE_JOUR)) %>%
#   group_by(DATE = ANNEE_MOIS,breakdown = CD_PERIODE_JOUR) %>%
#   summarise(CA = sum(CA_HTVA)) %>%
#   ggplot(aes(x=DATE,y=CA,group=breakdown,fill=breakdown))+
#   geom_area(position = "fill")
#
# DB_DATE %>%
#   left_join(DB_KPI) %>% filter(!is.na(CD_PERIODE_JOUR)) %>%
#   group_by(DATE = ANNEE_SEMAINE,breakdown = CD_PERIODE_SEMAINE) %>%
#   summarise(CA = sum(CA_HTVA)) %>%
#   ggplot(aes(x=DATE,y=CA,group=breakdown,fill=breakdown))+
#   geom_area(position = "fill")
#
# DB_DATE %>%
#   left_join(DB_KPI) %>% filter(!is.na(CD_PERIODE_JOUR)) %>%
#   group_by(DATE = ANNEE_MOIS,breakdown = CD_SECTEUR) %>%
#   summarise(CA = sum(CA_HTVA)) %>%
#   ggplot(aes(x=DATE,y=CA,group=breakdown,fill=breakdown))+
#   geom_area(position = "fill")
#
# DB_DATE %>%
#   left_join(DB_KPI) %>% filter(!is.na(CD_PERIODE_JOUR)) %>%
#   group_by(DATE = ANNEE_MOIS,CD_PERIODE_JOUR,CD_SECTEUR) %>%
#   summarise(CA = sum(CA_HTVA)) %>%
#   mutate(breakdown = paste0(CD_PERIODE_JOUR,"-",CD_SECTEUR)) %>%
#   ggplot(aes(x=DATE,y=CA,group=breakdown,fill=breakdown))+
#   geom_area(position = "fill")
#
#
# DB_DATE %>%
#   left_join(DB_KPI) %>% filter(!is.na(CD_PERIODE_JOUR)) %>%
#   group_by(DATE = ANNEE_MOIS,CD_PERIODE_SEMAINE,CD_SECTEUR) %>%
#   summarise(CA = sum(CA_HTVA)) %>%
#   mutate(breakdown = paste0(CD_PERIODE_SEMAINE,"-",CD_SECTEUR)) %>%
#   ggplot(aes(x=DATE,y=CA,group=breakdown,fill=breakdown))+
#   geom_area(position = "fill")


#### DB_HOREKO  ####

DB_HOREKO <- IMPORT_HOREKO %>%
  rename(DATE = DATEVALUE,
         Heures_Service = Service,
         Heures_Cuisine = Cuisine,
         Heures_Boulangerie = Boulangerie,
         Heures_Brasserie = Brasserie,
         Heures_Support = Coordination,
         Cout_Service = `Cout Service`,
         Cout_Cuisine = `Cout Cuisine`,
         Cout_Boulangerie = `Cout Boulangerie`,
         Cout_Brasserie = `Cout Brasserie`,
         Cout_Support = `Cout Coordination`) %>%
  mutate(DATE = ymd(DATE))

#### DB_OBJECTIFS ####

import_objectifs <- function(import){
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

DB_OBJECTIFS_2024 <- import_objectifs(IMPORT_OBJECTIFS)
DB_OBJECTIFS_2025 <- import_objectifs(IMPORT_OBJECTIFS_2025)
DB_OBJECTIFS_2026 <- import_objectifs(IMPORT_OBJECTIFS_2026)

DB_OBJECTIFS <- DB_OBJECTIFS_2024 %>%
  add_row(DB_OBJECTIFS_2025) %>%
  add_row(DB_OBJECTIFS_2026)

INFO_OBJECTIFS <- tibble(JOUR_SEMAINE = 1:7,
                         OBJECTIF_PCT = c(0,0.065,0.1,0.125,0.21,0.29,0.21))

# Appliquer la fonction pour chaque ligne des objectifs mensuels
DB_OBJECTIFS <- bind_rows(
  lapply(1:nrow(DB_OBJECTIFS), function(i) {
    with(DB_OBJECTIFS[i, ],
         calculer_objectifs_journaliers(ANNEE, MOIS, CA_HTVA, CA_TVAC))
  })
)

#### DB_COMPTA ####

only_na_cols <- IMPORT_COMPTES %>% slice(-1) %>%
  summarise(across(everything(), ~ all(is.na(.)))) %>%
  select(where(~ .)) %>% names()

DB_COMPTA_TEMP <- IMPORT_COMPTES %>% select(-any_of(only_na_cols))

liste_id_col_budget <- which(DB_COMPTA_TEMP %>% colnames() %>% str_detect("Business|Budget"))

DB_COMPTA_BASE <- DB_COMPTA_TEMP[,1:min(liste_id_col_budget)-1]
colnames(DB_COMPTA_BASE) <- c("CODE_COMPTE","LABEL_COMPTE","INDEX_PERIODE")
DB_COMPTA <- list()
for (id_col_budget in liste_id_col_budget){
  annee <- as.character(DB_COMPTA_TEMP[1,id_col_budget])
  TEMP <- DB_COMPTA_TEMP[,c(id_col_budget,id_col_budget+1)]
  colnames(TEMP) <- c("Budget","Realise")
  DB_COMPTA[[annee]] <- DB_COMPTA_BASE %>%
    mutate(ANNEE = annee) %>%
    add_column(TEMP)
}
DB_COMPTA <- DB_COMPTA %>% map_df(add_row) %>% filter(!is.na(Realise))

# Regarder ligne par ligne pour distinguer les Année/Trimestre/Mois

i_row_dep <- 0
i_row_fin <- 0
index <- 1
i_row <- 1
DB_COMPTA_FULL <- list()
while (i_row <= nrow(DB_COMPTA)){
# for (i_row in 1:100){
  if (i_row_dep == 0 & DB_COMPTA[i_row,"LABEL_COMPTE"] == "Chiffre d'affaires"){
    # Remonter d'une ligne
    i_row_dep <- i_row - 1
    i_row <- i_row + 1
  }
  if (i_row_dep != 0 & i_row_fin == 0 &
      DB_COMPTA[i_row,"LABEL_COMPTE"] == "Chiffre d'affaires"){
    # Remonter d'une ligne
    i_row_fin <- i_row - 2
  }
  if (i_row_dep != 0 & i_row == nrow(DB_COMPTA)){
    # Remonter d'une ligne
    i_row_fin <- i_row
  }
  if (i_row_dep != 0 && i_row_fin != 0){
    # Extraction
    TEMP <- DB_COMPTA[c(i_row_dep:i_row_fin),] %>%
      mutate(PERIODE = DB_COMPTA[i_row_dep,]$LABEL_COMPTE)
    if (!is.na(TEMP[1,]$INDEX_PERIODE)){
      TEMP$PERIODE <- "MOIS"
      mois <- month(TEMP[1,]$INDEX_PERIODE)
      periode <- ymd(paste(TEMP[1,]$ANNEE,mois,"01",sep="-"))
      TEMP$INDEX_PERIODE <- periode
      TEMP$MOIS <- mois
      TEMP$TRIM <- quarter(TEMP$INDEX_PERIODE)
    } else if (TEMP[1,]$LABEL_COMPTE == "ANNEE") {
      periode <- ymd(paste(TEMP[1,]$ANNEE,"01","01",sep="-"))
      TEMP$INDEX_PERIODE <- periode
      TEMP$MOIS <- NA
      TEMP$TRIM <- NA
    } else {
      mois <- 1 + (as.numeric(substr(TEMP[1,]$LABEL_COMPTE,2,3))-1)*3
      periode <- ymd(paste(TEMP[1,]$ANNEE,mois,"01",sep="-"))
      TEMP$INDEX_PERIODE <- periode
      TEMP$PERIODE <- "TRIMESTRE"
      TEMP$TRIM <- quarter(TEMP$INDEX_PERIODE)
      TEMP$MOIS <- NA
    }
    TEMP[1,]$LABEL_COMPTE <- "Résultat avant impôt"
    DB_COMPTA_FULL[[index]] <- TEMP
    index <- index + 1
    i_row_dep <- 0
    i_row_fin <- 0
    i_row <- i_row - 3
  }
  i_row <- i_row +1
}

DB_COMPTA_FULL <- DB_COMPTA_FULL %>% map_df(add_row) %>% filter(!is.na(LABEL_COMPTE))


DB_COMPTES_DETAIL <- IMPORT_COMPTES_DETAIL %>%
  mutate(mois = as.Date(mois),
         ANNEE = year(mois),
         TRIM = quarter(mois),
         MOIS = month(mois)) %>%
  rename(INDEX_PERIODE = mois,
         CODE_COMPTE = `Numéro de compte`,
         LABEL_COMPTE = `Libellé`,
         LABEL_COMPTE_SUM = Arborescence)

DB_COMPTES_DETAIL <- DB_COMPTES_DETAIL %>%
  select(-Catégorie) %>%
  arrange(CODE_COMPTE,LABEL_COMPTE,ANNEE,MOIS) %>%
  group_by(CODE_COMPTE,LABEL_COMPTE,ANNEE) %>%
  mutate(Realise = Solde - lag(Solde,default = 0)) %>%
  ungroup()

# Correction erreur Octobre
LISTE_MOIS_ERREUR <- DB_COMPTES_DETAIL %>%
  filter(LABEL_COMPTE_SUM == "Ressources humaines") %>%
  group_by(INDEX_PERIODE) %>%
  summarise(Realise = -sum(Realise),.groups = "drop") %>%
  filter(Realise > 150000) %>% pull(INDEX_PERIODE)

DB_COMPTES_DETAIL <- DB_COMPTES_DETAIL %>%
  filter(!INDEX_PERIODE %in% LISTE_MOIS_ERREUR)

DB_COMPTES_DETAIL <- DB_COMPTES_DETAIL %>%
  mutate(SECTEUR = case_when(
    CODE_COMPTE %in% c(604105,604205,609410) ~ "Brasserie",
    CODE_COMPTE %in% c(604110,604210,609405) ~ "Boulangerie",
    CODE_COMPTE %in% c(604115,604215,609420) ~ "Service",
    CODE_COMPTE %in% c(604120,604220,609415) ~ "Cuisine",
    LABEL_COMPTE_SUM == "Frais généraux" ~ "Support",
    TRUE ~ NA),
    STEP = case_when(
      CODE_COMPTE %in% c(604105,604205,604110,604210,604115,604215,604120,604220) ~ "Achats",
      CODE_COMPTE %in% c(609405,609420,609415) ~ "Stock",
      LABEL_COMPTE_SUM == "Frais généraux" ~ "Frais généraux",
      TRUE ~ NA)
  )

#### Coût des achats ####

COUT_ACHAT <- DB_COMPTES_DETAIL %>%
  filter(STEP == "Achats") %>%
  group_by(PREMIER_JOUR_MOIS=INDEX_PERIODE,SECTEUR) %>%
  summarise(Achat = -sum(Realise,na.rm=TRUE),.groups = "drop")

COUT_STOCK <- DB_COMPTES_DETAIL %>%
  filter(STEP == "Stock") %>%
  group_by(PREMIER_JOUR_MOIS=INDEX_PERIODE,SECTEUR) %>%
  summarise(Stock = -sum(Realise,na.rm=TRUE),.groups = "drop")

#### Frais généraux ####

COUT_GENERAUX <- DB_COMPTES_DETAIL %>%
  filter(STEP == "Frais généraux") %>%
  group_by(PREMIER_JOUR_MOIS=INDEX_PERIODE,SECTEUR) %>%
  summarise(Achat = -sum(Realise,na.rm=TRUE),.groups = "drop")

#### Frais de travail

COUT_TRAVAIL_COMPTA <- DB_COMPTES_DETAIL %>%
  filter(LABEL_COMPTE_SUM == "Ressources humaines") %>%
  group_by(PREMIER_JOUR_MOIS=INDEX_PERIODE,SECTEUR) %>%
  summarise(Achat = -sum(Realise,na.rm=TRUE),.groups = "drop")

COUT_TRAVAIL_COMPTA <- DB_COMPTES_DETAIL %>%
  filter(LABEL_COMPTE_SUM == "Ressources humaines") %>%
  mutate(Achat = -Realise)

#### DB_HEURES_NON_POINTES  ####

DB_HEURES_NON_POINTES <- IMPORT_HEURES_MANUELLES %>%
  rename(ANNEE_DEB = `Année Début`,
         ANNEE_FIN = `Année Fin`,
         SEMAINE_DEB = `Semaine début`,
         SEMAINE_FIN = `Semaine fin`,
         DATE_DEB = `Jour Min`,
         DATE_FIN = `Jour Max`,
         PERSONNE = Personne,
         SECTEUR = Secteur,
         NB_HEURES_SEMAINE = Heure,
         COUT_HORAIRE = Cout) %>%
  mutate(DATE_DEB = as.Date(DATE_DEB),
         DATE_FIN = as.Date(DATE_FIN)) %>%
  mutate(all_dates = map2(DATE_DEB, DATE_FIN, ~ seq.Date(.x, .y, by = "day"))) %>%
  unnest(cols = c(all_dates)) %>%
  rename(DATE = all_dates) %>%
  select(DATE,SECTEUR,PERSONNE,NB_HEURES_SEMAINE,COUT_HORAIRE) %>%
  mutate(NB_HEURES_JOUR = NB_HEURES_SEMAINE / 7,
         COUT_JOUR = NB_HEURES_JOUR*COUT_HORAIRE)

DB_HEURES_POINTES <- DB_HOREKO %>%
  select(-c(CA_HTVA,CA_TVAC,HEURES,HEURES_LISTEES,SALAIRES,METEO)) %>%
  pivot_longer( cols = -DATE,
    names_to = c("Type", "SECTEUR"),
    names_pattern = "^(.*?)_(.*?)$") %>%
  group_by(DATE,Type,SECTEUR) %>%
  summarise(value = sum(value,na.rm=TRUE),.groups = "drop") %>%
  pivot_wider(names_from = Type,values_from = value) %>%
  rename(
   COUT_JOUR = Cout,
   NB_HEURES_JOUR = Heures
  ) %>%
  mutate(PERSONNE = "Pointage",
         COUT_HORAIRE = COUT_JOUR/NB_HEURES_JOUR)

DB_HEURES <- DB_HEURES_NON_POINTES %>%
  add_row(DB_HEURES_POINTES)

# Coût du travail
COUT_TRAVAIL <- DB_HEURES %>%
  left_join(DB_DATE %>% select(DATE,PREMIER_JOUR_MOIS)) %>%
  group_by(PREMIER_JOUR_MOIS,SECTEUR) %>%
  summarise(HEURES = sum(NB_HEURES_JOUR,na.rm=TRUE),
            Travail = sum(COUT_JOUR,na.rm=TRUE),.groups = "drop")

DB_COUT_TOTAL <- COUT_ACHAT %>%
  add_row(COUT_GENERAUX) %>%
  full_join(COUT_STOCK) %>%
  full_join(COUT_TRAVAIL) %>%
  # select(-HEURES) %>%
  pivot_longer(cols = c(Achat,Stock,Travail),
               names_to = "TYPE_COUT",values_to = "COUT") %>%
  mutate(HEURES = replace_na(HEURES,0),
         COUT = replace_na(COUT,0),
         HEURES = ifelse(TYPE_COUT=="Travail",HEURES,NA))

#### DB_KPI_PART_CA ####

DB_KPI_NOURRITURE_CA <- DB_DATE %>%
  select(PREMIER_JOUR_MOIS) %>%
  pull %>% unique %>% map_df(~{
    kpi <- DB_DATE %>%
      # left_join(DB_KPI) %>%
      left_join(DB_KPI_SIMPLE %>%
                  select(DATE,Boisson,Nourriture) %>%
                  pivot_longer(-DATE,names_to = "CD_SECTEUR",
                               values_to = "CA_HTVA")) %>%
      filter(PREMIER_JOUR_MOIS == .x,CD_SECTEUR == "Nourriture")
    DB <- DB_COUT_TOTAL %>%
      filter(PREMIER_JOUR_MOIS == .x,SECTEUR %in% c("Cuisine","Boulangerie"))
    kpi_cout(kpi,DB)
  })

DB_KPI_BOISSON_CA <- DB_DATE %>%
  select(PREMIER_JOUR_MOIS) %>%
  pull %>% unique %>% map_df(~{
    kpi <- DB_DATE %>%
      # left_join(DB_KPI) %>%
      left_join(DB_KPI_SIMPLE %>%
                  select(DATE,Boisson,Nourriture) %>%
                  pivot_longer(-DATE,names_to = "CD_SECTEUR",
                               values_to = "CA_HTVA")) %>%
      filter(PREMIER_JOUR_MOIS == .x,CD_SECTEUR == "Boisson")
    DB <- DB_COUT_TOTAL %>%
      filter(PREMIER_JOUR_MOIS == .x,SECTEUR %in% c("Service","Brasserie"))
    kpi_cout(kpi,DB)
  })

DB_KPI_TOTAL_CA <- DB_DATE %>%
  select(PREMIER_JOUR_MOIS) %>%
  pull %>% unique %>% map_df(~{
    kpi <- DB_DATE %>%
      # left_join(DB_KPI) %>%
      left_join(DB_KPI_SIMPLE %>%
                  select(DATE,Boisson,Nourriture) %>%
                  pivot_longer(-DATE,names_to = "CD_SECTEUR",
                               values_to = "CA_HTVA")) %>%
      filter(PREMIER_JOUR_MOIS == .x)
    DB <- DB_COUT_TOTAL %>%
      filter(PREMIER_JOUR_MOIS == .x)
    kpi_cout_total(kpi,DB)
  })

# db_kpi <- DB_DATE %>% left_join(DB_KPI) %>% filter(PREMIER_JOUR_MOIS == "2024-09-01")
# db_cout_total <- DB_COUT_TOTAL %>% filter(PREMIER_JOUR_MOIS == "2024-09-01")


#### DB_BRASSINS ####

# Bug importation
# Fonction pour convertir les valeurs numériques au format Excel en dates
convert_excel_date <- function(var) {
  out <- unlist(sapply(var, function(x) {
    if (grepl("/", x)) {
      return(as.Date(x, format = "%d/%m/%Y"))
    } else {
      return(as.Date(as.numeric(x), origin = "1899-12-30"))
    }
  }))

  as.Date(unname(out), origin = "1970-01-01")
}

# dates_converted <- convert_excel_date(IMPORT_BRASSINS$`Date de fin de bière`)
#
# # Conversion des valeurs
# dates_converted <- sapply(IMPORT_BRASSINS$`Date de fin de bière`, function(x) {
#   if (is.na(x)) {
#     return(NA) # Si la valeur est NA, on la laisse telle quelle
#   } else if (grepl("^\\d+\\.?\\d*$", x)) {
#     return(convert_excel_date(x)) # Si c'est une valeur numérique, on la convertit
#   } else {
#     return(as.Date(x, format = "%d/%m/%Y")) # Si c'est une chaîne de caractères, on la convertit en date
#   }
# })
#
# dates_converted <- as.Date(dates_converted, origin = "1970-01-01") # Transformer le résultat en dates R



# Une ligne par brassin
DB_BRASSINS <- IMPORT_BRASSINS %>%
  filter(!is.na(`Numéro de brassin`) & !is.na(`Nom commercial`)) %>%
  select(`Numéro de brassin`,`Nom commercial`,`Date de brassage`,
         `Date de conditionnement`, `Volume ST (hL)`,`Volume Fûts (hL)`,
         `Date de fin de bière`,`Date de déclaration`,`Bière finie?`) %>%
  left_join(IMPORT_BIERES_CORRESPONDANCE) %>%
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
  ) %>%
  mutate(
    ID_BRASSIN = gsub("\\.0","",ID_BRASSIN),
    # DT_BRASSIN = ymd(DT_BRASSIN),
    # DT_CONDI = ymd(DT_CONDI),
    # DT_FIN = ymd(DT_FIN),
    # DT_DECLA = ymd(DT_DECLA),
    DT_BRASSIN = as.Date(DT_BRASSIN),
    DT_CONDI = as.Date(DT_CONDI),
    DT_FIN = as.Date(DT_FIN),
    DT_DECLA = as.Date(DT_DECLA),
    VOLUME_BRASSIN = VOLUME_BRASSIN * 100,
    VOLUME_BRASSIN_ADD = VOLUME_BRASSIN_ADD * 100,
    VOLUME_BRASSIN = VOLUME_BRASSIN + VOLUME_BRASSIN_ADD
  ) %>%
  mutate(DT_FIN = if_else(is.na(DT_FIN),DT_DECLA,DT_FIN)) %>%
  mutate(DT_FIN = if_else(is.na(DT_FIN),ymd(today()),DT_FIN))

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
DIFF_VOLUME <- DB_PRODUITS %>%
  filter(FL_FINI & VOLUME_BRASSIN > 400) %>%
  group_by(BOISSON,ID_BRASSIN,VOLUME_BRASSIN) %>%
  summarise(VOLUME_VENDU = sum(VOLUME_TOT_L),.groups = "drop") %>%
  mutate(DIFF = round(VOLUME_VENDU/VOLUME_BRASSIN,4)) %>%
  pull(DIFF)

# Attention, pour certaines bières, cette stat va provoquer un volume négatif
DB_PRODUITS <- DB_PRODUITS %>%
  mutate(VOLUME_BRASSIN_AJUST = VOLUME_BRASSIN*0.75)
# group_by(ID_BRASSIN) %>%
# mutate(VOLUME_BRASSIN_AJUST = VOLUME_BRASSIN*median(DIFF_VOLUME),
#        VOLUME_BRASSIN_AJUST = ifelse(sum(VOLUME_TOT_L) > VOLUME_BRASSIN_AJUST,
#                                      VOLUME_BRASSIN,VOLUME_BRASSIN_AJUST)) %>%
# ungroup()


PRIX_BIERES <- DB_PRODUITS %>%
  filter(CATEGORY ==  "BIÈRES" & !is.na(ID_BRASSIN) & VOLUME_CL == 33) %>%
  select(PRICE_33CL = PRICE,BOISSON) %>%
  distinct() %>%
  group_by(BOISSON) %>%
  arrange(-PRICE_33CL) %>%
  filter(row_number() == 1)

# Ajout des stats sur les ventes
DB_BIERES <- DB_PRODUITS %>%
  filter(CATEGORY ==  "BIÈRES" & !is.na(ID_BRASSIN)) %>%
  left_join(PRIX_BIERES) %>%
  right_join(DB_DATE) %>%
  group_by(CATEGORY,BOISSON,ID_BRASSIN,FL_FINI,DATE,
           VOLUME_BRASSIN,VOLUME_BRASSIN_AJUST,PRICE_33CL) %>%
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
  filter(!FL_FINI) %>%
  arrange(DATE) %>%
  group_by(ID_BRASSIN) %>% filter(row_number() == n()) %>% ungroup() %>%
  filter(today()-DATE > 10) %>% pull(ID_BRASSIN)

DB_BIERES[DB_BIERES$ID_BRASSIN %in% vec_brassin_bug,"FL_FINI"] <- TRUE
DB_BRASSINS[DB_BRASSINS$ID_BRASSIN %in% vec_brassin_bug,"FL_FINI"] <- TRUE

# Ajout des dates (uniquement la date, pas le reste)

# DB_JOURS <- DB_DATE %>% select(DATE) %>%
#   mutate_if(is.numeric,replace_na,0)
#   mutate_if(is.numeric & is.na,coalesce,0)

# DB_JOURS <- DB_JOURS %>%
#   mutate(CA_HTVA = if_else(is.na(CA_HTVA),0,CA_HTVA),
#          CA_TVAC = if_else(is.na(CA_TVAC),0,CA_TVAC))

#### Nettoyage ####


df_tailles <- data.frame(
  taille_MB = sapply(ls(), function(x) {
    as.numeric(object.size(get(x))) / (1024^2)
  }),
  stringsAsFactors = FALSE
)

df_tailles %>% arrange(-taille_MB)

rm(IMPORT_BIERES_CORRESPONDANCE, IMPORT_BRASSINS, IMPORT_CAISSE,
   IMPORT_COMPTES, IMPORT_DB_JOURS, IMPORT_HOREKO, IMPORT_LIGHTSPEED,
   IMPORT_OBJECTIFS, IMPORT_OLD, IMPORT_TICKET,
   DB_sheets,DB_TICKET_PRODUITS)


