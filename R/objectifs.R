# Couleur d'un CA selon l'atteinte de son objectif. Même convention que les
# box de ventes (cf. get_color_from_gradient) :
#   >= 100 %  -> vert    (objectif atteint)
#   >=  90 %  -> ambre   (tout proche)
#   <   90 %  -> rouge   (manqué)
# Sans objectif renseigné (0 ou NA), la barre reste neutre : on ne peut rien
# juger. Vectorisé, donc utilisable directement sur une colonne.
couleur_objectif <- function(reel, objectif, seuil_proche = 0.9) {
  pct <- ifelse(is.na(objectif) | objectif <= 0, NA_real_, reel / objectif)
  case_when(
    is.na(pct)          ~ COUL_NEUTRE,
    pct >= 1            ~ COUL_VERT,
    pct >= seuil_proche ~ COUL_AMBRE,
    TRUE                ~ COUL_ROUGE
  )
}

# Libellé "x % de l'objectif" pour les infobulles.
label_objectif <- function(reel, objectif) {
  ifelse(is.na(objectif) | objectif <= 0, "pas d'objectif",
         paste0(round(100 * reel / objectif), " % de l'objectif"))
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
  
  INFO_OBJECTIFS <- tibble(JOUR_SEMAINE = 1:7,
                           OBJECTIF_PCT = c(0,0.065,0.1,0.125,0.21,0.29,0.21))
  
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