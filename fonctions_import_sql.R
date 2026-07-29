vecteur_jours <- c("lundi","mardi","mercredi",
                   "jeudi","vendredi","samedi",
                   "dimanche")

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

d1_select <- function(sql) {
  r <- httr::POST(
    sprintf("https://api.cloudflare.com/client/v4/accounts/%s/d1/database/%s/query",
            Sys.getenv("CF_ACCOUNT_ID"), Sys.getenv("CF_D1_ID")),
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("CF_API_TOKEN"))),
    body = list(sql = sql), encode = "json"
  )
  httr::stop_for_status(r)
  dplyr::bind_rows(httr::content(r)$result[[1]]$results)
}