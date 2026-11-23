# Le calendrier de référence : une ligne par jour. Tout ce qui agrège par
# période s'y joint, pour qu'un jour sans vente existe dans la série.

#### Lire une date, d'où qu'elle vienne ####

# LE parseur de dates du projet : tout ce qui entre par un classeur passe par
# lui.
#
# Selon la source, une même colonne arrive en POSIXct, en Date, en numéro de
# série Excel ou en texte — et une seule cellule atypique, ou un rbind() avec
# une archive, fait basculer toute la colonne en texte. Ce parseur accepte
# toutes ces formes. Un format inconnu rend NA plutôt qu'une date inventée.
parse_date_souple <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))  # série Excel

  txt <- trimws(as.character(x))
  txt[txt == ""] <- NA_character_

  # Un POSIXct passé en texte garde son heure ; as.Date() avec un format
  # ignore la fin, c'est voulu.
  d <- as.Date(txt, format = "%Y-%m-%d")
  manquants <- is.na(d) & !is.na(txt)
  if (any(manquants))
    d[manquants] <- as.Date(txt[manquants], format = "%d/%m/%Y")
  # Un numéro de série Excel passé en texte : « 46282 ».
  manquants <- is.na(d) & !is.na(txt) & grepl("^[0-9]+([.][0-9]+)?$", txt)
  if (any(manquants))
    d[manquants] <- as.Date(as.numeric(txt[manquants]), origin = "1899-12-30")
  d
}

creer_db_date <- function(date_min = ymd("2023-01-01"), 
                          date_max = ceiling_date(today(), "year")-1){
  tibble(
    DATE = list(seq.Date(date_min, date_max, by= "1 day"))) |> 
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
}

prepa_db <- function(DB,var_tva){
  creer_db_date(date_min = min(DB$DATE)) %>%
    left_join(DB, by = "DATE") %>%
    mutate_if(is.numeric,replace_na,0) %>%
    mutate_if(is.character,replace_na,"") %>%
    rename(ventes = var_tva)
}

