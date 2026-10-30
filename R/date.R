# Le calendrier de référence.
#
# Une table d'une ligne par jour, qui porte le jour de semaine, le mois et
# l'année. Tout ce qui agrège par période s'y joint plutôt que de recalculer
# un format(), parce qu'un jour sans vente doit exister dans la série — sinon
# une fermeture se lit comme un trou de données.

correct_date <- function(x){
  if (class(x)[1] == "character") 
    janitor::excel_numeric_to_date(as.numeric(x))
  else
    x
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

