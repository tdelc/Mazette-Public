# Le calendrier de référence.
#
# Une table d'une ligne par jour, qui porte le jour de semaine, le mois et
# l'année. Tout ce qui agrège par période s'y joint plutôt que de recalculer
# un format(), parce qu'un jour sans vente doit exister dans la série — sinon
# une fermeture se lit comme un trou de données.

#### Lire une date, d'où qu'elle vienne ####

# LE parseur de dates du projet. Tout ce qui entre par un classeur passe par
# lui, et rien ne doit supposer le type de ce qui arrive.
#
# Pourquoi : un Google Sheet, un export Excel et une archive .RData ne rendent
# pas la même chose pour la même colonne. Selon le jour, DATEVALUE revient en
# POSIXct, en Date, en numéro de série Excel, ou en texte — il suffit qu'une
# cellule de la colonne contienne autre chose qu'une date, ou qu'un rbind()
# empile une archive dont le type diffère, pour que toute la colonne bascule
# en caractère.
#
# L'idiome qui traînait partout dans import.R, `ymd(format(x, "%Y-%m-%d"))`,
# ne survivait pas à cette bascule : sur un vecteur de texte, format() prend
# son deuxième argument pour `trim` et tombe sur « argument 'trim' incorrect »
# — un message qui ne nomme ni la colonne, ni la table, ni la cause, et qui
# arrête tout l'import. C'est ce défaut-là qui a motivé ce parseur.
#
# Un format inconnu rend NA plutôt qu'une date inventée : la ligne sera
# écartée, et l'import dit combien il en a écarté.
parse_date_souple <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))  # série Excel

  txt <- trimws(as.character(x))
  txt[txt == ""] <- NA_character_

  # Un POSIXct passé en texte par un rbind() garde son heure : « 2026-01-01
  # 00:00:00 ». as.Date() avec un format ignore ce qui suit, c'est voulu.
  d <- as.Date(txt, format = "%Y-%m-%d")
  manquants <- is.na(d) & !is.na(txt)
  if (any(manquants))
    d[manquants] <- as.Date(txt[manquants], format = "%d/%m/%Y")
  # Un numéro de série passé en texte, lui, n'est plus qu'une suite de
  # chiffres : sans cette branche, « 46282 » ne serait plus une date du tout.
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

