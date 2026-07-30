library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(janitor)
library(googledrive)
library(googlesheets4)
library(readxl)
library(scales)
library(plotly)
library(forecast)
library(DT)
library(zoo)
library(patchwork)

source("R/connect.R")
source("R/sql.R")
source("R/date.R")
source("R/objectifs.R")


##### Date #####

date_debut_semaine    <- floor_date(today() - 2, unit = "week") + 1
vecteur_jours <- c("lundi","mardi","mercredi",
                   "jeudi","vendredi","samedi",
                   "dimanche")

##### Conventions de couleurs ####

# Palette d'appréciation, partagée par tous les volets de la refonte.

COUL_VERT   <- "#5B7B5A"
COUL_AMBRE  <- "#d98236"
COUL_ROUGE  <- "#c0392b"
COUL_NEUTRE <- "#8d7b68"

source("donnees_fictives_compta.R")


from_product_to_boisson <- function(DB){
  DB %>%
    mutate(PRODUCT_VIDE = str_remove(PRODUCT," *[0-9]+ *[cC][lL]"),
           PRODUCT_VIDE = str_remove(PRODUCT_VIDE," verre"),
           PRODUCT_VIDE = str_remove(PRODUCT_VIDE," 1L"),
           VOLUME_CL = case_when(
             PRODUCT %in% c("Pépin blanc verre",
                            "Pépin rouge verre",
                            "Hurluberlu rouge verre") ~ 12.5,
             PRODUCT %in% c("Cidre Rhuys","Kefir") ~ 25,
             PRODUCT %in% c("Rhum Brussels") ~ 3,
             str_detect(PRODUCT,"1L") ~ 100,
             TRUE ~ as.numeric(str_extract(PRODUCT," *([0-9]+) *[cC]*[lL]",group= 1))
           ),
           BOISSON = case_when(
             is.na(VOLUME_CL) ~ "",
             TRUE ~ PRODUCT_VIDE
           )
    ) %>%
    rename(PRODUCT_FULL = PRODUCT,
           PRODUCT = PRODUCT_VIDE)
}


#### REFONTE — Volet "Maintenant" ####

# Tronque un nom de produit trop long
tronque_nom <- function(x, max = 40) {
  ifelse(nchar(x) > max, paste0(substr(x, 1, max), "…"), x)
}

# Produit "bière" = catégorie contenant BIÈRE (rotation constante -> exclu des comparaisons)
est_biere <- function(category) {
  str_detect(toupper(replace_na(category, "")), "BI[EÈ]RE")
}

# Top produits (CA HTVA) sur une période [date_debut, date_fin]
top_produits_periode <- function(db_produits, date_debut, date_fin, n = 10) {
  db_produits %>%
    filter(DATE >= date_debut, DATE <= date_fin) %>%
    group_by(PRODUIT) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(CA)) %>%
    slice_head(n = n) %>%
    transmute(Produit = tronque_nom(PRODUIT),
              Quantité = Quantite,
              `CA HTVA` = format_CA(CA, -1))
}

# Évolution des produits (hors bières) : semaine en cours vs semaine précédente
evolution_produits_semaine <- function(db_produits, date_debut_semaine, n = 10,
                                       sens = c("hausse", "baisse")) {
  sens <- match.arg(sens)

  agrege <- function(d1, d2) {
    db_produits %>%
      filter(DATE >= d1, DATE <= d2, !est_biere(CATEGORIE)) %>%
      group_by(PRODUIT) %>%
      summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE), .groups = "drop")
  }

  sem    <- agrege(date_debut_semaine,     date_debut_semaine + 6)
  sem_m1 <- agrege(date_debut_semaine - 7, date_debut_semaine - 1) %>%
    rename(QUANTITE_m1 = QUANTITE)

  evo <- inner_join(sem, sem_m1, by = "PRODUIT") %>%
    mutate(delta = QUANTITE - QUANTITE_m1)

  evo <- if (sens == "hausse") arrange(evo, desc(delta)) else arrange(evo, delta)

  evo %>%
    slice_head(n = n) %>%
    transmute(Produit = tronque_nom(PRODUIT),
              `Cette sem.` = QUANTITE,
              `Sem. -1` = QUANTITE_m1,
              `Δ` = delta)
}

# Cumul jour à jour du CA réalisé vs objectif sur un mois donné
progression_mois <- function(db_kpi, db_obj, mois = floor_date(today(), "month")) {
  fin <- ceiling_date(mois, "month") - 1
  
  reel <- db_kpi %>% filter(DATE >= mois, DATE <= fin) %>% transmute(DATE, ventes)
  obj  <- db_obj %>% filter(DATE >= mois, DATE <= fin) %>% transmute(DATE, objectif = ventes)
  
  full_join(reel, obj, by = "DATE") %>%
    arrange(DATE) %>%
    mutate(ventes   = replace_na(ventes, 0),
           objectif = replace_na(objectif, 0),
           cum_reel = cumsum(ventes),
           cum_obj  = cumsum(objectif),
           # on n'affiche pas le cumulé réalisé pour les jours pas encore passés
           cum_reel = ifelse(DATE > today(), NA, cum_reel))
}

# Graphe de progression mensuelle : objectif cumulé (pointillé) + réalisé cumulé (aire)
graph_progression_mois <- function(prog, mois = floor_date(today(), "month")) {
  plot_ly(prog) %>%
    add_lines(x = ~DATE, y = ~cum_obj, name = "Objectif cumulé",
              line = list(color = "#260b01", dash = "dot", width = 1.5),
              hovertemplate = ~paste0(format(DATE, "%d/%m"), "<br>Objectif ",
                                      format_CA(cum_obj, -1), "<extra></extra>")) %>%
    add_lines(x = ~DATE, y = ~cum_reel, name = "Réalisé cumulé",
              line = list(color = "#732c02", width = 3),
              fill = "tozeroy", fillcolor = "rgba(115,44,2,0.10)",
              connectgaps = FALSE,
              hovertemplate = ~paste0(format(DATE, "%d/%m"), "<br>Réalisé ",
                                      format_CA(cum_reel, -1), "<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA cumulé (€)"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}


#### REFONTE — Volet "Détail" ####

# Graphe des CA journaliers (barres cliquables) sur une période
graph_ca_jour <- function(db_kpi, db_obj, d1, d2, source = "detail_jour") {
  dat <- db_kpi %>%
    select(DATE, ventes) %>%
    left_join(db_obj %>% select(DATE, objectif = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2, ventes > 0) %>%
    arrange(DATE)

  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)

  plot_ly(dat, source = source) %>%
    add_bars(x = ~DATE, y = ~ventes, name = "CA",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(format(DATE, "%a %d/%m"), "<br>CA ",
                                     format_CA(ventes, -1), "<br>", atteinte,
                                     "<extra></extra>")) %>%
    add_lines(x = ~DATE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1)) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Graphe du CA agrégé par semaine ou par mois (barres cliquables).
# Même logique que graph_ca_jour, mais à la maille supérieure : sert au
# drill-down "Par semaine" / "Par mois" de l'onglet Détail.
graph_ca_periode <- function(db_kpi, db_obj, d1, d2,
                             unite = c("semaine", "mois"),
                             source = "detail_semaine") {
  unite <- match.arg(unite)

  dat <- db_kpi %>%
    select(DATE, ventes) %>%
    left_join(db_obj %>% select(DATE, objectif = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2) %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(ventes = sum(ventes, na.rm = TRUE),
              objectif = sum(objectif, na.rm = TRUE), .groups = "drop") %>%
    filter(ventes > 0) %>%
    arrange(PERIODE)

  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée sur la période"))

  lbl <- label_periode(dat$PERIODE, unite)
  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)

  plot_ly(dat, source = source) %>%
    add_bars(x = ~PERIODE, y = ~ventes, name = "CA",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(lbl, "<br>CA ", format_CA(ventes, -1),
                                     "<br>", atteinte, "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1)) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Répartition du CA à l'intérieur d'une période (jours d'une semaine, ou
# semaines d'un mois) -> contexte du drill-down.
graph_repartition_periode <- function(db_kpi, db_obj, periode,
                                      unite = c("semaine", "mois")) {
  unite <- match.arg(unite)
  d1 <- as.Date(periode)
  d2 <- fin_periode(d1, unite)

  dat <- db_kpi %>%
    select(DATE, ventes) %>%
    left_join(db_obj %>% select(DATE, objectif = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2) %>%
    arrange(DATE)

  if (nrow(dat) == 0 || sum(dat$ventes, na.rm = TRUE) == 0)
    return(plotly_empty() %>% layout(title = "Aucune vente sur la période"))

  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)

  plot_ly(dat) %>%
    add_bars(x = ~DATE, y = ~ventes, name = "CA",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(format(DATE, "%a %d/%m"), "<br>CA ",
                                     format_CA(ventes, -1), "<br>", atteinte,
                                     "<extra></extra>")) %>%
    add_lines(x = ~DATE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1)) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Liste des produits (CA, quantité) sur une période -> table sélectionnable
liste_produits_periode <- function(db_produits, d1, d2) {
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(Produit = PRODUIT) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(CA))
}

# Évolution hebdomadaire d'un produit
evolution_un_produit <- function(db_produits, produit, d1, d2) {
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) |> 
    mutate(SEMAINE = floor_date(DATE, unit = "week", week_start = 1)) %>%
    group_by(SEMAINE) %>%
    mutate(CA_TOT = sum(CA_HTVA, na.rm = TRUE)) |> 
    group_by(SEMAINE,CATEGORIE) %>%
    mutate(CA_CATEGORIE = sum(CA_HTVA, na.rm = TRUE)) |> 
    filter(PRODUIT == produit) %>%
    group_by(SEMAINE,CATEGORIE) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), 
              PC_ALL = CA / mean(CA_TOT, na.rm = TRUE), 
              PC_CATEGORIE = CA / mean(CA_CATEGORIE, na.rm = TRUE), 
              .groups = "drop") %>%
    arrange(SEMAINE)
}

# Graphe d'évolution d'un produit (CA en barres + quantité en ligne)
graph_evolution_produit <- function(evo, produit) {
  plot_ly(evo, source = "detail_produit") %>%
    add_bars(x = ~SEMAINE, y = ~CA, name = "CA (€)",
             marker = list(color = "#732c02"),
             hovertemplate = ~paste0("Semaine du ", format(SEMAINE, "%d/%m"),
                                     "<br>CA ", format_CA(CA, -1), "<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~Quantite, name = "Quantité", yaxis = "y2",
              line = list(color = "#d98236", width = 2),
              hovertemplate = ~paste0(Quantite, " vendus<extra></extra>")) %>%
    layout(yaxis = list(title = "CA (€)"),
           yaxis2 = list(title = "Quantité", overlaying = "y", side = "right",
                         showgrid = FALSE),
           xaxis = list(title = ""), legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}


#### REFONTE — Volet "Historique" ####

# Agrège le CA réalisé + objectif par semaine ou par mois.
# (db = sortie de prepa_db : colonnes DATE, ventes, PREMIER_JOUR_SEMAINE, PREMIER_JOUR_MOIS)
agrege_historique <- function(db_kpi, db_obj, unite = c("semaine", "mois"),
                              exclure_courant = TRUE) {
  unite <- match.arg(unite)
  col <- if (unite == "semaine") "PREMIER_JOUR_SEMAINE" else "PREMIER_JOUR_MOIS"

  reel <- db_kpi %>%
    group_by(PERIODE = .data[[col]]) %>%
    summarise(ventes = sum(ventes, na.rm = TRUE), .groups = "drop")
  obj <- db_obj %>%
    group_by(PERIODE = .data[[col]]) %>%
    summarise(objectif = sum(ventes, na.rm = TRUE), .groups = "drop")

  res <- left_join(reel, obj, by = "PERIODE") %>% arrange(PERIODE)

  if (exclure_courant) {
    courant <- if (unite == "semaine") floor_date(today(), "week", week_start = 1)
               else floor_date(today(), "month")
    res <- res %>% filter(PERIODE < courant)
  }
  res
}

# Graphe historique : barres (CA réalisé, coloré selon l'atteinte de
# l'objectif) + ligne objectif
graph_historique <- function(db_kpi, db_obj, unite = c("semaine", "mois"), n = 12) {
  unite <- match.arg(unite)
  dat <- agrege_historique(db_kpi, db_obj, unite) %>%
    filter(ventes > 0)
  # %>%  slice_tail(n = n)

  lbl <- if (unite == "semaine") paste0("Sem. du ", format(dat$PERIODE, "%d/%m/%Y"))
         else format(dat$PERIODE, "%B %Y")
  couleurs <- couleur_objectif(dat$ventes, dat$objectif)
  atteinte <- label_objectif(dat$ventes, dat$objectif)

  plot_ly(dat) %>%
    add_bars(x = ~PERIODE, y = ~ventes, name = "CA réalisé",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(lbl, "<br>CA ", format_CA(ventes, -1),
                                     "<br>", atteinte, "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~objectif, name = "Objectif",
              line = list(color = "#260b01", dash = "dot", width = 1.5),
              hovertemplate = ~paste0("Objectif ", format_CA(objectif, -1), "<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

graph_historique_tendance <- function(db_kpi, db_obj, unite = c("semaine", "mois"), n = 12) {
  
  unite <- match.arg(unite)
  dat <- agrege_historique(db_kpi, db_obj, unite) %>%
    filter(ventes > 0)
  
  dat$ma <- forecast::ma(dat$ventes,5)
  
  lbl <- if (unite == "semaine") paste0("Sem. du ", format(dat$PERIODE, "%d/%m/%Y"))
  else format(dat$PERIODE, "%B %Y")
  # (pas de couleur par objectif ici : ce graphe trace des lignes, pas des barres)

  plot_ly(dat) %>%
    add_lines(x = ~PERIODE, y = ~ventes, name = "CA réalisé",
              line = list(color = "#d98236"),
             hovertemplate = ~paste0(lbl, "<br>CA ", format_CA(ventes, -1), "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~ma, name = "",
              line = list(color = "#5B7BAA")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
           bargap = 0.3, legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  
  # p <- dat %>% 
  #   ggplot() +
  #   aes(x = PERIODE, y = ventes) +
  #   geom_line()+
  #   scale_x_date(breaks = "years")+
  #   labs(x = "", y = "CA (€)")+
  #   geom_smooth(method = "loess",formula = 'y ~ x')
  # 
  # ggplotly(p) %>% 
  #   layout(xaxis = list(title = ""), yaxis = list(title = "CA (€)"),
  #          bargap = 0.3, legend = list(orientation = "h"),
  #          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}


#### REFONTE — Volet "Bières" ####

# Niveau actuel de chaque bière en cours (dernière mesure connue)
niveau_bieres_actuel <- function(db_bieres,max_date = today()) {
  db_bieres %>%
    filter(!BIERE_FINIE, DATE <= max_date, DATE >= max_date - 30) %>%
    group_by(ID_BRASSIN, BOISSON) %>%
    arrange(DATE) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    transmute(ID_BRASSIN, BOISSON,
              VOLUME_TOTAL = VOLUME_BRASSIN_AJUST,
              VOLUME_RESTANT = pmax(0, DIFF),
              PCT = ifelse(VOLUME_BRASSIN_AJUST > 0,
                           round(100 * pmax(0, DIFF) / VOLUME_BRASSIN_AJUST), 0)) %>%
    arrange(PCT)
}

# Date de fin prévue par brassin (tôt / estimée / tard), à partir de la table
# de prédiction HoltWinters. Une ligne par ID_BRASSIN.
predictions_par_brassin <- function(db_predict) {
  vide <- tibble(ID_BRASSIN = character(), FIN_TOT = as.Date(character()),
                 FIN_EST = as.Date(character()), FIN_TARD = as.Date(character()))
  if (is.null(db_predict) || nrow(db_predict) == 0) return(vide)

  as_date <- function(x) as.Date(x, origin = "1970-01-01")
  unique(db_predict$ID_BRASSIN) %>%
    map_df(function(id) {
      fin <- predict_fin_brassin(db_predict, id)
      tibble(ID_BRASSIN = id, FIN_TOT = as_date(fin[1]),
             FIN_EST = as_date(fin[2]), FIN_TARD = as_date(fin[3]))
    })
}

# Libellé + couleur de l'échéance d'un fût, selon le nombre de jours restants.
# Sert de code couleur d'urgence sous la jauge.
etiquette_fin_fut <- function(fin_est, aujourdhui = today()) {
  if (is.null(fin_est) || length(fin_est) == 0 || is.na(fin_est))
    return(list(texte = "fin non prévisible", couleur = "#8d7b68"))

  jours <- as.numeric(as.Date(fin_est) - aujourdhui)
  # Une échéance déjà passée alors que le fût est toujours ouvert signale un
  # retard de saisie ou une consommation plus lente que prévu : on le dit,
  # plutôt que d'annoncer « aujourd'hui ».
  quand <- if (jours < 0) "échéance dépassée"
           else if (jours == 0) "aujourd'hui"
           else if (jours == 1) "demain"
           else paste0("dans ", jours, " j")
  couleur <- if (jours <= 3) COUL_ROUGE
             else if (jours <= 7) COUL_AMBRE
             else COUL_VERT
  list(texte = paste0("fin ~ ", format(as.Date(fin_est), "%a %d/%m"),
                      " (", quand, ")"),
       couleur = couleur)
}

# Cartes de niveau des fûts : une par bière en cours, avec sa jauge, son
# volume restant et — si la table de prédiction est fournie — sa date de fin
# prévue. On voit ainsi d'un coup d'œil la bière, son niveau et son échéance.
#
# La jauge est dessinée en CSS (dégradé conique) plutôt qu'en plotly : la
# grille plotly impose un nombre de colonnes fixe au moment du rendu, si bien
# que sur un téléphone les jauges se retrouvaient réduites à quelques dizaines
# de pixels. Ici, c'est la grille CSS qui décide du nombre de colonnes selon
# la largeur réellement disponible.
cartes_niveaux_bieres <- function(niveaux, db_predict = NULL) {
  if (is.null(niveaux) || nrow(niveaux) == 0)
    return(div(class = "text-muted small", "Aucune bière en cours."))

  if (!is.null(db_predict) && nrow(db_predict) > 0)
    niveaux <- niveaux %>%
      left_join(predictions_par_brassin(db_predict), by = "ID_BRASSIN")
  if (!"FIN_EST" %in% names(niveaux)) niveaux$FIN_EST <- as.Date(NA)

  carte <- function(i) {
    pct <- max(0, min(100, round(niveaux$PCT[i])))
    couleur <- if (pct < 20) COUL_ROUGE else if (pct < 40) COUL_AMBRE else COUL_VERT
    ech <- etiquette_fin_fut(niveaux$FIN_EST[i])

    div(
      class = "fut-carte",
      # Les deux variables CSS pilotent le dégradé conique de la jauge
      div(
        class = "fut-jauge",
        style = paste0("--pct:", pct, ";--coul:", couleur, ";"),
        div(class = "fut-jauge-trou",
            span(class = "fut-pct", style = paste0("color:", couleur, ";"),
                 paste0(pct, " %")))
      ),
      div(
        class = "fut-infos",
        div(class = "fut-nom", niveaux$BOISSON[i]),
        div(class = "fut-vol",
            round(niveaux$VOLUME_RESTANT[i]), " / ",
            round(niveaux$VOLUME_TOTAL[i]), " L"),
        div(class = "fut-fin", style = paste0("color:", ech$couleur, ";"),
            ech$texte)
      )
    )
  }

  div(class = "fut-grid", lapply(seq_len(nrow(niveaux)), carte))
}

# Évolution + prédiction du volume restant (version plotly de graph_evo_brassin)
graph_evo_brassin_plotly <- function(db, max_affichage = today() %m+% months(1)) {
  if (is.null(db) || nrow(db) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière en cours"))

  # On n'affiche la prédiction que jusqu'à un mois après aujourd'hui (au-delà,
  # l'extrapolation HoltWinters n'apporte rien et écrase l'échelle).
  db <- db %>% filter(DATE <= max_affichage)
  if (nrow(db) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière en cours"))

  bieres <- unique(db$BOISSON)
  pal <- setNames(
    grDevices::colorRampPalette(
      c("#732c02", "#d98236", "#5B7B5A", "#2980b9", "#9b59b6"))(length(bieres)),
    bieres)

  plt <- plot_ly()
  for (b in bieres) {
    sub  <- db %>% filter(BOISSON == b) %>% arrange(DATE)
    act  <- sub %>% filter(!FL_PREDICT)
    pred <- sub %>% filter(FL_PREDICT)
    couleur <- pal[[b]]

    plt <- plt %>% add_lines(
      data = act, x = ~DATE, y = ~VOLUME_RESTANT,
      name = b, legendgroup = b, line = list(color = couleur, width = 2.5),
      hovertemplate = ~paste0(BOISSON, "<br>", format(DATE, "%d/%m"), "<br>",
                              round(VOLUME_RESTANT), " L<extra></extra>"))

    if (nrow(pred) > 0) {
      pred2 <- bind_rows(slice_tail(act, n = 1), pred)
      plt <- plt %>% add_lines(
        data = pred2, x = ~DATE, y = ~VOLUME_RESTANT,
        name = b, legendgroup = b, showlegend = FALSE,
        line = list(color = couleur, width = 2, dash = "dot"),
        hovertemplate = ~paste0(BOISSON, " (prév.)<br>", format(DATE, "%d/%m"), "<br>",
                                round(VOLUME_RESTANT), " L<extra></extra>"))
    }
  }
  plt %>% layout(xaxis = list(title = ""),
                 yaxis = list(title = "Volume restant (L)", rangemode = "tozero",range = c(0, 400)),
                 legend = list(orientation = "h"),
                 paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Table des dates de fin de fût prévues (tôt / estimée / tard) par bière
table_predictions_fin <- function(db_predict) {
  if (is.null(db_predict) || nrow(db_predict) == 0)
    return(tibble(Bière = character(), `Volume restant` = character(),
                  `Fin (tôt)` = character(), `Fin estimée` = character(),
                  `Fin (tard)` = character()))

  fmt <- function(x) {
    x <- as.Date(x, origin = "1970-01-01")
    ifelse(is.na(x), "—", format(x, "%a %d/%m/%Y"))
  }

  unique(db_predict$ID_BRASSIN) %>%
    map_df(function(id) {
      fin  <- predict_fin_brassin(db_predict, id)
      info <- db_predict %>%
        filter(ID_BRASSIN == id, !FL_PREDICT) %>%
        slice_tail(n = 1)
      tibble(Bière = info$BOISSON,
             `Volume restant` = paste0(round(info$VOLUME_RESTANT), " L"),
             `Fin (tôt)`  = fmt(fin[1]),
             `Fin estimée` = fmt(fin[2]),
             `Fin (tard)` = fmt(fin[3]),
             ord = suppressWarnings(as.numeric(as.Date(fin[2], origin = "1970-01-01"))))
    }) %>%
    arrange(ord) %>%
    select(-ord)
}


#### REFONTE — Volet "Simulation" ####

# Base de simulation : par produit sur une période -> quantité, CA HTVA, prix moyen HTVA.
# Ordre stable (CATEGORIE puis CA décroissant) pour mapper les éditions par n° de ligne.
prepa_simulation <- function(db_produits, d1, d2) {
  db_produits %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(CATEGORIE, PRODUIT = PRODUIT_FULL) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    mutate(PRIX_MOYEN = round(CA / QUANTITE, 2)) %>%
    arrange(CATEGORIE, desc(CA))
}

# Applique un vecteur de prix simulés (indexé par n° de ligne) à la base.
# Hypothèse : quantité inchangée -> le CA varie au prorata du prix.
calc_simulation <- function(base, prix_simu) {
  if (is.null(prix_simu) || length(prix_simu) != nrow(base))
    prix_simu <- base$PRIX_MOYEN
  base %>%
    mutate(PRIX_SIMU = as.numeric(prix_simu),
           PRIX_SIMU = ifelse(is.na(PRIX_SIMU), PRIX_MOYEN, PRIX_SIMU),
           CA_SIMU = QUANTITE * PRIX_SIMU,
           DELTA = CA_SIMU - CA)
}

# Mise en forme pour affichage DT (table éditable côté serveur)
table_simulation_aff <- function(sim) {
  sim %>%
    transmute(Catégorie = CATEGORIE,
              Produit = tronque_nom(PRODUIT),
              Quantité = QUANTITE,
              `Prix moyen` = PRIX_MOYEN,
              `Prix simulé` = round(PRIX_SIMU, 2),
              `CA actuel` = round(CA),
              `CA simulé` = round(CA_SIMU),
              `Δ CA` = round(DELTA))
}


#### REFONTE — Volet "Compta / Gestion" ####
# Sources (fictives pour l'instant, cf. donnees_fictives_compta.R) :
#   DB_COUTS_TRAVAIL : DATE    x SECTEUR -> HEURES, COUT_TRAVAIL
#   DB_COUTS_MATIERE : SEMAINE x SECTEUR -> ACHATS, VARIATION_STOCK, COUT_MATIERE
# 4 secteurs, JAMAIS agrégés entre eux : Service / Transformation alimentaire /
# Brasserie / Support. Indicateurs : Food Cost, Work Cost, Prime Cost, Marge.
#
# NB : les coûts matière sont hebdomadaires. Quand on agrège au mois ou à
# l'année, chaque semaine est rattachée à la période de son LUNDI (règle simple
# et stable ; une semaine à cheval compte donc pour le mois de son lundi).

# Couleurs par secteur (déclinaison de la palette Mazette)
COULEURS_SECTEURS <- c(
  "Service"                    = "#2980b9",
  "Transformation alimentaire" = "#5B7B5A",
  "Brasserie"                  = "#d98236",
  "Support"                    = "#8d7b68"
)

COUL_MATIERE <- "#d3c0ac"   # coût matière / frais généraux
COUL_TRAVAIL <- "#732c02"   # coût du personnel
# COUL_VERT / COUL_AMBRE / COUL_ROUGE : cf. « Conventions de couleurs » en tête
# de fichier, partagées avec les barres de CA vs objectif.

##### Périodes #####

# Début de période d'une date selon la granularité.
debut_periode <- function(d, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
    semaine = floor_date(d, "week", week_start = 1),
    mois    = floor_date(d, "month"),
    annee   = floor_date(d, "year"))
}

# Dernier jour d'une période.
fin_periode <- function(periode, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
    semaine = periode + 6,
    mois    = ceiling_date(periode, "month") - 1,
    annee   = ceiling_date(periode, "year") - 1)
}

# Étiquette lisible d'une période (date = début de période).
label_periode <- function(periode, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
    semaine = paste0("Sem. ", format(periode, "%d/%m/%y")),
    mois    = format(periode, "%B %Y"),
    annee   = format(periode, "%Y"))
}

# Périodes disponibles (avec du CA), de la plus récente à la plus ancienne.
liste_periodes_dispo <- function(db_kpi, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  db_kpi %>%
    filter(ventes > 0) %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    distinct(PERIODE) %>%
    arrange(desc(PERIODE)) %>%
    pull(PERIODE)
}

##### Agrégation compta #####

# Agrège CA / matières / personnel / marge par période, avec les ratios KPI.
#   db_kpi     : sortie de prepa_db (DATE, ventes)
#   db_travail : DB_COUTS_TRAVAIL
#   db_matiere : DB_COUTS_MATIERE
agrege_compta <- function(db_kpi, db_travail, db_matiere,
                          unite = c("semaine", "mois", "annee"),
                          d1 = NULL, d2 = NULL) {
  unite <- match.arg(unite)

  if (!is.null(d1)) {
    d1 <- as.Date(d1)
    db_kpi     <- filter(db_kpi,     DATE    >= d1)
    db_travail <- filter(db_travail, DATE    >= d1)
    db_matiere <- filter(db_matiere, SEMAINE >= d1)
  }
  if (!is.null(d2)) {
    d2 <- as.Date(d2)
    db_kpi     <- filter(db_kpi,     DATE    <= d2)
    db_travail <- filter(db_travail, DATE    <= d2)
    db_matiere <- filter(db_matiere, SEMAINE <= d2)
  }
  
  # Se limiter aux données dispo en db_travail et en db_matiere
  db_kpi <- db_kpi |>
    filter(DATE %in% db_travail$DATE,
           PREMIER_JOUR_SEMAINE %in% db_matiere$SEMAINE)

  ca <- db_kpi %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(CA = sum(ventes, na.rm = TRUE), .groups = "drop")

  trav <- db_travail %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE),
              HEURES  = sum(HEURES, na.rm = TRUE), .groups = "drop")

  # On sépare les matières « métier » (Service / Transfo / Brasserie) des frais
  # généraux (Support) : le Prime Cost au sens de la restauration = matières +
  # personnel, les frais généraux étant suivis à part.
  mat <- db_matiere %>%
    mutate(PERIODE = debut_periode(SEMAINE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(FOOD    = sum(COUT_MATIERE[SECTEUR != "Support"], na.rm = TRUE),
              GENERAL = sum(COUT_MATIERE[SECTEUR == "Support"], na.rm = TRUE),
              .groups = "drop")

  ca %>%
    full_join(trav, by = "PERIODE") %>%
    full_join(mat,  by = "PERIODE") %>%
    filter(!is.na(CA)) |> 
    arrange(PERIODE) %>%
    mutate(across(c(CA, TRAVAIL, HEURES, FOOD, GENERAL), ~replace_na(., 0))) %>%
    mutate(MATIERE = FOOD + GENERAL,
           PRIME   = FOOD + TRAVAIL,
           CHARGES = PRIME + GENERAL,
           MARGE   = CA - CHARGES,
           FOOD_PCT    = ratio_pct(FOOD,    CA),
           WORK_PCT    = ratio_pct(TRAVAIL, CA),
           GENERAL_PCT = ratio_pct(GENERAL, CA),
           PRIME_PCT   = ratio_pct(PRIME,   CA),
           MARGE_PCT   = ratio_pct(MARGE,   CA)) %>%
    filter(CA > 0 | CHARGES > 0)
}

# Ratio en % (NA si dénominateur nul)
ratio_pct <- function(num, den) ifelse(den > 0, round(100 * num / den, 1), NA_real_)

# Détail par secteur sur une fenêtre de dates (une ligne par secteur + Total).
compta_secteurs <- function(db_travail, db_matiere, d1, d2) {
  d1 <- as.Date(d1); d2 <- as.Date(d2)

  trav <- db_travail %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(SECTEUR) %>%
    summarise(HEURES  = sum(HEURES, na.rm = TRUE),
              TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")

  mat <- db_matiere %>%
    filter(SEMAINE >= d1, SEMAINE <= d2) %>%
    group_by(SECTEUR) %>%
    summarise(ACHATS  = sum(ACHATS, na.rm = TRUE),
              STOCK   = sum(VARIATION_STOCK, na.rm = TRUE),
              MATIERE = sum(COUT_MATIERE, na.rm = TRUE), .groups = "drop")

  tibble(SECTEUR = SECTEURS_COMPTA) %>%
    left_join(trav, by = "SECTEUR") %>%
    left_join(mat,  by = "SECTEUR") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)),
           TOTAL = MATIERE + TRAVAIL)
}

# Synthèse d'UNE période : la ligne d'agrégat + le détail par secteur.
compta_apercu <- function(db_kpi, db_travail, db_matiere, periode,
                          unite = c("semaine", "mois", "annee")) {
  unite   <- match.arg(unite)
  periode <- as.Date(periode)
  d1 <- periode
  d2 <- fin_periode(periode, unite)

  res <- agrege_compta(db_kpi, db_travail, db_matiere, unite, d1 = d1, d2 = d2)
  if (nrow(res) == 0)
    res <- tibble(PERIODE = periode, CA = 0, TRAVAIL = 0, HEURES = 0,
                  FOOD = 0, GENERAL = 0, MATIERE = 0, PRIME = 0, CHARGES = 0,
                  MARGE = 0, FOOD_PCT = NA_real_, WORK_PCT = NA_real_,
                  GENERAL_PCT = NA_real_, PRIME_PCT = NA_real_,
                  MARGE_PCT = NA_real_)

  list(unite    = unite,
       periode  = periode,
       libelle  = label_periode(periode, unite),
       bornes   = c(d1, d2),
       total    = res[1, ],
       secteurs = compta_secteurs(db_travail, db_matiere, d1, d2))
}

##### Tuiles KPI #####

# Couleur d'un ratio où PLUS BAS = MIEUX (food/work/prime cost).
couleur_seuil <- function(x, bon, moyen) {
  if (is.na(x)) return("#9e9e9e")
  if (x <= bon) COUL_VERT else if (x <= moyen) COUL_AMBRE else COUL_ROUGE
}

# Couleur d'un ratio où PLUS HAUT = MIEUX (marge).
couleur_seuil_haut <- function(x, bon, moyen) {
  if (is.na(x)) return("#9e9e9e")
  if (x >= bon) COUL_VERT else if (x >= moyen) COUL_AMBRE else COUL_ROUGE
}

format_pct <- function(x, nb = 1) if (is.na(x)) "—" else paste0(round(x, nb), " %")

# Une tuile KPI (grand chiffre + libellé + icône en filigrane)
kpi_tile <- function(valeur, libelle, couleur, icone = NULL, sous_titre = NULL) {
  div(
    class = "kpi-tile", style = paste0("background:", couleur, ";"),
    if (!is.null(icone)) span(class = "kpi-tile-icon", icon(icone)),
    div(class = "kpi-tile-val", valeur),
    div(class = "kpi-tile-lab", libelle),
    if (!is.null(sous_titre)) div(class = "kpi-tile-sub", sous_titre)
  )
}

# Grille des KPI d'une période (sortie de compta_apercu).
kpi_compta_tiles <- function(ap) {
  t <- ap$total
  div(
    class = "kpi-grid",
    kpi_tile(format_CA(t$CA, -1), "CA HTVA", "#2e7d32", "euro-sign"),
    kpi_tile(format_CA(t$MARGE, -1), "Marge",
             if (t$MARGE >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(t$MARGE_PCT)),
    kpi_tile(format_pct(t$FOOD_PCT), "Food Cost / CA",
             couleur_seuil(t$FOOD_PCT, 30, 35), "cart-shopping",
             sous_titre = format_CA(t$FOOD, -1)),
    kpi_tile(format_pct(t$WORK_PCT), "Work Cost / CA",
             couleur_seuil(t$WORK_PCT, 35, 42), "person-running",
             sous_titre = format_CA(t$TRAVAIL, -1)),
    kpi_tile(format_pct(t$PRIME_PCT), "Prime Cost / CA",
             couleur_seuil(t$PRIME_PCT, 65, 72), "scale-balanced",
             sous_titre = format_CA(t$PRIME, -1)),
    kpi_tile(format_pct(t$GENERAL_PCT), "Frais généraux / CA",
             couleur_seuil(t$GENERAL_PCT, 12, 18), "receipt",
             sous_titre = format_CA(t$GENERAL, -1)),
    kpi_tile(format(round(t$HEURES)), "Heures prestées", "#8d7b68", "clock",
             sous_titre = if (t$HEURES > 0)
               paste0(format_CA(t$CA / t$HEURES, -1), " de CA / h") else NULL)
  )
}

# Bandeau de comparaison A vs B (écarts en € et en points de %).
kpi_ecarts_tiles <- function(ap_a, ap_b) {
  a <- ap_a$total; b <- ap_b$total
  ec <- function(x, y) x - y
  pt <- function(x, y) if (is.na(x) || is.na(y)) NA_real_ else x - y
  signe <- function(v, unite = "€") {
    if (is.na(v)) return("—")
    prefixe <- if (v > 0) "+" else ""
    if (unite == "€") paste0(prefixe, format_CA(v, -1))
    else paste0(prefixe, round(v, 1), " pt")
  }
  # Pour les coûts, une hausse est défavorable -> rouge
  coul_bas <- function(v) if (is.na(v)) "#9e9e9e" else if (v <= 0) COUL_VERT else COUL_ROUGE
  coul_haut <- function(v) if (is.na(v)) "#9e9e9e" else if (v >= 0) COUL_VERT else COUL_ROUGE

  div(
    class = "kpi-grid",
    kpi_tile(signe(ec(a$CA, b$CA)), "Écart CA", coul_haut(ec(a$CA, b$CA)), "euro-sign"),
    kpi_tile(signe(ec(a$MARGE, b$MARGE)), "Écart marge",
             coul_haut(ec(a$MARGE, b$MARGE)), "piggy-bank"),
    kpi_tile(signe(pt(a$FOOD_PCT, b$FOOD_PCT), "pt"), "Écart food cost",
             coul_bas(pt(a$FOOD_PCT, b$FOOD_PCT)), "cart-shopping"),
    kpi_tile(signe(pt(a$WORK_PCT, b$WORK_PCT), "pt"), "Écart work cost",
             coul_bas(pt(a$WORK_PCT, b$WORK_PCT)), "person-running"),
    kpi_tile(signe(pt(a$PRIME_PCT, b$PRIME_PCT), "pt"), "Écart prime cost",
             coul_bas(pt(a$PRIME_PCT, b$PRIME_PCT)), "scale-balanced"),
    kpi_tile(signe(pt(a$MARGE_PCT, b$MARGE_PCT), "pt"), "Écart marge %",
             coul_haut(pt(a$MARGE_PCT, b$MARGE_PCT)), "percent")
  )
}

##### Graphiques compta #####

# Évolution par période : coûts empilés + CA (ligne) + marge (losange).
# Cliquable : `source` permet de sélectionner une période au clic.
graph_evo_compta <- function(comptes, unite = c("semaine", "mois", "annee"),
                             source = "compta_evo", selection = NULL) {
  unite <- match.arg(unite)
  if (is.null(comptes) || nrow(comptes) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  lbl <- label_periode(comptes$PERIODE, unite)
  # La période sélectionnée est mise en évidence (les autres sont atténuées)
  op <- if (is.null(selection)) rep(1, nrow(comptes))
        else ifelse(comptes$PERIODE == as.Date(selection), 1, 0.45)

  plot_ly(comptes, source = source) %>%
    add_bars(x = ~PERIODE, y = ~FOOD, name = "Matières",
             marker = list(color = COUL_MATIERE, opacity = op),
             hovertemplate = ~paste0(lbl, "<br>Matières ",
                                     format_CA(FOOD, -1), "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~TRAVAIL, name = "Personnel",
             marker = list(color = COUL_TRAVAIL, opacity = op),
             hovertemplate = ~paste0(lbl, "<br>Personnel ",
                                     format_CA(TRAVAIL, -1), "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~GENERAL, name = "Frais généraux",
             marker = list(color = "#8d7b68", opacity = op),
             hovertemplate = ~paste0(lbl, "<br>Frais généraux ",
                                     format_CA(GENERAL, -1), "<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~CA, name = "CA HTVA",
              line = list(color = "#2e7d32", width = 2.5),
              hovertemplate = ~paste0(lbl, "<br>CA ",
                                      format_CA(CA, -1), "<extra></extra>")) %>%
    add_markers(x = ~PERIODE, y = ~MARGE, name = "Marge",
                marker = list(size = 9, symbol = "diamond",
                              color = ifelse(comptes$MARGE >= 0, COUL_VERT, COUL_ROUGE)),
                hovertemplate = ~paste0(lbl, "<br>Marge ", format_CA(MARGE, -1),
                                        " (", MARGE_PCT, " %)<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "€"),
           legend = list(orientation = "h"), hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Évolution des ratios (food / work / prime cost, en % du CA).
graph_evo_kpi_compta <- function(comptes, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(comptes) || nrow(comptes) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  lbl <- label_periode(comptes$PERIODE, unite)
  ligne <- function(p, col, nom, couleur) {
    p %>% add_lines(x = ~PERIODE, y = comptes[[col]], name = nom,
                    
                    line = list(color = couleur, width = 2),
                    hovertemplate = paste0(lbl, "<br>", nom, " %{y:.1f} %<extra></extra>"))
  }

  plot_ly(comptes) %>%
    ligne("FOOD_PCT",    "Food Cost",      COUL_MATIERE) %>%
    ligne("WORK_PCT",    "Work Cost",      COUL_TRAVAIL) %>%
    ligne("GENERAL_PCT", "Frais généraux", "#8d7b68") %>%
    ligne("PRIME_PCT",   "Prime Cost",     COUL_ROUGE) %>%
    ligne("MARGE_PCT",   "Marge",          COUL_VERT) %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = "% du CA", ticksuffix = " %", range = c(-100, 200)),
           legend = list(orientation = "h"), hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Barres horizontales empilées matière/personnel par secteur (+ ligne Prime Cost).
graph_secteurs_compta <- function(ap) {
  sect <- ap$secteurs %>% filter(TOTAL != 0)
  if (nrow(sect) == 0 | sum(sect$TRAVAIL) == 0)
    return(plotly_empty() %>% 
             layout(title = "Aucun coût sur la période",
                    paper_bgcolor = "rgba(0,0,0,0)", 
                    plot_bgcolor = "rgba(0,0,0,0)"))
  
  # Total en haut, puis les secteurs du plus coûteux au moins coûteux
  sect <- sect %>% arrange(TOTAL)
  tot <- tibble(SECTEUR = "Prime Cost",
                MATIERE = sum(sect$MATIERE), TRAVAIL = sum(sect$TRAVAIL),
                TOTAL = sum(sect$TOTAL))
  dat <- bind_rows(sect %>% select(SECTEUR, MATIERE, TRAVAIL, TOTAL), tot) %>%
    mutate(SECTEUR = factor(SECTEUR, levels = SECTEUR),
           PC_MAT = ifelse(TOTAL > 0, round(100 * MATIERE / TOTAL), NA),
           PC_TRA = ifelse(TOTAL > 0, round(100 * TRAVAIL / TOTAL), NA))

  etiquette <- function(pc) ifelse(is.na(pc) | abs(pc) < 8, "", paste0(pc, "%"))

  plot_ly(dat) %>%
    add_bars(y = ~SECTEUR, x = ~MATIERE, orientation = "h", name = "Matières",
             marker = list(color = COUL_MATIERE),
             text = etiquette(dat$PC_MAT), textposition = "inside",
             insidetextfont = list(color = "#260b01"),
             hovertemplate = ~paste0(SECTEUR, "<br>Matières ",
                                     format_CA(MATIERE, -1), "<extra></extra>")) %>%
    add_bars(y = ~SECTEUR, x = ~TRAVAIL, orientation = "h", name = "Personnel",
             marker = list(color = COUL_TRAVAIL),
             text = etiquette(dat$PC_TRA), textposition = "inside",
             insidetextfont = list(color = "#ffffff"),
             hovertemplate = ~paste0(SECTEUR, "<br>Personnel ",
                                     format_CA(TRAVAIL, -1), "<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = "€"), yaxis = list(title = ""),
           legend = list(orientation = "h"),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Tableau des coûts par secteur (+ ligne Total), style "Coûts par secteur".
table_secteurs_compta <- function(ap) {
  sect <- ap$secteurs
  ca <- ap$total$CA
  
  if (is.na(ca) || ca == 0) return(NULL)
  
  tot <- sect %>%
    summarise(SECTEUR = "Total", HEURES = sum(HEURES), ACHATS = sum(ACHATS),
              STOCK = sum(STOCK), MATIERE = sum(MATIERE),
              TRAVAIL = sum(TRAVAIL), TOTAL = sum(TOTAL))

  bind_rows(sect, tot) %>%
    transmute(Secteur    = SECTEUR,
              Heures     = round(HEURES),
              Achats     = format_CA(ACHATS, -1),
              Stock      = format_CA(STOCK, -1),
              Matières   = format_CA(MATIERE, -1),
              Personnel  = format_CA(TRAVAIL, -1),
              Total      = format_CA(TOTAL, -1),
              `% du CA`  = paste0(round(100 * TOTAL / ca, 1), " %"))
}


#### REFONTE — Volet "Comparaison" ####

# Tableau de comparaison : une ligne par période, ventes vs objectif ET compta.
comparaison_periodes <- function(db_kpi, db_obj, db_travail, db_matiere,
                                 unite = c("semaine", "mois", "annee"),
                                 periodes = NULL) {
  unite <- match.arg(unite)

  comptes <- agrege_compta(db_kpi, db_travail, db_matiere, unite)

  obj <- db_obj %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(OBJECTIF = sum(ventes, na.rm = TRUE), .groups = "drop")

  res <- comptes %>%
    left_join(obj, by = "PERIODE") %>%
    mutate(OBJECTIF = replace_na(OBJECTIF, 0),
           PCT_OBJ  = ratio_pct(CA, OBJECTIF))

  if (!is.null(periodes))
    res <- res %>% filter(PERIODE %in% as.Date(periodes))

  res %>% arrange(PERIODE)
}

# Barres groupées : CA réalisé / objectif / marge pour chaque période comparée.
graph_comparaison <- function(comp, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(comp) || nrow(comp) == 0)
    return(plotly_empty() %>% layout(title = "Sélectionnez des périodes à comparer"))

  comp <- comp %>% arrange(PERIODE)
  lab  <- factor(label_periode(comp$PERIODE, unite),
                 levels = label_periode(comp$PERIODE, unite))

  # La barre de CA prend la couleur de l'atteinte de son objectif ; l'objectif
  # lui-même reste neutre pour ne pas brouiller la lecture.
  atteinte <- label_objectif(comp$CA, comp$OBJECTIF)

  plot_ly(comp) %>%
    add_bars(x = lab, y = ~CA, name = "CA réalisé",
             marker = list(color = couleur_objectif(comp$CA, comp$OBJECTIF)),
             hovertemplate = ~paste0("CA ", format_CA(CA, -1), "<br>", atteinte,
                                     "<extra></extra>")) %>%
    add_bars(x = lab, y = ~OBJECTIF, name = "Objectif",
             marker = list(color = COUL_NEUTRE),
             hovertemplate = ~paste0("Objectif ", format_CA(OBJECTIF, -1), "<extra></extra>")) %>%
    add_bars(x = lab, y = ~MARGE, name = "Marge",
             marker = list(color = COUL_VERT),
             hovertemplate = ~paste0("Marge ", format_CA(MARGE, -1),
                                     " (", MARGE_PCT, " %)<extra></extra>")) %>%
    layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "€"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Tableau comparatif (ventes vs objectif + compta).
table_comparaison_aff <- function(comp, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(comp) || nrow(comp) == 0)
    return(tibble(Période = character()))
  comp %>%
    arrange(desc(PERIODE)) %>%
    transmute(Période       = label_periode(PERIODE, unite),
              `CA (HTVA)`   = format_CA(CA, -1),
              Objectif      = format_CA(OBJECTIF, -1),
              `% obj.`      = ifelse(is.na(PCT_OBJ), "—", paste0(PCT_OBJ, " %")),
              `Food cost`   = ifelse(is.na(FOOD_PCT), "—", paste0(FOOD_PCT, " %")),
              `Work cost`   = ifelse(is.na(WORK_PCT), "—", paste0(WORK_PCT, " %")),
              `Prime cost`  = ifelse(is.na(PRIME_PCT), "—", paste0(PRIME_PCT, " %")),
              Marge         = format_CA(MARGE, -1),
              `Marge %`     = ifelse(is.na(MARGE_PCT), "—", paste0(MARGE_PCT, " %")))
}


#### REFONTE — Volet "Année" ####
# Suivi annuel « à date » : on ne compare que les jours déjà écoulés, en cumulé.

# Série quotidienne de l'année : CA, objectif, marge (matière hebdo étalée /7).
serie_annuelle <- function(db_kpi, db_obj, db_travail, db_matiere,
                           annee = year(today())) {
  d1 <- as.Date(paste0(annee, "-01-01"))
  d2 <- as.Date(paste0(annee, "-12-31"))

  jours <- db_kpi %>%
    filter(DATE >= d1, DATE <= d2) %>%
    select(DATE, ventes) %>%
    arrange(DATE)

  obj <- db_obj %>%
    filter(DATE >= d1, DATE <= d2) %>%
    select(DATE, objectif = ventes)

  trav <- db_travail %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(DATE) %>%
    summarise(TRAVAIL = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")

  # Le coût matière est hebdomadaire -> réparti à parts égales sur les 7 jours
  mat <- db_matiere %>%
    group_by(SEMAINE) %>%
    summarise(MATIERE = sum(COUT_MATIERE, na.rm = TRUE), .groups = "drop")
  
  # jours <- jours |> 
  #   filter(DATE %in% trav$DATE) |> 
  #   filter(floor_date(DATE, "week", week_start = 1) %in% mat$SEMAINE)

  jours %>%
    left_join(obj,  by = "DATE") %>%
    left_join(trav, by = "DATE") %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    left_join(mat, by = "SEMAINE") %>%
    mutate(across(c(ventes, objectif, TRAVAIL, MATIERE), ~replace_na(., 0)),
           MATIERE = MATIERE / 7,
           MARGE   = ventes - TRAVAIL - MATIERE
           # MARGE   = ifelse(TRAVAIL == 0 | MATIERE == 0,NA, ventes - TRAVAIL - MATIERE
           )
}

# Graphe générique d'écart cumulé « à date » (aire verte au-dessus de 0, rouge en
# dessous) + point et annotation sur la dernière valeur connue.
graph_ecart_cumule <- function(dat, titre_y, libelle) {
  dat <- dat %>% filter(!is.na(ECART))
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  dernier <- dat %>% slice_tail(n = 1)
  couleur <- if (dernier$ECART >= 0) COUL_VERT else COUL_ROUGE

  plot_ly(dat) %>%
    add_lines(x = ~DATE, y = ~ECART, name = libelle,
              line = list(color = couleur, width = 2.5),
              fill = "tozeroy",
              fillcolor = if (dernier$ECART >= 0) "rgba(91,123,90,0.15)"
                          else "rgba(192,57,43,0.15)",
              hovertemplate = ~paste0(LABEL, "<extra></extra>")) %>%
    add_markers(data = dernier, x = ~DATE, y = ~ECART, name = "Dernier jour",
                marker = list(color = couleur, size = 10),
                hovertemplate = ~paste0(LABEL, "<extra></extra>")) %>%
    layout(
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         y0 = 0, y1 = 0,
                         line = list(color = "#260b01", width = 1.5))),
      annotations = list(list(
        x = dernier$DATE, y = dernier$ECART,
        text = paste0("<b>", format_CA(dernier$ECART, -1), "</b>"),
        showarrow = TRUE, arrowhead = 0, ax = -45, ay = -30,
        font = list(color = couleur, size = 13),
        bgcolor = "rgba(255,255,255,0.75)", bordercolor = couleur)),
      xaxis = list(title = ""),
      yaxis = list(title = titre_y),
      showlegend = FALSE,
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Écart cumulé de CA vs objectif, à date.
graph_ecart_objectif <- function(serie) {
  dat <- serie %>%
    filter(DATE < today()) %>%
    arrange(DATE) %>%
    mutate(ECART = cumsum(ventes - objectif),
           LABEL = paste0(format(DATE, "%d/%m/%Y"),
                          "<br>CA : ", format_CA(ventes, -1),
                          "<br>Objectif : ", format_CA(objectif, -1),
                          "<br><b>Écart cumulé : ", format_CA(ECART, -1), "</b>"))
  graph_ecart_cumule(dat, "Écart cumulé vs objectif (€)", "Écart vs objectif")
}

# Écart cumulé vs N-1 (aligné sur le même numéro de semaine et le même jour).
graph_ecart_ym1 <- function(db_kpi, annee = year(today()), var = c("ventes", "marge"),
                            serie = NULL, serie_m1 = NULL) {
  var <- match.arg(var)

  prep <- function(d) {
    d %>% mutate(WEEK = week(DATE), WDAY = wday(DATE))
  }

  if (var == "ventes") {
    cur <- db_kpi %>% filter(year(DATE) == annee) %>%
      transmute(DATE, VAL = ventes) %>% prep()
    prec <- db_kpi %>% filter(year(DATE) == annee - 1) %>%
      transmute(DATE, VAL_M1 = ventes) %>% prep() %>%
      select(WEEK, WDAY, VAL_M1)
    titre <- "Écart cumulé de CA vs N-1 (€)"
    nom   <- "CA"
  } else {
    cur <- serie %>% transmute(DATE, VAL = MARGE) %>% prep()
    prec <- serie_m1 %>% transmute(DATE, VAL_M1 = MARGE) %>% prep() %>%
      select(WEEK, WDAY, VAL_M1)
    titre <- "Écart cumulé de marge vs N-1 (€)"
    nom   <- "Marge"
  }

  dat <- cur %>%
    left_join(prec, by = c("WEEK", "WDAY")) %>%
    arrange(DATE) %>%
    filter(DATE < today()) %>%
    mutate(
      # VAL = replace_na(VAL, 0), VAL_M1 = replace_na(VAL_M1, 0),
           ECART = replace_na(cumsum(VAL - VAL_M1), 0),
           LABEL = paste0(format(DATE, "%d/%m/%Y"),
                          "<br>", nom, " : ", format_CA(VAL, -1),
                          "<br>", nom, " N-1 : ", format_CA(VAL_M1, -1),
                          "<br><b>Écart cumulé : ", format_CA(ECART, -1), "</b>"))

  graph_ecart_cumule(dat, titre, paste("Écart", nom, "vs N-1"))
}

# Tuiles de synthèse annuelle « à date ».
kpi_annee_tiles <- function(serie, serie_m1) {
  ecoule <- serie %>% filter(DATE < today())
  ca     <- sum(ecoule$ventes, na.rm = TRUE)
  obj    <- sum(ecoule$objectif, na.rm = TRUE)
  marge  <- sum(ecoule$MARGE, na.rm = TRUE)

  # N-1 sur le même nombre de jours d'ouverture écoulés
  n_jours <- nrow(ecoule %>% filter(ventes > 0))
  ecoule_m1 <- serie_m1 %>% filter(ventes > 0) %>% arrange(DATE) %>% head(n_jours)
  ca_m1    <- sum(ecoule_m1$ventes, na.rm = TRUE)
  marge_m1 <- sum(ecoule_m1$MARGE, na.rm = TRUE)

  pct <- function(x, y) if (y > 0) round(100 * x / y, 1) else NA_real_

  div(
    class = "kpi-grid",
    kpi_tile(format_CA(ca, -1), "CA à date", "#2e7d32", "euro-sign"),
    kpi_tile(format_CA(obj, -1), "Objectif à date", COUL_TRAVAIL, "bullseye",
             sous_titre = format_pct(pct(ca, obj))),
    kpi_tile(format_CA(ca - obj, -1), "Écart objectif",
             if (ca >= obj) COUL_VERT else COUL_ROUGE, "arrow-right-arrow-left"),
    kpi_tile(format_CA(ca - ca_m1, -1), "Écart CA vs N-1",
             if (ca >= ca_m1) COUL_VERT else COUL_ROUGE, "clock-rotate-left",
             sous_titre = paste0("N-1 : ", format_CA(ca_m1, -1))),
    kpi_tile(format_CA(marge, -1), "Marge à date",
             if (marge >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(pct(marge, ca))),
    kpi_tile(format_CA(marge - marge_m1, -1), "Écart marge vs N-1",
             if (marge >= marge_m1) COUL_VERT else COUL_ROUGE, "chart-line",
             sous_titre = paste0("N-1 : ", format_CA(marge_m1, -1)))
  )
}

#### REFONTE — Volet "Travail" ####
# Suivi de la productivité et du coût du travail, inspiré de l'étude
# « Réduction des jours d'ouverture » (avril 2026).
#
# Vocabulaire (repris de l'étude) :
#   Créneau      : une demi-journée d'ouverture — Midi (<17h) ou Soir (>=17h).
#                  Les mardis soir avec vente de pizza forment un créneau à
#                  part : la Pizzwanze.
#   Heures de service : heures directement liées à l'ouverture d'un créneau.
#   Heures indirectes : transformation alimentaire, brasserie et support. Elles
#                  sont mutualisées sur la semaine puis réparties entre les
#                  créneaux AU PRORATA DU CA, pour que les créneaux qui
#                  rapportent le plus portent la plus grande part de la
#                  structure.
#   Marge après travail : CA HTVA − coût de service − coûts indirects. Ce qui
#                  reste pour couvrir les matières, le loyer et l'énergie.
#
# NB : on ne traite volontairement PAS la question « faut-il fermer un
# créneau ? » — l'étude a montré qu'aucun scénario de fermeture n'améliore la
# marge. L'objet ici est le pilotage du staffing.

CRENEAUX_ORDRE <- c("Midi", "Soir", "Pizzwanze")
PAL_CRENEAU <- c("Midi" = "#e67e22", "Soir" = "#9b59b6", "Pizzwanze" = "#c0392b")

# Toutes les fonctions de ce volet attendent une table AU GRAIN HORAIRE
# (DATE x CD_HEURE x PRODUIT), c'est-à-dire TICKETS_HEURES — et non DB_PRODUITS,
# qui est agrégée à la journée et n'a donc pas de colonne CD_HEURE.

# Jours de Pizzwanze : mardi soir où l'on a vendu des pizzas.
jours_pizzwanze <- function(db_ventes_heure) {
  db_ventes_heure %>%
    filter(str_detect(toupper(PRODUIT), "PIZZ"),
           CD_HEURE == "Soir (>=17h)",
           wday(DATE, week_start = 1) == 2,
           CA_HTVA > 0) %>%
    distinct(DATE) %>%
    pull(DATE)
}

# Renomme le créneau "Soir" en "Pizzwanze" sur les dates concernées.
marque_pizzwanze <- function(db, dates_piz) {
  db %>%
    mutate(CRENEAU = ifelse(CRENEAU == "Soir" & DATE %in% dates_piz,
                            "Pizzwanze", CRENEAU),
           CRENEAU = factor(CRENEAU, levels = CRENEAUX_ORDRE))
}

# Normalisation des créneaux, reprise de l'étude de rentabilité :
#   - les lundis (rares ouvertures exceptionnelles) sont exclus ;
#   - le mardi est toujours un créneau « Soir » (ouverture à 17h) ;
#   - le dimanche est toujours un créneau « Midi » (le CA résiduel de soirée y
#     est rattaché, le service ferme à 18h).
# Sans cela on obtient des créneaux fantômes : du CA sans aucune heure de
# service en face, donc une productivité infinie.
normalise_creneaux <- function(db) {
  db %>%
    mutate(.wd = wday(DATE, week_start = 1)) %>%
    filter(.wd != 1) %>%
    mutate(CRENEAU = case_when(.wd == 2 ~ "Soir",
                               .wd == 7 ~ "Midi",
                               TRUE     ~ as.character(CRENEAU))) %>%
    select(-.wd)
}

# CA HTVA par jour et par créneau (Midi / Soir / Pizzwanze).
ca_par_creneau <- function(db_ventes_heure, d1 = NULL, d2 = NULL) {
  piz <- jours_pizzwanze(db_ventes_heure)
  db <- db_ventes_heure
  if (!is.null(d1)) db <- filter(db, DATE >= as.Date(d1))
  if (!is.null(d2)) db <- filter(db, DATE <= as.Date(d2))

  db %>%
    mutate(CRENEAU = ifelse(CD_HEURE == "Midi (<17h)", "Midi", "Soir")) %>%
    normalise_creneaux() %>%
    marque_pizzwanze(piz) %>%
    group_by(DATE, CRENEAU) %>%
    summarise(CA = sum(CA_HTVA, na.rm = TRUE), .groups = "drop") %>%
    filter(CA > 0)
}

# Base de travail : une ligne par (DATE, CRENEAU) avec le CA, les heures de
# service imputées directement, et les coûts indirects de la semaine répartis
# au prorata du CA du créneau.
base_travail <- function(db_ventes_heure, db_travail, d1, d2) {
  d1 <- as.Date(d1); d2 <- as.Date(d2)
  piz <- jours_pizzwanze(db_ventes_heure)

  # Ne garder que les jours pour lesquels on connaît aussi les heures travaillées
  db_ventes_heure <- db_ventes_heure |>
    filter(DATE %in% db_travail$DATE)

  ca <- ca_par_creneau(db_ventes_heure, d1, d2)

  service <- db_travail %>%
    filter(SECTEUR == "Service", CRENEAU %in% c("Midi", "Soir"),
           DATE >= d1, DATE <= d2) %>%
    normalise_creneaux() %>%
    marque_pizzwanze(piz) %>%
    group_by(DATE, CRENEAU) %>%
    summarise(H_SERVICE    = sum(HEURES, na.rm = TRUE),
              COUT_SERVICE = sum(COUT_TRAVAIL, na.rm = TRUE), .groups = "drop")

  # Coûts indirects, mutualisés à la semaine
  indirect <- db_travail %>%
    filter(SECTEUR != "Service", DATE >= d1, DATE <= d2
           # ,wday(DATE, week_start = 1)!= 1
           ) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1),
           EST_TRANSFO = SECTEUR == "Transformation alimentaire") %>%
    group_by(SEMAINE) %>%
    summarise(H_TRANSFO    = sum(HEURES[EST_TRANSFO], na.rm = TRUE),
              COUT_TRANSFO = sum(COUT_TRAVAIL[EST_TRANSFO], na.rm = TRUE),
              H_AUTRE      = sum(HEURES[!EST_TRANSFO], na.rm = TRUE),
              COUT_AUTRE   = sum(COUT_TRAVAIL[!EST_TRANSFO], na.rm = TRUE),
              .groups = "drop")

  full_join(ca, service, by = c("DATE", "CRENEAU")) %>%
    mutate(across(c(CA, H_SERVICE, COUT_SERVICE), ~replace_na(., 0)),
           SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    left_join(indirect, by = "SEMAINE") %>%
    mutate(across(c(H_TRANSFO, COUT_TRANSFO, H_AUTRE, COUT_AUTRE),
                  ~replace_na(., 0))) %>%
    # Répartition des coûts indirects au prorata du CA de la semaine.
    # NB : `if_else` (et non `ifelse`) car la condition porte sur un total de
    # groupe — `ifelse` renverrait une valeur de longueur 1, recyclée sur toutes
    # les lignes, et donnerait la même part à tous les créneaux.
    group_by(SEMAINE) %>%
    mutate(CA_SEMAINE = sum(CA, na.rm = TRUE),
           PART = if_else(CA_SEMAINE > 0, CA / CA_SEMAINE, 0),
           across(c(H_TRANSFO, COUT_TRANSFO, H_AUTRE, COUT_AUTRE), ~ . * PART)) %>%
    ungroup() %>%
    select(-CA_SEMAINE) %>%
    mutate(COUT_INDIRECT = COUT_TRANSFO + COUT_AUTRE,
           COUT_TOTAL    = COUT_SERVICE + COUT_INDIRECT,
           MARGE         = CA - COUT_TOTAL,
           JOUR_SEMAINE  = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1),
           CRENEAU       = factor(CRENEAU, levels = CRENEAUX_ORDRE)) %>%
    arrange(DATE, CRENEAU)
}

# Agrégat par période (semaine / mois / année) à partir de la base.
agrege_travail <- function(base, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  base %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE) %>%
    summarise(CA            = sum(CA, na.rm = TRUE),
              H_SERVICE     = sum(H_SERVICE, na.rm = TRUE),
              H_INDIRECT    = sum(H_TRANSFO + H_AUTRE, na.rm = TRUE),
              COUT_SERVICE  = sum(COUT_SERVICE, na.rm = TRUE),
              COUT_TRANSFO  = sum(COUT_TRANSFO, na.rm = TRUE),
              COUT_AUTRE    = sum(COUT_AUTRE, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(COUT_INDIRECT = COUT_TRANSFO + COUT_AUTRE,
           COUT_TOTAL    = COUT_SERVICE + COUT_INDIRECT,
           H_TOTAL       = H_SERVICE + H_INDIRECT,
           MARGE         = CA - COUT_TOTAL,
           CA_PAR_HEURE  = ifelse(H_SERVICE > 0, CA / H_SERVICE, NA_real_),
           RATIO_SERVICE = ratio_pct(COUT_SERVICE, CA),
           RATIO_TOTAL   = ratio_pct(COUT_TOTAL, CA),
           MARGE_PCT     = ratio_pct(MARGE, CA)) %>%
    arrange(PERIODE)
}

# CA par période, ventilé par créneau (Midi / Soir / Pizzwanze).
agrege_creneaux_periode <- function(base, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  base %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    group_by(PERIODE, CRENEAU) %>%
    summarise(CA = sum(CA, na.rm = TRUE),
              H_SERVICE = sum(H_SERVICE, na.rm = TRUE), .groups = "drop") %>%
    mutate(CA_PAR_HEURE = ifelse(H_SERVICE > 0, CA / H_SERVICE, NA_real_))
}

# Statistiques par jour de semaine x créneau : moyennes par ouverture.
# C'est la table qui permet de comparer les créneaux à armes égales.
stats_creneaux <- function(base) {
  base %>%
    filter(CA > 0 | H_SERVICE > 0) %>%
    group_by(JOUR_SEMAINE, CRENEAU) %>%
    summarise(nb_jours      = n_distinct(DATE),
              CA_total      = sum(CA, na.rm = TRUE),
              H_service     = sum(H_SERVICE, na.rm = TRUE),
              COUT_SERVICE  = sum(COUT_SERVICE, na.rm = TRUE),
              COUT_INDIRECT = sum(COUT_INDIRECT, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(COUT_TOTAL      = COUT_SERVICE + COUT_INDIRECT,
           MARGE           = CA_total - COUT_TOTAL,
           CA_moyen        = CA_total / nb_jours,
           H_service_moyen = H_service / nb_jours,
           MARGE_moyenne   = MARGE / nb_jours,
           CA_PAR_HEURE    = ifelse(H_service > 0, CA_total / H_service, NA_real_),
           RATIO_TOTAL     = ratio_pct(COUT_TOTAL, CA_total),
           CRENEAU_LABEL   = paste0(JOUR_SEMAINE, " — ", CRENEAU)) %>%
    arrange(desc(CA_PAR_HEURE))
}

##### Tuiles KPI #####

kpi_travail_tiles <- function(ag) {
  ca    <- sum(ag$CA, na.rm = TRUE)
  hs    <- sum(ag$H_SERVICE, na.rm = TRUE)
  ht    <- sum(ag$H_TOTAL, na.rm = TRUE)
  cs    <- sum(ag$COUT_SERVICE, na.rm = TRUE)
  ct    <- sum(ag$COUT_TOTAL, na.rm = TRUE)
  marge <- ca - ct
  cah   <- if (hs > 0) ca / hs else NA_real_

  div(
    class = "kpi-grid",
    kpi_tile(if (is.na(cah)) "—" else format_CA(cah, -1), "CA par heure de service",
             couleur_seuil_haut(cah, 90, 70), "gauge-high",
             sous_titre = paste0(format(round(hs)), " h de service")),
    kpi_tile(format(round(ht)), "Heures totales", "#8d7b68", "clock",
             sous_titre = paste0(round(ratio_pct(hs, ht)), " % en service")),
    kpi_tile(format_CA(cs, -1), "Coût de service", COUL_TRAVAIL, "person-running",
             sous_titre = format_pct(ratio_pct(cs, ca))),
    kpi_tile(format_CA(ct - cs, -1), "Coûts indirects", "#8d7b68", "people-roof",
             sous_titre = format_pct(ratio_pct(ct - cs, ca))),
    kpi_tile(format_pct(ratio_pct(ct, ca)), "Coût du travail / CA",
             couleur_seuil(ratio_pct(ct, ca), 35, 45), "scale-balanced",
             sous_titre = format_CA(ct, -1)),
    kpi_tile(format_CA(marge, -1), "Marge après travail",
             if (marge >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
             sous_titre = format_pct(ratio_pct(marge, ca)))
  )
}

##### Graphiques — suivi dans le temps #####

# Décomposition du CA : marge + coût service + transfo + autre, par période.
graph_structure_travail <- function(ag, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(ag) || nrow(ag) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  lbl <- label_periode(ag$PERIODE, unite)

  plot_ly(ag) %>%
    add_bars(x = ~PERIODE, y = ~COUT_SERVICE, name = "Coût service",
             marker = list(color = COUL_TRAVAIL),
             hovertemplate = ~paste0(lbl, "<br>Service ", format_CA(COUT_SERVICE, -1),
                                     "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~COUT_TRANSFO, name = "Coût transfo",
             marker = list(color = "#a2703f"),
             hovertemplate = ~paste0(lbl, "<br>Transfo ", format_CA(COUT_TRANSFO, -1),
                                     "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~COUT_AUTRE, name = "Autres secteurs",
             marker = list(color = "#8d7b68"),
             hovertemplate = ~paste0(lbl, "<br>Autres ", format_CA(COUT_AUTRE, -1),
                                     "<extra></extra>")) %>%
    add_bars(x = ~PERIODE, y = ~MARGE, name = "Marge après travail",
             marker = list(color = COUL_VERT),
             hovertemplate = ~paste0(lbl, "<br>Marge ", format_CA(MARGE, -1),
                                     " (", MARGE_PCT, " %)<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = ""),
           yaxis = list(title = "€"), legend = list(orientation = "h"),
           hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Productivité dans le temps : heures de service (barres) + CA/heure (ligne).
graph_productivite_temps <- function(ag, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(ag) || nrow(ag) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  lbl <- label_periode(ag$PERIODE, unite)
  moy <- sum(ag$CA, na.rm = TRUE) / sum(ag$H_SERVICE, na.rm = TRUE)

  plot_ly(ag) %>%
    add_bars(x = ~PERIODE, y = ~H_SERVICE, name = "Heures de service",
             marker = list(color = "#d3c0ac"),
             hovertemplate = ~paste0(lbl, "<br>", round(H_SERVICE),
                                     " h<extra></extra>")) %>%
    add_lines(x = ~PERIODE, y = ~CA_PAR_HEURE, name = "CA / heure", yaxis = "y2",
              line = list(color = COUL_TRAVAIL, width = 2.5),
              hovertemplate = ~paste0(lbl, "<br>", format_CA(CA_PAR_HEURE, -1),
                                      " / h<extra></extra>")) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Heures de service"),
      yaxis2 = list(title = "CA par heure (€/h)", overlaying = "y",
                    side = "right", showgrid = FALSE, rangemode = "tozero"),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         yref = "y2", y0 = moy, y1 = moy,
                         line = list(color = COUL_TRAVAIL, width = 1, dash = "dot"))),
      legend = list(orientation = "h"),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# CA par période ventilé Midi / Soir / Pizzwanze.
graph_ca_creneaux_temps <- function(cre, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  if (is.null(cre) || nrow(cre) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  p <- plot_ly()
  for (cr in CRENEAUX_ORDRE) {
    sub <- cre %>% filter(CRENEAU == cr)
    if (nrow(sub) == 0) next
    lbl <- label_periode(sub$PERIODE, unite)
    p <- p %>% add_bars(
      data = sub, x = ~PERIODE, y = ~CA, name = cr,
      marker = list(color = PAL_CRENEAU[[cr]]),
      hovertemplate = paste0(lbl, "<br>", cr, " ", format_CA(sub$CA, -1),
                             "<extra></extra>"))
  }
  p %>% layout(barmode = "stack", xaxis = list(title = ""),
               yaxis = list(title = "CA HTVA (€)"),
               legend = list(orientation = "h"), hovermode = "x unified",
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

##### Graphiques — analyse par créneau #####

# Nuage CA moyen vs heures de service moyennes, avec la droite de productivité
# moyenne. Plus un créneau est haut à gauche, plus il est efficace.
graph_nuage_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  dat <- stats %>% filter(H_service_moyen > 0, CA_moyen > 0)
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  # Droite passant par l'origine = productivité moyenne globale
  pente <- sum(dat$CA_total) / sum(dat$H_service)
  xr <- c(0, max(dat$H_service_moyen) * 1.08)

  plot_ly(dat) %>%
    add_lines(x = xr, y = pente * xr, name = "Productivité moyenne",
              line = list(color = "#260b01", dash = "dot", width = 1.5),
              hoverinfo = "skip") %>%
    add_markers(x = ~H_service_moyen, y = ~CA_moyen, color = ~CRENEAU,
                colors = PAL_CRENEAU, size = ~CA_total, sizes = c(80, 500),
                text = ~CRENEAU_LABEL,
                hovertemplate = ~paste0("<b>", CRENEAU_LABEL, "</b><br>",
                                        round(H_service_moyen, 1), " h de service<br>",
                                        format_CA(CA_moyen, -1), " de CA<br>",
                                        format_CA(CA_PAR_HEURE, -1), " / h",
                                        "<extra></extra>")) %>%
    layout(xaxis = list(title = "Heures de service moyennes par ouverture",
                        rangemode = "tozero"),
           yaxis = list(title = "CA HTVA moyen par ouverture (€)",
                        rangemode = "tozero"),
           # legend = list(orientation = "h"),
           legend = list(yref = "container", y = 0, yanchor = "bottom"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Classement des créneaux par productivité horaire (barres horizontales).
graph_productivite_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  dat <- stats %>% filter(!is.na(CA_PAR_HEURE)) %>% arrange(CA_PAR_HEURE)
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  dat <- dat %>% mutate(CRENEAU_LABEL = factor(CRENEAU_LABEL, levels = CRENEAU_LABEL))
  moy <- sum(dat$CA_total) / sum(dat$H_service)

  plot_ly(dat) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~CA_PAR_HEURE, orientation = "h",
             marker = list(color = unname(PAL_CRENEAU[as.character(dat$CRENEAU)])),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>",
                                     format_CA(CA_PAR_HEURE, -1), " / h<br>",
                                     round(H_service_moyen, 1), " h par ouverture",
                                     "<extra></extra>")) %>%
    layout(xaxis = list(title = "CA HTVA par heure de service (€/h)"),
           yaxis = list(title = ""),
           shapes = list(list(type = "line", yref = "paper", y0 = 0, y1 = 1,
                              x0 = moy, x1 = moy,
                              line = list(color = "#260b01", width = 1.5,
                                          dash = "dot"))),
           showlegend = FALSE, margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Heatmap jour x créneau sur l'indicateur choisi.
graph_heatmap_creneaux <- function(stats,
                                   var = c("CA_moyen", "CA_PAR_HEURE",
                                           "RATIO_TOTAL", "MARGE_moyenne")) {
  var <- match.arg(var)
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  titre <- c(CA_moyen      = "CA moyen par ouverture (€)",
             CA_PAR_HEURE  = "CA par heure de service (€/h)",
             RATIO_TOTAL   = "Coût du travail / CA (%)",
             MARGE_moyenne = "Marge moyenne par ouverture (€)")[[var]]
  # Pour le ratio de coût, une valeur basse est meilleure : on inverse l'échelle
  echelle <- if (var == "RATIO_TOTAL")
    list(c(0, COUL_VERT), c(1, COUL_ROUGE))
  else list(c(0, "#f2efe6"), c(1, COUL_TRAVAIL))

  dat <- stats %>%
    mutate(VAL = .data[[var]]) %>%
    select(JOUR_SEMAINE, CRENEAU, VAL) %>%
    complete(JOUR_SEMAINE, CRENEAU)

  jours <- levels(droplevels(dat$JOUR_SEMAINE))
  mat <- dat %>%
    pivot_wider(names_from = CRENEAU, values_from = VAL) %>%
    arrange(JOUR_SEMAINE)

  cols <- intersect(CRENEAUX_ORDRE, names(mat))
  z <- as.matrix(mat[, cols, drop = FALSE])
  fmt <- if (var == "RATIO_TOTAL") function(x) ifelse(is.na(x), "", paste0(round(x), " %"))
         else function(x) ifelse(is.na(x), "", format_CA(x, -1))

  plot_ly(x = cols, y = as.character(mat$JOUR_SEMAINE), z = z,
          type = "heatmap", colorscale = echelle,
          hovertemplate = "%{y} — %{x}<br>%{z:.0f}<extra></extra>",
          showscale = TRUE) %>%
    add_annotations(
      x = rep(cols, each = nrow(z)),
      y = rep(as.character(mat$JOUR_SEMAINE), times = length(cols)),
      text = fmt(as.vector(z)), showarrow = FALSE,
      font = list(size = 12, color = "#260b01")) %>%
    layout(title = list(text = titre, font = list(size = 13)),
           xaxis = list(title = "", side = "top"),
           yaxis = list(title = "", autorange = "reversed"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Décomposition du CA moyen par créneau : marge + coût service + indirect.
graph_decomposition_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  dat <- stats %>%
    arrange(MARGE_moyenne) %>%
    mutate(CRENEAU_LABEL = factor(CRENEAU_LABEL, levels = CRENEAU_LABEL),
           C_SERVICE  = COUT_SERVICE / nb_jours,
           C_INDIRECT = COUT_INDIRECT / nb_jours)

  plot_ly(dat) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~MARGE_moyenne, orientation = "h",
             name = "Marge après travail", marker = list(color = COUL_VERT),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>Marge ",
                                     format_CA(MARGE_moyenne, -1), "<extra></extra>")) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~C_SERVICE, orientation = "h",
             name = "Coût service", marker = list(color = COUL_TRAVAIL),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>Service ",
                                     format_CA(C_SERVICE, -1), "<extra></extra>")) %>%
    add_bars(y = ~CRENEAU_LABEL, x = ~C_INDIRECT, orientation = "h",
             name = "Coûts indirects", marker = list(color = "#8d7b68"),
             hovertemplate = ~paste0(CRENEAU_LABEL, "<br>Indirects ",
                                     format_CA(C_INDIRECT, -1), "<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = "€ par ouverture"),
           yaxis = list(title = ""), 
           legend = list(yref = "container", y = 0, yanchor = "bottom"),
           # legend = list(orientation = "h"),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Tableau récapitulatif par créneau.
table_creneaux <- function(stats) {
  if (is.null(stats) || nrow(stats) == 0)
    return(tibble(Créneau = character()))
  stats %>%
    arrange(desc(CA_PAR_HEURE)) %>%
    transmute(Créneau        = CRENEAU_LABEL,
              Ouvertures     = nb_jours,
              `CA moyen`     = format_CA(CA_moyen, -1),
              `Heures serv.` = round(H_service_moyen, 1),
              `CA / heure`   = format_CA(CA_PAR_HEURE, -1),
              `Coût travail` = format_CA(COUT_TOTAL / nb_jours, -1),
              `Marge moy.`   = format_CA(MARGE_moyenne, -1),
              `Coût / CA`    = ifelse(is.na(RATIO_TOTAL), "—",
                                      paste0(RATIO_TOTAL, " %")))
}

#### REFONTE — Volet "Consommation" (bières & focaccias) ####
# Deux volets de suivi de la consommation, à la maille SEMAINE, toujours
# comparés à la semaine précédente (S-1).
#
# Sources :
#   DB_TICKET   : une ligne par produit vendu, avec TIMESTAMP (donc l'heure),
#                 BOISSON et VOLUME_TOT_L -> analyse horaire et en litres.
#                 Attention : DATE est le JOUR DE SERVICE (une vente à 1h du
#                 matin est rattachée à la soirée de la veille).
#   DB_PRODUITS : une ligne par (jour, produit) avec PRODUIT_FULL complet,
#                 options comprises -> seule source qui porte les suppléments
#                 des focaccias. Peut contenir plusieurs lignes par jour et
#                 produit : toujours agréger.

# Heures de service, dans l'ordre d'une soirée (on ouvre le matin, on ferme
# après minuit) plutôt que dans l'ordre naturel 0..23.
# Palette locale (functions.R ne dépend pas de ui.R)
CONSO_BRUN  <- "#732c02"
CONSO_AMBRE <- "#d98236"

ORDRE_HEURES_SERVICE <- c(6:23, 0:5)

heure_service <- function(ts) {
  factor(hour(ts), levels = ORDRE_HEURES_SERVICE,
         labels = paste0(ORDRE_HEURES_SERVICE, "h"))
}

# Semaines (lundi) disponibles dans une table, de la plus récente à la plus
# ancienne. `complete_only` retire la semaine en cours, forcément partielle.
semaines_dispo <- function(db, col = "DATE", complete_only = TRUE) {
  s <- db %>%
    mutate(SEM = floor_date(.data[[col]], "week", week_start = 1)) %>%
    distinct(SEM) %>%
    filter(!is.na(SEM)) %>%
    arrange(desc(SEM)) %>%
    pull(SEM)
  if (complete_only) s <- s[s < floor_date(today(), "week", week_start = 1)]
  s
}

# Tuile d'évolution : valeur de la semaine + écart en % vs S-1.
# `sens_positif = FALSE` quand une hausse est une mauvaise nouvelle.
tuile_evolution <- function(valeur, reference, libelle, icone,
                            format_val = function(x) format(round(x)),
                            sens_positif = TRUE, suffixe = "vs S-1") {
  evo <- if (is.na(reference) || reference == 0) NA_real_
         else 100 * (valeur - reference) / reference
  couleur <- if (is.na(evo)) "#8d7b68"
             else if ((evo >= 0) == sens_positif) COUL_VERT else COUL_ROUGE
  sous <- if (is.na(evo)) paste("pas de référence", suffixe)
          else paste0(if (evo >= 0) "+" else "", round(evo, 1), " % ", suffixe)
  kpi_tile(format_val(valeur), libelle, couleur, icone, sous_titre = sous)
}

tuile_ecart <- function(valeur, reference, libelle, icone,
                        format_val = function(x) format(round(x)),
                        sens_positif = TRUE, suffixe = "vs S-1") {
  ecart <- if (is.na(reference) || reference == 0) NA_real_
  else valeur - reference
  couleur <- if (is.na(ecart)) "#8d7b68"
  else if ((ecart >= 0) == sens_positif) COUL_VERT else COUL_ROUGE
  sous <- if (is.na(ecart)) paste("pas de référence", suffixe)
  else if (ecart == 0) paste0("identique ",suffixe)
  else paste0(if (ecart >= 0) "+" else "", round(ecart, 0), " ", suffixe)
  kpi_tile(format_val(valeur), libelle, couleur, icone, sous_titre = sous)
}


##### Bières — consommation #####

# Référentiel des vraies bières (catégories BIÈRES / ANCIENNES BIÈRES), pour
# écarter les autres boissons volumétriques (limonade, kéfir, cola, cidre...).
ref_bieres <- function(db_produits) {
  db_produits %>%
    filter(est_biere(CATEGORIE), !is.na(BOISSON), BOISSON != "") %>%
    distinct(BOISSON) %>%
    pull(BOISSON)
}

# Lignes de ticket correspondant à des bières, sur une fenêtre de dates.
tickets_bieres <- function(db_ticket, ref, d1, d2) {
  db_ticket %>%
    filter(BOISSON %in% ref, DATE >= as.Date(d1), DATE <= as.Date(d2),
           QUANTITE > 0) %>%
    mutate(LITRES = replace_na(VOLUME_TOT_L, 0),
           HEURE  = heure_service(TIMESTAMP))
}

# Consommation par bière sur une fenêtre : verres, litres, CA.
conso_bieres <- function(db_ticket, ref, d1, d2) {
  tickets_bieres(db_ticket, ref, d1, d2) %>%
    group_by(BOISSON) %>%
    summarise(VERRES = sum(QUANTITE, na.rm = TRUE),
              LITRES = sum(LITRES, na.rm = TRUE),
              CA     = sum(PRIX_TOTAL, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(LITRES))
}

# Consommation d'une semaine, comparée à la semaine précédente.
conso_bieres_comparee <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  act <- conso_bieres(db_ticket, ref, semaine, semaine + 6)
  prec <- conso_bieres(db_ticket, ref, semaine - 7, semaine - 1) %>%
    rename(VERRES_M1 = VERRES, LITRES_M1 = LITRES, CA_M1 = CA)

  full_join(act, prec, by = "BOISSON") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)),
           DELTA_L   = LITRES - LITRES_M1,
           EVO_PCT   = ifelse(LITRES_M1 > 0,
                              round(100 * DELTA_L / LITRES_M1, 1), NA_real_),
           STATUT    = case_when(LITRES_M1 == 0 & LITRES > 0 ~ "Nouveauté",
                                 LITRES == 0 & LITRES_M1 > 0 ~ "Arrêtée",
                                 TRUE ~ "En cours")) %>%
    arrange(desc(LITRES))
}

# Litres par heure de service, semaine courante et S-1.
conso_bieres_horaire <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  par_heure <- function(d1, d2, nom) {
    tickets_bieres(db_ticket, ref, d1, d2) %>%
      group_by(HEURE) %>%
      summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
      mutate(PERIODE = nom)
  }
  bind_rows(par_heure(semaine, semaine + 6, "Semaine"),
            par_heure(semaine - 7, semaine - 1, "S-1")) %>%
    filter(!is.na(HEURE), LITRES > 0)
}

# Litres par jour de semaine et par heure (heatmap).
conso_bieres_jour_heure <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  tickets_bieres(db_ticket, ref, semaine, semaine + 6) %>%
    mutate(JOUR = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1)) %>%
    group_by(JOUR, HEURE) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(HEURE))
}

# Historique hebdomadaire des litres servis.
evo_conso_bieres <- function(db_ticket, ref, n_semaines = 26, fin = NULL) {
  fin <- if (is.null(fin)) max(db_ticket$DATE, na.rm = TRUE) else as.Date(fin)
  debut <- floor_date(fin, "week", week_start = 1) - weeks(n_semaines - 1)
  tickets_bieres(db_ticket, ref, debut, fin) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    group_by(SEMAINE) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE),
              VERRES = sum(QUANTITE, na.rm = TRUE),
              CA     = sum(PRIX_TOTAL, na.rm = TRUE),
              NB_BIERES = n_distinct(BOISSON), .groups = "drop") %>%
    arrange(SEMAINE)
}

# Trajectoire hebdomadaire des principales bières de la semaine choisie :
# permet de voir lesquelles montent, lesquelles s'essoufflent.
evo_top_bieres <- function(db_ticket, ref, semaine, n_top = 5, n_semaines = 12) {
  semaine <- as.Date(semaine)
  top <- conso_bieres(db_ticket, ref, semaine, semaine + 6) %>%
    slice_head(n = n_top) %>%
    pull(BOISSON)
  if (length(top) == 0) return(tibble(SEMAINE = as.Date(character()),
                                      BOISSON = character(), LITRES = numeric()))

  debut <- semaine - weeks(n_semaines - 1)
  tickets_bieres(db_ticket, ref, debut, semaine + 6) %>%
    filter(BOISSON %in% top) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    group_by(SEMAINE, BOISSON) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    complete(SEMAINE, BOISSON, fill = list(LITRES = 0)) %>%
    mutate(BOISSON = factor(BOISSON, levels = top)) %>%
    arrange(BOISSON, SEMAINE)
}

# Répartition des formats servis (33 cl, 50 cl, dégustation...).
formats_bieres <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  tickets_bieres(db_ticket, ref, semaine, semaine + 6) %>%
    filter(!is.na(VOLUME_CL)) %>%
    group_by(FORMAT = paste0(VOLUME_CL, " cl")) %>%
    summarise(VERRES = sum(QUANTITE, na.rm = TRUE),
              LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(VERRES))
}

kpi_bieres_tiles <- function(comp, formats, horaire = NULL) {
  litres  <- sum(comp$LITRES);    litres_m1 <- sum(comp$LITRES_M1)
  verres  <- sum(comp$VERRES);    verres_m1 <- sum(comp$VERRES_M1)
  ca      <- sum(comp$CA);        ca_m1     <- sum(comp$CA_M1)
  nb      <- sum(comp$LITRES > 0); nb_m1    <- sum(comp$LITRES_M1 > 0)
  tanker  <- litres / 500   # un tanker = 500 L

  # Heure de plus forte consommation sur la semaine
  pic <- NULL
  if (!is.null(horaire) && nrow(horaire) > 0) {
    h <- horaire %>% filter(PERIODE == "Semaine") %>% slice_max(LITRES, n = 1,
                                                                with_ties = FALSE)
    if (nrow(h) == 1) pic <- h
  }

  div(
    class = "kpi-grid",
    tuile_evolution(litres, litres_m1, "Litres servis", "beer-mug-empty",
                    function(x) paste0(format(round(x)), " L")),
    tuile_evolution(verres, verres_m1, "Verres servis", "wine-glass"),
    tuile_evolution(ca, ca_m1, "CA bières", "euro-sign",
                    function(x) format_CA(x, -1)),
    tuile_ecart(nb, nb_m1, "Bières différentes", "list-ul"),
    kpi_tile(paste0(round(tanker, 2)), "Équivalent tanker (500 L)", CONSO_BRUN,
             "boxes-stacked", sous_titre = paste0(round(litres / 7), " L / jour")),
    kpi_tile(if (is.null(pic)) "—" else as.character(pic$HEURE),
             "Pic de consommation", "#8d7b68", "clock",
             sous_titre = if (is.null(pic)) NULL
                          else paste0(round(pic$LITRES), " L sur la semaine"))
  )
}

# Top bières par litres, colorées selon l'évolution vs S-1.
graph_top_bieres <- function(comp, n = 12) {
  if (is.null(comp) || nrow(comp) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière servie"))

  dat <- comp %>% filter(LITRES > 0) %>% slice_head(n = n) %>% arrange(LITRES)
  if (nrow(dat) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière servie"))
  dat <- dat %>% mutate(BOISSON = factor(BOISSON, levels = BOISSON))

  plot_ly(dat) %>%
    add_bars(y = ~BOISSON, x = ~LITRES_M1, orientation = "h", name = "S-1",
             marker = list(color = "#d3c0ac"),
             hovertemplate = ~paste0(BOISSON, " (S-1)<br>", round(LITRES_M1),
                                     " L<extra></extra>")) %>%
    add_bars(y = ~BOISSON, x = ~LITRES, orientation = "h", name = "Semaine",
             marker = list(color = CONSO_BRUN),
             hovertemplate = ~paste0(BOISSON, "<br>", round(LITRES), " L — ",
                                     VERRES, " verres<extra></extra>")) %>%
    layout(barmode = "group", xaxis = list(title = "Litres"),
           yaxis = list(title = ""), legend = list(orientation = "h"),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Trajectoire des principales bières : une ligne par bière, litres par semaine.
# La semaine analysée est marquée d'un point, pour situer le contexte.
graph_tendance_bieres <- function(evo, semaine = NULL) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune bière servie"))

  bieres <- levels(droplevels(evo$BOISSON))
  pal <- setNames(
    grDevices::colorRampPalette(
      c(CONSO_BRUN, CONSO_AMBRE, "#5B7B5A", "#2980b9", "#9b59b6"))(length(bieres)),
    bieres)

  p <- plot_ly()
  for (b in bieres) {
    sub <- evo %>% filter(BOISSON == b) %>% arrange(SEMAINE)
    # Le hover lit la COLONNE BOISSON, pas la variable de boucle `b` : dans une
    # formule (~), l'expression est évaluée après la boucle, si bien que toutes
    # les traces afficheraient le nom de la dernière bière.
    p <- p %>% add_lines(
      data = sub, x = ~SEMAINE, y = ~LITRES, name = b, legendgroup = b,
      line = list(color = pal[[b]], width = 2.5),
      hovertemplate = ~paste0(BOISSON, "<br>Sem. ", format(SEMAINE, "%d/%m"),
                              "<br>", round(LITRES), " L<extra></extra>"))
    if (!is.null(semaine)) {
      pt <- sub %>% filter(SEMAINE == as.Date(semaine))
      if (nrow(pt) > 0)
        p <- p %>% add_markers(data = pt, x = ~SEMAINE, y = ~LITRES,
                               name = b, legendgroup = b, showlegend = FALSE,
                               marker = list(color = pal[[b]], size = 9),
                               hoverinfo = "skip")
    }
  }
  p %>% layout(xaxis = list(title = ""),
               yaxis = list(title = "Litres par semaine", rangemode = "tozero"),
               legend = list(orientation = "h"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Heatmap jour x heure des litres servis.
graph_heatmap_bieres <- function(jh) {
  if (is.null(jh) || nrow(jh) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  # pivot_wider ordonne les colonnes par ordre d'APPARITION dans les données
  # (18h, 19h, ... puis 11h). On complète la grille puis on resélectionne les
  # colonnes dans l'ordre des niveaux du facteur (heure de service).
  jh <- jh %>%
    mutate(HEURE = droplevels(HEURE)) %>%
    complete(JOUR, HEURE, fill = list(LITRES = 0))
  heures <- levels(jh$HEURE)

  mat <- jh %>%
    pivot_wider(names_from = HEURE, values_from = LITRES, values_fill = 0) %>%
    arrange(JOUR)
  z <- as.matrix(mat[, heures, drop = FALSE])

  plot_ly(x = heures, y = as.character(mat$JOUR), z = z, type = "heatmap",
          colorscale = list(c(0, "#f2efe6"), c(1, CONSO_BRUN)),
          hovertemplate = "%{y} — %{x}<br>%{z:.0f} L<extra></extra>") %>%
    layout(xaxis = list(title = "", side = "top"),
           yaxis = list(title = "", autorange = "reversed"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Historique hebdomadaire : litres en barres, nombre de bières en ligne.
graph_evo_conso_bieres <- function(evo, semaine = NULL) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  couleurs <- if (is.null(semaine)) CONSO_BRUN
              else ifelse(evo$SEMAINE == as.Date(semaine), CONSO_AMBRE, CONSO_BRUN)

  plot_ly(evo) %>%
    add_bars(x = ~SEMAINE, y = ~LITRES, name = "Litres",
             marker = list(color = couleurs),
             hovertemplate = ~paste0("Sem. ", format(SEMAINE, "%d/%m/%y"), "<br>",
                                     round(LITRES), " L — ", VERRES,
                                     " verres<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~NB_BIERES, name = "Bières à la carte",
              yaxis = "y2", line = list(color = "#5B7B5A", width = 2),
              hovertemplate = ~paste0(NB_BIERES, " bières<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "Litres"),
           yaxis2 = list(title = "Nb de bières", overlaying = "y", side = "right",
                         showgrid = FALSE, rangemode = "tozero"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Formats servis (33 cl, 50 cl, dégustation...).
graph_formats_bieres <- function(formats) {
  if (is.null(formats) || nrow(formats) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  dat <- formats %>% arrange(VERRES) %>%
    mutate(FORMAT = factor(FORMAT, levels = FORMAT))
  plot_ly(dat) %>%
    add_bars(y = ~FORMAT, x = ~VERRES, orientation = "h",
             marker = list(color = CONSO_AMBRE),
             hovertemplate = ~paste0(FORMAT, "<br>", VERRES, " verres — ",
                                     round(LITRES), " L<extra></extra>")) %>%
    layout(xaxis = list(title = "Verres servis"), yaxis = list(title = ""),
           showlegend = FALSE, margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

table_conso_bieres <- function(comp) {
  if (is.null(comp) || nrow(comp) == 0) return(tibble(Bière = character()))

  # Totaux calculés hors du transmute : une condition scalaire dans un
  # `ifelse` renverrait une valeur unique, recyclée sur toutes les lignes.
  total    <- sum(comp$LITRES, na.rm = TRUE)
  total_m1 <- sum(comp$LITRES_M1, na.rm = TRUE)

  comp %>%
    mutate(PART    = if (total > 0) 100 * LITRES / total else NA_real_,
           PART_M1 = if (total_m1 > 0) 100 * LITRES_M1 / total_m1 else NA_real_) %>%
    transmute(Bière      = BOISSON,
              Verres     = VERRES,
              Litres     = round(LITRES),
              Part       = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              `CA`       = format_CA(CA, -1),
              `Litres S-1` = round(LITRES_M1),
              `Part S-1` = ifelse(is.na(PART_M1), "—",
                                  paste0(round(PART_M1, 1), " %")),
              `Évol.`    = ifelse(is.na(EVO_PCT), "—",
                                  paste0(ifelse(EVO_PCT >= 0, "+", ""),
                                         EVO_PCT, " %")),
              Statut     = STATUT)
}


##### Focaccias #####

# Décompose un PRODUIT_FULL de focaccia en base + options.
# Les libellés viennent de la caisse :
#   "Focaccia du moment + Options focaccias: + SUPPL. Fromage + SUPPL. Viande
#    + Option pikant: !! SPICY HOT !!"
parse_focaccia <- function(pf) {
  tibble(
    BASE = case_when(
      str_detect(pf, regex("brunch", ignore_case = TRUE))         ~ "Brunch",
      str_detect(pf, regex("patates douces", ignore_case = TRUE)) ~ "Patates douces",
      str_detect(pf, regex("du moment", ignore_case = TRUE))      ~ "Du moment",
      TRUE                                                        ~ "Autre"),
    FROMAGE = str_detect(pf, fixed("SUPPL. Fromage")),
    VIANDE  = str_detect(pf, fixed("SUPPL. Viande")),
    SPICY   = str_detect(pf, fixed("SPICY HOT"))
  ) %>%
    mutate(GARNITURE = case_when(FROMAGE & VIANDE ~ "Fromage + Viande",
                                 FROMAGE          ~ "Fromage",
                                 VIANDE           ~ "Viande",
                                 TRUE             ~ "Nature"),
           VARIANTE = paste0(GARNITURE, ifelse(SPICY, " + Spicy", "")))
}

ORDRE_GARNITURES <- c("Nature", "Fromage", "Viande", "Fromage + Viande")

# Lignes de focaccia sur une fenêtre, décomposées en options.
# On écarte les remises et lignes négatives, qui ne sont pas des ventes.
conso_focaccias <- function(db_produits, d1, d2) {
  db <- db_produits %>%
    filter(str_detect(tolower(PRODUIT_FULL), "focaccia"),
           !str_detect(tolower(PRODUIT_FULL), "discount|% sur produit"),
           QUANTITE > 0, DATE >= as.Date(d1), DATE <= as.Date(d2))
  if (nrow(db) == 0)
    return(tibble(DATE = as.Date(character()), BASE = character(),
                  FROMAGE = logical(), VIANDE = logical(), SPICY = logical(),
                  GARNITURE = character(), VARIANTE = character(),
                  QUANTITE = numeric(), CA = numeric()))
  bind_cols(db %>% select(DATE, QUANTITE, CA = CA_HTVA),
            parse_focaccia(db$PRODUIT_FULL)) %>%
    mutate(GARNITURE = factor(GARNITURE, levels = ORDRE_GARNITURES))
}

# Synthèse d'une semaine, avec la semaine précédente pour comparaison.
focaccias_semaine <- function(db_produits, semaine) {
  semaine <- as.Date(semaine)
  list(semaine = semaine,
       act  = conso_focaccias(db_produits, semaine, semaine + 6),
       prec = conso_focaccias(db_produits, semaine - 7, semaine - 1))
}

# Nombre de focaccias par jour de la semaine choisie.
focaccias_par_jour <- function(fo, semaine) {
  semaine <- as.Date(semaine)
  jours <- tibble(DATE = seq(semaine, semaine + 6, by = "day")) %>%
    mutate(JOUR = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1))
  fo %>%
    group_by(DATE) %>%
    summarise(QUANTITE = sum(QUANTITE), CA = sum(CA), .groups = "drop") %>%
    right_join(jours, by = "DATE") %>%
    mutate(across(c(QUANTITE, CA), ~replace_na(., 0))) %>%
    arrange(DATE)
}

# Répartition par garniture x spicy.
focaccias_variantes <- function(fo) {
  if (nrow(fo) == 0)
    return(tibble(GARNITURE = factor(character(), levels = ORDRE_GARNITURES),
                  SPICY = logical(), QUANTITE = numeric()))
  fo %>%
    group_by(GARNITURE, SPICY) %>%
    summarise(QUANTITE = sum(QUANTITE), .groups = "drop")
}

# Historique hebdomadaire : volumes et taux d'options.
evo_focaccias <- function(db_produits, n_semaines = 26, fin = NULL) {
  fin <- if (is.null(fin)) max(db_produits$DATE, na.rm = TRUE) else as.Date(fin)
  debut <- floor_date(fin, "week", week_start = 1) - weeks(n_semaines - 1)
  # Les quantités par option sont calculées AVANT le regroupement : dans un
  # summarise(), `QUANTITE = sum(QUANTITE)` écrase la colonne, et un
  # `QUANTITE[FROMAGE]` écrit ensuite indexerait le total (scalaire) au lieu
  # des lignes — ce qui ne produit que des NA.
  conso_focaccias(db_produits, debut, fin) %>%
    mutate(SEMAINE   = floor_date(DATE, "week", week_start = 1),
           Q_FROMAGE = QUANTITE * FROMAGE,
           Q_VIANDE  = QUANTITE * VIANDE,
           Q_SPICY   = QUANTITE * SPICY) %>%
    group_by(SEMAINE) %>%
    summarise(QUANTITE  = sum(QUANTITE, na.rm = TRUE),
              CA        = sum(CA, na.rm = TRUE),
              Q_FROMAGE = sum(Q_FROMAGE, na.rm = TRUE),
              Q_VIANDE  = sum(Q_VIANDE, na.rm = TRUE),
              Q_SPICY   = sum(Q_SPICY, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(PCT_FROMAGE = ratio_pct(Q_FROMAGE, QUANTITE),
           PCT_VIANDE  = ratio_pct(Q_VIANDE,  QUANTITE),
           PCT_SPICY   = ratio_pct(Q_SPICY,   QUANTITE)) %>%
    arrange(SEMAINE)
}

kpi_focaccias_tiles <- function(fs) {
  act <- fs$act; prec <- fs$prec
  q  <- sum(act$QUANTITE);  q_m1  <- sum(prec$QUANTITE)
  ca <- sum(act$CA);        ca_m1 <- sum(prec$CA)
  pct <- function(d, col) if (sum(d$QUANTITE) > 0)
    100 * sum(d$QUANTITE[d[[col]]]) / sum(d$QUANTITE) else NA_real_
  jours_ouverts <- n_distinct(act$DATE)

  div(
    class = "kpi-grid",
    tuile_evolution(q, q_m1, "Focaccias vendues", "bread-slice"),
    tuile_evolution(ca, ca_m1, "CA focaccias", "euro-sign",
                    function(x) format_CA(x, -1)),
    kpi_tile(if (jours_ouverts > 0) format(round(q / jours_ouverts, 1)) else "—",
             "Par jour d'ouverture", CONSO_BRUN, "gauge-high",
             sous_titre = paste0(jours_ouverts, " jours servis")),
    tuile_evolution(pct(act, "FROMAGE"), pct(prec, "FROMAGE"),
                    "Avec fromage", "cheese", function(x) format_pct(x)),
    tuile_evolution(pct(act, "VIANDE"), pct(prec, "VIANDE"),
                    "Avec viande", "drumstick-bite", function(x) format_pct(x)),
    tuile_evolution(pct(act, "SPICY"), pct(prec, "SPICY"),
                    "Spicy hot", "pepper-hot", function(x) format_pct(x))
  )
}

# Rythme sur la semaine : quantités par jour (+ rappel de S-1 en pointillé).
graph_focaccias_jour <- function(jour_act, jour_prec) {
  if (is.null(jour_act) || nrow(jour_act) == 0)
    return(plotly_empty() %>% layout(title = "Aucune focaccia vendue"))

  # `add_lines` retrie les points par x : avec des jours en texte, la trace S-1
  # ressortait dans l'ordre alphabétique (dimanche, jeudi, lundi...). On passe
  # donc un FACTEUR ORDONNÉ, dont le tri suit les niveaux lundi -> dimanche.
  jours <- factor(as.character(jour_act$JOUR), levels = as.character(jour_act$JOUR))

  p <- plot_ly() %>%
    add_bars(x = jours, y = jour_act$QUANTITE, name = "Semaine",
             marker = list(color = CONSO_BRUN),
             hovertemplate = paste0(jours, "<br>", jour_act$QUANTITE,
                                    " focaccias<extra></extra>"))
  if (!is.null(jour_prec) && nrow(jour_prec) == nrow(jour_act))
    p <- p %>% add_lines(x = jours, y = jour_prec$QUANTITE, name = "S-1",
                         line = list(color = "#8d7b68", dash = "dot", width = 2),
                         hovertemplate = paste0("S-1 : ", jour_prec$QUANTITE,
                                                "<extra></extra>"))
  p %>% layout(xaxis = list(title = "", categoryorder = "array",
                            categoryarray = levels(jours)),
               yaxis = list(title = "Focaccias"),
               legend = list(orientation = "h"), bargap = 0.35,
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Popularité des variantes : garniture en barres, part spicy empilée.
graph_variantes_focaccias <- function(var) {
  if (is.null(var) || nrow(var) == 0)
    return(plotly_empty() %>% layout(title = "Aucune focaccia vendue"))

  dat <- var %>%
    mutate(GARNITURE = factor(as.character(GARNITURE),
                              levels = ORDRE_GARNITURES))
  doux  <- dat %>% filter(!SPICY)
  epice <- dat %>% filter(SPICY)

  plot_ly() %>%
    add_bars(x = as.character(doux$GARNITURE), y = doux$QUANTITE,
             name = "Standard", marker = list(color = CONSO_AMBRE),
             hovertemplate = paste0(doux$GARNITURE, "<br>", doux$QUANTITE,
                                    "<extra></extra>")) %>%
    add_bars(x = as.character(epice$GARNITURE), y = epice$QUANTITE,
             name = "Spicy hot", marker = list(color = "#c0392b"),
             hovertemplate = paste0(epice$GARNITURE, " (spicy)<br>",
                                    epice$QUANTITE, "<extra></extra>")) %>%
    layout(barmode = "stack",
           xaxis = list(title = "", categoryorder = "array",
                        categoryarray = ORDRE_GARNITURES),
           yaxis = list(title = "Focaccias"),
           legend = list(orientation = "h"), bargap = 0.35,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Historique : volumes hebdo en barres.
graph_evo_focaccias <- function(evo, semaine = NULL) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))
  couleurs <- if (is.null(semaine)) CONSO_BRUN
              else ifelse(evo$SEMAINE == as.Date(semaine), CONSO_AMBRE, CONSO_BRUN)

  plot_ly(evo) %>%
    add_bars(x = ~SEMAINE, y = ~QUANTITE, name = "Focaccias",
             marker = list(color = couleurs),
             hovertemplate = ~paste0("Sem. ", format(SEMAINE, "%d/%m/%y"), "<br>",
                                     QUANTITE, " focaccias — ",
                                     format_CA(CA, -1), "<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "Focaccias"),
           showlegend = FALSE,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Taux d'options dans le temps (fromage / viande / spicy).
graph_options_focaccias <- function(evo) {
  if (is.null(evo) || nrow(evo) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  plot_ly(evo) %>%
    add_lines(x = ~SEMAINE, y = ~PCT_FROMAGE, name = "Fromage",
              line = list(color = "#d4ac0d", width = 2),
              hovertemplate = ~paste0("Fromage %{y:.0f} %<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~PCT_VIANDE, name = "Viande",
              line = list(color = "#8d5524", width = 2),
              hovertemplate = ~paste0("Viande %{y:.0f} %<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~PCT_SPICY, name = "Spicy hot",
              line = list(color = "#c0392b", width = 2),
              hovertemplate = ~paste0("Spicy %{y:.0f} %<extra></extra>")) %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = "% des focaccias", ticksuffix = " %",
                        rangemode = "tozero"),
           legend = list(orientation = "h"), hovermode = "x unified",
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Détail par variante, avec comparaison S-1.
table_focaccias <- function(fs) {
  agg <- function(d) d %>% group_by(VARIANTE) %>%
    summarise(Q = sum(QUANTITE), CA = sum(CA), .groups = "drop")
  act <- agg(fs$act)
  prec <- agg(fs$prec) %>% rename(Q_M1 = Q, CA_M1 = CA)
  if (nrow(act) == 0 && nrow(prec) == 0) return(tibble(Variante = character()))

  res <- full_join(act, prec, by = "VARIANTE") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    arrange(desc(Q))

  # Le total est calculé hors du mutate : `ifelse(sum(Q) > 0, ...)` renverrait
  # une valeur de longueur 1 (la condition est un scalaire), recyclée sur
  # toutes les lignes — toutes les parts afficheraient le même pourcentage.
  total <- sum(res$Q)

  res %>%
    mutate(PART = if (total > 0) 100 * Q / total else NA_real_,
           EVO = ifelse(Q_M1 > 0, round(100 * (Q - Q_M1) / Q_M1, 1), NA_real_)) %>%
    transmute(Variante   = VARIANTE,
              Quantité   = Q,
              # une décimale : à l'entier, huit petites parts arrondies
              # chacune vers le haut faisaient une somme à 102 %
              `Part`     = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              `CA`       = format_CA(CA, -1),
              `Qté S-1`  = Q_M1,
              `Évol.`    = ifelse(is.na(EVO), "—",
                                  paste0(ifelse(EVO >= 0, "+", ""), EVO, " %")))
}

##### Focaccias — aide à la production #####
# Combien produire de chaque préparation pour la semaine à venir ?
#
# Assiette de chaque ingrédient (quelles focaccias le consomment) :
#   Crémeux et Légume -> TOUTES les focaccias, ils sont dans la recette de base
#   Fromage           -> celles qui portent le supplément fromage (« full »
#                        compris, puisqu'une focaccia complète en contient)
#   Viande            -> idem avec le supplément viande
#   Autre             -> ligne libre, rien n'est préchargé
#
# NB : la caisse ne connaît pas de « supplément légume » — les seules options
# sont Fromage, Viande et Spicy. Le légume est donc traité comme le crémeux,
# c'est-à-dire présent dans toutes les recettes.

INGREDIENTS_FOCACCIA <- tibble::tribble(
  ~ID, ~NOM,       ~ASSIETTE,  ~PORTION,
  1L,  "Crémeux",  "toutes",   40,
  2L,  "Légume",   "toutes",   60,
  3L,  "Fromage",  "fromage",  30,
  4L,  "Viande",   "viande",   50,
  5L,  "Autre",    NA,         NA
)

# Dernières semaines ENTIÈREMENT couvertes par les données. Une semaine
# tronquée par la fin du jeu de données tirerait la moyenne vers le bas.
semaines_completes <- function(dates, n = 3, fin = NULL) {
  dates <- as.Date(dates)
  fin <- if (is.null(fin)) max(dates, na.rm = TRUE) else as.Date(fin)
  sems <- sort(unique(floor_date(dates, "week", week_start = 1)))
  tail(sems[sems + 6 <= fin], n)
}

# Base de la carte production : nombre moyen de focaccias par semaine
# concernées par chaque ingrédient, et portion par défaut.
production_focaccias_base <- function(db_produits, n_semaines = 3, fin = NULL,
                                      marge = 1.1) {
  base <- INGREDIENTS_FOCACCIA %>% mutate(FOCACCIAS = NA_real_, SEMAINES = 0L)
  if (is.null(db_produits) || nrow(db_produits) == 0) return(base)

  fin_donnees <- if (is.null(fin)) max(db_produits$DATE, na.rm = TRUE)
                 else as.Date(fin)
  if (!is.finite(fin_donnees)) return(base)

  fo_all <- conso_focaccias(db_produits, as.Date("1900-01-01"), fin_donnees)
  if (nrow(fo_all) == 0) return(base)

  sems <- semaines_completes(fo_all$DATE, n_semaines, fin)
  if (length(sems) == 0) return(base)
  
  fo_sub <- fo_all %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) |> 
    filter(SEMAINE %in% sems)
  
  # Max sur les trois semaines, par ingrédient
  max_toutes <- fo_sub |> group_by(SEMAINE) |> 
    summarise(n = sum(QUANTITE)) |> pull(n) |> max()
  max_fromage <- fo_sub |> filter(FROMAGE) |> group_by(SEMAINE) |> 
    summarise(n = sum(QUANTITE)) |> pull(n) |> max()
  max_viande <- fo_sub |> filter(VIANDE) |> group_by(SEMAINE) |> 
    summarise(n = sum(QUANTITE)) |> pull(n) |> max()
  
  base |> 
    mutate(FOCACCIAS = case_when(
      ASSIETTE == "toutes" ~ round(max_toutes * marge),
      ASSIETTE == "fromage" ~ round(max_fromage * marge),
      ASSIETTE == "viande" ~ round(max_viande * marge),
      TRUE ~ NA_real_),
      SEMAINES  = length(sems)
    )
}

# Quantité en grammes, basculée en kilos quand ça devient lourd à lire.
format_qte_g <- function(x) {
  if (length(x) == 0 || is.na(x)) return("—")
  if (abs(x) >= 1000) paste0(format(round(x / 1000, 2), nsmall = 2), " kg")
  else paste0(round(x), " g")
}

#### REFONTE — Volet "Pizzwanze" ####
# La Pizzwanze est une soirée pizza qui revient toutes les trois à quatre
# semaines, le mardi soir. Quelques pizzas sont là à chaque fois, le reste
# change au gré des produits de saison.
#
# Détection : il n'existe pas de catégorie dédiée, on part donc du NOM du
# produit — tout ce qui contient « pizza », plus le Khachapuri, une pizza
# géorgienne qui ne porte pas le mot.
#
# Une soirée se reconnaît à sa CARTE : on y propose plusieurs pizzas. Les
# autres jours où l'on voit passer de la pizza n'ont qu'une seule référence
# (part vendue le lendemain, slices du Bal National...). Le critère est donc
# « au moins deux références », et non « c'est un mardi » : les soirées
# exceptionnelles hors mardi sont ainsi capturées elles aussi. Sur
# l'historique disponible, la règle retient exactement les 32 mardis.

# Un produit est une pizza si son nom le dit, Khachapuri compris.
# `as.character` d'abord : la colonne peut arriver en facteur, et un NA nu est
# logique — `replace_na` refuserait alors la valeur de remplacement texte.
est_pizza <- function(x) {
  str_detect(tolower(replace_na(as.character(x), "")), "pizz|khachapuri")
}

# Seuils de détection d'une soirée (documentés pour pouvoir être ajustés).
PIZZWANZE_MIN_REFS   <- 2   # une soirée propose une carte, pas un seul produit
PIZZWANZE_MIN_PIZZAS <- 5   # garde-fou volume, pour écarter les restes

# Dates des soirées Pizzwanze, de la plus ancienne à la plus récente.
soirees_pizzwanze <- function(db_produits,
                              min_refs = PIZZWANZE_MIN_REFS,
                              min_pizzas = PIZZWANZE_MIN_PIZZAS) {
  db_produits %>%
    filter(est_pizza(PRODUIT_FULL), QUANTITE > 0) %>%
    filter(!str_detect(PRODUIT_FULL,"Slice")) |> 
    group_by(DATE) %>%
    summarise(PIZZAS = sum(QUANTITE, na.rm = TRUE),
              N_REF  = n_distinct(PRODUIT_FULL), .groups = "drop") %>%
    filter(N_REF >= min_refs, PIZZAS >= min_pizzas) %>%
    arrange(DATE) %>%
    pull(DATE)
}

# Ventes de pizzas d'une ou plusieurs soirées, agrégées par date et produit.
conso_pizzas <- function(db_produits, dates) {
  dates <- as.Date(dates)
  db_produits %>%
    filter(est_pizza(PRODUIT_FULL), QUANTITE > 0, DATE %in% dates) %>%
    group_by(DATE, PIZZA = PRODUIT_FULL) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA       = sum(CA_HTVA, na.rm = TRUE), .groups = "drop")
}

# Statut de chaque pizza sur l'ensemble des soirées : incontournable, régulière
# ou occasionnelle, avec ses dates de première et dernière apparition.
statut_pizzas <- function(db_produits, soirees) {
  n_soirees <- length(soirees)
  if (n_soirees == 0)
    return(tibble(PIZZA = character(), N_SOIREES = integer(),
                  PREMIERE = as.Date(character()), DERNIERE = as.Date(character()),
                  QUANTITE = numeric(), STATUT = character()))

  conso_pizzas(db_produits, soirees) %>%
    group_by(PIZZA) %>%
    summarise(N_SOIREES = n_distinct(DATE),
              PREMIERE  = min(DATE),
              DERNIERE  = max(DATE),
              QUANTITE  = sum(QUANTITE, na.rm = TRUE), .groups = "drop") %>%
    mutate(PART_SOIREES = N_SOIREES / n_soirees,
           STATUT = case_when(PART_SOIREES >= 2/3 ~ "Incontournable",
                              PART_SOIREES >= 1/3 ~ "Régulière",
                              TRUE                ~ "Occasionnelle")) %>%
    arrange(desc(N_SOIREES), desc(QUANTITE))
}

STATUTS_PIZZA <- c("Incontournable", "Régulière", "Occasionnelle", "Nouveauté")
PAL_STATUT_PIZZA <- c("Incontournable" = "#732c02", "Régulière" = "#d98236",
                      "Occasionnelle"  = "#d3c0ac", "Nouveauté" = "#5B7B5A")

# Synthèse d'une soirée : ses ventes, celles de la soirée précédente, et le
# statut de chaque pizza. Une pizza dont la première apparition est ce soir-là
# est marquée « Nouveauté ».
pizzwanze_soiree <- function(db_produits, db_ticket, date_soiree, soirees = NULL) {
  date_soiree <- as.Date(date_soiree)
  if (is.null(soirees)) soirees <- soirees_pizzwanze(db_produits)

  precedente <- soirees[soirees < date_soiree]
  precedente <- if (length(precedente) == 0) NA else max(precedente)
  
  pic <- pizzas_par_heure(db_ticket, date_soiree) |> 
    arrange(-QUANTITE) |> filter(row_number() == 1) |> pull(HEURE)

  statuts <- statut_pizzas(db_produits, soirees)

  act <- conso_pizzas(db_produits, date_soiree) %>%
    left_join(statuts %>% select(PIZZA, N_SOIREES, PREMIERE, STATUT), by = "PIZZA") %>%
    mutate(NOUVEAUTE = !is.na(PREMIERE) & PREMIERE == date_soiree,
           STATUT_SOIR = ifelse(NOUVEAUTE, "Nouveauté", STATUT)) %>%
    arrange(desc(QUANTITE))

  prec <- if (is.na(precedente)) act[0, c("PIZZA", "QUANTITE", "CA")]
          else conso_pizzas(db_produits, precedente) %>% select(PIZZA, QUANTITE, CA)

  list(date = date_soiree,
       precedente = precedente,
       ecart_jours = if (is.na(precedente)) NA_real_
                     else as.numeric(date_soiree - precedente),
       act = act, prec = prec, statuts = statuts, pic = pic)
}

# Historique : une ligne par soirée, avec le nombre de nouveautés et l'écart
# depuis la soirée précédente.
historique_pizzwanze <- function(db_produits, soirees = NULL) {
  if (is.null(soirees)) soirees <- soirees_pizzwanze(db_produits)
  if (length(soirees) == 0)
    return(tibble(DATE = as.Date(character()), PIZZAS = numeric(),
                  CA = numeric(), N_REF = integer(), NOUVEAUTES = integer(),
                  ECART = numeric()))

  detail <- conso_pizzas(db_produits, soirees)
  premieres <- detail %>% group_by(PIZZA) %>%
    summarise(PREMIERE = min(DATE), .groups = "drop")

  detail %>%
    left_join(premieres, by = "PIZZA") %>%
    group_by(DATE) %>%
    summarise(PIZZAS     = sum(QUANTITE, na.rm = TRUE),
              CA         = sum(CA, na.rm = TRUE),
              N_REF      = n_distinct(PIZZA),
              NOUVEAUTES = sum(PREMIERE == DATE), .groups = "drop") %>%
    arrange(DATE) %>%
    mutate(ECART = as.numeric(DATE - lag(DATE)))
}

# Ventes de pizzas heure par heure sur une soirée (source : tickets).
pizzas_par_heure <- function(db_ticket, date_soiree) {
  date_soiree <- as.Date(date_soiree)
  db_ticket %>%
    filter(est_pizza(PRODUIT_FULL), DATE == date_soiree, QUANTITE > 0) %>%
    mutate(HEURE = heure_service(TIMESTAMP)) %>%
    group_by(HEURE) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(HEURE), QUANTITE > 0)
}

##### Tuiles KPI #####

kpi_pizzwanze_tiles <- function(ps) {
  act <- ps$act; prec <- ps$prec; pic <- ps$pic
  q    <- sum(act$QUANTITE);  q_m1  <- sum(prec$QUANTITE)
  ca   <- sum(act$CA);        ca_m1 <- sum(prec$CA)
  nref <- nrow(act);          nref_m1 <- nrow(prec)
  nouv <- sum(act$NOUVEAUTE, na.rm = TRUE)
  vedette <- if (nrow(act) > 0) act$PIZZA[1] else NULL

  div(
    class = "kpi-grid",
    tuile_evolution(q, q_m1, "Pizzas vendues", "pizza-slice",
                    suffixe = "vs soirée précédente"),
    tuile_evolution(ca, ca_m1, "CA pizzas", "euro-sign",
                    function(x) format_CA(x, -1),
                    suffixe = "vs soirée précédente"),
    tuile_ecart(nref, nref_m1, "Pizzas à la carte", "list-ul",
                    suffixe = "vs soirée précédente"),
    kpi_tile(if (is.na(ps$ecart_jours)) "—" else paste0(round(ps$ecart_jours), " j"),
             "Depuis la précédente", COUL_NEUTRE, "calendar-day",
             sous_titre = if (is.na(ps$precedente)) "première soirée"
             else format(ps$precedente, "%d/%m/%Y")),
    kpi_tile(if (is.null(vedette)) "—" else str_trunc(vedette, 18),
             "Pizza vedette", CONSO_BRUN, "trophy",
             sous_titre = if (is.null(vedette)) NULL
                          else paste0(act$QUANTITE[1], " vendues")),
    kpi_tile(as.character(pic), "Pic de consommation", "#8d7b68", "clock",
             sous_titre = "")
  )
}

##### Graphiques #####

# Historique : pizzas vendues par soirée, la soirée analysée mise en avant.
graph_evo_pizzwanze <- function(hist, soiree = NULL) {
  if (is.null(hist) || nrow(hist) == 0)
    return(plotly_empty() %>% layout(title = "Aucune soirée détectée"))

  couleurs <- if (is.null(soiree)) CONSO_BRUN
              else ifelse(hist$DATE == as.Date(soiree), CONSO_AMBRE, CONSO_BRUN)

  plot_ly(hist) %>%
    add_bars(x = ~DATE, y = ~PIZZAS, name = "Pizzas",
             marker = list(color = couleurs),
             hovertemplate = ~paste0(format(DATE, "%a %d/%m/%Y"), "<br>",
                                     PIZZAS, " pizzas — ", N_REF, " références<br>",
                                     format_CA(CA, -1),
                                     "<extra></extra>")) %>%
    # add_markers(x = ~DATE, y = ~PIZZAS, name = "Nouveautés",
    #             marker = list(color = COUL_VERT, size = 8, symbol = "diamond"),
    #             text = ~NOUVEAUTES,
    #             hovertemplate = ~paste0(NOUVEAUTES, " nouveauté(s)<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "Pizzas vendues"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Répartition de la soirée : une barre par pizza, colorée selon son statut.
graph_pizzas_soiree <- function(ps) {
  act <- ps$act
  if (is.null(act) || nrow(act) == 0)
    return(plotly_empty() %>% layout(title = "Aucune pizza ce soir-là"))

  dat <- act %>%
    arrange(QUANTITE) %>%
    mutate(PIZZA_LBL = factor(str_trunc(PIZZA, 30), levels = str_trunc(PIZZA, 30)),
           STATUT_SOIR = factor(STATUT_SOIR, levels = STATUTS_PIZZA))

  p <- plot_ly()
  for (st in STATUTS_PIZZA) {
    sub <- dat %>% filter(STATUT_SOIR == st)
    if (nrow(sub) == 0) next
    p <- p %>% add_bars(
      data = sub, y = ~PIZZA_LBL, x = ~QUANTITE, orientation = "h", name = st,
      marker = list(color = PAL_STATUT_PIZZA[[st]]),
      hovertemplate = ~paste0(PIZZA, "<br>", QUANTITE, " vendues — ",
                              format_CA(CA, -1), "<br>", st, ", vue ",
                              N_SOIREES, " fois<extra></extra>"))
  }
  p %>% layout(barmode = "stack", xaxis = list(title = "Pizzas vendues"),
               yaxis = list(title = ""), 
               # legend = list(orientation = "h"),
               legend = list(yref = "container", y = 0, yanchor = "bottom"),
               margin = list(l = 10),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Heatmap pizza x soirée : d'un coup d'œil, qui revient et qui ne fait que
# passer. Les pizzas les plus présentes sont en haut.
graph_carte_pizzwanze <- function(db_produits, soirees, n_soirees = NULL) {
  if (length(soirees) == 0)
    return(plotly_empty() %>% layout(title = "Aucune soirée détectée"))

  # n_soirees = NULL -> tout l'historique. C'est le réglage le plus parlant :
  # la carte s'est stabilisée sur les dernières soirées, les allées et venues
  # de pizzas ne se voient qu'en remontant plus loin.
  dernieres <- sort(soirees)
  if (!is.null(n_soirees)) dernieres <- tail(dernieres, n_soirees)
  detail <- conso_pizzas(db_produits, dernieres)
  if (nrow(detail) == 0)
    return(plotly_empty() %>% layout(title = "Aucune donnée"))

  ordre <- detail %>%
    group_by(PIZZA) %>%
    summarise(n = n_distinct(DATE), q = sum(QUANTITE), .groups = "drop") %>%
    arrange(n, q) %>%
    pull(PIZZA)

  grille <- detail %>%
    mutate(PIZZA = factor(PIZZA, levels = ordre)) %>%
    complete(PIZZA, DATE = dernieres, fill = list(QUANTITE = 0)) %>%
    arrange(PIZZA, DATE)

  mat <- grille %>%
    select(PIZZA, DATE, QUANTITE) %>%
    pivot_wider(names_from = DATE, values_from = QUANTITE, values_fill = 0) %>%
    arrange(PIZZA)
  cols <- as.character(dernieres)
  z <- as.matrix(mat[, cols, drop = FALSE])

  plot_ly(x = format(dernieres, "%d/%m/%y"),
          y = str_trunc(as.character(mat$PIZZA), 30), z = z,
          type = "heatmap", colorscale = list(c(0, "#f2efe6"), c(1, CONSO_BRUN)),
          hovertemplate = "%{y}<br>%{x} : %{z:.0f} vendues<extra></extra>") %>%
    layout(xaxis = list(title = "", side = "top"), yaxis = list(title = ""),
           margin = list(l = 10),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Rythme de la soirée : pizzas servies heure par heure.
graph_pizzas_heure <- function(par_heure) {
  if (is.null(par_heure) || nrow(par_heure) == 0)
    return(plotly_empty() %>% layout(title = "Pas de détail horaire"))

  plot_ly(par_heure) %>%
    add_bars(x = ~HEURE, y = ~QUANTITE,
             marker = list(color = CONSO_AMBRE),
             hovertemplate = ~paste0(HEURE, "<br>", QUANTITE,
                                     " pizzas<extra></extra>")) %>%
    layout(xaxis = list(title = "Heure de service"),
           yaxis = list(title = "Pizzas"), showlegend = FALSE, bargap = 0.3,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Détail par pizza, avec la part de la soirée et l'écart vs la précédente.
table_pizzwanze <- function(ps) {
  act <- ps$act
  if (is.null(act) || nrow(act) == 0) return(tibble(Pizza = character()))

  total <- sum(act$QUANTITE)
  act %>%
    left_join(ps$prec %>% rename(Q_M1 = QUANTITE) %>% select(PIZZA, Q_M1),
              by = "PIZZA") %>%
    mutate(Q_M1 = replace_na(Q_M1, 0),
           PART = if (total > 0) 100 * QUANTITE / total else NA_real_,
           EVO  = ifelse(Q_M1 > 0, round(100 * (QUANTITE - Q_M1) / Q_M1, 1),
                         NA_real_)) %>%
    transmute(Pizza      = PIZZA,
              Quantité   = QUANTITE,
              Part       = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              CA         = format_CA(CA, -1),
              # Statut     = STATUT_SOIR,
              # `Soirées`  = N_SOIREES,
              `Qté préc.` = Q_M1,
              `Évol.`    = ifelse(is.na(EVO), "—",
                                  paste0(ifelse(EVO >= 0, "+", ""), EVO, " %")))
}

#### REFONTE — Authentification persistante ####
# Jeton « rester connecté », déposé en cookie, pour éviter de retaper le mot
# de passe à chaque visite.
#
# Modèle de menace assumé : il s'agit d'empêcher qu'un visiteur tombe par
# hasard sur le tableau de bord, pas de protéger des secrets bancaires. Le
# jeton est une pièce au porteur : qui l'a est connecté — comme le cookie de
# session de n'importe quel site.
#
# Le jeton ne contient PAS le mot de passe. Il porte une date d'expiration et
# une signature HMAC-SHA256 calculée en prenant le mot de passe pour clé.
# Trois propriétés utiles en découlent :
#   - le mot de passe ne circule jamais et ne peut pas être relu du cookie ;
#   - changer le mot de passe invalide d'un coup tous les jetons émis ;
#   - aucune clé secrète n'est écrite dans le dépôt, qui est public — la seule
#     chose secrète reste le mot de passe, qui vit dans la feuille Google.

DUREE_JETON_JOURS <- 30
NOM_COOKIE_AUTH   <- "mazette_auth"

#### Générique ####

# Bandeau d'avertissement, à utiliser dans un renderUI.
# Ne renvoie quelque chose que si `afficher` est vrai ; sinon NULL, donc rien
# ne s'affiche et l'espace n'est pas réservé.
bandeau_alerte <- function(afficher, texte,
                           titre   = "À lire attentivement",
                           couleur = COUL_ROUGE,
                           icone   = "triangle-exclamation") {
  if (!isTRUE(afficher)) return(NULL)
  
  div(
    class = "d-flex align-items-start gap-2", role = "alert",
    style = paste0("background:", couleur, "1a;",
                   "border-left:4px solid ", couleur, ";",
                   "border-radius:0.5rem;padding:0.7rem 0.9rem;",
                   "margin-bottom:0.9rem;"),
    span(style = paste0("color:", couleur, ";font-size:1.15rem;line-height:1.2;"),
         icon(icone)),
    div(
      div(style = paste0("font-weight:700;color:", couleur, ";"), titre),
      div(class = "small", texte)
    )
  )
}

datatable_simple <- function(table){
  datatable(
    table,
    options = list(
      dom = 't', # 't' pour "table" - affiche uniquement le tableau sans contrôles
      paging = FALSE, # Désactive la pagination
      ordering = FALSE, # Désactive le tri
      searching = FALSE # Désactive la recherche
    ),
    rownames= FALSE
  )
}

theme_mazette <- function(){
  theme(
    axis.title.x.top = element_text(margin = margin(b=10)),
    axis.text = element_text(face = "bold",size = 12),
    axis.title = element_text(face = "bold",size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
}

prepa_db <- function(DB,var_tva){
  creer_db_date() %>%
    left_join(DB) %>%
    mutate_if(is.numeric,replace_na,0) %>%
    mutate_if(is.character,replace_na,"") %>%
    rename(ventes = var_tva)
}


##### Format #####

format_CA <- function(montant,nb_apres=0) {
  montant_formatte <- format(round(montant,nb_apres), big.mark = ".",
                               decimal.mark = ",", nsmall = max(nb_apres,0))
  montant_formatte <- paste0(montant_formatte, "€")

  montant_formatte[str_trim(montant_formatte) == "0€"] <- ""
  montant_formatte[montant_formatte == "€"] <- ""
  montant_formatte
}

##### Couleurs #####

pal_col <- tibble(name = character(), col= character(),icon = character()) %>%
  add_row(name = "Boulangerie - Global", col = "#A14466",icon = "bread-slice") %>%
  add_row(name = "Boulangerie - Achats", col = "#D2B48C") %>%
  add_row(name = "Boulangerie - Travail", col = "#8B4513") %>%
  add_row(name = "Cuisine - Global", col = "#589441",icon = "fire-burner") %>%
  add_row(name = "Cuisine - Achats", col = "#9CAF88") %>%
  add_row(name = "Cuisine - Travail", col = "#556B2F") %>%
  add_row(name = "Service - Global", col = "#40E0F0",icon = "mug-saucer") %>%
  add_row(name = "Service - Achats", col = "#ADD8E6") %>%
  add_row(name = "Service - Travail", col = "#40E0D0") %>%
  add_row(name = "Brasserie - Global", col = "#E9BF00",icon = "wheat-awn") %>%
  add_row(name = "Brasserie - Achats", col = "#FFD700") %>%
  add_row(name = "Brasserie - Travail", col = "#FFBF00") %>%
  add_row(name = "Support - Global", col = "#8080E0",icon = "right-left") %>%
  add_row(name = "Support - Achats", col = "#D3D3D3",icon = "receipt") %>%
  add_row(name = "Support - Travail", col = "#696969") %>%
  add_row(name = "Nourriture - Global", col = "#27ae60",icon = "utensils") %>%
  add_row(name = "Nourriture - Achat", col = "#B7B18A",icon = "cart-shopping") %>%
  add_row(name = "Nourriture - Travail", col = "#705821",icon = "person-running") %>%
  add_row(name = "Boisson - Global", col = "#d4ac0d",icon = "beer-mug-empty") %>%
  add_row(name = "Boisson - Achat", col = "#9FCF68",icon = "cart-shopping") %>%
  add_row(name = "Boisson - Travail", col = "#D6D773",icon = "person-running") %>%
  add_row(name = "Global - Achat", col = "#C9C78F",icon = "cart-shopping") %>%
  add_row(name = "Global - Travail", col = "#AF4553",icon = "person-running") %>%
  add_row(name = "Midi - Global", col = "#e67e22") %>%
  add_row(name = "Soir - Global", col = "#9b59b6") %>%
  add_row(name = "Semaine - Global", col = "#2980b9") %>%
  add_row(name = "Week-end - Global", col = "#c0392b") %>%
  add_row(name = "Prime Cost / CA", col = "red",icon = "scale-balanced") %>%
  add_row(name = "Prime Cost", col = "#BAB86C",icon = "credit-card") %>%
  add_row(name = "CA HTVA", col = "green",icon = "euro-sign") %>%
  mutate(SECTEUR = str_extract(name,"^(.*?) - (.*?)$",group = 1),
         STEP = str_extract(name,"^(.*?) - (.*?)$",group = 2),
         CD_SECTEUR = case_when(
           SECTEUR == "Support" ~ "Support",
           SECTEUR %in% c("Boulangerie","Cuisine") ~ "Nourriture",
           SECTEUR %in% c("Brasserie","Service") ~ "Boisson",
           TRUE ~ SECTEUR))


#### Tableaux génériques ####

# Tableau de ventes CA avec objectif

# Tableau de récapitulatif de coût

# Tableau d'objectif de ventes

# préparation de Comparaison entre trimestre

# Tableau de prédiction de bières
table_evo_brassins <- function(db_bieres,
                               max_date=today(),
                               length_predict = 200,
                               FL_ONLY_FINI = TRUE){

  if (is.null(max_date)) max_date <- today()

  table_evo_brassin_unique <- function(id_brassin){
    table_evo_brassin(db_bieres,id_brassin,length_predict,max_date)
  }

  if (FL_ONLY_FINI){
    vec_brassins_en_cours <- db_bieres %>%
      filter(!BIERE_FINIE & DATE >= max_date-20) %>% pull(ID_BRASSIN) %>% unique
  }else{
    vec_brassins_en_cours <- db_bieres %>%
      filter(DATE >= max_date-20) %>% pull(ID_BRASSIN) %>% unique
  }

  vec_brassins_en_cours %>%
    map_df(table_evo_brassin_unique) %>%
    mutate(DT_PREDICT = max_date)
}

#### Graphiques génériques ####

# Comptabilité

# Affluence

# Évolution des brassins

report_brassin <- function(DB_BRASSINS,DB_BIERES,DB_PRODUITS,id_brassin){

  DB_BRASSINS <- DB_BRASSINS %>%
    select(-any_of("DT_DEB")) %>%
    left_join(DB_BIERES %>% group_by(ID_BRASSIN) %>%
                summarise(DT_DEB = min(DATE),.groups = "drop"))

  info_brassin <- DB_BRASSINS %>% filter(ID_BRASSIN == id_brassin)
  ventes   <- DB_BIERES %>% filter(ID_BRASSIN == id_brassin)

  name_logo <- paste0(info_brassin$`NAME LOGO`,".png")
  path_logo <- NA
  # if (length(name_logo) != 0 & !is.na(name_logo)){
    try({
      path_logo <- paste0("logos/",name_logo)
      id_png <- df_logos %>% filter(name == name_logo) %>% pull(id)
      path_png <- paste0("https://drive.google.com/uc?id=",id_png,"&export=download")
      download.file(path_png,destfile = path_logo,mode = "wb")
      img_magick <- image_read(path_logo) %>% image_scale("200")
      img_grob <- rasterGrob(as.raster(img_magick),interpolate = TRUE)
    },silent = TRUE)
  # }

  # Repérer les autres bières en ventes à ce moment
  context_beers <- DB_BRASSINS %>%
    filter(!BOISSON %in% c("Schieven Architek","Rawette","Suur de BXL"),
           DT_DEB <= info_brassin$DT_FIN,
           DT_FIN >= info_brassin$DT_DEB)

  debut_graph <- info_brassin$DT_BRASSIN-31
  fin_graph <- info_brassin$DT_FIN+31

  context_beers <- context_beers %>%
    mutate(DT_DEB = pmax(DT_DEB,debut_graph),
           DT_FIN = pmin(DT_FIN,fin_graph))

  repartition <- DB_PRODUITS %>%
    filter(ID_BRASSIN == id_brassin) %>%
    group_by(VOLUME_CL) %>%
    summarise(VOLUME = sum(QUANTITE*VOLUME_CL)) %>%
    mutate(
      TYPE = paste0(VOLUME_CL,"cl"),
      VOLUME = VOLUME / sum(VOLUME)
    )

  # Palette de couleurs "Brasserie"
  col_beer_main <- "#f39c12" # Ambrée
  col_beer_dark <- "#d35400" # Sombre
  col_text <- "#2c3e50"
  col_bg <- "#ecf0f1"

  # --- PLOT 1 : TIMELINE DE VIE DU FÛT ---
  p1_timeline <- ggplot() +
    # Les autres bières (en gris)
    geom_segment(data = context_beers,
                 aes(x = DT_DEB, xend = DT_FIN, y = BOISSON, yend = BOISSON),
                 color = "grey80", size = 4) +

    # NOTRE bière (La star)
    # Phase 1: Brassage -> Vente (Production)
    geom_segment(data = info_brassin,
                 aes(x = DT_BRASSIN, xend = DT_DEB, y = NOM_BRASSIN, yend = NOM_BRASSIN),
                 color = col_beer_main, size = 2, linetype = "dotted") +
    # Phase 2: Vente -> Fin (Vie Publique)
    geom_segment(data = info_brassin,
                 aes(x = DT_DEB, xend = DT_FIN, y = NOM_BRASSIN, yend = NOM_BRASSIN),
                 color = col_beer_main, size = 6) +

    # Points clés
    geom_point(data = info_brassin, aes(x = DT_BRASSIN, y = NOM_BRASSIN), color = col_beer_dark, size = 3) +
    geom_text(data = info_brassin, aes(x = DT_BRASSIN, y = NOM_BRASSIN, label = "Brassage"), vjust = 2, size = 3) +

    geom_point(data = info_brassin, aes(x = DT_DEB, y = NOM_BRASSIN), color = col_beer_dark, size = 3) +
    geom_text(data = info_brassin, aes(x = DT_DEB, y = NOM_BRASSIN, label = ""), vjust = 2, size = 3, fontface="bold") +

    geom_text(data = context_beers,
              aes(x = DT_DEB, label = BOISSON,
                  y = reorder(BOISSON, DT_DEB,decreasing=T)),
              hjust = -0.1, vjust = 0, size = 3, color = "grey60") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    labs(title = "1. Chronologie & Contexte", x = "", y = "") +
    theme_minimal(base_size = 16) +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face="bold"))

  # --- PLOT 2 : COURBE DES VENTES (Volume journalier) ---
  p2_sales <- ggplot(ventes, aes(x = DATE, y = VOLUME_JOUR)) +
    # Barres discrètes en fond
    geom_col(fill = col_beer_main, alpha = 0.3, width = 0.8) +
    # Courbe lissée pour la tendance
    geom_smooth(method = "loess", se = FALSE, color = col_beer_dark, size = 1.2, span = 0.2) +
    # Highlight des pics
    geom_point(data = ventes %>% filter(VOLUME_JOUR > 20), color = "red", size = 2) +
    scale_y_continuous(labels = label_number(suffix = " L")) +
    scale_x_date(date_breaks = "1 week", date_labels = "Sem %V\n%d %b %y",minor_breaks = NULL) +
    labs(title = "2. Rythme d'écoulement (Litres/Jour)",x = "", y = "") +
    theme_minimal(base_size = 16) +
    theme(plot.subtitle = element_text(size = 9, color = "grey50"))

  # --- PLOT 3 : KPI & LOGO ---
  # Calcul des stats
  total_vol <- sum(ventes$VOLUME_JOUR)
  total_ca <- sum(ventes$CA_TVAC)
  duree <- as.numeric(max(ventes$DATE) - min(ventes$DATE))
  prix_moyen_L <- total_ca / total_vol

  p3_logo <- ggplot() + theme_void() + labs(title = "3. Fiche d'identité")

  try({
    p3_logo <- p3_logo +
      annotation_custom(img_grob, xmin=-0.15, xmax=Inf, ymin=-Inf, ymax=Inf)
  },silent = T)

  p3_kpi <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "white", color = "white") +
    # Textes Stats
    annotate("text", x = 0.1, y = 0.8, label = info_brassin$NOM_BRASSIN, hjust = 0, size = 6, fontface = "bold") +
    annotate("text", x = 0.1, y = 0.6, label = paste0("Total Vendu: ", round(total_vol, 0), " L / ", info_brassin$VOLUME_BRASSIN, " L"), hjust = 0, size = 5) +
    annotate("text", x = 0.1, y = 0.45, label = paste0("Chiffre d'Affaires HTVA: ", round(total_ca, 0), " €"), hjust = 0, size = 5, color = "darkgreen", fontface="bold") +
    annotate("text", x = 0.1, y = 0.3, label = paste0("Durée de vie: ", duree, " Jours"), hjust = 0, size = 5) +
    annotate("text", x = 0.1, y = 0.15, label = paste0("Rendement: ", round(prix_moyen_L, 1), " €/L"), hjust = 0, size = 5, fontface = "italic", color = "grey50")+
    theme_void(base_size = 16)

  # --- PLOT 4 : DONUT DES FORMATS ---
  p4_donut <- ggplot(repartition, aes(x = 2, y = VOLUME, fill = TYPE)) +
    geom_col(color = "white") +
    scale_fill_brewer(palette = "YlOrBr") +
    geom_text(aes(label = scales::percent(VOLUME,accuracy=1)),
              position = position_stack(vjust = 0.5), size = 5, fontface="bold") +
    labs(title = "4. Formats", fill = "") +
    theme_void(base_size = 16) +
    theme(legend.position = "right", plot.title = element_text(hjust = 0.5))


  layout <- "
AAAAA
BBBBB
CDDDE
"

  final_plot <- p1_timeline + p2_sales + p3_logo + p3_kpi + p4_donut +
    plot_layout(design = layout) + # Hauteur relative des panneaux
    plot_annotation(
      title = paste0("ANALYSE DE BRASSIN : ", info_brassin$NOM_BRASSIN,
                    " ( brassin n°",id_brassin,")"),
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", color = "#2c3e50"),
        plot.subtitle = element_text(size = 14, color = "#7f8c8d"),
        plot.background = element_rect(fill = "#fdfdfd", color = NA)
      )
    )

  final_plot
}

#### Box Ventes ####

box_ventes_jour <- function(db_kpi,db_obj,date_debut,nb_jours,
                            format_date = "%d",titre = "",
                            is_semaine=FALSE,is_midi=TRUE,is_boisson=TRUE,
                            is_objectif=TRUE, width = "14%"){
  plot_kpi <- db_kpi %>%
    left_join(db_obj%>%
                select(-starts_with("CA_")) %>%
                rename(ventes_obj = ventes)) %>%
    filter(DATE >= date_debut,DATE <= date_debut+days(nb_jours)) %>%
    # mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels=vecteur_jours_LOCAL,
    #                              labels = vecteur_jours)) %>%
    mutate(title = paste0(JOUR_SEMAINE," ",format(DATE,format = format_date)))

  if (titre != "") plot_kpi$title <- titre

  plot_kpi <- plot_kpi %>%
    table_kpi(fl_semaine = is_semaine,fl_midi = is_midi,
              fl_boisson = is_boisson,fl_objectif = is_objectif,
              width = width)

  return(
    div(class = "ventes-grid", do.call(tagList, plot_kpi))
  )
}

box_ventes_total <- function(db_kpi,db_obj,date_debut,nb_jours,
                            format_date = "%d",titre = "",
                            is_semaine=FALSE,is_midi=TRUE,is_boisson=TRUE,
                            is_objectif=TRUE){
  plot_kpi <- db_kpi %>%
    left_join(db_obj%>%
                select(-starts_with("CA_")) %>%
                rename(ventes_obj = ventes)) %>%
    mutate(ventes_obj = ventes_obj * (ventes>0)) %>%
    filter(DATE >= date_debut,DATE <= date_debut+days(nb_jours)) %>%
    summarise(ventes = sum(ventes,na.rm=TRUE),
              ventes_obj = sum(ventes_obj,na.rm=TRUE),
              Jour = sum(Jour),Soir = sum(Soir),
              Boisson = sum(Boisson),Nourriture = sum(Nourriture),
              Semaine = sum(Semaine),`Week-end` = sum(`Week-end`)) %>%
    mutate(title = titre) %>%
    table_kpi(fl_semaine = is_semaine,fl_midi = is_midi,
              fl_boisson = is_boisson,fl_objectif = is_objectif, width = "100%")

  return(
    div(class = "ventes-grid", do.call(tagList, plot_kpi))
  )
}


table_kpi <- function(db,fl_midi=TRUE,fl_boisson=TRUE,
                      fl_semaine=TRUE,fl_objectif=TRUE,width = "14%"){

  list_kpi <- list()
  for (i in 1:nrow(db)){
    ligne <- db[i,]
    title <- ligne$title
    ca <- ligne$ventes
    objectif <- ligne$ventes_obj
    # Même convention que les barres de CA du reste du tableau de bord :
    # vert atteint, ambre à partir de 90 %, rouge en dessous.
    couleur <- couleur_objectif(ca, ligne$ventes_obj)
    percent_midi <- round(100 * ligne$Jour / (ligne$Jour+ligne$Soir))
    percent_soir <- 100 - percent_midi
    percent_boisson <- round(100 * ligne$Boisson / (ligne$Boisson+ligne$Nourriture))
    percent_nourriture <- 100 - percent_boisson
    percent_semaine <- round(100 * ligne$Semaine / (ligne$Semaine+ligne$`Week-end`))
    percent_weekend <- 100 - percent_semaine

    if (!fl_midi) {
      percent_midi <- 0
      percent_soir <- 0
    }
    if (!fl_boisson) {
      percent_boisson <- 0
      percent_nourriture <- 0
    }
    if (!fl_semaine) {
      percent_semaine <- 0
      percent_weekend <- 0
    }
    if (!fl_objectif){
      objectif <- NULL
    }

    list_kpi[[i]] <- tagList(caInfoBox(title,ca,percent_midi,
                               percent_soir,percent_boisson,
                               percent_nourriture,percent_semaine,
                               percent_weekend,width,couleur,objectif))
  }
  return(list_kpi)
}


# Fonction pour générer une infoBox avec une info-bulle

# Barre de répartition en deux segments. La mise en forme vit dans style.css
# (.ventes-barre), pour que la barre s'adapte à la largeur de sa carte.
generate_bar <- function(percent1, percent2, color1, color2, title) {
  if (is.na(percent1) || is.na(percent2) || percent1 + percent2 <= 0) return(NULL)

  # On n'inscrit le pourcentage que si le segment est assez large : en dessous,
  # le texte était rogné et illisible (« 7% » dans un filet de 10 px).
  etiquette <- function(p) if (p >= 18) paste0(p, "%") else ""

  div(
    div(
      class = "ventes-barre",
      span(style = paste0("flex:", percent1, ";background:", color1, ";"),
           etiquette(percent1)),
      span(style = paste0("flex:", percent2, ";background:", color2, ";"),
           etiquette(percent2))
    ),
    div(class = "ventes-legende", title)
  )
}

# Carte de ventes d'une journée (ou d'un total) : CA, atteinte de l'objectif
# et barres de répartition.
# `width` n'est plus utilisé : la largeur est pilotée par la grille responsive
# qui contient les cartes (.ventes-grid). L'argument reste pour ne pas casser
# les appels existants.
caInfoBox <- function(title, ca, percent_midi, percent_soir, percent_boisson,
                      percent_nourriture, percent_semaine, percent_weekend,
                      width = NULL, ca_color = NULL, objectif = NULL) {

  couleur <- if (is.null(ca_color)) couleur_objectif(ca, objectif) else ca_color

  # Les barres n'ont pas de sens sur une journée sans activité
  barres <- if (!is.na(ca) && ca > 5) {
    div(
      class = "ventes-barres",
      generate_bar(percent_midi, percent_soir,
                   "#e67e22", "#9b59b6", "Midi / Soir"),
      generate_bar(percent_boisson, percent_nourriture,
                   "#d4ac0d", "#27ae60", "Boisson / Nourriture"),
      generate_bar(percent_semaine, percent_weekend,
                   "#2980b9", "#c0392b", "Semaine / Week-end")
    )
  } else NULL

  objectif_ligne <- if (!is.null(objectif) && !is.na(objectif) && objectif > 0) {
    div(class = "ventes-obj",
        "objectif ", format_CA(objectif, -1), " · ",
        tags$b(paste0(round(100 * ca / objectif), " %")))
  } else NULL

  div(
    class = "ventes-card",
    div(class = "ventes-jour", title),
    div(class = "ventes-ca", style = paste0("color:", couleur, ";"),
        format_CA(ca, -1)),
    objectif_ligne,
    barres
  )
}

# Récapitulatif compact des dernières semaines : une ligne par semaine, une
# colonne par jour, le CA coloré selon l'atteinte de son objectif. Remplace
# une pile de cartes qui occupait beaucoup de place pour la seule valeur du
# jour. Le détail (date complète, objectif, pourcentage) est en infobulle.
tableau_semaines <- function(db_kpi, db_obj, fin_semaine, n_semaines = 5) {
  fin <- floor_date(as.Date(fin_semaine), "week", week_start = 1)
  debut <- fin - weeks(n_semaines - 1)

  dat <- db_kpi %>%
    select(DATE, ventes) %>%
    left_join(db_obj %>% select(DATE, objectif = ventes), by = "DATE") %>%
    filter(DATE >= debut, DATE <= fin + 6) %>%
    mutate(objectif = replace_na(objectif, 0),
           ventes   = replace_na(ventes, 0),
           SEMAINE  = floor_date(DATE, "week", week_start = 1),
           JOUR     = as.integer(wday(DATE, week_start = 1)))

  if (nrow(dat) == 0)
    return(div(class = "text-muted small", "Aucune donnée sur la période."))

  jours_court <- c("lun", "mar", "mer", "jeu", "ven", "sam", "dim")

  cellule <- function(ca, obj, date, total = FALSE) {
    classe <- if (total) "rs-total" else NULL
    if (is.na(ca) || ca <= 0)
      return(tags$td(class = paste(c(classe, "rs-vide"), collapse = " "), "—"))
    coul <- couleur_objectif(ca, obj)
    tags$td(
      class = classe,
      # 1f = ~12 % d'opacité : un fond teinté qui reste lisible
      style = paste0("color:", coul, ";background:", coul, "1f;"),
      title = paste0(date, " — ", label_objectif(ca, obj)),
      format_CA(ca, -1)
    )
  }

  ligne <- function(sem) {
    jours <- dat %>% filter(SEMAINE == sem)
    cells <- lapply(1:7, function(j) {
      d <- jours %>% filter(JOUR == j)
      if (nrow(d) == 0) tags$td(class = "rs-vide", "—")
      else cellule(d$ventes[1], d$objectif[1],
                   format(d$DATE[1], "%A %d/%m/%Y"))
    })
    tags$tr(
      tags$td(class = "rs-sem", paste0("Sem. ", format(sem, "%d/%m"))),
      cells,
      cellule(sum(jours$ventes), sum(jours$objectif),
              paste0("semaine du ", format(sem, "%d/%m/%Y")), total = TRUE)
    )
  }

  semaines <- sort(unique(dat$SEMAINE), decreasing = TRUE)

  tags$table(
    class = "rs-table",
    tags$thead(tags$tr(
      tags$th(class = "rs-sem", "Semaine"),
      lapply(jours_court, function(j) tags$th(j)),
      tags$th("Total")
    )),
    tags$tbody(lapply(semaines, ligne))
  )
}




#### Econométrie ####

predict_fin_brassin <- function(DB_PREDICT,id_brassin){
  if (nrow(DB_PREDICT) == 0) return(c(NA,NA,NA))
  table <- DB_PREDICT %>% filter(ID_BRASSIN == id_brassin)

  zero_LO_50 <- which(table$LO_50 <= 0)[1]
  zero_HI_50 <- which(table$HI_50 <= 0)[1]
  zero_mean <- which(table$VOLUME_RESTANT <= 0)[1]

  if (!is.na(zero_mean)){
    c(table[zero_LO_50,]$DATE,
      table[zero_mean,]$DATE,
      table[zero_HI_50,]$DATE)
  }else{
    c(NA,NA,NA)
  }
}

# Ajout des prédictions
table_evo_brassin <- function(db_bieres,id_brassin,length_predict = 200,
                              max_date=today()){

  serie <- db_bieres %>%
    filter(DATE <= max_date) %>%
    filter(ID_BRASSIN == id_brassin)

  if (nrow(serie) == 0) return(NULL)

  serie <- serie %>%
    complete(DATE = seq.Date(min(DATE), max(DATE), by = "1 day")) %>%
    arrange(DATE) %>%
    mutate(across(everything(), ~na.locf(.)),
           VOLUME_RESTANT = VOLUME_BRASSIN_AJUST-VOLUME_TOT)

  boisson <- serie %>% pull(BOISSON) %>% unique()

  actual <- serie %>%
    select(DATE,ID_BRASSIN,BOISSON,VOLUME_RESTANT) %>%
    mutate(FL_PREDICT = FALSE,
           LO_50 = VOLUME_RESTANT,HI_50 = VOLUME_RESTANT,
           LO_75 = VOLUME_RESTANT,HI_75 = VOLUME_RESTANT,
           LO_95 = VOLUME_RESTANT,HI_95 = VOLUME_RESTANT)

  predict <- NULL

  complet <- actual
  try({

    if (nrow(serie) >= 7){

      if (nrow(serie) >= 14)
        fcmodel <- HoltWinters(ts(serie$VOLUME_RESTANT, frequency=7))
      else
        fcmodel <- HoltWinters(ts(serie$VOLUME_RESTANT, frequency=3))
      predict <- forecast(fcmodel, h=length_predict, level=c(50,75,95))
      predict <- tibble(as.data.frame(predict))
      colnames(predict) <- c("VOLUME_RESTANT","LO_50","HI_50",
                             "LO_75","HI_75","LO_95","HI_95")

      predict <- predict %>%
        mutate(DATE = seq(max(serie$DATE)+1, max(serie$DATE)+
                            days(length_predict),by=1),
               ID_BRASSIN = id_brassin,
               BOISSON = boisson,
               FL_PREDICT = TRUE,
               LO_50 = pmax(0,LO_50),HI_50 = pmax(0,HI_50),
               LO_75 = pmax(0,LO_75),HI_75 = pmax(0,HI_75),
               LO_95 = pmax(0,LO_95),HI_95 = pmax(0,HI_95))
    }

    complet <- rbind(actual,predict)
    complet <- complet %>% filter(HI_75 > 0)

  },silent = TRUE)

  complet
}

