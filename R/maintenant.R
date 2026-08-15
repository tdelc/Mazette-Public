#### REFONTE — Volet "Maintenant" ####

# Top produits (CA HTVA) sur une période [date_debut, date_fin]
top_produits_periode <- function(db_produits, date_debut, date_fin, n = 10,
                                 unite_tva = "HTVA") {
  
  col <- paste0("CA_",unite_tva)
  col_name <- paste("CA",unite_tva)
  
  db_produits %>%
    filter(DATE >= date_debut, DATE <= date_fin) %>%
    group_by(PRODUIT) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(!!sym(col), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(CA)) %>%
    slice_head(n = n) %>%
    transmute(Produit = tronque_nom(PRODUIT),
              Quantité = Quantite,
              !!sym(col_name) := format_CA(CA, -1))
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
           cum_reel = ifelse(DATE >= today(), NA, cum_reel))
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

# Récapitulatif compact des dernières semaines : une ligne par semaine, une
# colonne par jour, le CA coloré selon l'atteinte de son objectif. Remplace
# une pile de cartes qui occupait beaucoup de place pour la seule valeur du
# jour. Le détail (date complète, objectif, pourcentage) est en infobulle.
# L'unité se pose sur l'en-tête « Total » : le tableau compte une quarantaine
# de montants, une pastille par cellule serait illisible.
tableau_semaines <- function(db_kpi, db_obj, fin_semaine, n_semaines = 5,
                             unite_tva = "HTVA") {
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
      tags$th(titre_avec_tva("Total", unite_tva))
    )),
    tags$tbody(lapply(semaines, ligne))
  )
}

