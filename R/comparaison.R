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
table_comparaison_aff <- function(comp, unite = c("semaine", "mois", "annee"),
                                  unite_tva = NULL) {
  col_name <- paste("CA",unite_tva)
  unite <- match.arg(unite)
  if (is.null(comp) || nrow(comp) == 0)
    return(tibble(Période = character()))
  comp %>%
    arrange(desc(PERIODE)) %>%
    transmute(Période          = label_periode(PERIODE, unite),
              !!sym(col_name) := format_CA(CA, -1),
              Objectif         = format_CA(OBJECTIF, -1),
              `% obj.`         = ifelse(is.na(PCT_OBJ), "—", paste0(PCT_OBJ, " %")),
              `Food cost`      = ifelse(is.na(FOOD_PCT), "—", paste0(FOOD_PCT, " %")),
              `Work cost`      = ifelse(is.na(WORK_PCT), "—", paste0(WORK_PCT, " %")),
              `Prime cost`     = ifelse(is.na(PRIME_PCT), "—", paste0(PRIME_PCT, " %")),
              Marge            = format_CA(MARGE, -1),
              `Marge %`        = ifelse(is.na(MARGE_PCT), "—", paste0(MARGE_PCT, " %")))
}

