##### Boisson — consommation #####

ref_boissons <- function(db_produits) {
  db_produits %>%
    filter(est_biere(CATEGORIE), !is.na(BOISSON), BOISSON != "") %>%
    distinct(BOISSON) %>%
    pull(BOISSON)
}

# Lignes de ticket correspondant à des boissons, sur une fenêtre de dates.
tickets_boissons <- function(db_ticket, ref, d1, d2) {
  db_ticket %>%
    filter(BOISSON %in% ref, DATE >= as.Date(d1), DATE <= as.Date(d2),
           QUANTITE > 0) %>%
    mutate(LITRES = replace_na(VOLUME_TOT_L, 0),
           HEURE  = heure_service(TIMESTAMP))
}

# Consommation par boisson sur une fenêtre : verres, litres, CA.
conso_boissons <- function(db_ticket, ref, d1, d2, unite_tva) {
  
  col <- paste0("CA_",unite_tva)
  col_name <- paste("CA",unite_tva)
  
  tickets_boissons(db_ticket, ref, d1, d2) %>%
    group_by(BOISSON) %>%
    summarise(VERRES = sum(QUANTITE, na.rm = TRUE),
              LITRES = sum(LITRES, na.rm = TRUE),
              CA     = sum(!!sym(col), na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(LITRES))
}

# Consommation d'une semaine, comparée à la semaine précédente.
conso_boissons_comparee <- function(db_ticket, ref, semaine, unite_tva = "HTVA") {
  semaine <- as.Date(semaine)
  act <- conso_boissons(db_ticket, ref, semaine, semaine + 6, unite_tva)
  prec <- conso_boissons(db_ticket, ref, semaine - 7, semaine - 1, unite_tva) %>%
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
conso_boissons_horaire <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  par_heure <- function(d1, d2, nom) {
    tickets_boissons(db_ticket, ref, d1, d2) %>%
      group_by(HEURE) %>%
      summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
      mutate(PERIODE = nom)
  }
  bind_rows(par_heure(semaine, semaine + 6, "Semaine"),
            par_heure(semaine - 7, semaine - 1, "S-1")) %>%
    filter(!is.na(HEURE), LITRES > 0)
}

# Litres par jour de semaine et par heure (heatmap).
conso_boissons_jour_heure <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  tickets_boissons(db_ticket, ref, semaine, semaine + 6) %>%
    mutate(JOUR = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1)) %>%
    group_by(JOUR, HEURE) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(HEURE))
}

# Historique hebdomadaire des litres servis.
evo_conso_boissons <- function(db_ticket, ref, n_semaines = 26, fin = NULL) {
  fin <- if (is.null(fin)) max(db_ticket$DATE, na.rm = TRUE) else as.Date(fin)
  debut <- floor_date(fin, "week", week_start = 1) - weeks(n_semaines - 1)
  tickets_boissons(db_ticket, ref, debut, fin) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    group_by(SEMAINE) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE),
              VERRES = sum(QUANTITE, na.rm = TRUE),
              CA     = sum(PRIX_TOTAL, na.rm = TRUE),
              NB_BIERES = n_distinct(BOISSON), .groups = "drop") %>%
    arrange(SEMAINE)
}

# Trajectoire hebdomadaire des principales boissons de la semaine choisie :
# permet de voir lesquelles montent, lesquelles s'essoufflent.
evo_top_boissons <- function(db_ticket, ref, semaine, n_top = 5, n_semaines = 12,
                           unite_tva = "HTVA") {
  semaine <- as.Date(semaine)
  top <- conso_boissons(db_ticket, ref, semaine, semaine + 6, unite_tva) %>%
    slice_head(n = n_top) %>%
    pull(BOISSON)
  if (length(top) == 0) return(tibble(SEMAINE = as.Date(character()),
                                      BOISSON = character(), LITRES = numeric()))
  
  debut <- semaine - weeks(n_semaines - 1)
  tickets_boissons(db_ticket, ref, debut, semaine + 6) %>%
    filter(BOISSON %in% top) %>%
    mutate(SEMAINE = floor_date(DATE, "week", week_start = 1)) %>%
    group_by(SEMAINE, BOISSON) %>%
    summarise(LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    complete(SEMAINE, BOISSON, fill = list(LITRES = 0)) %>%
    mutate(BOISSON = factor(BOISSON, levels = top)) %>%
    arrange(BOISSON, SEMAINE)
}

# Répartition des formats servis (33 cl, 50 cl, dégustation...).
formats_boissons <- function(db_ticket, ref, semaine) {
  semaine <- as.Date(semaine)
  tickets_boissons(db_ticket, ref, semaine, semaine + 6) %>%
    filter(!is.na(VOLUME_CL)) %>%
    group_by(FORMAT = paste0(VOLUME_CL, " cl")) %>%
    summarise(VERRES = sum(QUANTITE, na.rm = TRUE),
              LITRES = sum(LITRES, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(VERRES))
}

kpi_boissons_tiles <- function(comp, formats, horaire = NULL, unite_tva = NULL,
                              categorie = "") {
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
    tuile_evolution(ca, ca_m1, titre_avec_tva(paste("CA",categorie), unite_tva), "euro-sign",
                    function(x) format_CA(x, -1)),
    tuile_ecart(nb, nb_m1, "Boissons différentes", "list-ul"),
    kpi_tile(paste0(round(tanker, 2)), "Équivalent tanker (500 L)", CONSO_BRUN,
             "boxes-stacked", sous_titre = paste0(round(litres / 7), " L / jour")),
    kpi_tile(if (is.null(pic)) "—" else as.character(pic$HEURE),
             "Pic de consommation", "#8d7b68", "clock",
             sous_titre = if (is.null(pic)) NULL
             else paste0(round(pic$LITRES), " L sur la semaine"))
  )
}

# Top boissons par litres, colorées selon l'évolution vs S-1.
graph_top_boissons <- function(comp, n = 12) {
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

# Trajectoire des principales boissons : une ligne par bière, litres par semaine.
# La semaine analysée est marquée d'un point, pour situer le contexte.
graph_tendance_boissons <- function(evo, semaine = NULL) {
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
graph_heatmap_boissons <- function(jh) {
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

# Historique hebdomadaire : litres en barres, nombre de boissons en ligne.
graph_evo_conso_boissons <- function(evo, semaine = NULL) {
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
    add_lines(x = ~SEMAINE, y = ~NB_BIERES, name = "Boissons à la carte",
              yaxis = "y2", line = list(color = "#5B7B5A", width = 2),
              hovertemplate = ~paste0(NB_BIERES, " boissons<extra></extra>")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = "Litres"),
           yaxis2 = list(title = "Nb de boissons", overlaying = "y", side = "right",
                         showgrid = FALSE, rangemode = "tozero"),
           legend = list(orientation = "h"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Formats servis (33 cl, 50 cl, dégustation...).
graph_formats_boissons <- function(formats) {
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

table_conso_boissons <- function(comp, unite_tva = NULL) {
  if (is.null(comp) || nrow(comp) == 0) return(tibble(Boisson = character()))
  
  col_name <- paste("CA",unite_tva)
  
  # Totaux calculés hors du transmute : une condition scalaire dans un
  # `ifelse` renverrait une valeur unique, recyclée sur toutes les lignes.
  total    <- sum(comp$LITRES, na.rm = TRUE)
  total_m1 <- sum(comp$LITRES_M1, na.rm = TRUE)
  
  comp %>%
    mutate(PART    = if (total > 0) 100 * LITRES / total else NA_real_,
           PART_M1 = if (total_m1 > 0) 100 * LITRES_M1 / total_m1 else NA_real_) %>%
    transmute(Bière            = BOISSON,
              Verres           = VERRES,
              Litres           = round(LITRES),
              Part             = ifelse(is.na(PART), "—", paste0(round(PART, 1), " %")),
              !!sym(col_name) := format_CA(CA, -1),
              `Litres S-1`     = round(LITRES_M1),
              `Part S-1`       = ifelse(is.na(PART_M1), "—",
                                        paste0(round(PART_M1, 1), " %")),
              `Évol.`          = ifelse(is.na(EVO_PCT), "—",
                                        paste0(ifelse(EVO_PCT >= 0, "+", ""),
                                         EVO_PCT, " %")),
              Statut           = STATUT)
}

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