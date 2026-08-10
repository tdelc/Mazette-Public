# Volet "Réservations".
#
# DB_RESA ne porte que trois colonnes : HEURE_DEB, HEURE_FIN, NB_PERS. Tout le
# reste — jour, créneau, durée, taille de groupe — s'en déduit, et rien d'autre
# n'est inventé. Il n'y a notamment ni nom, ni statut, ni annulation : on ne peut
# donc pas parler de taux de no-show, seulement de réservations enregistrées.

# Un groupe change de nature à partir de 6 : au-delà, il occupe plusieurs tables
# et se prépare. Les seuils sont ici pour pouvoir être ajustés d'un seul endroit.
SEUILS_GROUPE <- c(2, 4, 6, 10)

TAILLES_GROUPE <- c("1-2 pers.", "3-4 pers.", "5-6 pers.",
                    "7-10 pers.", "11 et +")

# Salle et terrasse ne se pilotent pas pareil : la terrasse dépend de la météo et
# ferme l'hiver, la salle est la capacité stable. Les distinguer partout sauf
# dans le lien avec le CA, où seul le total compte.
LIEUX <- c("SALLE", "TERRASSE")
COUL_LIEU <- c("SALLE" = "#732c02", "TERRASSE" = "#5B7B5A",
               "Non précisé" = "#8d7b68")

#' Normalise DB_RESA et en dérive les dimensions d'analyse.
#'
#' Le service du soir déborde après minuit : une réservation à 00h30 appartient
#' au service de la veille. On rattache donc au jour de service, pas au jour
#' calendaire — même convention que CD_HEURE côté tickets.
prepare_resa <- function(db) {
  if (is.null(db) || !nrow(db)) return(resa_vide())

  # LOCATION et A_VENIR peuvent manquer sur un ancien export : on retombe alors
  # sur des valeurs neutres plutôt que d'échouer.
  if (!"LOCATION" %in% names(db)) db$LOCATION <- NA_character_
  if (!"A_VENIR"  %in% names(db)) db$A_VENIR  <- NA

  d <- db %>%
    mutate(DEB = as.POSIXct(HEURE_DEB), FIN = as.POSIXct(HEURE_FIN),
           NB_PERS = as.numeric(NB_PERS),
           LOCATION = toupper(trimws(as.character(LOCATION))),
           LOCATION = if_else(LOCATION %in% LIEUX, LOCATION, "Non précisé"),
           # A_VENIR absent : on retombe sur la date, faute de mieux.
           A_VENIR  = if_else(is.na(A_VENIR), DEB >= now(), as.logical(A_VENIR))) %>%
    filter(!is.na(DEB))

  if (!nrow(d)) return(resa_vide())

  d %>%
    mutate(
      HEURE   = as.integer(hour(DEB)),
      # as.Date() sur un POSIXct bascule en UTC et recule d'un jour tout ce qui
      # est entre minuit et l'écart horaire. On passe donc par la représentation
      # locale, la seule qui corresponde à l'heure notée sur la réservation.
      DATE    = as.Date(format(DEB, "%Y-%m-%d")) - if_else(HEURE < 6, 1, 0),
      # Une réservation à 0 h 30 est la queue du service du soir, pas un midi :
      # le test sur l'heure seule la rangeait du mauvais côté.
      CRENEAU = if_else(HEURE >= 6 & HEURE < 17, "Midi", "Soir"),
      JOUR    = wday(DATE, label = TRUE, abbr = FALSE, week_start = 1),
      SEMAINE = floor_date(DATE, "week", week_start = 1),
      MOIS    = floor_date(DATE, "month"),
      # Une fin manquante ou antérieure au début ne donne pas de durée : on la
      # laisse à NA plutôt que d'imposer une valeur par défaut.
      DUREE_MIN = if_else(!is.na(FIN) & FIN > DEB,
                          as.numeric(difftime(FIN, DEB, units = "mins")),
                          NA_real_),
      NB_PERS = replace_na(NB_PERS, 0),
      TAILLE  = cut(NB_PERS, breaks = c(-Inf, SEUILS_GROUPE, Inf),
                    labels = TAILLES_GROUPE, right = TRUE),
      LIEU    = factor(LOCATION, levels = c(LIEUX, "Non précisé"))
    ) %>%
    arrange(DEB)
}

resa_vide <- function() {
  tibble(DEB = as.POSIXct(character()), FIN = as.POSIXct(character()),
         NB_PERS = numeric(), HEURE = integer(), DATE = as.Date(character()),
         CRENEAU = character(), JOUR = factor(), SEMAINE = as.Date(character()),
         MOIS = as.Date(character()), DUREE_MIN = numeric(), TAILLE = factor(),
         LOCATION = character(), LIEU = factor(), A_VENIR = logical())
}

# Dimension de répartition d'un graphe empilé. Un seul point d'entrée pour que
# les trois volets proposent exactement les mêmes découpages.
dimension_resa <- function(d, par = c("lieu", "creneau", "taille")) {
  par <- match.arg(par)
  switch(par,
    lieu    = list(v = d$LIEU,    couleurs = COUL_LIEU[levels(d$LIEU)],
                   titre = "Lieu"),
    creneau = list(v = factor(d$CRENEAU, levels = c("Midi", "Soir")),
                   couleurs = c("Midi" = COUL_AMBRE, "Soir" = "#8d5b8c"),
                   titre = "Créneau"),
    taille  = list(v = d$TAILLE,
                   couleurs = setNames(
                     colorRampPalette(c(COUL_AMBRE, COUL_BRUN))(nlevels(d$TAILLE)),
                     levels(d$TAILLE)),
                   titre = "Taille de groupe"))
}

##### Prochaines réservations #####

#' Réservations à venir, de la plus proche à la plus lointaine.
prochaines_resa <- function(resa, depuis = now(), jours = 21) {
  if (!nrow(resa)) return(resa_vide())
  resa %>% filter(A_VENIR, DEB >= depuis, DEB <= depuis + days(jours)) %>% arrange(DEB)
}

#' Une ligne par jour à venir : couverts attendus, créneaux, plus gros groupe.
agenda_resa <- function(resa, depuis = now(), jours = 21) {
  p <- prochaines_resa(resa, depuis, jours)
  if (!nrow(p)) return(tibble())
  p %>%
    group_by(DATE, JOUR) %>%
    summarise(RESA = n(), COUVERTS = sum(NB_PERS, na.rm = TRUE),
              MIDI = sum(NB_PERS[CRENEAU == "Midi"], na.rm = TRUE),
              SOIR = sum(NB_PERS[CRENEAU == "Soir"], na.rm = TRUE),
              SALLE = sum(NB_PERS[LOCATION == "SALLE"], na.rm = TRUE),
              TERRASSE = sum(NB_PERS[LOCATION == "TERRASSE"], na.rm = TRUE),
              MAX_GROUPE = max(NB_PERS, na.rm = TRUE),
              PREMIERE = min(DEB), DERNIERE = max(DEB), .groups = "drop") %>%
    arrange(DATE)
}

#' Tuiles : ce qui arrive dans les jours qui viennent.
kpi_prochaines_resa <- function(resa, depuis = now()) {
  if (!nrow(resa)) return(div(class = "text-muted small",
                              "Aucune réservation enregistrée."))
  bloc <- function(j) {
    d <- prochaines_resa(resa, depuis, j)
    list(n = nrow(d), c = sum(d$NB_PERS, na.rm = TRUE))
  }
  auj <- resa %>% filter(DATE == as.Date(depuis))
  a7  <- bloc(7); a30 <- bloc(30)
  p30  <- prochaines_resa(resa, depuis, 30)
  gros <- p30 %>% filter(NB_PERS >= SEUILS_GROUPE[3])
  lieu30 <- vapply(c("SALLE", "TERRASSE"),
                   function(l) sum(p30$NB_PERS[p30$LOCATION == l], na.rm = TRUE),
                   numeric(1))

  div(class = "kpi-grid",
      kpi_tile(sum(auj$NB_PERS, na.rm = TRUE), "Couverts réservés aujourd'hui",
               COUL_BRUN, "utensils",
               sous_titre = paste(nrow(auj), "réservations")),
      kpi_tile(a7$c, "Couverts sur 7 jours", COUL_AMBRE, "calendar-week",
               sous_titre = paste(a7$n, "réservations")),
      kpi_tile(a30$c, "Couverts sur 30 jours", COUL_VERT, "calendar-days",
               sous_titre = paste(a30$n, "réservations")),
      kpi_tile(nrow(gros), paste0("Groupes de ", SEUILS_GROUPE[3], "+ à venir"),
               if (nrow(gros)) COUL_ROUGE else COUL_NEUTRE, "users",
               sous_titre = if (nrow(gros))
                 paste("le plus gros :", max(gros$NB_PERS), "pers.") else "aucun"),
      kpi_tile(lieu30["SALLE"], "Couverts en salle (30 j)",
               COUL_LIEU[["SALLE"]], "utensils",
               sous_titre = paste0(format_pct(ratio_pct(lieu30["SALLE"], a30$c)),
                                   " des couverts")),
      kpi_tile(lieu30["TERRASSE"], "Couverts en terrasse (30 j)",
               COUL_LIEU[["TERRASSE"]], "sun",
               sous_titre = paste0(format_pct(ratio_pct(lieu30["TERRASSE"], a30$c)),
                                   " des couverts"))
  )
}

#' Tableau des prochaines réservations.
table_prochaines_resa <- function(resa, depuis = now(), jours = 21) {
  p <- prochaines_resa(resa, depuis, jours)
  if (!nrow(p)) return(tibble(`À venir` = "Aucune réservation sur la période."))
  p %>%
    transmute(Jour = format(DEB, "%a %d/%m"),
              Heure = format(DEB, "%H:%M"),
              Fin = if_else(is.na(FIN), "—", format(FIN, "%H:%M")),
              `Durée` = if_else(is.na(DUREE_MIN), "—",
                                paste0(round(DUREE_MIN), " min")),
              Personnes = NB_PERS,
              `Créneau` = CRENEAU,
              Lieu = str_to_title(as.character(LIEU)))
}

##### Statistiques récentes #####

#' Indicateurs sur les réservations d'une fenêtre passée.
kpi_stats_resa <- function(resa, d1, d2) {
  d <- resa %>% filter(DATE >= as.Date(d1), DATE <= as.Date(d2))
  if (!nrow(d)) return(div(class = "text-muted small",
                           "Aucune réservation sur la période."))
  jours_avec <- n_distinct(d$DATE)
  duree <- d$DUREE_MIN[!is.na(d$DUREE_MIN)]

  div(class = "kpi-grid",
      kpi_tile(nrow(d), "Réservations", COUL_BRUN, "book-bookmark",
               sous_titre = paste(jours_avec, "jours concernés")),
      kpi_tile(sum(d$NB_PERS, na.rm = TRUE), "Couverts réservés",
               COUL_AMBRE, "utensils",
               sous_titre = paste0(round(sum(d$NB_PERS, na.rm = TRUE) / jours_avec, 1),
                                   " par jour")),
      kpi_tile(round(mean(d$NB_PERS, na.rm = TRUE), 1), "Taille moyenne",
               COUL_VERT, "users",
               sous_titre = paste0("médiane ", median(d$NB_PERS, na.rm = TRUE),
                                   " · max ", max(d$NB_PERS, na.rm = TRUE))),
      kpi_tile(paste0(round(100 * mean(d$CRENEAU == "Soir"), 0), " %"),
               "Part du soir", "#8d5b8c", "moon",
               sous_titre = paste0(sum(d$CRENEAU == "Midi"), " midi · ",
                                   sum(d$CRENEAU == "Soir"), " soir")),
      if (!length(duree)) NULL else
        kpi_tile(paste0(round(mean(duree)), " min"), "Durée moyenne",
                 COUL_NEUTRE, "hourglass-half",
                 sous_titre = paste0("médiane ", round(median(duree)), " min")),
      kpi_tile(format_pct(ratio_pct(sum(d$NB_PERS[d$LOCATION == "TERRASSE"],
                                        na.rm = TRUE),
                                    sum(d$NB_PERS, na.rm = TRUE))),
               "Part de la terrasse", COUL_LIEU[["TERRASSE"]], "sun",
               sous_titre = paste0(
                 sum(d$NB_PERS[d$LOCATION == "SALLE"], na.rm = TRUE), " en salle · ",
                 sum(d$NB_PERS[d$LOCATION == "TERRASSE"], na.rm = TRUE), " en terrasse"))
  )
}

#' Répartition des réservations par heure d'arrivée et par créneau.
graph_heures_resa <- function(resa, d1, d2, par = "lieu") {
  d <- resa %>% filter(DATE >= as.Date(d1), DATE <= as.Date(d2))
  if (!nrow(d)) return(plotly_empty(type = "scatter", mode = "markers") %>%
                         layout(title = list(text = "Aucune réservation")))
  dim <- dimension_resa(d, par)
  h <- d %>%
    mutate(H = factor(paste0(HEURE, "h"),
                      levels = paste0(sort(unique(HEURE)), "h")),
           GRP = dim$v) %>%
    group_by(H, GRP) %>%
    summarise(RESA = n(), COUVERTS = sum(NB_PERS, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(GRP), COUVERTS > 0)

  plot_ly(h, x = ~H, y = ~COUVERTS, color = ~GRP, type = "bar",
          colors = dim$couleurs,
          hovertemplate = ~paste0(H, " — ", GRP, "<br>", COUVERTS,
                                  " couverts<br>", RESA, " réservations",
                                  "<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = "Heure d'arrivée"),
           yaxis = list(title = "Couverts réservés"),
           legend = list(orientation = "h", y = -0.2, title = list(text = "")),
           margin = list(b = 60))
}

#' Répartition par jour de semaine et par taille de groupe.
graph_jours_resa <- function(resa, d1, d2, par = "lieu") {
  d <- resa %>% filter(DATE >= as.Date(d1), DATE <= as.Date(d2))
  if (!nrow(d)) return(plotly_empty(type = "scatter", mode = "markers") %>%
                         layout(title = list(text = "Aucune réservation")))
  dim <- dimension_resa(d, par)
  j <- d %>% mutate(GRP = dim$v) %>% group_by(JOUR, GRP) %>%
    summarise(COUVERTS = sum(NB_PERS, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(GRP), COUVERTS > 0)

  plot_ly(j, x = ~JOUR, y = ~COUVERTS, color = ~GRP, type = "bar",
          colors = dim$couleurs,
          hovertemplate = ~paste0(JOUR, " — ", GRP, "<br>", COUVERTS,
                                  " couverts<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = ""),
           yaxis = list(title = "Couverts réservés"),
           legend = list(orientation = "h", y = -0.25, title = list(text = "")),
           margin = list(b = 80))
}

##### Historique #####

#' Série temporelle des réservations, par semaine ou par mois.
historique_resa <- function(resa, unite = c("semaine", "mois")) {
  unite <- match.arg(unite)
  if (!nrow(resa)) return(tibble())
  resa %>%
    mutate(PERIODE = if (unite == "semaine") SEMAINE else MOIS) %>%
    group_by(PERIODE) %>%
    summarise(RESA = n(), COUVERTS = sum(NB_PERS, na.rm = TRUE),
              SALLE    = sum(NB_PERS[LOCATION == "SALLE"], na.rm = TRUE),
              TERRASSE = sum(NB_PERS[LOCATION == "TERRASSE"], na.rm = TRUE),
              TAILLE_MOY = round(mean(NB_PERS, na.rm = TRUE), 2),
              PCT_SOIR = ratio_pct(sum(NB_PERS[CRENEAU == "Soir"], na.rm = TRUE),
                                   sum(NB_PERS, na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(PCT_TERRASSE = ratio_pct(TERRASSE, COUVERTS)) %>%
    arrange(PERIODE)
}

#' Historique empilé salle / terrasse, avec la taille moyenne en courbe.
#'
#' L'empilement par lieu est le plus parlant ici : il montre d'un coup la
#' saisonnalité de la terrasse, que le total seul masque.
graph_historique_resa <- function(hist, unite = "semaine") {
  if (!nrow(hist)) return(plotly_empty(type = "scatter", mode = "markers") %>%
                            layout(title = list(text = "Aucun historique")))
  hover <- function(lib, v) paste0(format(hist$PERIODE, "%d/%m/%Y"), "<br>",
                                   lib, " : ", v, " couverts<extra></extra>")
  plot_ly(hist) %>%
    add_bars(x = ~PERIODE, y = ~SALLE, name = "Salle",
             marker = list(color = COUL_LIEU[["SALLE"]]),
             hovertemplate = hover("Salle", hist$SALLE)) %>%
    add_bars(x = ~PERIODE, y = ~TERRASSE, name = "Terrasse",
             marker = list(color = COUL_LIEU[["TERRASSE"]]),
             hovertemplate = hover("Terrasse", hist$TERRASSE)) %>%
    add_trace(x = ~PERIODE, y = ~TAILLE_MOY, name = "Taille moyenne",
              type = "scatter", mode = "lines", yaxis = "y2",
              line = list(color = COUL_AMBRE, width = 2.5),
              hovertemplate = ~paste0("Taille moyenne : ", TAILLE_MOY,
                                      "<extra></extra>")) %>%
    layout(barmode = "stack", xaxis = list(title = ""),
           yaxis = list(title = "Couverts réservés"),
           yaxis2 = list(overlaying = "y", side = "right",
                         title = "Personnes par résa", showgrid = FALSE),
           legend = list(orientation = "h", y = -0.2), margin = list(b = 60))
}

##### Lien réservations / chiffre d'affaires #####

#' Croise les couverts réservés et le CA, jour par jour.
#'
#' Attention à ce que ce croisement dit et ne dit pas : la réservation ne couvre
#' qu'une partie de la clientèle — le reste entre sans réserver. La relation
#' mesure donc l'apport des résas au CA, pas le remplissage de la salle.
resa_vs_ca <- function(resa, db_kpi, d1 = NULL, d2 = NULL) {
  if (!nrow(resa)) return(tibble())
  r <- resa %>%
    group_by(DATE) %>%
    summarise(RESA = n(), COUVERTS = sum(NB_PERS, na.rm = TRUE),
              COUVERTS_MIDI = sum(NB_PERS[CRENEAU == "Midi"], na.rm = TRUE),
              COUVERTS_SOIR = sum(NB_PERS[CRENEAU == "Soir"], na.rm = TRUE),
              .groups = "drop")

  d <- db_kpi %>%
    filter(ventes > 0) %>%
    select(DATE, CA = ventes, JOUR_SEMAINE) %>%
    left_join(r, by = "DATE") %>%
    mutate(across(c(RESA, COUVERTS, COUVERTS_MIDI, COUVERTS_SOIR),
                  ~replace_na(., 0)))

  if (!is.null(d1)) d <- filter(d, DATE >= as.Date(d1))
  if (!is.null(d2)) d <- filter(d, DATE <= as.Date(d2))

  # On ne garde que la période où les réservations existent : avant, l'absence
  # de résa est un défaut de collecte, pas un jour sans réservation.
  debut <- min(resa$DATE, na.rm = TRUE)
  d %>% filter(DATE >= debut) %>%
    mutate(CA_PAR_COUVERT = if_else(COUVERTS > 0, CA / COUVERTS, NA_real_))
}

#' Nuage CA vs couverts réservés, avec droite d'ajustement.
graph_resa_ca <- function(rc) {
  if (is.null(rc) || nrow(rc) < 3)
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas assez de jours pour comparer")))

  g <- plot_ly(rc, x = ~COUVERTS, y = ~CA, type = "scatter", mode = "markers",
               color = ~JOUR_SEMAINE,
               marker = list(size = 8, opacity = 0.75),
               hovertemplate = ~paste0(format(DATE, "%a %d/%m/%Y"),
                                       "<br>", COUVERTS, " couverts réservés",
                                       "<br>CA : ", format_CA(CA, -1),
                                       "<extra></extra>"))

  # Régression simple : la pente s'interprète en euros de CA par couvert
  # réservé. Elle n'a de sens que si les couverts varient assez.
  if (n_distinct(rc$COUVERTS) >= 5) {
    fit <- lm(CA ~ COUVERTS, data = rc)
    xs <- seq(min(rc$COUVERTS), max(rc$COUVERTS), length.out = 50)
    ys <- predict(fit, newdata = data.frame(COUVERTS = xs))
    r2 <- summary(fit)$r.squared
    g <- g %>%
      add_lines(x = xs, y = ys, inherit = FALSE, name = "Tendance",
                line = list(color = COUL_BRUN, width = 2, dash = "dash"),
                hovertemplate = paste0(
                  "Pente : ", format_CA(coef(fit)[2], 0), " de CA par couvert",
                  "<br>R² = ", round(r2, 2), "<extra></extra>"))
  }
  g %>% layout(xaxis = list(title = "Couverts réservés dans la journée"),
               yaxis = list(title = "CA du jour (€)"),
               legend = list(orientation = "h", y = -0.2), margin = list(b = 60))
}

#' Ce que la réservation apporte, résumé en tuiles.
kpi_resa_ca <- function(rc) {
  if (is.null(rc) || !nrow(rc))
    return(div(class = "text-muted small", "Pas de données croisées."))

  avec  <- rc %>% filter(COUVERTS > 0)
  sans  <- rc %>% filter(COUVERTS == 0)
  pente <- if (n_distinct(rc$COUVERTS) >= 5) coef(lm(CA ~ COUVERTS, rc))[2] else NA
  r2    <- if (n_distinct(rc$COUVERTS) >= 5) summary(lm(CA ~ COUVERTS, rc))$r.squared else NA

  div(class = "kpi-grid",
      kpi_tile(paste0(round(100 * nrow(avec) / nrow(rc)), " %"),
               "Jours avec réservation", COUL_BRUN, "calendar-check",
               sous_titre = paste(nrow(avec), "sur", nrow(rc), "jours ouverts")),
      kpi_tile(format_CA(mean(avec$CA, na.rm = TRUE), -1), "CA moyen avec résa",
               COUL_VERT, "arrow-trend-up",
               sous_titre = if (nrow(sans))
                 paste("sans :", format_CA(mean(sans$CA, na.rm = TRUE), -1)) else NULL),
      kpi_tile(format_CA(median(avec$CA_PAR_COUVERT, na.rm = TRUE), 0),
               "CA par couvert réservé", COUL_AMBRE, "receipt",
               sous_titre = "médiane — inclut les clients sans résa"),
      if (is.na(pente)) NULL else
        kpi_tile(format_CA(pente, 0), "CA par couvert supplémentaire",
                 if (pente > 0) COUL_VERT else COUL_ROUGE, "chart-line",
                 sous_titre = paste0("pente de régression · R² = ", round(r2, 2)))
  )
}
