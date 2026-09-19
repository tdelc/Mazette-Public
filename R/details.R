# Volet « Détails » : la loupe sur UNE période, au grain le plus fin disponible.
#
# ---------------------------------------------------------------------------
# Ce que ce volet est, et ce qu'il n'est plus
# ---------------------------------------------------------------------------
# Il répond à une seule question : « qu'est-ce qui s'est passé ce jour-là, cette
# semaine-là, ce mois-là ? » — et il y répond en DESCENDANT, de la vue
# d'ensemble au ticket.
#
# Tout ce qui relève du pilotage en a été retiré, parce que les mêmes chiffres
# vivent ailleurs et mieux :
#
#   - les coûts, la marge et les ratios de gestion sont dans Compta, qui les
#     donne au grain où ils sont VRAIS (le mois) ;
#   - les heures et leur productivité sont dans Travail ;
#   - la comparaison entre périodes est dans Comparaison.
#
# Les y laisser ici les dupliquait, et cette duplication coûtait deux fois :
# une page dense où l'information utile se noyait, et un risque de divergence
# entre deux écrans censés dire la même chose.
#
# ---------------------------------------------------------------------------
# Pourquoi la comptabilité a disparu de ce volet
# ---------------------------------------------------------------------------
# La comptabilité est MENSUELLE. Pour l'afficher sur une semaine, la version
# précédente étalait le mois au prorata des jours — et le signalait par un
# bandeau. Mais un prorata affiché reste un chiffre lu : on retenait « la
# semaine a coûté 4 200 € de matières » alors que personne ne l'a mesuré. Le
# bandeau prévenait ; il ne suffisait pas.
#
# Un chiffre qu'on ne peut pas mesurer à cette maille ne s'affiche donc plus à
# cette maille. On dit où le trouver, ce qui est plus utile qu'une estimation.

##### Les trois mailles #####

# Une maille définit tout : la barre du graphe d'ensemble, la période qu'un clic
# sélectionne, et la façon dont cette période se décompose juste en dessous.
#
# FENETRE est le nombre de jours affichés par défaut dans la vue d'ensemble :
# assez pour voir une tendance, pas au point de rendre les barres illisibles.
# COMPOSITION est la maille d'un cran plus fine — celle du deuxième graphe.
MAILLES_DETAIL <- tibble::tribble(
  ~CLE,      ~LIBELLE,      ~ICONE,          ~FENETRE, ~COMPOSITION, ~COMPO_TITRE,
  "jour",    "Par jour",    "calendar-day",        56, "heure",      "Heure par heure",
  "semaine", "Par semaine", "calendar-week",      182, "jour",       "Jour par jour",
  "mois",    "Par mois",    "calendar-days",      730, "semaine",    "Semaine par semaine"
)

# Ligne du registre, avec repli sur le jour : un select vidé par un changement
# d'onglet ne doit pas faire tomber le volet.
maille_detail <- function(cle) {
  i <- match(cle %||% "", MAILLES_DETAIL$CLE)
  if (is.na(i)) i <- match("jour", MAILLES_DETAIL$CLE)
  as.list(MAILLES_DETAIL[i, ])
}

# debut_periode() de R/helpers.R ne connaît pas le jour — il n'en a jamais eu
# besoin. Ici si : la maille « jour » est une maille comme les autres, et tout
# le volet serait truffé de cas particuliers sans ces deux enveloppes.
debut_maille <- function(d, maille) {
  d <- as.Date(d)
  if (identical(maille, "jour")) d else debut_periode(d, maille)
}

fin_maille <- function(d, maille) {
  d <- as.Date(d)
  if (identical(maille, "jour")) d else fin_periode(debut_maille(d, maille), maille)
}

# Libellé d'une période, indépendant de la locale du SERVEUR.
#
# label_periode() de R/helpers.R rend « September 2026 » sur une machine en
# locale C — le cas courant d'un serveur Shiny — parce qu'il passe par %B. On
# repasse donc par vecteur_mois_court et vecteur_jours, comme partout ailleurs
# dans le dashboard depuis que le piège a été identifié.
libelle_maille <- function(d, maille) {
  # On normalise à l'entrée : la fonction doit nommer la PÉRIODE qui contient
  # cette date, pas la date elle-même. Sans cela, un jeudi passé à la maille
  # « semaine » rendait « Semaine du 17/09 au 20/09 » — bornes fausses et
  # durée de quatre jours.
  d <- debut_maille(d, maille)
  switch(maille,
         jour    = paste0(vecteur_jours[wday(d, week_start = 1)], " ",
                          format(d, "%d/%m/%Y")),
         semaine = paste0("Semaine du ", format(d, "%d/%m"), " au ",
                          format(fin_maille(d, "semaine"), "%d/%m/%Y")),
         mois    = paste0(vecteur_mois_court[month(d)], " ", year(d)),
         format(d, "%d/%m/%Y"))
}

# Libellé court, pour l'axe d'un graphe où la place manque.
libelle_maille_court <- function(d, maille) {
  d <- debut_maille(d, maille)
  switch(maille,
         jour    = paste0(substr(vecteur_jours[wday(d, week_start = 1)], 1, 3),
                          " ", format(d, "%d/%m")),
         semaine = paste0("S ", format(d, "%d/%m")),
         mois    = paste0(vecteur_mois_court[month(d)], " ", substr(year(d), 3, 4)),
         format(d, "%d/%m"))
}

# La période précédente à laquelle se comparer.
#
# Pour un jour, c'est le MÊME JOUR DE LA SEMAINE une semaine plus tôt, et non
# la veille : un lundi ne se compare pas à un dimanche. Pour une semaine ou un
# mois, la période immédiatement précédente suffit — la saisonnalité y joue
# déjà à l'intérieur.
periode_precedente <- function(d, maille) {
  d <- as.Date(d)
  switch(maille,
         jour    = d - 7,
         semaine = d - 7,
         mois    = debut_periode(d %m-% months(1), "mois"),
         d)
}

# Comment nommer cette comparaison à l'écran.
libelle_precedente <- function(maille) {
  switch(maille,
         jour    = "même jour, semaine précédente",
         semaine = "semaine précédente",
         mois    = "mois précédent",
         "période précédente")
}

##### Vue d'ensemble : la série de la fenêtre #####

# Une ligne par période de la maille, avec son CA et son objectif.
#
# Les jours sans vente sont ÉCARTÉS à la maille « jour » (un jour de fermeture
# n'est pas un mauvais jour), mais conservés dans l'agrégat des mailles
# supérieures : une semaine avec un jour de fermeture reste une semaine.
serie_detail <- function(db_kpi, db_obj, d1, d2, maille = "jour") {
  if (is.null(db_kpi) || !nrow(db_kpi)) return(NULL)
  d1 <- as.Date(d1); d2 <- as.Date(d2)

  d <- db_kpi %>%
    select(DATE, CA = ventes) %>%
    left_join(db_obj %>% select(DATE, OBJECTIF = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2)
  if (!nrow(d)) return(NULL)

  if (identical(maille, "jour")) d <- filter(d, CA > 0)
  if (!nrow(d)) return(NULL)

  d %>%
    mutate(PERIODE = debut_maille(DATE, maille)) %>%
    group_by(PERIODE) %>%
    # JOURS est calculé AVANT CA, et l'ordre n'est pas cosmétique : summarise()
    # évalue ses arguments dans l'ordre, et chacun voit les précédents. Placé
    # après, `sum(CA > 0)` lisait le CA déjà agrégé — un scalaire — et rendait
    # 1 pour toutes les périodes.
    summarise(JOURS = sum(CA > 0, na.rm = TRUE),
              CA = sum(CA, na.rm = TRUE),
              OBJECTIF = sum(OBJECTIF, na.rm = TRUE), .groups = "drop") %>%
    relocate(PERIODE, CA, OBJECTIF, JOURS) %>%
    filter(CA > 0) %>%
    mutate(LABEL = libelle_maille_court(PERIODE, maille)) %>%
    arrange(PERIODE)
}

# Le graphe d'ensemble : une barre par période, le trait de l'objectif.
#
# `customdata` porte la date en texte : c'est elle qui identifie la barre au
# clic. L'axe affiche des libellés (« ven 11/09 »), et as.Date() sur un libellé
# lève — le piège a déjà coûté un onglet cassé ailleurs dans ce dashboard.
graph_serie_detail <- function(serie, maille = "jour", source = "det_serie") {
  if (is.null(serie) || !nrow(serie))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune vente sur la fenêtre")))

  ordre <- factor(serie$LABEL, levels = serie$LABEL)
  couleurs <- couleur_objectif(serie$CA, serie$OBJECTIF)
  atteinte <- label_objectif(serie$CA, serie$OBJECTIF)
  cd <- as.character(serie$PERIODE)

  p <- plot_ly(source = source) %>%
    add_bars(x = ordre, y = serie$CA, name = "CA",
             marker = list(color = couleurs), customdata = cd,
             hovertemplate = paste0(
               "<b>", libelle_maille(serie$PERIODE, maille), "</b><br>",
               format_CA(serie$CA, -1), "<br>", atteinte,
               "<br><i>cliquez pour détailler</i><extra></extra>"))

  if (any(serie$OBJECTIF > 0, na.rm = TRUE))
    p <- p %>% add_lines(
      x = ordre, y = serie$OBJECTIF, name = "Objectif", customdata = cd,
      line = list(color = "#260b01", dash = "dot", width = 1),
      hovertemplate = paste0("objectif ", format_CA(serie$OBJECTIF, -1),
                             "<extra></extra>"))

  p %>%
    layout(xaxis = list(title = "", tickangle = -35),
           yaxis = list(title = "CA (€)", rangemode = "tozero"),
           bargap = 0.3, legend = list(orientation = "h", y = -0.28),
           margin = list(b = 80),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
    change_cursor_plotly()
}

##### La période choisie : le résumé #####

# Tout ce qu'on veut savoir d'une période, en une liste.
#
# Le nombre de tickets et le panier moyen ne sont calculables que si DB_TICKET
# porte ID_TICKET (cf. R/donnees.R). Quand il manque, les deux valent NA et
# l'écran le dit — plutôt que d'afficher un zéro qui se lirait comme « aucun
# ticket ».
resume_detail <- function(db_kpi, db_obj, db_ticket, periode, maille = "jour") {
  if (is.null(db_kpi) || !nrow(db_kpi)) return(NULL)
  d1 <- debut_maille(periode, maille); d2 <- fin_maille(periode, maille)

  somme <- function(a, b) {
    d <- db_kpi %>%
      select(DATE, CA = ventes) %>%
      left_join(db_obj %>% select(DATE, OBJECTIF = ventes), by = "DATE") %>%
      filter(DATE >= a, DATE <= b)
    list(CA = sum(d$CA, na.rm = TRUE),
         OBJECTIF = sum(d$OBJECTIF[d$CA > 0], na.rm = TRUE),
         JOURS = sum(d$CA > 0, na.rm = TRUE))
  }

  act <- somme(d1, d2)
  p0  <- periode_precedente(d1, maille)
  prec <- somme(p0, fin_maille(p0, maille))

  tk <- tickets_detail(db_ticket, d1, d2)
  n_tickets <- if (is.null(tk)) NA_integer_ else nrow(tk)

  list(
    PERIODE = d1, D1 = d1, D2 = d2, MAILLE = maille,
    LIBELLE = libelle_maille(d1, maille),
    CA = act$CA, OBJECTIF = act$OBJECTIF, JOURS = act$JOURS,
    ECART = act$CA - act$OBJECTIF,
    ECART_PCT = if (act$OBJECTIF > 0) 100 * (act$CA - act$OBJECTIF) / act$OBJECTIF
                else NA_real_,
    CA_PREC = prec$CA,
    EVOL_PCT = if (prec$CA > 0) 100 * (act$CA - prec$CA) / prec$CA else NA_real_,
    LIB_PREC = libelle_precedente(maille),
    N_TICKETS = n_tickets,
    PANIER = if (is.na(n_tickets) || n_tickets == 0) NA_real_
             else act$CA / n_tickets)
}

# Les tuiles de la période. Tout y est en ÉCART ou en niveau simple : aucune
# grandeur dérivée d'une autre dérivée, aucune estimation.
kpi_detail <- function(res) {
  if (is.null(res))
    return(div(class = "text-muted small p-2",
               "Cliquez une barre ci-dessus pour détailler une période."))

  signe <- function(x) if (!is.na(x) && x >= 0) "+" else ""
  coul_ecart <- if (is.na(res$ECART_PCT)) COUL_NEUTRE
                else if (res$ECART >= 0) COUL_VERT else COUL_ROUGE
  coul_evol <- if (is.na(res$EVOL_PCT)) COUL_NEUTRE
               else if (res$EVOL_PCT >= 0) COUL_VERT else COUL_ROUGE

  div(
    class = "kpi-grid",
    kpi_tile(format_CA(res$CA, -1), "Chiffre d'affaires", COUL_BRUN, "euro-sign",
             sous_titre = if (res$MAILLE == "jour") NULL
                          else paste0(res$JOURS, " jour",
                                      if (res$JOURS > 1) "s" else "", " ouvert",
                                      if (res$JOURS > 1) "s" else "")),
    kpi_tile(if (is.na(res$ECART_PCT)) "—"
             else paste0(signe(res$ECART), format_CA(res$ECART, -1)),
             "Écart à l'objectif", coul_ecart, "bullseye",
             sous_titre = if (res$OBJECTIF > 0)
               paste0("objectif ", format_CA(res$OBJECTIF, -1)) else "sans objectif"),
    kpi_tile(if (is.na(res$EVOL_PCT)) "—"
             else paste0(signe(res$EVOL_PCT), format_pct(res$EVOL_PCT)),
             "Évolution", coul_evol, "arrow-trend-up",
             sous_titre = paste0("vs ", res$LIB_PREC, " (",
                                 format_CA(res$CA_PREC, -1), ")")),
    kpi_tile(if (is.na(res$N_TICKETS)) "—" else format(res$N_TICKETS),
             "Tickets", COUL_AMBRE, "receipt",
             sous_titre = if (is.na(res$N_TICKETS)) "identifiant absent du cache"
                          else "lignes de caisse regroupées"),
    kpi_tile(if (is.na(res$PANIER)) "—" else format_CA(res$PANIER, 2),
             "Panier moyen", COUL_MATIERE, "basket-shopping",
             sous_titre = "CA / ticket")
  )
}

##### Comment la période se compose #####

# Un cran plus fin que la maille choisie : les heures d'un jour, les jours
# d'une semaine, les semaines d'un mois.
#
# Les heures viennent de DB_TICKET, les jours et semaines de db_kpi : c'est la
# même grandeur, mais db_kpi porte le CA corrigé du jour (cf. import.R) et fait
# donc foi dès qu'on peut l'utiliser.
composition_detail <- function(db_kpi, db_obj, db_ticket, periode,
                               maille = "jour", unite_tva = "HTVA") {
  d1 <- debut_maille(periode, maille); d2 <- fin_maille(periode, maille)
  compo <- maille_detail(maille)$COMPOSITION

  if (identical(compo, "heure")) {
    if (is.null(db_ticket) || !nrow(db_ticket)) return(NULL)
    col <- paste0("CA_", unite_tva)
    if (!col %in% names(db_ticket)) col <- "PRIX_TOTAL"
    d <- db_ticket %>% filter(DATE >= d1, DATE <= d2, .data[[col]] > 0)
    if (!nrow(d)) return(NULL)
    # Une heure est toujours entière : COMPLET vaut TRUE, mais la colonne doit
    # exister pour que le graphe n'ait pas à connaître la maille.
    #
    # OBJECTIF vaut NA et non zéro : aucun objectif n'est fixé à l'heure. Zéro
    # se lirait « objectif dépassé » et peindrait la journée en vert.
    return(d %>%
      group_by(HEURE) %>%
      summarise(CA = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
      transmute(CLE = HEURE, LABEL = sprintf("%02dh", HEURE), CA,
                OBJECTIF = NA_real_, COMPLET = TRUE) %>%
      arrange(CLE))
  }

  if (is.null(db_kpi) || !nrow(db_kpi)) return(NULL)
  d <- db_kpi %>%
    select(DATE, CA = ventes) %>%
    left_join(db_obj %>% select(DATE, OBJECTIF = ventes), by = "DATE") %>%
    filter(DATE >= d1, DATE <= d2)
  if (!nrow(d)) return(NULL)
  d %>%
    mutate(CLE = debut_maille(DATE, compo)) %>%
    group_by(CLE) %>%
    # L'objectif ne compte que les jours OUVERTS, comme dans resume_detail() :
    # un jour de fermeture n'a pas d'objectif à rater. Et il est tronqué de la
    # même façon que le CA — une semaine à cheval compare donc bien ses trois
    # jours affichés à l'objectif de ces trois jours-là.
    #
    # OBJECTIF est calculé AVANT CA : summarise() évalue ses arguments dans
    # l'ordre et chacun voit les précédents. Placé après, `CA > 0` lirait le CA
    # déjà agrégé — un scalaire — et l'objectif du jour de fermeture rentrerait
    # dans le total. Le piège a déjà coûté une colonne fausse dans ce fichier.
    summarise(OBJECTIF = sum(OBJECTIF[CA > 0], na.rm = TRUE),
              CA = sum(CA, na.rm = TRUE),
              .groups = "drop") %>%
    relocate(CLE, CA, OBJECTIF) %>%
    # Une semaine à cheval sur deux mois n'est comptée QUE pour ses jours du
    # mois affiché — c'est le seul calcul juste. Mais l'étiquette « S 31/08 »
    # se lit alors comme la semaine entière, et on comparerait 3 jours à 7.
    # On marque donc les périodes tronquées plutôt que de laisser croire.
    mutate(COMPLET = CLE >= d1 & fin_maille(CLE, compo) <= d2,
           LABEL = paste0(libelle_maille_court(CLE, compo),
                          if_else(COMPLET, "", " *"))) %>%
    arrange(CLE)
}

# La décomposition, barre par barre.
#
# La couleur dit l'atteinte de l'objectif, exactement comme dans le graphe
# d'ensemble : deux graphes qui montrent la même grandeur ne peuvent pas avoir
# deux conventions de couleur.
#
# La troncature ne peut donc plus être un gris — il est déjà pris par « sans
# objectif ». Elle passe en hachure et en transparence, qui se superposent à
# n'importe quelle couleur sans en changer la lecture.
graph_composition_detail <- function(comp, maille = "jour") {
  if (is.null(comp) || !nrow(comp))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune vente à décomposer")))

  ordre <- factor(comp$LABEL, levels = comp$LABEL)
  part <- ratio_pct(comp$CA, sum(comp$CA, na.rm = TRUE))
  obj <- if ("OBJECTIF" %in% names(comp)) comp$OBJECTIF else rep(NA_real_, nrow(comp))

  # Sans aucun objectif à cette finesse (les heures d'une journée), la couleur
  # ne dirait rien : une seule teinte vaut mieux qu'un gris « sans objectif »
  # répété, qui se lirait comme un jugement.
  couleurs <- if (all(is.na(obj))) rep(COUL_BRUN, nrow(comp))
              else couleur_objectif(comp$CA, obj)
  atteinte <- if (all(is.na(obj))) rep("", nrow(comp))
              else paste0("<br>", label_objectif(comp$CA, obj))

  plot_ly() %>%
    add_bars(x = ordre, y = comp$CA, name = "CA",
             marker = list(
               color = couleurs,
               opacity = if_else(comp$COMPLET, 1, 0.45),
               pattern = list(shape = if_else(comp$COMPLET, "", "/"),
                              fgcolor = "#fffaf4", size = 6, solidity = 0.3)),
             hovertemplate = paste0("<b>", comp$LABEL, "</b><br>",
                                    format_CA(comp$CA, -1), atteinte, "<br>",
                                    format_pct(part), " de la période",
                                    if_else(comp$COMPLET, "",
                                            "<br><i>période à cheval : seuls les jours affichés comptent</i>"),
                                    "<extra></extra>")) %>%
    layout(xaxis = list(title = "", tickangle = -35),
           yaxis = list(title = "CA (€)", rangemode = "tozero"),
           bargap = 0.3, showlegend = FALSE, margin = list(b = 70),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Le CA de la période coupé selon les deux axes que db_kpi porte déjà :
# boisson / nourriture, et midi / soir.
#
# Deux axes et pas un de plus : ce sont les seuls que la table donne SANS
# recalcul, et donc les seuls qui ne puissent pas diverger d'un autre écran.
repartition_detail <- function(db_kpi, periode, maille = "jour") {
  d1 <- debut_maille(periode, maille); d2 <- fin_maille(periode, maille)
  if (is.null(db_kpi) || !nrow(db_kpi)) return(NULL)
  d <- db_kpi %>% filter(DATE >= d1, DATE <= d2)
  if (!nrow(d)) return(NULL)

  bloc <- function(axe, cols) {
    dispo <- intersect(cols, names(d))
    if (!length(dispo)) return(NULL)
    tibble(AXE = axe, PART = dispo,
           CA = vapply(dispo, function(c) sum(d[[c]], na.rm = TRUE), 0))
  }
  res <- bind_rows(bloc("Nature", c("Boisson", "Nourriture")),
                   bloc("Moment", c("Jour", "Soir")))
  if (is.null(res) || !nrow(res) || sum(res$CA, na.rm = TRUE) <= 0) return(NULL)
  res %>% group_by(AXE) %>% mutate(PCT = ratio_pct(CA, sum(CA, na.rm = TRUE))) %>%
    ungroup()
}

PAL_REPARTITION <- c("Boisson" = "#d98236", "Nourriture" = "#5B7B5A",
                     "Jour" = "#e9c46a", "Soir" = "#5b7b9b")

graph_repartition_detail <- function(rep) {
  if (is.null(rep) || !nrow(rep))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de répartition disponible")))

  p <- plot_ly()
  for (i in seq_len(nrow(rep))) {
    r <- rep[i, ]
    p <- add_bars(p, x = r$CA, y = r$AXE, orientation = "h", name = r$PART,
                  marker = list(color = unname(PAL_REPARTITION[r$PART]) %|%
                                  COUL_NEUTRE),
                  hovertemplate = paste0("<b>", r$PART, "</b><br>",
                                         format_CA(r$CA, -1), "<br>",
                                         format_pct(r$PCT), "<extra></extra>"))
  }
  p %>% layout(barmode = "stack",
               xaxis = list(title = "CA (€)"), yaxis = list(title = ""),
               legend = list(orientation = "h", y = -0.3),
               margin = list(l = 10, b = 60),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Petit utilitaire : `a %|% b` rend b quand a est NA ou vide. Les palettes
# nommées rendent NA sur une clé inconnue, et un NA de couleur fait tomber
# plotly sans message utile.
`%|%` <- function(a, b) if (length(a) == 0 || is.na(a)) b else a

##### Le grain fin : produits, heures, tickets #####

# Nom de la colonne de CA correspondant à l'unité de TVA, avec repli.
colonne_ca <- function(db, unite_tva = "HTVA") {
  col <- paste0("CA_", unite_tva)
  if (col %in% names(db)) col else if ("CA_HTVA" %in% names(db)) "CA_HTVA"
  else "PRIX_TOTAL"
}

# Ce qui s'est vendu sur la période, du plus gros au plus petit.
produits_detail <- function(db_produits, periode, maille = "jour", n = 20,
                            unite_tva = "HTVA") {
  if (is.null(db_produits) || !nrow(db_produits)) return(NULL)
  d1 <- debut_maille(periode, maille); d2 <- fin_maille(periode, maille)
  col <- colonne_ca(db_produits, unite_tva)

  d <- db_produits %>%
    filter(DATE >= d1, DATE <= d2) %>%
    group_by(PRODUIT) %>%
    summarise(QUANTITE = sum(QUANTITE, na.rm = TRUE),
              CA = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    filter(CA > 0) %>%
    arrange(desc(CA))
  if (!nrow(d)) return(NULL)
  d %>% mutate(PART = ratio_pct(CA, sum(CA, na.rm = TRUE)),
               CUMUL = ratio_pct(cumsum(CA), sum(CA, na.rm = TRUE))) %>%
    head(n)
}

table_produits_detail <- function(prod, unite_tva = "HTVA") {
  if (is.null(prod) || !nrow(prod))
    return(tibble(Info = "Aucune vente sur la période."))
  prod %>% transmute(
    Produit = tronque_nom(PRODUIT, 45),
    Quantité = QUANTITE,
    !!paste("CA", unite_tva) := trimws(format_CA(CA, -1)),
    `Part` = format_pct(PART),
    `Cumul` = format_pct(CUMUL))
}

##### Les tickets #####

# DB_TICKET ne porte ID_TICKET que depuis qu'il a été ajouté aux colonnes
# conservées (cf. TICKET_COLONNES dans R/donnees.R). Un .RData plus ancien n'en
# a pas, et le volet doit alors se taire proprement plutôt que d'inventer un
# regroupement — deux lignes de la même minute ne sont pas forcément le même
# ticket.
tickets_disponibles <- function(db_ticket) {
  !is.null(db_ticket) && is.data.frame(db_ticket) &&
    "ID_TICKET" %in% names(db_ticket) && nrow(db_ticket) > 0
}

# Une ligne par ticket : l'heure, ce qu'il contenait, ce qu'il a rapporté.
tickets_detail <- function(db_ticket, d1, d2, unite_tva = "HTVA") {
  if (!tickets_disponibles(db_ticket)) return(NULL)
  col <- colonne_ca(db_ticket, unite_tva)

  d <- db_ticket %>%
    filter(DATE >= as.Date(d1), DATE <= as.Date(d2), !is.na(ID_TICKET))
  if (!nrow(d)) return(NULL)

  d %>%
    group_by(ID_TICKET) %>%
    summarise(DATE = min(DATE), HEURE = min(HEURE, na.rm = TRUE),
              LIGNES = n(), ARTICLES = sum(QUANTITE, na.rm = TRUE),
              CA = sum(.data[[col]], na.rm = TRUE),
              # Le ticket se lit par son contenu, pas par son numéro : on
              # compose donc son résumé ici, une fois, plutôt qu'au rendu.
              CONTENU = paste(head(unique(PRODUIT), 4), collapse = " · "),
              AUTRES = max(0L, n_distinct(PRODUIT) - 4L), .groups = "drop") %>%
    arrange(DATE, HEURE, ID_TICKET)
}

table_tickets <- function(tk, unite_tva = "HTVA") {
  if (is.null(tk))
    return(tibble(Info = paste0(
      "Le détail des tickets demande la colonne ID_TICKET, absente du cache. ",
      "Elle apparaîtra au prochain import complet.")))
  if (!nrow(tk)) return(tibble(Info = "Aucun ticket sur la période."))
  tk %>% transmute(
    Jour = format(DATE, "%d/%m"),
    Heure = sprintf("%02dh", HEURE),
    Ticket = ID_TICKET,
    Contenu = ifelse(AUTRES > 0, paste0(tronque_nom(CONTENU, 60), " (+", AUTRES, ")"),
                     tronque_nom(CONTENU, 60)),
    Articles = ARTICLES,
    !!paste("CA", unite_tva) := trimws(format_CA(CA, -1)))
}

# Le détail d'un ticket : ses lignes, telles que la caisse les a enregistrées.
lignes_ticket <- function(db_ticket, id, unite_tva = "HTVA") {
  if (!tickets_disponibles(db_ticket) || is.null(id) || is.na(id))
    return(tibble(Info = "Sélectionnez un ticket pour en voir les lignes."))
  col <- colonne_ca(db_ticket, unite_tva)
  d <- db_ticket %>% filter(ID_TICKET == id)
  if (!nrow(d)) return(tibble(Info = "Ticket introuvable."))
  d %>%
    arrange(desc(.data[[col]])) %>%
    # PRODUIT_FULL et non PRODUIT : le ticket se lit tel que la caisse l'a
    # enregistré, avec ses libellés libres. Le nom canonique, lui, sert aux
    # agrégats (« Ce qui s'est vendu »), où il regroupe les variantes.
    transmute(Produit = tronque_nom(PRODUIT_FULL, 55),
              Catégorie = CATEGORIE,
              Quantité = QUANTITE,
              !!paste("CA", unite_tva) := trimws(format_CA(.data[[col]], -1)))
}

# Distribution des tickets par tranche de montant : où se situe le panier.
#
# Une moyenne de panier cache tout : deux services à 18 € de moyenne peuvent
# être « beaucoup de petits tickets » ou « quelques gros ». L'histogramme le
# dit, la moyenne non.
graph_paniers <- function(tk) {
  if (is.null(tk) || !nrow(tk))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de tickets à distribuer")))

  plot_ly() %>%
    add_histogram(x = tk$CA, nbinsx = 30,
                  marker = list(color = COUL_AMBRE,
                                line = list(color = "#fffaf4", width = 1)),
                  hovertemplate = "%{y} tickets entre %{x}<extra></extra>") %>%
    layout(xaxis = list(title = "Montant du ticket (€)"),
           yaxis = list(title = "Nombre de tickets"),
           showlegend = FALSE, bargap = 0.05,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

##### Les heures pointées #####

# Les heures POINTÉES du jour, par secteur — pas les heures recalées.
#
# Depuis le recalage sur la paie (cf. R/sources_travail.R), HEURES porte un
# total mensuel réparti sur les jours. À la maille du jour, cette valeur est
# une répartition et non une mesure : afficher « 12,4 h le mardi » lui donnerait
# une précision qu'elle n'a pas. HEURES_HOREKO, lui, est ce que le pointage a
# enregistré ce jour-là. C'est la seule des deux qui ait un sens ici.
heures_pointees <- function(db_couts, periode, maille = "jour") {
  if (is.null(db_couts) || !nrow(db_couts)) return(NULL)
  d1 <- debut_maille(periode, maille); d2 <- fin_maille(periode, maille)
  col <- if ("HEURES_HOREKO" %in% names(db_couts)) "HEURES_HOREKO" else "HEURES"

  d <- db_couts %>% filter(DATE >= d1, DATE <= d2)
  if (!nrow(d)) return(NULL)
  d %>%
    group_by(SECTEUR, CRENEAU) %>%
    summarise(HEURES = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    filter(HEURES > 0) %>%
    arrange(SECTEUR, CRENEAU)
}

table_heures_pointees <- function(h, ca = NA_real_) {
  if (is.null(h) || !nrow(h))
    return(tibble(Info = "Aucune heure pointée sur la période."))
  total <- sum(h$HEURES, na.rm = TRUE)
  bind_rows(
    h %>% transmute(Secteur = SECTEUR, Créneau = as.character(CRENEAU),
                    Heures = round(HEURES, 1)),
    tibble(Secteur = "Total", Créneau = "", Heures = round(total, 1))) %>%
    mutate(`CA / heure` = c(rep("", nrow(h)),
                            if (is.na(ca) || total <= 0) "—"
                            else trimws(format_CA(ca / total, -1))))
}

##### Les heures réelles, au mois #####

# Au MOIS, et au mois seulement, les heures cessent d'être une estimation.
#
# Le pointage est la seule mesure quotidienne, mais il n'est qu'un pointage :
# il ignore les heures payées sans être pointées (congés, maladie, solde) et
# son coût est un tarif horaire supposé. La paie, elle, donne des heures
# réellement payées et un coût employeur réel, par mois et par secteur ; la
# comptabilité donne le coût du mois, sans secteur ni heures. Ces deux-là ne
# se découpent pas plus fin — les afficher à la semaine reviendrait à répartir
# un total, c'est-à-dire à inventer.
#
# D'où la règle de ce volet : jour et semaine montrent le pointage, le mois
# montre les trois sources côte à côte.
heures_mois <- function(db_couts, db_onss = NULL, periode = NULL) {
  m <- debut_maille(periode, "mois")
  sect <- sources_par_secteur(db_couts, db_onss)
  if (is.null(sect) || !nrow(sect)) return(NULL)

  d <- sect %>% filter(PERIODE == m)
  if (!nrow(d)) return(NULL)

  paie_la <- any(d$SOURCE == "paie")
  res <- d %>%
    group_by(SECTEUR) %>%
    summarise(H_POINTEES = sum(HEURES[SOURCE == "horeko"], na.rm = TRUE),
              H_PAYEES   = sum(HEURES[SOURCE == "paie"], na.rm = TRUE),
              COUT_PAIE  = sum(COUT[SOURCE == "paie"], na.rm = TRUE),
              .groups = "drop")

  # Sans fichier de paie pour ce mois, les colonnes correspondantes valent NA
  # et non zéro : « la paie ne dit rien » et « la paie dit zéro heure » se
  # lisent de deux façons opposées.
  if (!paie_la) res <- res %>% mutate(H_PAYEES = NA_real_, COUT_PAIE = NA_real_)

  res %>% filter(H_POINTEES > 0 | replace_na(H_PAYEES, 0) > 0 |
                   replace_na(COUT_PAIE, 0) != 0) %>%
    arrange(SECTEUR)
}

# Le coût comptable du mois, s'il est connu. Il ne se ventile pas : c'est un
# total, et il figure comme tel — une ligne à part, sans secteur ni heures.
cout_compta_mois <- function(db_couts, periode = NULL) {
  if (is.null(db_couts) || !nrow(db_couts) ||
      !"COUT_COMPTA" %in% names(db_couts)) return(NA_real_)
  m <- debut_maille(periode, "mois")
  d <- db_couts %>%
    filter(DATE >= m, DATE <= fin_maille(m, "mois")) %>%
    distinct(COUT_COMPTA)
  v <- d$COUT_COMPTA[!is.na(d$COUT_COMPTA)]
  if (!length(v)) NA_real_ else sum(v)
}

table_heures_mois <- function(h, compta = NA_real_, ca = NA_real_) {
  if (is.null(h) || !nrow(h))
    return(tibble(Info = "Aucune heure sur ce mois."))

  euro <- function(x) ifelse(is.na(x), "—", trimws(format_CA(x, -1)))
  # sprintf et non format() : format() aligne sur la plus large valeur du
  # vecteur, et rend « 12,0 » a cote de «  1 234,0 » avec des blancs devant.
  heure <- function(x) ifelse(is.na(x), "—", sprintf("%.1f", x))

  tot_p <- sum(h$H_POINTEES, na.rm = TRUE)
  tot_y <- if (all(is.na(h$H_PAYEES))) NA_real_ else sum(h$H_PAYEES, na.rm = TRUE)
  tot_c <- if (all(is.na(h$COUT_PAIE))) NA_real_ else sum(h$COUT_PAIE, na.rm = TRUE)

  # Le CA par heure se calcule sur les heures PAYÉES dès qu'elles existent :
  # ce sont celles que l'entreprise a effectivement supportées.
  base <- if (!is.na(tot_y) && tot_y > 0) tot_y else tot_p

  corps <- h %>% transmute(
    Secteur = SECTEUR,
    `Heures pointées` = heure(H_POINTEES),
    `Heures payées`   = heure(H_PAYEES),
    `Coût paie`       = euro(COUT_PAIE))

  bind_rows(
    corps,
    tibble(Secteur = "Total", `Heures pointées` = heure(tot_p),
           `Heures payées` = heure(tot_y), `Coût paie` = euro(tot_c)),
    tibble(Secteur = "Coût comptable du mois", `Heures pointées` = "—",
           `Heures payées` = "—", `Coût paie` = euro(compta)),
    tibble(Secteur = if (!is.na(tot_y) && tot_y > 0) "CA par heure payée"
                     else "CA par heure pointée",
           `Heures pointées` = "—", `Heures payées` = "—",
           `Coût paie` = if (is.na(ca) || is.na(base) || base <= 0) "—"
                         else euro(ca / base)))
}

# Le tableau des heures de la période, selon ce que la maille permet de dire.
table_heures_periode <- function(db_couts, db_onss, periode, maille = "jour",
                                 ca = NA_real_) {
  if (identical(maille, "mois")) {
    h <- heures_mois(db_couts, db_onss, periode)
    if (!is.null(h))
      return(table_heures_mois(h, cout_compta_mois(db_couts, periode), ca))
  }
  table_heures_pointees(heures_pointees(db_couts, periode, maille), ca)
}

##### Ce qui n'est plus ici #####

# Le renvoi vers les onglets qui portent les chiffres retirés de ce volet.
#
# Un onglet qui se contente de ne plus afficher quelque chose laisse l'utilisateur
# croire que le chiffre n'existe pas. Dire où il est coûte trois lignes et
# évite la question.
bandeau_ailleurs <- function(maille = "jour") {
  motif <- switch(
    maille,
    jour = paste0("La comptabilité est mensuelle : aucun coût ni aucune marge ",
                  "ne se mesure à la journée. "),
    semaine = paste0("La comptabilité est mensuelle : les coûts d'une semaine ",
                     "ne peuvent être qu'un prorata du mois, et un prorata ",
                     "affiché finit par être lu comme une mesure. "),
    paste0("Les coûts et la marge du mois sont réels, mais l'onglet Compta les ",
           "donne avec leur historique et leurs contrôles. "))

  # tagList et non paste0 : coller une balise Shiny dans une chaîne la rend
  # par as.character(), et div() échappe ensuite le HTML — on lirait
  # « <b>Compta</b> » en toutes lettres à l'écran.
  bandeau_alerte(
    TRUE,
    tagList(motif,
            "Coûts, marge et ratios sont dans l'onglet ", tags$b("Compta"),
            " ; les heures et la productivité dans ", tags$b("Travail"),
            " ; la comparaison entre périodes dans ", tags$b("Comparaison"), "."),
    titre = "Coûts et marge : voir ailleurs", couleur = COUL_NEUTRE,
    icone = "circle-info")
}

##### Sous-onglet « Par produit » #####
#
# L'autre axe du volet : un produit, suivi semaine après semaine. Ces trois
# fonctions viennent telles quelles de la version précédente — elles n'avaient
# aucun défaut, et la refonte ne portait pas sur cet axe.

# Les produits vendus sur une période, du plus gros au plus petit.
liste_produits_periode <- function(db_produits, d1, d2, unite_tva = "HTVA") {
  col <- colonne_ca(db_produits, unite_tva)
  db_produits %>%
    filter(DATE >= as.Date(d1), DATE <= as.Date(d2)) %>%
    group_by(Produit = PRODUIT) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(CA))
}

# Évolution hebdomadaire d'un produit, avec ce qu'il pèse dans le CA total et
# dans sa propre catégorie.
#
# Les deux parts ne disent pas la même chose, et c'est la seconde qui compte :
# un produit dont la part totale baisse pendant que sa part de catégorie monte
# ne perd pas de terrain — c'est sa famille qui recule.
evolution_un_produit <- function(db_produits, produit, d1, d2, unite_tva = "HTVA") {
  col <- colonne_ca(db_produits, unite_tva)
  db_produits %>%
    filter(DATE >= as.Date(d1), DATE <= as.Date(d2)) %>%
    mutate(SEMAINE = floor_date(DATE, unit = "week", week_start = 1)) %>%
    group_by(SEMAINE) %>%
    mutate(CA_TOT = sum(.data[[col]], na.rm = TRUE)) %>%
    group_by(SEMAINE, CATEGORIE) %>%
    mutate(CA_CATEGORIE = sum(.data[[col]], na.rm = TRUE)) %>%
    filter(PRODUIT == produit) %>%
    group_by(SEMAINE, CATEGORIE) %>%
    summarise(Quantite = sum(QUANTITE, na.rm = TRUE),
              CA = sum(.data[[col]], na.rm = TRUE),
              PC_ALL = CA / mean(CA_TOT, na.rm = TRUE),
              PC_CATEGORIE = CA / mean(CA_CATEGORIE, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(SEMAINE)
}

# CA en barres, quantité en ligne sur un second axe : les deux ne bougent pas
# ensemble dès qu'une promotion passe, et c'est l'écart qui est informatif.
graph_evolution_produit <- function(evo, produit) {
  if (is.null(evo) || !nrow(evo))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune vente pour ce produit")))
  plot_ly(evo, source = "detail_produit") %>%
    add_bars(x = ~SEMAINE, y = ~CA, name = "CA",
             marker = list(color = COUL_BRUN),
             hovertemplate = ~paste0("Semaine du ", format(SEMAINE, "%d/%m"),
                                     "<br>", format_CA(CA, -1),
                                     "<extra></extra>")) %>%
    add_lines(x = ~SEMAINE, y = ~Quantite, name = "Quantité", yaxis = "y2",
              line = list(color = COUL_AMBRE, width = 2),
              hovertemplate = ~paste0(Quantite, " vendus<extra></extra>")) %>%
    layout(yaxis = list(title = "CA (€)"),
           yaxis2 = list(title = "Quantité", overlaying = "y", side = "right",
                         showgrid = FALSE),
           xaxis = list(title = ""), legend = list(orientation = "h", y = -0.2),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}
