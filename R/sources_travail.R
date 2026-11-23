# Trois sources pour les heures et le coût du travail, et leur réconciliation.
#
#   HOREKO (DB_COUTS_TRAVAIL)  le pointage, seul grain jour × secteur × créneau.
#                              Coût estimé : heures × taux horaire du contrat.
#   COMPTABILITÉ (comptes 62)  le coût réel, mais un total mensuel sans
#                              ventilation ni heures.
#   PAIE / ONSS (DB_ONSS)      coût employeur réel et heures travaillées, par
#                              secteur et par mois ; rien au jour.
#
# Règle : la paie fait autorité sur le total du mois et du secteur ; Horeko
# ne sert qu'à répartir ce total sur les jours et les créneaux, au prorata des
# heures pointées et sans jamais franchir un secteur. On ne redresse PAS par
# la comptabilité : son écart (pécules, provisions, personnel non pointé) n'a
# aucune raison de suivre les heures.
#
# Les valeurs Horeko d'origine sont conservées ; le sous-onglet « Sources »
# affiche les trois côte à côte.

#### Contrat de DB_ONSS ####

# DB_ONSS : une ligne par ANNEE x MOIS x SECTEUR.
#
#   ANNEE, MOIS <int>   la période de paie
#   SECTEUR     <chr>   vocabulaire du dashboard (cf. normalise_secteur)
#   HEURES      <dbl>   heures réellement travaillées dans ce secteur
#   COUT        <dbl>   coût employeur imputé à ce secteur
#   N_FICHES    <int>   nombre de fiches de paie concernées
ONSS_COLONNES <- c("ANNEE", "MOIS", "SECTEUR", "HEURES", "COUT")

# Table optionnelle : le volet doit pouvoir s'en passer.
onss_valide <- function(db) {
  !is.null(db) && is.data.frame(db) &&
    all(ONSS_COLONNES %in% names(db)) && nrow(db) > 0
}

# Lignes de paie sans secteur (absence, solde de congés, fiche sans heures).
# Gardées sous ce libellé pour que le total retombe sur celui de la paie.
SECTEUR_NON_VENTILE <- "Non ventilé"

#### Lecture du fichier de paie ####

# Colonnes d'heures par secteur, nommées une à une : « Heures » et « Heures
# réellement travaillées » ne sont pas des secteurs.
COLONNES_SECTEURS_ONSS <- c(
  "Heures Service"              = "Service",
  "Heures Transfo alimentaire"  = "Transfo alimentaire",
  "Heures Support"              = "Support",
  "Heures Fabrique de boissons" = "Fabrique de boissons"
)

# Colonnes non sectorielles dont on a besoin, et leur nom de travail.
COLONNES_ONSS <- c(
  ANNEE       = "Année",
  MOIS        = "Mois",
  COUT        = "Coût employeur",
  HEURES_PAIE = "Heures",
  HEURES_REEL = "Heures réellement travaillées"
)

# Nombre écrit à l'européenne, quand le classeur rend du texte.
#
#   1. une virgule est présente : décimale ; les points sont des milliers
#      (« 1.121,97 » -> 1121.97) ;
#   2. sinon, des points qui découpent des groupes de trois chiffres sont des
#      milliers (« 2.683 » -> 2683) ;
#   3. sinon le point est la décimale (« 1121.97 »).
#
# La règle 2 tranche une ambiguïté : dans un fichier de paie, un montant à
# trois décimales n'existe pas.
nombre_europeen <- function(x) {
  if (is.numeric(x)) return(x)
  txt <- trimws(as.character(x))
  txt[txt %in% c("", "-", "NA")] <- NA_character_
  # Espaces (insécables compris) et devise d'abord.
  txt <- gsub("[[:space:]  €]", "", txt)

  avec_virgule <- grepl(",", txt, fixed = TRUE)
  txt[avec_virgule] <- gsub(".", "", txt[avec_virgule], fixed = TRUE)

  milliers <- !avec_virgule & !is.na(txt) &
    grepl("^[+-]?[0-9]{1,3}([.][0-9]{3})+$", txt)
  txt[milliers] <- gsub(".", "", txt[milliers], fixed = TRUE)

  txt <- gsub(",", ".", txt, fixed = TRUE)
  suppressWarnings(as.numeric(txt))
}

# Récupère une colonne du fichier de paie par son intitulé, en nombre.
# Renvoie un vecteur de NA si elle manque : le contrôle en aval le dira.
colonne_onss <- function(import, nom) {
  if (!nom %in% names(import)) return(rep(NA_real_, nrow(import)))
  nombre_europeen(import[[nom]])
}

# DB_ONSS à partir du fichier de paie brut. Le coût d'une fiche (un
# travailleur, un mois) est ventilé au prorata de ses heures par secteur ; une
# fiche sans heure sectorielle garde son coût sous SECTEUR_NON_VENTILE.
construit_onss <- function(import) {
  if (is.null(import) || !is.data.frame(import) || !nrow(import)) return(NULL)

  base <- tibble(
    ANNEE       = as.integer(colonne_onss(import, COLONNES_ONSS[["ANNEE"]])),
    MOIS        = as.integer(colonne_onss(import, COLONNES_ONSS[["MOIS"]])),
    COUT        = replace_na(colonne_onss(import, COLONNES_ONSS[["COUT"]]), 0),
    .LIGNE      = seq_len(nrow(import)))

  secteurs <- lapply(names(COLONNES_SECTEURS_ONSS), function(col) {
    tibble(.LIGNE = seq_len(nrow(import)),
           SECTEUR = normalise_secteur(COLONNES_SECTEURS_ONSS[[col]]),
           HEURES  = replace_na(colonne_onss(import, col), 0))
  })
  secteurs <- bind_rows(secteurs) %>% filter(HEURES > 0)

  d <- base %>%
    filter(!is.na(ANNEE), !is.na(MOIS)) %>%
    left_join(secteurs, by = ".LIGNE") %>%
    group_by(.LIGNE) %>%
    mutate(H_FICHE = sum(HEURES, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      SECTEUR = if_else(is.na(SECTEUR) | H_FICHE <= 0,
                        SECTEUR_NON_VENTILE, SECTEUR),
      HEURES  = replace_na(HEURES, 0),
      COUT    = if_else(H_FICHE > 0, COUT * HEURES / H_FICHE, COUT))

  res <- d %>%
    group_by(ANNEE, MOIS, SECTEUR) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE),
              COUT = sum(COUT, na.rm = TRUE),
              N_FICHES = n_distinct(.LIGNE), .groups = "drop") %>%
    filter(HEURES > 0 | COUT != 0) %>%
    arrange(ANNEE, MOIS, SECTEUR)

  if (!nrow(res)) NULL else res
}

# Contrôle de lecture du fichier de paie : si la somme des colonnes
# sectorielles ne retombe pas sur « Heures réellement travaillées », un
# secteur a changé de nom dans le fichier.
controle_onss <- function(import) {
  if (is.null(import) || !is.data.frame(import) || !nrow(import)) return(NULL)

  h_sect <- rowSums(
    vapply(names(COLONNES_SECTEURS_ONSS),
           function(col) replace_na(colonne_onss(import, col), 0),
           numeric(nrow(import))),
    na.rm = TRUE)
  h_reel <- replace_na(colonne_onss(import, COLONNES_ONSS[["HEURES_REEL"]]), 0)

  tibble(
    LIGNES          = nrow(import),
    H_SECTEURS      = sum(h_sect),
    H_REELLES       = sum(h_reel),
    ECART           = sum(h_sect) - sum(h_reel),
    LIGNES_ECART    = sum(abs(h_sect - h_reel) > 0.01),
    COLONNES_VUES   = sum(names(COLONNES_SECTEURS_ONSS) %in% names(import)),
    COLONNES_ATTENDUES = length(COLONNES_SECTEURS_ONSS))
}

# Onglet du classeur de paie ; à défaut, le premier.
SHEET_ONSS <- "PAIE"

# Lecture du classeur de paie, tout en texte puis reconverti par
# nombre_europeen() : indifférent au format d'affichage du Sheet.
lit_onss <- function(ss) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    cli::cli_alert_warning("googlesheets4 absent : fichier de paie ignoré")
    return(NULL)
  }
  lu <- try({
    onglets <- googlesheets4::sheet_names(ss)
    cible <- if (SHEET_ONSS %in% onglets) SHEET_ONSS else onglets[1]
    googlesheets4::read_sheet(ss, sheet = cible, col_types = "c")
  }, silent = TRUE)

  if (inherits(lu, "try-error")) {
    cli::cli_alert_warning("Fichier de paie illisible : {as.character(lu)}")
    return(NULL)
  }
  lu
}

#### Les contrats, et leur taux ####

#' Les contrats pointés qu'aucun taux horaire ne valorise.
#'
#' Le coût Horeko vaut heures × taux du contrat (onglet « Cout » du classeur).
#' Un contrat nouveau, absent de la table des taux, donne un coût NA, qui rend
#' NA tout le groupe du jour. Ce contrôle, appelé à l'import, le signale.
#'
#' @return une ligne par contrat sans taux, ou NULL si tout est valorisé.
contrats_sans_taux <- function(db_heures) {
  if (is.null(db_heures) || !is.data.frame(db_heures) || !nrow(db_heures) ||
      !all(c("CONTRAT", "NB_HEURES", "COUT_HEURE") %in% names(db_heures)))
    return(NULL)
  d <- db_heures %>%
    filter(is.na(COUT_HEURE), !is.na(NB_HEURES), NB_HEURES > 0) %>%
    group_by(CONTRAT) %>%
    summarise(HEURES = sum(NB_HEURES, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(HEURES))
  if (!nrow(d)) NULL else d
}

#### Recalage de DB_COUTS_TRAVAIL ####

# Libellés des trois sources, par confiance décroissante sur les heures.
SOURCES_TRAVAIL <- tibble::tribble(
  ~CLE,       ~LIBELLE,          ~HEURES, ~COUT,  ~COULEUR,
  "paie",     "Paie (ONSS)",     TRUE,    TRUE,   "#732c02",
  "horeko",   "Horeko",          TRUE,    TRUE,   "#d98236",
  "compta",   "Comptabilité",    FALSE,   TRUE,   "#5b7b9b"
)

# DB_COUTS_TRAVAIL recalé sur la paie.
#
# HEURES et COUT_TRAVAIL portent les valeurs retenues ; les valeurs Horeko
# restent sous HEURES_HOREKO et COUT_HOREKO_LIGNE, et SOURCE_HEURES dit
# laquelle a servi. Sans DB_ONSS, la fonction ajoute seulement ces colonnes.
recale_couts_travail <- function(db_couts, db_onss = NULL) {
  if (is.null(db_couts) || !nrow(db_couts)) return(db_couts)

  d <- db_couts
  if (!all(c("ANNEE", "MOIS") %in% names(d)))
    d <- mutate(d, ANNEE = year(DATE), MOIS = month(DATE))

  d <- d %>% mutate(HEURES_HOREKO = HEURES, COUT_HOREKO_LIGNE = COUT_TRAVAIL)

  if (!onss_valide(db_onss))
    return(d %>% mutate(SOURCE_HEURES = "Horeko",
                        HEURES_ONSS_MOIS = NA_real_,
                        COUT_ONSS_MOIS = NA_real_))

  onss <- db_onss %>%
    filter(SECTEUR != SECTEUR_NON_VENTILE) %>%
    group_by(ANNEE, MOIS, SECTEUR) %>%
    summarise(HEURES_ONSS_MOIS = sum(HEURES, na.rm = TRUE),
              COUT_ONSS_MOIS = sum(COUT, na.rm = TRUE), .groups = "drop")

  d %>%
    group_by(ANNEE, MOIS, SECTEUR) %>%
    mutate(H_MOIS_HOREKO = sum(HEURES_HOREKO, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(onss, by = c("ANNEE", "MOIS", "SECTEUR")) %>%
    mutate(
      # La part de la ligne dans son mois-secteur, selon Horeko.
      PART = if_else(H_MOIS_HOREKO > 0, HEURES_HOREKO / H_MOIS_HOREKO, NA_real_),
      recale = !is.na(HEURES_ONSS_MOIS) & !is.na(PART),
      SOURCE_HEURES = if_else(recale, "Paie", "Horeko"),
      HEURES       = if_else(recale, HEURES_ONSS_MOIS * PART, HEURES_HOREKO),
      COUT_TRAVAIL = if_else(recale & !is.na(COUT_ONSS_MOIS),
                             COUT_ONSS_MOIS * PART, COUT_HOREKO_LIGNE),
      TAUX_HORAIRE = if_else(HEURES > 0, COUT_TRAVAIL / HEURES, NA_real_)) %>%
    select(-H_MOIS_HOREKO, -PART, -recale)
}

# Heures de la paie qu'aucune ligne Horeko ne peut porter : un secteur payé
# mais non pointé ce mois-là. Comptées ici pour être affichées.
onss_non_rattache <- function(db_couts, db_onss) {
  if (!onss_valide(db_onss)) return(NULL)

  couvert <- if (is.null(db_couts) || !nrow(db_couts))
    tibble(ANNEE = integer(), MOIS = integer(), SECTEUR = character())
  else db_couts %>%
    mutate(ANNEE = year(DATE), MOIS = month(DATE)) %>%
    filter(HEURES > 0) %>%
    distinct(ANNEE, MOIS, SECTEUR)

  res <- db_onss %>%
    filter(SECTEUR != SECTEUR_NON_VENTILE) %>%
    anti_join(couvert, by = c("ANNEE", "MOIS", "SECTEUR")) %>%
    filter(HEURES > 0 | COUT != 0) %>%
    arrange(ANNEE, MOIS, SECTEUR)

  if (!nrow(res)) NULL else res
}

#### Volet « Sources » ####

# Heures et coût par mois et par source, en format long : la table dont
# vivent tous les rendus du volet. HEURES vaut NA pour la comptabilité.
sources_mensuelles <- function(db_couts, db_onss = NULL) {
  if (is.null(db_couts) || !nrow(db_couts)) return(NULL)

  d <- db_couts %>%
    mutate(ANNEE = year(DATE), MOIS = month(DATE),
           PERIODE = as.Date(sprintf("%04d-%02d-01", ANNEE, MOIS)))

  h_horeko <- if ("HEURES_HOREKO" %in% names(d)) d$HEURES_HOREKO else d$HEURES
  c_horeko <- if ("COUT_HOREKO_LIGNE" %in% names(d)) d$COUT_HOREKO_LIGNE
              else d$COUT_TRAVAIL

  horeko <- d %>%
    mutate(.H = h_horeko, .C = c_horeko) %>%
    group_by(PERIODE) %>%
    summarise(HEURES = sum(.H, na.rm = TRUE),
              COUT = sum(.C, na.rm = TRUE), .groups = "drop") %>%
    mutate(SOURCE = "horeko")

  compta <- if ("COUT_COMPTA" %in% names(d))
    d %>% distinct(PERIODE, COUT_COMPTA) %>%
      filter(!is.na(COUT_COMPTA)) %>%
      transmute(PERIODE, HEURES = NA_real_, COUT = COUT_COMPTA,
                SOURCE = "compta")
  else NULL

  paie <- if (onss_valide(db_onss))
    db_onss %>%
      mutate(PERIODE = as.Date(sprintf("%04d-%02d-01",
                                       as.integer(ANNEE), as.integer(MOIS)))) %>%
      group_by(PERIODE) %>%
      summarise(HEURES = sum(HEURES, na.rm = TRUE),
                COUT = sum(COUT, na.rm = TRUE), .groups = "drop") %>%
      mutate(SOURCE = "paie")
  else NULL

  res <- bind_rows(paie, horeko, compta) %>%
    left_join(SOURCES_TRAVAIL %>% select(SOURCE = CLE, LIBELLE), by = "SOURCE") %>%
    mutate(SOURCE = factor(SOURCE, levels = SOURCES_TRAVAIL$CLE),
           LIBELLE = factor(LIBELLE, levels = SOURCES_TRAVAIL$LIBELLE)) %>%
    arrange(PERIODE, SOURCE)

  if (!nrow(res)) NULL else res
}

# La même chose au grain secteur, sans la comptabilité, qui n'en a pas.
sources_par_secteur <- function(db_couts, db_onss = NULL) {
  if (is.null(db_couts) || !nrow(db_couts)) return(NULL)

  h_horeko <- if ("HEURES_HOREKO" %in% names(db_couts)) "HEURES_HOREKO" else "HEURES"
  c_horeko <- if ("COUT_HOREKO_LIGNE" %in% names(db_couts)) "COUT_HOREKO_LIGNE"
              else "COUT_TRAVAIL"

  horeko <- db_couts %>%
    mutate(ANNEE = year(DATE), MOIS = month(DATE),
           PERIODE = as.Date(sprintf("%04d-%02d-01", ANNEE, MOIS))) %>%
    group_by(PERIODE, SECTEUR) %>%
    summarise(HEURES = sum(.data[[h_horeko]], na.rm = TRUE),
              COUT = sum(.data[[c_horeko]], na.rm = TRUE), .groups = "drop") %>%
    mutate(SOURCE = "horeko")

  paie <- if (onss_valide(db_onss))
    db_onss %>%
      mutate(PERIODE = as.Date(sprintf("%04d-%02d-01",
                                       as.integer(ANNEE), as.integer(MOIS)))) %>%
      group_by(PERIODE, SECTEUR) %>%
      summarise(HEURES = sum(HEURES, na.rm = TRUE),
                COUT = sum(COUT, na.rm = TRUE), .groups = "drop") %>%
      mutate(SOURCE = "paie")
  else NULL

  bind_rows(paie, horeko) %>% arrange(PERIODE, SECTEUR, SOURCE)
}

# Une ligne par mois, une colonne par (source × grandeur), et les écarts en
# euros et en pour cent.
table_sources <- function(src) {
  if (is.null(src) || !nrow(src))
    return(tibble(Info = "Aucune source de coût du travail disponible."))

  large <- src %>%
    select(PERIODE, SOURCE, HEURES, COUT) %>%
    pivot_wider(names_from = SOURCE, values_from = c(HEURES, COUT),
                names_sep = "_")

  col <- function(nom) if (nom %in% names(large)) large[[nom]]
                       else rep(NA_real_, nrow(large))
  h_paie <- col("HEURES_paie");  h_hor <- col("HEURES_horeko")
  c_paie <- col("COUT_paie");    c_hor <- col("COUT_horeko")
  c_cpt  <- col("COUT_compta")

  # trimws : format_CA aligne les largeurs avec des espaces.
  eur <- function(x) ifelse(is.na(x), "—", trimws(format_CA(x, -1)))
  ecart <- function(a, b) ifelse(is.na(a) | is.na(b), "—",
                                 paste0(ifelse(a - b >= 0, "+", ""), eur(a - b)))
  ecart_pct <- function(a, b) ifelse(is.na(a) | is.na(b) | b == 0, "—",
                                     paste0(ifelse(a - b >= 0, "+", ""),
                                            format_pct(100 * (a - b) / abs(b))))

  tibble(
    Mois              = etiquette_periode(large$PERIODE, "mois"),
    `Heures paie`     = ifelse(is.na(h_paie), "—", format(round(h_paie, 1),
                                                          nsmall = 1, trim = TRUE)),
    `Heures Horeko`   = ifelse(is.na(h_hor), "—", format(round(h_hor, 1),
                                                         nsmall = 1, trim = TRUE)),
    `Écart heures`    = ecart_heures(h_paie, h_hor),
    `Coût paie`       = eur(c_paie),
    `Coût Horeko`     = eur(c_hor),
    `Coût compta`     = eur(c_cpt),
    `Paie vs Horeko`  = ecart(c_paie, c_hor),
    `Paie vs compta`  = ecart(c_paie, c_cpt),
    `Paie vs compta %`= ecart_pct(c_paie, c_cpt)
  ) %>% arrange(desc(row_number()))
}

# Écart d'heures, en heures et en pour cent.
ecart_heures <- function(a, b) {
  ifelse(is.na(a) | is.na(b), "—",
         paste0(ifelse(a - b >= 0, "+", ""), round(a - b, 1), " h",
                ifelse(is.na(b) | b == 0, "",
                       paste0(" (", ifelse(a - b >= 0, "+", ""),
                              round(100 * (a - b) / abs(b), 1), " %)"))))
}

##### Rendus #####

# Couleur d'une source, par sa clé.
couleur_source <- function(cle) {
  i <- match(cle, SOURCES_TRAVAIL$CLE)
  ifelse(is.na(i), COUL_NEUTRE, SOURCES_TRAVAIL$COULEUR[i])
}

# Les heures mois par mois, une courbe par source qui en mesure.
graph_sources_heures <- function(src, unite = "mois") {
  mesurent <- SOURCES_TRAVAIL$CLE[SOURCES_TRAVAIL$HEURES]
  d <- if (is.null(src)) NULL else src %>% filter(as.character(SOURCE) %in% mesurent)
  if (is.null(d) || !nrow(d))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune source d'heures disponible")))

  lab <- etiquette_periode(sort(unique(d$PERIODE)), unite)
  ordre <- factor(etiquette_periode(d$PERIODE, unite), levels = lab)

  p <- plot_ly()
  for (cle in mesurent) {
    s <- d %>% filter(as.character(SOURCE) == cle)
    if (!nrow(s)) next
    p <- add_trace(
      p, x = factor(etiquette_periode(s$PERIODE, unite), levels = lab),
      y = s$HEURES, type = "scatter", mode = "lines+markers",
      name = as.character(s$LIBELLE[1]),
      line = list(color = couleur_source(cle), width = 2.5),
      marker = list(color = couleur_source(cle), size = 7),
      hovertemplate = paste0("<b>", etiquette_periode(s$PERIODE, unite), "</b><br>",
                             as.character(s$LIBELLE[1]), " : ",
                             round(s$HEURES), " h<extra></extra>"))
  }
  p %>% layout(
    xaxis = list(title = "", tickangle = -35),
    yaxis = list(title = "Heures", rangemode = "tozero"),
    legend = list(orientation = "h", y = -0.3), margin = list(b = 90),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Le coût mois par mois, une courbe par source.
graph_sources_cout <- function(src, unite = "mois") {
  if (is.null(src) || !nrow(src))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune source de coût disponible")))

  lab <- etiquette_periode(sort(unique(src$PERIODE)), unite)
  p <- plot_ly()
  for (cle in SOURCES_TRAVAIL$CLE) {
    s <- src %>% filter(as.character(SOURCE) == cle, !is.na(COUT))
    if (!nrow(s)) next
    p <- add_trace(
      p, x = factor(etiquette_periode(s$PERIODE, unite), levels = lab),
      y = s$COUT, type = "scatter", mode = "lines+markers",
      name = as.character(s$LIBELLE[1]),
      line = list(color = couleur_source(cle), width = 2.5,
                  dash = if (cle == "horeko") "dot" else "solid"),
      marker = list(color = couleur_source(cle), size = 7),
      hovertemplate = paste0("<b>", etiquette_periode(s$PERIODE, unite), "</b><br>",
                             as.character(s$LIBELLE[1]), " : ",
                             format_CA(s$COUT, -1), "<extra></extra>"))
  }
  p %>% layout(
    xaxis = list(title = "", tickangle = -35),
    yaxis = list(title = "€", rangemode = "tozero"),
    legend = list(orientation = "h", y = -0.3), margin = list(b = 90),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# L'écart de coût entre la paie et les deux autres, en part de la paie. Un
# écart constant est une différence de périmètre ; un écart qui dérive, un
# problème de données.
graph_ecart_sources <- function(src, unite = "mois") {
  if (is.null(src) || !nrow(src))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucun écart à afficher")))

  ref <- src %>% filter(as.character(SOURCE) == "paie") %>%
    select(PERIODE, COUT_PAIE = COUT)
  if (!nrow(ref))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de paie : aucun écart à mesurer")))

  d <- src %>%
    filter(as.character(SOURCE) != "paie", !is.na(COUT)) %>%
    inner_join(ref, by = "PERIODE") %>%
    filter(COUT_PAIE != 0) %>%
    mutate(ECART_PCT = 100 * (COUT - COUT_PAIE) / abs(COUT_PAIE))
  if (!nrow(d))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucun écart à afficher")))

  lab <- etiquette_periode(sort(unique(d$PERIODE)), unite)
  p <- plot_ly()
  for (cle in setdiff(SOURCES_TRAVAIL$CLE, "paie")) {
    s <- d %>% filter(as.character(SOURCE) == cle)
    if (!nrow(s)) next
    p <- add_bars(
      p, x = factor(etiquette_periode(s$PERIODE, unite), levels = lab),
      y = s$ECART_PCT, name = as.character(s$LIBELLE[1]),
      marker = list(color = couleur_source(cle)),
      hovertemplate = paste0(
        "<b>", etiquette_periode(s$PERIODE, unite), "</b><br>",
        as.character(s$LIBELLE[1]), " : ", format_CA(s$COUT, -1),
        "<br>paie : ", format_CA(s$COUT_PAIE, -1),
        "<br>écart ", ifelse(s$ECART_PCT >= 0, "+", ""),
        round(s$ECART_PCT, 1), " %<extra></extra>"))
  }
  p %>% layout(
    barmode = "group",
    xaxis = list(title = "", tickangle = -35),
    yaxis = list(title = "Écart vs paie (%)", zeroline = TRUE,
                 zerolinecolor = "#8d7b68"),
    legend = list(orientation = "h", y = -0.3), margin = list(b = 90),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Les heures par secteur, paie contre Horeko : un même total peut cacher deux
# ventilations opposées.
graph_sources_secteurs <- function(sect) {
  if (is.null(sect) || !nrow(sect))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune ventilation par secteur")))

  d <- sect %>%
    group_by(SECTEUR, SOURCE) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE), .groups = "drop")
  ordre <- d %>% group_by(SECTEUR) %>%
    summarise(T = sum(HEURES), .groups = "drop") %>% arrange(desc(T))
  niveaux <- ordre$SECTEUR

  p <- plot_ly()
  for (cle in c("paie", "horeko")) {
    s <- d %>% filter(SOURCE == cle)
    if (!nrow(s)) next
    lib <- SOURCES_TRAVAIL$LIBELLE[SOURCES_TRAVAIL$CLE == cle]
    p <- add_bars(
      p, x = factor(s$SECTEUR, levels = niveaux), y = s$HEURES, name = lib,
      marker = list(color = couleur_source(cle)),
      hovertemplate = paste0("<b>", s$SECTEUR, "</b><br>", lib, " : ",
                             round(s$HEURES), " h<extra></extra>"))
  }
  p %>% layout(
    barmode = "group",
    xaxis = list(title = "", tickangle = -20),
    yaxis = list(title = "Heures", rangemode = "tozero"),
    legend = list(orientation = "h", y = -0.25), margin = list(b = 80),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# Tuiles de synthèse sur la fenêtre : les totaux par source, et les écarts.
kpi_sources <- function(src) {
  if (is.null(src) || !nrow(src))
    return(div(class = "text-muted small p-2",
               "Aucune source de coût du travail disponible."))

  tot <- src %>% group_by(SOURCE) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE),
              COUT = sum(COUT, na.rm = TRUE),
              N = sum(!is.na(COUT)), .groups = "drop")
  val <- function(cle, champ) {
    x <- tot[[champ]][as.character(tot$SOURCE) == cle]
    if (!length(x)) NA_real_ else x[1]
  }
  h_paie <- val("paie", "HEURES"); h_hor <- val("horeko", "HEURES")
  c_paie <- val("paie", "COUT");   c_hor <- val("horeko", "COUT")
  c_cpt  <- val("compta", "COUT")

  tuile_ecart <- function(a, b, lib, icone, unite = "eur") {
    if (is.na(a) || is.na(b))
      return(kpi_tile("—", lib, COUL_NEUTRE, icone,
                      sous_titre = "source absente sur la fenêtre"))
    e <- a - b
    pct <- if (b == 0) NA_real_ else 100 * e / abs(b)
    kpi_tile(
      if (unite == "eur") paste0(if (e >= 0) "+" else "", format_CA(e, -1))
      else paste0(if (e >= 0) "+" else "", round(e), " h"),
      lib,
      # Au-delà de 10 %, ce n'est plus du périmètre.
      if (is.na(pct)) COUL_NEUTRE
      else if (abs(pct) <= 10) COUL_VERT
      else if (abs(pct) <= 25) COUL_AMBRE else COUL_ROUGE,
      icone,
      sous_titre = if (is.na(pct)) NULL
                   else paste0(if (pct >= 0) "+" else "", format_pct(pct),
                               " de la référence"))
  }

  div(
    class = "kpi-grid",
    kpi_tile(if (is.na(c_paie)) "—" else format_CA(c_paie, -1),
             "Coût du travail — paie", COUL_BRUN, "file-invoice-dollar",
             sous_titre = if (is.na(h_paie)) NULL
                          else paste0(round(h_paie), " h réellement travaillées")),
    tuile_ecart(h_hor, h_paie, "Heures Horeko vs paie", "clock", "heures"),
    tuile_ecart(c_hor, c_paie, "Coût Horeko vs paie", "calculator"),
    tuile_ecart(c_cpt, c_paie, "Coût compta vs paie", "book"),
    kpi_tile(
      if (is.na(h_paie) || h_paie <= 0 || is.na(c_paie)) "—"
      else format_CA(c_paie / h_paie, 2),
      "Coût employeur par heure", COUL_TRAVAIL, "euro-sign",
      sous_titre = "d'après la paie, la seule mesure réelle")
  )
}

# Bandeau de contrôle de la lecture du fichier de paie : colonne de secteur
# introuvable, ou heures sectorielles qui ne retombent pas sur le total.
alerte_onss <- function(ctrl) {
  if (is.null(ctrl) || !nrow(ctrl)) return(NULL)

  if (ctrl$COLONNES_VUES < ctrl$COLONNES_ATTENDUES)
    return(bandeau_alerte(
      TRUE,
      paste0(ctrl$COLONNES_VUES, " colonne(s) de secteur trouvée(s) sur ",
             ctrl$COLONNES_ATTENDUES, " attendues dans le fichier de paie. ",
             "Un secteur a changé d'intitulé : ses heures n'entrent dans aucun ",
             "total. Les intitulés attendus sont ",
             paste0("« ", names(COLONNES_SECTEURS_ONSS), " »", collapse = ", "),
             "."),
      titre = "Colonne de secteur introuvable", couleur = COUL_ROUGE))

  if (abs(ctrl$ECART) > 1)
    return(bandeau_alerte(
      TRUE,
      paste0("La somme des heures par secteur (", round(ctrl$H_SECTEURS),
             " h) ne retombe pas sur les heures réellement travaillées (",
             round(ctrl$H_REELLES), " h) : ", round(ctrl$ECART),
             " h d'écart sur ", ctrl$LIGNES_ECART, " fiche(s) de paie. ",
             "Des heures sont ventilées ailleurs que dans les quatre secteurs, ",
             "ou pas ventilées du tout."),
      titre = "Ventilation incomplète", couleur = COUL_AMBRE))

  NULL
}

# Bandeau des heures de paie qu'aucun jour de pointage ne porte.
alerte_non_rattache <- function(nr) {
  if (is.null(nr) || !nrow(nr)) return(NULL)
  bandeau_alerte(
    TRUE,
    paste0(nrow(nr), " couple(s) mois x secteur existent dans la paie sans ",
           "aucune heure pointée dans Horeko : ", round(sum(nr$HEURES)),
           " h et ", format_CA(sum(nr$COUT), -1), " ne peuvent être rattachés ",
           "à aucun jour. Ils comptent dans les totaux de ce volet, mais pas ",
           "dans le détail journalier du volet Travail."),
    titre = "Heures de paie sans pointage", couleur = COUL_AMBRE,
    icone = "circle-info")
}

# Le détail des lignes non rattachées, pour aller les chercher.
table_non_rattache <- function(nr) {
  if (is.null(nr) || !nrow(nr))
    return(tibble(Contrôle = "Toutes les heures de paie ont un pointage en face."))
  nr %>% transmute(
    Mois = etiquette_periode(as.Date(sprintf("%04d-%02d-01",
                                             as.integer(ANNEE),
                                             as.integer(MOIS))), "mois"),
    Secteur = SECTEUR,
    Heures = round(HEURES, 1),
    `Coût` = format_CA(COUT, -1),
    `Nb fiches` = N_FICHES)
}
