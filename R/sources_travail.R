# Trois sources pour les heures et le coût du travail, et comment les réconcilier.
#
# ---------------------------------------------------------------------------
# Ce que chacune sait, et ce qu'elle ne sait pas
# ---------------------------------------------------------------------------
#
#   HOREKO (DB_COUTS_TRAVAIL)
#     Le pointage. Seule source au grain JOUR x SECTEUR x CRÉNEAU — donc la
#     seule qui sache dire « samedi soir ». Son coût est une ESTIMATION : un
#     taux horaire par type de contrat, multiplié par les heures. Il ignore
#     les pécules, les provisions, les charges patronales réelles.
#
#   COMPTABILITÉ (comptes 62)
#     Le coût VRAI, au centime, mais un total mensuel sans aucune ventilation.
#     Elle ne connaît pas les heures du tout.
#
#   PAIE / ONSS (DB_ONSS) — la nouvelle
#     Le coût employeur réel ET les heures réellement travaillées, ventilés
#     par secteur, au grain MOIS x TRAVAILLEUR. Elle ne descend pas au jour,
#     donc elle ne sait rien des créneaux.
#
# Aucune ne domine les autres sur tous les axes, d'où la réconciliation plutôt
# qu'un choix. Mais sur les deux axes qui comptent — combien d'heures, combien
# d'euros — la paie est la meilleure des trois : elle mesure, là où Horeko
# estime et où la comptabilité ne ventile pas.
#
# ---------------------------------------------------------------------------
# La règle de réconciliation
# ---------------------------------------------------------------------------
# La paie fait autorité sur le TOTAL du mois et du secteur. Horeko ne sert
# plus qu'à RÉPARTIR ce total sur les jours et les créneaux.
#
# C'est bien une règle de trois, et il faut dire pourquoi elle est légitime ici
# alors qu'elle avait été retirée de l'ancien redressement par la comptabilité
# (cf. import.R). La différence est nette :
#
#   - redresser le coût Horeko par le total COMPTABLE répartissait un écart qui
#     n'a aucune raison de suivre les heures : pécules, provisions et personnel
#     non pointé ne se rangent pas au prorata des heures pointées. On déplaçait
#     du coût d'un secteur vers un autre sans fondement ;
#
#   - recaler les heures d'un secteur sur le total de la PAIE répartit l'écart
#     entre DEUX MESURES DE LA MÊME GRANDEUR — les heures faites dans ce
#     secteur, ce mois-là. On ne sait pas quel jour l'écart est né ; le prorata
#     des heures pointées est alors la seule hypothèse neutre, et elle ne
#     traverse jamais la frontière d'un secteur.
#
# Les valeurs Horeko d'origine sont conservées colonne à colonne : rien n'est
# écrasé, et le sous-onglet « Sources » affiche les trois côte à côte.

#### Contrat de DB_ONSS ####

# DB_ONSS : une ligne par ANNEE x MOIS x SECTEUR.
#
#   ANNEE, MOIS <int>   la période de paie
#   SECTEUR     <chr>   vocabulaire du dashboard (cf. normalise_secteur)
#   HEURES      <dbl>   heures réellement travaillées dans ce secteur
#   COUT        <dbl>   coût employeur imputé à ce secteur
#   N_FICHES    <int>   nombre de fiches de paie concernées
ONSS_COLONNES <- c("ANNEE", "MOIS", "SECTEUR", "HEURES", "COUT")

# Table optionnelle : tant que l'import ne la produit pas, le volet doit le
# dire plutôt que planter.
onss_valide <- function(db) {
  !is.null(db) && is.data.frame(db) &&
    all(ONSS_COLONNES %in% names(db)) && nrow(db) > 0
}

# Secteur des lignes de paie qu'on ne peut rattacher à aucun secteur : un mois
# d'absence, un solde de congés, une fiche sans ventilation d'heures.
#
# On les GARDE, sous ce libellé, plutôt que de les jeter : sans elles le total
# de DB_ONSS ne retomberait pas sur le total de la paie, et le sous-onglet
# Sources comparerait un sous-ensemble à un tout — l'erreur exacte qu'il est
# censé rendre visible.
SECTEUR_NON_VENTILE <- "Non ventilé"

#### Lecture du fichier de paie ####

# Colonnes d'heures par secteur, et le libellé de secteur qu'elles portent.
#
# Nommées une à une plutôt que détectées par un préfixe : « Heures » et
# « Heures réellement travaillées » commencent aussi par « Heures » et ne sont
# pas des secteurs. Un startsWith() les aurait embarquées.
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

# Nombre écrit à l'européenne, quand la lecture du classeur rend du texte.
#
# googlesheets4 rend des nombres quand les cellules en portent, et du texte dès
# qu'une colonne mélange les types. Il faut donc savoir lire les deux, et le
# piège est le séparateur :
#
#   « 1.121,97 »  point = milliers, virgule = décimale  -> 1121.97
#   « 1121.97 »   point = décimale                       -> 1121.97
#
# Retirer les points sans condition casserait le second cas (112197). Trois
# règles, dans cet ordre :
#
#   1. une VIRGULE est présente -> elle est la décimale, les points sont des
#      milliers. « 1.121,97 » -> 1121.97. C'est le cas courant de ce fichier,
#      où tous les montants portent leurs deux décimales.
#
#   2. pas de virgule, mais les points découpent des GROUPES DE TROIS chiffres
#      -> ce sont des séparateurs de milliers. « 2.683 » -> 2683,
#      « 1.234.567 » -> 1234567.
#
#   3. sinon le point est la décimale. « 1121.97 » -> 1121.97, « 77.00 » -> 77.
#
# La règle 2 tranche une vraie ambiguïté : « 2.683 » peut s'écrire pour 2683
# (milliers à l'européenne) comme pour 2,683 (décimale à l'anglaise). Dans un
# fichier de paie, un montant à trois décimales n'existe pas et un millier est
# partout : on choisit donc 2683. L'ambiguïté est irréductible — aucune
# heuristique ne la lèvera — mais elle ne se présente que si le classeur cesse
# d'afficher ses décimales, ce que le contrôle de lecture verrait par ailleurs.
nombre_europeen <- function(x) {
  if (is.numeric(x)) return(x)
  txt <- trimws(as.character(x))
  txt[txt %in% c("", "-", "NA")] <- NA_character_
  # Espaces de milliers (insécables compris) et symboles de devise d'abord :
  # ils brouilleraient la reconnaissance des groupes de trois.
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

# DB_ONSS à partir du fichier de paie brut.
#
# Le coût employeur est porté par la FICHE (un travailleur, un mois) et non par
# le secteur. On le ventile au prorata des heures que cette fiche a faites dans
# chaque secteur : c'est le même travailleur, donc le même coût horaire, donc
# le prorata est exact et non une approximation.
#
# Une fiche sans aucune heure sectorielle garde tout son coût, sous
# SECTEUR_NON_VENTILE : le total du mois reste celui de la paie.
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
      # Prorata du coût de la fiche sur ses secteurs. Une fiche sans heures
      # ventilables porte son coût en entier sur la ligne « Non ventilé ».
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

# Contrôle de lecture du fichier de paie.
#
# À regarder après chaque import : si la somme des colonnes sectorielles ne
# retombe pas sur « Heures réellement travaillées », c'est qu'un secteur a
# changé de nom dans le fichier et que ses heures sont passées à la trappe.
# Un écart silencieux ici se lirait plus tard comme une baisse d'activité.
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

# Nom de l'onglet du classeur de paie. Nommé plutôt que « le premier onglet » :
# un classeur dont on lit le premier onglet finit un jour par lire autre chose.
# La lecture se rabat sur le premier onglet si celui-ci est absent, parce qu'un
# classeur fraîchement partagé porte souvent encore « Feuille 1 ».
SHEET_ONSS <- "PAIE"

# Lecture du classeur de paie.
#
# Tout est lu en TEXTE puis reconverti par nombre_europeen() : c'est le seul
# moyen d'être indifférent au format d'affichage choisi côté Google. Une
# colonne de montants mélangeant nombres et cellules vides revient sinon en
# liste, et le premier calcul tombe.
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

#### Recalage de DB_COUTS_TRAVAIL ####

# Libellés des trois sources, dans l'ordre de confiance décroissante sur les
# heures. Un registre plutôt que des chaînes recopiées : les couleurs, les
# légendes et les colonnes de tableau en dépendent toutes.
SOURCES_TRAVAIL <- tibble::tribble(
  ~CLE,       ~LIBELLE,          ~HEURES, ~COUT,  ~COULEUR,
  "paie",     "Paie (ONSS)",     TRUE,    TRUE,   "#732c02",
  "horeko",   "Horeko",          TRUE,    TRUE,   "#d98236",
  "compta",   "Comptabilité",    FALSE,   TRUE,   "#5b7b9b"
)

# DB_COUTS_TRAVAIL recalé sur la paie.
#
# En sortie, HEURES et COUT_TRAVAIL portent les valeurs RETENUES — celles que
# tout le dashboard consomme. Les valeurs Horeko d'origine restent disponibles
# sous HEURES_HOREKO et COUT_HOREKO_LIGNE, et SOURCE_HEURES dit, ligne à ligne,
# laquelle a servi.
#
# Sans DB_ONSS, la fonction est un passe-plat qui ajoute seulement ces colonnes :
# le volet Sources doit pouvoir se dessiner avant que la paie n'arrive.
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
      # La part de la ligne dans son mois-secteur : c'est elle qui porte la
      # répartition, et elle vient d'Horeko — la seule source qui descende au
      # jour et au créneau.
      PART = if_else(H_MOIS_HOREKO > 0, HEURES_HOREKO / H_MOIS_HOREKO, NA_real_),
      recale = !is.na(HEURES_ONSS_MOIS) & !is.na(PART),
      SOURCE_HEURES = if_else(recale, "Paie", "Horeko"),
      HEURES       = if_else(recale, HEURES_ONSS_MOIS * PART, HEURES_HOREKO),
      COUT_TRAVAIL = if_else(recale & !is.na(COUT_ONSS_MOIS),
                             COUT_ONSS_MOIS * PART, COUT_HOREKO_LIGNE),
      TAUX_HORAIRE = if_else(HEURES > 0, COUT_TRAVAIL / HEURES, NA_real_)) %>%
    select(-H_MOIS_HOREKO, -PART, -recale)
}

# Heures de la paie qu'aucune ligne Horeko ne peut porter.
#
# Un secteur présent dans la paie mais absent du pointage ce mois-là n'a aucun
# jour auquel se rattacher : ses heures sont perdues pour le grain journalier,
# et le recalage ne peut rien y faire. Plutôt que de les laisser disparaître en
# silence, on les compte ici — le volet Sources les affiche, et c'est ce qui
# transforme une perte invisible en écart lisible.
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

# Heures et coût par mois et par source, en format long.
#
# C'est la table unique dont vivent tous les rendus du volet. Une colonne par
# source aurait imposé de la remodeler à chaque graphe ; en long, la source est
# une variable comme une autre, et ajouter une quatrième source un jour ne
# touchera que SOURCES_TRAVAIL.
#
# HEURES vaut NA pour la comptabilité, qui n'en connaît aucune. NA et non zéro :
# « la compta ne mesure pas les heures » et « la compta mesure zéro heure » ne
# se lisent pas pareil, et un zéro tracerait une courbe au ras de l'axe.
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

# La même chose au grain secteur, pour les deux sources qui ventilent.
#
# La comptabilité en est absente par construction : elle n'a pas de secteur.
# L'afficher avec un secteur « Total » la ferait figurer au même rang que les
# autres, ce qui est exactement le malentendu que ce volet doit dissiper.
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

# Une ligne par mois, une colonne par (source x grandeur), plus les écarts.
#
# C'est LA table que le volet doit produire : « par mois, le nombre d'heures et
# le coût selon chaque source ». Les écarts sont donnés en euros ET en pour
# cent, parce qu'un écart de 3 000 € ne se juge pas sans savoir s'il porte sur
# 20 000 € ou sur 200 000 €.
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

  # trimws : format_CA aligne les largeurs sur le plus long élément du vecteur,
  # ce qui produit « +  270€ » à côté de « +2.070€ ». Utile dans une console,
  # parasite dans une cellule de tableau.
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

# Écart d'heures, en heures et en pour cent : la même exigence que pour les
# euros, dans l'unité qui convient.
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
#
# Des courbes et non des barres groupées : on cherche à voir si les deux
# sources RACONTENT LA MÊME HISTOIRE dans le temps. Deux courbes qui se suivent
# se lisent d'un coup d'œil ; vingt-quatre barres côte à côte, non.
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

# Le coût mois par mois, une courbe par source. Les trois y figurent : c'est le
# seul endroit du dashboard où la comptabilité, Horeko et la paie se comparent
# sur la même grandeur et le même axe.
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

# L'écart de coût entre la paie et les deux autres, en part de la paie.
#
# Le graphe qui compte vraiment : un écart CONSTANT est une différence de
# périmètre, et on peut vivre avec. Un écart qui DÉRIVE est un problème de
# données. Les niveaux absolus des deux courbes précédentes ne permettent pas
# de faire cette distinction à l'œil ; le rapporter à la paie, oui.
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

# Les heures par secteur, paie contre Horeko, sur toute la fenêtre.
#
# Le seul écran qui montre où les deux sources divergent VRAIMENT : un total
# mensuel identique peut cacher deux ventilations opposées, et c'est
# précisément la ventilation qu'on est venu chercher dans la paie.
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
      # Un écart n'est ni bon ni mauvais : c'est sa TAILLE qui alerte. Au-delà
      # de 10 % on ne parle plus de périmètre, on parle de données.
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

# Bandeau de contrôle de la lecture du fichier de paie.
#
# Il ne se déclenche que sur un problème STRUCTUREL — une colonne de secteur
# introuvable, ou des heures sectorielles qui ne retombent pas sur les heures
# réellement travaillées. Les deux se produisent quand le fichier change de
# nom de colonne, et les deux font disparaître des heures sans erreur.
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
