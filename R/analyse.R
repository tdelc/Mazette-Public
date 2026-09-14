# Volet « Analyse » de la comptabilité : le POURQUOI des deux autres.
#
# ---------------------------------------------------------------------------
# Ce que ce volet ajoute, et pourquoi il ne fait pas doublon
# ---------------------------------------------------------------------------
# Les deux sous-onglets existants répondent à « combien ? » :
#
#   - Exploitation      : le cheminement du CA à la marge, sur UNE période ;
#   - Comptabilité gén. : le compte de résultat, compte par compte.
#
# Aucun des deux ne répond à « pourquoi a-t-on 8 000 € de marge en moins
# qu'au trimestre précédent, et faut-il s'en inquiéter ? » C'est le seul objet
# de ce volet. Il ne recalcule rien : il part de postes_exploitation() et de
# classe_comptes(), exactement comme ses deux voisins, et se contente de les
# CONFRONTER — à une période de référence, à leur propre histoire, à la même
# période des années précédentes.
#
# Une règle traverse tout le fichier : ne jamais afficher un écart sans dire
# à quoi il est comparé. Un « −12 % » sans référence explicite est une
# statistique, pas une information.

##### Indicateurs suivis #####

# Les grandeurs que la tendance et la saisonnalité savent tracer.
#
# Un registre plutôt que six `if` recopiés : ajouter un indicateur, c'est
# ajouter une ligne. SENS dit dans quel sens un écart est une BONNE nouvelle
# (+1 produit, -1 charge), et sert au repérage comme au choix des couleurs.
# UNITE distingue les euros des points de pourcentage, ce qui décide du
# formatage et du libellé d'axe.
INDICATEURS_ANALYSE <- tibble::tribble(
  ~CLE,            ~LIBELLE,                      ~COLONNE,       ~UNITE, ~SENS,
  "ca",            "Chiffre d'affaires",          "CA",           "eur",      1,
  "marge",         "Marge avant amortissements",  "MARGE_AA",     "eur",      1,
  "matieres",      "Matières premières",          "MATIERES",     "eur",     -1,
  "remunerations", "Rémunérations",               "REMUNERATION", "eur",     -1,
  "generaux",      "Frais généraux",              "GENERAUX",     "eur",     -1,
  "pct_matieres",  "Matières en % du CA",         "PCT_MATIERES", "pct",     -1,
  "pct_travail",   "Rémunérations en % du CA",    "PCT_TRAVAIL",  "pct",     -1,
  "pct_generaux",  "Frais généraux en % du CA",   "PCT_GENERAUX", "pct",     -1,
  "pct_marge",     "Marge en % du CA",            "PCT_MARGE_AA", "pct",      1
)

# Ligne du registre correspondant à une clé, avec repli sur la marge : un
# select vidé par un changement de granularité ne doit pas faire tomber le volet.
indicateur_analyse <- function(cle) {
  i <- match(cle %||% "", INDICATEURS_ANALYSE$CLE)
  if (is.na(i)) i <- match("marge", INDICATEURS_ANALYSE$CLE)
  as.list(INDICATEURS_ANALYSE[i, ])
}

# Formatage d'une valeur selon l'unité de son indicateur.
format_indicateur <- function(x, unite) {
  if (identical(unite, "pct")) format_pct(x) else format_CA(x, -1)
}

##### Choix de la période de référence #####

# Trois façons de se comparer, et elles ne disent pas la même chose :
#
#   precedente  la période d'avant           -> « est-ce que ça bouge ? »
#   an_dernier  la même période, un an avant -> « est-ce que ça progresse ? »
#   habituelle  la médiane des N précédentes -> « est-ce que c'est normal ? »
#
# La troisième est la plus robuste pour un diagnostic : une seule période de
# référence peut elle-même être anormale, et on comparerait alors deux
# accidents. La médiane des précédentes ne bouge pas pour un mois atypique.
MODES_REFERENCE <- c("Période précédente"       = "precedente",
                     "Même période, un an plus tôt" = "an_dernier",
                     "Médiane des périodes précédentes" = "habituelle")

# Décale une période d'un an en arrière.
#
# `%m-% months(12)` et non `- 365` : une période est toujours identifiée par
# son premier jour (1er du mois, du trimestre ou de l'année), et le décalage
# doit retomber EXACTEMENT dessus. Retirer 365 jours manque la cible dès qu'une
# année bissextile traîne dans l'intervalle, et le join sur PERIODE rend alors
# zéro ligne — une référence qui disparaît sans erreur, le pire des deux mondes.
# Le décalage est le même aux trois granularités, d'où l'absence de switch.
periode_an_avant <- function(p, unite = "mois") {
  as.Date(p) %m-% months(12)
}

# La ligne de référence, sous la forme d'une ligne de `serie` (mêmes colonnes),
# plus un libellé qui dit ce qu'on regarde.
#
# Renvoie NULL quand la référence n'existe pas — pas une ligne de zéros : un
# écart contre zéro serait un écart de 100 %, ce qui est faux et alarmant.
reference_analyse <- function(serie, periode, mode = "precedente",
                              n_habituelle = 6, unite = "mois") {
  if (is.null(serie) || !nrow(serie)) return(NULL)
  periode <- as.Date(periode)
  passe <- serie %>% filter(PERIODE < periode) %>% arrange(PERIODE)

  if (identical(mode, "an_dernier")) {
    cible <- periode_an_avant(periode, unite)
    l <- serie %>% filter(PERIODE == cible)
    if (!nrow(l)) return(NULL)
    return(list(ligne = l, libelle = etiquette_periode(cible, unite),
                mode = mode))
  }

  if (identical(mode, "habituelle")) {
    # Au moins trois périodes : une médiane sur deux valeurs n'est qu'une
    # moyenne, et sur une seule c'est la comparaison « précédente » déguisée.
    ref <- tail(passe, n_habituelle)
    if (nrow(ref) < 3) return(NULL)
    # Médiane colonne par colonne. Les pourcentages sont RECALCULÉS à partir
    # des médianes d'euros et non médianés directement : la médiane d'un
    # rapport n'est pas le rapport des médianes, et afficher les deux côte à
    # côte laisserait un tableau qui ne s'additionne pas.
    med <- ref %>%
      summarise(across(c(CA, AUTRES, PRODUITS, MATIERES, REMUNERATION,
                         GENERAUX, AMORTISSEMENT, FINANCIER, CHARGES,
                         MARGE, MARGE_AA),
                       ~stats::median(., na.rm = TRUE))) %>%
      mutate(PERIODE = max(ref$PERIODE), N_MOIS = NA_integer_,
             PCT_MATIERES = ratio_pct(MATIERES, CA),
             PCT_TRAVAIL  = ratio_pct(REMUNERATION, CA),
             PCT_PRIME    = ratio_pct(MATIERES + REMUNERATION, CA),
             PCT_GENERAUX = ratio_pct(GENERAUX, CA),
             PCT_MARGE    = ratio_pct(MARGE, CA),
             PCT_MARGE_AA = ratio_pct(MARGE_AA, CA))
    return(list(ligne = med,
                libelle = paste0("médiane des ", nrow(ref), " périodes précédentes"),
                mode = mode))
  }

  if (!nrow(passe)) return(NULL)
  l <- tail(passe, 1)
  list(ligne = l, libelle = etiquette_periode(l$PERIODE, unite), mode = "precedente")
}

##### Le pont de marge #####

# Décomposition de la VARIATION de marge entre deux périodes.
#
# ---------------------------------------------------------------------------
# Pourquoi un effet volume séparé
# ---------------------------------------------------------------------------
# La décomposition naïve — Δmarge = ΔCA − Δmatières − Δrémunérations … — est
# exacte mais ne dit rien d'utile : quand le CA baisse de 20 %, TOUTES les
# charges baissent, et le pont affiche six barres qui pointent dans le même
# sens sans qu'on sache si la gestion a dérivé.
#
# On sépare donc ce qui vient du VOLUME de ce qui vient des TAUX. En écrivant
# la marge comme un taux appliqué au CA :
#
#   marge = CA x s   avec   s = 1 + a - m - r - g + f
#                           (a autres produits, m matières, r rémunérations,
#                            g frais généraux, f financier, tous en part du CA)
#
# on obtient l'identité EXACTE :
#
#   Δmarge = ΔCA x s0            <- effet volume : le CA a bougé, les taux non
#          + CA1 x Δa            <- les autres produits ont changé de poids
#          - CA1 x Δm            <- les matières ont dérivé (en % du CA)
#          - CA1 x Δr
#          - CA1 x Δg
#          + CA1 x Δf
#
# Chaque terme est en euros et se lit seul : « les matières ont coûté 3 200 €
# de marge de plus que si leur taux était resté celui de la référence ». La
# somme des six redonne exactement la variation, ce qu'un test vérifie.
#
# Quand le CA de référence est nul, les taux n'existent pas : on retombe sur
# la décomposition brute, qui reste exacte, et VOLUME_SEPARE le dit.
pont_marge <- function(actuel, reference) {
  if (is.null(actuel) || !nrow(actuel) || is.null(reference) || !nrow(reference))
    return(NULL)
  a <- as.list(actuel[1, ]); r <- as.list(reference[1, ])

  if (is.na(r$CA) || r$CA <= 0) {
    postes <- c("Chiffre d'affaires", "Autres produits", "Matières premières",
                "Rémunérations", "Frais généraux", "Résultat financier")
    effets <- c(a$CA - r$CA, a$AUTRES - r$AUTRES,
                -(a$MATIERES - r$MATIERES), -(a$REMUNERATION - r$REMUNERATION),
                -(a$GENERAUX - r$GENERAUX), a$FINANCIER - r$FINANCIER)
    return(tibble(POSTE = postes, EFFET = effets,
                  DEPART = r$MARGE_AA, ARRIVEE = a$MARGE_AA,
                  VOLUME_SEPARE = FALSE))
  }

  taux <- function(x, ca) x / ca
  s0 <- 1 + taux(r$AUTRES, r$CA) - taux(r$MATIERES, r$CA) -
        taux(r$REMUNERATION, r$CA) - taux(r$GENERAUX, r$CA) +
        taux(r$FINANCIER, r$CA)

  d_taux <- function(champ)
    taux(a[[champ]], a$CA) - taux(r[[champ]], r$CA)

  postes <- c("Effet volume (CA)", "Autres produits", "Matières premières",
              "Rémunérations", "Frais généraux", "Résultat financier")
  effets <- c(
    (a$CA - r$CA) * s0,
     a$CA * d_taux("AUTRES"),
    -a$CA * d_taux("MATIERES"),
    -a$CA * d_taux("REMUNERATION"),
    -a$CA * d_taux("GENERAUX"),
     a$CA * d_taux("FINANCIER"))

  tibble(POSTE = postes, EFFET = effets,
         DEPART = r$MARGE_AA, ARRIVEE = a$MARGE_AA, VOLUME_SEPARE = TRUE)
}

# Cascade du pont : on part de la marge de référence, chaque effet la creuse ou
# la remplit, on arrive à la marge de la période. La lecture est immédiate —
# c'est le seul graphe du dashboard qui réponde à « qu'est-ce qui a changé ».
graph_pont_marge <- function(pont, lib_actuel, lib_ref) {
  if (is.null(pont) || !nrow(pont))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de période de référence")))

  libelles <- c(paste0("Marge ", lib_ref), pont$POSTE,
                paste0("Marge ", lib_actuel))
  valeurs  <- c(pont$DEPART[1], pont$EFFET, pont$ARRIVEE[1])
  mesures  <- c("absolute", rep("relative", nrow(pont)), "total")
  
  ymin <- min(pont$DEPART[1], pont$ARRIVEE[1], 0) - 1000
  ymax <- (pont$DEPART[1]+pont$EFFET[1]+pont$EFFET[2]) * 1.1

  plot_ly(
    type = "waterfall", orientation = "v",
    x = ~factor(libelles, levels = libelles), y = valeurs, measure = mesures,
    text = format_CA(valeurs, -1), textposition = "outside",
    connector = list(line = list(color = "#d3c0ac")),
    increasing = list(marker = list(color = COUL_VERT)),
    decreasing = list(marker = list(color = COUL_ROUGE)),
    totals = list(marker = list(color = COUL_BRUN)),
    hovertemplate = paste0(
      libelles, "<br>", format_CA(valeurs, -1),
      c("", rep("<br><i>écart imputable à ce poste</i>", nrow(pont)), ""),
      "<extra></extra>")
  ) %>%
    layout(xaxis = list(title = "", tickangle = -25),
           yaxis = list(title = "€", zeroline = TRUE, zerolinecolor = "#8d7b68",
                        range = c(ymin, ymax)),
           margin = list(b = 110, t = 20), showlegend = FALSE)
}

# Le même pont en tableau, pour lire les montants exacts et vérifier que la
# somme des effets retombe bien sur l'écart.
table_pont_marge <- function(pont) {
  if (is.null(pont) || !nrow(pont)) return(tibble(Info = "Pas de référence."))
  bind_rows(
    pont %>% transmute(Poste = POSTE, Effet = EFFET),
    tibble(Poste = "Écart total", Effet = sum(pont$EFFET))) %>%
    mutate(Sens = case_when(Poste == "Écart total" ~ "",
                            Effet > 0 ~ "favorable",
                            Effet < 0 ~ "défavorable",
                            TRUE ~ "neutre"),
           Effet = format_CA(Effet, -1))
}

##### Contributions compte par compte #####

# Ce que chaque COMPTE a fait bouger entre deux périodes.
#
# C'est le forage sous le pont : le pont dit « les frais généraux ont dérivé de
# 3 000 € », cette table dit lequel des quarante comptes de la classe 61 en est
# responsable. Sans elle, le pont désigne un coupable trop gros pour être
# actionnable.
#
# Le signe est celui de l'effet SUR LA MARGE, pas celui du compte : un achat en
# hausse a un effet négatif. C'est la seule convention qui permette de trier
# les comptes « bonnes nouvelles » et « mauvaises nouvelles » dans la même
# colonne.
contributions_comptes <- function(db, periode, ref_periodes, unite = "mois") {
  if (is.null(db) || !nrow(db)) return(NULL)
  d <- consolide_periode(classe_comptes(db), unite)
  if (is.null(d) || !nrow(d)) return(NULL)

  periode <- as.Date(periode); ref_periodes <- as.Date(ref_periodes)
  if (!length(ref_periodes)) return(NULL)

  act <- d %>% filter(PERIODE == periode) %>%
    group_by(COMPTE, LIBELLE, POSTE, SENS_G) %>%
    summarise(V_ACT = sum(VALEUR, na.rm = TRUE), .groups = "drop")

  # La référence est une MOYENNE quand elle porte sur plusieurs périodes : on
  # compare une période à une période, jamais à un cumul.
  ref <- d %>% filter(PERIODE %in% ref_periodes) %>%
    group_by(COMPTE, LIBELLE, POSTE, SENS_G, PERIODE) %>%
    summarise(V = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
    group_by(COMPTE, LIBELLE, POSTE, SENS_G) %>%
    summarise(V_REF = sum(V) / length(ref_periodes), .groups = "drop")

  full_join(act, ref, by = c("COMPTE", "LIBELLE", "POSTE", "SENS_G")) %>%
    mutate(across(c(V_ACT, V_REF), ~replace_na(., 0)),
           # SENS_G vaut +1 pour un produit, -1 pour une charge : il convertit
           # directement une variation de compte en effet sur la marge.
           EFFET = replace_na(SENS_G, 0) * (V_ACT - V_REF),
           VARIATION_PCT = ratio_pct(V_ACT - V_REF, V_REF),
           NOUVEAU = V_REF == 0 & V_ACT != 0,
           DISPARU = V_ACT == 0 & V_REF != 0) %>%
    filter(EFFET != 0) %>%
    arrange(desc(abs(EFFET)))
}

# Les n comptes qui pèsent le plus, dans les deux sens. Un barres horizontales
# trié par effet : les mauvaises nouvelles d'un côté, les bonnes de l'autre.
graph_contributions <- function(contrib, n = 12) {
  if (is.null(contrib) || !nrow(contrib))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucun mouvement à expliquer")))

  d <- head(contrib, n) %>% arrange(EFFET)
  # tronque_nom garde le numéro de compte visible : deux libellés proches
  # (« Achats boissons » / « Achats boissons non alcoolisées ») ne doivent pas
  # se confondre une fois coupés.
  lab <- paste0(d$COMPTE, " · ", tronque_nom(d$LIBELLE, 34))

  plot_ly() %>%
    add_bars(
      x = d$EFFET, y = factor(lab, levels = lab), orientation = "h",
      marker = list(color = ifelse(d$EFFET >= 0, COUL_VERT, COUL_ROUGE)),
      hovertemplate = paste0(
        "<b>", d$COMPTE, " ", tronque_nom(d$LIBELLE, 45), "</b>",
        "<br>", d$POSTE,
        "<br>période : ", format_CA(d$V_ACT, -1),
        "<br>référence : ", format_CA(d$V_REF, -1),
        "<br>effet sur la marge : ", format_CA(d$EFFET, -1),
        ifelse(d$NOUVEAU, "<br><i>compte absent de la référence</i>", ""),
        ifelse(d$DISPARU, "<br><i>compte sans montant sur la période</i>", ""),
        "<extra></extra>")) %>%
    layout(xaxis = list(title = "Effet sur la marge (€)", zeroline = TRUE,
                        zerolinecolor = "#8d7b68"),
           yaxis = list(title = "", automargin = TRUE),
           margin = list(l = 10), showlegend = FALSE)
}

table_contributions <- function(contrib, n = 25) {
  if (is.null(contrib) || !nrow(contrib))
    return(tibble(Info = "Aucun mouvement à expliquer sur cette période."))
  head(contrib, n) %>%
    transmute(
      Compte   = COMPTE,
      Libellé  = tronque_nom(LIBELLE, 45),
      Poste    = POSTE,
      Période  = format_CA(V_ACT, -1),
      Référence = format_CA(V_REF, -1),
      `Effet marge` = format_CA(EFFET, -1),
      Variation = case_when(NOUVEAU ~ "nouveau",
                            DISPARU ~ "disparu",
                            is.na(VARIATION_PCT) ~ "—",
                            TRUE ~ paste0(ifelse(VARIATION_PCT >= 0, "+", ""),
                                          format_pct(VARIATION_PCT))))
}

##### Tendance d'un indicateur #####

# La série d'un indicateur, avec sa normale et son repérage.
#
# La normale est la MÉDIANE de la série et la bande vaut deux MAD, exactement
# comme le tableau du volet Exploitation (cf. flag_ecart) : les deux écrans
# doivent signaler les mêmes périodes, sinon l'un des deux ment. Reprendre ici
# une moyenne et un écart-type donnerait un graphe qui contredit son tableau.
serie_indicateur <- function(serie, cle) {
  ind <- indicateur_analyse(cle)
  if (is.null(serie) || !nrow(serie) || !ind$COLONNE %in% names(serie))
    return(NULL)
  v <- serie[[ind$COLONNE]]

  centre <- stats::median(v, na.rm = TRUE)
  s <- stats::mad(v, center = centre, constant = 1.4826, na.rm = TRUE)
  if (is.na(s) || s == 0) s <- 1.2533 * mean(abs(v - centre), na.rm = TRUE)
  if (is.na(s)) s <- 0

  serie %>%
    transmute(PERIODE, VALEUR = v,
              MEDIANE = centre, HAUT = centre + 2 * s, BAS = centre - 2 * s,
              FLAG = flag_ecart(v, sens = ind$SENS))
}

graph_tendance <- function(st, cle, unite = "mois") {
  ind <- indicateur_analyse(cle)
  if (is.null(st) || !nrow(st))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de série à tracer")))

  lab <- etiquette_periode(st$PERIODE, unite)
  ordre <- factor(lab, levels = lab)
  fmt <- function(x) format_indicateur(x, ind$UNITE)

  # Points colorés par le repérage : vert favorable, rouge défavorable, brun
  # sinon. La bande grise est la zone « normale » ; en sortir est ce qu'on
  # cherche à voir du premier coup d'œil.
  coul <- ifelse(st$FLAG == 1L, COUL_VERT,
          ifelse(st$FLAG == -1L, COUL_ROUGE, COUL_BRUN))

  p <- plot_ly()
  if (!all(is.na(st$HAUT)) && any(st$HAUT != st$BAS)) {
    p <- p %>%
      add_trace(x = ordre, y = st$HAUT, type = "scatter", mode = "lines",
                line = list(width = 0), showlegend = FALSE, hoverinfo = "skip",
                name = "haut") %>%
      add_trace(x = ordre, y = st$BAS, type = "scatter", mode = "lines",
                line = list(width = 0), fill = "tonexty",
                fillcolor = "rgba(141,123,104,0.14)", showlegend = TRUE,
                hoverinfo = "skip", name = "Zone habituelle (± 2 MAD)")
  }

  p %>%
    add_trace(x = ordre, y = st$MEDIANE, type = "scatter", mode = "lines",
              name = "Médiane de la série",
              line = list(color = COUL_NEUTRE, width = 1.5, dash = "dot"),
              hoverinfo = "skip") %>%
    add_trace(x = ordre, y = st$VALEUR, type = "scatter",
              mode = "lines+markers", name = ind$LIBELLE,
              line = list(color = COUL_BRUN, width = 2.5),
              marker = list(color = coul, size = 9,
                            line = list(color = "#fffaf4", width = 1)),
              hovertemplate = paste0(
                "<b>", lab, "</b><br>", fmt(st$VALEUR),
                "<br>habituel : ", fmt(st$MEDIANE),
                ifelse(st$FLAG == 0L, "",
                       ifelse(st$FLAG == 1L,
                              "<br><b>écart favorable</b>",
                              "<br><b>écart défavorable</b>")),
                "<extra></extra>")) %>%
    layout(xaxis = list(title = "", tickangle = -25),
           yaxis = list(title = if (ind$UNITE == "pct") "% du CA" else "€",
                        zeroline = TRUE, zerolinecolor = "#8d7b68"),
           legend = list(orientation = "h", y = -0.3), margin = list(b = 90))
}

##### Saisonnalité #####

# Le même indicateur, mois par mois, une courbe par année.
#
# Répond à la seule question que la tendance chronologique ne sait pas poser :
# « février est mauvais, mais février est-il toujours mauvais ? » Une série
# continue mélange l'effet de saison et l'effet de gestion ; les superposer par
# année les sépare.
#
# Travaille toujours sur les postes MENSUELS, quelle que soit la granularité
# choisie ailleurs : une saisonnalité par année n'aurait qu'un point par courbe.
saisonnalite <- function(postes_mensuels, cle) {
  ind <- indicateur_analyse(cle)
  if (is.null(postes_mensuels) || !nrow(postes_mensuels) ||
      !ind$COLONNE %in% names(postes_mensuels)) return(NULL)
  postes_mensuels %>%
    transmute(ANNEE = year(PERIODE), MOIS = month(PERIODE),
              VALEUR = .data[[ind$COLONNE]]) %>%
    filter(!is.na(VALEUR)) %>%
    arrange(ANNEE, MOIS)
}

graph_saisonnalite <- function(sais, cle) {
  ind <- indicateur_analyse(cle)
  if (is.null(sais) || !nrow(sais))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas assez d'historique mensuel")))

  annees <- sort(unique(sais$ANNEE))
  # L'année la plus récente en brun soutenu, les précédentes en dégradé pâle :
  # on veut lire l'année en cours SUR un fond d'années passées, pas six
  # courbes qui se disputent l'attention.
  pal <- setNames(
    c(rep(NA, max(0, length(annees) - 1)), COUL_BRUN)[seq_along(annees)],
    annees)
  anciennes <- head(annees, -1)
  if (length(anciennes))
    pal[as.character(anciennes)] <- grDevices::colorRampPalette(
      c("#e6d9c8", "#c0a183"))(length(anciennes))

  # vecteur_mois_court (global.R) plutôt que %b : ce dernier suit la locale du
  # serveur et rendrait « Feb » sur une machine en locale C.
  fmt <- function(x) format_indicateur(x, ind$UNITE)

  p <- plot_ly()
  for (an in annees) {
    d <- sais %>% filter(ANNEE == an)
    recente <- an == max(annees)
    p <- add_trace(
      p, x = factor(vecteur_mois_court[d$MOIS], levels = vecteur_mois_court),
      y = d$VALEUR, type = "scatter", mode = "lines+markers",
      name = as.character(an),
      line = list(color = pal[[as.character(an)]],
                  width = if (recente) 3 else 1.8),
      marker = list(color = pal[[as.character(an)]],
                    size = if (recente) 8 else 5),
      hovertemplate = paste0("<b>", vecteur_mois_court[d$MOIS], " ", an,
                             "</b><br>", fmt(d$VALEUR), "<extra></extra>"))
  }
  p %>% layout(
    xaxis = list(title = "", categoryorder = "array",
                 categoryarray = vecteur_mois_court),
    yaxis = list(title = if (ind$UNITE == "pct") "% du CA" else "€",
                 zeroline = TRUE, zerolinecolor = "#8d7b68"),
    legend = list(orientation = "h", y = -0.25), margin = list(b = 70))
}

##### Comparaison chiffrée #####

# Le tableau qui accompagne le pont : période, référence, écart, écart relatif.
#
# Les pourcentages de structure sont donnés en POINTS d'écart et non en pour
# cent de pour cent : passer de 30 % à 33 %, c'est « +3 pt », pas « +10 % ».
# La confusion entre les deux est l'erreur de lecture la plus courante sur ce
# genre de tableau, et elle se règle à l'affichage.
table_comparaison <- function(actuel, reference, lib_actuel, lib_ref) {
  if (is.null(actuel) || !nrow(actuel))
    return(tibble(Info = "Aucune période sélectionnée."))
  if (is.null(reference) || !nrow(reference))
    return(tibble(Info = "Pas de période de référence disponible."))
  a <- actuel[1, ]; r <- reference[1, ]

  ligne <- function(lib, champ, unite = "eur", sens = 1) {
    va <- a[[champ]]; vr <- r[[champ]]
    ecart <- va - vr
    tibble(
      Poste      = lib,
      `Période`  = format_indicateur(va, unite),
      `Référence`= format_indicateur(vr, unite),
      `Écart`    = if (unite == "pct")
                     paste0(ifelse(ecart >= 0, "+", ""), round(ecart, 1), " pt")
                   else paste0(ifelse(ecart >= 0, "+", ""), format_CA(ecart, -1)),
      `Écart %`  = if (unite == "pct") "—"
                   else if (is.na(vr) || vr == 0) "—"
                   else paste0(ifelse(ecart >= 0, "+", ""),
                               format_pct(100 * ecart / abs(vr))),
      .sens      = ifelse(is.na(ecart) | ecart == 0, 0L,
                          ifelse(sign(ecart) == sens, 1L, -1L)))
  }

  bind_rows(
    ligne("Chiffre d'affaires",           "CA",            "eur",  1),
    ligne("Autres produits",              "AUTRES",        "eur",  1),
    ligne("Matières premières",           "MATIERES",      "eur", -1),
    ligne("Matières en % du CA",          "PCT_MATIERES",  "pct", -1),
    ligne("Rémunérations",                "REMUNERATION",  "eur", -1),
    ligne("Rémunérations en % du CA",     "PCT_TRAVAIL",   "pct", -1),
    ligne("Frais généraux",               "GENERAUX",      "eur", -1),
    ligne("Frais généraux en % du CA",    "PCT_GENERAUX",  "pct", -1),
    ligne("Résultat financier",           "FINANCIER",     "eur",  1),
    ligne("Marge avant amortissements",   "MARGE_AA",      "eur",  1),
    ligne("Marge en % du CA",             "PCT_MARGE_AA",  "pct",  1)
  ) %>% rename(!!paste0("Période\n", lib_actuel) := `Période`,
               !!paste0("Référence\n", lib_ref)  := `Référence`)
}

##### Tuiles #####

# Quatre tuiles, toutes en ÉCART : c'est l'objet du volet. Le niveau absolu se
# lit dans le sous-titre, jamais en gros — le volet Exploitation est là pour ça.
kpi_analyse <- function(actuel, reference, lib_ref, unite = "mois") {
  if (is.null(actuel) || !nrow(actuel))
    return(div(class = "text-muted small p-2", "Aucune période sélectionnée."))
  a <- actuel[1, ]

  if (is.null(reference) || !nrow(reference))
    return(div(
      class = "kpi-grid",
      kpi_tile(format_CA(a$CA, -1), paste("CA —", etiquette_periode(a$PERIODE, unite)),
               COUL_BRUN, "euro-sign", sous_titre = "pas de référence"),
      kpi_tile(format_CA(a$MARGE_AA, -1), "Marge avant amortissements",
               if (a$MARGE_AA >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
               sous_titre = format_pct(a$PCT_MARGE_AA))))
  r <- reference[1, ]

  # sens = +1 quand une hausse est une bonne nouvelle.
  tuile_eur <- function(champ, lib, icone, sens) {
    e <- a[[champ]] - r[[champ]]
    pct <- if (is.na(r[[champ]]) || r[[champ]] == 0) NA_real_
           else 100 * e / abs(r[[champ]])
    kpi_tile(
      paste0(if (e >= 0) "+" else "", format_CA(e, -1)), lib,
      if (is.na(e) || e == 0) COUL_NEUTRE
      else if (sign(e) == sens) COUL_VERT else COUL_ROUGE,
      icone,
      sous_titre = paste0(format_indicateur(a[[champ]], "eur"), " vs ",
                          format_indicateur(r[[champ]], "eur"),
                          if (!is.na(pct)) paste0(" · ", if (pct >= 0) "+" else "",
                                                  format_pct(pct)) else ""))
  }
  tuile_pt <- function(champ, lib, icone, sens) {
    e <- a[[champ]] - r[[champ]]
    kpi_tile(
      paste0(if (!is.na(e) && e >= 0) "+" else "", round(e, 1), " pt"), lib,
      if (is.na(e) || e == 0) COUL_NEUTRE
      else if (sign(e) == sens) COUL_VERT else COUL_ROUGE,
      icone,
      sous_titre = paste0(format_pct(a[[champ]]), " vs ", format_pct(r[[champ]])))
  }

  div(
    class = "kpi-grid",
    kpi_tile(etiquette_periode(a$PERIODE, unite), "Période analysée",
             COUL_BRUN, "calendar-day", sous_titre = paste("comparée à", lib_ref)),
    tuile_eur("CA", "Écart de chiffre d'affaires", "euro-sign", 1),
    tuile_eur("MARGE_AA", "Écart de marge", "piggy-bank", 1),
    tuile_pt("PCT_MARGE_AA", "Écart de taux de marge", "percent", 1),
    tuile_pt("PCT_MATIERES", "Écart taux matières", "cart-shopping", -1),
    tuile_pt("PCT_TRAVAIL", "Écart taux rémunérations", "users", -1)
  )
}

##### Diagnostic automatique #####

# La liste de contrôles qui répond à « est-ce que tout va bien ? ».
#
# Chaque contrôle est une question qu'on se poserait à la main en ouvrant la
# comptabilité, écrite une fois pour toutes. Trois statuts seulement :
#
#   ok        rien à faire
#   attention à regarder, pas forcément un problème
#   alerte    quelque chose ne va pas, et on sait quoi
#
# Le volet n'invente aucun seuil comptable : les bornes de ratio sont celles
# déjà employées par kpi_exploitation, et le repérage statistique est celui de
# flag_ecart. Un diagnostic qui contredirait les autres écrans serait pire
# qu'aucun diagnostic.
SEUILS_RATIOS <- tibble::tribble(
  ~CHAMP,          ~LIBELLE,            ~BON, ~MOYEN,
  "PCT_MATIERES",  "Coût matières",       30,     35,
  "PCT_TRAVAIL",   "Coût du travail",     35,     42,
  "PCT_GENERAUX",  "Frais généraux",      20,     28
)

diagnostic_compta <- function(db, serie, actuel, unite = "mois",
                              aujourd_hui = Sys.Date()) {
  res <- list()
  ajoute <- function(titre, statut, message) {
    res[[length(res) + 1]] <<- tibble(TITRE = titre, STATUT = statut,
                                      MESSAGE = message)
  }

  # 1. Le découpage en postes retombe-t-il sur le solde publié ?
  ctrl <- try(controle_exploitation(db, postes_exploitation(db)), silent = TRUE)
  if (inherits(ctrl, "try-error") || is.null(ctrl) || !nrow(ctrl)) {
    ajoute("Contrôle du classement", "attention",
           "Impossible de confronter la marge reconstruite au solde comptable.")
  } else {
    n <- sum(abs(ctrl$ECART) > 1, na.rm = TRUE)
    ajoute("Contrôle du classement",
           if (n == 0) "ok" else "alerte",
           if (n == 0)
             paste0("Sur ", nrow(ctrl), " mois, la marge reconstruite retombe ",
                    "exactement sur le résultat d'exploitation publié.")
           else paste0(n, " mois sur ", nrow(ctrl), " où la marge reconstruite ",
                       "diffère du solde comptable de plus d'un euro. Un compte ",
                       "échappe au classement en postes."))
  }

  # 2. Des comptes que le plan ne sait pas ranger ?
  nc <- try(comptes_non_classes(db), silent = TRUE)
  if (!inherits(nc, "try-error")) {
    ajoute("Comptes non classés",
           if (!nrow(nc)) "ok" else "alerte",
           if (!nrow(nc))
             "Tous les comptes sont rattachés à un poste par leur numéro."
           else paste0(nrow(nc), " compte(s) hors plan, pour ",
                       format_CA(sum(abs(nc$TOTAL)), -1), " au total. Ils ",
                       "n'entrent dans aucun poste : le premier est ",
                       nc$COMPTE[1], " ", tronque_nom(nc$LIBELLE[1], 40), "."))
  }

  # 3. La période analysée est-elle close et complète ?
  et <- etat_periode(actuel, unite, aujourd_hui)
  if (!is.null(et)) {
    ajoute("Complétude de la période",
           if (!et$partielle) "ok" else if (et$en_cours) "attention" else "alerte",
           if (!et$partielle)
             paste0(et$libelle, " est close et complète.")
           else if (et$en_cours)
             paste0(et$libelle, " n'est pas terminé (", et$n_mois, " mois sur ",
                    et$attendu, ") : les totaux ne sont pas comparables à une ",
                    "période close.")
           else paste0(et$libelle, " est terminé mais la comptabilité n'en ",
                       "couvre que ", et$n_mois, " mois sur ", et$attendu, "."))
  }

  # 4. Les ratios de gestion tiennent-ils leurs bornes ?
  if (!is.null(actuel) && nrow(actuel)) {
    for (i in seq_len(nrow(SEUILS_RATIOS))) {
      champ <- SEUILS_RATIOS$CHAMP[i]
      v <- actuel[[champ]][1]
      if (is.na(v)) next
      statut <- if (v <= SEUILS_RATIOS$BON[i]) "ok"
                else if (v <= SEUILS_RATIOS$MOYEN[i]) "attention" else "alerte"
      ajoute(SEUILS_RATIOS$LIBELLE[i], statut,
             paste0(format_pct(v), " du CA — la borne de confort est ",
                    SEUILS_RATIOS$BON[i], " %, le seuil d'alerte ",
                    SEUILS_RATIOS$MOYEN[i], " %."))
    }
  }

  # 5. La période s'écarte-t-elle de sa propre série ?
  # On ne signale QUE la période analysée : lister tous les écarts de
  # l'historique ferait un diagnostic permanent, donc ignoré.
  if (!is.null(serie) && nrow(serie) >= 5 && !is.null(actuel) && nrow(actuel)) {
    suspects <- character(0)
    for (i in seq_len(nrow(INDICATEURS_ANALYSE))) {
      ind <- as.list(INDICATEURS_ANALYSE[i, ])
      if (ind$UNITE != "eur") next
      if (!ind$COLONNE %in% names(serie)) next
      f <- flag_ecart(serie[[ind$COLONNE]], sens = ind$SENS)
      j <- which(serie$PERIODE == actuel$PERIODE[1])
      if (length(j) == 1 && f[j] == -1L)
        suspects <- c(suspects, ind$LIBELLE)
    }
    ajoute("Écarts statistiques",
           if (!length(suspects)) "ok" else "attention",
           if (!length(suspects))
             paste0("Aucun poste de la période ne s'écarte de plus de deux MAD ",
                    "de sa propre série (", nrow(serie), " périodes).")
           else paste0("Écart défavorable de plus de deux MAD sur : ",
                       paste(suspects, collapse = ", "),
                       ". À vérifier en comptabilité avant d'y lire un fait ",
                       "de gestion."))
  }

  # 6. La marge est-elle positive ?
  if (!is.null(actuel) && nrow(actuel) && !is.na(actuel$MARGE_AA[1])) {
    m <- actuel$MARGE_AA[1]
    ajoute("Marge avant amortissements",
           if (m >= 0) "ok" else "alerte",
           paste0(format_CA(m, -1), " sur la période, soit ",
                  format_pct(actuel$PCT_MARGE_AA[1]), " du chiffre d'affaires.",
                  if (m < 0) " La période ne couvre pas ses charges." else ""))
  }

  bind_rows(res)
}

# Rendu du diagnostic : une ligne par contrôle, pastille de couleur à gauche.
# Volontairement pas un DT : c'est une liste à lire, pas un tableau à trier.
rendu_diagnostic <- function(diag) {
  if (is.null(diag) || !nrow(diag))
    return(div(class = "text-muted small p-2", "Aucun contrôle disponible."))

  couleur <- c(ok = COUL_VERT, attention = COUL_AMBRE, alerte = COUL_ROUGE)
  icone   <- c(ok = "circle-check", attention = "circle-exclamation",
               alerte = "triangle-exclamation")
  # Les alertes d'abord : un écran de diagnostic se lit du haut, et ce qui est
  # vert n'a pas besoin d'être lu.
  diag <- diag %>% mutate(RANG = match(STATUT, c("alerte", "attention", "ok"))) %>%
    arrange(RANG)

  div(
    class = "diagnostic-liste",
    lapply(seq_len(nrow(diag)), function(i) {
      st <- diag$STATUT[i]
      coul <- couleur[[st]]
      div(
        class = "d-flex align-items-start gap-2 diagnostic-ligne",
        style = paste0("border-left:4px solid ", coul, ";",
                       "background:", coul, "12;",
                       "border-radius:0.4rem;padding:0.55rem 0.8rem;",
                       "margin-bottom:0.45rem;"),
        span(style = paste0("color:", coul, ";font-size:1.05rem;line-height:1.3;"),
             icon(icone[[st]])),
        div(
          div(style = paste0("font-weight:650;color:", coul, ";"), diag$TITRE[i]),
          div(class = "small", diag$MESSAGE[i])))
    }))
}

# Compteur de synthèse, pour l'en-tête de la carte.
resume_diagnostic <- function(diag) {
  if (is.null(diag) || !nrow(diag)) return("")
  n <- table(factor(diag$STATUT, levels = c("alerte", "attention", "ok")))
  paste0(n[["ok"]], " au vert · ", n[["attention"]], " à regarder · ",
         n[["alerte"]], " en alerte")
}
