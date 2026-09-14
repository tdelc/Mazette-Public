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

##### Comparer une période partielle #####

# Une période en cours n'a pas fini de vivre : août 2026 apporte huit mois de
# comptabilité quand 2025 en apporte douze. Les confronter tels quels compare
# huit mois à douze, et l'écart affiché n'est qu'une différence de durée.
#
# La réponse est de RABOTER la référence à la même taille — plus exactement aux
# mêmes MOIS. Pas « les huit premiers mois » au sens positionnel, mais les mois
# de même rang dans leur période : si 2026 porte janvier à août, on ne garde de
# 2025 que janvier à août. La saisonnalité est ainsi respectée, ce qu'un simple
# prorata (x 12/8) manquerait complètement : raboter n'invente rien, il retire.
#
# Ça vaut aussi quand il manque un mois au milieu : si 2026 n'a pas mars, on
# retire mars de 2025 également. On compare toujours les mêmes rangs.

# Rang d'un mois dans sa période : 0 pour janvier d'une année, 2 pour le
# troisième mois d'un trimestre. C'est la clé sur laquelle tout le rabotage
# s'appuie, et elle se déduit de la seule granularité.
offset_mois <- function(mois, unite) {
  mois <- as.Date(mois)
  debut <- switch(unite,
                  mois      = mois,
                  trimestre = floor_date(mois, "quarter"),
                  annee     = floor_date(mois, "year"))
  # as.integer explicite : year() et month() rendent des doubles, et un rang de
  # mois qui se promène en double finit par se comparer de travers dans un
  # %in% ou un identical().
  as.integer(12L * (year(mois) - year(debut)) + (month(mois) - month(debut)))
}

# Nombre de mois qu'une période complète devrait porter.
mois_attendus <- function(unite) switch(unite, mois = 1L, trimestre = 3L,
                                        annee = 12L)

# Les rangs effectivement présents dans la période analysée. C'est le gabarit
# auquel toutes les autres périodes seront ramenées.
offsets_periode <- function(postes_mensuels, periode, unite) {
  if (is.null(postes_mensuels) || !nrow(postes_mensuels))
    return(seq_len(mois_attendus(unite)) - 1L)
  d <- postes_mensuels %>%
    filter(!is.na(PERIODE),
           consolide_periode(tibble(PERIODE = PERIODE), unite)$PERIODE ==
             as.Date(periode))
  if (!nrow(d)) return(seq_len(mois_attendus(unite)) - 1L)
  sort(unique(offset_mois(d$PERIODE, unite)))
}

# La série agrégée en ne gardant, dans CHAQUE période, que les mois dont le
# rang figure dans le gabarit. Sur une période complète le gabarit est complet
# et la fonction ne retire rien : c'est pour cela qu'on peut l'appliquer sans
# condition, et que le rabotage n'a d'effet que là où il en fallait un.
serie_rabotee <- function(postes_mensuels, unite, offsets) {
  if (is.null(postes_mensuels) || !nrow(postes_mensuels))
    return(agrege_exploitation(postes_mensuels, unite))
  postes_mensuels %>%
    filter(offset_mois(PERIODE, unite) %in% offsets) %>%
    agrege_exploitation(unite)
}

# Comment nommer une référence rabotée.
#
# Sur une année, les rangs sont les mois de l'année : on peut les nommer, et
# « janv.–août » se lit infiniment mieux que « 8 mois sur 12 ». Sur un
# trimestre, le rang 0 est tantôt janvier tantôt juillet : on s'en tient au
# décompte, qui reste juste dans tous les cas.
etiquette_rabotage <- function(offsets, unite) {
  n <- length(offsets); attendu <- mois_attendus(unite)
  if (n >= attendu) return(NULL)
  contigu <- identical(as.integer(offsets), seq_len(n) - 1L)
  if (identical(unite, "annee") && contigu)
    paste0(vecteur_mois_court[1], "–", vecteur_mois_court[n])
  else paste0(n, " mois sur ", attendu)
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
# La décomposition naïve — Δmarge = ΔCA - Δmatières - Δrémunérations … — est
# exacte mais ne dit rien d'utile : quand le CA baisse de 20 %, TOUTES les
# charges baissent, et le pont affiche six barres qui pointent dans le même
# sens sans qu'on sache si la gestion a dérivé.
#
# On sépare donc ce qui vient du VOLUME de ce qui vient des TAUX.
#
# ---------------------------------------------------------------------------
# Le calcul, en toutes lettres
# ---------------------------------------------------------------------------
# Deux formes de lignes, et deux seulement.
#
#   EFFET VOLUME    écart de CA  x  taux de marge de la référence
#
#     Le CA a bougé de +10 000 €. Si RIEN d'autre n'avait changé — si chaque
#     poste avait gardé exactement le même poids dans le CA — ces 10 000 €
#     auraient rapporté la marge qu'un euro de CA rapportait à la référence.
#     Le taux de marge de référence, c'est simplement sa marge divisée par son
#     CA : 12 000 / 100 000 = 12 %. Donc 10 000 x 12 % = +1 200 € de marge.
#
#   EFFET D'UN POSTE    ce qu'il a coûté  -  ce qu'il aurait dû coûter
#
#     Les matières pesaient 30 % du CA à la référence. À ce poids-là, sur les
#     110 000 € de CA de la période, elles auraient dû coûter 33 000 €. Elles
#     ont coûté 37 400 €. L'écart, 4 400 €, est exactement ce que ce poste a
#     coûté de marge. Une charge qui monte fait baisser la marge : l'effet
#     porte le signe opposé à l'écart.
#
#     C'est une soustraction d'euros, sans pourcentage et SANS ARRONDI. La
#     version « 4 points × 110 000 € » dit la même chose, mais les points
#     affichés sont arrondis au dixième : un lecteur qui refait le produit
#     tombe à côté de l'effet dès que le taux n'est pas rond. Ici, les trois
#     nombres affichés se soustraient exactement.
#
# Et ces six termes retombent EXACTEMENT sur la variation de marge. La preuve
# tient en trois lignes. En notant s le taux de marge (marge / CA) :
#
#   marge = CA x s        donc
#   Δmarge = CA1.s1 - CA0.s0
#          = (CA1 - CA0).s0  +  CA1.(s1 - s0)
#
# Le premier terme est l'effet volume. Le second se répartit entre les postes,
# puisque s = 1 + a - m - r - g + f (chaque lettre étant un poste rapporté au
# CA) : s1 - s0 = Δa - Δm - Δr - Δg + Δf, et multiplier par CA1 donne une ligne
# par poste. Aucun reste, aucun arrondi : c'est une identité, et un test la
# vérifie à l'euro près.
#
# La forme affichée d'une ligne de poste est la même chose écrite en euros :
#
#   -CA1 x (m1 - m0)  =  -(CA1.m1 - m0.CA1)  =  -(réel - attendu)
#
# avec « attendu » = m0 x CA1, ce que le poste aurait coûté en gardant son
# poids de référence sur le CA de la période.
#
# Le taux de marge de référence mérite d'être retenu sous sa forme courte :
#
#   s0 = 1 + a - m - r - g + f  =  MARGE_AA(référence) / CA(référence)
#
# Les deux écritures sont le même nombre — la seconde est celle qu'on affiche,
# parce qu'on peut la refaire de tête.
#
# Quand le CA de référence est nul, les taux n'existent pas : on retombe sur la
# décomposition brute, qui reste exacte, et VOLUME_SEPARE le dit.
#
# ---------------------------------------------------------------------------
# Les colonnes rendues
# ---------------------------------------------------------------------------
# Au-delà de EFFET, la table porte de quoi REFAIRE le calcul à la main — c'est
# tout l'objet du sous-onglet « Décomposition du pont ». Pour chaque ligne :
#
#   NATURE    "volume", "taux" ou "brut" (régime de repli)
#   ATTENDU   ligne de poste : ce qu'il aurait coûté au poids de référence ;
#             ligne volume : le CA de la référence
#   REEL      le montant de la période (ou son CA)
#   ECART     REEL - ATTENDU, toujours en euros
#   BASE      le taux de marge de référence, pour la seule ligne volume
#   TAUX_REF  le poids du poste dans le CA, à la référence puis à la période —
#   TAUX_ACT  affichés dans la phrase de lecture, jamais dans le calcul
#
# L'invariant qui rend la table vérifiable, et qu'un test rejoue sur le TEXTE
# affiché : ECART = REEL - ATTENDU sur toutes les lignes, puis
# EFFET = SIGNE x ECART, sauf la ligne volume où EFFET = ECART x BASE / 100.
pont_marge <- function(actuel, reference) {
  if (is.null(actuel) || !nrow(actuel) || is.null(reference) || !nrow(reference))
    return(NULL)
  a <- as.list(actuel[1, ]); r <- as.list(reference[1, ])

  if (is.na(r$CA) || r$CA <= 0) {
    postes <- c("Chiffre d'affaires", "Autres produits", "Matières premières",
                "Rémunérations", "Frais généraux", "Résultat financier")
    champs <- c("CA", "AUTRES", "MATIERES", "REMUNERATION", "GENERAUX",
                "FINANCIER")
    signe  <- c(1, 1, -1, -1, -1, 1)
    ref <- vapply(champs, function(x) as.numeric(r[[x]]), 0)
    act <- vapply(champs, function(x) as.numeric(a[[x]]), 0)
    return(tibble(
      POSTE = postes, NATURE = "brut",
      ATTENDU = ref, REEL = act, ECART = act - ref,
      BASE = NA_real_, SIGNE = signe,
      TAUX_REF = NA_real_, TAUX_ACT = NA_real_,
      EFFET = signe * (act - ref),
      DEPART = r$MARGE_AA, ARRIVEE = a$MARGE_AA, VOLUME_SEPARE = FALSE))
  }

  # Le taux de marge de la référence : sa marge divisée par son CA. C'est
  # exactement 1 + a - m - r - g + f, mais sous une forme qu'on peut refaire
  # de tête — et c'est la forme qu'affiche le tableau de décomposition.
  s0_pct <- 100 * r$MARGE_AA / r$CA
  taux <- function(champ, l) 100 * l[[champ]] / l$CA

  champs <- c("AUTRES", "MATIERES", "REMUNERATION", "GENERAUX", "FINANCIER")
  postes <- c("Autres produits", "Matières premières", "Rémunérations",
              "Frais généraux", "Résultat financier")
  signe  <- c(1, -1, -1, -1, 1)
  t_ref  <- vapply(champs, taux, 0, l = r)
  t_act  <- vapply(champs, taux, 0, l = a)

  # Ce que chaque poste aurait coûté (ou rapporté) en gardant son poids de
  # référence sur le CA de la période. C'est la grandeur qui rend la ligne
  # exacte à l'euro, sans passer par des points arrondis.
  attendu <- t_ref / 100 * a$CA
  reel    <- vapply(champs, function(x) as.numeric(a[[x]]), 0)

  bind_rows(
    tibble(POSTE = "Effet volume (CA)", NATURE = "volume",
           ATTENDU = r$CA, REEL = a$CA, ECART = a$CA - r$CA,
           BASE = s0_pct, SIGNE = 1,
           TAUX_REF = NA_real_, TAUX_ACT = NA_real_,
           EFFET = (a$CA - r$CA) * s0_pct / 100),
    tibble(POSTE = postes, NATURE = "taux",
           ATTENDU = attendu, REEL = reel, ECART = reel - attendu,
           BASE = NA_real_, SIGNE = signe,
           TAUX_REF = t_ref, TAUX_ACT = t_act,
           EFFET = signe * (reel - attendu))) %>%
    mutate(DEPART = r$MARGE_AA, ARRIVEE = a$MARGE_AA, VOLUME_SEPARE = TRUE)
}

# format_CA() blanchit les montants nuls et aligne les largeurs sur le plus
# long élément du vecteur. Les deux comportements sont utiles ailleurs et
# nuisibles ici : dans un tableau dont l'objet est de refaire l'arithmétique,
# une cellule vide se lit « donnée manquante » et non « zéro », et une cellule
# rembourrée d'espaces ne se compare plus à ce qu'on attend. Un poste dont le
# poids n'a pas bougé DOIT montrer 0 €, c'est même le résultat le plus parlant
# du pont.
euro_exact <- function(x) {
  ifelse(is.na(x), "—",
  ifelse(abs(x) < 0.5, "0 €", trimws(format_CA(x, -1))))
}

euro_signe <- function(x) {
  ifelse(is.na(x), "—",
  ifelse(abs(x) < 0.5, "0 €",
         paste0(ifelse(x > 0, "+", ""), trimws(format_CA(x, -1)))))
}

# Un poids dans le CA, en pourcentage à une décimale.
#
# scientific = FALSE, pour la même raison que dans format_CA() : sans lui,
# format() rend « 4e+00 » plutôt que « 4,0 » dès que la notation scientifique
# est plus courte. Le piège frappe tous les nombres ronds, et il est d'autant
# plus vicieux ici que 4 points d'écart sur les matières est exactement le
# genre de valeur qu'on veut lire.
format_points <- function(x, signe = FALSE, suffixe = " pt") {
  txt <- format(round(x, 1), nsmall = 1, trim = TRUE, decimal.mark = ",",
                scientific = FALSE)
  paste0(if (signe) ifelse(x >= 0, "+", "") else "", txt, suffixe)
}

# Les deux morceaux de phrase qui décrivent une ligne du pont : ce qu'on
# compare, et le cas échéant ce par quoi on multiplie. Partagés par le tableau
# et par l'infobulle du graphe, pour que les deux racontent mot pour mot le
# même calcul.
libelle_attendu_pont <- function(pont) {
  ifelse(pont$NATURE == "volume", paste0(euro_exact(pont$ATTENDU), " de CA"),
  ifelse(pont$NATURE == "brut",   euro_exact(pont$ATTENDU),
         paste0(euro_exact(pont$ATTENDU), " attendus")))
}

libelle_reel_pont <- function(pont) {
  ifelse(pont$NATURE == "volume", paste0(euro_exact(pont$REEL), " de CA"),
  ifelse(pont$NATURE == "brut",   euro_exact(pont$REEL),
         paste0(euro_exact(pont$REEL), " réels")))
}

# L'opération qui mène de l'écart à l'effet. Vide pour une ligne de poste :
# l'écart EST l'effet, au signe près, et une colonne « × 1 » n'apprendrait rien.
libelle_base_pont <- function(pont, lib_ref = "la référence") {
  ifelse(pont$NATURE == "volume",
         paste0("× ", format_pct(pont$BASE), " de marge"),
  ifelse(pont$SIGNE < 0, "charge : signe opposé", "produit : même signe"))
}

# La phrase de lecture d'une ligne : celle qu'on se dirait à voix haute en
# refaisant le calcul. C'est elle qui fait comprendre le pont la première fois.
lecture_pont <- function(pont, lib_actuel = "la période",
                         lib_ref = "la référence") {
  eff   <- euro_exact(abs(pont$EFFET))
  gagne <- ifelse(pont$EFFET >= 0, "rapporte ", "coûte ")

  phrases <- ifelse(
    pont$NATURE == "volume",
    paste0("le chiffre d'affaires passe de ", euro_exact(pont$ATTENDU), " à ",
           euro_exact(pont$REEL), " ;
ces ", euro_exact(abs(pont$ECART)),
           " au taux de marge de ", lib_ref, " (", format_pct(pont$BASE),
           ") ", ifelse(pont$EFFET >= 0, "rapportent ", "coûtent "), eff),
    ifelse(
      pont$NATURE == "brut",
      paste0("le poste passe de ", euro_exact(pont$ATTENDU), " à ",
             euro_exact(pont$REEL), ", ce qui ", gagne, eff, " de marge"),
      # « à la référence » plutôt que son libellé : il vaut tantôt « juin 2026 »,
      # tantôt « médiane des 6 périodes précédentes », et la phrase doit rester
      # lisible dans les deux cas. Le libellé est déjà en en-tête de colonne.
      paste0("
ce poste pesait ", format_points(pont$TAUX_REF, suffixe = " %"),
             " du CA à la référence ;
à ce poids, il aurait ",
             ifelse(pont$SIGNE < 0, "coûté ",
             ifelse(pont$ATTENDU < 0, "pesé ", "rapporté ")),
             euro_exact(pont$ATTENDU), " sur le CA de ", lib_actuel,
             ".
Il fait ", euro_exact(pont$REEL), " (",
             format_points(pont$TAUX_ACT, suffixe = " %"), "), soit ",
             euro_exact(abs(pont$ECART)),
             ifelse(pont$ECART >= 0, " de plus", " de moins"), " : ", gagne,
             eff, " de marge")))

  # « 0 € de plus coûte 0 € » est une phrase que personne ne lit jusqu'au bout.
  # Un poste qui n'a pas bougé mérite qu'on le dise en clair : c'est justement
  # le cas que le pont est là pour rendre visible.
  ifelse(abs(pont$EFFET) < 0.5,
         "ce poste garde le même poids dans le CA : il ne change rien à la marge",
         phrases)
}

# Cascade du pont : on part de la marge de référence, chaque effet la creuse ou
# la remplit, on arrive à la marge de la période. La lecture est immédiate —
# c'est le seul graphe du dashboard qui réponde à « qu'est-ce qui a changé ».
# Le titre porte la période, la référence ET la granularité : l'image doit se
# suffire à elle-même une fois copiée dans un mail ou un compte rendu, où elle
# n'a plus ni sélecteur ni barre latérale autour d'elle pour dire ce qu'elle
# montre. Un graphe sorti de son écran sans son titre est un graphe faux.
graph_pont_marge <- function(pont, lib_actuel = "la période",
                             lib_ref = "la référence", unite = "mois",
                             sous_titre = NULL) {
  if (is.null(pont) || !nrow(pont))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de période de référence")))

  libelles <- c(paste0("Marge ", lib_ref), pont$POSTE,
                paste0("Marge ", lib_actuel))
  valeurs  <- c(pont$DEPART[1], pont$EFFET, pont$ARRIVEE[1])
  mesures  <- c("absolute", rep("relative", nrow(pont)), "total")
  # Le calcul de la barre, dans son infobulle : mot pour mot celui du tableau
  # de décomposition. Deux formulations différentes du même chiffre donneraient
  # l'impression de deux calculs différents.
  calculs <- c("", paste0("<br>", libelle_attendu_pont(pont), " → ",
                          libelle_reel_pont(pont),
                          "<br>écart ", euro_signe(pont$ECART),
                          "<br><i>", lecture_pont(pont, lib_actuel, lib_ref),
                          "</i>"), "")

  # Échelle verticale resserrée : sans elle, une cascade qui va de 18 000 à
  # 21 000 € se lit sur un axe partant de zéro, et les marches deviennent
  # invisibles.
  #
  # Les bornes sont prises sur le CHEMIN CUMULÉ de la cascade — la hauteur
  # atteinte après chaque effet — et non sur les deux ou trois premières
  # marches : le sommet d'un pont n'est pas forcément au début. Un gros effet
  # favorable en cinquième position sortirait sinon du cadre, silencieusement.
  chemin <- pont$DEPART[1] + cumsum(pont$EFFET)
  hauteurs <- c(pont$DEPART[1], chemin, pont$ARRIVEE[1])
  marge_axe <- max(1000, 0.15 * diff(range(c(hauteurs, 0))))
  ymin <- min(hauteurs, 0) - marge_axe
  ymax <- max(hauteurs) + marge_axe

  plot_ly(
    type = "waterfall", orientation = "v",
    x = ~factor(libelles, levels = libelles), y = valeurs, measure = mesures,
    text = format_CA(valeurs, -1), textposition = "outside",
    connector = list(line = list(color = "#d3c0ac")),
    increasing = list(marker = list(color = COUL_VERT)),
    decreasing = list(marker = list(color = COUL_ROUGE)),
    totals = list(marker = list(color = COUL_BRUN)),
    hovertemplate = paste0(
      "<b>", libelles, "</b><br>", format_CA(valeurs, -1), calculs,
      "<extra></extra>")
  ) %>%
    layout(
      title = list(text = titre_pont(lib_actuel, lib_ref, unite, sous_titre),
                   font = list(size = 14), x = 0.02, xanchor = "left",
                   y = 0.95, yanchor = "top"),
      xaxis = list(title = "", tickangle = -25),
      yaxis = list(title = "€", zeroline = TRUE, zerolinecolor = "#8d7b68",
                   range = c(ymin, ymax)),
      # t = 78 : de quoi loger le titre ET sa ligne de contexte sans que la
      # première barre vienne mordre dessus.
      margin = list(b = 110, t = 78), showlegend = FALSE)
}

# Titre du pont, sur deux lignes : ce qu'on compare, puis à quel grain.
titre_pont <- function(lib_actuel, lib_ref, unite = "mois", sous_titre = NULL) {
  grain <- switch(unite, mois = "comparaison mensuelle",
                  trimestre = "comparaison trimestrielle",
                  annee = "comparaison annuelle", "comparaison")
  # contexte <- paste(c(grain, "marge avant amortissements", sous_titre),
  #                   collapse = " · ")
  contexte <- ""
  paste0("Pont de marge — ", lib_actuel, " comparé à ", lib_ref,
         "<br><span style='font-size:11px;color:#8d7b68'>", contexte, "</span>")
}

# Le pont en tableau, colonne par colonne, pour REFAIRE le calcul.
#
# Le graphe montre le résultat, ce tableau montre l'arithmétique. Les quatre
# colonnes du milieu se lisent comme une seule opération :
#
#     Écart  x  Base  =  Effet
#
# l'un des deux facteurs étant toujours un pourcentage. Rien n'est caché, et
# une ligne de total permet de vérifier que la somme retombe sur l'écart.
#
# La colonne « Lecture » dit la même chose en français. Elle fait double emploi
# avec les chiffres, et c'est voulu : la première fois, c'est elle qu'on lit ;
# ensuite, ce sont les colonnes.
table_pont_marge <- function(pont, lib_actuel = "la période",
                             lib_ref = "la référence") {
  if (is.null(pont) || !nrow(pont)) return(tibble(Info = "Pas de référence."))

  corps <- tibble(
    Poste       = pont$POSTE,
    `Attendu`   = libelle_attendu_pont(pont),
    `Réel`      = libelle_reel_pont(pont),
    `Écart`     = euro_signe(pont$ECART),
    `Opération` = libelle_base_pont(pont, lib_ref),
    `= Effet sur la marge` = euro_signe(pont$EFFET),
    Lecture     = lecture_pont(pont, lib_actuel, lib_ref))

  total <- sum(pont$EFFET)
  bind_rows(corps, tibble(
    Poste = "Écart total", `Attendu` = "", `Réel` = "", `Écart` = "",
    `Opération` = "", `= Effet sur la marge` = euro_signe(total),
    Lecture = paste0("somme des ", nrow(pont), " effets ci-dessus")))
}

# La preuve par trois lignes : marge de départ, somme des effets, marge
# d'arrivée. C'est ce qui transforme le pont d'illustration en démonstration —
# on voit que rien ne s'est perdu en route.
table_verification_pont <- function(pont, lib_actuel = "la période",
                                    lib_ref = "la référence") {
  if (is.null(pont) || !nrow(pont)) return(tibble(Info = "Pas de référence."))
  total   <- sum(pont$EFFET)
  arrivee <- pont$ARRIVEE[1]
  # L'écart de bouclage n'est jamais qu'un arrondi d'affichage : l'identité est
  # exacte. On l'affiche quand même — un contrôle qu'on ne montre pas est un
  # contrôle auquel on ne croit pas.
  reste <- pont$DEPART[1] + total - arrivee

  tibble(
    Étape = c(paste0("Marge de ", lib_ref),
              paste0("+ somme des ", nrow(pont), " effets"),
              paste0("= Marge de ", lib_actuel),
              "Écart de bouclage"),
    Montant = c(euro_exact(pont$DEPART[1]), euro_signe(total),
                euro_exact(arrivee),
                if (abs(reste) < 0.5) "0 € — le pont boucle"
                else format_CA(reste, -1)))
}

# Le paragraphe d'accueil du sous-onglet, écrit AVEC LES CHIFFRES DE LA PÉRIODE
# affichée plutôt qu'avec un exemple générique. Une formule illustrée par ses
# propres nombres se comprend du premier coup ; la même formule en lettres se
# relit trois fois.
#
# On détaille deux lignes seulement : l'effet volume, qui est le mécanisme à
# saisir, et le poste au plus fort effet, qui est celui qu'on est venu
# comprendre. Détailler les six noierait les deux qui comptent.
explication_pont <- function(pont, lib_actuel = "la période",
                             lib_ref = "la référence") {
  if (is.null(pont) || !nrow(pont))
    return(div(class = "small text-muted",
               "Choisissez une période de référence pour voir le calcul."))

  phrase <- function(p) lecture_pont(p, lib_actuel, lib_ref)
  vol <- pont[pont$NATURE == "volume", ]
  gros <- pont[pont$NATURE != "volume", ]
  gros <- if (nrow(gros)) gros[which.max(abs(gros$EFFET)), ] else NULL

  div(
    class = "small",
    p(tags$b("Deux sortes de lignes, et deux seulement."),
      " L'écart de marge se range entièrement dans l'une ou l'autre."),
    if (nrow(vol))
      p(tags$b("Le volume."), " Le chiffre d'affaires a bougé. Si rien",
        " d'autre n'avait changé, chaque euro de CA en plus aurait rapporté ce",
        " qu'il rapportait à ", lib_ref, " — sa marge divisée par son CA.",
        tags$br(),
        tags$span(class = "text-muted", "Ici : ", phrase(vol), ".")),
    if (!is.null(gros) && nrow(gros))
      p(tags$b("La dérive d'un poste."), " Un poste a changé de POIDS dans le",
        " CA. Ce sont ces points d'écart, appliqués au CA de ", lib_actuel,
        ", qui font la marge en plus ou en moins.", tags$br(),
        tags$span(class = "text-muted", "Ici, le poste qui pèse le plus est ",
                  tags$b(gros$POSTE), " : ", phrase(gros), ".")),
    p(class = "text-muted mb-0",
      "Un poste dont le poids n'a pas bougé a un effet nul, même si son",
      " montant en euros a suivi le CA. C'est exactement ce qu'on veut lire :",
      " le pont ne montre que ce qui a vraiment changé.")
  )
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
contributions_comptes <- function(db, periode, ref_periodes, unite = "mois",
                                  offsets = NULL) {
  if (is.null(db) || !nrow(db)) return(NULL)
  d <- classe_comptes(db)
  if (is.null(d) || !nrow(d)) return(NULL)

  # Le rabotage s'applique AVANT la consolidation, sur les mois eux-mêmes —
  # sinon il n'y aurait plus de mois à retirer. Il doit suivre exactement celui
  # de la série : si le pont compare huit mois à huit mois et que le forage en
  # compare huit à douze, les contributions ne somment plus à l'écart affiché
  # juste au-dessus, et c'est le forage qu'on croira faux.
  if (!is.null(offsets))
    d <- filter(d, offset_mois(PERIODE, unite) %in% offsets)
  d <- consolide_periode(d, unite)
  if (!nrow(d)) return(NULL)

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
    # kpi_tile(etiquette_periode(a$PERIODE, unite), "Période analysée",
    #          COUL_BRUN, "calendar-day", sous_titre = paste("comparée à", lib_ref)),
    tuile_eur("CA", "Écart de chiffre d'affaires", "euro-sign", 1),
    tuile_eur("MARGE_AA", "Écart de marge", "piggy-bank", 1),
    tuile_pt("PCT_MARGE_AA", "Écart de taux de marge", "percent", 1),
    tuile_pt("PCT_MATIERES", "Écart taux matières", "cart-shopping", -1),
    tuile_pt("PCT_TRAVAIL", "Écart taux rémunérations", "users", -1),
    tuile_pt("PCT_FRAIS_GENERAUX", "Écart frais généraux", "file-contract", -1)
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
