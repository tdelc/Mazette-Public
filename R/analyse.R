# Volets « Écarts », « Évolution » et « Contrôle » de la comptabilité : le
# POURQUOI.
#
# Rien n'y est recalculé : on part de postes_exploitation() et de
# classe_comptes() et on les confronte — à une période de référence, à leur
# propre histoire, aux années précédentes. Un écart n'est jamais affiché sans
# dire à quoi il est comparé.

##### Indicateurs suivis #####

# Les grandeurs que la tendance, la saisonnalité et les tuiles savent tracer :
# au moins tout ce que le pont décompose, plus les marges et les ratios.
# SENS : +1 si une hausse est une bonne nouvelle, -1 sinon. UNITE : euros ou
# points de pourcentage.
#
# Registre par défaut (marge avant amortissements) ; indicateurs_analyse() en
# dérive la variante après amortissements.
INDICATEURS_ANALYSE <- tibble::tribble(
  ~CLE,            ~LIBELLE,                      ~COLONNE,       ~UNITE, ~SENS,
  "ca",            "Chiffre d'affaires",          "CA",           "eur",      1,
  "autres",        "Autres produits",             "AUTRES",       "eur",      1,
  "marge",         "Marge avant amortissements",  "MARGE_AA",     "eur",      1,
  "matieres",      "Matières premières",          "MATIERES",     "eur",     -1,
  "remunerations", "Rémunérations",               "REMUNERATION", "eur",     -1,
  "generaux",      "Frais généraux",              "GENERAUX",     "eur",     -1,
  "financier",     "Résultat financier",          "FINANCIER",    "eur",      1,
  "pct_matieres",  "Matières en % du CA",         "PCT_MATIERES", "pct",     -1,
  "pct_travail",   "Rémunérations en % du CA",    "PCT_TRAVAIL",  "pct",     -1,
  "pct_prime",     "Matières + rémunérations en % du CA", "PCT_PRIME", "pct", -1,
  "pct_generaux",  "Frais généraux en % du CA",   "PCT_GENERAUX", "pct",     -1,
  "pct_marge",     "Marge en % du CA",            "PCT_MARGE_AA", "pct",      1
)

# Le registre pour une marge donnée. Les clés ne changent pas (« marge »
# reste « marge ») ; après amortissements, l'amortissement devient en plus un
# indicateur, puisque le pont en fait une ligne.
indicateurs_analyse <- function(marge = "avant") {
  m <- marge_gestion(marge)
  if (!m$APRES) return(INDICATEURS_ANALYSE)
  r <- INDICATEURS_ANALYSE
  r$COLONNE[r$CLE == "marge"]     <- m$COLONNE
  r$LIBELLE[r$CLE == "marge"]     <- m$LIBELLE
  r$COLONNE[r$CLE == "pct_marge"] <- m$PCT
  r$LIBELLE[r$CLE == "pct_marge"] <- "Marge après amort. en % du CA"
  i <- match("generaux", r$CLE)
  bind_rows(r[seq_len(i), ],
            tibble(CLE = "amortissements", LIBELLE = "Amortissements",
                   COLONNE = "AMORTISSEMENT", UNITE = "eur", SENS = -1),
            r[-seq_len(i), ])
}

# Ligne du registre, avec repli sur la marge (pour un select vidé).
indicateur_analyse <- function(cle, marge = "avant") {
  reg <- indicateurs_analyse(marge)
  i <- match(cle %||% "", reg$CLE)
  if (is.na(i)) i <- match("marge", reg$CLE)
  as.list(reg[i, ])
}

# La même, SANS repli : pour une clé écrite dans le code, une tuile en
# erreur vaut mieux qu'une tuile qui affiche autre chose.
indicateur_strict <- function(cle, marge = "avant") {
  reg <- indicateurs_analyse(marge)
  i <- match(cle %||% "", reg$CLE)
  if (is.na(i)) NULL else as.list(reg[i, ])
}

# Formatage d'une valeur selon l'unité de son indicateur.
format_indicateur <- function(x, unite) {
  if (identical(unite, "pct")) format_pct(x) else format_CA(x, -1)
}

##### Comparer une période partielle #####

# Une période en cours (huit mois de 2026) ne se compare pas à une période
# entière (douze mois de 2025). On RABOTE donc la référence aux mêmes mois :
# même rang dans leur période (janvier à août, ou sans mars si mars manque).
# La saisonnalité est respectée ; rien n'est inventé.

# Rang d'un mois dans sa période : 0 pour janvier d'une année, 2 pour le
# troisième mois d'un trimestre.
offset_mois <- function(mois, unite) {
  mois <- as.Date(mois)
  debut <- switch(unite,
                  mois      = mois,
                  trimestre = floor_date(mois, "quarter"),
                  annee     = floor_date(mois, "year"))
  # Entier explicite : un rang en double se compare de travers.
  as.integer(12L * (year(mois) - year(debut)) + (month(mois) - month(debut)))
}

# Nombre de mois qu'une période complète devrait porter.
mois_attendus <- function(unite) switch(unite, mois = 1L, trimestre = 3L,
                                        annee = 12L)

# Les rangs présents dans la période analysée : le gabarit des autres.
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

# La série agrégée en ne gardant, dans chaque période, que les mois du
# gabarit. Sur une période complète, rien n'est retiré.
serie_rabotee <- function(postes_mensuels, unite, offsets) {
  if (is.null(postes_mensuels) || !nrow(postes_mensuels))
    return(agrege_exploitation(postes_mensuels, unite))
  postes_mensuels %>%
    filter(offset_mois(PERIODE, unite) %in% offsets) %>%
    agrege_exploitation(unite)
}

# Comment nommer une référence rabotée : « janv.–août » sur une année,
# « 8 mois sur 12 » sinon.
etiquette_rabotage <- function(offsets, unite) {
  n <- length(offsets); attendu <- mois_attendus(unite)
  if (n >= attendu) return(NULL)
  contigu <- identical(as.integer(offsets), seq_len(n) - 1L)
  if (identical(unite, "annee") && contigu)
    paste0(vecteur_mois_court[1], "–", vecteur_mois_court[n])
  else paste0(n, " mois sur ", attendu)
}

##### Choix de la période de référence #####

# Trois façons de se comparer :
#
#   precedente  la période d'avant           -> « est-ce que ça bouge ? »
#   an_dernier  la même période, un an avant -> « est-ce que ça progresse ? »
#   habituelle  la médiane des N précédentes -> « est-ce que c'est normal ? »
MODES_REFERENCE <- c("Période précédente"       = "precedente",
                     "Même période, un an plus tôt" = "an_dernier",
                     "Médiane des périodes précédentes" = "habituelle")

# Une période un an plus tôt. %m-% et non - 365 : il faut retomber
# exactement sur le premier jour de la période, années bissextiles comprises.
periode_an_avant <- function(p, unite = "mois") {
  as.Date(p) %m-% months(12)
}

# La ligne de référence (mêmes colonnes que `serie`) et son libellé. NULL
# quand elle n'existe pas : un écart contre zéro serait faux.
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
    # Au moins trois périodes pour une médiane.
    ref <- tail(passe, n_habituelle)
    if (nrow(ref) < 3) return(NULL)
    if (!"MARGE_PA" %in% names(ref))
      ref <- mutate(ref, MARGE_PA = MARGE_AA - AMORTISSEMENT)
    # Médiane colonne par colonne ; les pourcentages sont recalculés à partir
    # des médianes d'euros.
    med <- ref %>%
      summarise(across(c(CA, AUTRES, PRODUITS, MATIERES, REMUNERATION,
                         GENERAUX, AMORTISSEMENT, FINANCIER, CHARGES,
                         MARGE, MARGE_AA, MARGE_PA),
                       ~stats::median(., na.rm = TRUE))) %>%
      mutate(PERIODE = max(ref$PERIODE), N_MOIS = NA_integer_,
             PCT_MATIERES = ratio_pct(MATIERES, CA),
             PCT_TRAVAIL  = ratio_pct(REMUNERATION, CA),
             PCT_PRIME    = ratio_pct(MATIERES + REMUNERATION, CA),
             PCT_GENERAUX = ratio_pct(GENERAUX, CA),
             PCT_MARGE    = ratio_pct(MARGE, CA),
             PCT_MARGE_AA = ratio_pct(MARGE_AA, CA),
             PCT_MARGE_PA = ratio_pct(MARGE_PA, CA))
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
# On sépare le VOLUME des TAUX : quand le CA baisse, toutes les charges
# baissent, et seule la dérive des poids dit si la gestion a bougé.
#
#   EFFET VOLUME      écart de CA × taux de marge de la référence
#   EFFET D'UN POSTE  ce qu'il a coûté − ce qu'il aurait coûté en gardant son
#                     poids de référence sur le CA de la période (signe opposé
#                     pour une charge)
#
# La somme retombe exactement sur la variation de marge. Avec s = marge / CA :
#
#   Δmarge = CA1.s1 − CA0.s0 = (CA1 − CA0).s0 + CA1.(s1 − s0)
#
# le premier terme est l'effet volume, le second se répartit entre les postes
# puisque s = 1 + a − m − r − g + f (chaque poste rapporté au CA). Un test le
# vérifie à l'euro. Après amortissements, l'amortissement est un poste de plus
# (signe −1). Sans CA de référence, on retombe sur la décomposition brute.
#
# Colonnes, pour refaire le calcul (sous-onglet « Décomposition du pont ») :
#
#   NATURE    "volume", "taux" ou "brut"
#   ATTENDU   poste : coût au poids de référence ; volume : CA de référence
#   REEL      le montant de la période (ou son CA)
#   ECART     REEL − ATTENDU, en euros
#   BASE      taux de marge de référence, ligne volume seulement
#   TAUX_REF  poids du poste dans le CA, à la référence et à la période —
#   TAUX_ACT  pour la phrase de lecture seulement
#
# Invariant : EFFET = SIGNE × ECART, sauf volume où EFFET = ECART × BASE / 100.
pont_marge <- function(actuel, reference, marge = "avant") {
  if (is.null(actuel) || !nrow(actuel) || is.null(reference) || !nrow(reference))
    return(NULL)
  m <- marge_gestion(marge)
  a <- as.list(actuel[1, ]); r <- as.list(reference[1, ])
  # La marge de chaque côté : la colonne, ou à défaut l'identité
  # MARGE_PA = MARGE_AA − AMORTISSEMENT.
  la_marge <- function(l) {
    v <- l[[m$COLONNE]]
    if (is.null(v)) v <- l$MARGE_AA - if (m$APRES) l$AMORTISSEMENT else 0
    as.numeric(v)
  }
  m_ref <- la_marge(r); m_act <- la_marge(a)

  if (is.na(r$CA) || r$CA <= 0) {
    postes <- c("Chiffre d'affaires", "Autres produits", "Matières premières",
                "Rémunérations", "Frais généraux",
                if (m$APRES) "Amortissements", "Résultat financier")
    champs <- c("CA", "AUTRES", "MATIERES", "REMUNERATION", "GENERAUX",
                if (m$APRES) "AMORTISSEMENT", "FINANCIER")
    signe  <- c(1, 1, -1, -1, -1, if (m$APRES) -1, 1)
    ref <- vapply(champs, function(x) as.numeric(r[[x]]), 0)
    act <- vapply(champs, function(x) as.numeric(a[[x]]), 0)
    return(tibble(
      POSTE = postes, NATURE = "brut",
      ATTENDU = ref, REEL = act, ECART = act - ref,
      BASE = NA_real_, SIGNE = signe,
      TAUX_REF = NA_real_, TAUX_ACT = NA_real_,
      EFFET = signe * (act - ref),
      DEPART = m_ref, ARRIVEE = m_act, VOLUME_SEPARE = FALSE))
  }

  # Taux de marge de la référence : sa marge divisée par son CA.
  s0_pct <- 100 * m_ref / r$CA
  taux <- function(champ, l) 100 * l[[champ]] / l$CA

  champs <- c("AUTRES", "MATIERES", "REMUNERATION", "GENERAUX",
              if (m$APRES) "AMORTISSEMENT", "FINANCIER")
  postes <- c("Autres produits", "Matières premières", "Rémunérations",
              "Frais généraux", if (m$APRES) "Amortissements",
              "Résultat financier")
  signe  <- c(1, -1, -1, -1, if (m$APRES) -1, 1)
  t_ref  <- vapply(champs, taux, 0, l = r)
  t_act  <- vapply(champs, taux, 0, l = a)

  # Ce que chaque poste aurait coûté (ou rapporté) au poids de référence.
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
    mutate(DEPART = m_ref, ARRIVEE = m_act, VOLUME_SEPARE = TRUE)
}


# Un poids dans le CA, en pourcentage à une décimale (scientific = FALSE,
# comme dans format_CA()).
format_points <- function(x, signe = FALSE, suffixe = " pt") {
  txt <- format(round(x, 1), nsmall = 1, trim = TRUE, decimal.mark = ",",
                scientific = FALSE)
  paste0(if (signe) ifelse(x >= 0, "+", "") else "", txt, suffixe)
}

# Ce qu'on compare sur une ligne du pont ; partagé par le tableau et
# l'infobulle.
libelle_attendu_pont <- function(pont) {
  euro_exact(pont$ATTENDU)
}

libelle_reel_pont <- function(pont) {
  euro_exact(pont$REEL)
}

# La phrase de lecture d'une ligne, celle qu'on se dirait en refaisant le
# calcul.
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
      # « à la référence » plutôt que son libellé, qui peut être long.
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

  # Un poste qui n'a pas bougé : on le dit en clair.
  ifelse(abs(pont$EFFET) < 0.5,
         "ce poste garde le même poids dans le CA : il ne change rien à la marge",
         phrases)
}

# Cascade du pont : de la marge de référence à celle de la période. Le titre
# porte la période et la référence : l'image doit se suffire une fois copiée.
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
  # L'infobulle reprend mot pour mot le tableau de décomposition.
  calculs <- c("", paste0("<br>", libelle_attendu_pont(pont), " → ",
                          libelle_reel_pont(pont),
                          "<br>écart ", euro_signe(pont$ECART),
                          "<br><i>", lecture_pont(pont, lib_actuel, lib_ref),
                          "</i>"), "")

  # Échelle resserrée, bornée sur le chemin cumulé de la cascade (le sommet
  # n'est pas forcément au début).
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
      # t = 78 : de la place pour le titre sur deux lignes.
      margin = list(b = 110, t = 78), showlegend = FALSE)
}

# Titre du pont.
titre_pont <- function(lib_actuel, lib_ref, unite = "mois", sous_titre = NULL) {
  grain <- switch(unite, mois = "comparaison mensuelle",
                  trimestre = "comparaison trimestrielle",
                  annee = "comparaison annuelle", "comparaison")
  contexte <- ""
  paste0("Pont de marge — ", lib_actuel, " comparé à ", lib_ref,
         "<br><span style='font-size:11px;color:#8d7b68'>", contexte, "</span>")
}

# Le pont en tableau, pour refaire le calcul ; une ligne de total vérifie
# que la somme retombe sur l'écart. La colonne « Lecture » dit la même chose
# en français.
table_pont_marge <- function(pont, lib_actuel = "la période",
                             lib_ref = "la référence") {
  if (is.null(pont) || !nrow(pont)) return(tibble(Info = "Pas de référence."))

  corps <- tibble(
    Poste       = pont$POSTE,
    `Attendu`   = libelle_attendu_pont(pont),
    `Réel`      = libelle_reel_pont(pont),
    `Écart`     = euro_signe(pont$ECART),
    `Effet sur la marge` = euro_signe(pont$EFFET),
    Lecture     = lecture_pont(pont, lib_actuel, lib_ref))

  total <- sum(pont$EFFET)
  bind_rows(corps, tibble(
    Poste = "Écart total", `Attendu` = "", `Réel` = "", `Écart` = "",
    `Effet sur la marge` = euro_signe(total),
    Lecture = paste0("somme des ", nrow(pont), " effets ci-dessus")))
}

# Le paragraphe du sous-onglet, avec les chiffres de la période : l'effet
# volume et le poste au plus fort effet.
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
    p(tags$b("Deux sortes de lignes :")),
    if (nrow(vol))
      p(tags$b("Le volume."), " Le chiffre d'affaires a bougé. Si rien",
        " d'autre n'avait changé, chaque euro de CA en plus aurait rapporté ce",
        " qu'il rapportait à ", lib_ref, " — sa marge divisée par son CA.",
        tags$br(),
        tags$span(class = "text-muted", "Ici : ", phrase(vol), ".")),
    if (!is.null(gros) && nrow(gros))
      p(tags$b("La dérive d'un poste."), " Un poste a changé de poids dans le",
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

# Ce que chaque COMPTE a fait bouger entre deux périodes : le forage sous le
# pont. Le signe est celui de l'effet sur la marge (un achat en hausse est
# négatif). Les comptes d'amortissement n'y entrent qu'après amortissements.
contributions_comptes <- function(db, periode, ref_periodes, unite = "mois",
                                  offsets = NULL, marge = "avant") {
  if (is.null(db) || !nrow(db)) return(NULL)
  d <- classe_comptes(db)
  if (is.null(d) || !nrow(d)) return(NULL)
  if (!marge_gestion(marge)$APRES) d <- filter(d, POSTE != "Amortissements")

  # Le rabotage avant la consolidation, comme pour la série : sinon les
  # contributions ne sommeraient plus à l'écart du pont.
  if (!is.null(offsets))
    d <- filter(d, offset_mois(PERIODE, unite) %in% offsets)
  d <- consolide_periode(d, unite)
  if (!nrow(d)) return(NULL)

  periode <- as.Date(periode); ref_periodes <- as.Date(ref_periodes)
  if (!length(ref_periodes)) return(NULL)

  act <- d %>% filter(PERIODE == periode) %>%
    group_by(COMPTE, LIBELLE, POSTE, SENS_G) %>%
    summarise(V_ACT = sum(VALEUR, na.rm = TRUE), .groups = "drop")

  # Plusieurs périodes de référence : leur moyenne.
  ref <- d %>% filter(PERIODE %in% ref_periodes) %>%
    group_by(COMPTE, LIBELLE, POSTE, SENS_G, PERIODE) %>%
    summarise(V = sum(VALEUR, na.rm = TRUE), .groups = "drop") %>%
    group_by(COMPTE, LIBELLE, POSTE, SENS_G) %>%
    summarise(V_REF = sum(V) / length(ref_periodes), .groups = "drop")

  full_join(act, ref, by = c("COMPTE", "LIBELLE", "POSTE", "SENS_G")) %>%
    mutate(across(c(V_ACT, V_REF), ~replace_na(., 0)),
           # SENS_G (+1 produit, -1 charge) convertit une variation en effet sur la
           # marge.
           EFFET = replace_na(SENS_G, 0) * (V_ACT - V_REF),
           VARIATION_PCT = ratio_pct(V_ACT - V_REF, V_REF),
           NOUVEAU = V_REF == 0 & V_ACT != 0,
           DISPARU = V_ACT == 0 & V_REF != 0) %>%
    filter(EFFET != 0) %>%
    arrange(desc(abs(EFFET)))
}

# Les n comptes qui pèsent le plus, dans les deux sens.
graph_contributions <- function(contrib, n = 12) {
  if (is.null(contrib) || !nrow(contrib))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucun mouvement à expliquer")))

  d <- head(contrib, n) %>% arrange(EFFET)
  # Le numéro de compte reste visible après troncature.
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

# La série d'un indicateur, avec sa médiane et sa bande à deux MAD : le même
# repérage que le tableau d'exploitation (cf. flag_ecart).
serie_indicateur <- function(serie, cle, marge = "avant") {
  ind <- indicateur_analyse(cle, marge)
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

graph_tendance <- function(st, cle, unite = "mois", marge = "avant") {
  ind <- indicateur_analyse(cle, marge)
  if (is.null(st) || !nrow(st))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas de série à tracer")))

  lab <- etiquette_periode(st$PERIODE, unite)
  ordre <- factor(lab, levels = lab)
  fmt <- function(x) format_indicateur(x, ind$UNITE)

  # Vert favorable, rouge défavorable, brun sinon.
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

# Le même indicateur, mois par mois, une courbe par année : « février est-il
# toujours mauvais ? ». Toujours sur les postes mensuels.
saisonnalite <- function(postes_mensuels, cle, marge = "avant") {
  ind <- indicateur_analyse(cle, marge)
  if (is.null(postes_mensuels) || !nrow(postes_mensuels) ||
      !ind$COLONNE %in% names(postes_mensuels)) return(NULL)
  postes_mensuels %>%
    transmute(ANNEE = year(PERIODE), MOIS = month(PERIODE),
              VALEUR = .data[[ind$COLONNE]]) %>%
    filter(!is.na(VALEUR)) %>%
    arrange(ANNEE, MOIS)
}

graph_saisonnalite <- function(sais, cle, marge = "avant") {
  ind <- indicateur_analyse(cle, marge)
  if (is.null(sais) || !nrow(sais))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas assez d'historique mensuel")))

  annees <- sort(unique(sais$ANNEE))
  # L'année la plus récente en brun soutenu, les autres en dégradé pâle.
  pal <- setNames(
    c(rep(NA, max(0, length(annees) - 1)), COUL_BRUN)[seq_along(annees)],
    annees)
  anciennes <- head(annees, -1)
  if (length(anciennes))
    pal[as.character(anciennes)] <- grDevices::colorRampPalette(
      c("#e6d9c8", "#c0a183"))(length(anciennes))

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

# Le tableau qui accompagne le pont : période, référence, écart, écart
# relatif. Les pourcentages de structure s'écartent en POINTS (30 % -> 33 %,
# c'est « +3 pt »).
table_comparaison <- function(actuel, reference, lib_actuel, lib_ref,
                              marge = "avant") {
  m <- marge_gestion(marge)
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
    if (m$APRES)
      ligne("Amortissements",             "AMORTISSEMENT", "eur", -1),
    ligne("Résultat financier",           "FINANCIER",     "eur",  1),
    ligne(m$LIBELLE,                      m$COLONNE,       "eur",  1),
    ligne("Marge en % du CA",             m$PCT,           "pct",  1)
  ) %>% rename(!!paste0("Période\n", lib_actuel) := `Période`,
               !!paste0("Référence\n", lib_ref)  := `Référence`)
}

##### Tuiles #####

# Les tuiles, toutes en ÉCART ; le niveau se lit dans le sous-titre. Chacune
# est désignée par sa clé dans le registre d'indicateurs.
kpi_analyse <- function(actuel, reference, lib_ref, unite = "mois",
                        marge = "avant") {
  if (is.null(actuel) || !nrow(actuel))
    return(div(class = "text-muted small p-2", "Aucune période sélectionnée."))
  a <- actuel[1, ]
  m <- marge_gestion(marge)

  if (is.null(reference) || !nrow(reference))
    return(div(
      class = "kpi-grid",
      kpi_tile(format_CA(a$CA, -1), paste("CA —", etiquette_periode(a$PERIODE, unite)),
               COUL_BRUN, "euro-sign", sous_titre = "pas de référence"),
      kpi_tile(format_CA(a[[m$COLONNE]], -1), m$LIBELLE,
               if (a[[m$COLONNE]] >= 0) COUL_VERT else COUL_ROUGE, "piggy-bank",
               sous_titre = format_pct(a[[m$PCT]]))))
  r <- reference[1, ]

  # Une tuile d'écart, désignée par sa CLÉ et jamais par un nom de colonne :
  # les noms ne se devinent pas (REMUNERATION -> PCT_TRAVAIL), et l'unité comme
  # le sens viennent du registre. Une clé inconnue donne une tuile grise, pas
  # une rangée de tuiles en erreur.
  tuile_ecart <- function(cle, lib, icone) {
    ind <- indicateur_strict(cle, marge)
    if (is.null(ind) || !ind$COLONNE %in% names(a) || !ind$COLONNE %in% names(r))
      return(tuile_indisponible(cle, lib, icone))

    champ <- ind$COLONNE
    va <- a[[champ]]; vr <- r[[champ]]
    e <- va - vr
    if (!length(e)) return(tuile_indisponible(cle, lib, icone))

    couleur <- if (is.na(e) || e == 0) COUL_NEUTRE
               else if (sign(e) == ind$SENS) COUL_VERT else COUL_ROUGE

    if (identical(ind$UNITE, "pct"))
      return(kpi_tile(
        paste0(if (!is.na(e) && e >= 0) "+" else "", round(e, 1), " pt"),
        lib, couleur, icone,
        sous_titre = paste0(format_pct(va), " vs ", format_pct(vr))))

    pct <- if (is.na(vr) || vr == 0) NA_real_ else 100 * e / abs(vr)
    kpi_tile(
      paste0(if (!is.na(e) && e >= 0) "+" else "", format_CA(e, -1)),
      lib, couleur, icone,
      sous_titre = paste0(format_CA(va, -1), " vs ", format_CA(vr, -1),
                          if (!is.na(pct)) paste0(" · ", if (pct >= 0) "+" else "",
                                                  format_pct(pct)) else ""))
  }

  div(
    class = "kpi-grid",
    tuile_ecart("ca",           "Écart de chiffre d'affaires", "euro-sign"),
    tuile_ecart("marge",        if (m$APRES) "Écart de marge après amort."
                                else "Écart de marge",         "piggy-bank"),
    tuile_ecart("pct_marge",    "Écart de taux de marge",      "percent"),
    tuile_ecart("pct_matieres", "Écart taux matières",         "cart-shopping"),
    tuile_ecart("pct_travail",  "Écart taux rémunérations",    "users"),
    tuile_ecart("pct_generaux", "Écart frais généraux",        "file-contract")
  )
}

# La tuile de repli : grise, et elle dit ce qui manque.
tuile_indisponible <- function(cle, lib, icone) {
  kpi_tile("—", lib, COUL_NEUTRE, icone,
           sous_titre = paste0("indicateur « ", cle %||% "", " » inconnu"))
}

# ##### Les alertes d'intégrité (onglet Contrôle) #####

# La qualité des DONNÉES, sur tout l'historique. Trois statuts :
#
#   ok        rien à faire
#   attention à regarder, pas forcément un problème
#   alerte    quelque chose ne va pas, et on sait quoi
#
# La qualité de la GESTION (ratios, marge) se lit sur les tuiles
# d'Exploitation, pas ici.

# Combien de mois on nomme avant de compter le reste.
MAX_MOIS_NOMMES <- 6

liste_mois <- function(d, max = MAX_MOIS_NOMMES) {
  d <- sort(unique(as.Date(d)), decreasing = TRUE)
  if (!length(d)) return("")
  noms <- etiquette_periode(head(d, max), "mois")
  if (length(d) <= max) return(paste(noms, collapse = ", "))
  paste0(paste(noms, collapse = ", "), ", et ", length(d) - max,
         " autre", if (length(d) - max > 1) "s" else "")
}

#' Toutes les alertes d'intégrité : une ligne par contrôle, pas par mois.
#'
#' @param db la comptabilité retenue, au grain ANNEE x MOIS.
#' @param postes sortie de postes_exploitation(db), au grain MENSUEL.
#' @param improb sortie de periodes_improbables() sur cette même série.
alertes_integrite <- function(db, postes, improb = NULL,
                              mois_en_cours = floor_date(today(), "month")) {
  res <- list()
  # FORAGE : le filtre qui mène aux écritures en cause ; NA si l'alerte ne
  # désigne rien de précis.
  ajoute <- function(titre, statut, message, forage = NA_character_)
    res[[length(res) + 1]] <<- tibble(TITRE = titre, STATUT = statut,
                                      MESSAGE = message, FORAGE = forage)

  ##### 1. Le découpage en postes retombe-t-il sur le solde publié ? #####
  ctrl <- try(controle_exploitation(db, postes), silent = TRUE)
  if (inherits(ctrl, "try-error") || is.null(ctrl) || !nrow(ctrl)) {
    # Une source sans ligne de solde (export Odoo) ne peut pas être confrontée :
    # ce n'est pas un échec.
    ajoute("Classement en postes", "ok",
           paste0("La source ne publie pas de solde d'exploitation : rien à ",
                  "confronter. Le contrôle vaut pour l'ancien export du ",
                  "comptable."))
  } else {
    faux <- ctrl$PERIODE[abs(ctrl$ECART) > 1]
    ajoute("Classement en postes", if (!length(faux)) "ok" else "alerte",
           if (!length(faux))
             paste0("Sur ", nrow(ctrl), " mois, la marge reconstruite retombe ",
                    "exactement sur le résultat d'exploitation publié.")
           else paste0(length(faux), " mois sur ", nrow(ctrl),
                       " où la marge reconstruite diffère du solde comptable ",
                       "de plus d'un euro — un compte échappe au classement : ",
                       liste_mois(faux), ". Écart maximum ",
                       euro_compta(max(abs(ctrl$ECART), na.rm = TRUE)), "."),
           if (length(faux)) forage_mois(max(faux)) else NA_character_)
  }

  ##### 2. Des comptes que le plan ne sait pas ranger ? #####
  nc <- try(comptes_non_classes(db), silent = TRUE)
  if (!inherits(nc, "try-error")) {
    ajoute("Comptes hors plan", if (!nrow(nc)) "ok" else "alerte",
           if (!nrow(nc))
             "Tous les comptes sont rattachés à un poste par leur numéro."
           else paste0(nrow(nc), " compte(s) n'entrent dans aucun poste, pour ",
                       euro_compta(sum(abs(nc$TOTAL))), " au total. Le premier ",
                       "est ", nc$COMPTE[1], " ", tronque_nom(nc$LIBELLE[1], 40),
                       " — le tableau ci-dessous les liste tous."),
           if (nrow(nc)) forage_spec(texte = nc$COMPTE[1]) else NA_character_)
  }

  ##### 3. Les mois dont les chiffres ne sont pas croyables #####
  if (!is.null(improb) && nrow(improb)) {
    faux <- unique(improb$PERIODE[improb$MOTIF != "en_cours"])
    if (length(faux))
      ajoute("Mois invraisemblables", "alerte",
             paste0(length(faux), " mois portent des chiffres improbables : ",
                    liste_mois(faux), ". Le tableau ci-dessous dit pourquoi, ",
                    "mois par mois, et permet de les écarter."),
             forage_mois(max(faux)))
    else
      ajoute("Mois invraisemblables", "ok",
             "Aucun mois clos ne porte de chiffres improbables.")
  }

  # ##### 4. Les mois qui s'écartent de leur propre série #####
  if (!is.null(postes) && nrow(postes) >= 5) {
    suspects <- list()
    for (i in seq_len(nrow(INDICATEURS_ANALYSE))) {
      ind <- as.list(INDICATEURS_ANALYSE[i, ])
      if (ind$UNITE != "eur" || !ind$COLONNE %in% names(postes)) next
      f <- flag_ecart(postes[[ind$COLONNE]], sens = ind$SENS)
      k <- which(f == -1L)
      if (length(k)) suspects[[ind$LIBELLE]] <- postes$PERIODE[k]
    }
    mois <- unique(unlist(lapply(suspects, as.character)))
    ajoute("Écarts statistiques", if (!length(mois)) "ok" else "attention",
           if (!length(mois))
             paste0("Aucun mois ne s'écarte de plus de deux MAD de sa propre ",
                    "série (", nrow(postes), " mois).")
           else paste0(length(mois), " mois portent un écart défavorable de ",
                       "plus de deux MAD : ", liste_mois(mois),
                       ". À vérifier en comptabilité avant d'y lire un fait ",
                       "de gestion (", paste(names(suspects), collapse = ", "),
                       ")."),
           if (length(mois)) forage_mois(max(as.Date(mois))) else NA_character_)
  }

  ##### 5. Le dernier mois est-il clos ? #####
  if (!is.null(postes) && nrow(postes)) {
    derniere <- max(postes$PERIODE)
    en_cours <- derniere >= as.Date(mois_en_cours)
    ajoute("Dernier mois", if (en_cours) "attention" else "ok",
           if (en_cours)
             paste0(etiquette_periode(derniere, "mois"), " n'est pas terminé : ",
                    "ses totaux ne sont comparables à aucun mois clos.")
           else paste0("Le dernier mois de la comptabilité, ",
                       etiquette_periode(derniere, "mois"), ", est clos."),
           forage_mois(derniere))
  }

  bind_rows(res)
}

#' Le compteur d'alertes du titre de l'onglet Contrôle : rien quand tout est
#' vert.
badge_alertes <- function(diag) {
  if (is.null(diag) || !nrow(diag)) return(NULL)
  n_alerte <- sum(diag$STATUT == "alerte")
  n_attn   <- sum(diag$STATUT == "attention")
  n <- n_alerte + n_attn
  if (!n) return(NULL)
  span(class = "badge-onglet",
       style = paste0("background:", if (n_alerte) COUL_ROUGE else COUL_AMBRE),
       n)
}

# Rendu du diagnostic : une liste à lire, pas un tableau à trier.
rendu_diagnostic <- function(diag) {
  if (is.null(diag) || !nrow(diag))
    return(div(class = "text-muted small p-2", "Aucun contrôle disponible."))

  couleur <- c(ok = COUL_VERT, attention = COUL_AMBRE, alerte = COUL_ROUGE)
  icone   <- c(ok = "circle-check", attention = "circle-exclamation",
               alerte = "triangle-exclamation")
  # Les alertes d'abord.
  diag <- diag %>% mutate(RANG = match(STATUT, c("alerte", "attention", "ok"))) %>%
    arrange(RANG)
  # Colonne FORAGE facultative : un lien vers les écritures en cause.
  forages <- if ("FORAGE" %in% names(diag)) diag$FORAGE
             else rep(NA_character_, nrow(diag))

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
          div(style = paste0("font-weight:600;color:", coul, ";"),
              diag$TITRE[i]),
          div(class = "small", diag$MESSAGE[i]),
          if (!is.na(forages[i]) && nzchar(forages[i]))
            div(class = "small mt-1",
                lien_forage("Voir les écritures", forages[i]))))
    }))
}

# Compteur de synthèse, pour l'en-tête de la carte.
resume_diagnostic <- function(diag) {
  if (is.null(diag) || !nrow(diag)) return("")
  n <- table(factor(diag$STATUT, levels = c("alerte", "attention", "ok")))
  paste0(n[["ok"]], " au vert · ", n[["attention"]], " à regarder · ",
         n[["alerte"]], " en alerte")
}
