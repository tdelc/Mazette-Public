# La fin de fût : ce qu'on en prédit, et ce que ça vaut.
#
# ---------------------------------------------------------------------------
# Le modèle historique, et ce qu'on peut lui reprocher
# ---------------------------------------------------------------------------
# table_evo_brassin() (R/brassins.R) ajuste un HoltWinters sur la série du
# VOLUME RESTANT, puis cherche le jour où la courbe prévue croise zéro. Quatre
# choses s'y jouent mal, et aucune n'est une question de réglage :
#
#   1. il modélise un NIVEAU là où le phénomène est un DÉBIT. Le volume
#      restant est monotone décroissant et borné à zéro ; sa « saisonnalité
#      hebdomadaire » n'est qu'un reflet de celle des ventes, estimée au
#      deuxième degré. Modéliser les litres du jour, puis les cumuler, dit la
#      même chose en une étape de moins et sans supposer la stationnarité de
#      quoi que ce soit ;
#
#   2. il ne connaît pas le calendrier. Cent litres restants un jeudi, avec
#      vendredi et samedi devant, ne durent pas autant que cent litres un
#      dimanche. Le HoltWinters le devine à moitié, sur sept observations de
#      niveau, quand le profil de semaine se mesure sur des mois de ventes de
#      TOUTES les bières ;
#
#   3. il ignore la borne. Rien n'empêche la courbe prévue de remonter, et
#      l'intervalle est gaussien symétrique autour d'un niveau qui ne peut pas
#      passer sous zéro — justement là où la réponse compte ;
#
#   4. il part d'un volume vendable faux. VOLUME_BRASSIN_AJUST vaut
#      VOLUME_BRASSIN x 0,75 (import.R), un abattement de 25 % identique pour
#      tous les brassins, jamais confronté au réel. Or le réel est là : pour
#      chaque fût terminé, le volume réellement vendu DIVISÉ par le volume
#      brassé EST le rendement. Une erreur de 5 points sur un fût de 400 L,
#      c'est 20 L, soit presque un jour de vente.
#
#      Et l'abattement ne fait pas qu'introduire un biais : si la perte réelle
#      est plus faible que 25 %, le volume restant devient NÉGATIF avant la
#      vraie fin. Le `filter(HI_75 > 0)` de table_evo_brassin() vide alors la
#      table, et le modèle ne répond plus du tout — dans les derniers jours,
#      c'est-à-dire là où on le regarde ;
#
#   5. il lui faut sept jours pour ouvrir la bouche. Sur un fût qui tient deux
#      semaines, c'est la moitié de sa vie sans aucune prédiction — et la
#      moitié pendant laquelle on a encore le temps de commander. Entre 7 et
#      13 jours, il bascule de surcroît sur une « saisonnalité » de trois
#      jours, qui ne correspond à rien.
#
# ---------------------------------------------------------------------------
# Ce qu'on met à la place
# ---------------------------------------------------------------------------
# Trois ingrédients, tous estimés sur l'historique de la maison :
#
#   le RENDEMENT        volume vendu / volume brassé, médiane des fûts
#                       terminés, par bière quand il y en a assez ;
#   le PROFIL DE SEMAINE part de chaque jour dans une semaine type, mesurée
#                       sur toutes les bières ;
#   le RYTHME DU FÛT    ses propres derniers jours, ramenés au « jour
#                       standard » par le profil.
#
# On avance ensuite jour par jour : chaque jour retire rythme x poids du jour,
# et la fin est le premier jour où il ne reste rien. Pas de boîte noire, et
# chaque chiffre affiché se raconte en une phrase.
#
# ---------------------------------------------------------------------------
# Et surtout : on mesure
# ---------------------------------------------------------------------------
# Les fûts terminés disent la vérité. backtest_futs() se replace à H jours de
# la fin réelle de chacun, demande à chaque modèle ce qu'il aurait annoncé ce
# jour-là, et compare. Un modèle sans mesure d'erreur ne se discute pas —
# c'est la même règle que pour la projection comptable.
#
# ---------------------------------------------------------------------------
# Ce que le banc d'essai a donné
# ---------------------------------------------------------------------------
# Sur soixante fûts SIMULÉS — profil de semaine, effet de nouveauté, bruit
# multiplicatif, rendement vrai de 80 % là où l'import en suppose 75 % — et
# sur les points de mesure où les DEUX modèles répondent :
#
#                         répond    erreur moyenne    fourchette juste
#   HoltWinters           40-76 %        4,1 j            5-45 %
#   Rythme + semaine      95-100 %       0,9 j           60-85 %
#
# La part du 0,75 dans l'écart n'est pas anecdotique : en rejouant le
# HoltWinters avec le rendement mesuré, son erreur à dix jours tombe de 3,7 à
# 2,5 jours, et son taux de réponse à un jour de la fin passe de 37 % à 78 %.
# Le reste vient de la méthode.
#
# Ces chiffres-là ne valent que pour le banc : les vrais sont ceux que le
# bouton « Lancer la vérification » calcule sur l'historique de la maison, et
# ce sont les seuls à regarder.
#
# Trois variantes ont été éprouvées et écartées, faute de gain mesurable :
# fenêtre de rythme à 7 et à 14 jours (MAE 1,17 et 1,16 contre 1,15), et
# abandon des deux premiers jours du fût pour sortir l'effet de nouveauté
# (1,13). Le modèle est insensible à ce réglage ; inutile d'y revenir.

#### Les constantes du modèle ####

# L'abattement en dur de l'import (import.R), gardé ici pour pouvoir être
# confronté au rendement observé. Le jour où l'import cesse de l'appliquer,
# c'est cette valeur-là qu'il faudra changer de place, pas une de plus.
RENDEMENT_SUPPOSE <- 0.75

# Un rendement hors de cette fourchette ne mesure pas une perte : il mesure un
# fût retiré avant la fin, un volume brassé mal saisi, ou deux brassins
# confondus. On l'écarte du calcul et on le signale.
RENDEMENT_MIN <- 0.45
RENDEMENT_MAX <- 1.05

# Combien de fûts terminés il faut à une bière pour avoir son propre
# rendement. En dessous, elle prend celui de la maison.
FUT_MIN_BRASSINS <- 3

# Sur combien de jours on lit le rythme d'un fût. Dix jours d'ouverture :
# assez pour couvrir une semaine entière et un week-end, assez peu pour que
# l'effet de nouveauté des premiers jours soit sorti de la fenêtre.
FUT_FENETRE_RYTHME <- 10

# Il faut au moins ça de jours vendus pour parler d'un rythme.
FUT_MIN_JOURS <- 3

# 0,6745 = le quartile de la loi normale : la fourchette vise 50 %, comme
# celle du modèle historique (LO_50 / HI_50), pour que les deux se comparent.
# Ce qu'elle couvre VRAIMENT est mesuré par le back-test, pas supposé.
FUT_K_FOURCHETTE <- 0.6745

# Un fût dont on ne sait rien ne vaut pas une fourchette nulle : plancher sur
# la variabilité relative du rythme.
FUT_CV_PLANCHER <- 0.15

# Depuis combien de jours sans vente un fût est réputé terminé.
#
# Ce n'est pas un détail : rejouer le passé exige de savoir ce qu'on savait
# CE JOUR-LÀ. Or BIERE_FINIE est un drapeau posé une fois pour toutes par
# l'import — il vaut TRUE, dans toute la table, pour un fût qui ne sera fini
# que six mois plus tard. S'y fier en se replaçant au passé ferait entrer
# dans les références un fût encore en cours, avec son volume vendu tronqué,
# donc un rendement faux. Le silence, lui, se constate à la date voulue.
#
# Dix jours, comme la règle de l'import qui force BIERE_FINIE (import.R).
FUT_SILENCE_FIN <- 10

#### La vérité : les fûts terminés ####

#' Une ligne par fût terminé : quand il a fini, et ce qu'il a donné.
#'
#' La fin retenue est le DERNIER JOUR DE VENTE, qui est mesuré, et non la
#' « date de fin » saisie dans le classeur des brassins — celle-ci est
#' facultative, et l'import la remplace par aujourd'hui quand elle manque
#' (import.R), ce qui en ferait une vérité fabriquée.
#'
#' Un fût compte comme terminé s'il est marqué fini ET qu'il n'a plus vendu
#' depuis FUT_SILENCE_FIN jours à la date demandée : c'est ce qui rend la
#' fonction rejouable dans le passé (cf. la note sur BIERE_FINIE ci-dessus).
fins_observees <- function(db_bieres, aujourd_hui = today()) {
  vide <- tibble(ID_BRASSIN = character(), BOISSON = character(),
                 DEBUT = as.Date(character()), FIN = as.Date(character()),
                 VENDU = numeric(), VOLUME_BRASSIN = numeric(),
                 RENDEMENT = numeric(), N_JOURS = numeric(),
                 N_JOURS_VENTE = integer())
  if (is.null(db_bieres) || !nrow(db_bieres)) return(vide)
  d <- db_bieres %>%
    filter(!is.na(ID_BRASSIN), !is.na(DATE), DATE < as.Date(aujourd_hui))
  if (!nrow(d)) return(vide)

  d %>%
    group_by(ID_BRASSIN, BOISSON) %>%
    summarise(DEBUT = min(DATE), FIN = max(DATE[VOLUME_JOUR > 0]),
              VENDU = max(VOLUME_TOT, na.rm = TRUE),
              VOLUME_BRASSIN = max(VOLUME_BRASSIN, na.rm = TRUE),
              FINIE = any(BIERE_FINIE), N_JOURS_VENTE = sum(VOLUME_JOUR > 0),
              .groups = "drop") %>%
    filter(FINIE, !is.na(FIN), VOLUME_BRASSIN > 0, N_JOURS_VENTE >= 1,
           FIN <= as.Date(aujourd_hui) - FUT_SILENCE_FIN) %>%
    transmute(ID_BRASSIN, BOISSON, DEBUT, FIN, VENDU, VOLUME_BRASSIN,
              RENDEMENT = VENDU / VOLUME_BRASSIN,
              N_JOURS = as.numeric(FIN - DEBUT) + 1, N_JOURS_VENTE) %>%
    arrange(desc(FIN))
}

#' Le rendement, par bière et pour la maison.
#'
#' @return une ligne par bière ayant assez de fûts, plus une ligne « Toutes »
#'   qui sert de repli. ECART_TYPE porte la dispersion : c'est elle qui donne
#'   sa largeur à la fourchette, et elle n'a rien d'anecdotique — deux fûts de
#'   la même bière ne rendent pas le même volume.
rendements_observes <- function(fins) {
  vide <- tibble(BOISSON = character(), N = integer(), RENDEMENT = numeric(),
                 ECART_TYPE = numeric())
  if (is.null(fins) || !nrow(fins)) return(vide)
  bons <- fins %>% filter(RENDEMENT >= RENDEMENT_MIN, RENDEMENT <= RENDEMENT_MAX)
  if (!nrow(bons)) return(vide)

  global <- tibble(BOISSON = "Toutes", N = nrow(bons),
                   RENDEMENT = median(bons$RENDEMENT),
                   ECART_TYPE = stats::sd(bons$RENDEMENT))
  par_biere <- bons %>%
    group_by(BOISSON) %>%
    filter(n() >= FUT_MIN_BRASSINS) %>%
    summarise(N = n(), RENDEMENT = median(RENDEMENT),
              ECART_TYPE = stats::sd(RENDEMENT), .groups = "drop")
  res <- bind_rows(global, par_biere)
  # Un écart-type manquant (une seule valeur) ou nul rendrait la fourchette
  # vide : on retombe alors sur la dispersion de la maison.
  res %>% mutate(ECART_TYPE = if_else(is.na(ECART_TYPE) | ECART_TYPE <= 0,
                                      global$ECART_TYPE, ECART_TYPE))
}

#' Le rendement à retenir pour une bière : le sien, sinon celui de la maison,
#' sinon l'abattement supposé de l'import.
rendement_pour <- function(rendements, boisson) {
  defaut <- list(RENDEMENT = RENDEMENT_SUPPOSE, ECART_TYPE = 0.05,
                 SOURCE = "abattement supposé")
  if (is.null(rendements) || !nrow(rendements)) return(defaut)
  l <- rendements[rendements$BOISSON == boisson, ]
  if (nrow(l)) return(list(RENDEMENT = l$RENDEMENT[1],
                           ECART_TYPE = l$ECART_TYPE[1],
                           SOURCE = paste0(l$N[1], " fûts de ", boisson)))
  g <- rendements[rendements$BOISSON == "Toutes", ]
  if (nrow(g)) return(list(RENDEMENT = g$RENDEMENT[1],
                           ECART_TYPE = g$ECART_TYPE[1],
                           SOURCE = paste0(g$N[1], " fûts, toutes bières")))
  defaut
}

#### Le profil de semaine ####

# Une semaine où moins de jours que cela ont vendu ne dit rien de la FORME
# d'une semaine : elle dit seulement qu'un fût s'y est terminé. On l'écarte.
FUT_MIN_JOURS_SEMAINE <- 4

#' Part de chaque jour de la semaine dans une semaine type.
#'
#' Mesuré sur TOUTES les bières : un fût seul n'a pas assez de semaines pour
#' que son propre profil veuille dire quelque chose, et il n'y a aucune raison
#' qu'une bière se vende un autre jour que les autres.
#'
#' Le calcul passe par la PART de chaque jour DANS SA SEMAINE, moyennée sur
#' les semaines, et non par la moyenne des volumes. La différence compte dès
#' que l'activité est intermittente : un volume moyen par jour de semaine se
#' laisse diluer par les périodes sans fût en perce, et dans des proportions
#' qui dépendent du jour où ces périodes commencent. Une part dans la semaine,
#' elle, ne dépend ni du niveau des ventes ni de la saison.
#'
#' @return POIDS, de moyenne 1 sur les sept jours. Un jour de fermeture pèse
#'   donc 0, et les jours ouverts se partagent les sept parts.
profil_semaine_bieres <- function(db_bieres, fin = today(), n_semaines = 26) {
  neutre <- tibble(JOUR = 1:7, POIDS = 1)
  if (is.null(db_bieres) || !nrow(db_bieres)) return(neutre)
  d <- db_bieres %>%
    filter(DATE <= as.Date(fin), DATE > as.Date(fin) - 7 * n_semaines,
           !is.na(VOLUME_JOUR))
  if (!nrow(d)) return(neutre)

  # Les jours sans la moindre ligne comptent, et ils comptent zéro : sans eux,
  # le lundi n'apparaîtrait nulle part et ne pèserait donc jamais zéro.
  jour <- tibble(DATE = seq(min(d$DATE), max(d$DATE), by = "day")) %>%
    left_join(d %>% group_by(DATE) %>%
                summarise(V = sum(VOLUME_JOUR, na.rm = TRUE), .groups = "drop"),
              by = "DATE") %>%
    mutate(V = replace_na(V, 0),
           JOUR = wday(DATE, week_start = 1),
           SEMAINE = floor_date(DATE, "week", week_start = 1))

  # Deux passes, et la seconde n'est pas un raffinement : la première sert à
  # DÉCOUVRIR quels jours sont habituellement ouverts, la seconde à ne garder
  # que les semaines où tous ces jours-là ont effectivement vendu. Sans elle,
  # la semaine où un fût est mis en perce un mercredi fait disparaître le
  # mardi de la moyenne, et toute la forme de la semaine se déforme.
  moyenne_parts <- function(semaines) {
    if (!nrow(semaines)) return(NULL)
    p <- semaines %>% group_by(JOUR) %>%
      summarise(PART = mean(PART), .groups = "drop")
    p <- tibble(JOUR = 1:7) %>% left_join(p, by = "JOUR") %>%
      mutate(PART = replace_na(PART, 0))
    if (sum(p$PART) <= 0) return(NULL)
    p %>% transmute(JOUR, POIDS = 7 * PART / sum(PART))
  }
  avec_parts <- function(d) d %>% group_by(SEMAINE) %>%
    mutate(PART = V / sum(V)) %>% ungroup()

  p1 <- moyenne_parts(avec_parts(
    jour %>% group_by(SEMAINE) %>%
      filter(n() == 7, sum(V) > 0, sum(V > 0) >= FUT_MIN_JOURS_SEMAINE) %>%
      ungroup()))
  if (is.null(p1)) return(neutre)

  ouverts <- p1$JOUR[p1$POIDS > 0.05]
  p2 <- moyenne_parts(avec_parts(
    jour %>% group_by(SEMAINE) %>%
      filter(n() == 7, sum(V) > 0, all(V[JOUR %in% ouverts] > 0)) %>%
      ungroup()))
  if (is.null(p2)) p1 else p2
}

poids_jour <- function(profil, dates) {
  j <- wday(as.Date(dates), week_start = 1)
  p <- profil$POIDS[match(j, profil$JOUR)]
  replace_na(p, 1)
}

#### Le rythme d'un fût ####

#' La série quotidienne d'un fût, trous compris.
#'
#' DB_BIERES ne porte une ligne que les jours où la bière s'est VENDUE : un
#' jour d'ouverture sans aucune vente de cette bière-là n'y figure pas. Lire
#' le rythme sur les seules lignes présentes reviendrait à ne compter que les
#' bons jours, donc à surestimer la vitesse et à annoncer la fin trop tôt.
#'
#' On complète donc jusqu'au jour demandé, à zéro litre. Les jours de
#' fermeture, eux, seront écartés plus loin par leur poids nul : un zéro un
#' lundi n'est pas un jour sans vente, c'est un jour sans service.
serie_quotidienne <- function(db_bieres, id_brassin, a_la_date) {
  d <- db_bieres %>%
    filter(ID_BRASSIN == id_brassin, DATE <= as.Date(a_la_date)) %>%
    arrange(DATE)
  if (!nrow(d)) return(d)
  jours <- tibble(DATE = seq(min(d$DATE), max(d$DATE), by = "day"))
  jours %>%
    left_join(d %>% select(DATE, VOLUME_JOUR, VOLUME_TOT), by = "DATE") %>%
    mutate(VOLUME_JOUR = replace_na(VOLUME_JOUR, 0),
           VOLUME_TOT = cummax(replace_na(VOLUME_TOT, 0)))
}

#' Litres par « jour standard », lus sur les derniers jours du fût.
#'
#' Chaque jour observé est divisé par le poids de son jour de semaine : un
#' samedi à 40 L et un mardi à 18 L peuvent dire le même rythme. On prend la
#' MÉDIANE de ces valeurs ramenées, pour qu'une soirée exceptionnelle ne
#' décide pas de la date de fin.
#'
#' Les jours de fermeture sortent du calcul : diviser par un poids nul n'a pas
#' de sens, et un zéro un lundi n'apprend rien sur le rythme.
rythme_fut <- function(serie, profil, fenetre = FUT_FENETRE_RYTHME) {
  vide <- list(RYTHME = NA_real_, CV = NA_real_, N = 0L)
  if (is.null(serie) || !nrow(serie)) return(vide)
  d <- serie %>%
    mutate(POIDS = poids_jour(profil, DATE)) %>%
    filter(POIDS > 0.05, !is.na(VOLUME_JOUR)) %>%
    arrange(DATE) %>%
    tail(fenetre) %>%
    mutate(NORME = VOLUME_JOUR / POIDS)
  if (nrow(d) < FUT_MIN_JOURS) return(vide)
  m <- median(d$NORME)
  if (is.na(m) || m <= 0) return(vide)
  list(RYTHME = m,
       CV = max(FUT_CV_PLANCHER, stats::sd(d$NORME) / m, na.rm = TRUE),
       N = nrow(d))
}

#' À quelle date le fût sera vide, en avançant jour par jour.
#'
#' Pas de division « reste / rythme » : elle ignorerait le calendrier, qui est
#' tout l'intérêt. On retire chaque jour sa part, et on s'arrête au premier
#' jour où il ne reste rien.
date_epuisement <- function(reste, depuis, rythme, profil, horizon = 180) {
  if (is.na(reste) || is.na(rythme) || rythme <= 0) return(as.Date(NA))
  if (reste <= 0) return(as.Date(depuis))
  jours <- seq(as.Date(depuis) + 1, as.Date(depuis) + horizon, by = "day")
  cum <- cumsum(rythme * poids_jour(profil, jours))
  i <- which(cum >= reste)[1]
  if (is.na(i)) as.Date(NA) else jours[i]
}

#' La prédiction de fin de fût, telle que l'écran l'affiche : tôt, estimée,
#' tard.
#'
#' Deux incertitudes, et elles ne vont pas dans le même sens :
#'   - le RENDEMENT joue sur ce qu'il reste. Moins de volume vendable, c'est
#'     une fin plus tôt ;
#'   - le RYTHME joue sur la vitesse. Plus vite, c'est aussi une fin plus tôt.
#' La borne « tôt » cumule donc le rendement bas et le rythme haut, et la
#' borne « tard » l'inverse.
#'
#' @return un vecteur de trois dates, même contrat que predict_fin_brassin()
#'   de R/brassins.R — c'est ce qui permet au back-test de les confronter.
predit_fin_fut <- function(db_bieres, id_brassin, a_la_date = today(),
                           profil = NULL, rendements = NULL,
                           fenetre = FUT_FENETRE_RYTHME) {
  vide <- rep(as.Date(NA), 3)
  if (is.null(db_bieres) || !nrow(db_bieres)) return(vide)
  a_la_date <- as.Date(a_la_date)

  info <- db_bieres %>%
    filter(ID_BRASSIN == id_brassin, DATE <= a_la_date) %>%
    arrange(DATE)
  if (!nrow(info)) return(vide)
  serie <- serie_quotidienne(db_bieres, id_brassin, a_la_date)

  if (is.null(profil)) profil <- profil_semaine_bieres(db_bieres, a_la_date)
  if (is.null(rendements))
    rendements <- rendements_observes(fins_observees(db_bieres, a_la_date))

  r <- rythme_fut(serie, profil, fenetre)
  if (is.na(r$RYTHME)) return(vide)

  rend <- rendement_pour(rendements, info$BOISSON[1])
  v0 <- max(info$VOLUME_BRASSIN, na.rm = TRUE)
  vendu <- max(info$VOLUME_TOT, na.rm = TRUE)

  k <- FUT_K_FOURCHETTE
  reste <- function(taux) v0 * taux - vendu
  # L'incertitude du rythme s'atténue avec le nombre de jours observés : on
  # projette une MOYENNE de jours à venir, pas un jour isolé.
  err <- k * r$CV / sqrt(r$N)

  c(date_epuisement(reste(rend$RENDEMENT - k * rend$ECART_TYPE), a_la_date,
                    r$RYTHME * (1 + err), profil),
    date_epuisement(reste(rend$RENDEMENT), a_la_date, r$RYTHME, profil),
    date_epuisement(reste(rend$RENDEMENT + k * rend$ECART_TYPE), a_la_date,
                    r$RYTHME * (1 - err), profil))
}

#### Le back-test ####

# L'horizon de projection des adaptateurs. Quatre-vingt-dix jours : un fût qui
# durerait plus longtemps que ça n'a de toute façon pas de date de fin utile.
#
# Ce n'est pas une coquetterie. predict.HoltWinters coûte 258 ms à h = 200,
# contre 37 ms à h = 60 et 9 ms pour l'ajustement lui-même : l'horizon, et lui
# seul, fait le prix du modèle actuel. table_evo_brassins() le laisse à 200 en
# production, ce qui coûte près d'une seconde par fût ouvert, à chaque
# affichage de l'onglet Fûts ET de l'accueil.
FUT_HORIZON_PREDICTION <- 90

#' Le modèle historique, mis au même contrat que le nouveau.
#'
#' Même signature, même sortie : trois dates. C'est ce qui permet de les
#' confronter sans que le back-test ait à savoir lequel il interroge.
predit_fin_holtwinters <- function(db_bieres, id_brassin, a_la_date = today(),
                                   horizon = FUT_HORIZON_PREDICTION, ...) {
  p <- try(table_evo_brassin(db_bieres, id_brassin, horizon, as.Date(a_la_date)),
           silent = TRUE)
  if (inherits(p, "try-error") || is.null(p) || !nrow(p))
    return(rep(as.Date(NA), 3))
  f <- try(predict_fin_brassin(p, id_brassin), silent = TRUE)
  if (inherits(f, "try-error")) return(rep(as.Date(NA), 3))
  as.Date(as.numeric(f), origin = "1970-01-01")
}

#' Les modèles confrontés.
#'
#' Fonction et non constante : elle désigne des fonctions définies ailleurs
#' (R/brassins.R), et l'ordre alphabétique de chargement de global.R ne
#' garantit pas qu'elles existent au moment où ce fichier est lu.
modeles_fut <- function() list(
  rythme      = list(LIBELLE = "Rythme + jour de semaine",
                     FONCTION = predit_fin_fut),
  holtwinters = list(LIBELLE = "HoltWinters (actuel)",
                     FONCTION = predit_fin_holtwinters))

#' Ce que chaque modèle aurait annoncé, et ce qui s'est passé.
#'
#' Pour chaque fût terminé et chaque horizon H, on se replace à H jours de sa
#' fin réelle et on interroge les modèles sur les seules données de ce
#' jour-là. Rien d'autre ne garantit qu'une prédiction vaut quelque chose.
#'
#' Le contexte du nouveau modèle — profil de semaine, rendements — est calculé
#' au PREMIER DU MOIS du point de vue, et mémorisé : c'est de l'information
#' strictement passée, et cela évite de rebâtir l'historique à chaque appel.
#'
#' @param n_max ne garder que les N fûts les plus récents. Le modèle actuel
#'   coûte près d'un dixième de seconde par point de mesure.
backtest_futs <- function(db_bieres, horizons = c(1, 3, 5, 7, 10, 14),
                          modeles = modeles_fut(), fins = NULL, n_max = 40,
                          aujourd_hui = today()) {
  vide <- tibble(ID_BRASSIN = character(), BOISSON = character(),
                 MODELE = character(), LIBELLE = character(),
                 HORIZON = numeric(), VANTAGE = as.Date(character()),
                 FIN_VRAIE = as.Date(character()), DEBUT = as.Date(character()),
                 TOT = as.Date(character()), EST = as.Date(character()),
                 TARD = as.Date(character()), ERREUR = numeric(),
                 DANS_FOURCHETTE = logical())
  if (is.null(fins)) fins <- fins_observees(db_bieres, aujourd_hui)
  if (is.null(fins) || !nrow(fins) || !length(modeles)) return(vide)
  fins <- fins %>% arrange(desc(FIN))
  if (!is.null(n_max)) fins <- head(fins, n_max)

  memo <- new.env(parent = emptyenv())
  contexte <- function(d) {
    cle <- format(floor_date(as.Date(d), "month"))
    if (!is.null(memo[[cle]])) return(memo[[cle]])
    ref <- floor_date(as.Date(d), "month")
    ctx <- list(profil = profil_semaine_bieres(db_bieres, ref),
                rendements = rendements_observes(fins_observees(db_bieres, ref)))
    assign(cle, ctx, envir = memo)
    ctx
  }

  res <- list(); k <- 0
  for (i in seq_len(nrow(fins))) {
    f <- fins[i, ]
    for (h in horizons) {
      v <- f$FIN - h
      # Un fût ne peut pas être interrogé avant d'avoir commencé à couler.
      if (v < f$DEBUT + 1) next
      ctx <- contexte(v)
      for (nom in names(modeles)) {
        m <- modeles[[nom]]
        d <- try(m$FONCTION(db_bieres, f$ID_BRASSIN, v,
                            profil = ctx$profil, rendements = ctx$rendements),
                 silent = TRUE)
        if (inherits(d, "try-error")) d <- rep(as.Date(NA), 3)
        k <- k + 1
        res[[k]] <- tibble(
          ID_BRASSIN = f$ID_BRASSIN, BOISSON = f$BOISSON, MODELE = nom,
          LIBELLE = m$LIBELLE, HORIZON = h, VANTAGE = v, FIN_VRAIE = f$FIN,
          DEBUT = f$DEBUT, TOT = d[1], EST = d[2], TARD = d[3])
      }
    }
  }
  if (!k) return(vide)
  bind_rows(res) %>%
    mutate(ERREUR = as.numeric(EST - FIN_VRAIE),
           DANS_FOURCHETTE = !is.na(TOT) & !is.na(TARD) &
             FIN_VRAIE >= TOT & FIN_VRAIE <= TARD)
}

#' Le verdict, par modèle et par horizon.
#'
#' RÉPOND compte d'abord : un modèle muet n'a pas d'erreur, et une moyenne
#' calculée sur les seuls cas où il a bien voulu répondre flatte celui qui se
#' tait le plus souvent. C'est exactement le piège du modèle actuel, qui ne
#' répond qu'une fois sur deux à dix jours de la fin.
resume_backtest <- function(bt) {
  vide <- tibble(MODELE = character(), LIBELLE = character(),
                 HORIZON = numeric(), N = integer(), REPOND = numeric(),
                 BIAIS = numeric(), MAE = numeric(), P90 = numeric(),
                 COUVERTURE = numeric())
  if (is.null(bt) || !nrow(bt)) return(vide)
  bt %>%
    group_by(MODELE, LIBELLE, HORIZON) %>%
    summarise(N = n(),
              REPOND = 100 * mean(!is.na(ERREUR)),
              BIAIS = median(ERREUR, na.rm = TRUE),
              MAE = mean(abs(ERREUR), na.rm = TRUE),
              P90 = unname(quantile(abs(ERREUR), 0.9, na.rm = TRUE)),
              COUVERTURE = 100 * mean(DANS_FOURCHETTE),
              .groups = "drop") %>%
    arrange(HORIZON, MODELE)
}

#' Les fûts que le modèle a le plus mal vus.
#'
#' Une ligne par fût : son pire écart, et à quel horizon il s'est produit. On
#' ne liste pas chaque point de mesure — un même fût raté à cinq horizons
#' remplirait le tableau et cacherait les autres.
pires_predictions <- function(bt, modele = "rythme", n = 12) {
  vide <- tibble(ID_BRASSIN = character(), BOISSON = character(),
                 DEBUT = as.Date(character()), FIN_VRAIE = as.Date(character()),
                 HORIZON = numeric(), EST = as.Date(character()),
                 ERREUR = numeric())
  if (is.null(bt) || !nrow(bt)) return(vide)
  d <- bt %>% filter(MODELE == modele, !is.na(ERREUR))
  if (!nrow(d)) return(vide)
  d %>%
    group_by(ID_BRASSIN, BOISSON, DEBUT, FIN_VRAIE) %>%
    slice_max(abs(ERREUR), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(abs(ERREUR))) %>%
    head(n) %>%
    select(ID_BRASSIN, BOISSON, DEBUT, FIN_VRAIE, HORIZON, EST, ERREUR)
}

#### Rendus ####

#' Les tuiles du verdict : ce que le back-test a trouvé, en cinq chiffres.
#'
#' L'horizon de référence est sept jours — à une semaine de la fin, on a
#' encore le temps de commander, et c'est là que la prédiction sert.
FUT_HORIZON_TUILE <- 7

kpi_backtest <- function(resume, fins = NULL, rendements = NULL,
                         horizon = FUT_HORIZON_TUILE) {
  if (is.null(resume) || !nrow(resume))
    return(div(class = "text-muted small p-2",
               "Pas encore assez de fûts terminés pour juger les modèles."))
  a <- resume %>% filter(HORIZON == horizon)
  if (!nrow(a)) a <- resume %>% filter(HORIZON == max(HORIZON))
  ligne <- function(m) a[a$MODELE == m, ]
  nv <- ligne("rythme"); hw <- ligne("holtwinters")
  jours <- function(x) if (!nrow(x) || is.na(x$MAE[1])) "—"
                       else paste0("± ", round(x$MAE[1], 1), " j")
  pct <- function(x, col) if (!nrow(x)) "—" else format_pct(x[[col]][1], 0)

  rend <- if (is.null(rendements) || !nrow(rendements)) NULL else
    rendements[rendements$BOISSON == "Toutes", ]
  n_futs <- if (is.null(fins)) NA_integer_ else nrow(fins)

  div(
    class = "kpi-grid",
    kpi_tile(if (is.na(n_futs)) "—" else as.character(n_futs),
             "Fûts terminés", COUL_BRUN, "beer-mug-empty",
             sous_titre = "la base de la vérification"),
    kpi_tile(jours(nv), paste0("Erreur à ", horizon, " jours"),
             if (!nrow(nv) || is.na(nv$MAE[1])) COUL_NEUTRE
             else couleur_seuil(nv$MAE[1], 1.5, 3),
             "bullseye",
             sous_titre = paste0("modèle actuel : ", jours(hw))),
    kpi_tile(pct(nv, "REPOND"), "Réponses du nouveau",
             if (!nrow(nv)) COUL_NEUTRE else couleur_seuil_haut(nv$REPOND[1], 95, 80),
             "comment", sous_titre = paste0("actuel : ", pct(hw, "REPOND"))),
    kpi_tile(pct(nv, "COUVERTURE"), "Fourchette juste",
             if (!nrow(nv)) COUL_NEUTRE
             else couleur_seuil_haut(nv$COUVERTURE[1], 50, 35),
             "arrows-left-right",
             sous_titre = paste0("actuel : ", pct(hw, "COUVERTURE"))),
    kpi_tile(if (is.null(rend) || !nrow(rend)) "—"
             else format_pct(100 * rend$RENDEMENT[1], 0),
             "Rendement observé", COUL_VERT, "flask",
             sous_titre = paste0("l'import suppose ",
                                 format_pct(100 * RENDEMENT_SUPPOSE, 0))))
}

#' Le verdict en tableau : une ligne par modèle et par horizon.
table_resume_backtest <- function(resume) {
  if (is.null(resume) || !nrow(resume))
    return(tibble(Vérification = "Aucun fût terminé à confronter."))
  resume %>%
    transmute(
      Horizon = paste0(HORIZON, " j"), `.tri:Horizon` = HORIZON,
      Modèle = LIBELLE,
      Cas = N, `.tri:Cas` = N,
      `Répond` = format_pct(REPOND, 0), `.tri:Répond` = REPOND,
      Biais = ifelse(is.na(BIAIS), "—",
                     paste0(ifelse(BIAIS > 0, "+", ""), round(BIAIS, 1), " j")),
      `.tri:Biais` = BIAIS,
      `Erreur moyenne` = ifelse(is.na(MAE), "—", paste0(round(MAE, 1), " j")),
      `.tri:Erreur moyenne` = MAE,
      `9 fois sur 10` = ifelse(is.na(P90), "—", paste0("≤ ", round(P90, 1), " j")),
      `.tri:9 fois sur 10` = P90,
      `Fourchette juste` = format_pct(COUVERTURE, 0),
      `.tri:Fourchette juste` = COUVERTURE)
}

#' L'erreur moyenne selon l'avance : une courbe par modèle.
#'
#' C'est LE graphe de la vérification. Un modèle qui ne vaut que la veille au
#' soir ne sert à rien — on commande une semaine à l'avance.
graph_backtest <- function(resume) {
  if (is.null(resume) || !nrow(resume))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucun fût terminé à confronter")))
  coul <- c(rythme = COUL_VERT, holtwinters = COUL_NEUTRE)
  g <- plot_ly()
  for (m in unique(resume$MODELE)) {
    d <- resume %>% filter(MODELE == m) %>% arrange(HORIZON)
    c_m <- unname(coul[m]); if (is.na(c_m)) c_m <- COUL_BRUN
    g <- g %>% add_trace(
      x = d$HORIZON, y = d$MAE, type = "scatter", mode = "lines+markers",
      name = d$LIBELLE[1],
      line = list(color = c_m, width = 3,
                  dash = if (m == "holtwinters") "dot" else "solid"),
      marker = list(color = c_m, size = 8),
      hovertemplate = paste0(d$LIBELLE[1], "<br>", d$HORIZON, " jours avant la fin",
                             "<br>erreur moyenne : ", round(d$MAE, 1), " j",
                             "<br>répond dans ", round(d$REPOND), " % des cas",
                             "<extra></extra>"))
  }
  g %>% layout(
    xaxis = list(title = "Jours avant la fin réelle", autorange = "reversed"),
    yaxis = list(title = "Erreur moyenne (jours)", rangemode = "tozero"),
    legend = list(orientation = "h", y = -0.25),
    margin = list(b = 70),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Les fûts les moins bien prédits, en tableau.
table_pires_futs <- function(pires) {
  if (is.null(pires) || !nrow(pires))
    return(tibble(Fûts = "Aucun écart à signaler."))
  pires %>%
    transmute(
      Bière = BOISSON, Brassin = ID_BRASSIN,
      `Du` = format(DEBUT, "%d/%m/%y"), `.tri:Du` = as.integer(DEBUT),
      `Fin réelle` = format(FIN_VRAIE, "%a %d/%m/%y"),
      `.tri:Fin réelle` = as.integer(FIN_VRAIE),
      `Vu à` = paste0(HORIZON, " j"), `.tri:Vu à` = HORIZON,
      `Fin annoncée` = ifelse(is.na(EST), "—", format(EST, "%a %d/%m/%y")),
      Écart = paste0(ifelse(ERREUR > 0, "+", ""), round(ERREUR), " j"),
      `.tri:Écart` = abs(ERREUR))
}

#' Le rendement des fûts terminés, et l'abattement supposé.
#'
#' Chaque point est un fût : on voit la dispersion, pas seulement la médiane.
#' C'est elle qui donne sa largeur à la fourchette, et c'est elle qui dit si
#' un rendement unique a seulement un sens.
graph_rendements <- function(fins, rendements = NULL) {
  if (is.null(fins) || !nrow(fins))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucun fût terminé")))
  d <- fins %>% arrange(FIN) %>%
    mutate(PCT = 100 * RENDEMENT,
           HORS = RENDEMENT < RENDEMENT_MIN | RENDEMENT > RENDEMENT_MAX)
  med <- if (is.null(rendements) || !nrow(rendements)) NA_real_ else
    100 * rendements$RENDEMENT[rendements$BOISSON == "Toutes"][1]

  g <- plot_ly() %>%
    add_trace(x = d$FIN, y = d$PCT, type = "scatter", mode = "markers",
              name = "Un fût",
              marker = list(size = 9,
                            color = ifelse(d$HORS, COUL_ROUGE, COUL_VERT),
                            opacity = 0.75),
              hovertemplate = paste0(d$BOISSON, " (", d$ID_BRASSIN, ")",
                                     "<br>fin : ", format(d$FIN, "%d/%m/%y"),
                                     "<br>brassé : ", round(d$VOLUME_BRASSIN), " L",
                                     "<br>vendu : ", round(d$VENDU), " L",
                                     "<br><b>rendement : ", round(d$PCT), " %</b>",
                                     "<extra></extra>"))
  trait <- function(g, y, coul, nom, dash) {
    if (is.na(y)) return(g)
    g %>% add_trace(x = range(d$FIN), y = c(y, y), type = "scatter",
                    mode = "lines", name = nom,
                    line = list(color = coul, width = 2, dash = dash),
                    hovertemplate = paste0(nom, " : ", round(y), " %",
                                           "<extra></extra>"))
  }
  g %>%
    trait(100 * RENDEMENT_SUPPOSE, COUL_ROUGE, "Supposé par l'import", "dot") %>%
    trait(med, COUL_VERT, "Médiane observée", "dash") %>%
    layout(xaxis = list(title = ""),
           yaxis = list(title = "Volume vendu / volume brassé (%)"),
           legend = list(orientation = "h", y = -0.2),
           margin = list(b = 60),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Le profil de semaine appris : ce que pèse chaque jour.
graph_profil_semaine <- function(profil) {
  if (is.null(profil) || !nrow(profil))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Pas assez de ventes")))
  d <- profil %>% arrange(JOUR) %>%
    mutate(NOM = vecteur_jours[JOUR],
           COUL = ifelse(POIDS >= 1, COUL_BRUN, COUL_MATIERE))
  plot_ly() %>%
    add_trace(x = d$NOM, y = d$POIDS, type = "bar", name = "Poids",
              marker = list(color = d$COUL),
              hovertemplate = paste0(d$NOM, " pèse ", round(d$POIDS, 2),
                                     " jour", ifelse(d$POIDS >= 2, "s", ""),
                                     " moyen<extra></extra>")) %>%
    layout(xaxis = list(title = "", type = "category",
                        categoryorder = "array", categoryarray = d$NOM),
           yaxis = list(title = "Poids (1 = jour moyen)"),
           showlegend = FALSE,
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

#' Les fins de fût prévues, par les deux modèles côte à côte.
#'
#' Tant que le back-test n'a pas tranché, on affiche les deux : c'est la seule
#' façon de voir, semaine après semaine, lequel tombe juste sur les fûts en
#' cours. Les deux volumes restants figurent aussi, et ils diffèrent — l'un
#' retranche 25 % de perte par convention, l'autre le rendement mesuré sur les
#' fûts terminés. C'est précisément le désaccord qu'on veut rendre visible.
table_fins_prevues <- function(db_bieres, db_predict = NULL, profil = NULL,
                               rendements = NULL, aujourd_hui = today()) {
  vide <- tibble(Bière = character())
  if (is.null(db_bieres) || !nrow(db_bieres)) return(vide)
  en_cours <- db_bieres %>%
    filter(!BIERE_FINIE, DATE <= aujourd_hui, DATE >= aujourd_hui - 30) %>%
    group_by(ID_BRASSIN, BOISSON) %>%
    arrange(DATE) %>% slice_tail(n = 1) %>% ungroup()
  if (!nrow(en_cours)) return(vide)

  if (is.null(profil)) profil <- profil_semaine_bieres(db_bieres, aujourd_hui)
  if (is.null(rendements))
    rendements <- rendements_observes(fins_observees(db_bieres, aujourd_hui))

  fmt <- function(x) ifelse(is.na(x), "—", format(as.Date(x), "%a %d/%m"))
  lignes <- lapply(seq_len(nrow(en_cours)), function(i) {
    l <- en_cours[i, ]
    nv <- predit_fin_fut(db_bieres, l$ID_BRASSIN, aujourd_hui, profil, rendements)
    hw <- if (is.null(db_predict) || !nrow(db_predict)) rep(as.Date(NA), 3)
          else as.Date(as.numeric(predict_fin_brassin(db_predict, l$ID_BRASSIN)),
                       origin = "1970-01-01")
    rend <- rendement_pour(rendements, l$BOISSON)
    tibble(Bière = l$BOISSON,
           `Restant (0,75)` = paste0(round(pmax(0, l$DIFF)), " L"),
           `.tri:Restant (0,75)` = pmax(0, l$DIFF),
           `Restant (mesuré)` = paste0(
             round(pmax(0, l$VOLUME_BRASSIN * rend$RENDEMENT - l$VOLUME_TOT)), " L"),
           `.tri:Restant (mesuré)` =
             pmax(0, l$VOLUME_BRASSIN * rend$RENDEMENT - l$VOLUME_TOT),
           `Fin (rythme)` = fmt(nv[2]), `.tri:Fin (rythme)` = as.integer(nv[2]),
           Fourchette = paste0(fmt(nv[1]), " → ", fmt(nv[3])),
           `Fin (actuel)` = fmt(hw[2]), `.tri:Fin (actuel)` = as.integer(hw[2]))
  })
  bind_rows(lignes) %>% arrange(`.tri:Fin (rythme)`)
}
