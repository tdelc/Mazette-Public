# Volet « Projection » : compléter un mois dont la comptabilité n'est pas
# encore entière.
#
# Le besoin est né d'un fait : depuis juillet 2026, les rémunérations ne sont
# pas encodées. Les mois existent, leurs ventes existent, leurs achats
# existent — il ne manque qu'un poste, et ce poste suffit à rendre la marge
# fausse de bout en bout. Le dashboard savait DÉTECTER ces mois (le topo) et
# les ÉCARTER (l'onglet Contrôle) ; il ne savait pas dire ce qu'ils auraient
# donné.
#
# ---------------------------------------------------------------------------
# Trois règles, et elles ne se négocient pas
# ---------------------------------------------------------------------------
#
#   1. RIEN NE SORT D'ICI. La projection ne touche ni DB_COMPTA, ni la compta
#      retenue, ni aucun autre onglet. Un chiffre modélisé qui s'échapperait
#      dans l'onglet Exploitation y serait indiscernable d'un chiffre réel, et
#      le jour où quelqu'un le lirait sans le savoir, tout le reste deviendrait
#      douteux. Ce fichier ne produit donc qu'un AFFICHAGE.
#
#   2. CHAQUE CHIFFRE DIT D'OÙ IL VIENT. Toute valeur projetée porte sa méthode
#      et sa base de calcul, affichées à côté d'elle. Une estimation qu'on ne
#      peut pas contester n'est pas une estimation, c'est une rumeur.
#
#   3. ON MODÉLISE CE QU'ON SAIT MESURER AILLEURS. Les quatre postes projetés
#      ont chacun une source indépendante de la comptabilité :
#
#        CA             les ventes de caisse du mois (DB_KPI_SIMPLE)
#        Matières       le ratio médian matières / CA des mois de référence
#        Rémunérations  ce qui a été MESURÉ ce mois-là — la paie, à défaut les
#                       heures pointées, à défaut les heures planifiées —
#                       recalé sur la comptabilité (cf. PROJ_SOURCES_REMU)
#        Frais généraux la médiane des mois de référence
#
#      Les autres postes — autres produits, amortissements, financier — sont
#      repris tels quels. On ne les invente pas : ils ne se déduisent de rien.
#
# ---------------------------------------------------------------------------
# Pourquoi la MÉDIANE partout
# ---------------------------------------------------------------------------
# Une moyenne suit le mois exceptionnel ; la médiane l'ignore. Or les mois de
# référence d'une brasserie en comptent toujours un : une régularisation de
# paie, un gros achat de matériel passé en frais généraux, un mois de
# fermeture. La moyenne de six mois dont un double est fausse de 17 % ; la
# médiane ne bouge pas. C'est le même choix que le contrôle de vraisemblance
# (médiane ± MAD, cf. R/exploitation.R), et pour la même raison.

#### Ce qu'on sait projeter ####

# Une ligne par poste projetable. CLE pilote les cases à cocher, BLOC désigne
# la colonne de postes_exploitation() à remplacer.
#
# L'ORDRE COMPTE : le CA se projette en premier, parce que les matières se
# calculent sur le CA retenu — projeté s'il l'est, réel sinon.
PROJ_POSTES <- tibble::tribble(
  ~CLE,           ~BLOC,          ~LIBELLE,             ~SOURCE,
  "ca",           "CA",           "Chiffre d'affaires", "les ventes de caisse",
  "matieres",     "MATIERES",     "Matières premières", "le ratio au CA",
  "remuneration", "REMUNERATION", "Rémunérations",      "les heures planifiées",
  "generaux",     "GENERAUX",     "Frais généraux",     "les mois précédents")

# Combien de mois la trajectoire montre. Partagé par le graphe, le tableau de
# résultat et le sélecteur de la cascade : les trois doivent parler des mêmes
# mois, sinon on choisit dans la liste un mois qui n'est pas sur la courbe.
PROJ_MOIS_GRAPHE <- 14

# Nombre de mois de référence par défaut. Six : assez pour qu'une médiane ait
# un sens, assez peu pour rester dans la saison en cours.
PROJ_DEFAUT_REF <- 6

# En-dessous de trois mois de référence, un ratio médian n'est plus une
# statistique, c'est une anecdote. On projette quand même — mais l'écran le
# dit, et le tableau le signale.
PROJ_MIN_REF <- 3

# À partir de quel montant un poste est réputé PRÉSENT dans un mois. Un euro :
# en comptabilité, un poste vide vaut exactement zéro ; le seuil n'est là que
# pour absorber un arrondi.
PROJ_SEUIL_PRESENT <- 1

#### Les sources extérieures à la comptabilité ####

# Les jours d'ouverture HABITUELS, par jour de semaine.
#
# Il faut ce repère pour savoir ce qu'un mois en cours a encore devant lui :
# le 3 novembre, on ne sait pas combien de jours il reste à faire si on ne
# sait pas lesquels sont ouverts. Un jour de semaine est réputé ouvert s'il
# l'a été plus d'une fois sur deux sur la fenêtre regardée — la règle encaisse
# un lundi férié sans se retourner.
jours_ouverts_habituels <- function(journalier, avant, semaines = 12) {
  f <- journalier %>%
    filter(DATE < as.Date(avant),
           DATE >= as.Date(avant) - 7 * semaines)
  if (!nrow(f)) return(1:7)
  f %>% group_by(J = wday(DATE)) %>%
    summarise(PART = mean(V > 0), .groups = "drop") %>%
    filter(PART >= 0.5) %>% pull(J)
}

# Ventes de caisse par mois, HTVA, et le rythme du mois en cours.
#
# HTVA et pas TVAC, sans égard pour le sélecteur de TVA du dashboard : ici on
# compare à des comptes 70, qui sont hors taxe par construction. Laisser
# l'utilisateur choisir ferait apparaître un CA projeté 21 % trop haut sans
# que rien ne le signale.
#
# ATTENDUS / ECOULES / FACTEUR : LE mois en cours n'est pas un mois. Le 3
# novembre, sa caisse annonce deux jours de ventes — et les rémunérations, les
# frais généraux, les matières qu'on projetterait en face, eux, valent pour un
# mois entier. Un CA à date confronté à des charges à mois plein donne une
# marge catastrophique et parfaitement fausse. On porte donc le CA du mois en
# cours à son mois complet, au prorata des jours d'ouverture.
#
# Un mois CLOS n'est jamais extrapolé, même s'il compte moins de jours
# d'ouverture que d'habitude : il a été ce qu'il a été, et une fermeture
# exceptionnelle n'est pas un trou à combler.
ventes_par_mois <- function(db_kpi, colonne = "CA_HTVA",
                            aujourd_hui = today()) {
  if (is.null(db_kpi) || !is.data.frame(db_kpi) || !nrow(db_kpi) ||
      !all(c("DATE", colonne) %in% names(db_kpi)))
    return(NULL)
  j <- db_kpi %>%
    transmute(DATE = as.Date(DATE),
              V = suppressWarnings(as.numeric(.data[[colonne]]))) %>%
    filter(!is.na(DATE), !is.na(V))
  if (!nrow(j)) return(NULL)

  aujourd_hui <- as.Date(aujourd_hui)
  en_cours <- floor_date(aujourd_hui, "month")
  ouverts  <- jours_ouverts_habituels(j, en_cours)
  # Arrêté à hier, comme partout ailleurs dans le dashboard : la journée en
  # cours n'est pas finie, et son CA partiel ferait baisser le rythme sur
  # lequel tout le mois est porté.
  j <- j %>% filter(DATE < aujourd_hui)
  if (!nrow(j)) return(NULL)

  j %>%
    mutate(PERIODE = floor_date(DATE, "month")) %>%
    group_by(PERIODE) %>%
    summarise(VENTES = sum(V, na.rm = TRUE), JOURS = sum(V > 0),
              .groups = "drop") %>%
    mutate(
      EN_COURS = PERIODE == en_cours,
      ATTENDUS = vapply(PERIODE, function(m)
        sum(wday(seq(m, fin_periode(m, "mois"), by = "day")) %in% ouverts),
        numeric(1)),
      ECOULES  = ifelse(EN_COURS,
        vapply(PERIODE, function(m) {
          if (aujourd_hui <= m) return(0)
          d <- seq(m, min(fin_periode(m, "mois"), aujourd_hui - 1), by = "day")
          sum(wday(d) %in% ouverts)
        }, numeric(1)),
        ATTENDUS),
      FACTEUR = ifelse(!EN_COURS, 1,
                       ifelse(ECOULES > 0, ATTENDUS / ECOULES, NA_real_))) %>%
    arrange(PERIODE)
}

# Jusqu'où une source couvre chaque mois.
#
# Une source au grain JOUR peut s'arrêter en cours de mois — le calendrier de
# shifts ne garde que l'avenir proche, le pointage s'arrête à la date
# d'import. Un mois à moitié couvert donnerait une estimation amputée de
# moitié, et rien ne le dirait.
couverture_mois <- function(dates) {
  d <- as.Date(dates)
  d <- d[!is.na(d)]
  if (!length(d)) return(NULL)
  tibble(PERIODE = floor_date(d, "month"), DATE = d) %>%
    group_by(PERIODE) %>%
    summarise(DERNIER = max(DATE), .groups = "drop") %>%
    mutate(FIN = fin_periode(PERIODE, "mois"))
}

# Heures planifiées par mois, tous services confondus.
#
# Tous services, et non les seules heures « variables » du volet Planning : en
# face, le compte 62 paie aussi la cuisine, la brasserie et le support. Un
# ratio euro/heure calculé sur les seules heures de service serait juste, mais
# il ne voudrait rien dire.
# DERNIER : jusqu'où le planning du mois va. Le calendrier de shifts ne garde
# que l'avenir proche, et un mois planifié jusqu'au 20 seulement donnerait des
# rémunérations amputées d'un tiers.
#
# On le SIGNALE, et on ne le redresse pas. Redresser au prorata supposerait
# que les jours manquants ressemblent aux précédents — alors qu'un mois dont
# le dernier jour est un lundi de fermeture paraîtrait tronqué tous les mois,
# et gonflerait de 3 % en silence. Un biais silencieux coûte plus cher qu'une
# estimation basse qu'on voit.
heures_par_mois <- function(db_planning) {
  if (!planning_valide(db_planning)) return(NULL)
  d <- db_planning %>%
    transmute(DATE = as.Date(DATE),
              PERIODE = floor_date(as.Date(DATE), "month"), HEURES) %>%
    filter(!is.na(PERIODE))
  if (!nrow(d)) return(NULL)
  d %>%
    group_by(PERIODE) %>%
    summarise(HEURES = sum(HEURES, na.rm = TRUE), .groups = "drop") %>%
    filter(HEURES > 0) %>%
    left_join(couverture_mois(d$DATE), by = "PERIODE") %>%
    arrange(PERIODE)
}

#### Les sources de la rémunération, de la meilleure à la dernière ####

# On ne modélise les rémunérations que si quelque chose les a MESURÉES. Trois
# sources le font, et elles ne se valent pas :
#
#   la PAIE (DB_ONSS) mesure le coût employeur du mois, c'est-à-dire presque
#   exactement ce qu'on cherche. Il ne manque que ce que la comptabilité y
#   ajoute — provisions, pécules — et c'est un rapport stable, donc modélisable ;
#
#   le POINTAGE (DB_COUTS_TRAVAIL) mesure les heures réellement faites. Il
#   ignore les charges patronales et le personnel non pointé, mais un coût
#   horaire médian les rattrape ;
#
#   le PLANNING (DB_HEURES_PLANNING) annonce les heures qu'on s'apprête à
#   poser. C'est une prévision, pas une mesure — mais c'est la seule source
#   qui existe pour un mois qui n'est pas fini.
#
# D'où la règle de choix : sur un mois CLOS, on descend la liste et les
# sources qui MESURENT répondent les premières — préférer le planning à un
# pointage, ce serait préférer ce qu'on avait prévu à ce qui s'est passé. Le
# planning ne répond alors que si aucune des deux n'est là. Sur un mois EN
# COURS, les mesures sont forcément partielles (le pointage s'arrête hier) :
# elles sont écartées, et le planning, qui couvre le mois entier, reste seul.
#
# Le recalage est propre à chaque source : le rapport médian se calcule
# toujours entre LA MÊME source et le compte 62 des mois de référence. Un
# coût horaire calculé sur des heures pointées ne s'applique pas à des heures
# planifiées, qui n'en sont pas le même compte.
PROJ_SOURCES_REMU <- tibble::tribble(
  ~CLE,       ~LIBELLE,            ~UNITE, ~QUOI,                ~MOIS_EN_COURS,
  "paie",     "Coût de la paie",   "euro", "de coût employeur",  FALSE,
  "horeko",   "Heures pointées",   "heure", "h pointées",        FALSE,
  "planning", "Heures planifiées", "heure", "h planifiées",      TRUE)

#' Les trois sources, mises à la même forme : PERIODE, VALEUR, et la
#' couverture du mois.
#'
#' sources_mensuelles() (R/sources_travail.R) fait déjà le gros du travail
#' pour la paie et le pointage — leurs colonnes changent de nom d'un import à
#' l'autre, et cette fonction-là connaît le piège. En refaire une ici en
#' aurait fait deux à maintenir.
sources_remuneration <- function(db_couts = NULL, db_onss = NULL,
                                 db_planning = NULL) {
  src <- list(paie = NULL, horeko = NULL, planning = NULL)

  utilisable <- !is.null(db_couts) && is.data.frame(db_couts) &&
    nrow(db_couts) > 0 && "DATE" %in% names(db_couts) &&
    any(c("HEURES", "HEURES_HOREKO") %in% names(db_couts))
  if (utilisable) {
    m <- sources_mensuelles(db_couts, db_onss)
    cv <- couverture_mois(db_couts$DATE)
    if (!is.null(m)) {
      h <- m %>% filter(SOURCE == "horeko", !is.na(HEURES), HEURES > 0) %>%
        transmute(PERIODE, VALEUR = HEURES)
      if (nrow(h)) src$horeko <- h %>% left_join(cv, by = "PERIODE")
      # La paie est mensuelle : présente, elle couvre son mois entier.
      p <- m %>% filter(SOURCE == "paie", !is.na(COUT), COUT > 0) %>%
        transmute(PERIODE, VALEUR = COUT)
      if (nrow(p)) src$paie <- p %>%
        mutate(DERNIER = fin_periode(PERIODE, "mois"),
               FIN = fin_periode(PERIODE, "mois"))
    }
  }

  pl <- heures_par_mois(db_planning)
  if (!is.null(pl) && nrow(pl))
    src$planning <- pl %>% transmute(PERIODE, VALEUR = HEURES, DERNIER, FIN)
  src
}

#### Les mois de référence ####

#' Les mois sur lesquels le modèle s'appuie, pour UN poste.
#'
#' Trois filtres, et chacun a sa raison :
#'   - antérieurs au mois cible : on ne modélise pas avec l'avenir ;
#'   - ni écartés, ni eux-mêmes projetés : un mois dont on doute ne peut pas
#'     servir de référence, sinon le doute se propage sans qu'on le voie ;
#'   - le poste y est présent : un mois sans rémunérations ne dit rien du coût
#'     horaire. C'est le filtre qui compte le plus — sans lui, les mois
#'     incomplets qu'on cherche justement à combler tireraient la médiane vers
#'     zéro, et le modèle confirmerait le trou qu'il doit remplir.
mois_reference <- function(postes, cible, bloc = NULL, n = PROJ_DEFAUT_REF,
                           exclus = NULL, a_projeter = NULL) {
  if (is.null(postes) || !nrow(postes)) return(postes)
  d <- postes %>% filter(PERIODE < as.Date(cible))
  if (length(exclus))     d <- d %>% filter(!PERIODE %in% as.Date(exclus))
  if (length(a_projeter)) d <- d %>% filter(!PERIODE %in% as.Date(a_projeter))
  if (!is.null(bloc) && bloc %in% names(d))
    d <- d %>% filter(!is.na(.data[[bloc]]),
                      abs(.data[[bloc]]) > PROJ_SEUIL_PRESENT)
  d %>% arrange(desc(PERIODE)) %>% head(n) %>% arrange(PERIODE)
}

#### Les quatre estimateurs ####

# Chacun rend la même chose : une valeur, la méthode qui l'a produite, la base
# de calcul en clair, et le nombre de mois de référence derrière. Le tableau
# affiche les quatre colonnes telles quelles — c'est la règle 2.
proj_valeur <- function(valeur, methode, detail, n_ref = 0L) {
  list(VALEUR = as.numeric(valeur), METHODE = methode,
       DETAIL = detail, N_REF = as.integer(n_ref))
}

# Un taux au centime : « 24,30€/h » et pas « 20€/h ». format_CA(x, -1)
# arrondirait à la dizaine d'euros, ce qui sur un taux horaire est absurde.
proj_taux <- function(x) paste0(euro_compta(x), "/h")

# Un rapport sans unité (le calibrage caisse -> compta), à trois décimales.
proj_facteur <- function(x) sub(".", ",", format(round(x, 3), nsmall = 3),
                                fixed = TRUE)

#' CA : les ventes de caisse du mois, recalées sur la comptabilité.
#'
#' La caisse et le compte 70 ne comptent jamais tout à fait la même chose —
#' offerts, arrondis, régularisations, ventes encaissées à cheval sur deux
#' mois. L'écart est petit mais systématique, donc modélisable : on prend le
#' rapport médian compta / caisse des mois de référence et on l'applique. Sans
#' ce recalage, le CA projeté porterait un biais connu qu'on aurait choisi
#' d'ignorer.
projette_ca <- function(cible, ventes, ref) {
  cible <- as.Date(cible)
  ligne <- if (is.null(ventes)) NULL else ventes[ventes$PERIODE == cible, ]
  v <- if (is.null(ligne) || !nrow(ligne)) numeric(0) else ligne$VENTES[1]

  # Un mois à peine commencé n'a rien à extrapoler : zéro jour écoulé ne
  # donne aucun rythme. Plutôt que d'inventer un facteur, on reconduit la
  # médiane — et la méthode affichée change, pour qu'on le sache.
  if (!length(v) || is.na(v) || v <= 0 ||
      (isTRUE(ligne$EN_COURS[1]) && is.na(ligne$FACTEUR[1]))) {
    if (!nrow(ref))
      return(proj_valeur(NA_real_, "Ventes de caisse",
                         "Aucune vente de caisse sur ce mois."))
    return(proj_valeur(median(ref$CA), "Médiane des mois précédents",
      paste0("Médiane de ", nrow(ref), " mois (mois à peine commencé : ",
             "aucun jour d'ouverture écoulé)"), nrow(ref)))
  }

  cal <- ref %>% inner_join(ventes, by = "PERIODE") %>%
    filter(VENTES > 0, CA > 0)
  k <- if (nrow(cal) >= PROJ_MIN_REF) median(cal$CA / cal$VENTES) else NA_real_
  f <- if (is.null(ligne$FACTEUR[1]) || is.na(ligne$FACTEUR[1])) 1
       else ligne$FACTEUR[1]
  rythme <- if (f == 1) "" else
    paste0(" sur ", round(ligne$ECOULES[1]), " jours d'ouverture, portés à ",
           round(ligne$ATTENDUS[1]), " jours")

  if (is.na(k))
    return(proj_valeur(v * f, "Ventes de caisse",
      paste0(euro_exact(v), " encaissés", rythme,
             ", sans recalage : moins de ", PROJ_MIN_REF,
             " mois de référence.")))
  proj_valeur(v * f * k,
    if (f == 1) "Ventes de caisse" else "Ventes de caisse, mois extrapolé",
    paste0(euro_exact(v), " encaissés", rythme, " x ", proj_facteur(k),
           " (rapport médian compta/caisse sur ", nrow(cal), " mois)"),
    nrow(cal))
}

#' Matières : le ratio médian au CA, appliqué au CA retenu.
#'
#' Le ratio matière est la grandeur la plus stable de la maison — c'est la
#' recette, pas la fréquentation. D'où le choix de le modéliser lui, et non le
#' montant : un mois à +20 % de CA doit porter +20 % de matières, ce qu'une
#' médiane de montants raterait complètement.
projette_matieres <- function(ca, ref) {
  r <- ref %>% filter(CA > PROJ_SEUIL_PRESENT) %>% mutate(R = MATIERES / CA)
  if (!nrow(r) || is.na(ca))
    return(proj_valeur(NA_real_, "Ratio médian au CA",
                       "Pas de mois de référence avec un CA."))
  m <- median(r$R)
  proj_valeur(ca * m, "Ratio médian au CA",
    paste0(format_pct(100 * m), " du CA, médiane de ", nrow(r), " mois, ",
           "appliquée à ", euro_exact(ca)),
    nrow(r))
}

#' Rémunérations : ce qui a été mesuré ce mois-là, recalé sur le compte 62.
#'
#' On descend PROJ_SOURCES_REMU jusqu'à ce qu'une source réponde. Pour chacune,
#' le recalage se fait entre elle-même et le compte 62 des mois de référence :
#' un coût horaire calculé sur des heures pointées ne s'applique pas à des
#' heures planifiées, qui ne comptent pas la même chose.
#'
#' Sur un mois en cours, les sources qui MESURENT sont forcément partielles —
#' le pointage s'arrête hier. Elles sont donc écartées, et c'est le planning,
#' puis la médiane, qui répondent.
#'
#' Dernier recours : la médiane des montants. L'écran le dit, la méthode
#' affichée change.
projette_remuneration <- function(cible, sources, ref, aujourd_hui = today()) {
  cible <- as.Date(cible)
  clos  <- fin_periode(cible, "mois") < as.Date(aujourd_hui)
  if (is.null(sources) || !is.list(sources)) sources <- list()
  raisons <- character()

  for (i in seq_len(nrow(PROJ_SOURCES_REMU))) {
    s   <- PROJ_SOURCES_REMU[i, ]
    tbl <- sources[[s$CLE]]
    if (is.null(tbl) || !nrow(tbl)) next
    if (!clos && !s$MOIS_EN_COURS) {
      raisons <- c(raisons, paste0(tolower(s$LIBELLE),
                                   " : le mois n'est pas fini"))
      next
    }
    ligne <- tbl[tbl$PERIODE == cible, ]
    if (!nrow(ligne) || is.na(ligne$VALEUR[1]) || ligne$VALEUR[1] <= 0) next
    v <- ligne$VALEUR[1]

    cal <- ref %>% inner_join(tbl, by = "PERIODE") %>%
      filter(VALEUR > 0, REMUNERATION > PROJ_SEUIL_PRESENT)
    if (nrow(cal) < PROJ_MIN_REF) {
      raisons <- c(raisons, paste0(tolower(s$LIBELLE), " : ", nrow(cal),
                                   " mois de référence seulement"))
      next
    }
    r <- median(cal$REMUNERATION / cal$VALEUR)

    # Une source au grain jour qui s'arrête avant la fin du mois : on le dit.
    # L'estimation est alors basse de ce qui manque — et la redresser au
    # prorata supposerait que les jours absents ressemblent aux autres.
    tronque <- !is.null(ligne$DERNIER) && !is.na(ligne$DERNIER[1]) &&
      ligne$DERNIER[1] < ligne$FIN[1]
    base <- if (s$UNITE == "euro")
      paste0(euro_exact(v), " ", s$QUOI, " x ", proj_facteur(r),
             " (rapport médian compta/paie sur ", nrow(cal), " mois)")
    else
      paste0(round(v), " ", s$QUOI, " x ", proj_taux(r),
             " (coût horaire médian sur ", nrow(cal), " mois)")
    return(proj_valeur(v * r, s$LIBELLE,
      paste0(base, if (tronque) paste0(" — source arrêtée au ",
                                       format(ligne$DERNIER[1], "%d/%m")) else ""),
      nrow(cal)))
  }

  if (!nrow(ref))
    return(proj_valeur(NA_real_, "Médiane des mois précédents",
                       "Aucun mois de référence avec des rémunérations."))
  if (!length(raisons)) raisons <- "aucune source d'heures ou de paie"
  proj_valeur(median(ref$REMUNERATION), "Médiane des mois précédents",
    paste0("Médiane de ", nrow(ref), " mois (", paste(raisons, collapse = " ; "),
           ")"), nrow(ref))
}

#' Frais généraux : la médiane des mois de référence.
#'
#' Aucune source extérieure ne les annonce — assurances, énergie, honoraires,
#' entretien tombent quand ils tombent. La médiane est ici le moins mauvais
#' estimateur, et surtout le seul honnête : on ne prétend pas prévoir, on
#' reconduit.
projette_generaux <- function(ref) {
  if (!nrow(ref))
    return(proj_valeur(NA_real_, "Médiane des mois précédents",
                       "Aucun mois de référence."))
  proj_valeur(median(ref$GENERAUX), "Médiane des mois précédents",
    paste0("Médiane de ", nrow(ref), " mois"), nrow(ref))
}

#### L'orchestration ####

#' Projette UN mois, poste par poste.
#'
#' @param postes sortie de postes_exploitation() sur la compta brute.
#' @param cible premier jour du mois à projeter.
#' @param cles les postes à projeter (clés de PROJ_POSTES).
#' @param ventes sortie de ventes_par_mois(), ou NULL.
#' @param sources sortie de sources_remuneration(), ou NULL.
#' @param ajustements vecteur nommé par clé, en % : l'hypothèse de
#'   l'utilisateur, appliquée APRÈS le modèle. Le tableau montre les deux.
#' @return une ligne par poste projeté.
projette_mois <- function(postes, cible, cles, ventes = NULL, sources = NULL,
                          n_ref = PROJ_DEFAUT_REF, exclus = NULL,
                          a_projeter = NULL, ajustements = NULL) {
  vide <- tibble(PERIODE = as.Date(character()), CLE = character(),
                 BLOC = character(), LIBELLE = character(), REEL = numeric(),
                 MODELE = numeric(), AJUST = numeric(), RETENU = numeric(),
                 METHODE = character(), DETAIL = character(),
                 N_REF = integer())
  cles <- intersect(PROJ_POSTES$CLE, cles)   # l'ordre de PROJ_POSTES, toujours
  if (is.null(postes) || !length(cles)) return(vide)
  cible <- as.Date(cible)

  reel <- postes %>% filter(PERIODE == cible)
  lire <- function(bloc) if (nrow(reel)) as.numeric(reel[[bloc]][1]) else 0
  aj <- function(cle) {
    a <- if (is.null(ajustements)) NULL else ajustements[[cle]]
    if (is.null(a) || is.na(a)) 0 else as.numeric(a)
  }

  # Le CA retenu sert de base aux matières. Il part du réel et devient le
  # projeté dès que le CA est lui-même projeté — c'est pour cela que l'ordre
  # de PROJ_POSTES n'est pas décoratif.
  ca_retenu <- lire("CA")
  lignes <- list()

  for (cle in cles) {
    bloc <- PROJ_POSTES$BLOC[PROJ_POSTES$CLE == cle]
    ref  <- mois_reference(postes, cible, bloc, n_ref, exclus, a_projeter)
    r <- switch(cle,
      ca           = projette_ca(cible, ventes, ref),
      matieres     = projette_matieres(ca_retenu, ref),
      remuneration = projette_remuneration(cible, sources, ref),
      generaux     = projette_generaux(ref))

    a <- aj(cle)
    retenu <- r$VALEUR * (1 + a / 100)
    if (cle == "ca" && !is.na(retenu)) ca_retenu <- retenu

    lignes[[length(lignes) + 1L]] <- tibble(
      PERIODE = cible, CLE = cle, BLOC = bloc,
      LIBELLE = PROJ_POSTES$LIBELLE[PROJ_POSTES$CLE == cle],
      REEL = lire(bloc), MODELE = r$VALEUR, AJUST = a, RETENU = retenu,
      METHODE = r$METHODE, DETAIL = r$DETAIL, N_REF = r$N_REF)
  }
  bind_rows(lignes)
}

#' La même chose sur plusieurs mois.
#'
#' Chaque mois se projette sans les autres : ses références excluent TOUS les
#' mois projetés, y compris ceux qui le précèdent. Sinon juillet servirait de
#' référence à août, et le modèle se nourrirait de lui-même — trois mois plus
#' loin, plus personne ne saurait sur quoi repose le chiffre.
projette_mois_multiples <- function(postes, mois, cles, ventes = NULL,
                                    sources = NULL, n_ref = PROJ_DEFAUT_REF,
                                    exclus = NULL, ajustements = NULL) {
  mois <- sort(as.Date(mois))
  if (!length(mois)) return(projette_mois(postes, Sys.Date(), character()))
  bind_rows(lapply(mois, function(m)
    projette_mois(postes, m, cles, ventes, sources, n_ref, exclus,
                  a_projeter = mois, ajustements = ajustements)))
}

#### Du poste projeté au compte d'exploitation ####

# Les colonnes dérivées de postes_exploitation(), recalculées après
# substitution.
#
# Ce calcul EXISTE DÉJÀ dans postes_exploitation() et il est reproduit ici :
# c'est le genre de doublon qui diverge un jour en silence. Le garde-fou est
# un test — test_projection.R applique une projection VIDE à une vraie sortie
# de postes_exploitation() et exige que tout retombe au centime. Le jour où
# l'une des deux formules bouge, la suite devient rouge.
recalcule_gestion <- function(p) {
  p %>% mutate(
    PRODUITS      = CA + AUTRES,
    CHARGES       = MATIERES + REMUNERATION + GENERAUX + AMORTISSEMENT,
    MARGE         = PRODUITS - CHARGES,
    MARGE_AA      = PRODUITS - MATIERES - REMUNERATION - GENERAUX + FINANCIER,
    PCT_MATIERES  = ratio_pct(MATIERES, CA),
    PCT_TRAVAIL   = ratio_pct(REMUNERATION, CA),
    PCT_PRIME     = ratio_pct(MATIERES + REMUNERATION, CA),
    PCT_GENERAUX  = ratio_pct(GENERAUX, CA),
    PCT_MARGE     = ratio_pct(MARGE, CA),
    PCT_MARGE_AA  = ratio_pct(MARGE_AA, CA))
}

#' Le compte d'exploitation tel qu'il serait, projection appliquée.
#'
#' Un mois projeté qui n'existe pas encore dans la comptabilité est CRÉÉ à
#' zéro puis rempli : c'est le cas d'un mois dont rien n'est encore encodé, et
#' c'est précisément celui où la projection sert le plus.
applique_projection <- function(postes, proj) {
  if (is.null(postes)) return(NULL)
  if (is.null(proj) || !nrow(proj))
    return(postes %>% mutate(PROJETE = FALSE))

  mois <- sort(unique(proj$PERIODE))
  manquants <- setdiff(mois, postes$PERIODE)
  if (length(manquants)) {
    neuf <- postes[0, , drop = FALSE]
    neuf <- bind_rows(neuf, tibble(PERIODE = as.Date(manquants, origin = "1970-01-01")))
    num <- setdiff(names(postes), "PERIODE")
    for (col in num) neuf[[col]] <- 0
    postes <- bind_rows(postes, neuf) %>% arrange(PERIODE)
  }

  for (i in seq_len(nrow(proj))) {
    if (is.na(proj$RETENU[i])) next
    j <- which(postes$PERIODE == proj$PERIODE[i])
    if (length(j)) postes[[proj$BLOC[i]]][j] <- proj$RETENU[i]
  }
  recalcule_gestion(postes) %>% mutate(PROJETE = PERIODE %in% mois)
}

#### Ce qui manque, et qu'on propose de combler ####

#' Les postes absents d'un mois alors qu'ils devraient y être.
#'
#' « Absent » et pas « bizarre » : un poste à zéro là où les mois voisins en
#' portent des milliers d'euros n'est pas une anomalie de gestion, c'est une
#' écriture qui n'est pas passée. C'est ce signal-là, et lui seul, qui
#' préselectionne les cases de l'écran — le reste est à l'utilisateur.
#'
#' @return une ligne par mois x poste manquant.
postes_manquants <- function(postes, depuis = NULL) {
  vide <- tibble(PERIODE = as.Date(character()), CLE = character(),
                 LIBELLE = character(), MEDIANE = numeric())
  if (is.null(postes) || !nrow(postes)) return(vide)
  d <- if (is.null(depuis)) postes else
    postes %>% filter(PERIODE >= as.Date(depuis))
  if (!nrow(d)) return(vide)

  res <- lapply(seq_len(nrow(PROJ_POSTES)), function(i) {
    bloc <- PROJ_POSTES$BLOC[i]
    v <- postes[[bloc]]
    # La médiane se prend sur les mois où le poste EST là : sinon trois mois
    # troués sur six la ramènent à zéro, et plus rien ne paraît manquer.
    presents <- v[abs(v) > PROJ_SEUIL_PRESENT]
    if (length(presents) < PROJ_MIN_REF) return(NULL)
    med <- median(presents)
    tibble(PERIODE = d$PERIODE[abs(d[[bloc]]) <= PROJ_SEUIL_PRESENT],
           CLE = PROJ_POSTES$CLE[i], LIBELLE = PROJ_POSTES$LIBELLE[i],
           MEDIANE = med)
  })
  res <- bind_rows(res)
  if (!nrow(res)) vide else res %>% arrange(desc(PERIODE), CLE)
}

#### Le contrôle du modèle ####

#' Ce que le modèle aurait prédit sur des mois qu'on connaît.
#'
#' Un estimateur sans mesure d'erreur ne se discute pas. On rejoue donc le
#' modèle sur les derniers mois COMPLETS, chacun projeté comme s'il était
#' inconnu — ses propres références s'arrêtant avant lui — et on confronte au
#' réel. L'écart médian qui en sort est la seule chose qui permette de dire si
#' un chiffre projeté vaut 5 % près ou 30 %.
controle_modele <- function(postes, cles, ventes = NULL, sources = NULL,
                            n_ref = PROJ_DEFAUT_REF, exclus = NULL,
                            n_mois = 6) {
  vide <- tibble(PERIODE = as.Date(character()), CLE = character(),
                 LIBELLE = character(), REEL = numeric(), MODELE = numeric(),
                 ECART = numeric(), ECART_PCT = numeric())
  cles <- intersect(PROJ_POSTES$CLE, cles)
  if (is.null(postes) || !nrow(postes) || !length(cles)) return(vide)

  if (is.null(exclus)) exclus <- as.Date(character())
  candidats <- postes %>% filter(!PERIODE %in% as.Date(exclus))
  candidats <- candidats %>% arrange(desc(PERIODE)) %>%
    head(n_mois + 1) %>% pull(PERIODE)
  # Le mois le plus ancien du lot n'a pas de passé à lui : il servirait de
  # témoin sans référence.
  candidats <- sort(candidats)
  if (length(candidats) < 2) return(vide)
  candidats <- candidats[-1]

  res <- lapply(candidats, function(m) {
    p <- projette_mois(postes, m, cles, ventes, sources, n_ref, exclus,
                       a_projeter = m)
    # On ne juge que les postes RÉELLEMENT présents ce mois-là : comparer un
    # modèle à un zéro manquant mesurerait le trou, pas le modèle.
    p %>% filter(abs(REEL) > PROJ_SEUIL_PRESENT, !is.na(MODELE)) %>%
      transmute(PERIODE, CLE, LIBELLE, REEL, MODELE,
                ECART = MODELE - REEL, ECART_PCT = ratio_pct(MODELE - REEL, abs(REEL)))
  })
  bind_rows(res)
}

#' L'écart médian absolu, par poste : la fiabilité en une ligne.
fiabilite_modele <- function(ctrl) {
  if (is.null(ctrl) || !nrow(ctrl))
    return(tibble(CLE = character(), LIBELLE = character(),
                  N = integer(), ECART_MEDIAN = numeric()))
  ctrl %>%
    filter(!is.na(ECART_PCT)) %>%
    group_by(CLE, LIBELLE) %>%
    summarise(N = n(), ECART_MEDIAN = median(abs(ECART_PCT)), .groups = "drop") %>%
    arrange(match(CLE, PROJ_POSTES$CLE))
}

#### Rendus ####

# Le signe d'un chiffre modélisé, partout dans l'écran. Un caractère plutôt
# qu'une couleur : un tableau se relit en noir et blanc, et se photocopie.
PROJ_MARQUE <- "≈ "

proj_marque_cellule <- function(valeur, periode, bloc, proj) {
  txt <- euro_exact(valeur)
  if (is.null(proj) || !nrow(proj)) return(txt)
  cle <- paste(periode, bloc)
  modelise <- cle %in% paste(proj$PERIODE[!is.na(proj$RETENU)],
                             proj$BLOC[!is.na(proj$RETENU)])
  ifelse(modelise, paste0(PROJ_MARQUE, txt), txt)
}

#' Les tuiles : ce que pèsent les mois choisis, une fois complétés.
#'
#' Le « ≈ » n'est pas décoratif : sur un mois où seules les rémunérations sont
#' projetées, le CA affiché est RÉEL. Une tuile « CA projeté » mentirait, et
#' c'est le genre de mensonge qui se recopie dans un mail.
kpi_projection <- function(p, mois, proj = NULL) {
  sel <- p %>% filter(PERIODE %in% as.Date(mois))
  if (!nrow(sel))
    return(div(class = "text-muted small p-2",
               "Choisissez un mois à projeter."))
  modelise <- function(bloc) {
    if (is.null(proj) || !nrow(proj)) return(FALSE)
    any(proj$BLOC == bloc & proj$PERIODE %in% sel$PERIODE & !is.na(proj$RETENU))
  }
  tuile <- function(bloc, libelle, couleur, icone) {
    v <- sum(sel[[bloc]], na.rm = TRUE)
    kpi_tile(paste0(if (modelise(bloc)) PROJ_MARQUE else "", euro_exact(v)),
             libelle, couleur, icone,
             sous_titre = format_pct(ratio_pct(v, sum(sel$CA, na.rm = TRUE))))
  }
  ca    <- sum(sel$CA, na.rm = TRUE)
  marge <- sum(sel$MARGE_AA, na.rm = TRUE)
  div(
    class = "kpi-grid",
    kpi_tile(paste0(if (modelise("CA")) PROJ_MARQUE else "", euro_exact(ca)),
             "Chiffre d'affaires", COUL_BRUN, "euro-sign",
             sous_titre = paste0(nrow(sel), " mois complété",
                                 if (nrow(sel) > 1) "s" else "")),
    tuile("MATIERES", "Matières", COUL_MATIERE, "carrot"),
    tuile("REMUNERATION", "Rémunérations", COUL_TRAVAIL, "user-group"),
    tuile("GENERAUX", "Frais généraux", COUL_NEUTRE, "file-invoice"),
    kpi_tile(paste0(PROJ_MARQUE, euro_exact(marge)), "Marge avant amort.",
             if (is.na(marge) || marge >= 0) COUL_VERT else COUL_ROUGE,
             "piggy-bank",
             sous_titre = paste(format_pct(ratio_pct(marge, ca)), "du CA")))
}

#' Le travail du modèle, ligne par ligne : c'est la pièce justificative.
table_projection_detail <- function(proj) {
  if (is.null(proj) || !nrow(proj))
    return(tibble(Projection = "Aucun poste à projeter."))
  proj %>%
    transmute(
      Mois = etiquette_periode(PERIODE, "mois"),
      `.tri:Mois` = as.integer(PERIODE),
      Poste = LIBELLE,
      Réel = euro_exact(REEL),
      `.tri:Réel` = REEL,
      Modèle = euro_exact(MODELE),
      `.tri:Modèle` = MODELE,
      Hypothèse = ifelse(AJUST == 0, "",
                         paste0(ifelse(AJUST > 0, "+", ""), round(AJUST), " %")),
      Retenu = euro_exact(RETENU),
      `.tri:Retenu` = RETENU,
      Méthode = METHODE,
      `Base de calcul` = DETAIL,
      # Le nombre de mois derrière la médiane, en clair : une projection
      # bâtie sur deux mois n'a pas le même poids qu'une bâtie sur six, et
      # c'est invisible sur le montant seul.
      Réf = ifelse(N_REF == 0, "—", as.character(N_REF)),
      `.tri:Réf` = N_REF)
}

#' Le compte d'exploitation mois par mois, projection comprise.
table_projection_resultat <- function(p, proj, n = PROJ_MOIS_GRAPHE) {
  if (is.null(p) || !nrow(p)) return(tibble(Résultat = "Aucune comptabilité."))
  d <- p %>% arrange(desc(PERIODE)) %>% head(n)
  cell <- function(col) proj_marque_cellule(d[[col]], d$PERIODE, col, proj)
  tibble(
    Mois = etiquette_periode(d$PERIODE, "mois"),
    `.tri:Mois` = as.integer(d$PERIODE),
    État = pastille(ifelse(d$PROJETE, "Projeté", "Réel"),
                    ifelse(d$PROJETE, COUL_AMBRE, COUL_VERT)),
    `Chiffre d'affaires` = cell("CA"),
    `.tri:Chiffre d'affaires` = d$CA,
    Matières = cell("MATIERES"),
    `.tri:Matières` = d$MATIERES,
    Rémunérations = cell("REMUNERATION"),
    `.tri:Rémunérations` = d$REMUNERATION,
    `Frais généraux` = cell("GENERAUX"),
    `.tri:Frais généraux` = d$GENERAUX,
    `Marge av. amort.` = euro_exact(d$MARGE_AA),
    `.tri:Marge av. amort.` = d$MARGE_AA,
    `% du CA` = format_pct(d$PCT_MARGE_AA),
    `.tri:% du CA` = d$PCT_MARGE_AA)
}

#' La trajectoire : la marge mois par mois, le projeté hachuré.
#'
#' Hachures et non seconde couleur : la couleur dit déjà le signe de la marge,
#' et lui faire dire deux choses à la fois la rend muette. La texture est le
#' seul canal libre.
graph_projection <- function(p, n = PROJ_MOIS_GRAPHE, source = "proj_traj") {
  if (is.null(p) || !nrow(p))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune comptabilité")))
  d <- p %>% arrange(PERIODE) %>% tail(n)
  # Des étiquettes de mois sur un axe « category » : plotly les RANGE PAR
  # ORDRE ALPHABÉTIQUE dès que deux traces les partagent, et la chronologie
  # part en « août, avr., déc., févr. ». L'ordre doit donc être imposé.
  axe <- etiquette_periode(d$PERIODE, "mois")
  coul <- ifelse(d$MARGE_AA >= 0, COUL_VERT, COUL_ROUGE)
  reels <- d %>% filter(!PROJETE)
  repere <- if (nrow(reels) >= 3) median(reels$MARGE_AA) else NA_real_

  g <- plot_ly(source = source) %>%
    add_trace(
      x = axe, y = d$MARGE_AA,
      type = "bar", name = "Marge avant amort.",
      marker = list(
        color = coul,
        pattern = list(shape = ifelse(d$PROJETE, "/", ""),
                       fgcolor = "#ffffff", bgcolor = coul,
                       size = 7, solidity = 0.35),
        line = list(color = coul, width = 1)),
      hovertemplate = paste0(
        axe, ifelse(d$PROJETE, " (projeté)", ""),
        "<br>Marge avant amort. : ", euro_exact(d$MARGE_AA),
        "<br>CA : ", euro_exact(d$CA), "<extra></extra>"))

  if (!is.na(repere))
    g <- g %>% add_trace(
      x = axe, y = rep(repere, nrow(d)), type = "scatter", mode = "lines",
      name = "Médiane des mois réels",
      line = list(color = COUL_NEUTRE, width = 1.5, dash = "dot"),
      hovertemplate = paste0("Médiane des mois réels : ",
                             euro_exact(repere), "<extra></extra>"))

  # event_register : sans lui, plotly n'envoie pas l'événement à Shiny et le
  # clic sur une barre ne change rien — en silence, comme toujours.
  g <- event_register(g, "plotly_click")

  g %>% layout(
    xaxis = list(title = "", type = "category",
                 categoryorder = "array", categoryarray = axe),
    yaxis = list(title = "Marge avant amortissements (€)", zeroline = TRUE,
                 zerolinecolor = "#8d7b68"),
    legend = list(orientation = "h", y = -0.2),
    margin = list(b = 60),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
}

# euro_signe() blanchit ce qui s'arrondit à zéro à la dizaine d'euros près :
# un écart de 2 € s'y affichait « + », un signe tout seul. Ici la colonne
# existe pour être lue à côté d'un pourcentage, on la formate donc à part.
proj_ecart <- function(x)
  ifelse(is.na(x), "—",
  ifelse(abs(x) < 5, "0 €",
         paste0(ifelse(x > 0, "+", "-"), trimws(format_CA(abs(x), -1)))))

#' Le contrôle du modèle, mois à mois.
table_controle_modele <- function(ctrl) {
  if (is.null(ctrl) || !nrow(ctrl))
    return(tibble(Contrôle = paste("Pas assez de mois complets pour éprouver",
                                   "le modèle.")))
  ctrl %>%
    arrange(desc(PERIODE), match(CLE, PROJ_POSTES$CLE)) %>%
    transmute(
      Mois = etiquette_periode(PERIODE, "mois"),
      `.tri:Mois` = as.integer(PERIODE),
      Poste = LIBELLE,
      Réel = euro_exact(REEL), `.tri:Réel` = REEL,
      Modèle = euro_exact(MODELE), `.tri:Modèle` = MODELE,
      Écart = proj_ecart(ECART), `.tri:Écart` = ECART,
      `Écart %` = ifelse(is.na(ECART_PCT), "—",
                         paste0(ifelse(ECART_PCT > 0, "+", ""),
                                round(ECART_PCT, 1), " %")),
      `.tri:Écart %` = ECART_PCT)
}

#' La fiabilité en une phrase par poste.
#'
#' Les seuils sont volontairement larges : on mesure ici la capacité à
#' annoncer un ordre de grandeur, pas à remplacer la comptabilité.
resume_fiabilite <- function(fiab) {
  if (is.null(fiab) || !nrow(fiab))
    return(div(class = "text-muted small",
               "Le modèle n'a pas encore assez de mois complets pour se",
               " mesurer."))
  div(
    class = "d-flex flex-wrap gap-2",
    lapply(seq_len(nrow(fiab)), function(i) {
      e <- fiab$ECART_MEDIAN[i]
      coul <- couleur_seuil(e, 5, 15)
      div(class = "small",
          style = paste0("border-left:3px solid ", coul, ";",
                         "padding:0.15rem 0.55rem;"),
          tags$b(fiab$LIBELLE[i]), tags$br(),
          span(style = paste0("color:", coul, ";font-weight:600"),
               paste0("± ", format_pct(e))),
          span(class = "text-muted",
               paste0(" sur ", fiab$N[i], " mois")))
    }))
}

#' Ce qui empêche le modèle de bien travailler, s'il y a lieu.
bandeau_projection <- function(proj, source_mesuree = TRUE) {
  if (is.null(proj) || !nrow(proj)) return(NULL)
  maigres <- proj %>% filter(!is.na(RETENU), N_REF > 0, N_REF < PROJ_MIN_REF)
  sans <- proj %>% filter(is.na(RETENU))
  msg <- c(
    if (!source_mesuree && "remuneration" %in% proj$CLE)
      paste0("Ni paie, ni pointage, ni planning : les rémunérations sont",
             " reconduites à la médiane, ce qui ignore l'activité du mois."),
    if (nrow(maigres))
      paste0(nrow(maigres), " projection", if (nrow(maigres) > 1) "s" else "",
             " repose", if (nrow(maigres) > 1) "nt" else "",
             " sur moins de ", PROJ_MIN_REF, " mois de référence."),
    if (nrow(sans))
      paste0(nrow(sans), " poste", if (nrow(sans) > 1) "s" else "",
             " n'a pas pu être projeté, faute de référence ou de source."))
  if (!length(msg)) return(NULL)
  bandeau_alerte(TRUE, paste(msg, collapse = " "),
                 titre = "Ce que le modèle ne sait pas faire",
                 couleur = COUL_AMBRE, icone = "circle-info")
}
