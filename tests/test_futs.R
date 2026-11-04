# La fin de fût : le modèle, et la vérification qui le juge.
#
# Trois choses se cassent en silence ici, et ce sont elles qu'on tient :
#
#   1. LA FUITE. Un back-test qui laisse entrer l'avenir ne mesure rien. La
#      prédiction au jour J doit être identique, que les jours suivants
#      existent ou non dans la table ;
#   2. LES JOURS SANS VENTE. DB_BIERES ne porte une ligne que les jours où la
#      bière s'est vendue. Lire le rythme sur les seules lignes présentes ne
#      compte que les bons jours, surestime la vitesse, et annonce la fin trop
#      tôt ;
#   3. LE RENDEMENT. BIERE_FINIE est un drapeau posé une fois pour toutes :
#      un fût qui ne sera fini que dans six mois le porte déjà. S'y fier en se
#      replaçant au passé ferait entrer dans les références un fût en cours,
#      avec son volume vendu tronqué.
suppressMessages({library(dplyr); library(tibble); library(tidyr)
                  library(lubridate); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"; COUL_MATIERE<-"#d3c0ac"
vecteur_jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi",
                   "dimanche")
balise <- function(...) structure(list(args = list(...)), class = "tag")
div<-balise; span<-balise; tagList<-balise; icon<-balise; h6<-balise
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise)
TRACES <- list(); MISE_EN_PAGE <- list()
plot_ly <- function(...) structure(list(), class = "p")
plotly_empty <- function(...) structure(list(vide = TRUE), class = "p")
add_trace <- function(p, ...) { TRACES[[length(TRACES)+1L]] <<- list(...); p }
layout <- function(p, ...) { MISE_EN_PAGE <<- list(...); p }

for (f in c("R/helpers.R", "R/theme.R", "R/kpi.R", "R/futs.R"))
  source(f, encoding = "UTF-8", local = TRUE)

#### Un fixture dont on connaît toutes les réponses ####

# Lundi fermé, samedi double, le reste à 1. Le profil attendu est donc
# 0, 1, 1, 1, 1, 2, 1 — de moyenne 1 sur les sept jours.
POIDS <- c(0, 1, 1, 1, 1, 2, 1)
RENDEMENT <- 0.80
BASE <- 10                                   # litres par « jour standard »

# Un fût : il commence le jour dit, vend BASE x poids chaque jour, et s'arrête
# quand il a donné VOLUME x RENDEMENT. Aucune part de hasard : la date de fin
# est calculable à la main, et c'est elle que le modèle doit retrouver.
fabrique_fut <- function(id, boisson, debut, volume, base = BASE,
                         rendement = RENDEMENT) {
  jours <- seq(as.Date(debut), by = "day", length.out = 120)
  v <- base * POIDS[wday(jours, week_start = 1)]
  cum <- cumsum(v)
  vendable <- volume * rendement
  fin <- which(cum >= vendable)[1]
  v <- v[1:fin]; v[fin] <- vendable - sum(v[-fin])
  tibble(ID_BRASSIN = id, BOISSON = boisson, DATE = jours[1:fin],
         VOLUME_JOUR = v, VOLUME_BRASSIN = volume) %>%
    filter(VOLUME_JOUR > 0) %>%                # comme DB_BIERES : pas de ligne
    mutate(VOLUME_TOT = cumsum(VOLUME_JOUR),   # les jours sans vente
           VOLUME_BRASSIN_AJUST = VOLUME_BRASSIN * 0.75,
           DIFF = VOLUME_BRASSIN_AJUST - VOLUME_TOT,
           BIERE_FINIE = TRUE)
}

# Six fûts terminés en 2024, plus un en cours.
FUTS <- bind_rows(
  fabrique_fut("B1", "Blonde", "2024-01-02", 300),
  fabrique_fut("B2", "Blonde", "2024-02-06", 300),
  fabrique_fut("B3", "Blonde", "2024-03-05", 400),
  fabrique_fut("B4", "IPA",    "2024-04-02", 300),
  fabrique_fut("B5", "IPA",    "2024-05-07", 400),
  fabrique_fut("B6", "IPA",    "2024-06-04", 300))
AUJ <- as.Date("2024-12-31")

FINS <- fins_observees(FUTS, AUJ)
ok("un fût terminé par ligne, du plus récent au plus ancien",
   nrow(FINS) == 6 && FINS$ID_BRASSIN[1] == "B6")
ok("la fin retenue est le dernier jour de vente",
   FINS$FIN[FINS$ID_BRASSIN == "B1"] ==
     max(FUTS$DATE[FUTS$ID_BRASSIN == "B1"]))
ok("le rendement est le volume vendu sur le volume brassé",
   all(abs(FINS$RENDEMENT - RENDEMENT) < 1e-9))

# LE piège du drapeau : B6 est marqué fini dans toute la table, y compris sur
# ses propres lignes de juin. Se replacer au 10 juin ne doit pas le compter.
ok("un fût encore en cours à la date demandée n'est pas une référence",
   { f <- fins_observees(FUTS, as.Date("2024-06-20"))
     !"B6" %in% f$ID_BRASSIN && "B5" %in% f$ID_BRASSIN })
ok("et un fût tout juste terminé attend d'être silencieux",
   { fin_b5 <- max(FUTS$DATE[FUTS$ID_BRASSIN == "B5"])
     f <- fins_observees(FUTS, fin_b5 + 2)
     !"B5" %in% f$ID_BRASSIN })

#### Le rendement ####

R <- rendements_observes(FINS)
ok("le rendement de la maison est la médiane des fûts",
   abs(R$RENDEMENT[R$BOISSON == "Toutes"] - RENDEMENT) < 1e-9)
ok("chaque bière a le sien dès qu'elle a assez de fûts",
   setequal(R$BOISSON, c("Toutes", "Blonde", "IPA")))
ok("un fût aberrant ne compte pas",
   { f <- FINS; f$RENDEMENT[1] <- 2.5   # volume brassé mal saisi
     r <- rendements_observes(f)
     r$N[r$BOISSON == "Toutes"] == 5 })
ok("une bière sans assez de fûts prend celui de la maison",
   { p <- rendement_pour(R, "Brune")
     abs(p$RENDEMENT - RENDEMENT) < 1e-9 && grepl("toutes bières", p$SOURCE) })
ok("et sans aucun historique, l'abattement supposé de l'import",
   { p <- rendement_pour(rendements_observes(FINS[0, ]), "Blonde")
     p$RENDEMENT == RENDEMENT_SUPPOSE })

#### Le profil de semaine ####

# Sur une maison qui a toujours une bière en perce — ce qui est le cas réel :
# vingt semaines pleines, au rythme du profil. Le fixture à six fûts, lui,
# ménage des trous entre deux brassins, et une semaine coupée en deux ne dit
# rien de la forme d'une semaine.
CONTINU <- tibble(DATE = seq(as.Date("2024-01-01"), by = "day",
                             length.out = 7 * 20)) %>%
  mutate(ID_BRASSIN = "C1", BOISSON = "Blonde", BIERE_FINIE = TRUE,
         VOLUME_BRASSIN = 10000,
         VOLUME_JOUR = BASE * POIDS[wday(DATE, week_start = 1)],
         VOLUME_TOT = cumsum(VOLUME_JOUR))

P <- profil_semaine_bieres(CONTINU, as.Date("2024-12-31"), n_semaines = 520)
ok("le profil pèse sept parts", abs(sum(P$POIDS) - 7) < 1e-9)
ok("le jour de fermeture pèse zéro", P$POIDS[P$JOUR == 1] == 0)
ok("et le samedi pèse le double d'un mardi",
   abs(P$POIDS[P$JOUR == 6] - 2 * P$POIDS[P$JOUR == 2]) < 1e-9)
ok("sans ventes, un profil neutre plutôt qu'une division par zéro",
   all(profil_semaine_bieres(FUTS[0, ])$POIDS == 1))
# Une semaine où la bière n'a coulé que trois jours ne pèse pas dans la forme
# de la semaine : elle dit qu'un fût s'y est terminé, rien de plus.
ok("une semaine tronquée est écartée",
   { tronque <- CONTINU %>%
       filter(!(DATE >= as.Date("2024-02-05") & DATE <= as.Date("2024-02-08")))
     p <- profil_semaine_bieres(tronque, as.Date("2024-12-31"), 520)
     abs(p$POIDS[p$JOUR == 6] - 2 * p$POIDS[p$JOUR == 2]) < 1e-9 })

# Les tests qui suivent éprouvent le RYTHME et la PRÉDICTION : on leur donne
# le profil exact, pour qu'un écart ne puisse venir que d'eux.
P <- tibble(JOUR = 1:7, POIDS = POIDS)

#### Le rythme, et les jours sans vente ####

s6 <- serie_quotidienne(FUTS, "B6", AUJ)
ok("la série est complétée jour par jour",
   nrow(s6) == as.numeric(max(s6$DATE) - min(s6$DATE)) + 1)
ok("un jour sans ligne devient un jour à zéro litre",
   any(s6$VOLUME_JOUR == 0))
ok("et le cumul vendu ne redescend jamais",
   all(diff(s6$VOLUME_TOT) >= 0))

ok("le rythme est lu en litres par jour standard",
   abs(rythme_fut(s6, P)$RYTHME - BASE) < 1e-9)
# LE point : sans complétion, les lundis manquants sortent d'eux-mêmes (poids
# nul), mais un jour d'ouverture sans vente, lui, gonflerait le rythme.
ok("un jour d'ouverture sans vente tire le rythme vers le bas",
   { creux <- s6; creux$VOLUME_JOUR[creux$DATE == creux$DATE[3]] <- 0
     rythme_fut(creux, P)$RYTHME <= rythme_fut(s6, P)$RYTHME })
ok("une soirée exceptionnelle ne décide pas de la date de fin",
   { pic <- s6; pic$VOLUME_JOUR[4] <- pic$VOLUME_JOUR[4] * 10
     abs(rythme_fut(pic, P)$RYTHME - rythme_fut(s6, P)$RYTHME) < 1e-9 })
ok("trois jours de vente au minimum",
   is.na(rythme_fut(head(s6, 2), P)$RYTHME))

#### L'épuisement, jour par jour ####

# Cent litres au rythme de dix, un mardi : mardi à vendredi donnent 40 L,
# samedi 20, dimanche 10 — il reste 30 L le lundi (fermé), puis mardi,
# mercredi, jeudi. La date se compte à la main, et elle tombe le jeudi.
ok("l'épuisement suit le calendrier, pas une division",
   date_epuisement(100, as.Date("2024-01-01"), 10, P) == as.Date("2024-01-11"))
ok("un fût déjà vide finit aujourd'hui",
   date_epuisement(0, as.Date("2024-01-01"), 10, P) == as.Date("2024-01-01"))
ok("un rythme nul ne donne pas de date",
   is.na(date_epuisement(100, as.Date("2024-01-01"), 0, P)))

#### La prédiction ####

# Le fixture est déterministe : le modèle doit retrouver la vraie fin, pile.
fin_b6 <- FINS$FIN[FINS$ID_BRASSIN == "B6"]
ref <- fins_observees(FUTS, as.Date("2024-06-01"))   # B1 à B4, pas B6
pred <- predit_fin_fut(FUTS, "B6", fin_b6 - 7, P, rendements_observes(ref))
ok("à sept jours de la fin, le modèle tombe juste", pred[2] == fin_b6)
ok("et la fourchette encadre l'estimation",
   !is.na(pred[1]) && !is.na(pred[3]) && pred[1] <= pred[2] && pred[2] <= pred[3])

# LA FUITE : la prédiction au jour J ne doit rien devoir aux jours suivants.
ok("la prédiction ignore l'avenir",
   { tronque <- FUTS %>% filter(DATE <= fin_b6 - 7)
     a <- predit_fin_fut(FUTS, "B6", fin_b6 - 7, P, rendements_observes(ref))
     b <- predit_fin_fut(tronque, "B6", fin_b6 - 7, P, rendements_observes(ref))
     identical(a, b) })
ok("un fût sans assez de jours ne reçoit pas de date",
   all(is.na(predit_fin_fut(FUTS, "B6", min(FUTS$DATE[FUTS$ID_BRASSIN=="B6"]) + 1,
                            P, R))))
ok("un brassin inconnu non plus",
   all(is.na(predit_fin_fut(FUTS, "B999", AUJ, P, R))))

#### Le back-test ####

BT <- backtest_futs(FUTS, horizons = c(3, 7),
                    modeles = list(rythme = list(LIBELLE = "Rythme",
                                                 FONCTION = predit_fin_fut)),
                    aujourd_hui = AUJ)
ok("une ligne par fût, horizon et modèle", nrow(BT) > 0 &&
     all(BT$MODELE == "rythme"))
ok("le point de vue est bien à H jours de la fin",
   all(as.numeric(BT$FIN_VRAIE - BT$VANTAGE) == BT$HORIZON))
ok("sur un fixture déterministe, le modèle ne se trompe pas",
   all(abs(BT$ERREUR) <= 1, na.rm = TRUE))
# La fourchette ne peut pas tout contenir sur un fixture sans hasard : son
# ingrédient principal, la dispersion du rendement, y vaut zéro. Ce qu'on
# tient ici, c'est qu'elle s'ÉLARGIT avec l'avance — une prédiction à deux
# semaines ne peut pas être aussi serrée qu'une prédiction à trois jours.
ok("la fourchette s'élargit à mesure qu'on s'y prend plus tôt",
   { l <- BT %>% filter(!is.na(TOT), !is.na(TARD)) %>%
       mutate(LARGEUR = as.numeric(TARD - TOT)) %>%
       group_by(HORIZON) %>% summarise(L = mean(LARGEUR), .groups = "drop")
     nrow(l) == 2 && l$L[l$HORIZON == 7] >= l$L[l$HORIZON == 3] })

# Un modèle muet doit être compté comme muet, pas ignoré.
BT2 <- backtest_futs(FUTS, horizons = c(3, 7), aujourd_hui = AUJ,
  modeles = list(rythme = list(LIBELLE = "Rythme", FONCTION = predit_fin_fut),
                 muet = list(LIBELLE = "Muet",
                             FONCTION = function(...) rep(as.Date(NA), 3))))
RES <- resume_backtest(BT2)
ok("le résumé compte les deux modèles", nrow(RES) == 4)
ok("un modèle qui se tait répond dans 0 % des cas",
   all(RES$REPOND[RES$MODELE == "muet"] == 0))
ok("et son erreur reste inconnue, pas nulle",
   all(is.na(RES$MAE[RES$MODELE == "muet"])))
ok("n_max ne garde que les fûts les plus récents",
   { b <- backtest_futs(FUTS, horizons = 7, n_max = 2, aujourd_hui = AUJ)
     length(unique(b$ID_BRASSIN)) <= 2 })

#### Les pires, et les rendus ####

PIRES <- pires_predictions(BT, "rythme", n = 3)
ok("un fût par ligne, le pire écart en tête",
   nrow(PIRES) <= 3 && !any(duplicated(PIRES$ID_BRASSIN)))
ok("sans back-test, les tableaux le disent plutôt que de rester vides",
   ncol(table_resume_backtest(NULL)) == 1 &&
     ncol(table_pires_futs(NULL)) == 1)
ok("le tableau de résumé porte ses colonnes de tri",
   { t <- table_resume_backtest(RES)
     any(startsWith(names(t), PREFIXE_TRI)) && nrow(t) == 4 })
ok("les tuiles disent le rendement observé face au supposé",
   { txt <- paste(unlist(kpi_backtest(RES, FINS, R)), collapse = " | ")
     grepl("Rendement observé", txt, fixed = TRUE) &&
       grepl("80 %", txt, fixed = TRUE) })
ok("le graphe trace une courbe par modèle",
   { TRACES <- list(); graph_backtest(RES); length(TRACES) == 2 })
ok("le profil de semaine se trace",
   { TRACES <- list(); graph_profil_semaine(P)
     length(TRACES[[1]]$y) == 7 })
ok("le rendement observé se trace avec ses deux repères",
   { TRACES <- list(); graph_rendements(FINS, R); length(TRACES) == 3 })

#### La table des fûts en cours ####

EN_COURS <- bind_rows(
  FUTS,
  fabrique_fut("B7", "Blonde", "2024-12-03", 300) %>% mutate(BIERE_FINIE = FALSE))
T7 <- table_fins_prevues(EN_COURS, NULL, P, R, aujourd_hui = as.Date("2024-12-10"))
ok("le fût en cours y figure, avec ses deux volumes restants",
   nrow(T7) == 1 && all(c("Restant (0,75)", "Restant (mesuré)") %in% names(T7)))
ok("et le restant mesuré dépasse celui de l'abattement supposé",
   T7$`.tri:Restant (mesuré)` > T7$`.tri:Restant (0,75)`)
