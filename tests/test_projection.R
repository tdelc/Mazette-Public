# Le volet Projection : combler un mois dont la comptabilité n'est pas entière.
#
# Ce fichier éprouve le MODÈLE, pas le rendu. Trois choses y comptent plus que
# les autres, et ce sont celles qui se cassent en silence :
#
#   1. les mois de référence. Un mois écarté, un mois lui-même projeté ou un
#      mois sans le poste modélisé ne doivent JAMAIS entrer dans une médiane —
#      sinon le trou qu'on veut combler tire l'estimation vers zéro, et le
#      modèle confirme le défaut ;
#   2. l'ordre des postes. Les matières se calculent sur le CA RETENU : si le
#      CA est projeté, c'est lui qu'il faut, pas le zéro de la comptabilité ;
#   3. recalcule_gestion(), qui reproduit la formule de postes_exploitation().
#      Deux copies d'une même formule finissent par diverger ; le test de
#      l'aller-retour ci-dessous est le seul garde-fou.
suppressMessages({library(dplyr); library(tibble); library(tidyr)
                  library(lubridate); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"; COUL_MATIERE<-"#d3c0ac"
COUL_TRAVAIL<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
balise <- function(...) structure(list(args = list(...)), class = "tag")
div<-balise; span<-balise; tagList<-balise; icon<-balise
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise)
plot_ly <- function(...) structure(list(), class = "p")
plotly_empty <- function(...) structure(list(vide = TRUE), class = "p")
TRACES <- list()
add_trace <- function(p, ...) { TRACES[[length(TRACES)+1L]] <<- list(...); p }
MISE_EN_PAGE <- list()
layout <- function(p, ...) { MISE_EN_PAGE <<- list(...); p }

for (f in c("R/helpers.R", "R/theme.R", "R/kpi.R", "R/plan_comptable.R",
            "R/exploitation.R", "R/planning.R", "R/projection.R"))
  source(f, encoding = "UTF-8", local = TRUE)

#### Un fixture dont on connaît les réponses ####

# Dix-huit mois réguliers, puis trois mois amputés — exactement la situation
# de la maison : depuis juillet, les rémunérations ne sont pas encodées.
MOIS <- seq(as.Date("2024-01-01"), by = "month", length.out = 21)
TROUES <- tail(MOIS, 3)

CA_MOIS     <- 60000 + 1000 * (seq_along(MOIS) %% 5)   # entre 60k et 64k
RATIO_MAT   <- 0.30
TAUX_HORAIRE<- 25
HEURES_MOIS <- 900 + 10 * (seq_along(MOIS) %% 7)
GENERAUX    <- 8000
AMORT       <- 1700
CALIBRAGE   <- 0.95            # la caisse annonce plus que la compta

ligne <- function(i) {
  m <- MOIS[i]
  troue <- m %in% TROUES
  cpt <- c("700100", "600100", "630000")
  val <- c(CA_MOIS[i], RATIO_MAT * CA_MOIS[i], AMORT)
  if (!troue) {
    cpt <- c(cpt, "620000", "610000")
    val <- c(val, TAUX_HORAIRE * HEURES_MOIS[i], GENERAUX)
  }
  tibble(ANNEE = year(m), MOIS = month(m), COMPTE = cpt, LIBELLE = "x",
         VALEUR = val, TYPE = "compte", AGREGE = TRUE)
}
DB <- bind_rows(lapply(seq_along(MOIS), ligne))
POSTES <- postes_exploitation(DB)

jours <- seq(min(MOIS), fin_periode(max(MOIS), "mois"), by = "day")
KPI <- tibble(DATE = jours) %>%
  mutate(M = floor_date(DATE, "month"),
         CA_HTVA = CA_MOIS[match(M, MOIS)] / CALIBRAGE / days_in_month(M)) %>%
  select(DATE, CA_HTVA)
PLANNING <- tibble(DATE = MOIS, SERVICE = "Salle",
                   HEURES = HEURES_MOIS)

VENTES <- ventes_par_mois(KPI)
HEURES <- heures_par_mois(PLANNING)
CIBLE  <- TROUES[1]

ok("le fixture a bien les mois qu'on croit", nrow(POSTES) == length(MOIS))
ok("les ventes de caisse se somment par mois",
   nrow(VENTES) == length(MOIS) &&
   abs(VENTES$VENTES[1] - CA_MOIS[1] / CALIBRAGE) < 0.01)
ok("les heures planifiées se somment par mois",
   nrow(HEURES) == length(MOIS) && HEURES$HEURES[1] == HEURES_MOIS[1])
ok("une table de planning absente ne casse rien",
   is.null(heures_par_mois(NULL)) && is.null(heures_par_mois(tibble(A = 1))))
ok("un CA HTVA absent de la table de ventes non plus",
   is.null(ventes_par_mois(tibble(DATE = jours[1]))))

#### Le mois en cours, qui n'est pas un mois ####

# Le piège le plus coûteux de tout le volet : au 3 du mois, la caisse annonce
# deux jours de ventes, et on projetterait en face des charges de mois entier.
# La marge affichée serait catastrophique et parfaitement fausse.
AU_MILIEU <- as.Date("2025-09-15")      # le dernier mois du fixture, à mi-course
v_mi <- ventes_par_mois(KPI, aujourd_hui = AU_MILIEU)
l_sept <- v_mi[v_mi$PERIODE == as.Date("2025-09-01"), ]
l_aout <- v_mi[v_mi$PERIODE == as.Date("2025-08-01"), ]

ok("un mois clos n'est jamais extrapolé",
   !l_aout$EN_COURS && l_aout$FACTEUR == 1 &&
     l_aout$ECOULES == l_aout$ATTENDUS)
ok("le mois en cours sait ce qu'il a devant lui",
   l_sept$EN_COURS && l_sept$ECOULES == 14 && l_sept$ATTENDUS == 30)
ok("et son facteur porte la caisse au mois entier",
   abs(l_sept$FACTEUR - 30 / 14) < 1e-9)
ok("le CA projeté d'un mois en cours vaut bien un mois entier",
   { r <- projette_ca(as.Date("2025-09-01"), v_mi,
                      mois_reference(POSTES, as.Date("2025-09-01"), "CA", 6))
     abs(r$VALEUR - CA_MOIS[match(as.Date("2025-09-01"), MOIS)]) < 1 })
ok("et il annonce l'extrapolation plutôt que de la taire",
   { r <- projette_ca(as.Date("2025-09-01"), v_mi,
                      mois_reference(POSTES, as.Date("2025-09-01"), "CA", 6))
     grepl("extrapolé", r$METHODE) && grepl("14 jours", r$DETAIL) })

# Le 1er du mois : aucun jour écoulé, donc aucun rythme. Inventer un facteur
# là-dessus donnerait n'importe quoi.
v_1er <- ventes_par_mois(KPI, aujourd_hui = as.Date("2025-09-01"))
ok("un mois à peine commencé n'a pas encore de ventes à date",
   !as.Date("2025-09-01") %in% v_1er$PERIODE)
ok("et son CA se reconduit, en le disant",
   { r <- projette_ca(as.Date("2025-09-01"), v_1er,
                      mois_reference(POSTES, as.Date("2025-09-01"), "CA", 6))
     r$METHODE == "Médiane des mois précédents" &&
       grepl("à peine commencé", r$DETAIL) })

# Les jours d'ouverture se déduisent des ventes, pas du calendrier : la maison
# ferme le lundi, et un lundi férié ne doit pas suffire à le faire oublier.
ok("les jours de fermeture habituels sont reconnus",
   { j <- tibble(DATE = seq(as.Date("2025-01-06"), as.Date("2025-06-30"),
                            by = "day")) %>%
       mutate(V = ifelse(wday(DATE) == 2, 0, 1500))
     setequal(jours_ouverts_habituels(j, as.Date("2025-07-01")),
              setdiff(1:7, 2)) })
ok("et une fermeture exceptionnelle ne les change pas",
   { j <- tibble(DATE = seq(as.Date("2025-01-06"), as.Date("2025-06-30"),
                            by = "day")) %>%
       mutate(V = ifelse(wday(DATE) == 2 | DATE == as.Date("2025-05-07"),
                         0, 1500))
     setequal(jours_ouverts_habituels(j, as.Date("2025-07-01")),
              setdiff(1:7, 2)) })

# Le planning tronqué : on le signale, on ne le redresse pas. Un redressement
# au prorata gonflerait de 3 % tous les mois dont le dernier jour est fermé.
ok("un planning arrêté en cours de mois est signalé, pas gonflé",
   { pl <- tibble(DATE = seq(floor_date(today(), "month"),
                             floor_date(today(), "month") + 9, by = "day"),
                  SERVICE = "Salle", HEURES = 30)
     h <- heures_par_mois(bind_rows(PLANNING, pl))
     r <- projette_remuneration(floor_date(today(), "month"), h,
            mois_reference(POSTES, max(MOIS) + 1, "REMUNERATION", 6))
     abs(r$VALEUR - 300 * median(
       (POSTES %>% filter(REMUNERATION > 1) %>% tail(6))$REMUNERATION /
       HEURES_MOIS[match((POSTES %>% filter(REMUNERATION > 1) %>%
                          tail(6))$PERIODE, MOIS)])) < 0.01 &&
       grepl("planning arrêté au", r$DETAIL) })

#### Les mois de référence ####

ref <- mois_reference(POSTES, CIBLE, "CA", n = 6)
ok("la référence s'arrête avant le mois visé", all(ref$PERIODE < CIBLE))
ok("et en prend exactement le nombre demandé", nrow(ref) == 6)
ok("ce sont bien les six derniers",
   identical(ref$PERIODE, tail(MOIS[MOIS < CIBLE], 6)))
ok("un mois écarté n'est pas une référence",
   { r <- mois_reference(POSTES, CIBLE, "CA", 6, exclus = MOIS[15])
     !MOIS[15] %in% r$PERIODE && nrow(r) == 6 })
ok("un mois projeté non plus",
   { r <- mois_reference(POSTES, CIBLE, "CA", 6, a_projeter = MOIS[16])
     !MOIS[16] %in% r$PERIODE })
# LE filtre qui compte : sans lui, les mois sans rémunérations tireraient la
# médiane du coût horaire vers zéro, et le modèle confirmerait le trou.
ok("un mois sans le poste modélisé n'est pas une référence",
   { r <- mois_reference(POSTES, max(MOIS), "REMUNERATION", 6)
     !any(r$PERIODE %in% TROUES) })
ok("alors qu'il reste une référence pour un poste qu'il porte",
   { r <- mois_reference(POSTES, max(MOIS), "CA", 6)
     any(r$PERIODE %in% TROUES) })

#### Les quatre estimateurs ####

ca <- projette_ca(CIBLE, VENTES, mois_reference(POSTES, CIBLE, "CA", 6))
ok("le CA projeté est la caisse recalée sur la comptabilité",
   abs(ca$VALEUR - CA_MOIS[match(CIBLE, MOIS)]) < 1)
ok("et il dit sur combien de mois il s'est calé", ca$N_REF == 6)
# Sans caisse du tout, on reconduit la médiane plutôt que de ne rien dire —
# mais la MÉTHODE change, pour qu'on ne prenne pas une reconduction pour une
# mesure.
ok("sans ventes, le CA se reconduit et le dit",
   { r <- projette_ca(CIBLE, NULL, ref)
     r$METHODE == "Médiane des mois précédents" &&
       abs(r$VALEUR - median(ref$CA)) < 0.01 })
ok("et sans référence non plus, il ne dit rien",
   is.na(projette_ca(CIBLE, NULL, ref[0, ])$VALEUR))
# Moins de trois mois de référence : on projette quand même, mais SANS
# recalage, et la base de calcul le dit.
ok("avec trop peu de références, la caisse passe telle quelle",
   { r <- projette_ca(CIBLE, VENTES, mois_reference(POSTES, CIBLE, "CA", 2))
     abs(r$VALEUR - VENTES$VENTES[VENTES$PERIODE == CIBLE]) < 0.01 &&
     grepl("sans recalage", r$DETAIL) })

mat <- projette_matieres(100000, mois_reference(POSTES, CIBLE, "MATIERES", 6))
ok("les matières suivent le ratio médian au CA",
   abs(mat$VALEUR - 100000 * RATIO_MAT) < 0.01)

rem <- projette_remuneration(CIBLE, HEURES,
                             mois_reference(POSTES, CIBLE, "REMUNERATION", 6))
ok("les rémunérations sont les heures planifiées au coût horaire médian",
   abs(rem$VALEUR - HEURES_MOIS[match(CIBLE, MOIS)] * TAUX_HORAIRE) < 0.01)
ok("et la méthode le dit", rem$METHODE == "Heures planifiées")
ok("sans planning, on reconduit la médiane et on change de méthode",
   { r <- projette_remuneration(CIBLE, NULL,
            mois_reference(POSTES, CIBLE, "REMUNERATION", 6))
     r$METHODE == "Médiane des mois précédents" &&
     grepl("pas d'heures planifiées", r$DETAIL) })

gen <- projette_generaux(mois_reference(POSTES, CIBLE, "GENERAUX", 6))
ok("les frais généraux sont la médiane des mois de référence",
   abs(gen$VALEUR - GENERAUX) < 0.01)

#### L'orchestration ####

proj <- projette_mois(POSTES, CIBLE, c("remuneration", "generaux"),
                      VENTES, HEURES, 6, a_projeter = TROUES)
ok("une ligne par poste demandé", nrow(proj) == 2)
ok("le réel du mois troué est bien zéro", all(proj$REEL == 0))
ok("et le retenu, lui, ne l'est pas", all(proj$RETENU > 0))

# L'ordre de PROJ_POSTES n'est pas décoratif : les matières se calculent sur
# le CA RETENU. Projeter les deux doit donner les matières du CA projeté, et
# surtout pas celles d'un CA à zéro.
sans_ca <- projette_mois(POSTES, max(MOIS), c("matieres"), VENTES, HEURES, 6,
                         a_projeter = TROUES)
avec_ca <- projette_mois(POSTES, max(MOIS), c("ca", "matieres"), VENTES,
                         HEURES, 6, a_projeter = TROUES)
ok("les matières se calculent sur le CA retenu, projeté compris",
   { r <- avec_ca$RETENU[avec_ca$CLE == "ca"]
     abs(avec_ca$RETENU[avec_ca$CLE == "matieres"] - r * RATIO_MAT) < 0.01 })
ok("et sur le CA réel quand celui-ci n'est pas projeté",
   abs(sans_ca$RETENU[1] -
       POSTES$CA[POSTES$PERIODE == max(MOIS)] * RATIO_MAT) < 0.01)

aj <- projette_mois(POSTES, CIBLE, c("generaux"), VENTES, HEURES, 6,
                    a_projeter = TROUES, ajustements = list(generaux = 10))
ok("l'hypothèse s'applique APRÈS le modèle, et les deux restent lisibles",
   { abs(aj$MODELE - GENERAUX) < 0.01 &&
     abs(aj$RETENU - GENERAUX * 1.1) < 0.01 && aj$AJUST == 10 })

multi <- projette_mois_multiples(POSTES, TROUES, c("remuneration"),
                                 VENTES, HEURES, 6)
ok("plusieurs mois se projettent d'un coup", nrow(multi) == 3)
# Le point : juillet ne sert pas de référence à août. Sinon le modèle se
# nourrirait de lui-même, et l'erreur du premier mois se propagerait.
ok("et aucun ne sert de référence aux autres",
   { h <- HEURES$HEURES[match(TROUES, HEURES$PERIODE)]
     all(abs(multi$RETENU - h * TAUX_HORAIRE) < 0.01) })

#### Le compte d'exploitation projeté ####

# L'ALLER-RETOUR : une projection vide ne doit RIEN changer. C'est le test qui
# garde recalcule_gestion() aligné sur postes_exploitation().
vide <- applique_projection(POSTES, NULL)
colonnes <- setdiff(names(POSTES), "PROJETE")
ok("une projection vide laisse le compte d'exploitation intact",
   all(vapply(colonnes, function(c)
     isTRUE(all.equal(vide[[c]], POSTES[[c]])), logical(1))))
ok("et rien n'y est marqué projeté", !any(vide$PROJETE))

compte <- applique_projection(POSTES, multi)
ok("les mois projetés sont marqués comme tels",
   identical(sort(compte$PERIODE[compte$PROJETE]), sort(TROUES)))
ok("la marge du mois projeté tient compte du poste comblé",
   { m <- compte %>% filter(PERIODE == TROUES[1])
     avant <- POSTES %>% filter(PERIODE == TROUES[1])
     abs(m$MARGE_AA - (avant$MARGE_AA - multi$RETENU[1])) < 0.01 })
ok("et les autres mois n'ont pas bougé",
   { a <- compte %>% filter(!PROJETE) %>% pull(MARGE_AA)
     b <- POSTES %>% filter(!PERIODE %in% TROUES) %>% pull(MARGE_AA)
     isTRUE(all.equal(a, b)) })

# Un mois sans la moindre écriture : c'est celui où la projection sert le
# plus, et il n'existe nulle part dans la comptabilité.
neuf <- as.Date("2025-11-01")
p_neuf <- applique_projection(
  POSTES, projette_mois(POSTES, neuf, c("ca", "matieres", "generaux"),
                        ventes_par_mois(KPI %>%
                          bind_rows(tibble(DATE = seq(neuf, neuf + 29, by = "day"),
                                           CA_HTVA = 2000))),
                        HEURES, 6))
ok("un mois absent de la comptabilité est créé pour être projeté",
   neuf %in% p_neuf$PERIODE && isTRUE(p_neuf$PROJETE[p_neuf$PERIODE == neuf]))
ok("et son CA projeté vient bien de la caisse",
   p_neuf$CA[p_neuf$PERIODE == neuf] > 0)

#### Ce qui manque ####

manque <- postes_manquants(POSTES)
ok("les postes absents des mois troués sont repérés",
   setequal(unique(manque$CLE), c("remuneration", "generaux")) &&
   setequal(unique(manque$PERIODE), TROUES))
ok("et rien n'est signalé sur les mois complets",
   !any(manque$PERIODE %in% setdiff(MOIS, TROUES)))
# Un poste qu'on n'a JAMAIS : il ne manque pas, il n'existe pas. Le proposer
# ferait inventer une rubrique que la maison n'a pas.
ok("un poste absent de tout l'historique n'est pas réputé manquant",
   { sans_fg <- DB %>% filter(COMPTE != "610000")
     m <- postes_manquants(postes_exploitation(sans_fg))
     !"generaux" %in% m$CLE })

#### Le contrôle du modèle ####

ctrl <- controle_modele(POSTES, c("matieres", "generaux"), VENTES, HEURES,
                        n_ref = 6, n_mois = 5)
ok("le contrôle rejoue le modèle sur des mois connus", nrow(ctrl) > 0)
ok("il ne juge que des mois où le poste est réellement là",
   all(abs(ctrl$REEL) > PROJ_SEUIL_PRESENT))
# Le fixture est régulier : un modèle correct doit y tomber juste. Un écart
# non nul signerait une référence mal choisie — le mois lui-même, par exemple.
ok("et sur un fixture régulier il tombe juste",
   all(abs(ctrl$ECART_PCT) < 0.01, na.rm = TRUE))

fiab <- fiabilite_modele(ctrl)
ok("la fiabilité se résume par poste",
   nrow(fiab) == 2 && all(fiab$ECART_MEDIAN < 0.01))
ok("sans contrôle, elle se tait plutôt que d'inventer",
   nrow(fiabilite_modele(NULL)) == 0)

#### Les rendus ####

t_detail <- table_projection_detail(multi)
ok("le tableau du modèle dit la méthode et la base",
   all(c("Méthode", "Base de calcul", "Réf") %in% names(t_detail)) &&
   nrow(t_detail) == 3)
ok("sans projection, il le dit plutôt que de rendre un tableau vide",
   ncol(table_projection_detail(NULL)) == 1)

t_res <- table_projection_resultat(compte, multi)
ok("le tableau de résultat marque les montants modélisés",
   any(grepl(PROJ_MARQUE, t_res$Rémunérations, fixed = TRUE)))
ok("et seulement eux",
   !any(grepl(PROJ_MARQUE, t_res$`Chiffre d'affaires`, fixed = TRUE)))
ok("la trajectoire hachure les mois projetés",
   { TRACES <<- list(); graph_projection(compte)
     p <- TRACES[[1]]$marker$pattern$shape
     sum(p == "/") == 3 && any(p == "") })
# Plotly range les catégories par ordre ALPHABÉTIQUE dès que deux traces les
# partagent : « août, avr., déc., févr. ». L'ordre doit être imposé.
ok("et les mois y restent dans l'ordre chronologique",
   { TRACES <<- list(); graph_projection(compte)
     attendu <- etiquette_periode(tail(sort(compte$PERIODE), 14), "mois")
     identical(MISE_EN_PAGE$xaxis$categoryarray, attendu) &&
       MISE_EN_PAGE$xaxis$categoryorder == "array" })

# Une tuile « CA projeté » sur un CA réel serait un mensonge, et c'est le
# genre de mensonge qui se recopie dans un mail.
mots <- function(x) paste(unlist(x), collapse = " | ")
tuiles <- mots(kpi_projection(compte, TROUES, multi))
ok("les tuiles marquent les postes modélisés",
   grepl(paste0(PROJ_MARQUE, "[0-9]"), gsub(" ", "", tuiles)) ||
   grepl(PROJ_MARQUE, tuiles, fixed = TRUE))
ok("et laissent le CA réel sans marque",
   { ca <- euro_exact(sum(compte$CA[compte$PERIODE %in% TROUES]))
     grepl(ca, tuiles, fixed = TRUE) &&
       !grepl(paste0(PROJ_MARQUE, ca), tuiles, fixed = TRUE) })
ok("sans mois choisi, elles invitent à en prendre un",
   grepl("Choisissez", mots(kpi_projection(compte, as.Date(character())))))

# format_CA(x, -1) blanchit ce qui s'arrondit à zéro : un écart de 2 EUR
# s'affichait « + », un signe tout seul.
ok("un écart minuscule s'écrit zéro, pas « + »",
   proj_ecart(2) == "0 €" && proj_ecart(-2) == "0 €")
ok("un écart lisible garde son signe",
   proj_ecart(540) == "+540€" && proj_ecart(-540) == "-540€")
ok("un bandeau prévient quand le planning manque",
   !is.null(bandeau_projection(
     projette_mois(POSTES, CIBLE, "remuneration", VENTES, NULL, 6), FALSE)))
ok("et se tait quand tout va bien",
   is.null(bandeau_projection(multi, TRUE)))
