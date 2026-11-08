# Trancher un mois douteux : le journal, et ce qu'il recouvre.
#
# Le topo ne sait juger que des chiffres, et il les juge à chaque ouverture :
# un mois « à risque » le reste pour toujours, même après qu'on est allé voir.
# Une alerte qui ne s'éteint jamais finit par ne plus rien dire.
#
# Ce fichier éprouve ce qui l'éteint — et surtout ce qui la rallume : une
# décision porte sur des CHIFFRES, pas sur un nom de mois, et elle doit tomber
# d'elle-même le jour où ces chiffres bougent.
suppressMessages({library(dplyr); library(tibble); library(lubridate)
                  library(tidyr); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
source("R/date.R",         encoding = "UTF-8", local = TRUE)
source("R/helpers.R",      encoding = "UTF-8", local = TRUE)
source("R/theme.R",        encoding = "UTF-8", local = TRUE)
source("R/kpi.R",          encoding = "UTF-8", local = TRUE)
source("R/fraicheur.R",    encoding = "UTF-8", local = TRUE)
source("R/plan_comptable.R", encoding = "UTF-8", local = TRUE)
source("R/exploitation.R", encoding = "UTF-8", local = TRUE)
source("R/validation.R",   encoding = "UTF-8", local = TRUE)

M <- function(x) as.Date(x)

#### Lire le classeur, quoi qu'il contienne ####

brut <- tibble(
  MOIS = c("2026-10-01", "01/09/2026", "", "2026-08-01"),
  DECISION = c("valide", "REJETE ", "valide", "n'importe quoi"),
  COMMENTAIRE = c("export fini", "trou dans la paie", "x", "y"),
  QUI = c("Thomas", "Thomas", "x", "y"),
  QUAND = c("2026-11-08 10:00:00", "2026-11-08 11:00:00", "", ""),
  CA = c("77500", "62000", "1", "1"),
  N_COMPTES = c("57", "48", "1", "1"))
v <- normalise_validations(brut)
ok("les deux lignes exploitables passent", nrow(v) == 2)
ok("le format jour/mois/année est lu aussi", M("2026-09-01") %in% v$MOIS)
ok("la décision est mise au propre", all(v$DECISION %in% c("valide", "rejete")))
ok("les chiffres redeviennent des nombres",
   is.numeric(v$CA) && is.integer(v$N_COMPTES) && v$CA[1] == 77500)
# Un classeur se modifie à la main : une ligne abîmée ne doit pas emporter les
# autres, mais elle ne doit pas non plus disparaître sans un mot.
ok("une ligne sans mois est écartée", !any(is.na(v$MOIS)))
ok("une décision inconnue est écartée", !"n'importe quoi" %in% v$DECISION)
ok("et on sait combien de lignes ont été perdues",
   lignes_validation_perdues(brut) == 2)
ok("rien du tout : un tableau vide, pas une erreur",
   nrow(normalise_validations(NULL)) == 0 &&
   identical(names(normalise_validations(NULL)), COLONNES_VALIDATION))
ok("une colonne manquante ne fait pas tomber la lecture",
   nrow(normalise_validations(tibble(MOIS = "2026-10-01",
                                     DECISION = "valide"))) == 1)

#### La décision qui compte est la dernière ####

journal <- tibble(
  MOIS = c("2026-10-01", "2026-10-01", "2026-09-01", "2026-07-01", "2026-07-01"),
  DECISION = c("valide", "rejete", "valide", "valide", "revoque"),
  COMMENTAIRE = "c", QUI = "Thomas",
  QUAND = c("2026-11-01 09:00:00", "2026-11-05 09:00:00", "2026-11-02 09:00:00",
            "2026-11-03 09:00:00", "2026-11-06 09:00:00"),
  CA = 1000, N_COMPTES = 50L)
d <- derniere_decision(journal)
ok("un mois, une décision", nrow(d) == 2)
ok("c'est la plus récente qui gagne",
   d$DECISION[d$MOIS == M("2026-10-01")] == "rejete")
# Révoquer n'efface rien : ça ajoute une ligne qui annule. Le mois doit alors
# sortir des décisions, pas y rester avec un état bizarre.
ok("une révocation retire le mois des décisions",
   !M("2026-07-01") %in% d$MOIS)
ok("journal vide : aucune décision", nrow(derniere_decision(NULL)) == 0)

# À horodatage identique — deux clics dans la même seconde — c'est la ligne la
# plus basse du classeur qui l'emporte, comme dans n'importe quel journal.
ex_aequo <- tibble(MOIS = "2026-10-01", DECISION = c("valide", "rejete"),
                   COMMENTAIRE = "c", QUI = "T",
                   QUAND = "2026-11-08 10:00:00", CA = 1, N_COMPTES = 1L)
ok("à la seconde près, la dernière ligne gagne",
   derniere_decision(ex_aequo)$DECISION == "rejete")

#### Recouvrir le verdict calculé ####

topo <- tibble(
  PERIODE = M(c("2026-10-01", "2026-09-01", "2026-05-01", "2026-04-01")),
  CA = c(77500, 62000, 70000, 71000),
  N_COMPTES = c(57L, 48L, 55L, 56L),
  ETAT = c("risque", "risque", "ancien", "ok"),
  RANG = c(2L, 2L, 4L, 5L),
  A_ECARTER = c(TRUE, TRUE, FALSE, FALSE),
  POURQUOI = c("Rémunérations en baisse de 60 %", "des charges sans aucune vente",
               "Matières en baisse de 55 %", ""))

ok("sans décision, les colonnes existent quand même",
   all(c("DECISION", "DECIDE_PAR", "DECIDE_LE") %in%
       names(applique_validations(topo, NULL))))
ok("et rien n'est touché",
   identical(applique_validations(topo, NULL)$ETAT, topo$ETAT))

dec <- function(mois, decision, ca, nc, com = "vérifié avec le comptable")
  tibble(MOIS = mois, DECISION = decision, COMMENTAIRE = com, QUI = "Thomas",
         QUAND = "2026-11-08 10:12:33", CA = ca, N_COMPTES = nc)

t <- applique_validations(topo, dec("2026-10-01", "valide", 77500, 57L))
i <- 1
ok("validé : l'état devient « validé à la main »", t$ETAT[i] == "valide")
ok("validé : le mois n'est plus proposé à l'écartement", !t$A_ECARTER[i])
ok("validé : le rang suit l'état, pas les chiffres",
   t$RANG[i] == etats_topo()$RANG[etats_topo()$ETAT == "valide"])
ok("validé : on sait qui, quand et pourquoi",
   grepl("Thomas", t$POURQUOI[i]) && grepl("8 nov. 2026", t$POURQUOI[i]) &&
   grepl("comptable", t$POURQUOI[i]))
ok("validé : le motif calculé a cédé la place",
   !grepl("baisse", t$POURQUOI[i]))
ok("les autres mois ne bougent pas", identical(t$ETAT[-i], topo$ETAT[-i]))

# Un mois rejeté est confirmé mauvais : il doit rester proposé à l'écartement
# même s'il est trop vieux pour que le calcul s'en inquiète encore.
t <- applique_validations(topo, dec("2026-05-01", "rejete", 70000, 55L))
ok("rejeté : l'état devient « écarté à la main »", t$ETAT[3] == "rejete")
ok("rejeté : un mois ANCIEN redevient à écarter", t$A_ECARTER[3])

t <- applique_validations(topo, bind_rows(
  dec("2026-10-01", "valide", 77500, 57L),
  dec("2026-10-01", "revoque", 77500, 57L) %>% mutate(QUAND = "2026-11-09 08:00:00")))
ok("révoqué : le mois retrouve son état calculé", t$ETAT[1] == "risque")
ok("et son motif calculé", grepl("baisse", t$POURQUOI[1]))

#### Une décision porte sur des chiffres, pas sur un nom de mois ####

# C'est le cœur du fichier. Valider octobre le 8, puis recevoir quinze
# écritures de plus le 9, ne valide pas le nouvel octobre : ce n'est plus le
# mois qu'on a regardé.
t <- applique_validations(topo, dec("2026-10-01", "valide", 70000, 57L))
ok("le CA a bougé : la décision ne porte plus", t$ETAT[1] == "risque")
ok("et le tableau dit pourquoi, au lieu de la faire disparaître",
   grepl("ne porte plus", t$POURQUOI[1]))
ok("le motif calculé reste à côté", grepl("baisse", t$POURQUOI[1]))

t <- applique_validations(topo, dec("2026-10-01", "valide", 77500, 61L))
ok("le nombre de comptes a bougé : la décision ne porte plus non plus",
   t$ETAT[1] == "risque")

ok("un écart d'un euro ne révoque rien",
   applique_validations(topo, dec("2026-10-01", "valide", 77500.5, 57L))$ETAT[1]
   == "valide")
ok("au-delà de la tolérance, si",
   applique_validations(topo, dec("2026-10-01", "valide", 77510, 57L))$ETAT[1]
   == "risque")
# Classeur rempli à la main, colonnes de chiffres laissées vides : on ne peut
# rien comparer, donc on ne révoque rien. Révoquer en silence des décisions
# parfaitement valables serait le pire des deux.
ok("sans chiffres enregistrés, la décision tient",
   applique_validations(topo, dec("2026-10-01", "valide", NA, NA_integer_))$ETAT[1]
   == "valide")

ok("un topo vide ne fait pas tomber l'application",
   nrow(applique_validations(topo[0, ], dec("2026-10-01", "valide", 1, 1L))) == 0)

#### Ce qui sort des alertes, et ce qui y reste ####

improb <- tibble(PERIODE = M(c("2026-10-01", "2026-05-01")),
                 MOTIF = "chute", LIBELLE = "Rémunérations", CHUTE = 60)
t <- applique_validations(topo, bind_rows(dec("2026-10-01", "valide", 77500, 57L),
                                          dec("2026-05-01", "rejete", 70000, 55L)))
ok("un mois validé sort des alertes d'intégrité",
   !M("2026-10-01") %in% improb_hors_valides(improb, t)$PERIODE)
# Un mois rejeté porte bien des chiffres improbables : c'est confirmé, pas
# résolu. Le masquer rendrait l'alerte muette sur un vrai problème de source.
ok("un mois rejeté y reste",
   M("2026-05-01") %in% improb_hors_valides(improb, t)$PERIODE)
ok("sans validation, rien n'est retiré",
   nrow(improb_hors_valides(improb, applique_validations(topo, NULL))) == 2)

#### La ligne qu'on ajoute ####

l <- ligne_validation("2026-10-01", "valide", "  export fini  ", "Thomas", topo,
                      quand = as.POSIXct("2026-11-08 10:12:33", tz = "UTC"))
ok("les chiffres viennent du topo, pas de la saisie",
   l$CA == 77500 && l$N_COMPTES == 57L)
ok("l'horodatage est ISO, pour que l'ordre du texte soit l'ordre du temps",
   l$QUAND == "2026-11-08 10:12:33")
ok("le commentaire est nettoyé", l$COMMENTAIRE == "export fini")
ok("le mois est ramené au premier du mois",
   ligne_validation("2026-10-17", "valide", "c", "T", topo)$MOIS == M("2026-10-01"))
ok("sans nom d'utilisateur, un tiret plutôt qu'un vide",
   ligne_validation("2026-10-01", "valide", "c", "", topo)$QUI == "—")
ok("un mois absent du topo : des chiffres NA, pas une erreur",
   is.na(ligne_validation("2020-01-01", "valide", "c", "T", topo)$CA))
ok("et la ligne relue redonne la décision",
   derniere_decision(l)$DECISION == "valide")

#### Le classeur ####

VU <- list()
io_double <- function(contenu) list(
  lire    = function(ss) if (is.null(contenu)) stop("vide") else contenu,
  ecrire  = function(ss, df) VU <<- list(geste = "ecrire",  df = df, ss = ss),
  ajouter = function(ss, df) VU <<- list(geste = "ajouter", df = df, ss = ss))

ligne <- ligne_validation("2026-10-01", "valide", "export fini", "Thomas", topo)

# Classeur encore vide : la première écriture doit poser l'en-tête, sans quoi
# la relecture prendrait la première décision pour des noms de colonnes.
VU <- list()
ecrit_validation(ligne, "xxx", io_double(NULL))
ok("classeur vide : on réécrit, en-tête compris", VU$geste == "ecrire")
ok("et les colonnes y sont toutes",
   identical(names(VU$df), COLONNES_VALIDATION))
ok("le mois part en texte, pas en nombre de jours depuis 1970",
   identical(VU$df$MOIS, "2026-10-01"))

VU <- list()
ecrit_validation(ligne, "xxx", io_double(
  tibble(MOIS = "2026-09-01", DECISION = "valide", COMMENTAIRE = "c",
         QUI = "T", QUAND = "2026-11-01 09:00:00", CA = 1, N_COMPTES = 1L)))
ok("en-tête déjà posé : on AJOUTE une ligne", VU$geste == "ajouter")
ok("et une seule", nrow(VU$df) == 1)

# Un classeur commencé à la main, sans toutes les colonnes : on réécrit, mais
# on ne jette pas ce qui s'y trouvait.
VU <- list()
ecrit_validation(ligne, "xxx", io_double(
  tibble(MOIS = "2026-09-01", DECISION = "valide")))
ok("en-tête incomplet : on réécrit sans perdre l'existant",
   VU$geste == "ecrire" && nrow(VU$df) == 2)

ok("sans PATH_VALIDATION_COMPTA, on refuse plutôt que d'écrire ailleurs",
   inherits(try(ecrit_validation(ligne, "", io_double(NULL)), silent = TRUE),
            "try-error"))
# L'appelant doit pouvoir ne RIEN changer à l'écran quand l'écriture échoue :
# il faut donc que l'échec remonte.
io_cassee <- list(lire = function(ss) tibble(),
                  ecrire = function(ss, df) stop("Drive refuse"),
                  ajouter = function(ss, df) stop("Drive refuse"))
ok("une écriture refusée lève",
   inherits(try(ecrit_validation(ligne, "xxx", io_cassee), silent = TRUE),
            "try-error"))

# La lecture, elle, ne doit jamais lever : au premier passage il n'y a rien à
# lire, et le dashboard doit s'ouvrir comme avant que ce classeur existe.
ok("classeur illisible : un tableau vide, pas une erreur",
   nrow(suppressMessages(lit_validations("xxx", io_double(NULL)))) == 0)
ok("sans variable d'environnement non plus",
   nrow(suppressMessages(lit_validations("", io_double(NULL)))) == 0)

#### Le dialogue ####

t <- applique_validations(topo, dec("2026-10-01", "valide", 77500, 57L))
choix <- mois_a_trancher(t)
ok("les mois plausibles ne sont pas à trancher",
   !"2026-04-01" %in% as.character(choix))
ok("les mois douteux le sont", "2026-09-01" %in% as.character(choix))
ok("les mois déjà tranchés restent proposés : on peut changer d'avis",
   "2026-10-01" %in% as.character(choix))
ok("le plus récent d'abord", as.character(choix)[1] == "2026-10-01")
ok("et l'état est dans le libellé", grepl("Validé à la main", names(choix)[1]))

# Le mois EN COURS est partiel par construction : le valider n'aurait aucun
# sens, et la décision tomberait dès le lendemain puisque ses chiffres auront
# bougé.
en_cours <- topo %>% mutate(ETAT = c("en_cours", ETAT[-1]))
ok("le mois en cours ne se tranche pas",
   !"2026-10-01" %in% as.character(mois_a_trancher(en_cours)))
ok("aucun mois douteux : rien à trancher",
   length(mois_a_trancher(topo %>% mutate(ETAT = "ok"))) == 0)

if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("  (shiny absent : dialogue non éprouvé)\n")
} else {
  library(shiny)
  html <- function(x) paste(as.character(x), collapse = "")
  d <- html(dialogue_validation(t, dec("2026-10-01", "valide", 77500, 57L)))
  ok("le dialogue propose les trois décisions",
     all(sapply(c("valide", "rejete", "revoque"),
                function(x) grepl(x, d, fixed = TRUE))))
  ok("il demande un commentaire",
     grepl("valid_commentaire", d, fixed = TRUE))
  ok("il prévient que la décision tombera si les chiffres bougent",
     grepl("cessera", d, fixed = TRUE))
  ok("sans mois à trancher, il le dit au lieu d'un formulaire vide",
     grepl("Aucun mois", html(dialogue_validation(topo %>% mutate(ETAT = "ok"))),
           fixed = TRUE))
  a <- html(apercu_decision(dec("2026-10-01", "valide", 1, 1L), "2026-10-01"))
  ok("la décision en cours est rappelée avant d'en prendre une autre",
     grepl("Décision actuelle", a, fixed = TRUE))
  ok("et rien ne s'affiche pour un mois jamais tranché",
     is.null(apercu_decision(dec("2026-10-01", "valide", 1, 1L), "2026-04-01")))
}

#### Bout en bout, par topo_mois() ####

db <- bind_rows(lapply(1:10, function(k) tibble(
  COMPTE = c("700000", "600100", "620000"),
  VALEUR = c(60000, 18000, if (k == 9) 2000 else 24000),
  ANNEE = 2026L, MOIS = k, LIBELLE = "x", TYPE = "compte", AGREGE = TRUE)))
im <- periodes_improbables(postes_exploitation(db),
                           mois_en_cours = M("2026-11-01"))
sans <- topo_mois(db, im, NULL, mois_en_cours = M("2026-11-01"))
ok("septembre est à risque avant décision",
   sans$ETAT[sans$PERIODE == M("2026-09-01")] == "risque")

ca_sept <- sans$CA[sans$PERIODE == M("2026-09-01")]
nc_sept <- sans$N_COMPTES[sans$PERIODE == M("2026-09-01")]
avec <- topo_mois(db, im, dec("2026-09-01", "valide", ca_sept, nc_sept),
                  mois_en_cours = M("2026-11-01"))
ok("et validé après", avec$ETAT[avec$PERIODE == M("2026-09-01")] == "valide")
ok("le reste du tableau est identique",
   identical(avec$CA, sans$CA) && identical(avec$PERIODE, sans$PERIODE))
# Le calcul ne doit JAMAIS tenir compte des décisions : sinon valider un mois
# changerait les médianes, donc les verdicts des autres mois.
ok("valider un mois ne change le verdict d'aucun autre",
   identical(avec$ETAT[avec$PERIODE != M("2026-09-01")],
             sans$ETAT[sans$PERIODE != M("2026-09-01")]))
