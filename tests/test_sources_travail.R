# Les trois sources d'heures et de cout du travail.
#
# La regle qui porte tout : la PAIE fait foi sur les totaux mois x secteur,
# Horeko ne fait que repartir. Un recalage faux ne leve aucune erreur.
suppressMessages({library(dplyr); library(tibble); library(lubridate); library(tidyr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COULEURS_SECTEURS <- c("Service"="#2980b9", "Transformation alimentaire"="#5B7B5A",
                       "Brasserie"="#d98236", "Support"="#8d7b68")
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"; COUL_TRAVAIL<-"#8d5b8c"
source("R/planning.R",        encoding = "UTF-8", local = TRUE)   # normalise_secteur
source("R/sources_travail.R", encoding = "UTF-8", local = TRUE)

# ---------- 1. les nombres europeens ----------
# Trois regles, et la troisieme est la seule qui demande un arbitrage.
ok("la virgule est decimale", nombre_europeen("1234,56") == 1234.56)
ok("un point qui separe des groupes de trois est un millier",
   nombre_europeen("1.234") == 1234)
ok("et avec virgule, les deux cohabitent", nombre_europeen("1.234,56") == 1234.56)
ok("plusieurs groupes de trois", nombre_europeen("1.234.567") == 1234567)
# « 1.5 » n'est pas un groupe de trois : le point y est decimal.
ok("un point suivi d'autre chose que trois chiffres reste decimal",
   nombre_europeen("1.5") == 1.5)
ok("les espaces de milliers sont nettoyes", nombre_europeen("1 234,56") == 1234.56)
ok("l'espace insecable aussi", nombre_europeen("1 234,56") == 1234.56)
ok("le symbole euro aussi", nombre_europeen("1.234,56 €") == 1234.56)
ok("un nombre deja numerique passe tel quel", nombre_europeen(1234.56) == 1234.56)
ok("le vide rend NA", is.na(nombre_europeen("")))
ok("le tiret seul aussi", is.na(nombre_europeen("-")))
ok("le signe est conserve", nombre_europeen("-1.234,56") == -1234.56)
ok("vectorise", identical(nombre_europeen(c("1.000", "2,5")), c(1000, 2.5)))

# ---------- 2. la paie, du fichier brut a DB_ONSS ----------
# Le cout employeur est porte par la FICHE, pas par le secteur : on le ventile
# au prorata des heures. Meme travailleur, donc meme cout horaire, donc le
# prorata est exact et non une approximation.
paie <- tibble(
  `Année` = c(2026, 2026, 2026),
  Mois = c(9, 9, 9),
  `Coût employeur` = c("3.000,00", "1.000,00", "500,00"),
  Heures = c("150", "100", "0"),
  `Heures réellement travaillées` = c("150", "100", "0"),
  `Heures Service` = c("100", "0", "0"),
  `Heures Transfo alimentaire` = c("50", "0", "0"),
  `Heures Support` = c("0", "100", "0"),
  `Heures Fabrique de boissons` = c("0", "0", "0"))

o <- construit_onss(paie)
ok("une ligne par mois x secteur", nrow(o) == 4)
ok("les heures de service sont celles de la fiche",
   o$HEURES[o$SECTEUR == "Service"] == 100)
ok("le secteur est normalise",
   "Transformation alimentaire" %in% o$SECTEUR && "Service" %in% o$SECTEUR)
# 3000 EUR sur 150 h, dont 100 en service : 2000 EUR au service.
ok("le cout est ventile au prorata des heures",
   abs(o$COUT[o$SECTEUR == "Service"] - 2000) < 1e-9)
ok("et le reste a la transformation",
   abs(o$COUT[o$SECTEUR == "Transformation alimentaire"] - 1000) < 1e-9)
# La fiche sans aucune heure garde tout son cout, sous « Non ventile ».
ok("une fiche sans heures garde son cout",
   o$COUT[o$SECTEUR == SECTEUR_NON_VENTILE] == 500)
ok("le total retombe sur le total de la paie", abs(sum(o$COUT) - 4500) < 1e-9)
ok("un fichier vide rend NULL", is.null(construit_onss(paie[0, ])))
ok("un fichier NULL aussi", is.null(construit_onss(NULL)))

# Le controle de lecture : il attrape une colonne de secteur renommee.
ct <- controle_onss(paie)
ok("le controle voit les quatre colonnes de secteur", ct$COLONNES_VUES == 4)
ok("et ne trouve aucun ecart", abs(ct$ECART) < 0.01)
sans <- paie %>% select(-`Heures Support`)
ok("une colonne renommee se voit", controle_onss(sans)$COLONNES_VUES == 3)
ok("et les heures manquantes aussi", controle_onss(sans)$ECART == -100)

# ---------- 3. le recalage ----------
# La paie fait foi sur le TOTAL du mois x secteur ; Horeko ne fait que le
# repartir sur les jours. C'est legitime ici — contrairement a la regle de
# trois retiree de la compta — parce qu'on partage un ecart entre DEUX MESURES
# DE LA MEME GRANDEUR.
jours <- seq(as.Date("2026-09-01"), as.Date("2026-09-30"), by = "day")
couts <- tibble(DATE = rep(jours, each = 2),
                SECTEUR = rep(c("Service", "Support"), length(jours)),
                CRENEAU = "Journée",
                HEURES = rep(c(2, 1), length(jours)),
                COUT_TRAVAIL = rep(c(40, 20), length(jours)))
onss <- tibble(ANNEE = 2026L, MOIS = 9L, SECTEUR = c("Service", "Support"),
               HEURES = c(90, 60), COUT = c(2700, 1200), N_FICHES = 2L)

r <- recale_couts_travail(couts, onss)
ok("les heures pointees restent disponibles", "HEURES_HOREKO" %in% names(r))
ok("le cout Horeko d'origine aussi", "COUT_HOREKO_LIGNE" %in% names(r))
ok("la source est dite ligne a ligne", all(r$SOURCE_HEURES == "Paie"))
# 60 h pointees au service, 90 payees : chaque jour est multiplie par 1,5.
ok("le total d'heures du mois est celui de la paie",
   abs(sum(r$HEURES[r$SECTEUR == "Service"]) - 90) < 1e-9)
ok("le total de cout aussi",
   abs(sum(r$COUT_TRAVAIL[r$SECTEUR == "Service"]) - 2700) < 1e-9)
ok("la repartition reste celle d'Horeko",
   { s <- r %>% filter(SECTEUR == "Service"); length(unique(round(s$HEURES, 6))) == 1 })
ok("le pointage n'est pas ecrase", all(r$HEURES_HOREKO == rep(c(2, 1), length(jours))))

# Sans paie, Horeko reste tel quel : surtout pas de zero.
r0 <- recale_couts_travail(couts, NULL)
ok("sans paie, les heures sont celles d'Horeko", all(r0$HEURES == r0$HEURES_HOREKO))
ok("sans paie, la source le dit", all(r0$SOURCE_HEURES == "Horeko"))
ok("sans paie, le total du mois est NA et non zero", all(is.na(r0$HEURES_ONSS_MOIS)))

# Un secteur paye mais jamais pointe n'a aucun jour ou se poser : ses heures
# sont perdues pour le grain journalier. Les compter, c'est transformer une
# perte invisible en ecart lisible.
onss2 <- bind_rows(onss, tibble(ANNEE = 2026L, MOIS = 9L, SECTEUR = "Brasserie",
                                HEURES = 20, COUT = 600, N_FICHES = 1L))
nr <- onss_non_rattache(couts, onss2)
ok("un secteur paye sans pointage est compte",
   nrow(nr) == 1 && nr$SECTEUR == "Brasserie" && nr$HEURES == 20)
ok("rien a signaler -> NULL", is.null(onss_non_rattache(couts, onss)))
ok("le recalage ignore ce secteur plutot que d'inventer des jours",
   abs(sum(recale_couts_travail(couts, onss2)$HEURES) - 150) < 1e-9)
