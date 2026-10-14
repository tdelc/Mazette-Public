# Planning : l'historisation du calendrier, et la notion d'heure VARIABLE.
suppressMessages({library(dplyr); library(tibble); library(lubridate)
                  library(tidyr); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"
COULEURS_SECTEURS <- c("Service"="#2980b9", "Transformation alimentaire"="#5B7B5A",
                       "Brasserie"="#d98236", "Support"="#8d7b68")
vecteur_jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
balise <- function(...) structure(list(args=list(...)), class="tag")
div<-balise; span<-balise; tagList<-balise; icon<-balise
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise)
plot_ly<-function(...) structure(list(),class="p"); plotly_empty<-function(...) structure(list(),class="p")
add_bars<-function(p,...){force(p);p}; add_lines<-function(p,...){force(p);p}
add_trace<-function(p,...){force(p);p}; layout<-function(p,...){force(p);p}
change_cursor_plotly<-function(p){force(p);p}; kpi_tile<-function(...) balise(...)
source("R/helpers.R",  encoding = "UTF-8", local = TRUE)
source("R/theme.R",    encoding = "UTF-8", local = TRUE)
source("R/kpi.R",      encoding = "UTF-8", local = TRUE)
source("R/travail.R",  encoding = "UTF-8", local = TRUE)   # jour_ferme
source("R/planning.R", encoding = "UTF-8", local = TRUE)

# ---------- 1. la date, d'ou qu'elle vienne ----------
# Un aller-retour par Google Sheets rend les dates en texte, en POSIXct, ou en
# numero de serie. On ne peut rien supposer de ce qui revient.
ok("une date ISO", parse_date_souple("2026-09-17") == as.Date("2026-09-17"))
ok("une date belge saisie a la main", parse_date_souple("17/09/2026") == as.Date("2026-09-17"))
ok("une Date passe telle quelle", parse_date_souple(as.Date("2026-09-17")) == as.Date("2026-09-17"))
ok("un POSIXct aussi", parse_date_souple(as.POSIXct("2026-09-17 14:00", tz="UTC")) == as.Date("2026-09-17"))
# Le numero de serie : origine 1899-12-30, et surtout pas 1900-01-01.
ok("un numero de serie Sheets", parse_date_souple(46282) == as.Date("2026-09-17"))
ok("un format inconnu rend NA, et la ligne sera ecartee",
   is.na(parse_date_souple("pas une date")))
ok("vectorise", length(parse_date_souple(c("2026-09-17", "18/09/2026"))) == 2)

# googlesheets4 rend une colonne en LISTE des qu'il y voit des types meles.
# unlist() aplatirait en perdant l'alignement sur les cellules vides.
ok("une colonne en liste est aplatie sans perdre de ligne",
   length(aplatit_colonne(list("a", character(0), "c"))) == 3)
ok("et la cellule vide devient NA", is.na(aplatit_colonne(list("a", character(0)))[2]))
ok("une colonne ordinaire n'est pas touchee",
   identical(aplatit_colonne(c("a","b")), c("a","b")))

# ---------- 2. la normalisation ----------
pl <- tibble(DATE = c("2026-09-17", "2026-09-17", "17/09/2026", ""),
             SERVICE = c("Service", "Service", "Cuisine", "Service"),
             HEURES = c("4,5", "3", "6", "2"))
n <- normalise_planning(pl)
ok("les doublons jour x service sont additionnes",
   n$HEURES[n$SERVICE == "Service"] == 7.5)
ok("la virgule decimale est lue", 7.5 %in% n$HEURES)
ok("les deux formats de date se rejoignent", n_distinct(n$DATE) == 1)
ok("une ligne sans date est ecartee", nrow(n) == 2)
ok("une table vide rend NULL", is.null(normalise_planning(pl[0, ])))
ok("une table sans les colonnes du contrat aussi",
   is.null(normalise_planning(tibble(X = 1))))

# ---------- 3. la fusion ----------
# La deduplication se fait PAR JOUR, pas par jour x service : sur un jour que
# le calendrier couvre encore, c'est lui qui fait autorite EN ENTIER. Sinon la
# ligne d'un service annule depuis resterait a vie.
ancien <- tibble(DATE = c("2026-09-16", "2026-09-17", "2026-09-17"),
                 SERVICE = c("Service", "Service", "Cuisine"),
                 HEURES = c(10, 8, 6))
nouveau <- tibble(DATE = "2026-09-17", SERVICE = "Service", HEURES = 5)
f <- fusionne_planning(ancien, nouveau)
ok("le jour couvert par l'import est remplace EN ENTIER",
   nrow(f[f$DATE == as.Date("2026-09-17"), ]) == 1)
ok("un service annule ne survit pas",
   !"Cuisine" %in% f$SERVICE[f$DATE == as.Date("2026-09-17")])
ok("les jours que l'import ne couvre pas sont gardes",
   f$HEURES[f$DATE == as.Date("2026-09-16")] == 10)
ok("sans import, l'historique est rendu tel quel",
   nrow(fusionne_planning(ancien, NULL)) == 3)
ok("sans historique, l'import suffit", nrow(fusionne_planning(NULL, nouveau)) == 1)
ok("sans rien, NULL", is.null(fusionne_planning(NULL, NULL)))

# ---------- 4. le jour ferme ----------
# La regle vivait en creux dans un filter(.wd != 1) : lisible pour qui lisait
# cette fonction, invisible pour le reste du dashboard. Le volet Planning l'a
# d'ailleurs manquee, et comptait les heures de preparation du lundi comme du
# service.
ok("le lundi est ferme", jour_ferme(as.Date("2026-09-14")))
ok("le mardi ne l'est pas", !jour_ferme(as.Date("2026-09-15")))
ok("vectorise", identical(jour_ferme(as.Date(c("2026-09-14","2026-09-15"))),
                          c(TRUE, FALSE)))

# ---------- 5. l'heure variable ----------
# « Heure variable » = service, jour ouvert, creneau de service. Les trois
# conditions ensemble, et la meme definition que dans le volet Travail.
tr <- tibble(DATE = as.Date(c("2026-09-15","2026-09-15","2026-09-14","2026-09-15")),
             SECTEUR = c("Service","Support","Service","Service"),
             CRENEAU = c("Soir","Journée","Soir","Journée"),
             HEURES = c(10, 5, 7, 3))
hv <- heures_variables_reelles(tr)
ok("seules les heures de service comptent", sum(hv$VALEUR) == 10)
ok("le support n'est pas variable", sum(hv$VALEUR) != 15)
ok("un lundi ne compte pas", sum(hv$VALEUR) != 17)
ok("un creneau hors service non plus", sum(hv$VALEUR) != 13)

# ---------- 6. le secteur ----------
ok("le libelle d'Horeko est normalise",
   normalise_secteur("Transfo alimentaire") == "Transformation alimentaire")
ok("celui de la paie aussi", normalise_secteur("Fabrique de boissons") == "Brasserie")
ok("l'ancien libelle d'Horeko aussi", normalise_secteur("Fabrik de boissons") == "Brasserie")
ok("un libelle deja au bon format passe", normalise_secteur("Service") == "Service")
# Un secteur inattendu doit RESTER VISIBLE plutot que de disparaitre.
ok("un libelle inconnu est nomme, pas jete",
   normalise_secteur("Atelier poterie") == "Secteur inconnu")
