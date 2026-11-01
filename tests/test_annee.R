# Le volet Année : le cumul de marge, et ce qu'il fait des mois inconnus.
#
# Deux defauts y vivaient, et aucun ne faisait d'erreur — ils affichaient
# simplement autre chose que la verite :
#
#   1. la courbe de marge cumulee NUMEROTAIT les jours apres avoir retire ceux
#      dont la marge est inconnue. Elle raccourcissait donc sans rien dire, et
#      se DECALAIT : un jour de decembre prenait le rang d'un debut d'octobre,
#      et se retrouvait compare a l'an dernier a cette date-la ;
#   2. l'ecart de marge vs N-1 remplacait une marge inconnue par ZERO. Un mois
#      ecarte plongeait alors l'ecart cumule de toute la marge de l'an dernier,
#      comme si on n'avait rien gagne ce mois-la.
suppressMessages({library(dplyr); library(tibble); library(tidyr)
                  library(lubridate); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
balise <- function(...) structure(list(args=list(...)), class="tag")
div<-balise; span<-balise; tagList<-balise; icon<-balise
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise)
kpi_tile <- function(...) balise(...)

# On CAPTURE ce que plotly recoit : c'est la seule facon d'eprouver un graphe
# sans navigateur, et c'est bien la donnee tracee qu'on veut verifier.
TRACES <- list()
plot_ly <- function(...) structure(list(), class = "p")
plotly_empty <- function(...) structure(list(vide = TRUE), class = "p")
add_trace <- function(p, ...) { TRACES[[length(TRACES) + 1L]] <<- list(...); p }
add_lines <- add_trace; add_bars <- add_trace
layout <- function(p, ...) p
ECART_RECU <- NULL

source("R/helpers.R",        encoding = "UTF-8", local = TRUE)
source("R/theme.R",          encoding = "UTF-8", local = TRUE)
source("R/kpi.R",            encoding = "UTF-8", local = TRUE)
source("R/plan_comptable.R", encoding = "UTF-8", local = TRUE)
source("R/exploitation.R",   encoding = "UTF-8", local = TRUE)
source("R/annee.R",          encoding = "UTF-8", local = TRUE)
# APRES le source : R/annee.R definit lui aussi graph_ecart_cumule(),
# et le sourcer apres la doublure l'ecraserait.
graph_ecart_cumule <- function(dat, ...) {
  ECART_RECU <<- dat; structure(list(), class = "p")
}

# Une annee complete, bien passee : aucun « aujourd'hui » ne vient la tronquer.
ANNEE <- 2024L
jours <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
db_kpi <- tibble(DATE = jours, ventes = 1000)
db_obj <- tibble(DATE = jours, ventes = 900)   # serie_annuelle() y lit « ventes »
mois_compta <- function(mois)
  bind_rows(lapply(mois, function(m) tibble(
    ANNEE = ANNEE, MOIS = m, COMPTE = c("700000", "610000"), LIBELLE = "x",
    VALEUR = c(30000, 12000), TYPE = "compte", AGREGE = TRUE)))

complet <- serie_annuelle(db_kpi, db_obj, mois_compta(1:12), ANNEE)
# Octobre manque : c'est le mois ecarte.
troue   <- serie_annuelle(db_kpi, db_obj, mois_compta(setdiff(1:12, 10)), ANNEE)

ok("une annee complete a une marge tous les jours", !any(is.na(complet$MARGE)))
ok("le mois retire laisse ses jours sans marge",
   all(is.na(troue$MARGE[month(troue$DATE) == 10])))
ok("et les autres mois n'y perdent rien",
   identical(troue$MARGE[month(troue$DATE) != 10],
             complet$MARGE[month(complet$DATE) != 10]))

# Un jour de FERMETURE dans le mois retire : zero vente, zero charge repartie,
# donc une marge qui a l'air connue et nulle. Elle ne l'est pas, et la laisser
# passer ferait peser le mois ecarte sur la comparaison avec N-1 par ses lundis.
ferme <- tibble(DATE = jours,
                ventes = if_else(wday(jours) == 2, 0, 1000))
s_ferme <- serie_annuelle(ferme, db_obj, mois_compta(setdiff(1:12, 10)), ANNEE)
ok("les jours de fermeture du mois retire sont inconnus eux aussi",
   all(is.na(s_ferme$MARGE[month(s_ferme$DATE) == 10])))
ok("mais un jour de fermeture d'un mois connu vaut bien zero",
   { l <- s_ferme %>% filter(month(DATE) == 9, ventes == 0)
     nrow(l) > 0 && all(abs(l$MARGE) < 0.01) })

# ---------- la courbe de marge cumulee ----------
trace_de <- function(serie, serie_m1) {
  TRACES <<- list()
  graph_marge_cumulee(serie, serie_m1, ANNEE)
  TRACES
}
t_plein <- trace_de(complet, complet)
y_plein <- t_plein[[1]]$y
ok("un point par jour d'ouverture", length(y_plein) == length(jours))
ok("le cumul monte", y_plein[length(y_plein)] > y_plein[1])

t_troue <- trace_de(troue, complet)
y_troue <- t_troue[[1]]$y
# LE point : la courbe garde sa LONGUEUR. Raccourcir la decalerait contre
# l'annee precedente, tracee sur le meme axe.
ok("la courbe garde un point par jour, trou compris",
   length(y_troue) == length(y_plein))
ok("les jours du mois retire sont des trous, pas des zeros",
   all(is.na(y_troue[month(jours) == 10])))
ok("le cumul n'avance pas pendant le trou",
   { sept <- max(y_troue[month(jours) == 9], na.rm = TRUE)
     nov1 <- which(month(jours) == 11)[1]
     # Le premier jour de novembre reprend exactement ou septembre s'est
     # arrete, plus sa propre marge : les 31 jours d'octobre n'ont rien ajoute.
     abs(y_troue[nov1] - sept - troue$MARGE[nov1]) < 0.01 })
ok("et il reprend ensuite, sans rattraper le mois perdu",
   y_troue[length(y_troue)] < y_plein[length(y_plein)])
# Sans connectgaps = FALSE, plotly relie les deux bords du trou et le mois
# ecarte redevient invisible.
ok("le trou est demande explicitement a plotly",
   isFALSE(t_troue[[1]]$connectgaps))
ok("sur l'annee precedente aussi", isFALSE(t_troue[[2]]$connectgaps))
ok("une annee sans aucune comptabilite ne trace rien",
   { s <- serie_annuelle(db_kpi, db_obj, NULL, ANNEE)
     isTRUE(graph_marge_cumulee(s, s, ANNEE)$vide) })

# ---------- l'ecart de marge vs N-1 ----------
ecart_de <- function(serie, serie_m1) {
  ECART_RECU <<- NULL
  graph_ecart_ym1(db_kpi, ANNEE, var = "marge", serie = serie, serie_m1 = serie_m1)
  ECART_RECU
}
e_plein <- ecart_de(complet, complet)
ok("deux annees identiques ne laissent aucun ecart",
   all(abs(e_plein$ECART) < 1))

e_troue <- ecart_de(troue, complet)
# LE point : un mois inconnu sort de la comparaison. Le compter pour zero
# ferait plonger l'ecart de toute la marge de l'an dernier.
ok("les jours sans marge sortent de la comparaison",
   nrow(e_troue) < nrow(e_plein))
ok("aucun jour d'octobre n'y figure", !any(month(e_troue$DATE) == 10))
ok("et l'ecart reste nul : on ne compare que ce qu'on connait",
   all(abs(e_troue$ECART) < 1))
# Le CA, lui, garde son zero : un jour sans vente a bien vendu zero.
ok("pour le CA, un jour sans vente vaut toujours zero",
   { ECART_RECU <<- NULL
     graph_ecart_ym1(db_kpi, ANNEE, var = "ventes")
     nrow(ECART_RECU) == sum(year(jours) == ANNEE) })
