# Ce que plotly trace vraiment, une fois la courbe construite.
#
# test_annee.R double add_trace() et verifie la DONNEE : un point par jour
# d'ouverture, trou compris. Mais plotly retouche la serie au moment du trace,
# et le navigateur montre donc autre chose que ce que R a calcule : les jours
# inconnus de FIN de serie disparaissent (la courbe s'arrete au dernier mois
# connu), et un trou INTERIEUR se reduit a une seule coupure.
#
# Ce qui doit survivre a cette retouche, c'est le RANG des jours connus : c'est
# lui qui aligne l'annee sur N-1 tracee au-dessous. On l'eprouve ici sur le
# graphe reellement bati, parce qu'aucune doublure ne peut le faire.
suppressMessages({library(dplyr); library(tibble); library(tidyr)
                  library(lubridate); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")

if (!requireNamespace("plotly", quietly = TRUE)) {
  cat("  ok   test ignore : plotly absent\n")
} else {
suppressMessages(library(plotly))

COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
for (f in c("R/helpers.R", "R/theme.R", "R/kpi.R", "R/plan_comptable.R",
            "R/exploitation.R", "R/annee.R"))
  source(f, encoding = "UTF-8", local = TRUE)

# Une annee passee, ouverte tous les jours : le rang d'un jour est donc son
# numero dans l'annee, ce qui rend les attentes lisibles.
ANNEE <- 2024L
jours <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
db_kpi <- tibble(DATE = jours, ventes = 1000)
db_obj <- tibble(DATE = jours, ventes = 900)
compta <- function(mois)
  bind_rows(lapply(mois, function(m) tibble(
    ANNEE = ANNEE, MOIS = m, COMPTE = c("700000", "610000"), LIBELLE = "x",
    VALEUR = c(30000, 12000), TYPE = "compte", AGREGE = TRUE)))
serie <- function(mois) serie_annuelle(db_kpi, db_obj, compta(mois), ANNEE)

# La trace telle que le navigateur la recevra.
tracee <- function(s) {
  d <- plotly::plotly_build(graph_marge_cumulee(s, s, ANNEE))$x$data[[1]]
  list(x = d$x, y = d$y, trous = which(is.na(d$y)))
}

plein <- tracee(serie(1:12))
ok("l'annee complete trace un point par jour",
   length(plein$y) == length(jours))
ok("sans aucune coupure", length(plein$trous) == 0)

# Octobre ecarte : 31 jours inconnus au MILIEU de l'annee.
milieu <- tracee(serie(setdiff(1:12, 10)))
ok("un trou interieur se reduit a une seule coupure",
   length(milieu$trous) == 1)
ok("les jours du trou ne sont plus traces",
   length(milieu$y) == length(jours) - 31 + 1)
# LE point : novembre et decembre gardent leur rang. Sans cela la courbe se
# tasserait vers la gauche et se comparerait au mauvais moment de N-1.
ok("le dernier jour garde son rang",
   milieu$x[length(milieu$x)] == length(jours))
ok("et la reprise se pose bien au 1er novembre",
   milieu$x[milieu$trous + 1] == which(jours == as.Date("2024-11-01")))

# Decembre ecarte : les jours inconnus sont en FIN de serie.
queue <- tracee(serie(1:11))
ok("un trou de fin de serie arrete simplement la courbe",
   length(queue$trous) == 0 &&
   queue$x[length(queue$x)] == which(jours == as.Date("2024-11-30")))
ok("et le cumul affiche est celui de fin novembre",
   abs(tail(queue$y, 1) - tail(na.omit(plein$y[month(jours) == 11]), 1)) < 0.01)
}
