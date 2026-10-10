# Volet Details : mailles, serie, composition, tickets, heures.
#
# Trois pieges de ce fichier ont deja coute une colonne fausse. Ils sont
# chacun couverts ici, nommement.
suppressMessages({library(dplyr); library(tibble); library(lubridate)
                  library(tidyr); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"; COUL_MATIERE<-"#d3c0ac"
COULEURS_SECTEURS <- c("Service"="#2980b9", "Transformation alimentaire"="#5B7B5A",
                       "Brasserie"="#d98236", "Support"="#8d7b68")
vecteur_jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
PREFIXE_TRI <- ".tri:"; col_tri <- function(n) paste0(PREFIXE_TRI, n)

# Doublures : plotly et shiny ne sont pas requis pour eprouver du calcul.
balise <- function(...) structure(list(args = list(...)), class = "tag")
div<-balise; span<-balise; p<-balise; icon<-balise; tagList<-balise
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise)
kpi_tile <- function(valeur, libelle, couleur, icone=NULL, sous_titre=NULL)
  structure(list(args=list(valeur, libelle, sous_titre %||% "")), class="tag")
bandeau_alerte <- function(afficher, texte, titre="", couleur="", icone="") {
  if (!isTRUE(afficher)) return(NULL); list(titre=titre, texte=texte) }
appels <- character(0); traces <- list()
plot_ly <- function(...) { appels <<- c(appels, "plot_ly")
  if (length(list(...))) traces[[length(traces)+1]] <<- list(...)
  structure(list(), class="p") }
plotly_empty <- function(...) { appels <<- c(appels, "plotly_empty"); structure(list(), class="p") }
add_bars<-function(p,...){force(p); traces[[length(traces)+1]]<<-list(...); p}
add_lines<-function(p,...){force(p); traces[[length(traces)+1]]<<-list(...); p}
add_histogram<-function(p,...){force(p); traces[[length(traces)+1]]<<-list(...); p}
add_trace<-function(p,...){force(p); traces[[length(traces)+1]]<<-list(...); p}
layout<-function(p,...){force(p); p}; change_cursor_plotly<-function(p){force(p); p}
raz <- function() { appels <<- character(0); traces <<- list() }
texte_tag <- function(x) {
  if (is.character(x)) return(paste(x, collapse=" "))
  if (inherits(x,"tag")) return(paste(vapply(x$args %||% list(), texte_tag, ""), collapse=" "))
  if (is.list(x)) return(paste(vapply(x, texte_tag, ""), collapse=" "))
  "" }

source("R/helpers.R",         encoding = "UTF-8", local = TRUE)
source("R/theme.R",           encoding = "UTF-8", local = TRUE)
source("R/kpi.R",             encoding = "UTF-8", local = TRUE)   # format_pct
source("R/objectifs.R",       encoding = "UTF-8", local = TRUE)
source("R/planning.R",        encoding = "UTF-8", local = TRUE)
source("R/sources_travail.R", encoding = "UTF-8", local = TRUE)
source("R/details.R",         encoding = "UTF-8", local = TRUE)

# ---------- 1. les mailles ----------
J <- as.Date("2026-09-17")   # un jeudi
ok("trois mailles", nrow(MAILLES_DETAIL) == 3)
ok("une cle inconnue retombe sur le jour", maille_detail("n'importe quoi")$CLE == "jour")
ok("une cle NULL aussi", maille_detail(NULL)$CLE == "jour")
ok("debut_maille(jour) est l'identite", debut_maille(J, "jour") == J)
ok("debut_maille(semaine) tombe le lundi", debut_maille(J, "semaine") == as.Date("2026-09-14"))
ok("fin_maille(semaine) tombe le dimanche", fin_maille(J, "semaine") == as.Date("2026-09-20"))
ok("debut_maille(mois) tombe le 1er", debut_maille(J, "mois") == as.Date("2026-09-01"))
ok("fin_maille(mois) tombe le dernier jour", fin_maille(J, "mois") == as.Date("2026-09-30"))
ok("fevrier bissextile fait 29 jours",
   fin_maille(as.Date("2028-02-10"), "mois") == as.Date("2028-02-29"))
# Le piege : libelle_maille() supposait recevoir un DEBUT de periode. Un jeudi
# passe a la maille « semaine » rendait « du 17/09 au 20/09 » — bornes fausses
# et duree de quatre jours.
ok("un jour quelconque est d'abord ramene au debut de sa periode",
   grepl("14/09", libelle_maille(J, "semaine")) && grepl("20/09", libelle_maille(J, "semaine")))
ok("le libelle court aussi", libelle_maille_court(J, "semaine") == libelle_maille_court(as.Date("2026-09-14"), "semaine"))
ok("la periode precedente d'un jour est le meme jour la semaine d'avant",
   periode_precedente(J, "jour") == J - 7)
ok("celle d'un mois est le mois d'avant",
   periode_precedente(as.Date("2026-09-01"), "mois") == as.Date("2026-08-01"))

# ---------- 2. la serie ----------
jours <- seq(as.Date("2026-08-01"), as.Date("2026-09-30"), by = "day")
lundi <- wday(jours, week_start = 1) == 1
KPI <- tibble(DATE = jours,
              ventes = ifelse(lundi, 0, 1500),
              Boisson = 600, Nourriture = 900, Jour = 500, Soir = 1000)
# L'objectif d'un lundi vaut zero : c'est ce que produit
# calculer_objectifs_journaliers() (R/objectifs.R), le lundi etant ferme.
OBJ <- tibble(DATE = jours, ventes = ifelse(lundi, 0, 1600))

s <- serie_detail(KPI, OBJ, min(jours), max(jours), "semaine")
# LE piege de summarise() : JOURS doit etre calcule AVANT CA. Place apres,
# `sum(CA > 0)` lit le CA deja agrege — un scalaire — et rend 1 partout.
ok("JOURS compte les jours OUVERTS, pas 1", all(s$JOURS > 1))
ok("une semaine pleine compte six jours ouverts", max(s$JOURS) == 6)
ok("le CA de la semaine est la somme de ses jours", all(s$CA == s$JOURS * 1500))
ok("l'objectif ne compte que les jours ouverts", all(s$OBJECTIF == s$JOURS * 1600))
# A la maille jour, un jour de fermeture n'est pas un mauvais jour : il sort.
ok("les jours fermes sont ecartes a la maille jour",
   nrow(serie_detail(KPI, OBJ, min(jours), max(jours), "jour")) == sum(!lundi))
ok("la serie est triee", !is.unsorted(s$PERIODE))
ok("base vide -> NULL", is.null(serie_detail(KPI[0,], OBJ, min(jours), max(jours))))

# ---------- 3. la composition ----------
cm <- composition_detail(KPI, OBJ, NULL, as.Date("2026-09-01"), "mois")
ok("un mois se compose de ses semaines", nrow(cm) >= 4)
ok("l'objectif descend a la maille de la composition", "OBJECTIF" %in% names(cm))
# Une semaine a cheval sur deux mois n'est comptee que pour ses jours du mois.
ok("les periodes a cheval sont marquees",
   any(!cm$COMPLET) && all(grepl("\\*$", cm$LABEL[!cm$COMPLET])))
ok("les periodes entieres ne le sont pas", !any(grepl("\\*$", cm$LABEL[cm$COMPLET])))
ok("le total du mois est juste",
   abs(sum(cm$CA) - sum(KPI$ventes[month(KPI$DATE) == 9])) < 1e-6)
# Meme piege de summarise() : OBJECTIF doit etre calcule AVANT CA, sinon
# l'objectif d'un jour ferme rentre dans le total.
ok("un jour ferme n'apporte pas son objectif",
   all(cm$OBJECTIF == cm$CA / 1500 * 1600))
# A l'heure, aucun objectif n'existe : NA, et surtout pas zero, qui se lirait
# « objectif depasse » et peindrait la journee en vert.
TICK <- tibble(DATE = rep(jours, each = 3), HEURE = rep(c(12, 19, 20), length(jours)),
               ID_TICKET = seq_len(3 * length(jours)),
               PRODUIT = "Café", PRODUIT_FULL = "Café", CATEGORIE = "BOISSONS CHAUDES",
               QUANTITE = 1, CA_HTVA = 10, CA_TVAC = 12.1)
cj <- composition_detail(KPI, OBJ, TICK, J, "jour")
ok("la journee se compose de ses heures", nrow(cj) == 3)
ok("pas d'objectif a l'heure", all(is.na(cj$OBJECTIF)))
ok("une heure est toujours complete", all(cj$COMPLET))
raz(); invisible(graph_composition_detail(cj, "jour"))
ok("sans objectif, une seule teinte, et pas le gris « sans objectif »",
   { t <- Filter(function(x) !is.null(x$y), traces)[[1]]
     length(unique(t$marker$color)) == 1 && t$marker$color[1] != COUL_NEUTRE })
raz(); invisible(graph_composition_detail(cm, "mois"))
ok("avec objectif, la couleur est celle de l'atteinte",
   { t <- Filter(function(x) !is.null(x$y), traces)[[1]]
     identical(t$marker$color, couleur_objectif(cm$CA, cm$OBJECTIF)) })
ok("la troncature est hachuree, pas grisee : le gris dit deja « sans objectif »",
   { t <- Filter(function(x) !is.null(x$y), traces)[[1]]
     all(t$marker$pattern$shape[!cm$COMPLET] == "/") &&
     all(t$marker$opacity[!cm$COMPLET] < 1) })

# ---------- 4. les tickets ----------
tk <- tickets_detail(TICK, as.Date("2026-09-14"), as.Date("2026-09-20"))
ok("une ligne par ticket", nrow(tk) == n_distinct(
     TICK$ID_TICKET[TICK$DATE >= as.Date("2026-09-14") & TICK$DATE <= as.Date("2026-09-20")]))
ok("le CA total est conserve",
   abs(sum(tk$CA) - sum(TICK$CA_HTVA[TICK$DATE >= as.Date("2026-09-14") &
                                     TICK$DATE <= as.Date("2026-09-20")])) < 1e-6)
ok("sans ID_TICKET, le volet se tait proprement",
   is.null(tickets_detail(TICK |> select(-ID_TICKET), min(jours), max(jours))))
ok("et le dit", grepl("ID_TICKET", table_tickets(NULL)$Info))

tt <- table_tickets(tk, "HTVA")
ok("le tableau porte ses jumelles de tri",
   all(c(col_tri("Jour"), col_tri("CA HTVA")) %in% names(tt)))
ok("et elles sont numeriques",
   is.numeric(tt[[col_tri("Jour")]]) && is.numeric(tt[[col_tri("CA HTVA")]]))

# Un produit sans taux de TVA : son CA HTVA vaut NA. Un ticket qui n'en
# contient QUE doit dire « inconnu », pas « zero » — zero se lit « offert ».
TROU <- TICK; TROU$CA_HTVA[TROU$HEURE == 20] <- NA_real_
tv <- tva_inconnue(TROU, min(jours), max(jours))
ok("les lignes sans taux sont comptees", tv$LIGNES == sum(TROU$HEURE == 20))
ok("leur montant TVAC reste connu", tv$CA_TVAC > 0)
ok("rien a signaler -> NULL", is.null(tva_inconnue(TICK, min(jours), max(jours))))
tk2 <- tickets_detail(TROU, min(jours), max(jours))
ok("un ticket entierement sans taux vaut NA, pas zero",
   { ids <- TROU$ID_TICKET[is.na(TROU$CA_HTVA)]
     all(is.na(tk2$CA[tk2$ID_TICKET %in% ids])) })
ok("et il s'affiche en tiret",
   table_tickets(tk2[is.na(tk2$CA), ][1, ], "HTVA")$`CA HTVA` == "—")
# bandeau_alerte() vient de R/helpers.R : le bandeau est une balise entiere,
# on l'aplatit plutot que d'aller chercher un champ qui n'existe pas.
al <- texte_tag(alerte_tickets(TROU, min(jours), max(jours)))
ok("l'alerte explique le taux manquant", grepl("taux de TVA", al))
ok("et renvoie vers le TVAC", grepl("TVAC", al))
ok("les balises restent des balises", !grepl("&lt;b&gt;", al))
ok("sans ID_TICKET, c'est l'autre manque qui prime",
   grepl("identifiant de ticket",
         texte_tag(alerte_tickets(TICK |> select(-ID_TICKET), J, J))))
# Un histogramme ne permet pas de lire ses propres extremes.
rp <- resume_paniers(tk)
ok("le resume donne le plus gros ticket", grepl("plus gros", rp))
ok("et la mediane", grepl("médiane", rp))
ok("sans ticket, une chaine vide", resume_paniers(NULL) == "")

# ---------- 5. les heures ----------
COUTS <- tibble(DATE = rep(jours, each = 2),
                SECTEUR = rep(c("Service","Support"), length(jours)),
                CRENEAU = rep(c("Soir","Journée"), length(jours)),
                HEURES = rep(c(12, 4), length(jours)),
                HEURES_HOREKO = rep(c(10, 3), length(jours)),
                COUT_TRAVAIL = rep(c(240, 80), length(jours)),
                COUT_HOREKO_LIGNE = rep(c(200, 60), length(jours)),
                COUT_COMPTA = 9000)
# A la journee, HEURES est une repartition d'un total mensuel, pas une mesure.
H <- heures_pointees(COUTS, J, "jour")
ok("ce sont les heures POINTEES", sum(H$HEURES) == 13)
ok("surtout pas les heures recalees", sum(H$HEURES) != 16)
ok("repli sur HEURES si le pointage manque",
   sum(heures_pointees(COUTS |> select(-HEURES_HOREKO), J, "jour")$HEURES) == 16)

ONSS <- tibble(ANNEE = 2026L, MOIS = 9L, SECTEUR = c("Service","Support"),
               HEURES = c(400, 120), COUT = c(9500, 2800), N_FICHES = c(5L, 2L))
hm <- heures_mois(COUTS, ONSS, as.Date("2026-09-01"))
ok("au mois, les heures payees apparaissent", sum(hm$H_PAYEES, na.rm=TRUE) == 520)
ok("le pointage reste a cote", all(hm$H_POINTEES > 0))
# Sans paie : NA et non zero. « La paie ne dit rien » et « la paie dit zero
# heure » ne se lisent pas pareil.
ok("sans paie, les heures payees sont NA",
   all(is.na(heures_mois(COUTS, NULL, as.Date("2026-09-01"))$H_PAYEES)))
ok("un mois sans donnee -> NULL", is.null(heures_mois(COUTS, ONSS, as.Date("2019-03-01"))))
ok("le cout comptable du mois est lu", cout_compta_mois(COUTS, as.Date("2026-09-01")) == 9000)
tm <- table_heures_mois(hm, 9000, ca = 26000)
ok("le CA par heure est calcule sur les heures PAYEES",
   { l <- tm[grepl("CA par heure", tm$Secteur), ]
     grepl("payée", l$Secteur) && l$`Coût paie` == "50€" })
ok("pas d'espaces parasites", !any(grepl("^ | $", unlist(tm))))
# L'aiguillage : le mois montre le reel, le reste le pointage.
ok("au mois, les heures payees",
   "Heures payées" %in% names(table_heures_periode(COUTS, ONSS, as.Date("2026-09-01"), "mois")))
for (m in c("jour", "semaine"))
  ok(paste0("a la maille ", m, ", le pointage seul"),
     "Créneau" %in% names(table_heures_periode(COUTS, ONSS, J, m)))
