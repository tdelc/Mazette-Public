# Le grand livre : filtres, agregats, tableau.
#
# Rien ici ne touche au reseau ni a Odoo : on part d'ecritures deja
# normalisees, c'est-a-dire de ce que normalise_ecritures() rend.
suppressMessages({library(dplyr); library(tibble); library(tidyr)
                  library(lubridate); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COUL_ROUGE<-"#c0392b"; COUL_AMBRE<-"#d98236"; COUL_VERT<-"#5B7B5A"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
balise <- function(...) structure(list(args = list(...)), class = "tag")
div<-balise; span<-balise; tagList<-balise; icon<-balise
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise)
texte_tag <- function(x) {
  if (is.character(x)) return(paste(x, collapse=" "))
  if (is.numeric(x)) return(paste(x, collapse=" "))
  if (inherits(x,"tag")) return(paste(vapply(x$args, texte_tag, ""), collapse=" "))
  if (is.list(x)) return(paste(vapply(x, texte_tag, ""), collapse=" "))
  "" }
# Le stub colle un separateur entre chaque argument : on compare des phrases,
# pas des espaces.
net <- function(x) gsub("\\s+", " ", texte_tag(x))
# Les graphes ne sont pas testes ici : on verifie qu'ils ne tombent pas.
plot_ly<-function(...) structure(list(n=0),class="p")
plotly_empty<-function(...) structure(list(n=0),class="p")
add_bars<-function(p,...){p$n <- p$n + 1; p}
layout<-function(p,...){force(p);p}; change_cursor_plotly<-function(p){force(p);p}
source("R/helpers.R",        encoding="UTF-8", local = TRUE)
source("R/theme.R",          encoding="UTF-8", local = TRUE)
source("R/plan_comptable.R", encoding="UTF-8", local = TRUE)
source("R/exploitation.R",   encoding="UTF-8", local = TRUE)   # COUL_POSTES
source("R/ecritures.R",      encoding="UTF-8", local = TRUE)

ecr <- function(date, compte, libelle, debit = 0, credit = 0,
                journal = "MZACH", piece = "MZACH/2026/07/0001",
                texte = "x", partenaire = NA_character_)
  tibble(DATE = as.Date(date), PIECE = piece, TEXTE = texte,
         PARTENAIRE = partenaire, DEBIT = debit, CREDIT = credit,
         JOURNAL = journal, COMPTE = compte, LIBELLE = libelle,
         CLASSE = substr(compte, 1, 1),
         ANNEE = year(as.Date(date)), MOIS = month(as.Date(date)))

brut <- bind_rows(
  ecr("2026-07-05", "600100", "Achats de boissons",  300),
  ecr("2026-07-05", "440000", "Fournisseurs",          0, 300),
  ecr("2026-07-20", "600100", "Achats de boissons",  500, 0,
      piece = "MZACH/2026/07/0002",
      partenaire = "Brasserie du Coin", texte = "Commande fûts"),
  ecr("2026-07-20", "440000", "Fournisseurs",            0, 500,
      piece = "MZACH/2026/07/0002", partenaire = "Brasserie du Coin"),
  ecr("2026-08-10", "700100", "Ventes boissons",       0, 2000,
      journal = "MZVEN", piece = "MZVEN/2026/08/0001"),
  ecr("2026-08-10", "411000", "Clients",            2000, 0,
      journal = "MZVEN", piece = "MZVEN/2026/08/0001"),
  ecr("2026-08-25", "620000", "Rémunérations",      4000, 0,
      journal = "SAL", piece = "SAL/2026/08/0001", partenaire = "ONSS"),
  ecr("2026-08-25", "455000", "Dettes salariales",      0, 4000,
      journal = "SAL", piece = "SAL/2026/08/0001", partenaire = "ONSS"))

e <- enrichit_ecritures(brut)

# ---------- l'enrichissement ----------
ok("une ligne reste une ligne", nrow(e) == nrow(brut))
# Le MEME vocabulaire que les trois onglets d'a cote : le plan comptable dit
# « Achats », la gestion dit « Matieres premieres », et faire cohabiter les
# deux dans un meme volet obligerait a traduire de tete.
ok("le poste PCMN est traduit en bloc de gestion",
   e$POSTE[e$COMPTE == "600100"][1] == "Achats" &&
   e$GESTION[e$COMPTE == "600100"][1] == "Matières premières")
ok("une vente aussi", e$GESTION[e$COMPTE == "700100"] == "Chiffre d'affaires")
ok("et le bilan est dit hors gestion",
   all(e$GESTION[e$BLOC == "Bilan"] == "Hors gestion"))
# Deux lectures d'un meme montant, et il faut les deux.
ok("SOLDE est la lecture comptable : debit moins credit",
   e$SOLDE[e$COMPTE == "700100"] == -2000)
ok("VALEUR est la lecture de gestion : positive quand le poste grossit",
   e$VALEUR[e$COMPTE == "700100"] == 2000 &&
   e$VALEUR[e$COMPTE == "620000"] == 4000)
# Un compte de bilan n'a aucun sens de gestion. Zero voudrait dire « il n'a
# rien fait », ce qui est faux : c'est NA.
ok("un compte de bilan n'a pas de valeur de gestion",
   is.na(e$VALEUR[e$COMPTE == "411000"]))
ok("le bloc separe resultat et bilan",
   all(e$BLOC[e$CLASSE %in% c("6","7")] == "Résultat") &&
   all(e$BLOC[!e$CLASSE %in% c("6","7")] == "Bilan"))
ok("aucune ecriture rend NULL, pas une erreur",
   is.null(enrichit_ecritures(brut[0, ])) && is.null(enrichit_ecritures(NULL)))

# ---------- les filtres ----------
ok("sans filtre, tout le resultat", nrow(filtre_ecritures(e)) == 4)
ok("le bilan a sa vue", nrow(filtre_ecritures(e, bloc = "Bilan")) == 4)
ok("et « Tout » ne retire rien", nrow(filtre_ecritures(e, bloc = "Tout")) == 8)
# « Resultat » par defaut n'est pas neutre : un compte bancaire voit passer
# dix fois le CA et ecraserait tout classement par montant.
ok("le defaut est le resultat, pas le bilan",
   all(filtre_ecritures(e)$BLOC == "Résultat"))
ok("les dates bornent des deux cotes",
   nrow(filtre_ecritures(e, du = "2026-08-01", bloc = "Tout")) == 4 &&
   nrow(filtre_ecritures(e, au = "2026-07-31", bloc = "Tout")) == 4)
ok("le journal filtre", nrow(filtre_ecritures(e, bloc = "Tout",
                                              journaux = "MZVEN")) == 2)
ok("plusieurs journaux a la fois",
   nrow(filtre_ecritures(e, bloc = "Tout",
                         journaux = c("MZVEN", "SAL"))) == 4)
ok("le poste filtre",
   nrow(filtre_ecritures(e, postes = "Matières premières")) == 2)
ok("le compte filtre",
   nrow(filtre_ecritures(e, bloc = "Tout", comptes = "411000")) == 1)
ok("les filtres se combinent",
   nrow(filtre_ecritures(e, du = "2026-07-10", postes = "Matières premières")) == 1)

# ---------- la recherche texte ----------
ok("elle cherche dans le libelle de la ligne",
   nrow(filtre_ecritures(e, texte = "fûts")) == 1)
ok("dans le partenaire",
   nrow(filtre_ecritures(e, bloc = "Tout", texte = "ONSS")) == 2)
ok("dans la piece",
   nrow(filtre_ecritures(e, bloc = "Tout", texte = "MZVEN")) == 2)
ok("dans le numero de compte",
   nrow(filtre_ecritures(e, texte = "600100")) == 2)
ok("sans se soucier de la casse",
   nrow(filtre_ecritures(e, texte = "BRASSERIE DU COIN")) == 1)
# fixed = TRUE : le texte vient d'un champ libre. Une parenthese non fermee
# ferait tomber tout le volet si on l'interpretait comme une regex.
ok("une parenthese ne fait pas tomber le volet",
   { r <- try(filtre_ecritures(e, texte = "Achat ("), silent = TRUE)
     !inherits(r, "try-error") && nrow(r) == 0 })
ok("un point n'est pas un joker",
   nrow(filtre_ecritures(e, bloc = "Tout", texte = "6001.0")) == 0)
ok("une recherche vide ne retire rien",
   nrow(filtre_ecritures(e, texte = "   ")) == 4)
# Le partenaire est NA sur la plupart des lignes : il ne doit pas emporter
# avec lui les autres colonnes cherchees.
ok("un partenaire absent ne fait pas disparaitre la ligne de la recherche",
   nrow(filtre_ecritures(e, texte = "Achats de boissons")) == 2)
ok("et la recherche traverse bien plusieurs colonnes",
   nrow(filtre_ecritures(e, texte = "boissons")) == 3)

# ---------- les KPI ----------
k <- net(kpi_ecritures(filtre_ecritures(e, bloc = "Tout")))
ok("le nombre d'ecritures y est", grepl("8 écritures", k))
ok("le nombre de pieces aussi", grepl("4 pièces", k))
ok("les totaux debit et credit", grepl("au débit", k) && grepl("au crédit", k))
# Sur une selection entiere, debit = credit. C'est un CONTROLE, pas une mesure.
ok("une selection entiere est equilibree", grepl("équilibré", k))
ok("et le vert le dit", grepl("#5B7B5A", k))
# Filtrer coupe des pieces en deux : le desequilibre devient normal, et le
# KPI doit le dire plutot que d'alarmer.
kf <- net(kpi_ecritures(filtre_ecritures(e), filtre_actif = TRUE))
ok("une selection filtree se desequilibre", grepl("d'écart", kf))
ok("mais le message l'explique", grepl("le filtre coupe des pièces", kf))
ok("et sans filtre actif, le meme ecart inquiete",
   grepl("c'est anormal", net(kpi_ecritures(filtre_ecritures(e)))))
ok("aucune ecriture se dit", grepl("Aucune écriture",
                                   net(kpi_ecritures(e[0, ]))))

# ---------- le classement des comptes ----------
t <- top_comptes(filtre_ecritures(e, bloc = "Tout"))
ok("un compte par ligne", nrow(t) == 6)
ok("le solde est comptable", t$SOLDE[t$COMPTE == "600100"] == 800)
ok("les ecritures sont comptees", t$N[t$COMPTE == "600100"] == 2)
# Trier sur le solde signe mettrait toutes les ventes en bas du graphe.
ok("le classement se fait sur la valeur absolue",
   { d <- top_comptes(filtre_ecritures(e, bloc = "Tout"), n = 2)
     setequal(d$COMPTE, c("620000", "455000")) })
ok("et le graphe rend les barres de bas en haut",
   all(diff(top_comptes(filtre_ecritures(e, bloc = "Tout"))$POIDS) >= 0))
ok("n limite le nombre de comptes",
   nrow(top_comptes(filtre_ecritures(e, bloc = "Tout"), n = 3)) == 3)
ok("aucune ecriture rend NULL", is.null(top_comptes(e[0, ])))

ok("un poste connu prend sa couleur",
   unname(couleur_poste("Matières premières")) == COUL_POSTES[["Matières premières"]])
ok("un poste hors plan prend le neutre",
   unname(couleur_poste("Non classé")) == COUL_NEUTRE)
ok("et le graphe ne tombe pas sur une selection vide",
   !is.null(graph_top_comptes(NULL)) && !is.null(graph_top_comptes(t)))

# ---------- le temps ----------
m <- solde_mensuel(filtre_ecritures(e, bloc = "Tout"))
ok("un mois x poste par ligne", nrow(m) == 5)
ok("les mois sont au premier du mois",
   all(m$PERIODE %in% as.Date(c("2026-07-01", "2026-08-01"))))
ok("juillet cumule les deux achats",
   m$SOLDE[m$PERIODE == as.Date("2026-07-01") &
           m$GESTION == "Matières premières"] == 800)
ok("une trace par poste dans le graphe",
   graph_solde_mensuel(m)$n == length(unique(m$GESTION)))
ok("et rien a tracer ne tombe pas", !is.null(graph_solde_mensuel(NULL)))

# ---------- le tableau ----------
tb <- table_ecritures(filtre_ecritures(e, bloc = "Tout"))
ok("une colonne par chose a lire",
   all(c("Date","Pièce","Journal","Compte","Intitulé","Libellé","Partenaire",
         "Poste","Débit","Crédit") %in% names(tb)))
# Debit et credit SEPARES, pas un solde signe : c'est la forme du grand livre,
# celle qui permet de retrouver la ligne dans Odoo sans refaire une
# soustraction de tete.
ok("le debit et le credit restent separes",
   all(xor(nzchar(tb$`Débit`), nzchar(tb$`Crédit`))))
ok("du plus recent au plus ancien", tb$Date[1] == "25/08/2026")
ok("la colonne Poste parle gestion, pas PCMN",
   "Matières premières" %in% tb$Poste && !"Achats" %in% tb$Poste)
ok("la date est lisible, pas une date ISO", grepl("^\\d{2}/\\d{2}/\\d{4}$", tb$Date[1]))
# DT trie sur ce qu'il AFFICHE : « 31/01/2026 » se classerait par son jour.
ok("les jumelles de tri sont la",
   all(col_tri(c("Date", "Débit", "Crédit")) %in% names(tb)))
ok("et celle de la date est numerique", is.numeric(tb[[col_tri("Date")]]))
ok("un partenaire absent s'affiche vide, pas « NA »",
   all(tb$Partenaire != "NA"))
ok("le tableau se plafonne",
   nrow(table_ecritures(filtre_ecritures(e, bloc = "Tout"), n_max = 2)) == 2)
# Zero ligne garde les MEMES en-tetes : une recherche qui ne trouve rien ne
# doit pas donner l'impression que l'ecran s'est casse.
ok("aucune ecriture rend un tableau vide, pas une erreur",
   nrow(table_ecritures(e[0, ])) == 0)
ok("et il garde toutes ses colonnes",
   identical(names(table_ecritures(e[0, ])), names(tb)))
ok("NULL, lui, rend bien une table vide", nrow(table_ecritures(NULL)) == 0)

# ---------- l'export ----------
ok("le nom du fichier est horodate",
   grepl("^ecritures_[0-9]{8}_[0-9]{4}\\.csv$",
         nom_export_ecritures(as.POSIXct("2026-10-29 14:05:00"))))
ok("et porte bien l'instant donne",
   nom_export_ecritures(as.POSIXct("2026-10-29 14:05:00")) ==
     "ecritures_20261029_1405.csv")
