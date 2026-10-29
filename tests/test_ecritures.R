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
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise, a=balise)
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
# Le defaut est « Tout » : un grand livre montre le grand livre, et le
# perimetre est un filtre qu'on applique ensuite.
ok("sans filtre, tout", nrow(filtre_ecritures(e)) == 8)
ok("le resultat a sa vue", nrow(filtre_ecritures(e, bloc = "Résultat")) == 4)
ok("le bilan a sa vue", nrow(filtre_ecritures(e, bloc = "Bilan")) == 4)
ok("et « Tout » ne retire rien", nrow(filtre_ecritures(e, bloc = "Tout")) == 8)
ok("les dates bornent des deux cotes",
   nrow(filtre_ecritures(e, du = "2026-08-01", bloc = "Tout")) == 4 &&
   nrow(filtre_ecritures(e, au = "2026-07-31", bloc = "Tout")) == 4)
ok("le journal filtre", nrow(filtre_ecritures(e, bloc = "Tout",
                                              journaux = "MZVEN")) == 2)
ok("plusieurs journaux a la fois",
   nrow(filtre_ecritures(e, bloc = "Tout",
                         journaux = c("MZVEN", "SAL"))) == 4)
ok("le poste filtre",
   nrow(filtre_ecritures(e, bloc = "Résultat", postes = "Matières premières")) == 2)
ok("le compte filtre",
   nrow(filtre_ecritures(e, bloc = "Tout", comptes = "411000")) == 1)
ok("les filtres se combinent",
   nrow(filtre_ecritures(e, bloc = "Résultat", du = "2026-07-10", postes = "Matières premières")) == 1)

# ---------- la recherche texte ----------
ok("elle cherche dans le libelle de la ligne",
   nrow(filtre_ecritures(e, bloc = "Résultat", texte = "fûts")) == 1)
ok("dans le partenaire",
   nrow(filtre_ecritures(e, bloc = "Tout", texte = "ONSS")) == 2)
ok("dans la piece",
   nrow(filtre_ecritures(e, bloc = "Tout", texte = "MZVEN")) == 2)
ok("dans le numero de compte",
   nrow(filtre_ecritures(e, bloc = "Résultat", texte = "600100")) == 2)
ok("sans se soucier de la casse",
   nrow(filtre_ecritures(e, bloc = "Résultat", texte = "BRASSERIE DU COIN")) == 1)
# fixed = TRUE : le texte vient d'un champ libre. Une parenthese non fermee
# ferait tomber tout le volet si on l'interpretait comme une regex.
ok("une parenthese ne fait pas tomber le volet",
   { r <- try(filtre_ecritures(e, bloc = "Résultat", texte = "Achat ("), silent = TRUE)
     !inherits(r, "try-error") && nrow(r) == 0 })
ok("un point n'est pas un joker",
   nrow(filtre_ecritures(e, bloc = "Tout", texte = "6001.0")) == 0)
ok("une recherche vide ne retire rien",
   nrow(filtre_ecritures(e, bloc = "Résultat", texte = "   ")) == 4)
# Le partenaire est NA sur la plupart des lignes : il ne doit pas emporter
# avec lui les autres colonnes cherchees.
ok("un partenaire absent ne fait pas disparaitre la ligne de la recherche",
   nrow(filtre_ecritures(e, bloc = "Résultat", texte = "Achats de boissons")) == 2)
ok("et la recherche traverse bien plusieurs colonnes",
   nrow(filtre_ecritures(e, bloc = "Résultat", texte = "boissons")) == 3)

# ---------- les KPI ----------
k <- net(kpi_ecritures(filtre_ecritures(e, bloc = "Tout")))
ok("le nombre d'ecritures y est", grepl("8 écritures", k))
ok("le nombre de pieces aussi", grepl("4 pièces", k))
ok("les totaux debit et credit", grepl("au débit", k) && grepl("au crédit", k))
# Sur une selection entiere, debit = credit. C'est un CONTROLE, pas une mesure.
ok("une selection entiere est equilibree", grepl("équilibré", k))
ok("et le vert le dit", grepl("#5B7B5A", k))
# Filtrer coupe des pieces en deux : le desequilibre redevient normal, et
# l'ambre le dit sans alarmer.
kf <- net(kpi_ecritures(filtre_ecritures(e, bloc = "Résultat")))
ok("une selection filtree se desequilibre", grepl("d'écart", kf))
ok("en ambre, pas en rouge", grepl("#d98236", kf) && !grepl("#c0392b", kf))
ok("aucune ecriture se dit", grepl("Aucune écriture",
                                   net(kpi_ecritures(e[0, ]))))

# ---------- le recapitulatif par poste ----------
# Il remplace deux graphiques qui faisaient joli sans rien apprendre : une
# ligne de pastilles est plus dense, et chacune est un filtre.
r <- recap_postes(filtre_ecritures(e, bloc = "Tout"))
ok("un poste par ligne", nrow(r) == 4)
ok("le solde est comptable, comme le tableau",
   r$SOLDE[r$GESTION == "Matières premières"] == 800)
ok("les ecritures sont comptees", r$N[r$GESTION == "Matières premières"] == 2)
# Trier sur le solde signe mettrait toutes les ventes en queue.
ok("le classement se fait sur la valeur absolue",
   abs(r$SOLDE[1]) == max(abs(r$SOLDE)))
ok("aucune ecriture rend NULL", is.null(recap_postes(e[0, ])))

# Les pastilles sont des BOUTONS : leurs identifiants viennent d'une liste
# fermee, pour qu'ils ne bougent pas quand la selection change.
ok("la liste des postes est fermee et complete",
   all(unique(e$GESTION) %in% postes_gestion()))
ok("et elle contient le hors gestion", HORS_GESTION %in% postes_gestion())
ok("l'identifiant d'une pastille ne depend que de son rang",
   id_chip_poste(3) == "ecr_chip_3")
ok("le meme poste garde le meme identifiant d'une selection a l'autre",
   match("Rémunérations", postes_gestion()) ==
     match("Rémunérations", postes_gestion()))

ok("un poste connu prend sa couleur",
   unname(couleur_poste("Matières premières")) == COUL_POSTES[["Matières premières"]])
ok("un poste hors plan prend le neutre",
   unname(couleur_poste("Hors gestion")) == COUL_NEUTRE)

# ---------- les couleurs de journal ----------
# Elles n'ont pas de sens en elles-memes : elles servent a reperer qu'une
# ligne vient d'ailleurs que ses voisines. Il faut donc qu'elles soient
# STABLES — une couleur qui change a chaque filtre n'apprend plus rien.
tous_j <- sort(unique(e$JOURNAL))
ok("un journal a une couleur", nzchar(couleur_journal("MZACH", tous_j)))
ok("deux journaux differents en ont deux",
   couleur_journal("MZACH", tous_j) != couleur_journal("MZVEN", tous_j))
ok("la couleur ne depend QUE de la liste de reference",
   couleur_journal("SAL", tous_j) ==
     couleur_journal("SAL", tous_j))
ok("retirer un autre journal de la selection ne la change pas",
   { t2 <- table_ecritures(filtre_ecritures(e, journaux = "SAL"),
                           journaux = tous_j)
     t1 <- table_ecritures(filtre_ecritures(e, bloc = "Tout"), journaux = tous_j)
     cj <- function(t, j) unique(t$Journal[grepl(j, t$Journal)])
     identical(cj(t2, "SAL"), cj(t1, "SAL")) })
ok("plus de journaux que de couleurs : la palette boucle, elle ne casse pas",
   { j <- paste0("J", sprintf("%02d", 1:20))
     length(unique(couleur_journal(j, j))) == length(PALETTE_JOURNAUX) })

# ---------- le tableau ----------
tb <- table_ecritures(filtre_ecritures(e, bloc = "Tout"), journaux = tous_j)
ok("une colonne par chose a lire",
   all(c("Date","Journal","Pièce","Compte","Libellé","Partenaire",
         "Poste","Débit","Crédit") %in% names(tb)))
# Le compte et son intitule ne se cherchent jamais l'un sans l'autre : une
# seule cellule, comme Odoo les affiche.
ok("le compte porte son intitule",
   any(grepl("^600100 · Achats de boissons", tb$Compte)))
# Debit et credit SEPARES, pas un solde signe : c'est la forme du grand
# livre, celle qui permet de retrouver la ligne dans Odoo sans refaire une
# soustraction de tete.
ok("le debit et le credit restent separes",
   all(xor(nzchar(tb$`Débit`), nzchar(tb$`Crédit`))))
# AU CENTIME. Arrondir a la dizaine d'euros — ce que faisait format_CA(x,-1) —
# fait douter de tout l'ecran des qu'un total ne retombe pas sur ses pieds.
ok("les montants sont au centime",
   grepl("^[0-9.]+,[0-9]{2}€$", tb$`Débit`[nzchar(tb$`Débit`)][1]))
ok("et rien n'est arrondi a la dizaine",
   { g <- filtre_ecritures(e, bloc = "Tout")
     g$DEBIT[1] <- 1234.56
     grepl("1\\.234,56€", table_ecritures(g)$`Débit`[1]) ||
       "1.234,56€" %in% table_ecritures(g)$`Débit` })
ok("du plus recent au plus ancien", tb$Date[1] == "25/08/2026")
ok("la date est lisible, pas une date ISO", grepl("^\\d{2}/\\d{2}/\\d{4}$", tb$Date[1]))
# Journal et poste sont des PASTILLES : sur deux cents lignes, une couleur se
# repere d'un coup d'oeil la ou un mot doit se lire.
ok("le journal est une pastille coloree",
   grepl("badge-etat", tb$Journal[1]) && grepl("#", tb$Journal[1]))
ok("le poste aussi",
   grepl("badge-etat", tb$Poste[1]) && grepl("Rémunérations|Hors gestion", tb$Poste[1]))
ok("et le texte y est, pas seulement la couleur",
   any(grepl("Matières premières", tb$Poste)))
# DT trie sur ce qu'il AFFICHE : une pastille se trierait par son code HTML.
ok("les jumelles de tri sont la",
   all(col_tri(c("Date", "Journal", "Poste", "Débit", "Crédit")) %in% names(tb)))
ok("celle de la date est numerique", is.numeric(tb[[col_tri("Date")]]))
ok("celle du poste porte le texte, pas le HTML",
   !any(grepl("<span", tb[[col_tri("Poste")]])))
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

# ---------- le forage : d'une alerte vers les ecritures ----------
# UN SEUL canal pour tout le volet : un lien pose sa consigne dans un input
# unique, et un seul observateur l'applique. Un bouton Shiny par lien
# demanderait des identifiants stables, donc des observateurs crees a la
# volee, qui s'empilent a chaque redessin de la liste d'alertes.
ok("une consigne nomme ses filtres",
   forage_spec(texte = "793000") == "texte=793000;bloc=Tout")
ok("les dates y entrent au format ISO",
   forage_spec(du = "2026-06-01", au = "2026-06-30") ==
     "du=2026-06-01;au=2026-06-30;bloc=Tout")
ok("un forage de mois prend le mois ENTIER, bornes comprises",
   forage_mois(as.Date("2026-06-01")) ==
     "du=2026-06-01;au=2026-06-30;bloc=Tout")
ok("meme sur un mois de 31 jours",
   grepl("au=2026-07-31", forage_mois(as.Date("2026-07-01"))))
ok("et il peut porter un texte en plus",
   grepl("texte=604000", forage_mois(as.Date("2026-06-01"), texte = "604000")))

f <- analyse_forage(forage_mois(as.Date("2026-06-01"), texte = "x"))
ok("la consigne se relit", f$texte == "x" && f$du == "2026-06-01")
ok("et rend aussi le perimetre", f$bloc == "Tout")
ok("une consigne vide ne rend rien",
   is.null(analyse_forage("")) && is.null(analyse_forage(NULL)))

# Le lien est un <a> brut : pas d'identifiant, donc autant qu'on veut.
l <- lien_forage("Voir les écritures", forage_spec(texte = "793000"))
ok("le lien porte la consigne", grepl("texte=793000", l$args$onclick))
ok("il ecrit dans l'input unique", grepl(INPUT_FORAGE, l$args$onclick))
ok("et il n'emporte pas la page avec lui",
   grepl("return false", l$args$onclick))
ok("sans consigne, pas de lien", is.null(lien_forage("x", NULL)))
# Une apostrophe dans la consigne fermerait la chaine JavaScript qui
# l'entoure : la consigne doit arriver SANS apostrophe entre ses guillemets.
ok("les apostrophes sont retirees de la consigne",
   grepl("'texte=lété;bloc=Tout'",
         lien_forage("x", forage_spec(texte = "l'été"))$args$onclick,
         fixed = TRUE))

h <- lien_forage_html("Écritures", forage_spec(texte = "604000"))
ok("la version HTML sert dans une cellule de tableau",
   grepl("^<a ", h) && grepl("604000", h))
ok("et rend du vide quand il n'y a rien a montrer",
   lien_forage_html("x", "") == "")

# La destination des controles d'import se DEDUIT de leur nom : ces tables
# sont sauvees dans le .RData, leur ajouter une colonne casserait les caches.
ok("un compte hors plan mene a ce compte",
   grepl("texte=793000",
         forage_import("Comptes 6 et 7 classés", "793000 — hors du plan.")))
ok("la piece de reprise mene a sa date",
   forage_import("Pièce de reprise", "x", reprise = as.Date("2026-06-30")) ==
     "du=2026-06-30;au=2026-06-30;bloc=Tout")
ok("un mois ecarte mene a ce mois",
   grepl("du=2026-06-01",
         forage_import("Mois d'Odoo écartés", "1 mois ... (06/2026) ...")))
ok("un controle sans destination n'en invente pas",
   is.na(forage_import("Débit = crédit", "écart global de 0 €")))
ok("c'est vectorise, comme la table qu'il lit",
   length(forage_import(c("Pièce de reprise", "Débit = crédit"), c("a", "b"),
                        reprise = as.Date("2026-06-30"))) == 2)
