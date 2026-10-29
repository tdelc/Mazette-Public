# Le classement PCMN : la seule chose dont tout le volet Compta depend.
suppressMessages({library(dplyr); library(tibble); library(lubridate); library(tidyr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
source("R/helpers.R",       encoding = "UTF-8", local = TRUE)
source("R/exploitation.R",  encoding = "UTF-8", local = TRUE)   # etiquette_periode
source("R/plan_comptable.R", encoding = "UTF-8", local = TRUE)

cpt <- function(compte, valeur, annee = 2026L, mois = 1L, libelle = "x")
  tibble(ANNEE = annee, MOIS = mois, COMPTE = compte, LIBELLE = libelle,
         VALEUR = valeur, TYPE = "compte", AGREGE = TRUE)

db <- bind_rows(
  cpt("700000", 10000, libelle = "Ventes"),
  cpt("604000",  3000, libelle = "Achats"),
  cpt("609000",   500, libelle = "Variation de stocks"),
  cpt("620000",  4000, libelle = "Rémunérations"),
  cpt("630000",   800, libelle = "Amortissements"),
  cpt("610000",  1200, libelle = "Services et biens divers"),
  cpt("650000",   100, libelle = "Charges financières"),
  cpt("750000",    50, libelle = "Produits financiers"))

c1 <- classe_comptes(db)
# 609 doit l'emporter sur 60 : teste du plus long prefixe au plus court.
ok("609 est une variation de stocks, pas un achat",
   c1$POSTE[c1$COMPTE == "609000"] == "Variation de stocks")
ok("604 reste un achat", c1$POSTE[c1$COMPTE == "604000"] == "Achats")
ok("70 est du chiffre d'affaires",
   c1$POSTE[c1$COMPTE == "700000"] == "Chiffre d'affaires")
ok("un produit a le sens +1", c1$SENS_G[c1$COMPTE == "700000"] == 1)
ok("une charge a le sens -1", c1$SENS_G[c1$COMPTE == "604000"] == -1)
ok("PERIODE se deduit d'ANNEE et MOIS",
   all(c1$PERIODE == as.Date("2026-01-01")))

# Un compte hors plan doit SE VOIR, pas disparaitre d'un total.
hors <- bind_rows(db, cpt("123456", 999, libelle = "Compte de bilan"))
ok("un compte inconnu tombe dans Non classe",
   classe_comptes(hors)$POSTE[classe_comptes(hors)$COMPTE == "123456"] == "Non classé")
ok("et le controle le nomme",
   { nc <- comptes_non_classes(hors); nrow(nc) == 1 && nc$COMPTE == "123456" })
ok("rien d'inclassable -> table vide", nrow(comptes_non_classes(db)) == 0)

# Les lignes de rubrique de l'ancien export feraient double emploi.
avec_rubrique <- bind_rows(db, cpt("700000", 10000) |> mutate(TYPE = "rubrique"))
ok("les lignes de rubrique sont ecartees",
   nrow(classe_comptes(avec_rubrique)) == nrow(db))
ok("les reprises « dont » aussi",
   nrow(classe_comptes(bind_rows(db, cpt("700000", 1) |> mutate(AGREGE = FALSE)))) == nrow(db))

# ---------- les soldes ----------
cr <- compte_resultat(db)
solde <- function(nom) cr$VALEUR[cr$POSTE == nom]
ok("la marge brute : CA - achats - variation",
   abs(solde("Marge brute d'exploitation") - (10000 - 3000 - 500)) < 1e-9)
ok("le resultat d'exploitation retire aussi les charges d'exploitation",
   abs(solde("Résultat d'exploitation") - (10000 - 3000 - 500 - 1200 - 4000 - 800)) < 1e-9)
ok("le resultat avant impots ajoute le financier",
   abs(solde("Résultat avant impôts") -
       (10000 - 3000 - 500 - 1200 - 4000 - 800 - 100 + 50)) < 1e-9)
ok("chaque solde est marque comme tel",
   all(cr$TYPE_LIGNE[cr$POSTE %in% SOLDES_PCMN$SOLDE] == "solde"))
ok("les postes ne le sont pas",
   all(cr$TYPE_LIGNE[cr$POSTE == "Chiffre d'affaires"] == "poste"))

# ---------- la consolidation ----------
# Elle doit se faire EN AMONT du regroupement : consolider apres coup donnerait
# le meme total mais un detail de comptes incoherent avec lui.
trim <- bind_rows(cpt("700000", 100, mois = 1L), cpt("700000", 200, mois = 2L),
                  cpt("700000", 300, mois = 3L), cpt("700000", 400, mois = 4L))
r <- resultat_par_poste(trim, unite = "trimestre")
ok("un trimestre regroupe ses trois mois", nrow(r) == 2)
ok("et somme juste", r$VALEUR[r$PERIODE == as.Date("2026-01-01")] == 600)
ok("l'annee regroupe tout",
   resultat_par_poste(trim, unite = "annee")$VALEUR == 1000)
ok("N_MOIS dit combien de mois composent la periode",
   periodes_compta(trim, "trimestre")$N_MOIS[
     periodes_compta(trim, "trimestre")$PERIODE == as.Date("2026-01-01")] == 3)
# Un trimestre a 2 mois sur 3 doit se voir : c'est ce que N_MOIS sert a dire.
ok("un trimestre incomplet se voit",
   periodes_compta(trim |> filter(MOIS != 2), "trimestre")$N_MOIS[1] == 1)

ok("une table vide ne fait pas tomber le classement",
   { v <- db[0, ]; is.null(classe_comptes(v)) || nrow(classe_comptes(v)) == 0 })
ok("un compte de resultat vide rend une table vide",
   nrow(compte_resultat(db[0, ])) == 0)

# ---------- la classe 79 : l'affectation du resultat, pas du resultat ----------
# 793000 « perte a reporter » arrive avec la piece de reprise et porte, d'un
# coup, le resultat de tout l'exercice precedent. La classer ne doit RIEN
# changer aux chiffres : elle doit rester hors des blocs de gestion, comme les
# impots — seul le bruit « compte hors plan » disparait.
db79 <- bind_rows(
  tibble(ANNEE = 2026L, MOIS = 6L, COMPTE = "700000", LIBELLE = "Ventes",
         VALEUR = 10000, TYPE = "compte", AGREGE = TRUE),
  tibble(ANNEE = 2026L, MOIS = 6L, COMPTE = "793000", LIBELLE = "Perte à reporter",
         VALEUR = 250000, TYPE = "compte", AGREGE = TRUE))
c79 <- classe_comptes(db79)
ok("793000 est classe", c79$POSTE[c79$COMPTE == "793000"] == "Affectation du résultat")
ok("et n'est donc plus un compte hors plan",
   !"793000" %in% comptes_non_classes(db79)$COMPTE)
ok("sa section n'est reconnue par aucun bloc de gestion",
   is.na(bloc_gestion("Affectation du résultat", "Affectation du résultat")))
ok("le chiffre d'affaires ne bouge pas d'un centime",
   postes_exploitation(db79)$CA == 10000)
ok("ni les charges", postes_exploitation(db79)$CHARGES == 0)
ok("ni la marge", postes_exploitation(db79)$MARGE == 10000)
