# R/odoo.R : des ecritures Odoo a DB_COMPTA.
#
# La fixture n'est pas inventee : c'est l'extrait reel de septembre 2026
# (journaux MZACH, MZVEN, TRI1), au format que la vue liste exporte.
suppressMessages({library(dplyr); library(tibble); library(stringr)
                  library(lubridate); library(tidyr); library(readr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")

COULEURS_SECTEURS <- c("Service"="#2980b9", "Transformation alimentaire"="#5B7B5A",
                       "Brasserie"="#d98236", "Support"="#8d7b68")
source("R/plan_comptable.R", encoding="UTF-8")
source("R/sources_travail.R", encoding="UTF-8")   # nombre_europeen, SECTEUR_NON_VENTILE
source("R/planning.R",        encoding="UTF-8")   # parse_date_souple
source("R/helpers.R",         encoding="UTF-8")   # ratio_pct
source("R/exploitation.R",    encoding="UTF-8")   # postes_exploitation
source("R/odoo.R",            encoding="UTF-8")

FIXTURE <- "tests/fixtures/ecritures_odoo.tsv"

# ---------- 1. lecture ----------
brut <- lit_ecritures_fichier(FIXTURE)
ok("le separateur est devine (tabulation)", ncol(brut) == 9)
ok("toutes les lignes sont lues", nrow(brut) == 25)
ok("un fichier absent rend NULL", is.null(lit_ecritures_fichier("/tmp/pas_la.csv")))
ok("un chemin NULL rend NULL", is.null(lit_ecritures_fichier(NULL)))
# Le point-virgule : un CSV passe par un tableur belge.
pv <- tempfile(fileext=".csv")
writeLines(c("Date;Numéro;Compte;Partenaire;Libellé;Débit;Crédit",
             "2026-09-03;MZVEN/2026/00001;700200 Ventes 12%;X;Nourriture;;1.785,50"), pv)
ok("le point-virgule aussi", ncol(lit_ecritures_fichier(pv)) == 7)

# ---------- 2. normalisation ----------
e <- normalise_ecritures(brut)
ok("une ligne par ecriture", nrow(e) == 25)
ok("le code de compte est separe du libelle",
   all(c("623700","700200","550003") %in% e$COMPTE))
ok("le libelle est nettoye",
   e$LIBELLE[e$COMPTE == "623700"][1] == "Titres-repas")
ok("le journal vient du prefixe de la piece",
   setequal(unique(e$JOURNAL), c("MZACH","TRI1","MZVEN")))
# « 1.234,12 » : point de milliers, virgule decimale.
ok("les nombres belges sont lus",
   abs(sum(e$DEBIT) - 9420.47) < 0.01 && abs(sum(e$CREDIT) - 9846.27) < 0.01)
ok("une case vide vaut zero, pas NA", !any(is.na(e$DEBIT)) && !any(is.na(e$CREDIT)))
ok("les dates sont des dates", inherits(e$DATE, "Date") && !any(is.na(e$DATE)))
ok("la classe est le premier chiffre", all(e$CLASSE == substr(e$COMPTE, 1, 1)))
# Une ligne sans compte est un reste de mise en page, pas une ecriture.
sale <- bind_rows(brut, brut[1,] |> mutate(Compte = "", Débit = ""))
ok("une ligne sans compte est ecartee", nrow(normalise_ecritures(sale)) == 25)
ok("une colonne manquante est NOMMEE, pas devinee",
   { m <- try(normalise_ecritures(brut |> select(-Débit)), silent=TRUE)
     inherits(m,"try-error") && grepl("Débit", m) })

# ---------- 3. le secteur des comptes d'achat ----------
ok("fabrique de boisson -> Brasserie", secteur_par_compte("600100") == "Brasserie")
ok("sa variation de stock aussi",    secteur_par_compte("609010") == "Brasserie")
ok("transformation alimentaire",      secteur_par_compte("600200") == "Transformation alimentaire")
ok("bar -> Service",                  secteur_par_compte("604100") == "Service")
ok("variation marchandises bar -> Service", secteur_par_compte("609410") == "Service")
ok("fournitures -> Support",          secteur_par_compte("601000") == "Support")
# 604000 est volontairement absent de la table : on ignore comment le ventiler.
ok("604000 tombe dans Non ventile",   secteur_par_compte("604000") == SECTEUR_NON_VENTILE)
# Hors perimetre matiere : la question ne se pose pas, et NA le dit.
ok("une vente n'a pas de secteur",    is.na(secteur_par_compte("700200")))
ok("un salaire non plus",             is.na(secteur_par_compte("623700")))
ok("un compte de bilan non plus",     is.na(secteur_par_compte("550003")))
ok("vectorise", length(secteur_par_compte(c("600100","700200","604000"))) == 3)
ok("le prefixe le plus long l'emporte",
   { t <- tibble(PREFIXE = c("60","600100"), SECTEUR = c("Support","Brasserie"))
     secteur_par_compte("600100", t) == "Brasserie" })

# ---------- 4. l'agregation ----------
cp <- compta_mensuelle(e)
ok("seules les classes 6 et 7 subsistent", all(substr(cp$COMPTE,1,1) %in% c("6","7")))
ok("sept comptes de resultat dans l'extrait", nrow(cp) == 7)
# LA formule : VALEUR = SENS * (CREDIT - DEBIT), magnitude positive.
ok("une charge sort positive", cp$VALEUR[cp$COMPTE == "623700"] == 800)
ok("un produit aussi",         cp$VALEUR[cp$COMPTE == "700200"] == 1785.50)
ok("et l'autre produit",       cp$VALEUR[cp$COMPTE == "700300"] == 620.00)
# Une note de credit DOIT sortir negative : elle diminue la charge.
avoir <- e |> filter(COMPTE == "623700") |> slice(1) |>
  mutate(DEBIT = 0, CREDIT = 100)
ok("une note de credit diminue la charge",
   compta_mensuelle(bind_rows(e, avoir))$VALEUR[
     compta_mensuelle(bind_rows(e, avoir))$COMPTE == "623700"] == 700)
ok("le mois vient de la date", all(cp$ANNEE == 2026) && all(cp$MOIS == 9))
ok("base vide -> NULL", is.null(compta_mensuelle(NULL)))
ok("aucune classe 6/7 -> NULL",
   is.null(compta_mensuelle(e |> filter(!CLASSE %in% c("6","7")))))

# ---------- 5. la sortie est-elle CONSOMMABLE ? ----------
# C'est le seul test qui compte vraiment : le dashboard doit savoir la lire.
attendues <- c("ANNEE","MOIS","COMPTE","LIBELLE","VALEUR","TYPE","AGREGE",
               "SECTEUR","CATEGORIE","SECTION")
ok("toutes les colonnes du contrat sont la", setequal(names(cp), attendues))
ok("TYPE et AGREGE valent ce que classe_comptes() filtre",
   all(cp$TYPE == "compte") && all(cp$AGREGE))

cc <- classe_comptes(cp)
ok("classe_comptes() ne perd aucune ligne", nrow(cc) == nrow(cp))
ok("623700 est bien une remuneration", cc$POSTE[cc$COMPTE=="623700"] == "Rémunérations")
ok("700200 est bien du chiffre d'affaires",
   cc$POSTE[cc$COMPTE=="700200"] == "Chiffre d'affaires")
ok("aucun compte non classe", !any(cc$POSTE == "Non classé"))

pe <- postes_exploitation(cp)
ok("postes_exploitation() rend une periode", nrow(pe) == 1)
ok("le CA y est (6 + 12 + 21 %)", abs(pe$CA - 2905.50) < 0.01)
ok("les remunerations y sont", abs(pe$REMUNERATION - 800) < 0.01)
ok("les matieres y sont (600200 + 604100 + 604000)", abs(pe$MATIERES - 600) < 0.01)
ok("la marge se calcule", abs(pe$MARGE - (2905.50 - 800 - 600)) < 0.01)

# Le filtre exact de DB_HEURES_COMPTA (import.R).
ok("le filtre de DB_HEURES_COMPTA trouve les remunerations",
   { r <- cp |> filter(TYPE == "compte", CATEGORIE == "REMUNERATION")
     nrow(r) == 1 && r$VALEUR == 800 })
# Le filtre exact de DB_COUTS_MATIERE (server.R).
ok("le filtre de DB_COUTS_MATIERE trouve les achats, ventiles",
   { m <- cp |> filter(TYPE == "compte", AGREGE,
                       CATEGORIE %in% c("ACHATS","VARIATION_STOCK"),
                       SECTION == "Coût des ventes et prestations")
     nrow(m) == 3 &&
     m$SECTEUR[m$COMPTE == "600200"] == "Transformation alimentaire" &&
     m$SECTEUR[m$COMPTE == "604100"] == "Service" &&
     m$SECTEUR[m$COMPTE == "604000"] == SECTEUR_NON_VENTILE })
ok("les ventes n'entrent pas dans le filtre matiere",
   !any(cp$CATEGORIE[substr(cp$COMPTE,1,1) == "7"] %in% c("ACHATS","VARIATION_STOCK")))

# ---------- 6. les controles ----------
ct <- controle_ecritures(e, cp)
ok("le controle rend une table, jamais une erreur", is.data.frame(ct) && nrow(ct) >= 3)
# L'extrait est tronque : deux pieces n'ont pas leur contrepartie. Le controle
# DOIT le voir — c'est exactement ce qu'il est la pour attraper.
ok("un extrait tronque est detecte",
   ct$VERDICT[ct$CONTROLE == "Pièces équilibrées"] == "alerte")
ok("et il dit combien", grepl("^2 pièce", ct$DETAIL[ct$CONTROLE == "Pièces équilibrées"]))
ok("les comptes 6 et 7 sont tous classes",
   ct$VERDICT[ct$CONTROLE == "Comptes 6 et 7 classés"] == "ok")
# Un compte 6 hors plan doit lever une alerte nommee.
hs <- e |> slice(1) |> mutate(COMPTE = "689000", CLASSE = "6")
ok("un compte 6 inconnu du plan est nomme",
   { c2 <- controle_ecritures(bind_rows(e, hs))
     l <- c2[c2$CONTROLE == "Comptes 6 et 7 classés", ]
     l$VERDICT == "alerte" && grepl("689000", l$DETAIL) })
ok("ce que pese Non ventile est chiffre",
   { l <- ct[ct$CONTROLE == "Matières non ventilées", ]
     l$VERDICT == "alerte" && l$VALEUR == 50 })
# La reprise ne doit toucher que le bilan.
ok("une reprise propre passe",
   controle_ecritures(e, cp, reprise = "2026-06-30")$VERDICT[
     controle_ecritures(e, cp, reprise = "2026-06-30")$CONTROLE == "Reprise sans résultat"] == "ok")
ok("une classe 6 a la date de reprise est signalee",
   { sale <- e |> slice(1) |> mutate(DATE = as.Date("2026-06-30"), COMPTE="623700", CLASSE="6")
     c4 <- controle_ecritures(bind_rows(e, sale), cp, reprise = "2026-06-30")
     c4$VERDICT[c4$CONTROLE == "Reprise sans résultat"] == "alerte" })
ok("aucune ecriture -> alerte, pas une erreur",
   controle_ecritures(NULL)$VERDICT == "alerte")

# ---------- 7. la couture avec l'ancienne compta ----------
anc <- tibble(ANNEE = 2026L, MOIS = 1:6, COMPTE = "700000", LIBELLE = "Ventes",
              VALEUR = 1000, TYPE = "compte", AGREGE = TRUE,
              SECTEUR = NA_character_, CATEGORIE = NA_character_, SECTION = "x")
odo <- tibble(ANNEE = 2026L, MOIS = 7:9, COMPTE = "700200", LIBELLE = "Ventes 12%",
              VALEUR = 2000, TYPE = "compte", AGREGE = TRUE,
              SECTEUR = NA_character_, CATEGORIE = NA_character_, SECTION = "x")
co <- coud_compta(anc, odo)
ok("la couture rend neuf mois", nrow(co) == 9)
ok("l'ancien tient jusqu'en juin", all(co$COMPTE[co$MOIS <= 6] == "700000"))
ok("Odoo prend la suite en juillet", all(co$COMPTE[co$MOIS >= 7] == "700200"))
ok("aucun mois en double", !any(duplicated(paste(co$ANNEE, co$MOIS))))
# Un recouvrement doit etre VU, pas absorbe en silence.
ok("un recouvrement est detecte",
   controle_couture(anc, odo |> add_row(anc[6,]))$VERDICT[1] == "alerte")
ok("un trou est detecte",
   controle_couture(anc |> filter(MOIS != 3), odo)$VERDICT[2] == "alerte")
ok("un Odoo qui deborde avant la bascule est detecte",
   controle_couture(anc, odo |> add_row(anc[5,] |> mutate(COMPTE="700200")))$VERDICT[3] == "alerte")
ok("une couture saine ne dit rien", all(controle_couture(anc, odo)$VERDICT == "ok"))
ok("sans ancien, Odoo seul", nrow(coud_compta(NULL, odo)) == 3)
ok("sans rien, NULL", is.null(coud_compta(NULL, NULL)))
