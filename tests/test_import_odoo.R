# Le branchement d'Odoo dans import.R, sur le VRAI bloc du fichier.
#
# Le code est extrait d'import.R plutot que recopie : une copie diverge, et
# c'est justement le branchement — pas les fonctions — qui doit etre eprouve.
suppressMessages({library(dplyr); library(tibble); library(stringr)
                  library(lubridate); library(tidyr); library(readr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")

COULEURS_SECTEURS <- c("Service"="#2980b9", "Transformation alimentaire"="#5B7B5A",
                       "Brasserie"="#d98236", "Support"="#8d7b68")
source("R/plan_comptable.R", encoding="UTF-8", local = TRUE)
source("R/sources_travail.R", encoding="UTF-8", local = TRUE)
source("R/planning.R",        encoding="UTF-8", local = TRUE)
source("R/odoo.R",            encoding="UTF-8", local = TRUE)
source("R/odoo_api.R",        encoding="UTF-8", local = TRUE)

# On parse la SUITE du fichier et on garde tout jusqu'au if/else inclus : les
# affectations qui le preparent, puis lui. Decouper au prochain titre « #### »
# attrapait aussi l'import des reservations, qui suit sans titre a lui ; un
# decoupage a `[1:2]` se perimait des qu'une variable s'ajoutait avant le if.
src <- readLines("import.R", encoding = "UTF-8", warn = FALSE)
i <- grep("^#### Compta Odoo ####", src)
stopifnot(length(i) == 1)
TOUT <- parse(text = paste(src[(i + 1):length(src)], collapse = "\n"),
              encoding = "UTF-8")
fin <- which(vapply(as.list(TOUT), function(e)
  is.call(e) && identical(as.character(e[[1]]), "if"), logical(1)))[1]
stopifnot(!is.na(fin))
BLOC <- TOUT[1:fin]
cat("Bloc Odoo extrait d'import.R :", length(BLOC), "expressions\n")

# L'ancienne compta : janvier a juin 2026, comme le .rds du comptable.
ANCIEN <- tibble(ANNEE = 2026L, MOIS = 1:6, COMPTE = "700000", LIBELLE = "Ventes",
                 VALEUR = 1000, TYPE = "compte", AGREGE = TRUE,
                 SECTEUR = NA_character_, CATEGORIE = NA_character_,
                 SECTION = "Produits d'exploitation")

# Un import simule : seules les fonctions Drive, l'API et cli sont des
# doublures. Les variables d'environnement sont posees explicitement a chaque
# appel — sinon le resultat dependrait de la machine qui lance les tests, et
# un ODOO_URL traine suffirait a faire passer le test pour la mauvaise raison.
joue <- function(id = "fake-id", fichier = "tests/fixtures/ecritures_odoo.tsv",
                 api = NULL) {
  avant <- Sys.getenv(ODOO_ENV)
  on.exit(do.call(Sys.setenv, as.list(avant)), add = TRUE)
  do.call(Sys.setenv, as.list(setNames(
    if (is.null(api)) rep("", length(ODOO_ENV))
    else c("https://faux.odoo.com", "mazette", "x@y.be", "cle"), ODOO_ENV)))

  # parent = l'appel courant, pour que les doublures voient `id` et `fichier`.
  env <- new.env(parent = environment())
  local({
    get_path <- function(k) if (identical(k, "ID_ODOO_ECRITURES")) id else character(0)
    drive_get <- function(...) list(id = id)
    drive_download <- function(...) list(local_path = fichier)
    lit_ecritures_odoo <- function(...) api
    DB_COMPTA <- ANCIEN
    for (e in as.list(BLOC)) eval(e)
    env$DB_COMPTA <- DB_COMPTA
    env$CONTROLE_ODOO <- if (exists("CONTROLE_ODOO", inherits = FALSE)) CONTROLE_ODOO else NULL
    env$COMPTA_ODOO <- if (exists("COMPTA_ODOO", inherits = FALSE)) COMPTA_ODOO else NULL
  }, envir = env)
  env
}
# ---------- 1. la cle est renseignee ----------
r <- suppressMessages(joue())
ok("la compta Odoo est produite", !is.null(r$COMPTA_ODOO) && nrow(r$COMPTA_ODOO) > 0)
ok("DB_COMPTA est cousue", nrow(r$DB_COMPTA) > nrow(ANCIEN))
ok("l'ancien tient jusqu'en juin",
   all(r$DB_COMPTA$COMPTE[r$DB_COMPTA$MOIS <= 6] == "700000"))
ok("Odoo prend la suite a partir de juillet",
   all(r$DB_COMPTA$MOIS[r$DB_COMPTA$COMPTE != "700000"] >= 7))
ok("aucun mois en double",
   !any(duplicated(paste(r$DB_COMPTA$ANNEE, r$DB_COMPTA$MOIS, r$DB_COMPTA$COMPTE))))
ok("le controle voyage avec les donnees",
   is.data.frame(r$CONTROLE_ODOO) && nrow(r$CONTROLE_ODOO) >= 4)
# Sans cette ligne, le controle existe a l'import et disparait du cache.
ok("il est dans la liste des tables sauvegardees",
   { e <- new.env(); source("R/connect.R", local = e, encoding = "UTF-8")
     "CONTROLE_ODOO" %in% e$TABLES_OPTIONNELLES })

# La sortie reste consommable par le dashboard apres couture.
ok("classe_comptes() lit la table cousue",
   nrow(classe_comptes(r$DB_COMPTA)) == nrow(r$DB_COMPTA))

# ---------- 2. la cle est absente ----------
# C'est le cas de TOUT LE MONDE tant que Thomas n'a pas rempli les PATHS :
# l'import doit se comporter exactement comme avant.
r0 <- suppressMessages(joue(id = character(0)))
ok("sans la cle, DB_COMPTA est inchangee", identical(r0$DB_COMPTA, ANCIEN))
ok("sans la cle, aucune compta Odoo", is.null(r0$COMPTA_ODOO))

# ---------- 3. la lecture echoue ----------
# Un export au mauvais format ne doit pas emporter tout l'import.
r1 <- suppressMessages(joue(fichier = "tests/fixtures/pas_un_export.csv"))
ok("un fichier introuvable n'arrete pas l'import", is.null(r1$COMPTA_ODOO))
ok("et DB_COMPTA reste celle du .rds", identical(r1$DB_COMPTA, ANCIEN))

mauvais <- tempfile(fileext = ".csv")
writeLines(c("Date;Compte;Montant", "2026-09-01;700200;100"), mauvais)
r2 <- suppressMessages(joue(fichier = mauvais))
ok("un export aux mauvaises colonnes n'arrete pas l'import", is.null(r2$COMPTA_ODOO))
ok("et DB_COMPTA reste celle du .rds", identical(r2$DB_COMPTA, ANCIEN))

# ---------- 4. l'API ----------
# Le tableau que rend lit_ecritures_odoo() porte les MEMES colonnes que le
# fichier : c'est tout l'interet, et c'est ce qui se verifie ici — le bloc
# d'import ne contient aucune branche « si API » apres la lecture.
LIGNES_API <- tibble(
  Date = c("2026-07-15", "2026-07-15", "2026-08-10"),
  `Numéro` = c("MZVEN/2026/07/0001", "MZACH/2026/07/0002", "MZVEN/2026/08/0001"),
  Compte = c("700100 Ventes boissons", "600100 Achats de boissons",
             "700100 Ventes boissons"),
  Partenaire = NA_character_,
  `Libellé` = "x",
  `Débit`  = c(0, 300, 0),
  `Crédit` = c(2000, 0, 2500))

ra <- suppressMessages(joue(api = LIGNES_API))
ok("avec les variables ODOO_*, la compta vient de l'API",
   !is.null(ra$COMPTA_ODOO) && nrow(ra$COMPTA_ODOO) == 3)
ok("et elle se coud a l'ancienne comme le fichier",
   nrow(ra$DB_COMPTA) == nrow(ANCIEN) + 3)
ok("la convention de signe survit au trajet",
   ra$COMPTA_ODOO$VALEUR[ra$COMPTA_ODOO$COMPTE == "700100" &
                         ra$COMPTA_ODOO$MOIS == 7] == 2000)
ok("les controles sont produits comme sur le fichier",
   is.data.frame(ra$CONTROLE_ODOO) && nrow(ra$CONTROLE_ODOO) >= 4)

# L'API passe DEVANT le fichier : si les deux sont configures, on lit celle
# qui est a jour, pas l'export depose il y a trois semaines.
ok("l'API l'emporte sur le fichier quand les deux sont la",
   { r <- suppressMessages(joue(api = LIGNES_API,
                                fichier = "tests/fixtures/ecritures_odoo.tsv"))
     nrow(r$COMPTA_ODOO) == 3 })
# Et une API en panne ne doit pas plus casser l'import qu'un fichier illisible.
ok("une API qui tombe laisse DB_COMPTA intacte",
   { r <- suppressMessages(joue(api = "pas un tableau"))
     is.null(r$COMPTA_ODOO) && identical(r$DB_COMPTA, ANCIEN) })
