# Quel cache le dashboard charge, et quand il refait l'import.
#
# Tout ce fichier tient à une phrase : « de base, on prend le dernier rds,
# même si ce n'est pas la bonne date ». Avant, server.R réclamait le cache de
# la VEILLE, nommément, et refaisait tout l'import s'il ne le trouvait pas —
# ce qui faisait attendre plusieurs minutes la première personne connectée du
# matin, et elle seule.
#
# Le risque du nouveau comportement est l'inverse : servir en silence des
# chiffres d'avant-hier. C'est pourquoi la fraîcheur est affichée
# (test_fraicheur.R) et c'est pourquoi les cas limites sont ici.
suppressMessages({library(tibble)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
source("R/connect.R", encoding = "UTF-8", local = TRUE)

#### Lire la date dans un nom de fichier ####

ok("la date est lue dans le nom",
   identical(date_du_rds("R_new_env_2026-11-07.RData"), as.Date("2026-11-07")))
ok("un nom étranger ne porte pas de date",
   is.na(date_du_rds("IMPORT 2025.RData")))
ok("un nom presque juste n'en porte pas non plus",
   is.na(date_du_rds("R_new_env_2026-11.RData")))
ok("nom_rds et date_du_rds font l'aller-retour",
   identical(date_du_rds(nom_rds(as.Date("2026-03-01"))), as.Date("2026-03-01")))

#### Choisir le plus récent ####

noms <- c("R_new_env_2026-10-02.RData", "README.md",
          "R_new_env_2026-11-07.RData", "R_new_env_2026-05-30.RData",
          "IMPORT 2023-2024.RData")
d <- dernier_rds(noms)
ok("le plus récent gagne", identical(d$nom, "R_new_env_2026-11-07.RData"))
ok("et il connaît sa date", identical(d$date, as.Date("2026-11-07")))
# Le tri doit porter sur la DATE lue, pas sur le nom : un fichier étranger ne
# doit pas pouvoir passer devant parce qu'il commence par un Z.
ok("un fichier étranger ne passe pas devant",
   identical(dernier_rds(c("zzz.RData", "R_new_env_2026-01-01.RData"))$nom,
             "R_new_env_2026-01-01.RData"))
ok("aucun cache : NULL", is.null(dernier_rds(c("README.md", "test.txt"))))
ok("dossier vide : NULL", is.null(dernier_rds(character(0))))

#### Local ou Drive ####

J <- function(x) as.Date(x)
ok("égalité : le local, on s'épargne le téléchargement",
   identical(source_du_cache(J("2026-11-07"), J("2026-11-07")), "local"))
ok("Drive plus frais : Drive",
   identical(source_du_cache(J("2026-11-05"), J("2026-11-07")), "drive"))
ok("local plus frais : local",
   identical(source_du_cache(J("2026-11-07"), J("2026-11-05")), "local"))
ok("Drive injoignable : le local, quelle que soit sa date",
   identical(source_du_cache(J("2026-01-01"), as.Date(NA)), "local"))
ok("rien en local : Drive",
   identical(source_du_cache(as.Date(NA), J("2026-11-07")), "drive"))
ok("rien nulle part : l'import",
   identical(source_du_cache(as.Date(NA), as.Date(NA)), "import"))

#### Le cache le plus frais de Drive ####

liste_drive <- function()
  tibble(name = c("R_new_env_2026-11-06.RData", "autre.xlsx",
                  "R_new_env_2026-11-07.RData"),
         id   = c("id-06", "id-autre", "id-07"))
d <- dernier_rds_drive()
ok("Drive : le plus récent", identical(d$nom, "R_new_env_2026-11-07.RData"))
ok("Drive : avec son identifiant", identical(d$id, "id-07"))

liste_drive <- function() tibble(name = character(0), id = character(0))
ok("dossier Drive vide : NULL", is.null(dernier_rds_drive()))

#### Le chemin complet, avec de vrais fichiers ####

# Les doublures : tout ce qui touche au réseau, et la reconstruction d'après
# chargement (elle a ses propres tests ailleurs).
APPELS <- character(0)
finalise_donnees <- function(envir) { APPELS <<- c(APPELS, "finalise"); TRUE }
refait_import <- function(prefix = PREFIXE_RDS, dossier = "outputs",
                          envir = parent.frame()) {
  APPELS <<- c(APPELS, "import")
  list(fichier = "import", date = as.Date("2026-11-07"), source = "import")
}

dossier <- file.path(tempdir(), "caches")
dir.create(dossier, showWarnings = FALSE)
pose <- function(nom, valeur) {
  TEMOIN <- valeur
  save(TEMOIN, file = file.path(dossier, nom))
}
unlink(list.files(dossier, full.names = TRUE))
pose("R_new_env_2026-10-02.RData", "vieux")
pose("R_new_env_2026-11-04.RData", "frais")
file.create(file.path(dossier, "README.md"))

# 1. Drive ne propose rien : le dernier local sert, même vieux de trois jours.
liste_drive <- function() tibble(name = character(0), id = character(0))
APPELS <- character(0)
e <- new.env()
res <- connexion_ou_creation(dossier = dossier, envir = e)
ok("un cache périmé est chargé plutôt que refaire l'import",
   identical(res$source, "local") && identical(res$date, as.Date("2026-11-04")))
ok("et c'est bien le plus frais des locaux", identical(get("TEMOIN", e), "frais"))
ok("aucun import n'a été lancé", !("import" %in% APPELS))
ok("la reconstruction d'après chargement a eu lieu", "finalise" %in% APPELS)

# 2. Drive est strictement plus frais : on télécharge.
liste_drive <- function()
  tibble(name = "R_new_env_2026-11-07.RData", id = "id-07")
telecharge_rds <- function(info, dossier = "outputs") {
  APPELS <<- c(APPELS, "telecharge")
  pose(info$nom, "drive")
  file.path(dossier, info$nom)
}
APPELS <- character(0)
e <- new.env()
res <- connexion_ou_creation(dossier = dossier, envir = e)
ok("Drive plus frais : il est téléchargé",
   identical(res$source, "drive") && "telecharge" %in% APPELS)
ok("et c'est son contenu qui arrive", identical(get("TEMOIN", e), "drive"))

# 3. Drive injoignable : le local, et le dashboard s'ouvre quand même.
liste_drive <- function() stop("pas de réseau")
APPELS <- character(0)
e <- new.env()
res <- suppressMessages(connexion_ou_creation(dossier = dossier, envir = e))
ok("Drive injoignable : on charge le local sans échouer",
   identical(res$source, "local"))
ok("et toujours aucun import", !("import" %in% APPELS))

# 4. Rien nulle part : là, et là seulement, l'import.
vide <- file.path(tempdir(), "vide"); dir.create(vide, showWarnings = FALSE)
unlink(list.files(vide, full.names = TRUE))
liste_drive <- function() tibble(name = character(0), id = character(0))
APPELS <- character(0)
res <- suppressMessages(connexion_ou_creation(dossier = vide, envir = new.env()))
ok("aucun cache : import complet", identical(APPELS, "import"))

# 5. Un cache illisible — tronqué, écrit à moitié — ne bloque pas le dashboard.
writeLines("ceci n'est pas un RData", file.path(vide, "R_new_env_2026-11-07.RData"))
APPELS <- character(0)
res <- suppressMessages(connexion_ou_creation(dossier = vide, envir = new.env()))
ok("un cache illisible renvoie à l'import", identical(APPELS, "import"))

# 6. force_import : on ne regarde NI le local NI Drive.
liste_drive <- function() stop("Drive ne doit pas être interrogé")
APPELS <- character(0)
res <- connexion_ou_creation(dossier = dossier, force_import = TRUE,
                             envir = new.env())
ok("force_import va droit à l'import", identical(APPELS, "import"))
