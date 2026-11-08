# Déclencher l'import ailleurs, et savoir où il en est.
#
# Le dashboard tourne sur un processus R unique : y lancer l'import, c'est
# geler l'écran de tout le monde pendant quelques minutes. Le travail part
# donc sur un exécutant GitHub Actions, et ce fichier éprouve le fil qui les
# relie — sans réseau, en doublant l'appel HTTP.
#
# Ce qui est testé ici n'est pas le protocole de GitHub (on ne peut pas le
# vérifier d'ici) mais ce qu'on en FAIT : l'URL construite, le corps envoyé,
# et surtout la traduction des échecs. « 404 » n'a jamais aidé personne à
# comprendre qu'un fichier de workflow n'était pas sur la bonne branche.
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
source("R/import_distant.R", encoding = "UTF-8", local = TRUE)

#### La configuration vient de l'environnement ####

sans <- c(token = "ZZ_ABSENT_1", depot = "ZZ_ABSENT_2", ref = "ZZ_ABSENT_3")
ok("sans jeton, l'import distant n'est pas configuré",
   is.null(config_import_distant(sans)))

Sys.setenv(ZZ_TOKEN = "jeton-x")
conf <- config_import_distant(c(token = "ZZ_TOKEN", depot = "ZZ_ABSENT_2",
                                ref = "ZZ_ABSENT_3"))
ok("le jeton suffit", identical(conf$token, "jeton-x"))
ok("le dépôt a une valeur par défaut", identical(conf$depot, DEPOT_DEFAUT))
ok("la branche aussi", identical(conf$ref, REF_DEFAUT))

Sys.setenv(ZZ_DEPOT = "qui/autre", ZZ_REF = "dev")
conf2 <- config_import_distant(c(token = "ZZ_TOKEN", depot = "ZZ_DEPOT",
                                 ref = "ZZ_REF"))
ok("et on peut les surcharger",
   identical(conf2$depot, "qui/autre") && identical(conf2$ref, "dev"))
Sys.unsetenv(c("ZZ_TOKEN", "ZZ_DEPOT", "ZZ_REF"))

#### Déclencher ####

VU <- list()
doublure <- function(code, contenu = NULL)
  function(methode, url, conf, corps = NULL) {
    VU <<- list(methode = methode, url = url, corps = corps, conf = conf)
    list(code = code, contenu = contenu)
  }

r <- declenche_import_distant(conf, appel = doublure(204))
ok("204 : la demande est passée", isTRUE(r$ok))
ok("c'est un POST", identical(VU$methode, "POST"))
ok("sur le point de dispatch du workflow",
   identical(VU$url, paste0("https://api.github.com/repos/", DEPOT_DEFAUT,
                            "/actions/workflows/import.yml/dispatches")))
ok("et il porte la branche", identical(VU$corps$ref, REF_DEFAUT))
ok("le message nomme le dépôt et la branche",
   grepl(DEPOT_DEFAUT, r$message, fixed = TRUE))

# Les quatre refus, et ce qu'il faut aller corriger pour chacun.
ok("401 : le jeton",
   grepl("invalide ou expiré",
         declenche_import_distant(conf, appel = doublure(401))$message))
ok("403 : le droit qui manque au jeton",
   grepl("Actions: write",
         declenche_import_distant(conf, appel = doublure(403))$message,
         fixed = TRUE))
ok("404 : le workflow pas encore poussé sur la branche",
   grepl("introuvable sur la branche",
         declenche_import_distant(conf, appel = doublure(404))$message))
ok("422 : la branche",
   grepl("inconnue du dépôt",
         declenche_import_distant(conf, appel = doublure(422))$message))
ok("un code inattendu ne passe pas pour un succès",
   isFALSE(declenche_import_distant(conf, appel = doublure(500))$ok))
ok("sans configuration, on le dit plutôt que d'appeler",
   grepl("GH_TOKEN_IMPORT",
         declenche_import_distant(NULL)$message, fixed = TRUE))

#### Suivre ####

run <- function(statut, conclusion = NULL)
  list(workflow_runs = list(list(status = statut, conclusion = conclusion,
                                 html_url = "https://exemple/run/1",
                                 run_started_at = "2026-11-08T04:43:00Z")))

e <- etat_import_distant(conf, appel = doublure(200, run("queued")))
ok("en attente : pas fini", isTRUE(e$ok) && isFALSE(e$fini))
ok("et c'est dit en français", grepl("attente", e$message))
ok("l'URL de suivi remonte", identical(e$url, "https://exemple/run/1"))

e <- etat_import_distant(conf, appel = doublure(200, run("in_progress")))
ok("en cours : pas fini non plus", isFALSE(e$fini))

e <- etat_import_distant(conf, appel = doublure(200, run("completed", "success")))
ok("terminé et réussi", isTRUE(e$fini) && isTRUE(e$reussi))

e <- etat_import_distant(conf, appel = doublure(200, run("completed", "failure")))
ok("terminé mais échoué : fini, pas réussi",
   isTRUE(e$fini) && isFALSE(e$reussi))
ok("et le message renvoie au journal", grepl("journal sur GitHub", e$message))

# Le piège à éviter : prendre un échec pour un succès et recharger un fichier
# qui n'a pas été écrit.
ok("annulé n'est pas réussi",
   isFALSE(etat_import_distant(conf, appel = doublure(200, run("completed", "cancelled")))$reussi))
ok("dépassement de temps non plus",
   isFALSE(etat_import_distant(conf, appel = doublure(200, run("completed", "timed_out")))$reussi))

e <- etat_import_distant(conf, appel = doublure(200, list(workflow_runs = list())))
ok("aucune exécution connue", isTRUE(e$ok) && isFALSE(e$fini))

e <- etat_import_distant(conf, appel = doublure(403))
ok("suivi refusé : ok vaut FALSE", isFALSE(e$ok))

#### Le battement ne doit pas se mordre la queue ####

# Un observateur qui LIT la valeur qu'il ÉCRIT se réinvalide lui-même : le
# battement de quinze secondes deviendrait une boucle serrée sur l'API de
# GitHub, et rien ne le signalerait — ni une erreur, ni un test de calcul.
# Seule la forme du code le dit, donc c'est la forme qu'on relit.
src <- readLines("server.R", encoding = "UTF-8", warn = FALSE)
debut <- grep("req\\(EN_COURS\\(\\)\\)", src)
ok("le battement existe", length(debut) == 1)

if (length(debut) == 1) {
  # Du observe({ qui précède jusqu'au }) qui ferme.
  ouvre <- max(grep("^  observe\\(\\{", src[seq_len(debut)]))
  ferme <- ouvre + min(which(grepl("^  \\}\\)", src[(ouvre + 1):length(src)])))
  bloc  <- src[ouvre:ferme]

  ok("il interroge bien GitHub",
     any(grepl("etat_import_distant", bloc, fixed = TRUE)))
  ok("il ne lit pas le réactif qu'il écrit",
     !any(grepl("SUIVI()", bloc, fixed = TRUE)))
  ok("il se donne une fin",
     any(grepl("invalidateLater", bloc, fixed = TRUE)) &&
     any(grepl("EN_COURS(FALSE)", src, fixed = TRUE)))
  # Recharger après un import qui a échoué servirait un fichier qui n'a pas
  # été écrit — ou, pire, l'ancien, en annonçant qu'il est neuf.
  ok("il ne recharge qu'après un succès",
     any(grepl("etat$reussi", bloc, fixed = TRUE)))
}
