# Faire tourner l'import ailleurs qu'ici.
#
# ---------------------------------------------------------------------------
# Le problème
# ---------------------------------------------------------------------------
# L'import lit une dizaine de classeurs Drive et interroge Odoo : quelques
# minutes, et une pointe de mémoire. Le lancer DANS le processus Shiny, c'est
# geler le dashboard pour tout le monde pendant ce temps — R n'a qu'un fil
# d'exécution, et l'hébergeur ne promet qu'un gigaoctet.
#
# Le travail va donc ailleurs : un workflow GitHub Actions
# (.github/workflows/import.yml) fait exactement ce que faisait l'import au
# démarrage — lire les sources, écrire le .RData, le déposer sur Drive — et le
# dashboard n'a plus qu'à reprendre le fichier (cf. recharge_donnees()).
#
# Ce fichier est le fil qui relie les deux : déclencher le workflow, et savoir
# où il en est.
#
# ---------------------------------------------------------------------------
# Ce qui n'est pas ici
# ---------------------------------------------------------------------------
# Le jeton. Le dépôt est public : GH_TOKEN_IMPORT vit dans les variables
# d'environnement de l'hébergeur, comme LINK_JSON et les identifiants Odoo.
# Sans lui, config_import_distant() rend NULL et le dashboard se rabat sur
# l'import local — ce qui est le bon comportement en développement, où bloquer
# son propre R pendant trois minutes ne dérange personne.

DEPOT_DEFAUT    <- "tdelc/Mazette-Public"
REF_DEFAUT      <- "main"
WORKFLOW_IMPORT <- "import.yml"

IMPORT_DISTANT_ENV <- c(token = "GH_TOKEN_IMPORT",  # un PAT « fine-grained »,
                                                    # portée Actions: write
                        depot = "GH_DEPOT",         # facultatif
                        ref   = "GH_REF")           # facultatif

#' Les identifiants du dépôt, ou NULL si l'import distant n'est pas configuré.
#'
#' Seul le jeton est obligatoire : le dépôt et la branche ont des valeurs par
#' défaut justes, et les écrire deux fois serait deux fois à les corriger.
config_import_distant <- function(env = IMPORT_DISTANT_ENV) {
  v <- vapply(env, Sys.getenv, "")
  if (!nzchar(v[["token"]])) return(NULL)
  list(token = v[["token"]],
       depot = if (nzchar(v[["depot"]])) v[["depot"]] else DEPOT_DEFAUT,
       ref   = if (nzchar(v[["ref"]]))   v[["ref"]]   else REF_DEFAUT)
}

#' L'appel HTTP, isolé pour que tout le reste se teste sans réseau.
#'
#' Rend toujours la même forme — un code et un contenu — quel que soit le
#' verbe. La suite de tests passe une doublure à sa place.
appel_github <- function(methode, url, conf, corps = NULL) {
  entetes <- httr::add_headers(
    Authorization          = paste("Bearer", conf$token),
    Accept                 = "application/vnd.github+json",
    `X-GitHub-Api-Version` = "2022-11-28")
  r <- if (identical(methode, "POST"))
         httr::POST(url, entetes, body = corps, encode = "json")
       else
         httr::GET(url, entetes)
  list(code    = httr::status_code(r),
       contenu = tryCatch(httr::content(r, as = "parsed",
                                        type = "application/json"),
                          error = function(e) NULL))
}

url_workflow <- function(conf, workflow = WORKFLOW_IMPORT, suffixe)
  paste0("https://api.github.com/repos/", conf$depot,
         "/actions/workflows/", workflow, "/", suffixe)

#' Demander un import complet au serveur d'intégration.
#'
#' @return `list(ok, message)`. Le message est destiné à l'écran : il nomme ce
#'   qui manque plutôt que le code HTTP, parce que « 404 » n'a jamais aidé
#'   personne à comprendre qu'un fichier de workflow n'était pas sur la branche.
declenche_import_distant <- function(conf = config_import_distant(),
                                     workflow = WORKFLOW_IMPORT,
                                     appel = appel_github) {
  if (is.null(conf))
    return(list(ok = FALSE, message = "Import distant non configuré (GH_TOKEN_IMPORT absent)."))

  r <- appel("POST", url_workflow(conf, workflow, "dispatches"), conf,
             corps = list(ref = conf$ref))

  if (identical(r$code, 204L) || identical(r$code, 204))
    return(list(ok = TRUE,
                message = paste0("Import demandé sur ", conf$depot, " (", conf$ref, ").")))

  list(ok = FALSE, message = switch(
    as.character(r$code),
    "401" = "Jeton refusé : GH_TOKEN_IMPORT est invalide ou expiré.",
    "403" = "Jeton sans le droit « Actions: write » sur le dépôt.",
    "404" = paste0("Workflow ", workflow, " introuvable sur la branche ",
                   conf$ref, " — il doit y être poussé pour être déclenchable."),
    "422" = paste0("Branche ", conf$ref, " inconnue du dépôt."),
    paste0("GitHub a répondu ", r$code, ".")))
}

#' Où en est le dernier import lancé là-bas ?
#'
#' @return `list(ok, statut, conclusion, url, fini, reussi, message)`.
#'   `statut` est celui de GitHub (queued / in_progress / completed).
etat_import_distant <- function(conf = config_import_distant(),
                                workflow = WORKFLOW_IMPORT,
                                appel = appel_github) {
  if (is.null(conf))
    return(list(ok = FALSE, message = "Import distant non configuré."))

  r <- appel("GET", paste0(url_workflow(conf, workflow, "runs"), "?per_page=1"),
             conf)
  if (!(identical(r$code, 200L) || identical(r$code, 200)))
    return(list(ok = FALSE, message = paste0("GitHub a répondu ", r$code, ".")))

  runs <- r$contenu$workflow_runs
  if (is.null(runs) || !length(runs))
    return(list(ok = TRUE, statut = NA_character_, fini = FALSE, reussi = FALSE,
                message = "Aucune exécution connue."))

  run <- runs[[1]]
  fini <- identical(run$status, "completed")
  list(ok         = TRUE,
       statut     = ou_na(run$status),
       conclusion = ou_na(run$conclusion),
       url        = ou_na(run$html_url),
       debut      = ou_na(run$run_started_at),
       fini       = fini,
       reussi     = fini && identical(run$conclusion, "success"),
       message    = texte_etat_import(run))
}

# `x %||% y` est dans base depuis R 4.4 seulement, et le dépôt doit tourner
# sur un peu plus ancien : on se passe de l'opérateur.
ou_na <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)

#' L'état d'une exécution, en une ligne lisible.
texte_etat_import <- function(run) {
  if (is.null(run$status)) return("état inconnu")
  if (!identical(run$status, "completed"))
    return(switch(run$status,
                  queued      = "en attente d'un exécutant…",
                  in_progress = "import en cours…",
                  paste0("état : ", run$status)))
  switch(as.character(run$conclusion),
         success   = "import terminé.",
         failure   = "l'import a échoué — voir le journal sur GitHub.",
         cancelled = "import annulé.",
         timed_out = "l'import a dépassé le temps imparti.",
         paste0("import terminé : ", run$conclusion))
}
