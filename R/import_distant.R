# Lancer l'import ailleurs que dans le processus Shiny, qui n'a qu'un fil
# d'exécution : le workflow GitHub Actions (.github/workflows/import.yml)
# relit les sources et dépose le cache sur Drive, que le dashboard reprend
# ensuite (cf. recharge_donnees()).
#
# Le jeton GH_TOKEN_IMPORT vit dans les variables de l'hébergeur. Sans lui,
# config_import_distant() rend NULL et l'import se fait sur place.

DEPOT_DEFAUT    <- "tdelc/Mazette-Public"
REF_DEFAUT      <- "main"
WORKFLOW_IMPORT <- "import.yml"

IMPORT_DISTANT_ENV <- c(token = "GH_TOKEN_IMPORT",  # un PAT « fine-grained »,
                                                    # portée Actions: write
                        depot = "GH_DEPOT",         # facultatif
                        ref   = "GH_REF")           # facultatif

#' Les identifiants du dépôt, ou NULL si l'import distant n'est pas configuré.
#' Seul le jeton est obligatoire.
config_import_distant <- function(env = IMPORT_DISTANT_ENV) {
  v <- vapply(env, Sys.getenv, "")
  if (!nzchar(v[["token"]])) return(NULL)
  list(token = v[["token"]],
       depot = if (nzchar(v[["depot"]])) v[["depot"]] else DEPOT_DEFAUT,
       ref   = if (nzchar(v[["ref"]]))   v[["ref"]]   else REF_DEFAUT)
}

#' L'appel HTTP, isolé pour que le reste se teste sans réseau.
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

#' Demander un import complet à GitHub Actions.
#'
#' @return `list(ok, message)`, le message étant écrit pour l'écran.
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

# %||% n'est dans base que depuis R 4.4.
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
