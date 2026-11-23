# Odoo par l'API JSON-RPC : la même lecture qu'un export, par un autre tuyau.
#
# lit_ecritures_odoo() rend exactement ce que rend lit_ecritures_fichier() :
# un tableau brut aux colonnes de l'export (ODOO_COLONNES). Normalisation,
# agrégation, contrôles et couture sont donc communs aux deux sources.
#
# JSON-RPC plutôt que XML-RPC : httr et jsonlite suffisent. essai_odoo_api.R
# vérifie le protocole point par point contre l'instance.

#### Identifiants ####

# Les identifiants passent par l'environnement, jamais par le dépôt (public).
ODOO_ENV <- c(url   = "ODOO_URL",      # https://mazette.odoo.com
              base  = "ODOO_DB",       # le nom de la base
              login = "ODOO_LOGIN",    # l'adresse e-mail du compte
              cle   = "ODOO_CLE_API")  # Préférences -> Sécurité -> Clés API

#' Les identifiants, ou NULL s'ils ne sont pas tous là : tout ou rien.
identifiants_odoo <- function(env = ODOO_ENV) {
  v <- lit_env_odoo(env)
  if (any(!nzchar(v))) return(NULL)
  # Une barre finale en trop donnerait « https://x//jsonrpc » (404).
  v[["url"]] <- sub("/+$", "", v[["url"]])
  as.list(v)
}

#' Ce qui manque, pour le dire sans jamais afficher la clé.
manque_odoo <- function(env = ODOO_ENV) {
  names(env)[!nzchar(lit_env_odoo(env))]
}

# Les quatre valeurs, nettoyées de ce qu'un copier-coller y laisse dans
# l'interface d'un hébergeur : espaces, retour à la ligne, guillemets. Pour
# Odoo, « mazette » et « mazette\n » sont deux bases différentes.
lit_env_odoo <- function(env = ODOO_ENV) {
  v <- trimws(vapply(env, Sys.getenv, ""))
  v <- sub('^"(.*)"$', "\\1", v)
  v <- sub("^'(.*)'$", "\\1", v)
  trimws(v)
}

#### Le transport ####

# L'unique endroit qui parle au réseau ; les tests substituent leur `poste`.
odoo_poste <- function(url, corps_json) {
  rep <- httr::POST(paste0(url, "/jsonrpc"),
                    body = corps_json,
                    httr::content_type_json(),
                    httr::timeout(120))
  # Odoo répond 200 pour une erreur applicative, 404 pour une URL fausse.
  if (httr::status_code(rep) >= 400)
    stop("Odoo a répondu HTTP ", httr::status_code(rep),
         " sur ", url, "/jsonrpc")
  httr::content(rep, "text", encoding = "UTF-8")
}

# jsonlite rend `[]` pour une liste vide ; execute_kw exige `{}`.
OBJET_VIDE <- structure(list(), names = character(0))

#' Un appel JSON-RPC, et la lecture de sa réponse.
#'
#' auto_unbox = TRUE : sinon chaque chaîne partirait en tableau.
#' simplifyVector = FALSE : les champs relationnels arrivent en [id, "libellé"]
#' ou FALSE ; on les range à la main (champ_texte / champ_nombre).
odoo_appel <- function(url, service, methode, args, poste = odoo_poste) {
  corps <- list(jsonrpc = "2.0", method = "call",
                params = list(service = service, method = methode,
                              args = args))
  brut <- poste(url, jsonlite::toJSON(corps, auto_unbox = TRUE, null = "null"))
  res <- jsonlite::fromJSON(brut, simplifyVector = FALSE)

  if (!is.null(res$error)) {
    # Pas de %||% : ce fichier doit se tester sans shiny.
    premier <- function(...) {
      for (v in list(...)) if (!is.null(v) && nzchar(as.character(v)[1]))
        return(as.character(v)[1])
      ""
    }
    e <- res$error
    detail <- premier(e$data$message, e$message, "erreur sans message")
    nom    <- premier(e$data$name)
    stop("Odoo a refusé l'appel ", service, ".", methode, " : ", detail,
         if (nzchar(nom)) paste0(" (", nom, ")") else "")
  }
  res$result
}

#' La version du serveur, sans authentification : s'il échoue, c'est l'URL
#' ou le réseau, pas la clé.
odoo_version <- function(url, poste = odoo_poste) {
  odoo_appel(url, "common", "version", list(), poste)
}

#' L'uid, ou une erreur qui dit ce qui ne va pas.
#'
#' De mauvais identifiants rendent `false`, pas une erreur. Une base inconnue
#' d'un serveur multi-bases rend une erreur brute de psycopg2 (« connection to
#' server on socket ... failed ») : on la traduit en nommant la base.
odoo_connexion <- function(ids, poste = odoo_poste) {
  uid <- tryCatch(
    odoo_appel(ids$url, "common", "authenticate",
               list(ids$base, ids$login, ids$cle, OBJET_VIDE), poste),
    error = function(e) {
      m <- conditionMessage(e)
      if (grepl("psycopg2|PGSQL|OperationalError|does not exist", m))
        stop("Le serveur Odoo ne trouve pas la base « ", ids$base, " » (",
             nchar(ids$base), " caractères). Vérifie ODOO_DB sur la machine ",
             "qui lance l'import : même valeur exacte que sur le PC où ",
             "l'import marche, sans espace, guillemet ni retour à la ligne. ",
             "Réponse d'Odoo : ", m, call. = FALSE)
      stop(e)
    })
  if (is.null(uid) || isFALSE(uid) || !is.numeric(uid))
    stop("Odoo a refusé les identifiants (base « ", ids$base, " », compte « ",
         ids$login, " »). Vérifie le nom de la base et la clé API.")
  as.integer(uid)
}

#' Un appel de modèle : execute_kw, la porte d'entrée de tout le reste.
odoo_execute <- function(ids, uid, modele, methode, args = list(),
                         kwargs = OBJET_VIDE, poste = odoo_poste) {
  odoo_appel(ids$url, "object", "execute_kw",
             list(ids$base, uid, ids$cle, modele, methode, args, kwargs),
             poste)
}

#### Lire des lignes ####

# Champs demandés à account.move.line (à gauche) et libellés de l'export
# correspondants (à droite) : l'API rend le même tableau que le fichier.
CHAMPS_ODOO <- c(date       = "Date",
                 move_name  = "Numéro",
                 account_id = "Compte",
                 partner_id = "Partenaire",
                 name       = "Libellé",
                 debit      = "Débit",
                 credit     = "Crédit",
                 balance    = "Solde")

# « Comptabilisé » seulement : les brouillons ne sont pas de la comptabilité.
# Même filtre que pour l'export manuel (cf. docs/maintenance.md).
DOMAINE_ODOO <- function(depuis = NULL) {
  d <- list(list("parent_state", "=", "posted"))
  if (!is.null(depuis))
    d <- c(d, list(list("date", ">=", format(as.Date(depuis), "%Y-%m-%d"))))
  d
}

# Un champ relationnel arrive en [id, "libellé"], un champ vide en FALSE.
champ_texte <- function(x) {
  if (is.null(x) || isFALSE(x)) return(NA_character_)
  if (is.list(x)) return(if (length(x) >= 2) as.character(x[[2]])
                         else NA_character_)
  as.character(x)
}

# FALSE veut dire vide : un montant vide est un zéro.
champ_nombre <- function(x) {
  if (is.null(x) || isFALSE(x)) return(0)
  as.numeric(x)
}

#' Range la réponse de search_read en tableau, colonne par colonne.
range_lignes <- function(lignes, champs = CHAMPS_ODOO) {
  if (!length(lignes)) return(NULL)

  colonne <- function(cle) {
    brut <- lapply(lignes, function(l) l[[cle]])
    if (cle %in% c("debit", "credit", "balance"))
      vapply(brut, champ_nombre, numeric(1))
    else
      vapply(brut, champ_texte, character(1))
  }

  presents <- intersect(names(champs), names(lignes[[1]]))
  out <- lapply(presents, colonne)
  names(out) <- unname(champs[presents])
  tibble::as_tibble(out)
}

#' search_read paginé, ordonné par id (sans ordre, deux pages peuvent se
#' recouvrir).
#' @param exige_complet TRUE : atteindre `max_pages` est une erreur (résultat
#'   tronqué). FALSE : la limite est voulue (échantillon).
odoo_search_read <- function(ids, uid, modele, domaine, champs,
                             taille = 2000, max_pages = 500,
                             poste = odoo_poste, trace = NULL,
                             exige_complet = TRUE) {
  pages <- list()
  offset <- 0L
  for (i in seq_len(max_pages)) {
    page <- odoo_execute(
      ids, uid, modele, "search_read",
      args = list(domaine),
      kwargs = list(fields = as.list(champs), limit = taille,
                    offset = offset, order = "id"),
      poste = poste)
    n <- length(page)
    if (n) pages[[length(pages) + 1L]] <- page
    if (!is.null(trace)) trace(offset + n)
    if (n < taille) break
    offset <- offset + taille
    if (i == max_pages) {
      if (!exige_complet) break
      stop("Odoo : ", max_pages, " pages de ", taille,
           " lignes atteintes sans fin de résultat. Augmente max_pages, ",
           "ou resserre le domaine — mais ne lis pas un résultat tronqué.")
    }
  }
  unlist(pages, recursive = FALSE)
}

#' Les écritures d'Odoo, dans la forme de l'export fichier. NULL sans
#' identifiants : l'import continue sans Odoo.
#'
#' @param depuis date minimale, ou NULL (défaut) pour tout l'historique : une
#'   écriture passée rétroactivement échapperait à un filtre incrémental.
lit_ecritures_odoo <- function(ids = identifiants_odoo(), depuis = NULL,
                               taille = 2000, poste = odoo_poste,
                               trace = NULL) {
  if (is.null(ids)) return(NULL)
  uid <- odoo_connexion(ids, poste)
  lignes <- odoo_search_read(ids, uid, "account.move.line",
                             DOMAINE_ODOO(depuis), names(CHAMPS_ODOO),
                             taille = taille, poste = poste, trace = trace)
  range_lignes(lignes)
}

#### Vérifier le protocole contre l'instance ####

#' Les champs demandés existent-ils sur ce modèle ? Un nom faux fait échouer
#' tout l'appel ; fields_get permet de le nommer.
champs_absents <- function(ids, uid, modele = "account.move.line",
                           champs = names(CHAMPS_ODOO), poste = odoo_poste) {
  connus <- odoo_execute(ids, uid, modele, "fields_get",
                         args = list(list(), list("string")),
                         poste = poste)
  setdiff(champs, names(connus))
}

#' Combien de lignes le domaine désigne : comparé au nombre rangé, il
#' attrape une page manquante.
compte_ecritures <- function(ids, uid, depuis = NULL, poste = odoo_poste) {
  odoo_execute(ids, uid, "account.move.line", "search_count",
               args = list(DOMAINE_ODOO(depuis)), poste = poste)
}
