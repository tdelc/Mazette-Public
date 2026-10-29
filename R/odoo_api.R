# Odoo par l'API : la même lecture, par un autre tuyau.
#
# ---------------------------------------------------------------------------
# Ce que ce fichier remplace — et rien d'autre
# ---------------------------------------------------------------------------
# R/odoo.R annonce son découpage ainsi :
#
#   lit_ecritures_fichier()  lecture brute          <- SEULE à changer avec l'API
#   normalise_ecritures()    typage, code de compte, journal
#   compta_mensuelle()       agrégation -> DB_COMPTA
#   controle_ecritures()     les garde-fous
#   coud_compta()            la jonction avec l'ancienne source
#
# Ce fichier tient cette promesse. Il fournit lit_ecritures_odoo(), qui rend
# EXACTEMENT la même chose que lit_ecritures_fichier() : un tableau brut dont
# les colonnes portent les libellés de l'export Odoo (ODOO_COLONNES). Tout ce
# qui suit — normalisation, agrégation, contrôles, couture, tests — ne bouge
# pas d'une ligne, et les deux sources restent comparables parce qu'elles
# traversent le même code.
#
# Le jour où l'API et le fichier donnent deux chiffres différents, ce ne sera
# donc pas la faute de deux chaînes de calcul divergentes : il n'y en a qu'une.
#
# ---------------------------------------------------------------------------
# JSON-RPC, et pas XML-RPC
# ---------------------------------------------------------------------------
# Odoo expose les deux. XML-RPC n'a pas de client R maintenu ; JSON-RPC se fait
# avec httr et jsonlite, tous deux DÉJÀ dans manifest.json. Aucune dépendance
# nouvelle, donc aucun risque sur le déploiement.
#
# ---------------------------------------------------------------------------
# Un avertissement honnête
# ---------------------------------------------------------------------------
# Ce fichier a été écrit sans pouvoir consulter la documentation d'Odoo (le
# réseau de l'environnement la refuse). Le protocole décrit ici est celui que
# je connais ; il n'a PAS été rejoué contre une instance réelle.
#
# C'est pour ça que essai_odoo_api.R existe. Il ne se contente pas d'appeler :
# il vérifie, un par un, chacun des points sur lesquels je peux m'être trompé
# — le nom du service, celui de la méthode, l'existence de chaque champ
# demandé — et il nomme celui qui cloche. Une erreur de protocole doit se lire
# en une ligne, pas se deviner dans un 500.

#### Identifiants ####

# Les secrets voyagent par l'environnement, comme LINK_JSON et PATH_DRIVE.
# Jamais dans le dépôt : Mazette-Public est public.
ODOO_ENV <- c(url   = "ODOO_URL",      # https://mazette.odoo.com
              base  = "ODOO_DB",       # le nom de la base
              login = "ODOO_LOGIN",    # l'adresse e-mail du compte
              cle   = "ODOO_CLE_API")  # Préférences -> Sécurité -> Clés API

#' Les identifiants, ou NULL s'ils ne sont pas tous là.
#'
#' Tout ou rien, à dessein. Trois variables sur quatre ne donnent pas « presque
#' une connexion » : elles donnent une erreur d'authentification dont la cause
#' est la variable manquante, et qu'on cherchera ailleurs.
identifiants_odoo <- function(env = ODOO_ENV) {
  v <- vapply(env, Sys.getenv, "")
  if (any(!nzchar(v))) return(NULL)
  # L'URL est écrite à la main : une barre finale en trop donnerait
  # « https://x//jsonrpc », qu'Odoo sert en 404 sans rien expliquer.
  v[["url"]] <- sub("/+$", "", v[["url"]])
  as.list(v)
}

#' Ce qui manque, pour le dire sans jamais afficher la clé.
manque_odoo <- function(env = ODOO_ENV) {
  names(env)[!nzchar(vapply(env, Sys.getenv, ""))]
}

#### Le transport ####

# L'unique endroit qui parle au réseau. Isolé exprès : les tests substituent
# leur propre `poste`, et toute la couche au-dessus se teste sans Odoo, sans
# jeton, et sans sortir de la machine.
odoo_poste <- function(url, corps_json) {
  rep <- httr::POST(paste0(url, "/jsonrpc"),
                    body = corps_json,
                    httr::content_type_json(),
                    httr::timeout(120))
  # Odoo répond 200 même pour une erreur applicative, et 404 pour une URL
  # fausse. On distingue les deux : un 404 n'a pas de corps JSON à lire.
  if (httr::status_code(rep) >= 400)
    stop("Odoo a répondu HTTP ", httr::status_code(rep),
         " sur ", url, "/jsonrpc")
  httr::content(rep, "text", encoding = "UTF-8")
}

# jsonlite rend `[]` pour une liste vide, là où Odoo attend `{}`. La nuance
# n'est pas décorative : execute_kw refuse un kwargs en tableau.
OBJET_VIDE <- structure(list(), names = character(0))

#' Un appel JSON-RPC, et la lecture de sa réponse.
#'
#' auto_unbox = TRUE : sans lui, chaque chaîne partirait en tableau d'un
#' élément et le domaine deviendrait illisible pour Odoo.
#' simplifyVector = FALSE à la lecture : search_read rend des champs
#' relationnels sous forme [id, "libellé"] et FALSE quand ils sont vides.
#' Laisser jsonlite « simplifier » ça produit des colonnes de types mélangés,
#' silencieusement. On lit donc la structure telle quelle, et on la range à la
#' main (champ_texte / champ_nombre).
odoo_appel <- function(url, service, methode, args, poste = odoo_poste) {
  corps <- list(jsonrpc = "2.0", method = "call",
                params = list(service = service, method = methode,
                              args = args))
  brut <- poste(url, jsonlite::toJSON(corps, auto_unbox = TRUE, null = "null"))
  res <- jsonlite::fromJSON(brut, simplifyVector = FALSE)

  if (!is.null(res$error)) {
    # Pas de %||% : il vient de shiny, et ce fichier doit pouvoir se lire et
    # se tester sans charger le dashboard.
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

#' La version du serveur. Le seul appel qui ne demande pas d'authentification.
#'
#' C'est donc le bon premier test : s'il échoue, le problème est l'URL ou le
#' réseau, pas la clé. Distinguer les deux fait gagner une heure.
odoo_version <- function(url, poste = odoo_poste) {
  odoo_appel(url, "common", "version", list(), poste)
}

#' L'uid, ou une erreur qui dit ce qui ne va pas.
#'
#' Odoo rend `false` — pas une erreur — quand les identifiants sont mauvais.
#' Le laisser passer donnerait un uid FALSE, un execute_kw refusé, et un
#' message parlant d'un droit d'accès. On tranche ici.
odoo_connexion <- function(ids, poste = odoo_poste) {
  uid <- odoo_appel(ids$url, "common", "authenticate",
                    list(ids$base, ids$login, ids$cle, OBJET_VIDE), poste)
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

# Les champs demandés à account.move.line, et ce qu'ils deviennent.
#
# Les noms de GAUCHE sont ceux d'Odoo, ceux de DROITE les libellés de l'export
# de la vue liste — donc ODOO_COLONNES. C'est tout le tour de passe-passe :
# l'API rend le même tableau que le fichier, et normalise_ecritures() ne sait
# pas lequel des deux elle lit.
CHAMPS_ODOO <- c(date       = "Date",
                 move_name  = "Numéro",
                 account_id = "Compte",
                 partner_id = "Partenaire",
                 name       = "Libellé",
                 debit      = "Débit",
                 credit     = "Crédit",
                 balance    = "Solde")

# « Comptabilisé » : les brouillons ne sont pas de la comptabilité. Même filtre
# qu'au §2.2 de docs/odoo.md pour l'export manuel — les deux chemins doivent
# voir le même périmètre, sinon les comparer ne prouve rien.
DOMAINE_ODOO <- function(depuis = NULL) {
  d <- list(list("parent_state", "=", "posted"))
  if (!is.null(depuis))
    d <- c(d, list(list("date", ">=", format(as.Date(depuis), "%Y-%m-%d"))))
  d
}

# Un champ relationnel arrive en [id, "libellé"], un champ vide en FALSE.
# « 600100 Achats de boissons » est exactement ce que la colonne « Compte » de
# l'export contient, donc ce que normalise_ecritures() sait déjà découper.
champ_texte <- function(x) {
  if (is.null(x) || isFALSE(x)) return(NA_character_)
  if (is.list(x)) return(if (length(x) >= 2) as.character(x[[2]])
                         else NA_character_)
  as.character(x)
}

# FALSE veut dire « vide » ; en comptabilité un montant vide est un zéro, pas
# un inconnu. Un NA ici remonterait jusqu'à une VALEUR manquante.
champ_nombre <- function(x) {
  if (is.null(x) || isFALSE(x)) return(0)
  as.numeric(x)
}

#' Range la réponse de search_read en tableau, colonne par colonne.
#'
#' Explicite plutôt que simplifié : jsonlite ferait un data.frame aux colonnes
#' de type imprévisible dès qu'un champ relationnel est vide sur une ligne.
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

#' search_read paginé.
#'
#' Odoo plafonne une réponse, et une instance en ligne coupe avant nous. On
#' pagine donc toujours, même pour « tout prendre », et on ORDONNE par id :
#' sans ordre explicite, deux pages successives peuvent se recouvrir ou
#' s'omettre, et on ne le verrait pas — le total aurait l'air plausible.
#' @param exige_complet TRUE : atteindre `max_pages` est une ERREUR, parce que
#'   le résultat serait tronqué sans que rien ne le dise. FALSE : la limite est
#'   voulue (on lit un échantillon), et on rend ce qu'on a. Les deux cas se
#'   ressemblent de l'extérieur et ne doivent surtout pas se confondre : l'un
#'   est un garde-fou, l'autre une demande.
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

#' LA fonction : les écritures d'Odoo, dans la forme de l'export fichier.
#'
#' Rend NULL quand les identifiants ne sont pas là, comme
#' lit_ecritures_fichier() rend NULL sur un chemin absent : l'import doit
#' pouvoir continuer sans Odoo, pas tomber.
#'
#' @param depuis date minimale, ou NULL pour tout l'historique. Par défaut
#'   NULL : docs/odoo.md §3 tranche pour un rechargement complet à chaque
#'   import, parce qu'une écriture passée rétroactivement sur un mois ancien
#'   échapperait à tout filtre incrémental — et en comptabilité, ça arrive.
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

#' Les champs demandés existent-ils vraiment sur ce modèle ?
#'
#' Un nom de champ faux ne rend pas une colonne vide : il fait échouer tout
#' l'appel. Le demander à fields_get AVANT de lire 40 000 lignes transforme un
#' échec opaque en une phrase — « move_name n'existe pas sur ce modèle ».
champs_absents <- function(ids, uid, modele = "account.move.line",
                           champs = names(CHAMPS_ODOO), poste = odoo_poste) {
  connus <- odoo_execute(ids, uid, modele, "fields_get",
                         args = list(list(), list("string")),
                         poste = poste)
  setdiff(champs, names(connus))
}

#' Combien de lignes le domaine désigne, avant d'en lire une seule.
#'
#' Sert de garde-fou de volume et de point de comparaison : si search_count
#' dit 41 230 et qu'on en range 41 230, la pagination n'a rien perdu. C'est le
#' seul contrôle qui attrape une page manquante au milieu.
compte_ecritures <- function(ids, uid, depuis = NULL, poste = odoo_poste) {
  odoo_execute(ids, uid, "account.move.line", "search_count",
               args = list(DOMAINE_ODOO(depuis)), poste = poste)
}
