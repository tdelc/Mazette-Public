# L'API Odoo, sans Odoo.
#
# Tout le fichier tourne sur un FAUX transport : `poste` est substitue, il
# enregistre l'enveloppe JSON envoyee et rend une reponse ecrite a la main.
# On teste donc ce qu'on peut reellement tester hors ligne — la forme de
# l'enveloppe, la pagination, la lecture des champs relationnels, les
# messages d'erreur — et on ne teste PAS que le protocole est le bon.
#
# Cette derniere verification ne peut se faire que contre l'instance : c'est
# le role de essai_odoo_api.R, et la raison pour laquelle il existe.
suppressMessages({library(tibble); library(dplyr); library(stringr)
                  library(lubridate); library(jsonlite); library(tidyr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
source("R/odoo_api.R", encoding = "UTF-8", local = TRUE)

# Le faux transport : il garde ce qu'on lui envoie, et rend ce qu'on lui dit.
transport <- function(reponses) {
  envois <- list()
  i <- 0
  f <- function(url, corps_json) {
    i <<- i + 1
    envois[[i]] <<- list(url = url,
                         corps = fromJSON(corps_json, simplifyVector = FALSE))
    if (is.function(reponses)) reponses(i) else reponses[[min(i, length(reponses))]]
  }
  list(poste = f, envois = function() envois, n = function() i)
}

resultat <- function(x) toJSON(list(jsonrpc = "2.0", id = 1, result = x),
                               auto_unbox = TRUE, null = "null")

IDS <- list(url = "https://exemple.odoo.com", base = "mazette",
            login = "x@y.be", cle = "SECRET")

# ---------- l'enveloppe ----------
t1 <- transport(list(resultat(7)))
uid <- odoo_connexion(IDS, t1$poste)
e <- t1$envois()[[1]]
ok("l'appel part sur /jsonrpc", e$url == "https://exemple.odoo.com")
ok("l'enveloppe est du JSON-RPC 2.0",
   e$corps$jsonrpc == "2.0" && e$corps$method == "call")
ok("l'authentification passe par le service common",
   e$corps$params$service == "common" && e$corps$params$method == "authenticate")
ok("et porte base, login, cle dans cet ordre",
   identical(e$corps$params$args[1:3], list("mazette", "x@y.be", "SECRET")))
# jsonlite rend `[]` pour une liste vide ; execute_kw refuse un kwargs en
# tableau. La nuance se verifie sur le texte, pas sur l'objet relu.
ok("le 4e argument part en objet vide, pas en tableau vide",
   { t <- transport(list(resultat(7)))
     f <- function(url, corps) { assign("vu", corps, inherits = TRUE); resultat(7) }
     vu <- ""
     odoo_connexion(IDS, f)
     grepl('\\{\\}\\]', vu) })
ok("l'uid revient en entier", identical(uid, 7L))

# Un compte refuse : Odoo rend `false`, PAS une erreur. Le laisser passer
# donnerait un uid FALSE et, plus loin, un message parlant de droits d'acces.
ok("un refus d'identifiants est dit comme tel",
   { r <- try(odoo_connexion(IDS, transport(list(resultat(FALSE)))$poste),
              silent = TRUE)
     inherits(r, "try-error") && grepl("refusé les identifiants", r) })
ok("et il nomme la base et le compte, jamais la cle",
   { r <- try(odoo_connexion(IDS, transport(list(resultat(FALSE)))$poste),
              silent = TRUE)
     grepl("mazette", r) && grepl("x@y.be", r) && !grepl("SECRET", r) })

# ---------- les erreurs applicatives ----------
# Odoo repond 200 avec une cle `error`. Sans cette lecture, on prendrait
# l'erreur pour un resultat vide.
err <- toJSON(list(jsonrpc = "2.0", id = 1, error = list(
  code = 200, message = "Odoo Server Error",
  data = list(name = "odoo.exceptions.AccessError",
              message = "Vous n'avez pas accès à ce document"))),
  auto_unbox = TRUE)
ok("une erreur applicative devient une erreur R",
   { r <- try(odoo_version("https://x", transport(list(err))$poste), silent = TRUE)
     inherits(r, "try-error") })
ok("et elle garde le message d'Odoo",
   { r <- try(odoo_version("https://x", transport(list(err))$poste), silent = TRUE)
     grepl("pas accès à ce document", r) && grepl("AccessError", r) })
ok("l'appel fautif est nomme",
   { r <- try(odoo_version("https://x", transport(list(err))$poste), silent = TRUE)
     grepl("common.version", r) })

# ---------- execute_kw ----------
t2 <- transport(list(resultat(list())))
invisible(odoo_execute(IDS, 7L, "account.move.line", "search_read",
                       args = list(DOMAINE_ODOO()), kwargs = list(limit = 10),
                       poste = t2$poste))
a <- t2$envois()[[1]]$corps$params$args
ok("execute_kw passe par le service object",
   t2$envois()[[1]]$corps$params$method == "execute_kw")
ok("les six premiers arguments sont base, uid, cle, modele, methode",
   identical(a[1:5], list("mazette", 7L, "SECRET", "account.move.line",
                          "search_read")))
ok("le domaine est une liste de conditions, dans une liste d'arguments",
   identical(a[[6]], list(list(list("parent_state", "=", "posted")))))
ok("les kwargs sont le 7e argument", identical(a[[7]], list(limit = 10L)))

# Le filtre de date, quand on en veut un.
ok("une date minimale s'ajoute au domaine",
   identical(DOMAINE_ODOO(as.Date("2026-07-01"))[[2]],
             list("date", ">=", "2026-07-01")))
ok("et sans date, le domaine n'a que le filtre « comptabilise »",
   length(DOMAINE_ODOO()) == 1)

# ---------- la pagination ----------
# Sans ordre explicite, deux pages peuvent se recouvrir ou s'omettre sans que
# rien ne le montre : le total aurait l'air plausible.
ligne <- function(i) list(date = "2026-09-30", move_name = "MZACH/2026/09/0001",
                          account_id = list(42, "600100 Achats de boissons"),
                          partner_id = FALSE, name = paste("ligne", i),
                          debit = 10, credit = 0, balance = 10)
pages <- function(n_total, taille) function(i) {
  deja <- (i - 1) * taille
  k <- max(0, min(taille, n_total - deja))
  resultat(lapply(seq_len(k), ligne))
}
t3 <- transport(pages(5, 2))
l3 <- odoo_search_read(IDS, 7L, "account.move.line", DOMAINE_ODOO(),
                       names(CHAMPS_ODOO), taille = 2, poste = t3$poste)
ok("la pagination ramasse toutes les lignes", length(l3) == 5)
ok("elle s'arrete sur une page incomplete", t3$n() == 3)
offsets <- vapply(t3$envois(), function(e) e$corps$params$args[[7]]$offset, 0)
ok("les offsets avancent par pas de page", identical(offsets, c(0, 2, 4)))
ok("et chaque page est ordonnee par id",
   all(vapply(t3$envois(), function(e) e$corps$params$args[[7]]$order, "") == "id"))
# Un total multiple de la taille de page : la derniere page est vide, et il
# faut bien aller la chercher pour savoir qu'on a fini.
t4 <- transport(pages(4, 2))
ok("un total multiple de la page demande une page de plus",
   length(odoo_search_read(IDS, 7L, "x", DOMAINE_ODOO(), names(CHAMPS_ODOO),
                           taille = 2, poste = t4$poste)) == 4 && t4$n() == 3)
ok("aucune ligne du tout ne casse rien",
   { t <- transport(pages(0, 2))
     length(odoo_search_read(IDS, 7L, "x", DOMAINE_ODOO(), names(CHAMPS_ODOO),
                             taille = 2, poste = t$poste)) == 0 })
# Un resultat sans fin doit s'arreter en le DISANT : lire un tableau tronque
# en croyant qu'il est complet est le pire des deux echecs.
ok("une lecture sans fin s'arrete en erreur, pas en silence",
   { t <- transport(function(i) resultat(lapply(1:2, ligne)))
     r <- try(odoo_search_read(IDS, 7L, "x", DOMAINE_ODOO(),
                               names(CHAMPS_ODOO), taille = 2, max_pages = 3,
                               poste = t$poste), silent = TRUE)
     inherits(r, "try-error") && grepl("tronqué", r) })

# ---------- la mise en tableau ----------
tab <- range_lignes(lapply(1:3, ligne))
ok("les colonnes portent les libelles de l'export, pas les noms Odoo",
   all(c("Date", "Numéro", "Compte", "Partenaire", "Libellé", "Débit",
         "Crédit") %in% names(tab)))
ok("un champ relationnel rend son libelle affiche",
   tab$Compte[1] == "600100 Achats de boissons")
ok("un relationnel vide (FALSE) rend NA, pas « FALSE »",
   is.na(tab$Partenaire[1]))
ok("les montants sont numeriques", is.numeric(tab$Débit) && tab$Débit[1] == 10)
# FALSE sur un montant veut dire « vide » ; en compta, un montant vide est un
# zero. Un NA remonterait jusqu'a une VALEUR manquante dans DB_COMPTA.
ok("un montant vide est un zero, pas un NA",
   { l <- ligne(1); l$credit <- FALSE
     range_lignes(list(l))$Crédit == 0 })
ok("aucune ligne rend NULL, comme la lecture d'un fichier absent",
   is.null(range_lignes(list())))

# ---------- LE point : la meme forme que le fichier ----------
# Si cette assertion tombe, tout l'argument de docs/odoo.md tombe avec elle :
# « seule lit_ecritures_fichier() est remplacee ».
source("R/plan_comptable.R", encoding = "UTF-8", local = TRUE)
source("R/sources_travail.R", encoding = "UTF-8", local = TRUE)  # nombre_europeen
source("R/planning.R",        encoding = "UTF-8", local = TRUE)  # parse_date_souple
source("R/helpers.R",         encoding = "UTF-8", local = TRUE)  # ratio_pct
source("R/odoo.R",            encoding = "UTF-8", local = TRUE)
ok("le tableau de l'API porte exactement les colonnes attendues du fichier",
   length(setdiff(unname(ODOO_COLONNES), names(tab))) == 0)
n <- normalise_ecritures(tab)
ok("normalise_ecritures() le lit sans rien changer", nrow(n) == 3)
ok("le code de compte est separe du libelle",
   n$COMPTE[1] == "600100" && n$LIBELLE[1] == "Achats de boissons")
ok("le journal sort du numero de piece", n$JOURNAL[1] == "MZACH")
ok("la date est une vraie date", n$DATE[1] == as.Date("2026-09-30"))
ok("et l'annee et le mois en decoulent", n$ANNEE[1] == 2026 && n$MOIS[1] == 9)

# Jusqu'a DB_COMPTA, sur une vente et un achat.
vente <- ligne(9); vente$account_id <- list(1, "700100 Ventes boissons")
vente$debit <- 0; vente$credit <- 1000; vente$balance <- -1000
cp <- compta_mensuelle(normalise_ecritures(range_lignes(list(ligne(1), vente))))
ok("compta_mensuelle() produit DB_COMPTA depuis l'API",
   nrow(cp) == 2 && all(c("ANNEE", "MOIS", "COMPTE", "VALEUR") %in% names(cp)))
ok("et la convention de signe est respectee",
   cp$VALEUR[cp$COMPTE == "700100"] == 1000 &&
   cp$VALEUR[cp$COMPTE == "600100"] == 10)

# ---------- les identifiants ----------
ok("sans variables d'environnement, pas d'identifiants",
   { withr <- Sys.getenv(ODOO_ENV); on.exit(do.call(Sys.setenv, as.list(withr)))
     do.call(Sys.setenv, as.list(setNames(rep("", 4), ODOO_ENV)))
     is.null(identifiants_odoo()) })
ok("et lit_ecritures_odoo() rend NULL plutot que de tomber",
   { withr <- Sys.getenv(ODOO_ENV); on.exit(do.call(Sys.setenv, as.list(withr)))
     do.call(Sys.setenv, as.list(setNames(rep("", 4), ODOO_ENV)))
     is.null(lit_ecritures_odoo()) })
ok("trois variables sur quatre ne suffisent pas, et on dit laquelle manque",
   { withr <- Sys.getenv(ODOO_ENV); on.exit(do.call(Sys.setenv, as.list(withr)))
     do.call(Sys.setenv, as.list(setNames(c("u", "b", "l", ""), ODOO_ENV)))
     is.null(identifiants_odoo()) && identical(manque_odoo(), "cle") })
ok("une barre finale en trop dans l'URL est retiree",
   { withr <- Sys.getenv(ODOO_ENV); on.exit(do.call(Sys.setenv, as.list(withr)))
     do.call(Sys.setenv, as.list(setNames(c("https://x.odoo.com/", "b", "l", "k"),
                                          ODOO_ENV)))
     identifiants_odoo()$url == "https://x.odoo.com" })
