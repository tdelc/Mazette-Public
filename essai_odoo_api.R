# Essai de l'API Odoo, étape par étape.
#
#   Rscript essai_odoo_api.R
#
# ---------------------------------------------------------------------------
# Pourquoi ce script existe
# ---------------------------------------------------------------------------
# R/odoo_api.R a été écrit sans accès à la documentation d'Odoo. Le protocole
# qu'il met en œuvre est donc une hypothèse — solide, mais une hypothèse.
#
# Ce script la met à l'épreuve point par point, du plus bête au plus engageant,
# et S'ARRÊTE au premier qui tombe en disant lequel. L'ordre n'est pas
# cosmétique : il sépare les causes. Si l'étape 2 passe et la 3 échoue, le
# réseau et l'URL sont bons et le problème est la clé ; si la 4 échoue, les
# identifiants sont bons et c'est un nom de champ qui est faux. Un seul appel
# « qui ne marche pas » ne dirait rien de tout cela.
#
# Il ne modifie RIEN : que des lectures, et aucun fichier écrit sauf si tu
# passes --csv.
#
# Options :
#   --depuis=2026-07-01   ne lire qu'à partir de cette date (défaut : tout)
#   --max=500             s'arrêter après N lignes (essai rapide)
#   --csv=chemin.csv      écrire les écritures normalisées pour les relire
#   --fichier=export.xlsx comparer avec un export manuel du même périmètre

suppressMessages({
  library(dplyr); library(tibble); library(tidyr); library(stringr)
  library(lubridate); library(jsonlite); library(httr)
})

for (f in c("R/plan_comptable.R", "R/sources_travail.R", "R/planning.R",
            "R/helpers.R", "R/exploitation.R", "R/odoo.R", "R/odoo_api.R"))
  source(f, encoding = "UTF-8")

args <- commandArgs(TRUE)
opt <- function(nom, defaut = NULL) {
  v <- grep(paste0("^--", nom, "="), args, value = TRUE)
  if (!length(v)) defaut else sub(paste0("^--", nom, "="), "", v[1])
}
DEPUIS  <- opt("depuis")
MAXLIG  <- as.integer(opt("max", "0"))
CSV     <- opt("csv")
FICHIER <- opt("fichier")

etape <- function(n, titre) cat(sprintf("\n[%s] %s\n", n, titre))
vert  <- function(...) cat("   OK   ", ..., "\n")
rouge <- function(...) { cat("   STOP ", ..., "\n"); quit(status = 1) }
note  <- function(...) cat("   ...  ", ..., "\n")

# ---------------------------------------------------------------------------
etape(1, "Les identifiants")
# ---------------------------------------------------------------------------
absents <- manque_odoo()
if (length(absents))
  rouge("variables d'environnement manquantes : ",
        paste(ODOO_ENV[absents], collapse = ", "),
        "\n          Pose-les dans ~/.Renviron (puis relance R), par exemple :",
        "\n          ODOO_URL=https://mazette.odoo.com",
        "\n          ODOO_DB=le-nom-de-la-base",
        "\n          ODOO_LOGIN=ton.adresse@mazette.be",
        "\n          ODOO_CLE_API=la-cle-generee-dans-Odoo")
ids <- identifiants_odoo()
# La clé n'est jamais affichée, même tronquée : un bout de secret dans un log
# reste un bout de secret.
vert("url =", ids$url, "| base =", ids$base, "| login =", ids$login,
     "| clé = définie (", nchar(ids$cle), "caractères )")

# ---------------------------------------------------------------------------
etape(2, "Le serveur répond (sans authentification)")
# ---------------------------------------------------------------------------
# Premier appel volontairement anonyme : s'il échoue, le problème est l'URL ou
# le réseau, et surtout PAS la clé. C'est la moitié du temps de débogage gagnée.
v <- tryCatch(odoo_version(ids$url), error = function(e)
  rouge(paste0("impossible de joindre ", ids$url, "/jsonrpc"),
        "\n          ", conditionMessage(e),
        "\n          Vérifie ODOO_URL (sans barre finale, avec https://)."))
vert("Odoo", v$server_version, "— protocole", v$protocol_version)

# ---------------------------------------------------------------------------
etape(3, "L'authentification par clé API")
# ---------------------------------------------------------------------------
uid <- tryCatch(odoo_connexion(ids), error = function(e)
  rouge(conditionMessage(e),
        "\n          La clé se génère dans Odoo : Préférences -> Sécurité du",
        "compte -> Clés API.",
        "\n          Le nom de la base est visible sur /web/database/selector",
        "ou en bas de l'écran de connexion."))
vert("connecté, uid =", uid)

# ---------------------------------------------------------------------------
etape(4, "Les champs demandés existent sur account.move.line")
# ---------------------------------------------------------------------------
# Un nom de champ faux ne rend pas une colonne vide : il fait échouer tout
# l'appel, avec un message qui ne dit pas lequel. On demande donc d'abord.
abs_champs <- tryCatch(champs_absents(ids, uid), error = function(e)
  rouge("fields_get a échoué : ", conditionMessage(e)))
if (length(abs_champs))
  rouge("ces champs n'existent pas sur ce modèle : ",
        paste(abs_champs, collapse = ", "),
        "\n          Corrige CHAMPS_ODOO dans R/odoo_api.R.",
        "\n          Candidats usuels : move_name -> move_id,",
        "parent_state -> move_id.state.")
vert("les", length(CHAMPS_ODOO), "champs sont connus du modèle")

# ---------------------------------------------------------------------------
etape(5, "Combien de lignes, avant d'en lire une seule")
# ---------------------------------------------------------------------------
n_attendu <- tryCatch(compte_ecritures(ids, uid, DEPUIS), error = function(e)
  rouge("search_count a échoué : ", conditionMessage(e),
        "\n          Si le message parle de parent_state, ce champ n'est pas",
        "filtrable sur cette version :",
        "\n          remplace-le par list(\"move_id.state\", \"=\", \"posted\")",
        "dans DOMAINE_ODOO()."))
if (n_attendu == 0)
  rouge("le domaine ne désigne AUCUNE ligne.",
        "\n          Soit la base est vide, soit le filtre « comptabilisé »",
        "ne dit pas ce qu'on croit.",
        "\n          Essaie sans filtre de date, ou relis DOMAINE_ODOO().")
vert(format(n_attendu, big.mark = " "), "écritures comptabilisées",
     if (is.null(DEPUIS)) "(tout l'historique)" else paste("depuis", DEPUIS))

# ---------------------------------------------------------------------------
etape(6, "Une ligne, en détail")
# ---------------------------------------------------------------------------
# Avant de tirer 40 000 lignes, on en regarde UNE. C'est là qu'on voit si
# « Compte » arrive bien en « 600100 Libellé » et non en identifiant nu.
une <- odoo_search_read(ids, uid, "account.move.line", DOMAINE_ODOO(DEPUIS),
                        names(CHAMPS_ODOO), taille = 1, max_pages = 1,
                        exige_complet = FALSE)
brut1 <- range_lignes(une[1])
for (cl in names(brut1))
  cat(sprintf("        %-12s : %s\n", cl, format(brut1[[cl]][1])))
if (is.na(brut1[["Compte"]][1]) ||
    !grepl("^[0-9]", brut1[["Compte"]][1]))
  rouge("la colonne « Compte » ne commence pas par un numéro.",
        "\n          normalise_ecritures() en tire COMPTE et LIBELLE : sans",
        "numéro en tête, tout le classement PCMN tombe.",
        "\n          Demande account_id.code séparément plutôt que le display",
        "name.")
vert("la colonne « Compte » porte bien le numéro en tête")

# ---------------------------------------------------------------------------
etape(7, "La lecture complète")
# ---------------------------------------------------------------------------
taille <- if (MAXLIG > 0) min(MAXLIG, 2000) else 2000
pages  <- if (MAXLIG > 0) ceiling(MAXLIG / taille) else 500
t0 <- Sys.time()
lignes <- odoo_search_read(ids, uid, "account.move.line", DOMAINE_ODOO(DEPUIS),
                           names(CHAMPS_ODOO), taille = taille,
                           max_pages = pages, exige_complet = (MAXLIG == 0),
                           trace = function(n) cat("\r        ", n, "lignes"))
cat("\r")
brut <- range_lignes(lignes)
duree <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
vert(nrow(brut), "lignes en", duree, "s")

# Le seul contrôle qui attrape une page perdue au milieu : search_count a
# annoncé un nombre, on doit en ranger exactement autant.
if (MAXLIG == 0 && nrow(brut) != n_attendu)
  rouge("search_count annonçait ", n_attendu, " lignes, on en a rangé ",
        nrow(brut), ".",
        "\n          Une page s'est perdue : ne te sers pas de ce résultat.")
if (MAXLIG == 0) vert("le compte correspond exactement à search_count")

# ---------------------------------------------------------------------------
etape(8, "La chaîne du dashboard, inchangée")
# ---------------------------------------------------------------------------
# Le point de tout l'exercice : à partir d'ici, c'est le code de la phase 1,
# celui qui tourne déjà sur les exports fichier. Aucune branche « si API ».
ecr <- normalise_ecritures(brut)
vert(nrow(ecr), "écritures normalisées (",
     nrow(brut) - nrow(ecr), "lignes sans compte ou sans date écartées )")

cp <- compta_mensuelle(ecr)
vert(nrow(cp), "lignes de DB_COMPTA, de",
     format(min(as.Date(sprintf("%d-%02d-01", cp$ANNEE, cp$MOIS))), "%m/%Y"),
     "à",
     format(max(as.Date(sprintf("%d-%02d-01", cp$ANNEE, cp$MOIS))), "%m/%Y"))

ctrl <- controle_ecritures(ecr, cp, reprise = REPRISE_ODOO)
cat("\n")
for (i in seq_len(nrow(ctrl)))
  cat(sprintf("        [%-4s] %-34s %s\n", ctrl$VERDICT[i], ctrl$CONTROLE[i],
              ctrl$DETAIL[i]))

# ---------------------------------------------------------------------------
etape(9, "Les postes d'exploitation")
# ---------------------------------------------------------------------------
p <- postes_exploitation(cp)
if (!nrow(p)) {
  note("aucun poste : aucune ligne de classe 6 ou 7 sur la période.")
} else {
  aff <- p %>% transmute(Mois = format(PERIODE, "%m/%Y"),
                         CA = round(CA), Matières = round(MATIERES),
                         Rémun. = round(REMUNERATION), Généraux = round(GENERAUX),
                         Marge = round(MARGE))
  print(as.data.frame(tail(aff, 12)), row.names = FALSE)
}

# ---------------------------------------------------------------------------
if (!is.null(FICHIER)) {
  etape(10, "Comparaison avec l'export manuel")
  # Les deux sources doivent donner le même compte de résultat au centime près
  # (docs/odoo.md §4, étape 4). Un écart n'est pas un arrondi à accepter.
  cf <- compta_mensuelle(normalise_ecritures(lit_ecritures_fichier(FICHIER)))
  comp <- full_join(cp %>% select(ANNEE, MOIS, COMPTE, API = VALEUR),
                    cf %>% select(ANNEE, MOIS, COMPTE, FICHIER = VALEUR),
                    by = c("ANNEE", "MOIS", "COMPTE")) %>%
    mutate(across(c(API, FICHIER), ~replace_na(.x, 0)),
           ECART = round(API - FICHIER, 2)) %>%
    filter(abs(ECART) > 0.005)
  if (!nrow(comp)) vert("les deux sources donnent exactement la même chose")
  else {
    cat("\n")
    print(as.data.frame(head(comp, 30)), row.names = FALSE)
    note(nrow(comp), "écarts. Tant qu'il en reste un, garde l'ancienne source.")
  }
}

if (!is.null(CSV)) {
  write.csv2(ecr, CSV, row.names = FALSE, fileEncoding = "UTF-8")
  note("écritures normalisées écrites dans", CSV)
}

cat("\nTerminé.\n")
