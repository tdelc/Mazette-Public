#!/usr/bin/env Rscript
# L'import de la nuit, sans dashboard autour.
#
# ---------------------------------------------------------------------------
# À quoi ça sert
# ---------------------------------------------------------------------------
# Jusqu'ici, l'import tournait quand quelqu'un ouvrait le dashboard sans
# trouver de cache à la bonne date : la première personne connectée du matin
# attendait plusieurs minutes, les suivantes non. Ce script fait le même
# travail, mais la nuit et ailleurs — sur un exécutant GitHub Actions
# (cf. .github/workflows/import.yml) — de sorte que le dashboard n'ait plus
# qu'à reprendre le fichier déposé sur Drive.
#
# ---------------------------------------------------------------------------
# Ce qu'il ne fait pas
# ---------------------------------------------------------------------------
# Il ne contient AUCUNE logique d'import, et aucun code de sauvegarde. Tout
# est dans refait_import() (R/connect.R), qui est aussi ce que lance le bouton
# « refaire l'import » du dashboard. Un seul chemin, donc un seul format de
# fichier, un seul contrat de tables, et un seul endroit à corriger. Un script
# de nuit qui recopierait le save() finirait par produire un fichier que le
# dashboard ne sait plus lire, et personne ne saurait lequel des deux a tort.
#
# ---------------------------------------------------------------------------
# Les journaux d'Actions d'un dépôt public sont PUBLICS
# ---------------------------------------------------------------------------
# D'où le détour par sink() : la trace complète de l'import reste dans
# outputs/import.log, sur l'exécutant, qui est détruit à la fin. Ce qui monte
# dans le résumé est choisi ligne par ligne ci-dessous — des noms de tables,
# des nombres de lignes, des dates, et le SUJET des contrôles sans leur
# détail, parce qu'un détail de contrôle comptable porte des montants.
#
# En cas d'échec, les dernières lignes du journal sont imprimées : ce sont des
# titres d'étape et des décomptes (tous les appels de trace d'import.R sont des
# cli_h*/cli_alert_* ; aucun n'imprime une table ni un montant), et sans elles
# un échec n'est qu'un carré rouge.

t0 <- Sys.time()
options(warn = 1)

suppressWarnings(dir.create("outputs", showWarnings = FALSE))

cat("Chargement du socle…\n")
source("global.R", encoding = "UTF-8")

journal <- "outputs/import.log"
con <- file(journal, open = "wt")
sink(con); sink(con, type = "message")

res <- tryCatch(connexion_ou_creation(force_import = TRUE, envir = globalenv()),
                error = function(e) e)

sink(type = "message"); sink(); close(con)

minutes <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)

#### Ce qui sort d'ici ####

lignes <- character(0)
dit <- function(...) lignes <<- c(lignes, paste0(...))

if (inherits(res, "error")) {
  dit("## Import en échec")
  dit("")
  dit("**", conditionMessage(res), "**")
  dit("")
  dit("Dernières lignes du journal :")
  dit("")
  dit("```")
  trace <- tryCatch(readLines(journal, warn = FALSE), error = function(e) character(0))
  dit(paste(utils::tail(trace, 40), collapse = "\n"))
  dit("```")
} else {
  dit("## Import réussi en ", minutes, " min")
  dit("")
  dit("Fichier : `", res$fichier, "`")
  dit("")
  dit("| Table | Lignes |")
  dit("|---|---:|")
  for (t in sort(res$tables)) {
    obj <- tryCatch(get(t, envir = globalenv()), error = function(e) NULL)
    n <- if (is.data.frame(obj)) nrow(obj) else length(obj)
    dit("| ", t, " | ", format(n, big.mark = " "), " |")
  }

  jours <- tryCatch(get("DB_JOURS", envir = globalenv()), error = function(e) NULL)
  if (is.data.frame(jours) && nrow(jours)) {
    dit("")
    dit("DB_JOURS : du ", min(jours$DATE), " au ", max(jours$DATE),
        " (", nrow(jours), " jours)")
  }

  # Les contrôles de l'import, sujet et verdict. Jamais le détail : il porte
  # des montants, et ce journal est public.
  for (nom in c("CONTROLE_ODOO", "CONTROLE_ONSS", "CONTROLE_COUTURE")) {
    ctrl <- tryCatch(get(nom, envir = globalenv()), error = function(e) NULL)
    if (!is.data.frame(ctrl) || !nrow(ctrl)) next
    dit("")
    dit("**", nom, "**")
    dit("")
    for (i in seq_len(nrow(ctrl)))
      dit("- ", if (identical(ctrl$VERDICT[i], "ok")) "✅" else "⚠️", " ",
          ctrl$CONTROLE[i])
  }

  # Un import qui réussit mais ne rapporte rien de neuf est le scénario du
  # 21 octobre : la table n'avait pas bougé depuis des jours et personne ne
  # l'a vu. On le dit ici, là où ça se lit.
  if (is.data.frame(jours) && nrow(jours)) {
    retard <- as.integer(Sys.Date() - max(jours$DATE)) - 1L
    if (retard > 0) {
      dit("")
      dit("> ⚠️ Le dernier jour connu a ", retard, " jour(s) de retard sur hier.",
          " L'import a réussi, mais la SOURCE n'est pas à jour.")
    }
  }
}

texte <- paste(lignes, collapse = "\n")
cat(texte, "\n", sep = "")

resume <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(resume)) cat(texte, "\n", file = resume, append = TRUE, sep = "")

if (inherits(res, "error")) quit(status = 1L)
