# L'import quotidien, sans dashboard autour, lancé par GitHub Actions
# (.github/workflows/import.yml) : il refait l'import et dépose le cache sur
# Drive, où le dashboard le reprend.
#
# Toute la logique est dans refait_import() (R/connect.R), comme pour le
# bouton « Refaire l'import » du dashboard.
#
# Les journaux d'Actions de ce dépôt sont PUBLICS : la trace complète de
# l'import est détournée vers outputs/import.log (détruit avec l'exécutant),
# et le résumé ne publie que des noms de tables, des nombres de lignes, des
# dates et le SUJET des contrôles — jamais un montant. En cas d'échec, les
# dernières lignes du journal sont publiées : ce ne sont que des titres
# d'étape et des décomptes.

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

  # D'où viennent les écritures et jusqu'où elles vont : des dates et des
  # décomptes, jamais un montant.
  ecr <- tryCatch(get("DB_ECRITURES", envir = globalenv()), error = function(e) NULL)
  ids <- tryCatch(get("IDS_ODOO", envir = globalenv()), error = function(e) NULL)
  if (is.data.frame(ecr) && nrow(ecr) && "DATE" %in% names(ecr)) {
    d <- as.Date(ecr$DATE)
    passees <- d[!is.na(d) & d <= Sys.Date()]
    dit("")
    dit("DB_ECRITURES : lues par ",
        if (!is.null(ids)) "l'API Odoo" else "le fichier d'export sur Drive",
        " (écritures comptabilisées seulement, pas les brouillons) ; ",
        "dernière écriture passée le ",
        if (length(passees)) format(max(passees)) else "—",
        if (any(d > Sys.Date(), na.rm = TRUE))
          paste0(", plus ", sum(d > Sys.Date(), na.rm = TRUE),
                 " lignes datées du futur") else "")
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

  # Un import qui réussit sur une source qui n'a pas bougé : on le dit.
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
