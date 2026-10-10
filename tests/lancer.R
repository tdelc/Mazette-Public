# Lance tous les tests du dossier. A executer depuis la racine du depot :
#   Rscript tests/lancer.R
#
# Pas de framework : chaque fichier imprime « ok » ou « FAIL » par assertion,
# et ce script compte. Les paquets lourds du dashboard (plotly, DT, shiny) ne
# sont pas requis — les tests ne chargent que ce dont ils ont besoin.
fichiers <- sort(list.files("tests", "^test_.*\\.R$", full.names = TRUE))
total <- 0; echecs <- 0

for (f in fichiers) {
  cat("\n==", basename(f), "==\n")
  sortie <- tryCatch(
    utils::capture.output(source(f, encoding = "UTF-8", local = new.env())),
    error = function(e) paste("  FAIL erreur fatale :", conditionMessage(e)))
  cat(paste(sortie, collapse = "\n"), "\n")
  total  <- total  + sum(grepl("^  ok", sortie))
  echecs <- echecs + sum(grepl("FAIL",  sortie))
}

cat(sprintf("\n%d assertions, %d échec(s)\n", total, echecs))
if (echecs > 0) quit(status = 1)
