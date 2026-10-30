# Lance toute la suite de tests.
#
#   Rscript tests/lancer.R            depuis la racine du dépôt
#   Rscript tests/lancer.R analyse    seulement les fichiers dont le nom
#                                     contient « analyse »
#
# Pas de framework : chaque fichier imprime « ok » ou « FAIL » par assertion,
# et ce script compte. Les paquets lourds du dashboard ne sont pas tous
# requis — chaque test ne charge que ce dont il a besoin, et se met de côté
# proprement quand un paquet manque.
#
# Chaque fichier tourne dans SON environnement, pour qu'une doublure définie
# dans l'un ne contamine pas le suivant. Conséquence à connaître : un fichier
# de test doit sourcer le dashboard avec `local = TRUE`, sinon son `source()`
# retombe dans l'environnement global et les fonctions chargées n'y voient
# plus les constantes définies juste au-dessus d'elles.

# Depuis la racine, quel que soit l'endroit d'où on lance le script.
if (!dir.exists("tests") && basename(getwd()) == "tests") setwd("..")
if (!dir.exists("tests"))
  stop("Lance-moi depuis la racine du dépôt : Rscript tests/lancer.R")

filtre <- commandArgs(TRUE)
fichiers <- sort(list.files("tests", "^test_.*\\.R$", full.names = TRUE))
if (length(filtre))
  fichiers <- fichiers[grepl(paste(filtre, collapse = "|"), basename(fichiers))]
if (!length(fichiers)) stop("Aucun fichier de test ne correspond.")

total <- 0; echecs <- 0; details <- list()
t0 <- Sys.time()

for (f in fichiers) {
  d0 <- Sys.time()
  sortie <- tryCatch(
    utils::capture.output(source(f, encoding = "UTF-8", local = new.env())),
    error = function(e) paste("  FAIL erreur fatale :", conditionMessage(e)))
  n_ok <- sum(grepl("^  ok", sortie))
  n_ko <- sum(grepl("FAIL",  sortie))
  secondes <- round(as.numeric(difftime(Sys.time(), d0, units = "secs")), 1)

  cat(sprintf("\n== %-28s %3d ok, %d échec%s, %ss\n", basename(f), n_ok, n_ko,
              if (n_ko > 1) "s" else "", secondes))
  # Le détail n'est imprimé que s'il y a quelque chose à lire : une suite verte
  # de six cents lignes, personne ne la relit, et elle cache les trois qui
  # comptent le jour où elles apparaissent.
  if (n_ko > 0 || length(filtre)) cat(paste(sortie, collapse = "\n"), "\n")

  total  <- total + n_ok
  echecs <- echecs + n_ko
  if (n_ko) details[[basename(f)]] <- grep("FAIL", sortie, value = TRUE)
}

cat(sprintf("\n%s\n%d assertions, %d échec(s), %ss\n",
            strrep("-", 56), total, echecs,
            round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)))

if (echecs > 0) {
  cat("\nCe qui a échoué :\n")
  for (f in names(details))
    cat(" ", f, "\n", paste0("   ", details[[f]], collapse = "\n"), "\n")
  quit(status = 1)
}
cat("Tout est vert.\n")
