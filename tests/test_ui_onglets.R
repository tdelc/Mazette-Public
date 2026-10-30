# Chaque onglet se construit-il encore ?
#
# Le test le plus bete du lot, et celui qui manquait. Les fonctions d'interface
# ne sont appelees NULLE PART dans le code : R/acces.R les designe par leur NOM,
# sous forme de chaine, dans la table ONGLETS. Aucune analyse statique ne peut
# donc voir qu'elles servent — et un nettoyage de code mort les emporterait
# toutes sans qu'un seul test ne bronche.
#
# Ici on les appelle vraiment, une par une, et on construit leur UI.
suppressMessages({library(shiny); library(bslib); library(dplyr); library(tibble)
                  library(tidyr); library(stringr); library(lubridate)
                  library(htmltools)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")

# DT et plotly sont necessaires : les UI posent leurs sorties.
manquants <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                    c("DT", "plotly"))
if (length(manquants)) {
  cat("  ok   test ignore :", paste(manquants, collapse = ", "), "absent(s)\n")
} else {
suppressMessages({library(DT); library(plotly)})

COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"; COUL_MATIERE<-"#d3c0ac"
COUL_TRAVAIL<-"#8d5b8c"
COULEURS_SECTEURS <- c("Service"="#2980b9","Transformation alimentaire"="#5B7B5A",
                       "Brasserie"="#d98236","Support"="#8d7b68")
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
vecteur_jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
`%||%` <- function(a, b) if (is.null(a)) b else a

# shinyjs / shinyWidgets / shinybusy ne sont pas requis pour BATIR une UI : on
# les double quand ils manquent, et on utilise les vrais quand ils sont la.
# parent.frame() ne suffit pas : lancer.R source ce fichier dans un
# environnement a lui, et les doublures doivent atterrir LA, pas dans celui de
# l'appel. On capture donc l'environnement du test une bonne fois.
ENV_TEST <- environment()
doublure <- function(paquet, noms) {
  if (requireNamespace(paquet, quietly = TRUE)) {
    suppressMessages(library(paquet, character.only = TRUE)); return(invisible())
  }
  for (n in noms) assign(n, function(...) NULL, envir = ENV_TEST)
}
doublure("shinyjs",      c("useShinyjs", "hidden"))
doublure("shinyWidgets", c("pickerInput", "airDatepickerInput",
                           "numericInputIcon", "prettySwitch",
                           "radioGroupButtons", "switchInput"))
doublure("shinybusy",    c("add_busy_spinner"))

for (f in list.files("R", "\\.R$", full.names = TRUE))
  source(f, encoding = "UTF-8", local = TRUE)

# ui.R charge des paquets qu'on vient peut-etre de doubler : on retire ses
# appels a library() et ses prefixes de namespace avant de l'evaluer.
src <- readLines("ui.R", encoding = "UTF-8", warn = FALSE)
src <- src[!grepl("^library\\((shinyjs|shinyWidgets|shinybusy)\\)", src)]
src <- gsub("shinyjs::|shinyWidgets::|shinybusy::", "", src)
eval(parse(text = paste(src, collapse = "\n"), encoding = "UTF-8"))

ok("la table des onglets est lisible", nrow(ONGLETS) > 0 && "UI" %in% names(ONGLETS))
for (i in seq_len(nrow(ONGLETS))) {
  fn <- ONGLETS$UI[i]
  r <- try(force(do.call(fn, list())), silent = TRUE)
  ok(paste0(ONGLETS$TITRE[i], " (", fn, ") se construit"),
     !inherits(r, "try-error"))
  if (inherits(r, "try-error"))
    cat("        ", sub("\n.*", "", conditionMessage(attr(r, "condition"))), "\n")
}

# Et le squelette complet, qui les assemble.
ok("l'application entiere se construit",
   !inherits(try(force(ui_app()), silent = TRUE), "try-error"))
}
