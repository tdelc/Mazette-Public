# LE parseur de dates du projet, et le defaut qui l'a fait remonter au socle.
#
# L'import a casse net sur « argument 'trim' incorrect », au milieu de la DB
# JOURS. La cause : `ymd(format(DATEVALUE, "%Y-%m-%d"))`, un idiome qui
# suppose que DATEVALUE est une Date ou un POSIXct. Le jour ou la colonne
# revient en TEXTE — une cellule saisie a la main dans le classeur, ou un
# rbind() avec une archive dont le type differe — format() prend son deuxieme
# argument pour `trim`, et tout l'import tombe sur un message qui ne nomme ni
# la colonne, ni la table, ni la cause.
#
# Ces tests tiennent les deux bouts : le parseur accepte tout ce qu'un
# classeur sait rendre, et le bloc REEL d'import.R ne tombe plus.
suppressMessages({library(dplyr); library(tibble); library(tidyr)
                  library(lubridate); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
vecteur_jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi",
                   "dimanche")
source("R/date.R", encoding = "UTF-8", local = TRUE)

CIBLE <- as.Date("2026-09-17")

#### Tout ce qu'un classeur sait rendre ####

ok("une Date passe telle quelle", parse_date_souple(CIBLE) == CIBLE)
ok("un POSIXct devient sa date",
   parse_date_souple(as.POSIXct("2026-09-17 14:00", tz = "UTC")) == CIBLE)
ok("une date ISO en texte", parse_date_souple("2026-09-17") == CIBLE)
ok("une date belge saisie a la main", parse_date_souple("17/09/2026") == CIBLE)
# Le numero de serie : origine 1899-12-30, et surtout pas 1900-01-01.
ok("un numero de serie Excel", parse_date_souple(46282) == CIBLE)

# LE cas qui a casse l'import : un rbind() entre une archive et l'import du
# jour fabrique UNE colonne a partir de deux. Un POSIXct empile sur du texte
# devient du texte, heure comprise.
ok("un POSIXct passe en texte par un rbind",
   parse_date_souple("2026-09-17 00:00:00") == CIBLE)
ok("et un numero de serie passe en texte",
   parse_date_souple("46282") == CIBLE)

ok("un format inconnu rend NA, et la ligne sera ecartee",
   is.na(parse_date_souple("pas une date")))
ok("une cellule vide aussi", is.na(parse_date_souple("")))
ok("vectorise, et sans perdre de ligne",
   { v <- parse_date_souple(c("2026-09-17", "18/09/2026", "", "n'importe"))
     length(v) == 4 && sum(is.na(v)) == 2 })
ok("un melange de formats dans la meme colonne",
   { v <- parse_date_souple(c("2026-09-17", "17/09/2026", "46282"))
     all(v == CIBLE) })

#### Le bloc REEL de l'import ####

# Extrait d'import.R, pas recopie : c'est le branchement qui doit etre
# eprouve, et une copie diverge.
src <- readLines("import.R", encoding = "UTF-8", warn = FALSE)
i <- grep("^##### IMPORT DB JOURS ####", src)
j <- grep("^##### IMPORT DB OLD ####", src)
stopifnot(length(i) == 1, length(j) == 1, j > i)
BLOC <- parse(text = paste(src[(i + 1):(j - 1)], collapse = "\n"),
              encoding = "UTF-8")
cat("Bloc DB JOURS extrait d'import.R :", length(BLOC), "expressions\n")

jours <- seq(as.Date("2026-01-01"), as.Date("2026-01-10"), by = "day")
brut <- function(datevalue) tibble(
  DATEVALUE = datevalue, CARTES = 100, CASH = 50, GIFTCARD = 0, VIREMENT = 10,
  `CA HTVA` = 140, `CA TVAC` = 160)

joue <- function(datevalue) {
  env <- new.env(parent = environment())
  env$IMPORT_DB_JOURS <- brut(datevalue)
  res <- try(suppressMessages(
    utils::capture.output(for (e in as.list(BLOC)) eval(e, envir = env))),
    silent = TRUE)
  if (inherits(res, "try-error")) NULL else env$DB_JOURS
}

ok("des POSIXct : le cas nominal",
   { d <- joue(as.POSIXct(jours, tz = "UTC"))
     !is.null(d) && nrow(d) == 10 && inherits(d$DATE, "Date") })
# LE test : c'est exactement ce qui tombait.
ok("une colonne de dates en TEXTE ne fait plus tomber l'import",
   { d <- joue(format(jours, "%Y-%m-%d %H:%M:%S"))
     !is.null(d) && nrow(d) == 10 && all(d$DATE == jours) })
ok("des numeros de serie non plus",
   { d <- joue(as.numeric(jours - as.Date("1899-12-30")))
     !is.null(d) && all(d$DATE == jours) })
ok("une cellule illisible fait perdre SA ligne, pas l'import",
   { txt <- format(jours, "%Y-%m-%d"); txt[3] <- "a completer"
     d <- joue(txt)
     !is.null(d) && nrow(d) == 9 && !jours[3] %in% d$DATE })
ok("et les colonnes attendues sont toutes la",
   { d <- joue(as.POSIXct(jours, tz = "UTC"))
     identical(names(d), c("DATE", "CARTES", "CASH", "GIFTCARD", "VIREMENT",
                           "CA_HTVA", "CA_TVAC")) })
