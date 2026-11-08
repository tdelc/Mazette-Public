# La date des données, et ce qu'on en dit.
#
# Le dashboard ne reconstruit plus les données au démarrage : il ouvre sur le
# dernier cache, quelle que soit sa date (cf. test_connect.R). Le prix de cette
# rapidité est un risque nouveau — afficher en silence des chiffres de la
# semaine dernière — et ce fichier éprouve ce qui l'en empêche.
suppressMessages({library(lubridate)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
source("R/fraicheur.R", encoding = "UTF-8", local = TRUE)

AUJ <- as.Date("2026-11-08")
J <- function(n) AUJ - n   # il y a n jours

#### Le retard se compte à partir de la veille ####

# Le cache de la nuit porte la date d'HIER : c'est le dernier jour que
# DB_JOURS connaisse. Zéro jour de retard, donc, et non un.
e <- etat_fraicheur(J(1), AUJ)
ok("un cache d'hier est à jour", e$retard == 0 && e$verdict == "ok")
ok("et il le dit en français", e$libelle == "à jour")
ok("en vert", e$couleur == COUL_VERT)

e <- etat_fraicheur(J(2), AUJ)
ok("deux jours : un jour de retard", e$retard == 1)
ok("et le verdict passe à l'ambre",
   e$verdict == "tard" && e$couleur == COUL_AMBRE)
ok("au singulier", e$libelle == "1 jour de retard")

ok("trois jours de retard, encore ambre",
   etat_fraicheur(J(4), AUJ)$verdict == "tard")
ok("quatre, c'est rouge",
   etat_fraicheur(J(5), AUJ)$verdict == "vieux")
ok("et le rouge est le rouge de la palette",
   etat_fraicheur(J(5), AUJ)$couleur == COUL_ROUGE)
ok("au pluriel", etat_fraicheur(J(5), AUJ)$libelle == "4 jours de retard")

# Un import lancé à la main après la fermeture, ou une horloge décalée, peut
# produire un cache daté du jour. Ce n'est pas « en avance », c'est à jour.
ok("un cache du jour n'est pas en avance",
   etat_fraicheur(AUJ, AUJ)$retard == 0)
ok("ni un cache de demain", etat_fraicheur(AUJ + 1, AUJ)$retard == 0)

#### Quand on ne sait pas ####

ok("date absente : verdict inconnu",
   etat_fraicheur(NULL, AUJ)$verdict == "inconnu")
ok("date NA : verdict inconnu",
   etat_fraicheur(as.Date(NA), AUJ)$verdict == "inconnu")
ok("et aucune couleur d'appréciation",
   etat_fraicheur(NULL, AUJ)$couleur == COUL_NEUTRE)
ok("le retard est NA, pas zéro",
   is.na(etat_fraicheur(NULL, AUJ)$retard))

#### La date, sans dépendre de la locale du serveur ####

# format(x, "%b") suit la locale du SERVEUR : un serveur en locale C rend
# « Nov » là où on attend « nov. ». C'est le mois de global.R qui doit sortir.
ok("le mois est celui du dashboard", date_fr(as.Date("2026-11-08")) == "8 nov. 2026")
ok("janvier aussi", date_fr(as.Date("2026-01-31")) == "31 janv. 2026")
ok("pas de date : un tiret", date_fr(as.Date(NA)) == "—")

#### La pastille et son dialogue ####

if (!requireNamespace("shiny", quietly = TRUE)) {
  cat("  (shiny absent : pastille et dialogue non éprouvés)\n")
} else {
  library(shiny)
  html <- function(x) paste(as.character(x), collapse = "")

  b <- html(badge_fraicheur(etat_fraicheur(J(1), AUJ), J(1)))
  ok("la pastille porte la date", grepl("7 nov. 2026", b, fixed = TRUE))
  ok("elle est cliquable", grepl("ouvrir_fraicheur", b, fixed = TRUE))
  # Sur la barre brune, la palette d'appréciation n'a plus de contraste : la
  # pastille a la sienne (cf. COUL_BARRE).
  ok("et sa couleur est celle de la barre, pas celle du fond crème",
     grepl(COUL_BARRE[["ok"]], b, fixed = TRUE) &&
     !grepl(COUL_VERT, b, fixed = TRUE))
  ok("un retard se voit à la couleur",
     grepl(COUL_BARRE[["vieux"]], html(badge_fraicheur(etat_fraicheur(J(9), AUJ), J(9))),
           fixed = TRUE))

  # Une autre session a importé : cet écran est périmé et doit le dire, même
  # si la date qu'il connaît, elle, était fraîche.
  r <- html(badge_fraicheur(etat_fraicheur(J(1), AUJ), J(1), a_recharger = TRUE))
  ok("données remplacées ailleurs : la pastille appelle au rechargement",
     grepl("nouvelles données", r, fixed = TRUE))

  d <- html(dialogue_fraicheur(etat_fraicheur(J(1), AUJ), J(1),
                               peut_importer = FALSE))
  ok("le dialogue propose toujours de reprendre le dernier import",
     grepl("recharger_donnees", d, fixed = TRUE))
  ok("mais pas de refaire l'import quand on n'y a pas droit",
     !grepl("import_complet", d, fixed = TRUE))
  ok("et il le propose quand on y a droit",
     grepl("import_complet",
           html(dialogue_fraicheur(etat_fraicheur(J(1), AUJ), J(1), TRUE)),
           fixed = TRUE))
  # htmltools sépare les enfants d'une balise par un retour à la ligne : la
  # phrase est bien là, mais coupée. On cherche donc le mot, pas la phrase.
  ok("il annonce que le mot de passe sera redemandé",
     grepl("ressaisir", d, fixed = TRUE))
  ok("un cache vieux appelle à chercher la cause, pas juste à recliquer",
     grepl("script planifié",
           html(dialogue_fraicheur(etat_fraicheur(J(9), AUJ), J(9), TRUE)),
           fixed = TRUE))
}
