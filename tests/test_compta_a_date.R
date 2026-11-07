# Les mois à VENIR ne doivent atteindre aucun volet.
#
# Odoo porte désormais des écritures datées jusqu'en juin 2027 — loyers,
# assurances, amortissements programmés. Elles sont justes, et elles n'ont
# rien à faire dans une analyse de ce qui s'est passé : un chiffre d'affaires
# de juin 2027 dans une tendance, des charges que personne n'a payées dans une
# marge annuelle.
#
# Ce n'est pas un écartement au sens de ecarte_mois(), et la distinction porte
# tout le fichier. Un mois écarté est un mois RÉEL dont on doute, choisi à la
# main et réversible. Un mois à venir n'a pas eu lieu : rien à discuter, rien
# à cocher, il ne doit pas exister dans les totaux.
#
# Une seule exception : le grand livre de l'onglet Écritures, qui lit
# DB_ECRITURES. C'est le journal d'Odoo, il montre ce qu'Odoo contient.
suppressMessages({library(dplyr); library(tibble); library(tidyr)
                  library(lubridate); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
source("R/helpers.R",      encoding = "UTF-8", local = TRUE)
source("R/theme.R",        encoding = "UTF-8", local = TRUE)
source("R/kpi.R",          encoding = "UTF-8", local = TRUE)
source("R/plan_comptable.R", encoding = "UTF-8", local = TRUE)
source("R/exploitation.R", encoding = "UTF-8", local = TRUE)

#### La coupe elle-même ####

AUJ <- as.Date("2026-11-07")
db <- tibble(ANNEE = 2026L, MOIS = 6:12, COMPTE = "700000", LIBELLE = "Ventes",
             VALEUR = 1000, TYPE = "compte", AGREGE = TRUE) %>%
  bind_rows(tibble(ANNEE = 2027L, MOIS = 1:6, COMPTE = "610000",
                   LIBELLE = "Loyer", VALEUR = 900, TYPE = "compte",
                   AGREGE = TRUE))

coupe <- compta_a_date(db, AUJ)
ok("les mois passés restent", all(c(6:10) %in% coupe$MOIS[coupe$ANNEE == 2026]))
# Le mois en cours est partiel, pas futur : il reste.
ok("le mois en cours reste, tout partiel qu'il soit",
   11 %in% coupe$MOIS[coupe$ANNEE == 2026])
ok("décembre et toute l'année suivante partent",
   !any(coupe$ANNEE == 2027) && !12 %in% coupe$MOIS[coupe$ANNEE == 2026])
ok("et rien d'autre n'a bougé", ncol(coupe) == ncol(db))

ok("les mois retirés se nomment",
   { m <- mois_a_venir(db, AUJ)
     length(m) == 7 && min(m) == as.Date("2026-12-01") &&
       max(m) == as.Date("2027-06-01") })
ok("sans mois à venir, la liste est vide",
   length(mois_a_venir(coupe, AUJ)) == 0)
ok("une table vide ou nulle traverse sans erreur",
   is.null(compta_a_date(NULL)) && nrow(compta_a_date(db[0, ], AUJ)) == 0)
ok("un mois illisible ne passe pas pour du passé",
   { sale <- db %>% mutate(MOIS = if_else(row_number() == 1, NA_integer_, MOIS))
     nrow(compta_a_date(sale, AUJ)) == nrow(coupe) - 1 })

# La coupe n'est PAS un écartement : les deux doivent pouvoir se composer
# sans que l'une défasse l'autre.
ok("elle se compose avec l'écartement d'un mois douteux",
   { r <- ecarte_mois(compta_a_date(db, AUJ), as.Date("2026-09-01"))
     !9 %in% r$MOIS[r$ANNEE == 2026] && 10 %in% r$MOIS[r$ANNEE == 2026] })

# Les postes d'exploitation ne doivent plus voir l'avenir.
ok("le compte d'exploitation s'arrête au mois en cours",
   { p <- postes_exploitation(compta_a_date(db, AUJ))
     max(p$PERIODE) == as.Date("2026-11-01") })

#### Le garde-fou : aucun autre chemin vers DB_COMPTA ####

# server.R lit DB_COMPTA à quatre endroits — coûts matière, accueil, socle du
# volet Compta, comparaison de périodes. Un cinquième ajouté sans la coupe
# ferait rentrer juin 2027 par la fenêtre, et personne ne le verrait : les
# montants sont justes, c'est leur DATE qui ne l'est pas.
srv <- readLines("server.R", encoding = "UTF-8", warn = FALSE)
lignes <- grep("DB_COMPTA", srv)
lignes <- lignes[!grepl("^\\s*#", srv[lignes])]        # les commentaires, non
reste <- vapply(lignes, function(i) {
  l <- srv[i]
  l <- gsub('exists\\("DB_COMPTA"\\)', "", l, perl = TRUE)   # un garde
  l <- gsub("compta_a_date\\(DB_COMPTA", "", l, perl = TRUE) # le bon chemin
  grepl("DB_COMPTA", l)
}, logical(1))

ok(paste0("tout passage par DB_COMPTA passe par compta_a_date()",
          if (any(reste)) paste0(" (ligne(s) ",
                                 paste(lignes[reste], collapse = ", "), ")")
          else ""),
   !any(reste))
ok("et il y a bien des passages à surveiller",
   sum(grepl("compta_a_date\\(DB_COMPTA", srv)) >= 3)
# Le grand livre, lui, ne doit PAS être coupé.
ok("les écritures ne passent pas par la coupe",
   !any(grepl("compta_a_date\\(DB_ECRITURES", srv)))
