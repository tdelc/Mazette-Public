# Stockage normalise des tickets, et la nature d'une vente.
#
# Deux regles qui ont ete fausses, et dont l'erreur ne levait aucune exception.
suppressMessages({library(dplyr); library(tibble); library(lubridate)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
source("R/donnees.R", encoding = "UTF-8")

# ---------- 1. un produit, plusieurs libelles ----------
# La caisse laisse renommer une ligne : le meme ID_PRODUIT y apparait sous
# « Latte », « Latte deca », « Latte chaud ». La jointure de reconstruction se
# faisait sur le seul ID_PRODUIT et rendait CHAQUE ligne autant de fois qu'il
# existait de libelles.
caisse <- tibble(
  DATE = as.Date("2026-09-10") + c(0,0,1,1,2),
  TIMESTAMP = paste(DATE, c("14:00:00","14:00:00","09:00:00","09:00:00","20:00:00")),
  ID_TICKET = c(1,1,2,2,3),
  ID_PRODUIT = c(10, 20, 10, 20, 30),
  PRODUIT_FULL = c("Latte","Cookie figues","Latte deca","Cookie noix","Pizza"),
  PRODUIT   = c("Latte","Cookie figues","Latte","Cookie figues","Pizza"),
  BOISSON   = c("Latte","","Latte","",""),
  CATEGORIE = c("BOISSONS CHAUDES","SUCRÉ","BOISSONS CHAUDES","SUCRÉ","SALÉ"),
  TAUX_TVA  = c(0.21, 0.06, 0.21, 0.06, 0.12),
  VOLUME_CL = c(4, NA, 4, NA, NA),
  QUANTITE  = 1,
  PRIX_TOTAL = c(4, 3, 4, 3, 14))

n <- normalise_tickets(caisse)
ok("la table reduite garde une ligne par ligne de caisse", nrow(n$DB_TICKET) == nrow(caisse))
ok("elle porte ID_REF, pas ID_PRODUIT",
   "ID_REF" %in% names(n$DB_TICKET) && !"ID_PRODUIT" %in% names(n$DB_TICKET))
ok("le referentiel a une ligne par combinaison d'attributs", nrow(n$REF_PRODUITS) == 5)
ok("un meme produit peut y avoir plusieurs libelles",
   sum(n$REF_PRODUITS$ID_PRODUIT == 10) == 2)

h <- hydrate_donnees(n$DB_TICKET, n$REF_PRODUITS)$DB_TICKET
ok("la reconstruction ne duplique rien", nrow(h) == nrow(caisse))
ok("le CA TVAC est celui de la caisse", abs(sum(h$CA_TVAC) - sum(caisse$PRIX_TOTAL)) < 1e-9)
ok("les quantites aussi", sum(h$QUANTITE) == sum(caisse$QUANTITE))
ok("chaque ligne retrouve son libelle",
   identical(sort(h$PRODUIT_FULL), sort(caisse$PRODUIT_FULL)))
ok("un ticket de deux articles a deux lignes", nrow(filter(h, ID_TICKET == 1)) == 2)

# Un cache d'avant ID_REF : la jointure ne peut se faire que sur ID_PRODUIT, et
# le referentiel en porte plusieurs lignes. Sans precaution, c'est le bug.
ancien <- n$DB_TICKET %>%
  left_join(n$REF_PRODUITS %>% select(ID_REF, ID_PRODUIT), by = "ID_REF") %>%
  select(-ID_REF)
h2 <- hydrate_donnees(ancien, n$REF_PRODUITS)$DB_TICKET
ok("ancien cache : aucune duplication", nrow(h2) == nrow(caisse))
ok("ancien cache : le CA reste juste", abs(sum(h2$CA_TVAC) - sum(caisse$PRIX_TOTAL)) < 1e-9)
ok("sans ID_TICKET, la reduction ne tombe pas",
   nrow(normalise_tickets(caisse %>% select(-ID_TICKET))$DB_TICKET) == nrow(caisse))
ok("une caisse vide se reduit sans erreur",
   nrow(normalise_tickets(caisse[0, ])$DB_TICKET) == 0)

# ---------- 2. la nature d'une vente ----------
# Le test ne portait que sur le 12 % : tout le reste tombait en « Boisson »,
# 6 % compris. Or le 6 % est de la nourriture a emporter, et rien d'autre.
ok("le 12 % est de la nourriture", 0.12 %in% TAUX_NOURRITURE)
ok("le 6 % AUSSI : c'est la nourriture a emporter", 0.06 %in% TAUX_NOURRITURE)
ok("le 21 % reste de la boisson", !0.21 %in% TAUX_NOURRITURE)
ok("le pain a emporter, a 6 %, est de la nourriture",
   h$CD_SECTEUR[h$PRODUIT_FULL == "Cookie figues"][1] == "Nourriture")
ok("une biere a 21 % reste une boisson",
   h$CD_SECTEUR[h$PRODUIT_FULL == "Latte"][1] == "Boisson")
ok("la pizza sur place, a 12 %, est de la nourriture",
   h$CD_SECTEUR[h$PRODUIT_FULL == "Pizza"] == "Nourriture")
ok("aucune vente sans nature", !any(is.na(h$CD_SECTEUR)))
