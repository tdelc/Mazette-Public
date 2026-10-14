# Le pont de marge : l'identite comptable, et sa LISIBILITE.
#
# Deux exigences, et la seconde a deja ete prise en defaut : le pont doit
# boucler a l'euro, et le lecteur doit pouvoir refaire le calcul avec les
# seuls chiffres affiches a l'ecran.
suppressMessages({library(dplyr); library(tibble); library(lubridate)
                  library(tidyr); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
COUL_VERT<-"#5B7B5A"; COUL_AMBRE<-"#d98236"; COUL_ROUGE<-"#c0392b"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"; COUL_MATIERE<-"#d3c0ac"
COUL_TRAVAIL<-"#8d5b8c"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
balise <- function(...) structure(list(args = list(...)), class = "tag")
div<-balise; span<-balise; p<-balise; icon<-balise; tagList<-balise
tags <- list(b=balise, br=balise, i=balise, span=balise, div=balise)
kpi_tile <- function(...) balise(...)
plot_ly<-function(...) structure(list(),class="p"); plotly_empty<-function(...) structure(list(),class="p")
add_bars<-function(p,...){force(p);p}; add_lines<-function(p,...){force(p);p}
add_trace<-function(p,...){force(p);p}; layout<-function(p,...){force(p);p}
change_cursor_plotly<-function(p){force(p);p}
texte_tag <- function(x) {
  if (is.character(x)) return(paste(x, collapse=" "))
  if (inherits(x,"tag")) return(paste(vapply(x$args %||% list(), texte_tag, ""), collapse=" "))
  if (is.list(x)) return(paste(vapply(x, texte_tag, ""), collapse=" "))
  "" }

source("R/helpers.R",        encoding = "UTF-8", local = TRUE)
source("R/theme.R",          encoding = "UTF-8", local = TRUE)
source("R/kpi.R",            encoding = "UTF-8", local = TRUE)
source("R/plan_comptable.R", encoding = "UTF-8", local = TRUE)
source("R/exploitation.R",   encoding = "UTF-8", local = TRUE)
source("R/analyse.R",        encoding = "UTF-8", local = TRUE)

periode <- function(ca, autres, mat, rem, gen, fin, p = "2026-09-01") {
  tibble(PERIODE = as.Date(p), CA = ca, AUTRES = autres, PRODUITS = ca + autres,
         MATIERES = mat, REMUNERATION = rem, GENERAUX = gen,
         AMORTISSEMENT = 0, FINANCIER = fin,
         CHARGES = mat + rem + gen,
         MARGE = ca + autres - mat - rem - gen,
         MARGE_AA = ca + autres - mat - rem - gen + fin, N_MOIS = 1L) |>
    mutate(PCT_MATIERES = ratio_pct(MATIERES, CA),
           PCT_TRAVAIL  = ratio_pct(REMUNERATION, CA),
           PCT_PRIME    = ratio_pct(MATIERES + REMUNERATION, CA),
           PCT_GENERAUX = ratio_pct(GENERAUX, CA),
           PCT_MARGE    = ratio_pct(MARGE, CA),
           PCT_MARGE_AA = ratio_pct(MARGE_AA, CA))
}
REF <- periode(100000, 2000, 30000, 40000, 10000, -500, "2025-09-01")
ACT <- periode(110000, 2500, 35000, 42000, 11000, -400, "2026-09-01")

# ---------- 1. le pont boucle ----------
pt <- pont_marge(ACT, REF)
ok("un effet volume plus un effet par poste", nrow(pt) == 6)
ok("l'effet volume est a part", sum(pt$NATURE == "volume") == 1)
# L'identite : marge de depart + somme des effets = marge d'arrivee, A L'EURO.
ok("le pont boucle a l'euro",
   abs(pt$DEPART[1] + sum(pt$EFFET) - pt$ARRIVEE[1]) < 0.01)
ok("le depart est la marge de la reference", pt$DEPART[1] == REF$MARGE_AA)
ok("l'arrivee est celle de la periode", pt$ARRIVEE[1] == ACT$MARGE_AA)
# L'effet volume : la croissance du CA au taux de marge de la reference.
s0 <- REF$MARGE_AA / REF$CA
ok("l'effet volume vaut delta CA x taux de marge de reference",
   abs(pt$EFFET[pt$NATURE == "volume"] - (ACT$CA - REF$CA) * s0) < 0.01)
# Un poste de charge qui augmente plus vite que le CA pese NEGATIVEMENT.
ok("une charge qui derive pese en negatif",
   pt$EFFET[pt$POSTE == "Matières premières"] < 0)
ok("et l'attendu est le poids de reference applique au CA de la periode",
   abs(pt$ATTENDU[pt$POSTE == "Matières premières"] -
       REF$MATIERES / REF$CA * ACT$CA) < 0.01)

# Sans CA de reference, on ne peut pas raisonner en taux : repli sur le brut.
sans_ca <- periode(0, 0, 0, 0, 0, 0)
ok("sans CA de reference, le pont passe en brut",
   all(pont_marge(ACT, sans_ca)$NATURE == "brut"))
ok("et il boucle quand meme",
   { b <- pont_marge(ACT, sans_ca)
     abs(b$DEPART[1] + sum(b$EFFET) - b$ARRIVEE[1]) < 0.01 })
ok("sans reference du tout -> NULL", is.null(pont_marge(ACT, NULL)))

# ---------- 2. le calcul doit etre REFAISABLE a l'ecran ----------
# Le reproche d'origine : « je percois le principe, mais je n'arrive pas a
# refaire le calcul ». Le tableau affiche donc des EUROS, pas des points
# arrondis : reel - attendu doit redonner l'effet, au centime.
tv <- table_verification_pont(pt)
ok("la verification affiche les trois etapes et le bouclage", nrow(tv) == 4)
ok("et elle annonce que le pont boucle",
   grepl("boucle", tv$Montant[tv$Étape == "Écart de bouclage"]))
tb <- table_pont_marge(pt)
ok("le tableau du pont a une ligne par effet", nrow(tb) >= nrow(pt))
# La soustraction affichee doit tomber juste : c'est tout l'enjeu. Elle ne
# vaut QUE pour les lignes de taux — la ligne de volume, elle, se refait par
# une multiplication, et c'est pour ca qu'elle est presentee a part.
tx <- pt[pt$NATURE == "taux", ]
ok("sur une ligne de taux, reel moins attendu redonne l'effet au centime",
   all(abs(tx$SIGNE * (tx$REEL - tx$ATTENDU) - tx$EFFET) < 0.01))
vo <- pt[pt$NATURE == "volume", ]
ok("sur la ligne de volume, c'est l'ecart de CA fois le taux de marge",
   abs((vo$REEL - vo$ATTENDU) * vo$BASE / 100 - vo$EFFET) < 0.01)
ok("et la base affichee EST le taux de marge de la reference",
   abs(vo$BASE - 100 * REF$MARGE_AA / REF$CA) < 0.01)

# format_points : meme piege que format_CA. « 4 pt » d'ecart sur les matieres
# est exactement le genre de valeur ronde qui partait en « 4e+00 ».
ok("un nombre rond ne part pas en notation scientifique",
   format_points(100000) == "100000,0 pt")
ok("le signe est optionnel", format_points(4, signe = TRUE) == "+4,0 pt")
ok("la virgule est decimale", format_points(4.25) == "4,2 pt" || format_points(4.25) == "4,3 pt")

# ---------- 3. le registre des indicateurs ----------
# Ecrire PCT_FRAIS_GENERAUX (au lieu de PCT_GENERAUX) faisait tomber TOUTE la
# rangee de tuiles : a[["colonne inconnue"]] vaut NULL, NULL - NULL vaut
# numeric(0), et `if (e >= 0)` sur numeric(0) leve.
ok("le registre a des cles uniques", !any(duplicated(INDICATEURS_ANALYSE$CLE)))
ok("chaque indicateur nomme une colonne existante",
   all(INDICATEURS_ANALYSE$COLONNE %in% names(ACT)))
ok("les frais generaux y sont", "generaux" %in% INDICATEURS_ANALYSE$CLE)
ok("une cle inconnue retombe sur la marge, sans lever",
   !is.null(indicateur_analyse("n'importe quoi")))
ok("mais la version stricte rend NULL", is.null(indicateur_strict("n'importe quoi")))
ok("chaque indicateur a un sens", all(INDICATEURS_ANALYSE$SENS %in% c(-1, 1)))

# ---------- 4. le rabotage ----------
# Comparer une periode incomplete a une periode complete fausse tout. On peut
# donc raboter la reference a la meme taille.
ok("un mois attend un mois", mois_attendus("mois") == 1)
ok("un trimestre en attend trois", mois_attendus("trimestre") == 3)
ok("une annee en attend douze", mois_attendus("annee") == 12)
mensuel <- bind_rows(lapply(1:5, function(m)
  periode(1000 * m, 0, 0, 0, 0, 0, sprintf("2026-%02d-01", m))))
off <- offsets_periode(mensuel, as.Date("2026-01-01"), "annee")
ok("une annee a cinq mois donne cinq offsets", length(off) == 5)
ok("les offsets partent de zero", min(off) == 0)
ok("offset_mois rend un entier", is.integer(offset_mois(as.Date("2026-03-01"), "annee")))
rab <- serie_rabotee(mensuel, "annee", 0:2)
ok("la serie rabotee ne garde que les mois demandes", rab$CA == 1000 + 2000 + 3000)
ok("l'etiquette dit ce qui a ete garde", nchar(etiquette_rabotage(0:2, "annee")) > 0)
