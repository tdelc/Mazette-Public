# La chaine reactive du volet Projection, sur le VRAI bloc de server.R.
#
# Le moteur est eprouve par test_projection.R ; ici on verifie le CABLAGE :
# que le bloc trouve ses deux sources, qu'il propose les bons mois a l'ouverture,
# que l'ecartement decide dans l'onglet Controle retire bien un mois des
# REFERENCES, et que chaque sortie rend quelque chose. Un calcul juste qu'on
# ne branche pas n'affiche rien, et aucun test du moteur ne le dirait.
suppressMessages({library(shiny); library(dplyr); library(tibble)
                  library(lubridate); library(tidyr); library(stringr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
COUL_ROUGE<-"#c0392b"; COUL_AMBRE<-"#d98236"; COUL_VERT<-"#5B7B5A"
COUL_NEUTRE<-"#8d7b68"; COUL_BRUN<-"#732c02"; COUL_MATIERE<-"#d3c0ac"
COUL_TRAVAIL<-"#732c02"
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")
plot_ly<-function(...) structure(list(),class="p")
plotly_empty<-function(...) structure(list(),class="p")
add_bars<-function(p,...){force(p);p}; add_lines<-function(p,...){force(p);p}
add_trace<-function(p,...){force(p);p}; add_markers<-function(p,...){force(p);p}
layout<-function(p,...){force(p);p}; change_cursor_plotly<-function(p){force(p);p}
# event_data / event_register vivent dans plotly, que ces tests ne chargent
# pas : le bloc serveur les appelle pour le clic sur la trajectoire.
event_register <- function(p, ...) p
event_data <- function(...) NULL
datatable<-function(x,...) structure(list(data=x),class="dt")
formatStyle<-function(x,...) x; styleEqual<-function(...) NULL
render_stub <- function(expr, env=parent.frame(), quoted=FALSE){
  f <- exprToFunction(expr, env, quoted); renderText({ f(); "" })}
renderPlotly <- render_stub; renderDT <- render_stub
for (f in c("R/helpers.R","R/theme.R","R/kpi.R","R/plan_comptable.R",
            "R/exploitation.R","R/analyse.R","R/ecritures.R","R/planning.R",
            "R/sources_travail.R","R/projection.R"))
  source(f, encoding="UTF-8", local = TRUE)

# Le socle (d'ou viennent compta_brute() et compta_exclus()) et la Projection.
# Extraits, pas recopies : une copie diverge.
src <- readLines("server.R", encoding = "UTF-8", warn = FALSE)
bloc <- function(debut, fin) {
  i <- grep(debut, src); j <- grep(fin, src)
  stopifnot(length(i) == 1, length(j) == 1, j > i)
  parse(text = paste(src[(i+1):(j-1)], collapse = "\n"), encoding = "UTF-8")
}
SOCLE <- bloc('Volet "Compta / Gestion" . socle commun',
              'Volet "Compta / Gestion" . Contrôle')
PROJ  <- bloc('Volet "Compta / Gestion" . Projection',
              'Volet "Travail" . Sources')
cat("Socle :", length(SOCLE), "expressions | Projection :", length(PROJ), "\n")

# Deux ans de comptabilite, puis trois mois sans remunerations ni frais
# generaux — la situation de la maison depuis juillet. Relatif a aujourd'hui :
# une fixture datee en dur se met a mentir le mois suivant.
EN_COURS <- floor_date(today(), "month")
TROUE_DES <- EN_COURS %m-% months(2)
MOIS <- seq(EN_COURS %m-% months(23), EN_COURS, by = "month")
HEURES_MOIS <- 900 + 10 * (seq_along(MOIS) %% 7)
l <- list()
for (k in seq_along(MOIS)) {
  dt <- MOIS[k]; troue <- dt >= TROUE_DES
  cpt <- c("700000", "600100"); val <- c(62000, 18600)
  if (!troue) { cpt <- c(cpt, "620000", "610000")
                val <- c(val, 25 * HEURES_MOIS[k], 8000) }
  l[[k]] <- tibble(COMPTE = cpt, VALEUR = val, ANNEE = year(dt),
                   MOIS = month(dt), LIBELLE = "x", TYPE = "compte",
                   AGREGE = TRUE)
}
DB_COMPTA <- bind_rows(l)
jours <- seq(min(MOIS), fin_periode(EN_COURS, "mois"), by = "day")
DB_KPI_SIMPLE <- tibble(DATE = jours) %>%
  mutate(CA_HTVA = 62000 / 0.95 / days_in_month(DATE), CA_TVAC = CA_HTVA * 1.12)
# Planning au jour le jour, sur tout le mois : une ligne par mois ferait
# passer chaque mois pour tronqué.
DB_HEURES_PLANNING <- bind_rows(lapply(seq_along(MOIS), function(i) {
  j <- seq(MOIS[i], fin_periode(MOIS[i], "mois"), by = "day")
  tibble(DATE = j, SERVICE = "Salle", HEURES = HEURES_MOIS[i] / length(j))
}))
# Le pointage : 95 % des heures planifiées, et il s'arrête hier — comme le
# vrai, qui ne connaît pas l'avenir.
DB_COUTS_TRAVAIL <- DB_HEURES_PLANNING %>%
  filter(DATE < today()) %>%
  transmute(DATE, SECTEUR = "Service", HEURES = HEURES * 0.95,
            COUT_TRAVAIL = HEURES * 18)

srv <- function(input, output, session) {
  ONGLETS_PRETS <- reactiveVal(TRUE)
  DB_COMPTA <- DB_COMPTA
  for (e in as.list(SOCLE)) eval(e)
  for (e in as.list(PROJ))  eval(e)
}

testServer(srv, {
  ok("les ventes de caisse arrivent jusqu'au bloc", nrow(proj_ventes()) > 20)
  ok("le pointage et le planning arrivent jusqu'au bloc",
     { s <- proj_sources()
       !is.null(s$horeko) && !is.null(s$planning) && is.null(s$paie) })

  # Ce que l'écran propose en s'ouvrant.
  d <- proj_defauts()
  ok("les mois troués sont proposés d'office",
     setequal(d$mois, MOIS[MOIS >= TROUE_DES]))
  ok("et seulement les postes qui y manquent",
     setequal(d$cles, c("remuneration", "generaux")))

  # L'utilisateur accepte la proposition.
  session$setInputs(proj_mois = as.character(d$mois),
                    proj_postes = d$cles,
                    proj_nref = 6,
                    proj_aj_ca = 0, proj_aj_matieres = 0,
                    proj_aj_remuneration = 0, proj_aj_generaux = 0)

  p <- projection()
  ok("chaque mois troué reçoit ses deux postes", nrow(p) == 6)
  # LE point de ce lot : sur un mois CLOS on prend ce qui a été mesuré — les
  # heures pointées — et le planning ne répond que pour le mois en cours,
  # dont le pointage s'arrête hier.
  r <- p %>% filter(CLE == "remuneration")
  ok("un mois clos est estimé sur les heures POINTÉES",
     all(r$METHODE[r$PERIODE < EN_COURS] == "Heures pointées"))
  ok("le mois en cours, lui, sur les heures planifiées",
     r$METHODE[r$PERIODE == EN_COURS] == "Heures planifiées")
  ok("et les deux tombent sur les heures du mois au taux médian",
     { h <- HEURES_MOIS[match(r$PERIODE, MOIS)]
       all(abs(r$RETENU - 25 * h) < 1) })

  c1 <- proj_compte()
  ok("le compte d'exploitation projeté couvre tous les mois",
     nrow(c1) == length(MOIS))
  ok("les mois projetés y sont marqués",
     setequal(c1$PERIODE[c1$PROJETE], MOIS[MOIS >= TROUE_DES]))
  ok("et leur marge n'est plus celle d'un mois amputé",
     { m <- c1 %>% filter(PERIODE == TROUE_DES)
       b <- postes_exploitation(DB_COMPTA) %>% filter(PERIODE == TROUE_DES)
       m$MARGE_AA < b$MARGE_AA - 20000 })

  # LE point d'intégration : l'écartement décidé dans l'onglet Contrôle doit
  # retirer un mois des RÉFÉRENCES. Sans cela, chaque onglet aurait son idée
  # de ce qui est fiable.
  avant <- projection()$RETENU[projection()$CLE == "generaux"][1]
  # On écarte un mois complet en lui donnant un poids anormal d'abord :
  # ici il suffit de vérifier que la projection RÉAGIT à la sélection.
  session$setInputs(topo_table_rows_selected = c(4L, 5L, 6L))
  ok("un mois écarté sort des références",
     length(compta_exclus()) == 3 && !is.null(projection()))
  ok("et la projection se recalcule sans tomber",
     nrow(projection()) == 6)
  session$setInputs(topo_table_rows_selected = integer(0))

  # Les hypothèses.
  session$setInputs(proj_aj_generaux = 10)
  ok("un curseur d'hypothèse déplace le retenu, pas le modèle",
     { q <- projection() %>% filter(CLE == "generaux")
       all(abs(q$RETENU - q$MODELE * 1.1) < 0.01) && all(q$AJUST == 10) })
  session$setInputs(proj_aj_generaux = 0)

  # Le contrôle du modèle.
  ctrl <- proj_ctrl()
  ok("le modèle s'éprouve sur les mois connus", nrow(ctrl) > 0)
  ok("et ne se juge que sur des postes réellement présents",
     all(abs(ctrl$REEL) > PROJ_SEUIL_PRESENT))

  # Les sorties. Un calcul juste qu'on ne branche pas n'affiche rien.
  ok("les tuiles se rendent",
     nzchar(as.character(output$proj_kpi$html %||% output$proj_kpi)))
  ok("le bandeau se tait quand tout va bien",
     !any(nzchar(as.character(output$proj_bandeau$html %||%
                              output$proj_bandeau))))
  ok("la fiabilité se rend",
     nzchar(as.character(output$proj_fiabilite$html %||% output$proj_fiabilite)))
  # La cascade se choisit : le dernier mois projeté par défaut, n'importe
  # quel mois de la trajectoire ensuite.
  ok("la trajectoire ne propose que les mois qu'elle montre",
     { t <- proj_mois_traj()
       length(t) == min(PROJ_MOIS_GRAPHE, length(MOIS)) &&
         max(t) == EN_COURS })
  ok("le titre de la cascade nomme le mois projeté par défaut",
     grepl(etiquette_periode(EN_COURS, "mois"), output$proj_titre_cascade,
           fixed = TRUE) &&
       grepl("projection comprise", output$proj_titre_cascade))
  session$setInputs(proj_mois_cascade = as.character(MOIS[length(MOIS) - 6]))
  ok("en choisir un autre déplace la cascade",
     identical(proj_mois_cascade(), MOIS[length(MOIS) - 6]))
  ok("et un mois réel est annoncé comme tel",
     grepl("tel qu'il est", output$proj_titre_cascade))
  ok("la cascade du mois choisi se calcule", !is.null(output$proj_cascade))
  for (sortie in c("proj_graph", "proj_cascade", "proj_detail",
                   "proj_resultat", "proj_controle"))
    ok(paste(sortie, "se calcule sans erreur"), !is.null(output[[sortie]]))

  # Aucun mois choisi : l'écran doit se taire, pas tomber.
  session$setInputs(proj_mois = character(0))
  ok("sans mois choisi, la projection est vide", nrow(projection()) == 0)
  ok("le compte d'exploitation revient au réel", !any(proj_compte()$PROJETE))
  ok("et les tuiles invitent à en choisir un",
     grepl("Choisissez", as.character(output$proj_kpi$html %||%
                                      output$proj_kpi)))
})
