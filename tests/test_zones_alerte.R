# Le verrou des zones d'alerte.
#
# Un uiOutput vide est masque par `.zone-alerte:empty { display: none }`, pour
# qu'une alerte absente ne laisse pas de bande blanche. Mais Shiny SUSPEND les
# sorties masquees : vide -> masquee -> suspendue -> jamais calculee -> vide.
# Le verrou est parfait et parfaitement silencieux : la sortie reste sur
# « recalculating », sans erreur ni message, et n'affiche jamais rien.
#
# Dix bandeaux en etaient morts. Ce test empeche le onzieme.
suppressMessages({library(dplyr)})
ok <- function(l, c) cat(if (isTRUE(c)) "  ok  " else "  FAIL", l, "\n")
source("R/helpers.R", encoding = "UTF-8", local = TRUE)

ui  <- readLines("ui.R",     encoding = "UTF-8", warn = FALSE)
srv <- readLines("server.R", encoding = "UTF-8", warn = FALSE)
css <- readLines("www/style.css", encoding = "UTF-8", warn = FALSE)

# Les zones declarees dans l'interface, lues sur le fichier lui-meme.
declarees <- regmatches(ui, regexpr('uiOutput\\("[a-zA-Z_]+", class = "zone-alerte"\\)', ui))
declarees <- sub('uiOutput\\("', "", sub('", class = "zone-alerte"\\)', "", declarees))

ok("l'interface declare des zones d'alerte", length(declarees) > 0)
ok("la regle CSS qui les masque existe toujours",
   any(grepl("\\.zone-alerte:empty", css)))

# LE test : la liste nommee doit couvrir EXACTEMENT ce que l'interface declare.
manquantes <- setdiff(declarees, ZONES_ALERTE)
ok(paste0("aucune zone oubliee dans ZONES_ALERTE",
          if (length(manquantes)) paste0(" (manque : ", paste(manquantes, collapse = ", "), ")") else ""),
   length(manquantes) == 0)
fantomes <- setdiff(ZONES_ALERTE, declarees)
ok(paste0("aucune zone fantome dans ZONES_ALERTE",
          if (length(fantomes)) paste0(" (en trop : ", paste(fantomes, collapse = ", "), ")") else ""),
   length(fantomes) == 0)

# Chaque zone doit avoir sa sortie, sinon outputOptions() fait tomber le
# serveur au demarrage.
sans_sortie <- Filter(function(id) !any(grepl(paste0("output\\$", id, " *<-"), srv)),
                      ZONES_ALERTE)
ok(paste0("chaque zone a sa sortie dans server.R",
          if (length(sans_sortie)) paste0(" (sans sortie : ", paste(sans_sortie, collapse = ", "), ")") else ""),
   length(sans_sortie) == 0)

# Et le reveil doit etre appele, sinon tout ce qui precede ne sert a rien.
ok("reveille_zones_alerte() est appelee dans server.R",
   any(grepl("reveille_zones_alerte\\(output\\)", srv)))
# En DERNIER : outputOptions() exige que les sorties existent deja.
ok("et elle est appelee apres la derniere sortie",
   { appel <- grep("reveille_zones_alerte\\(output\\)", srv)
     derniere <- max(grep("output\\$[a-zA-Z_]+ *<-", srv))
     tail(appel, 1) > derniere })
