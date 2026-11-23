library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(shinybusy)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(lubridate)
library(googledrive)
library(readxl)
library(scales)
library(plotly)
library(forecast)
library(DT)
library(zoo)
library(patchwork)

file_names <- list.files(path = "R", pattern = "\\.R$")
invisible(lapply(file.path("R",file_names), source))

# Les constantes ci-dessous sont définies APRÈS le chargement de R/ : aucune
# constante de premier niveau de R/ ne peut donc s'en servir (cf. R/README.md).

#### Dates, en français quelle que soit la locale du serveur ####

date_debut_semaine <- floor_date(today() - 2, unit = "week") + 1
vecteur_jours <- c("lundi","mardi","mercredi",
                   "jeudi","vendredi","samedi",
                   "dimanche")
vecteur_mois_court <- c("janv.","févr.","mars","avr.","mai","juin",
                        "juil.","août","sept.","oct.","nov.","déc.")

#### Couleurs ####

# Appréciation : vert bon, ambre à surveiller, rouge mauvais.
COUL_VERT   <- "#5B7B5A"
COUL_AMBRE  <- "#d98236"
COUL_ROUGE  <- "#c0392b"
COUL_NEUTRE <- "#8d7b68"
COUL_BRUN   <- "#732c02"

COULEURS_SECTEURS <- c(
  "Service"                    = "#2980b9",
  "Transformation alimentaire" = "#5B7B5A",
  "Brasserie"                  = "#d98236",
  "Support"                    = "#8d7b68"
)

COUL_MATIERE <- "#d3c0ac"   # coût matière / frais généraux
COUL_TRAVAIL <- "#732c02"   # coût du personnel

#### Travail ####

CRENEAUX_ORDRE <- c("Midi", "Soir", "Pizzwanze")
PAL_CRENEAU <- c("Midi" = "#e67e22", "Soir" = "#9b59b6", "Pizzwanze" = "#c0392b")

#### Consommation ####

# Heures de service dans l'ordre d'une journée (ouverture le matin, fermeture
# après minuit).
ORDRE_HEURES_SERVICE <- c(6:23, 0:5)

#### Focaccias : assiette de chaque ingrédient ####

#   Crémeux, Légume -> toutes les focaccias (recette de base ; la caisse ne
#                      connaît pas de supplément légume)
#   Fromage, Viande -> celles qui portent ce supplément (« full » compris)
#   Autre           -> ligne libre

INGREDIENTS_FOCACCIA <- tibble::tribble(
  ~ID, ~NOM,       ~ASSIETTE,  ~PORTION,
  1L,  "Crémeux",  "toutes",   40,
  2L,  "Légume",   "toutes",   60,
  3L,  "Fromage",  "fromage",  30,
  4L,  "Viande",   "viande",   50,
  5L,  "Autre",    NA,         NA
)

#### Pizzwanze : détection d'une soirée ####

PIZZWANZE_MIN_REFS   <- 2   # une soirée propose une carte, pas un seul produit
PIZZWANZE_MIN_PIZZAS <- 5   # garde-fou volume, pour écarter les restes
