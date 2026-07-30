library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
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

#### Date #####

date_debut_semaine    <- floor_date(today() - 2, unit = "week") + 1
vecteur_jours <- c("lundi","mardi","mercredi",
                   "jeudi","vendredi","samedi",
                   "dimanche")

#### Conventions de couleurs ####

# Palette d'appréciation, partagée par tous les volets de la refonte.

COUL_VERT   <- "#5B7B5A"
COUL_AMBRE  <- "#d98236"
COUL_ROUGE  <- "#c0392b"
COUL_NEUTRE <- "#8d7b68"
COUL_BRUN   <- "#732c02"

# Couleurs par secteur (déclinaison de la palette Mazette)
COULEURS_SECTEURS <- c(
  "Service"                    = "#2980b9",
  "Transformation alimentaire" = "#5B7B5A",
  "Brasserie"                  = "#d98236",
  "Support"                    = "#8d7b68"
)

COUL_MATIERE <- "#d3c0ac"   # coût matière / frais généraux
COUL_TRAVAIL <- "#732c02"   # coût du personnel
# COUL_VERT / COUL_AMBRE / COUL_ROUGE : cf. « Conventions de couleurs » en tête
# de fichier, partagées avec les barres de CA vs objectif.

##### Volet Travail ####

CRENEAUX_ORDRE <- c("Midi", "Soir", "Pizzwanze")
PAL_CRENEAU <- c("Midi" = "#e67e22", "Soir" = "#9b59b6", "Pizzwanze" = "#c0392b")

##### Volet Consommation #####

# Heures de service, dans l'ordre d'une soirée (on ouvre le matin, on ferme
# après minuit) plutôt que dans l'ordre naturel 0..23.
# Palette locale (functions.R ne dépend pas de ui.R)
CONSO_BRUN  <- "#732c02"
CONSO_AMBRE <- "#d98236"

ORDRE_HEURES_SERVICE <- c(6:23, 0:5)


#### Ingrédients #####

# Assiette de chaque ingrédient (quelles focaccias le consomment) :
#   Crémeux et Légume -> TOUTES les focaccias, ils sont dans la recette de base
#   Fromage           -> celles qui portent le supplément fromage (« full »
#                        compris, puisqu'une focaccia complète en contient)
#   Viande            -> idem avec le supplément viande
#   Autre             -> ligne libre, rien n'est préchargé
#
# NB : la caisse ne connaît pas de « supplément légume » — les seules options
# sont Fromage, Viande et Spicy. Le légume est donc traité comme le crémeux,
# c'est-à-dire présent dans toutes les recettes.

INGREDIENTS_FOCACCIA <- tibble::tribble(
  ~ID, ~NOM,       ~ASSIETTE,  ~PORTION,
  1L,  "Crémeux",  "toutes",   40,
  2L,  "Légume",   "toutes",   60,
  3L,  "Fromage",  "fromage",  30,
  4L,  "Viande",   "viande",   50,
  5L,  "Autre",    NA,         NA
)

##### Pizzwanze #####

# Seuils de détection d'une soirée (documentés pour pouvoir être ajustés).
PIZZWANZE_MIN_REFS   <- 2   # une soirée propose une carte, pas un seul produit
PIZZWANZE_MIN_PIZZAS <- 5   # garde-fou volume, pour écarter les restes


source("donnees_fictives_compta.R")
