# La date des chiffres qu'on regarde, affichée en permanence.
#
# ---------------------------------------------------------------------------
# Pourquoi ce fichier existe
# ---------------------------------------------------------------------------
# Le dashboard ne reconstruit plus les données au démarrage : il prend le
# dernier cache disponible, quelle que soit sa date (cf. R/connect.R). C'est ce
# qui lui permet de s'ouvrir en quelques secondes au lieu de quelques minutes.
#
# Mais c'est aussi une promesse rompue : jusqu'ici, « le dashboard est ouvert »
# valait « les données sont d'hier ». Ce n'est plus vrai, et personne ne peut
# le deviner en regardant un graphique — un CA de mardi ressemble exactement à
# un CA de mardi, qu'il ait été lu cette nuit ou il y a cinq jours.
#
# Le 21 octobre a montré le prix de ce silence : l'import échouait chaque
# matin, et le dashboard affichait sans broncher des chiffres qui ne bougeaient
# plus. Un volet entier (Contrôle) sert à vérifier la comptabilité ; il n'y
# avait rien pour vérifier la fraîcheur de l'ensemble.
#
# Ce fichier est cette vérification, et elle est affichée là où on ne peut pas
# la manquer : dans la barre de navigation, à côté du nom de l'utilisateur.

# Le cache de la nuit porte la date de la VEILLE : c'est le dernier jour que
# DB_JOURS connaisse. Un cache d'hier est donc un cache à jour, et le retard
# se compte à partir de là — d'où le « - 1 » dans etat_fraicheur().
RETARD_TARD  <- 1   # l'import de cette nuit n'a pas tourné : ça arrive
RETARD_VIEUX <- 3   # trois matins de suite : ce n'est plus un accident

#' L'âge des données, et ce qu'on en pense.
#'
#' @param date_donnees la date portée par le cache chargé (cf. dernier_rds()).
#' @return une liste : `retard` en jours (0 = à jour), `verdict`
#'   (ok / tard / vieux / inconnu), `libelle` et `couleur`.
#'
#' Le verdict ne juge pas la brasserie, il juge la CHAÎNE D'IMPORT. « vieux »
#' ne veut pas dire « mauvais chiffres », il veut dire « personne n'a lu les
#' sources depuis trois jours » — et donc : va voir pourquoi.
etat_fraicheur <- function(date_donnees, aujourd_hui = today()) {
  if (is.null(date_donnees) || length(date_donnees) != 1 || is.na(date_donnees))
    return(list(retard = NA_integer_, verdict = "inconnu",
                libelle = "date inconnue", couleur = COUL_NEUTRE))

  retard <- as.integer(as.Date(aujourd_hui) - as.Date(date_donnees)) - 1L
  # Un cache daté d'aujourd'hui ou de demain (horloge décalée, import lancé à
  # la main après la fermeture) n'est pas « en avance » : il est à jour.
  retard <- max(retard, 0L)

  verdict <- if (retard < RETARD_TARD)       "ok"
             else if (retard <= RETARD_VIEUX) "tard"
             else                             "vieux"

  list(retard  = retard,
       verdict = verdict,
       libelle = if (retard == 0) "à jour"
                 else paste(retard, if (retard == 1) "jour de retard"
                                    else "jours de retard"),
       couleur = switch(verdict, ok = COUL_VERT, tard = COUL_AMBRE,
                        vieux = COUL_ROUGE, COUL_NEUTRE))
}

#' La date, en français, indépendamment de la locale du serveur.
#'
#' format(x, "%d %b") suit la locale du SERVEUR : un serveur en locale C rend
#' « Nov » là où on attend « nov. ». Le dashboard passe donc partout par
#' vecteur_mois_court (cf. global.R).
date_fr <- function(d) {
  d <- as.Date(d)
  if (length(d) != 1 || is.na(d)) return("—")
  paste(day(d), vecteur_mois_court[month(d)], year(d))
}

#### La pastille de la barre de navigation ####

# La barre de navigation est brune (#732c02). La palette d'appréciation de
# global.R est faite pour un fond crème : COUL_VERT y descend à 1,6:1 de
# contraste, COUL_ROUGE à 1,2:1 — illisibles. D'où ces trois tons, choisis
# pour CE fond, et le beige pour « rien à signaler », comme le reste de la
# barre.
COUL_BARRE <- c(ok      = "#d3c0ac",
                tard    = "#f0a85c",
                vieux   = "#ff9b8f",
                inconnu = "#d3c0ac")

coul_barre <- function(verdict) {
  c <- COUL_BARRE[[verdict]]
  if (is.null(c)) "#d3c0ac" else c
}

#' Le lien cliquable qui porte la date des données.
#'
#' `a_recharger` : une autre session a mis les données à jour pendant que
#' celle-ci était ouverte. Ses réactifs ont été calculés sur les tables
#' d'AVANT et ne se recalculeront pas tout seuls — la pastille le dit, et le
#' dialogue propose le rechargement.
badge_fraicheur <- function(etat, date_donnees, a_recharger = FALSE) {
  if (isTRUE(a_recharger))
    return(actionLink(
      "ouvrir_fraicheur", class = "badge-fraicheur",
      title = "De nouvelles données ont été importées",
      style = paste0("color:", coul_barre("tard"), ";"),
      label = tagList(icon("rotate"),
                      tags$span(class = "d-none d-lg-inline ms-1",
                                "nouvelles données"))))

  actionLink(
    "ouvrir_fraicheur", class = "badge-fraicheur",
    title = paste0("Données du ", date_fr(date_donnees), " (", etat$libelle,
                   ") — cliquer pour mettre à jour"),
    style = paste0("color:", coul_barre(etat$verdict), ";"),
    label = tagList(
      icon(switch(etat$verdict, ok = "circle-check", tard = "clock",
                  vieux = "triangle-exclamation", "circle-question")),
      tags$span(class = "d-none d-lg-inline ms-1", date_fr(date_donnees))))
}

#### Le dialogue de mise à jour ####

#' Ce que propose la pastille quand on clique dessus.
#'
#' Deux gestes, et ils ne coûtent pas la même chose — c'est pour ça qu'ils ne
#' sont pas un seul bouton « rafraîchir » :
#'
#'   reprendre le dernier import  quelques secondes. Le cache de Drive est
#'                                plus frais que celui que cette instance a
#'                                chargé — ce qui est le cas tous les matins,
#'                                puisque le script de la nuit l'y dépose.
#'
#'   refaire l'import            quelques minutes, et ça relit TOUTES les
#'                                sources. Réservé aux associés : c'est le
#'                                geste à faire quand une correction vient
#'                                d'être saisie dans un classeur.
dialogue_fraicheur <- function(etat, date_donnees, peut_importer = FALSE) {
  # Chaque geste porte son coût sous lui, et prend toute la largeur. Trois
  # boutons alignés dans le pied de page se ressemblaient trop : rien ne
  # disait que l'un prend trois secondes et l'autre trois minutes.
  geste <- function(id, titre, classe, explication)
    div(class = "mb-3",
        actionButton(id, titre, class = paste(classe, "w-100")),
        p(class = "text-muted small mt-1 mb-0", explication))

  modalDialog(
    title = "Fraîcheur des données",
    size = "m", easyClose = TRUE,
    div(
      p(strong("Dernier jour connu : "), date_fr(date_donnees),
        tags$span(style = paste0("color:", etat$couleur, ";"),
                  paste0(" (", etat$libelle, ")"))),

      if (identical(etat$verdict, "vieux"))
        div(class = "alert alert-warning py-2",
            "L'import ne tourne plus depuis plusieurs jours. Refaire l'import ",
            "donnera les chiffres du jour, mais la cause reste à chercher du ",
            "côté du script planifié."),

      geste("recharger_donnees", "Reprendre le dernier import", "btn-secondary",
            paste("Quelques secondes. Ne relit pas les classeurs : récupère le",
                  "fichier déposé par l'import de la nuit.")),

      if (peut_importer)
        geste("import_complet", "Refaire l'import", "btn-primary",
              paste("Quelques minutes. Relit toutes les sources — Drive, les",
                    "classeurs, Odoo. À faire après une correction dans un",
                    "classeur.")),

      p(class = "text-muted small fst-italic mb-0",
        "Dans les deux cas, l'écran se recharge à la fin : il faudra ",
        "ressaisir le mot de passe.")
    ),
    footer = modalButton("Fermer")
  )
}

#' Le dialogue de suivi, pendant qu'un import tourne ailleurs.
dialogue_suivi_import <- function() {
  modalDialog(
    title = "Import en cours",
    size = "m", easyClose = FALSE,
    div(
      p(class = "text-muted small",
        "L'import tourne sur le serveur d'intégration, pas ici : le dashboard ",
        "reste utilisable pendant ce temps, avec les chiffres d'avant."),
      uiOutput("suivi_import")
    ),
    footer = modalButton("Fermer")
  )
}
