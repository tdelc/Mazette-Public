# Trancher un mois douteux, et que ça se sache.
#
# ---------------------------------------------------------------------------
# Le problème
# ---------------------------------------------------------------------------
# Le topo des mois (R/exploitation.R) juge les chiffres, et il ne sait juger
# qu'eux : un mois dont la rémunération a chuté de 60 % est « à risque », et il
# le restera à chaque ouverture du dashboard. Même après qu'on a ouvert Odoo,
# regardé, compris que c'était un décalage de paie, et conclu que les chiffres
# sont bons.
#
# Le verdict ne retient donc rien du travail de vérification. L'alerte reste
# rouge pour toujours, et une alerte qui ne s'éteint jamais finit par ne plus
# rien vouloir dire — c'est la pire chose qui puisse arriver à un contrôle.
#
# ---------------------------------------------------------------------------
# Ce que ce fichier ajoute
# ---------------------------------------------------------------------------
# Une DÉCISION humaine, qui recouvre le verdict calculé :
#
#   valide   j'ai vérifié, les chiffres sont bons  -> pastille verte, le mois
#                                                     n'est plus proposé à
#                                                     l'écartement
#   rejete   j'ai vérifié, les chiffres sont faux  -> pastille rouge, le mois
#                                                     est TOUJOURS proposé à
#                                                     l'écartement, même vieux
#   revoque  j'annule ma décision                  -> le mois revient à son
#                                                     état calculé
#
# Chaque décision porte un commentaire, un auteur et une date, et part en une
# LIGNE DE PLUS dans un Google Sheet. Rien n'est jamais modifié ni supprimé :
# pour changer d'avis, on ajoute une ligne. Le classeur est donc un journal, et
# la décision qui compte pour un mois est simplement la dernière le concernant.
#
# ---------------------------------------------------------------------------
# Une décision porte sur des CHIFFRES, pas sur un mois
# ---------------------------------------------------------------------------
# « Octobre est bon » ne veut rien dire en soi : ce qui a été vérifié, c'est un
# CA et un nombre de comptes, à une date. Si l'export Odoo du lendemain apporte
# quinze écritures de plus, le mois validé n'est plus celui qu'on a regardé.
#
# La décision enregistre donc les deux chiffres qu'elle a vus, et cesse de
# s'appliquer s'ils ont bougé. Le mois retrouve alors son état calculé, et le
# tableau dit pourquoi. C'est la seule façon qu'une validation ne devienne pas,
# avec le temps, un tapis sous lequel balayer.

#### Le journal des décisions ####

# Les colonnes du classeur, dans cet ordre. Il est vide au départ : la première
# écriture pose l'en-tête (cf. ecrit_validation).
COLONNES_VALIDATION <- c("MOIS", "DECISION", "COMMENTAIRE", "QUI", "QUAND",
                         "CA", "N_COMPTES")

DECISIONS_VALIDATION <- c("valide", "rejete", "revoque")

# De combien le CA peut bouger sans que la décision tombe. Un euro : on cherche
# des écritures ajoutées, pas du bruit d'arrondi.
TOLERANCE_CA_VALIDATION <- 1

validations_vides <- function()
  tibble::tibble(MOIS = as.Date(character()), DECISION = character(),
                 COMMENTAIRE = character(), QUI = character(),
                 QUAND = character(), CA = numeric(), N_COMPTES = integer())

#' Met un classeur lu en forme, quoi qu'il contienne.
#'
#' Tolérant par construction : le classeur est modifiable à la main, et une
#' ligne abîmée ne doit pas emporter les autres. Une ligne sans mois lisible ou
#' sans décision connue est écartée — et lit_validations() le dit à l'import,
#' plutôt que de la perdre en silence.
normalise_validations <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(validations_vides())
  manque <- setdiff(COLONNES_VALIDATION, names(df))
  for (m in manque) df[[m]] <- NA
  df <- df[, COLONNES_VALIDATION, drop = FALSE]

  res <- tibble::tibble(
    MOIS        = lubridate::floor_date(parse_date_souple(df$MOIS), "month"),
    DECISION    = tolower(trimws(as.character(df$DECISION))),
    COMMENTAIRE = trimws(as.character(df$COMMENTAIRE)),
    QUI         = trimws(as.character(df$QUI)),
    QUAND       = trimws(as.character(df$QUAND)),
    CA          = suppressWarnings(as.numeric(df$CA)),
    N_COMPTES   = suppressWarnings(as.integer(df$N_COMPTES)))
  res$COMMENTAIRE[is.na(res$COMMENTAIRE)] <- ""
  res[!is.na(res$MOIS) & res$DECISION %in% DECISIONS_VALIDATION, , drop = FALSE]
}

#' Combien de lignes le classeur portait qui ne sont pas exploitables.
lignes_validation_perdues <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(0L)
  max(0L, nrow(df) - nrow(normalise_validations(df)))
}

#' La décision qui compte pour chaque mois : la dernière, hors révocations.
#'
#' Le classeur est un journal, donc l'ordre fait foi. QUAND est écrit au
#' format ISO (« 2026-11-08 10:12:33 ») précisément pour que l'ordre
#' alphabétique soit l'ordre chronologique — et, à QUAND égal, c'est la ligne
#' la plus basse du classeur qui gagne.
derniere_decision <- function(validations) {
  v <- normalise_validations(validations)
  if (!nrow(v)) return(v)
  v$RANG_ <- seq_len(nrow(v))
  v <- v[order(v$MOIS, v$QUAND, v$RANG_), , drop = FALSE]
  derniere <- !duplicated(v$MOIS, fromLast = TRUE)
  v <- v[derniere, setdiff(names(v), "RANG_"), drop = FALSE]
  v[v$DECISION != "revoque", , drop = FALSE]
}

#### Appliquer les décisions au topo ####

#' Recouvre l'état calculé de chaque mois par la décision humaine, s'il y en a.
#'
#' @param topo sortie de topo_mois(), AVANT décisions.
#' @param validations le journal, tel que lu.
#' @return le même topo, avec trois colonnes de plus (DECISION, DECIDE_PAR,
#'   DECIDE_LE) et, pour les mois tranchés, ETAT / A_ECARTER / POURQUOI revus.
#'
#' Les colonnes sont ajoutées même sans aucune décision : un volet en aval ne
#' doit pas avoir à tester leur existence avant de les lire.
applique_validations <- function(topo, validations = NULL,
                                 tolerance_ca = TOLERANCE_CA_VALIDATION) {
  if (is.null(topo) || !nrow(topo)) {
    if (is.data.frame(topo)) {
      topo$DECISION   <- character()
      topo$DECIDE_PAR <- character()
      topo$DECIDE_LE  <- as.Date(character())
    }
    return(topo)
  }
  topo$DECISION   <- NA_character_
  topo$DECIDE_PAR <- NA_character_
  topo$DECIDE_LE  <- as.Date(NA)

  d <- derniere_decision(validations)
  if (!nrow(d)) return(topo)

  for (i in seq_len(nrow(topo))) {
    k <- match(topo$PERIODE[i], d$MOIS)
    if (is.na(k)) next
    quand <- substr(d$QUAND[k], 1, 10)
    le    <- suppressWarnings(as.Date(quand))

    if (chiffres_ont_bouge(topo[i, ], d[k, ], tolerance_ca)) {
      # La décision ne porte plus. On ne la cache pas pour autant : la dire
      # périmée vaut mieux que de la faire disparaître, sinon on croit n'avoir
      # jamais vérifié ce mois.
      topo$POURQUOI[i] <- ajoute_motif(
        topo$POURQUOI[i],
        paste0("la décision du ", if (is.na(le)) "?" else date_fr(le),
               " ne porte plus : les chiffres ont changé depuis"))
      next
    }

    topo$DECISION[i]   <- d$DECISION[k]
    topo$DECIDE_PAR[i] <- d$QUI[k]
    topo$DECIDE_LE[i]  <- le
    topo$ETAT[i]       <- if (d$DECISION[k] == "valide") "valide" else "rejete"
    topo$A_ECARTER[i]  <- d$DECISION[k] == "rejete"
    topo$POURQUOI[i]   <- motif_decision(d[k, ], le)
  }

  ref <- etats_topo()
  topo$RANG <- ref$RANG[match(topo$ETAT, ref$ETAT)]
  topo
}

# Les chiffres regardés au moment de la décision sont-ils encore ceux-là ?
#
# Un chiffre absent de la décision (classeur rempli à la main, colonne vide)
# ne fait pas tomber la validation : on ne peut pas comparer, on ne conclut
# donc rien. C'est le sens le moins désagréable des deux — l'autre révoquerait
# en silence des décisions parfaitement valables.
chiffres_ont_bouge <- function(ligne_topo, decision, tolerance_ca) {
  ca <- !is.na(decision$CA) && !is.na(ligne_topo$CA) &&
        abs(ligne_topo$CA - decision$CA) > tolerance_ca
  nc <- !is.na(decision$N_COMPTES) && !is.na(ligne_topo$N_COMPTES) &&
        ligne_topo$N_COMPTES != decision$N_COMPTES
  isTRUE(ca) || isTRUE(nc)
}

ajoute_motif <- function(ancien, ajout) {
  if (is.na(ancien) || !nzchar(ancien)) ajout else paste0(ancien, " ; ", ajout)
}

motif_decision <- function(decision, le) {
  qui <- if (is.na(decision$QUI) || !nzchar(decision$QUI)) "" else
           paste0(" par ", decision$QUI)
  quand <- if (is.na(le)) "" else paste0(" le ", date_fr(le))
  verbe <- if (decision$DECISION == "valide") "Validé" else "Écarté"
  com <- if (is.na(decision$COMMENTAIRE) || !nzchar(decision$COMMENTAIRE)) ""
         else paste0(" — ", decision$COMMENTAIRE)
  paste0(verbe, qui, quand, com)
}

#' Les mois dont les chiffres ont été vérifiés et jugés bons.
#'
#' Ce sont les seuls à sortir des alertes d'intégrité : un mois REJETÉ, lui,
#' porte bien des chiffres improbables — c'est confirmé, pas résolu — et doit
#' continuer de se voir tant que la source n'est pas corrigée.
mois_valides <- function(topo) {
  if (is.null(topo) || !nrow(topo) || !"ETAT" %in% names(topo))
    return(as.Date(character()))
  topo$PERIODE[topo$ETAT == "valide"]
}

#' Retire des périodes improbables celles qu'on a vérifiées et validées.
improb_hors_valides <- function(improb, topo) {
  valides <- mois_valides(topo)
  if (is.null(improb) || !nrow(improb) || !length(valides)) return(improb)
  improb[!improb$PERIODE %in% valides, , drop = FALSE]
}

#### La ligne qu'on ajoute au classeur ####

#' Fabrique la ligne de journal correspondant à une décision.
#'
#' Les chiffres sont pris dans le topo, et non saisis : ce sont EXACTEMENT
#' ceux que la personne avait sous les yeux en décidant.
ligne_validation <- function(mois, decision, commentaire, qui, topo,
                             quand = Sys.time()) {
  mois <- lubridate::floor_date(as.Date(mois), "month")
  i <- match(mois, topo$PERIODE)
  tibble::tibble(
    MOIS        = mois,
    DECISION    = decision,
    COMMENTAIRE = trimws(as.character(commentaire)),
    QUI         = if (is.null(qui) || !nzchar(qui)) "—" else as.character(qui),
    QUAND       = format(quand, "%Y-%m-%d %H:%M:%S"),
    CA          = if (is.na(i)) NA_real_    else as.numeric(topo$CA[i]),
    N_COMPTES   = if (is.na(i)) NA_integer_ else as.integer(topo$N_COMPTES[i]))
}

#### Le classeur ####

chemin_validation <- function() Sys.getenv("PATH_VALIDATION_COMPTA")

# googlesheets4 n'est pas attaché (cf. global.R) : il partage le jeton du
# compte de service déjà obtenu par googledrive, comme le fait l'historique du
# planning dans import.R.
auth_sheets <- function() {
  # Le garde-fou est ICI, et pas dans les fonctions qui s'en servent : elles
  # acceptent toutes une doublure d'accès, et une doublure n'a besoin
  # d'aucun paquet. Un test qui échouerait faute de googlesheets4
  # n'éprouverait plus rien de ce qu'il croit éprouver.
  if (!requireNamespace("googlesheets4", quietly = TRUE))
    stop("googlesheets4 n'est pas installé : classeur de validation hors d'atteinte.")
  auth_drive()
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  invisible(TRUE)
}

# Les deux accès au classeur, isolés pour que tout le reste se teste sans
# réseau. La suite de tests passe une doublure à leur place.
io_validation <- function() list(
  lire = function(ss) {
    auth_sheets()
    googlesheets4::read_sheet(googlesheets4::as_sheets_id(ss), sheet = 1,
                              col_types = "c")
  },
  ecrire = function(ss, df) {
    auth_sheets()
    googlesheets4::write_sheet(df, ss = googlesheets4::as_sheets_id(ss),
                               sheet = 1)
  },
  ajouter = function(ss, df) {
    auth_sheets()
    googlesheets4::sheet_append(googlesheets4::as_sheets_id(ss), df, sheet = 1)
  })

# Ce qui part dans le classeur : des dates et des nombres, pas des objets R.
# Une Date écrite telle quelle revient en nombre de jours depuis 1970 dans une
# cellule, et plus personne ne sait la lire à l'œil.
pour_le_classeur <- function(df) {
  df <- normalise_validations(df)
  df$MOIS <- format(df$MOIS, "%Y-%m-%d")
  df
}

#' Lit le journal. Rend un tableau vide, jamais une erreur.
#'
#' Au premier passage le classeur est vide : il n'y a rien à lire, et c'est
#' normal. L'import ne doit pas tomber pour ça, et le dashboard doit s'ouvrir
#' sans journal comme il s'ouvrait avant qu'il existe.
lit_validations <- function(ss = chemin_validation(), io = io_validation()) {
  if (!nzchar(ss)) {
    cli::cli_alert_warning(
      "{.envvar PATH_VALIDATION_COMPTA} absent : aucune validation de mois")
    return(validations_vides())
  }
  lu <- try(io$lire(ss), silent = TRUE)
  if (inherits(lu, "try-error")) {
    cli::cli_alert_warning("Validations de mois illisibles : {as.character(lu)}")
    return(validations_vides())
  }
  perdues <- lignes_validation_perdues(lu)
  if (perdues)
    cli::cli_alert_warning(
      "Validations : {perdues} ligne(s) sans mois lisible ou sans décision connue")
  v <- normalise_validations(lu)
  cli::cli_alert_success(
    "Validations : {nrow(v)} ligne(s), {nrow(derniere_decision(v))} mois tranché(s)")
  v
}

#' Ajoute une ligne au journal. Lève si l'écriture échoue — à dessein.
#'
#' L'appelant ne doit mettre la décision à l'écran qu'une fois le classeur
#' écrit. Une pastille verte sur une décision qui n'a pas été enregistrée est
#' le pire des deux mondes : elle disparaît au prochain import, et on ne sait
#' pas pourquoi.
ecrit_validation <- function(ligne, ss = chemin_validation(),
                             io = io_validation()) {
  if (!nzchar(ss))
    stop("PATH_VALIDATION_COMPTA n'est pas renseigné : impossible d'enregistrer.")
  existant <- try(io$lire(ss), silent = TRUE)
  entete_pose <- !inherits(existant, "try-error") &&
                 is.data.frame(existant) &&
                 all(COLONNES_VALIDATION %in% names(existant))

  if (entete_pose) {
    io$ajouter(ss, pour_le_classeur(ligne))
  } else {
    # Classeur encore vide, ou en-tête absent : la première écriture pose les
    # colonnes. On réécrit ce qui était lisible AVEC la nouvelle ligne, pour
    # ne rien perdre d'un classeur qu'on aurait commencé à la main.
    ancien <- if (inherits(existant, "try-error")) NULL else existant
    io$ecrire(ss, pour_le_classeur(dplyr::bind_rows(
      normalise_validations(ancien), normalise_validations(ligne))))
  }
  invisible(TRUE)
}

#' Le journal tel que le processus le connaît au démarrage.
validations_chargees <- function(envir = globalenv()) {
  if (exists("DB_VALIDATION_COMPTA", envir = envir, inherits = FALSE))
    normalise_validations(get("DB_VALIDATION_COMPTA", envir = envir))
  else validations_vides()
}

#### Le dialogue ####

#' Les mois qu'on peut trancher, du plus récent au plus ancien.
#'
#' Tout sauf les mois plausibles — et sauf le mois EN COURS, qui est partiel
#' par construction : le valider n'aurait aucun sens aujourd'hui, et la
#' décision tomberait demain de toute façon, puisque ses chiffres auront bougé.
#' Les mois déjà tranchés restent dans la liste : on doit pouvoir changer
#' d'avis.
mois_a_trancher <- function(topo) {
  if (is.null(topo) || !nrow(topo)) return(character(0))
  gardes <- !topo$ETAT %in% c("ok", "en_cours")
  if (!any(gardes)) return(character(0))
  t <- topo[gardes, , drop = FALSE]
  t <- t[order(t$PERIODE, decreasing = TRUE), , drop = FALSE]
  ref <- etats_topo()
  libelles <- paste0(etiquette_periode(t$PERIODE, "mois"), " — ",
                     ref$LIBELLE[match(t$ETAT, ref$ETAT)])
  stats::setNames(as.character(t$PERIODE), libelles)
}

#' Ce qui est déjà décidé pour ce mois, à rappeler avant d'en décider autre
#' chose. NULL s'il n'y a rien.
apercu_decision <- function(validations, mois) {
  if (is.null(mois) || !length(mois) || !nzchar(as.character(mois))) return(NULL)
  d <- derniere_decision(validations)
  k <- match(lubridate::floor_date(as.Date(mois), "month"), d$MOIS)
  if (is.na(k)) return(NULL)
  div(class = "alert alert-secondary py-2 small",
      tags$b("Décision actuelle : "),
      motif_decision(d[k, ], suppressWarnings(as.Date(substr(d$QUAND[k], 1, 10)))),
      tags$br(),
      tags$span(class = "text-muted",
                "Enregistrer en ajoutera une nouvelle, qui la remplacera."))
}

#' Le dialogue de décision.
dialogue_validation <- function(topo, validations = NULL) {
  choix <- mois_a_trancher(topo)
  if (!length(choix))
    return(modalDialog(
      title = "Trancher un mois",
      p("Aucun mois ne demande de décision : tous les mois clos sont ",
        "plausibles, et le mois en cours ne se tranche pas tant qu'il court."),
      footer = modalButton("Fermer")))

  modalDialog(
    title = "Trancher un mois douteux",
    size = "m", easyClose = FALSE,
    selectInput("valid_mois", "Mois à trancher", choices = choix,
                selectize = FALSE),
    uiOutput("valid_actuel"),
    # Libellés courts : la conséquence de chaque choix tient sous le groupe,
    # plutôt que dans une étiquette de radio qui se coupe en deux lignes mal
    # alignées dès qu'elle dépasse la largeur du dialogue.
    radioButtons(
      "valid_decision", "Après vérification",
      choiceNames  = list("Les chiffres sont bons",
                          "Les chiffres sont faux",
                          "Annuler la décision précédente"),
      choiceValues = list("valide", "rejete", "revoque"),
      selected = "valide"),
    p(class = "text-muted small",
      "Bons : le mois sort des alertes. Faux : il reste proposé à ",
      "l'écartement, même avec le temps."),
    textAreaInput("valid_commentaire", "Ce que vous avez vérifié", rows = 2,
                  placeholder = "ex. : décalage de paie, tout est passé en novembre"),
    p(class = "text-muted small mb-0",
      "La décision part en une ligne de plus dans le classeur, avec votre nom ",
      "et la date — rien n'est jamais effacé. Elle enregistre aussi le CA et ",
      "le nombre de comptes d'aujourd'hui : si l'export en apporte d'autres, ",
      "elle cessera de s'appliquer, ce ne serait plus le mois que vous avez ",
      "regardé."),
    footer = tagList(
      modalButton("Annuler"),
      actionButton("valid_enregistrer", "Enregistrer", class = "btn-primary"))
  )
}
