# Trancher un mois douteux (cf. docs/maintenance.md).
#
# Le topo des mois (R/exploitation.R) juge les chiffres : un mois « à risque »
# le resterait à chaque ouverture, même vérifié. Une DÉCISION humaine recouvre
# donc le verdict calculé :
#
#   valide   chiffres vérifiés et bons  -> pastille verte, plus proposé à
#                                          l'écartement
#   rejete   chiffres vérifiés et faux  -> pastille rouge, toujours proposé
#   revoque  annule la décision         -> retour à l'état calculé
#
# Chaque décision (commentaire, auteur, date) s'ajoute en une ligne à un
# Google Sheet ; rien n'y est modifié ni supprimé, la dernière ligne d'un mois
# fait foi.
#
# Une décision enregistre le CA et le nombre de comptes qu'elle a vus, et cesse
# de s'appliquer s'ils ont bougé.

#### Le journal des décisions ####

# Les colonnes du classeur ; la première écriture pose l'en-tête.
COLONNES_VALIDATION <- c("MOIS", "DECISION", "COMMENTAIRE", "QUI", "QUAND",
                         "CA", "N_COMPTES")

DECISIONS_VALIDATION <- c("valide", "rejete", "revoque")

# De combien le CA peut bouger sans que la décision tombe.
TOLERANCE_CA_VALIDATION <- 1

validations_vides <- function()
  tibble::tibble(MOIS = as.Date(character()), DECISION = character(),
                 COMMENTAIRE = character(), QUI = character(),
                 QUAND = character(), CA = numeric(), N_COMPTES = integer())

#' Met un classeur lu en forme. Une ligne sans mois lisible ou sans décision
#' connue est écartée (et comptée par lignes_validation_perdues()).
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
#' QUAND est au format ISO, donc l'ordre alphabétique est chronologique ; à
#' QUAND égal, la ligne la plus basse gagne.
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

#' Recouvre l'état calculé de chaque mois par la décision humaine.
#'
#' @param topo sortie de topo_mois(), AVANT décisions.
#' @param validations le journal, tel que lu.
#' @return le même topo, avec DECISION, DECIDE_PAR et DECIDE_LE (toujours
#'   présentes) et, pour les mois tranchés, ETAT / A_ECARTER / POURQUOI revus.
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
      # Une décision périmée est dite, pas cachée.
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

# Les chiffres de la décision sont-ils encore ceux du mois ? Un chiffre
# absent de la décision ne la fait pas tomber.
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

#' Les mois vérifiés et jugés bons : les seuls à sortir des alertes
#' d'intégrité. Un mois rejeté y reste tant que la source n'est pas corrigée.
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

#' La ligne de journal d'une décision, avec les chiffres pris dans le topo :
#' ceux que la personne avait sous les yeux.
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

# googlesheets4 n'est pas attaché (cf. global.R) : il reprend le jeton du
# compte de service de googledrive.
auth_sheets <- function() {
  # Le test du paquet est ici : les fonctions appelantes acceptent une
  # doublure qui n'en a pas besoin.
  if (!requireNamespace("googlesheets4", quietly = TRUE))
    stop("googlesheets4 n'est pas installé : classeur de validation hors d'atteinte.")
  auth_drive()
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  invisible(TRUE)
}

# Les deux accès au classeur, remplaçables pour les tests.
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

# Des dates en texte : une Date brute deviendrait un nombre de jours.
pour_le_classeur <- function(df) {
  df <- normalise_validations(df)
  df$MOIS <- format(df$MOIS, "%Y-%m-%d")
  df
}

#' Lit le journal. Rend un tableau vide, jamais une erreur.
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

#' Ajoute une ligne au journal. Lève si l'écriture échoue : la décision ne
#' doit s'afficher qu'une fois enregistrée.
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
    # Classeur vide ou sans en-tête : on réécrit ce qui était lisible avec la
    # nouvelle ligne.
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

#' Les mois qu'on peut trancher : tous sauf les plausibles et le mois en
#' cours. Les mois déjà tranchés restent, pour pouvoir changer d'avis.
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

#' Ce qui est déjà décidé pour ce mois, ou NULL.
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
    # Libellés courts ; la conséquence de chaque choix est écrite dessous.
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
