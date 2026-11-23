# Le grand livre : une ligne par écriture, et de quoi y chercher. DB_COMPTA
# n'est qu'un agrégat mensuel ; les écritures portent la date, la pièce et le
# partenaire.
#
# Deux lectures d'un même montant, séparées et nommées :
#
#   SOLDE  = DÉBIT − CRÉDIT : la lecture comptable, celle d'Odoo (tableau).
#   VALEUR = SENS_G × (CRÉDIT − DÉBIT) : la lecture de gestion, celle de
#            DB_COMPTA, positive quand le poste grossit (graphes).

#### Enrichissement ####

#' Ajoute à des écritures ce que le plan comptable en sait.
enrichit_ecritures <- function(ecr) {
  if (is.null(ecr) || !nrow(ecr)) return(NULL)
  classe_comptes(ecr) %>%
    mutate(
      # Le vocabulaire de gestion des autres onglets (« Matières premières »).
      GESTION = libelle_bloc(bloc_gestion(POSTE, SECTION_G)),
      SOLDE  = DEBIT - CREDIT,
      # Un compte de bilan n'a pas de valeur de gestion : NA.
      VALEUR = ifelse(is.na(SENS_G), NA_real_, SENS_G * (CREDIT - DEBIT)),
      CLASSE = substr(as.character(COMPTE), 1, 1),
      BLOC   = ifelse(CLASSE %in% c("6", "7"), "Résultat", "Bilan"))
}

#### Filtres ####

# Les périmètres proposés, « Tout » par défaut : le seul où le contrôle
# débit = crédit a un sens, puisqu'il ne coupe aucune pièce.
BLOCS_ECRITURES <- c("Tout"                      = "Tout",
                     "Résultat (classes 6 et 7)" = "Résultat",
                     "Bilan (classes 1 à 5)"     = "Bilan")

#' Recherche sans casse, en texte littéral (fixed = TRUE).
contient_texte <- function(cols, texte) {
  if (is.null(texte) || !nzchar(trimws(texte)))
    return(rep(TRUE, nrow(cols)))
  cible <- tolower(trimws(texte))
  Reduce(`|`, lapply(cols, function(x)
    grepl(cible, tolower(replace_na(as.character(x), "")), fixed = TRUE)))
}

#' Le filtre unique : il pilote les tuiles, le récapitulatif et le tableau.
filtre_ecritures <- function(e, du = NULL, au = NULL, bloc = "Tout",
                             journaux = NULL, postes = NULL, comptes = NULL,
                             texte = NULL) {
  if (is.null(e) || !nrow(e)) return(e)
  g <- e
  if (!is.null(du))  g <- g[g$DATE >= as.Date(du), , drop = FALSE]
  if (!is.null(au))  g <- g[g$DATE <= as.Date(au), , drop = FALSE]
  if (!is.null(bloc) && !identical(bloc, "Tout"))
    g <- g[g$BLOC == bloc, , drop = FALSE]
  if (length(journaux)) g <- g[g$JOURNAL %in% journaux, , drop = FALSE]
  if (length(postes))   g <- g[g$GESTION %in% postes, , drop = FALSE]
  if (length(comptes))  g <- g[g$COMPTE  %in% comptes, , drop = FALSE]
  if (!is.null(texte) && nzchar(trimws(texte)))
    g <- g[contient_texte(g[, c("TEXTE", "PARTENAIRE", "PIECE", "LIBELLE",
                                "COMPTE")], texte), , drop = FALSE]
  g
}

#### Ce qu'on en dit ####

#' Les quatre nombres du haut. L'écart débit/crédit est un contrôle : nul sur
#' une sélection entière, normal sur une sélection filtrée (d'où l'ambre).
kpi_ecritures <- function(g) {
  if (is.null(g) || !nrow(g))
    return(div(class = "small text-muted mb-2", "Aucune écriture."))
  ecart <- sum(g$DEBIT) - sum(g$CREDIT)
  equilibre <- abs(ecart) < 0.01
  bloc <- function(n, mot, coul)
    tagList(tags$b(style = paste0("color:", coul, ";font-size:1.05rem"), n),
            " ", mot)
  div(
    class = "small text-muted mb-2 d-flex gap-3 flex-wrap align-items-baseline",
    bloc(format(nrow(g), big.mark = "."),
         if (nrow(g) > 1) "écritures" else "écriture", COUL_BRUN),
    tags$span("·"),
    bloc(length(unique(g$PIECE)), "pièces", COUL_NEUTRE),
    tags$span("·"),
    bloc(euro_compta_total(sum(g$DEBIT)), "au débit", COUL_NEUTRE),
    tags$span("·"),
    bloc(euro_compta_total(sum(g$CREDIT)), "au crédit", COUL_NEUTRE),
    tags$span("·"),
    if (equilibre) bloc("équilibré", "", COUL_VERT)
    else bloc(euro_compta_total(ecart), "d'écart", COUL_AMBRE))
}

#' Le récapitulatif par poste de la sélection : une pastille par poste, et
#' chaque pastille filtre le tableau.
recap_postes <- function(g) {
  if (is.null(g) || !nrow(g)) return(NULL)
  g %>%
    group_by(GESTION) %>%
    summarise(SOLDE = sum(DEBIT) - sum(CREDIT), N = n(), .groups = "drop") %>%
    arrange(desc(abs(SOLDE)))
}

# La liste fermée des postes de gestion : les pastilles sont des boutons à
# identifiant stable, créés une fois pour toutes.
postes_gestion <- function()
  c(unname(unique(LIBELLES_BLOCS)), HORS_GESTION)

id_chip_poste <- function(i) paste0("ecr_chip_", i)

#' La ligne de pastilles cliquables.
chips_postes <- function(recap, actifs = NULL) {
  if (is.null(recap) || !nrow(recap)) return(NULL)
  ref <- postes_gestion()
  div(
    class = "d-flex gap-2 flex-wrap align-items-center mb-2",
    tags$span(class = "small text-muted", "Par poste :"),
    lapply(seq_len(nrow(recap)), function(k) {
      ps <- recap$GESTION[k]
      i <- match(ps, ref)
      if (is.na(i)) return(NULL)
      coul <- unname(couleur_poste(ps))
      actif <- ps %in% actifs
      actionLink(
        id_chip_poste(i),
        label = tagList(
          tags$b(ps), " ", euro_compta_total(recap$SOLDE[k]),
          tags$span(class = "text-muted", paste0(" · ", recap$N[k])),
          if (actif) icon("xmark")),
        class = "chip-poste",
        style = paste0("border-color:", coul, if (actif) "" else "55", ";",
                       "color:", coul, ";",
                       "background:", coul, if (actif) "2e" else "12", ";",
                       if (actif) "font-weight:600;" else ""))
    }))
}

# Un poste hors gestion prend la couleur neutre.
couleur_poste <- function(poste) {
  c <- COUL_POSTES[poste]
  ifelse(is.na(c), COUL_NEUTRE, c)
}

# Une couleur stable par journal : l'ordre alphabétique décide.
PALETTE_JOURNAUX <- c("#5b7b9b", "#8d5b8c", "#6b8f7a", "#a8763f",
                      "#7a6b8f", "#8f6b6b", "#5f8f8f", "#8d7b68")

couleur_journal <- function(journal, tous = sort(unique(journal))) {
  i <- match(journal, tous)
  PALETTE_JOURNAUX[((i - 1L) %% length(PALETTE_JOURNAUX)) + 1L]
}

#### Le forage ####

# Aller d'une alerte aux écritures qui l'ont causée.
#
# Un seul canal : un lien de forage pose sa consigne dans l'input
# `ctrl_forage`, et un unique observateur l'applique (pas d'observateurs créés
# à la volée). La consigne est une chaîne « clé=valeur;clé=valeur » qui ne
# porte que des filtres de lecture.
INPUT_FORAGE <- "ctrl_forage"

# Scalaire : une consigne désigne une destination. Boucler à l'appel pour
# en fabriquer plusieurs.
forage_spec <- function(texte = NULL, du = NULL, au = NULL, bloc = "Tout") {
  stopifnot(length(texte) <= 1, length(du) <= 1, length(au) <= 1)
  parts <- c(if (length(texte) && nzchar(texte)) paste0("texte=", texte),
             if (!is.null(du)) paste0("du=", format(as.Date(du), "%Y-%m-%d")),
             if (!is.null(au)) paste0("au=", format(as.Date(au), "%Y-%m-%d")),
             if (!is.null(bloc)) paste0("bloc=", bloc))
  paste(parts, collapse = ";")
}

# Le mois entier, bornes comprises : c'est la maille de toutes les alertes.
forage_mois <- function(m, texte = NULL) {
  m <- as.Date(m)
  forage_spec(texte = texte, du = m, au = ceiling_date(m, "month") - 1)
}

analyse_forage <- function(spec) {
  if (is.null(spec) || !nzchar(spec)) return(NULL)
  bouts <- strsplit(strsplit(spec, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  out <- lapply(bouts, function(b) if (length(b) >= 2)
    paste(b[-1], collapse = "=") else "")
  names(out) <- vapply(bouts, `[`, "", 1)
  out
}

#' Un lien qui emmène aux écritures : un <a> brut, sans identifiant.
lien_forage <- function(libelle, spec, classe = "lien-forage") {
  if (is.null(spec) || !nzchar(spec)) return(NULL)
  tags$a(
    class = classe, href = "#",
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
      INPUT_FORAGE, gsub("'", "", spec)),
    libelle, icon("arrow-right-long"))
}

# La même chose en HTML brut, une consigne par élément, pour une colonne de
# DataTable.
forages_texte <- function(textes)
  vapply(textes, function(x) forage_spec(texte = x), character(1),
         USE.NAMES = FALSE)

lien_forage_html <- function(libelle, spec) {
  ifelse(is.na(spec) | !nzchar(spec), "",
         sprintf(paste0("<a class=\"lien-forage\" href=\"#\" onclick=\"",
                        "Shiny.setInputValue('%s','%s',{priority:'event'});",
                        "return false;\">%s &rarr;</a>"),
                 INPUT_FORAGE, gsub("'", "", spec), libelle))
}

#' Où mène chacun des contrôles d'import, déduit de son nom.
#'
#' @param detail le texte du contrôle, d'où l'on tire le numéro de compte ou
#'   la date.
forage_import <- function(controle, detail, reprise = NULL) {
  if (is.null(reprise) && exists("REPRISE_ODOO")) reprise <- REPRISE_ODOO
  vapply(seq_along(controle), function(i) {
    ctl <- controle[i]; det <- detail[i]
    # Six chiffres au moins : un numéro de compte, pas un montant.
    if (grepl("^Comptes 6 et 7|^Matières non ventilées", ctl)) {
      cpt <- regmatches(det, regexpr("[0-9]{6,}", det))
      return(if (length(cpt)) forage_spec(texte = cpt[1]) else NA_character_)
    }
    if (grepl("^Pièce de reprise", ctl) && !is.null(reprise))
      return(forage_spec(du = reprise, au = reprise))
    if (grepl("^Mois d'Odoo écartés", ctl)) {
      m <- regmatches(det, gregexpr("[0-9]{2}/[0-9]{4}", det))[[1]]
      if (!length(m)) return(NA_character_)
      d <- as.Date(paste0("01/", m[1]), format = "%d/%m/%Y")
      return(forage_mois(d))
    }
    # Ces deux-là ne désignent pas un compte : la destination se construit côté
    # serveur.
    NA_character_
  }, character(1))
}

#### Le tableau ####

#' Une ligne par écriture, telle qu'Odoo la montre : débit et crédit séparés,
#' au centime, « 600100 · Achats de boissons » dans une cellule, journal et
#' poste en pastilles colorées.
#'
#' @param journaux tous les journaux connus, pour des couleurs stables.
table_ecritures <- function(g, n_max = 20000, journaux = NULL) {
  if (is.null(g)) return(tibble(Date = character()))
  # Zéro ligne traverse le chemin normal : les en-têtes restent.
  g <- g %>% arrange(desc(DATE), PIECE, COMPTE)
  if (nrow(g) > n_max) g <- head(g, n_max)
  if (is.null(journaux)) journaux <- sort(unique(g$JOURNAL))
  tibble(
    Date         = format(g$DATE, "%d/%m/%Y"),
    Journal      = pastille(g$JOURNAL, couleur_journal(g$JOURNAL, journaux)),
    `Pièce`      = g$PIECE,
    Compte       = paste0(g$COMPTE, " · ", tronque_nom(g$LIBELLE, 34)),
    `Libellé`    = tronque_nom(replace_na(g$TEXTE, ""), 48),
    Partenaire   = tronque_nom(replace_na(g$PARTENAIRE, ""), 26),
    Poste        = pastille(g$GESTION, unname(couleur_poste(g$GESTION))),
    `Débit`      = euro_compta(g$DEBIT),
    `Crédit`     = euro_compta(g$CREDIT),
    # Les jumelles de tri (cf. defs_tri dans R/theme.R).
    !!col_tri("Date")    := as.numeric(g$DATE),
    !!col_tri("Journal") := g$JOURNAL,
    !!col_tri("Poste")   := g$GESTION,
    !!col_tri("Débit")   := g$DEBIT,
    !!col_tri("Crédit")  := g$CREDIT)
}

# Colonnes à aligner à droite : elles portent des nombres sous forme de texte.
COLONNES_CHIFFREES_ECR <- c("Débit", "Crédit")

#' Le nom du fichier d'export, horodaté.
nom_export_ecritures <- function(instant = Sys.time())
  paste0("ecritures_", format(instant, "%Y%m%d_%H%M"), ".csv")
