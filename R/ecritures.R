# Le grand livre : une ligne par écriture, et de quoi y chercher.
#
# ---------------------------------------------------------------------------
# Pourquoi un volet pour ça
# ---------------------------------------------------------------------------
# DB_COMPTA est un agrégat : un montant par mois et par compte. Il suffit à
# tous les calculs du dashboard, et à rien d'autre. Dès qu'un poste surprend —
# « pourquoi 4 000 € de frais généraux en mars ? » — il ne sait plus répondre :
# ni la date exacte, ni la pièce, ni le fournisseur n'y ont survécu.
#
# Les écritures, elles, portent tout cela. Elles ne remplacent aucun calcul :
# elles répondent à la question d'après.
#
# ---------------------------------------------------------------------------
# Deux lectures d'un même montant, et il faut les deux
# ---------------------------------------------------------------------------
# SOLDE  = DÉBIT − CRÉDIT. La lecture COMPTABLE, celle du grand livre. Un achat
#          est positif, une vente est négative. C'est ce qu'on veut voir dans
#          le tableau, parce que c'est ce qu'Odoo affiche et qu'un chiffre
#          doit pouvoir se retrouver à l'identique dans sa source.
# VALEUR = SENS_G x (CRÉDIT − DÉBIT). La lecture de GESTION, celle de
#          DB_COMPTA : positive quand le poste grossit, qu'il s'agisse d'une
#          vente ou d'une charge. C'est ce qu'on veut dans les graphes, pour
#          que « les matières pèsent 19 000 € » ne s'affiche pas en négatif.
#
# Mélanger les deux produirait un volet où le même euro change de signe selon
# l'endroit où on le regarde. On les sépare donc, et on les nomme.

#### Enrichissement ####

#' Ajoute à des écritures ce que le plan comptable en sait.
#'
#' classe_comptes() veut ANNEE et MOIS, que normalise_ecritures() fournit déjà.
#' Ses filtres TYPE / AGREGE ne s'appliquent qu'aux tables qui portent ces
#' colonnes : les écritures les traversent sans rien perdre.
enrichit_ecritures <- function(ecr) {
  if (is.null(ecr) || !nrow(ecr)) return(NULL)
  classe_comptes(ecr) %>%
    mutate(
      # Le MÊME vocabulaire que les trois onglets d'à côté : « Matières
      # premières », pas « Achats ». Le plan comptable nomme ses postes
      # autrement que la gestion, et faire cohabiter les deux vocabulaires
      # dans un même volet obligerait à traduire de tête.
      GESTION = libelle_bloc(bloc_gestion(POSTE, SECTION_G)),
      SOLDE  = DEBIT - CREDIT,
      # Un compte de bilan n'a pas de sens de gestion : son « VALEUR » n'a
      # aucune signification, et vaut donc NA plutôt que zéro — zéro voudrait
      # dire « ce compte n'a rien fait », ce qui est faux.
      VALEUR = ifelse(is.na(SENS_G), NA_real_, SENS_G * (CREDIT - DEBIT)),
      CLASSE = substr(as.character(COMPTE), 1, 1),
      BLOC   = ifelse(CLASSE %in% c("6", "7"), "Résultat", "Bilan"))
}

#### Filtres ####

# Les trois périmètres proposés, « Tout » par défaut : un grand livre montre
# le grand livre, et le périmètre est un filtre qu'on applique ensuite. C'est
# aussi le seul réglage où le contrôle débit = crédit a un sens, puisque lui
# seul ne coupe aucune pièce en deux.
BLOCS_ECRITURES <- c("Tout"                      = "Tout",
                     "Résultat (classes 6 et 7)" = "Résultat",
                     "Bilan (classes 1 à 5)"     = "Bilan")

#' Cherche sans se soucier de la casse ni des expressions régulières.
#'
#' fixed = TRUE : la recherche vient d'un champ libre. « Achat (2026) » doit
#' chercher ce texte, pas faire tomber le volet sur une parenthèse non fermée.
contient_texte <- function(cols, texte) {
  if (is.null(texte) || !nzchar(trimws(texte)))
    return(rep(TRUE, nrow(cols)))
  cible <- tolower(trimws(texte))
  Reduce(`|`, lapply(cols, function(x)
    grepl(cible, tolower(replace_na(as.character(x), "")), fixed = TRUE)))
}

#' Le filtre unique. Il pilote les KPI, les deux graphes ET le tableau.
#'
#' Un seul filtre, une seule vérité : si le tableau avait sa propre recherche
#' en plus, un total afficherait la somme d'autre chose que ce qu'on lit en
#' dessous, et personne ne saurait lequel des deux croire.
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

#' Les quatre nombres du haut.
#'
#' L'écart débit/crédit est le seul qui soit un CONTRÔLE et non une mesure :
#' sur une sélection entière il doit valoir zéro. Filtrée, elle coupe des
#' pièces en deux et l'écart redevient normal — d'où l'ambre et non le rouge.
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

#' Le récapitulatif par poste de la sélection.
#'
#' Il remplace deux graphiques qui faisaient joli sans rien apprendre. Une
#' ligne de pastilles est plus dense qu'un diagramme en barres, tient sur un
#' quart de la hauteur, et chacune est un FILTRE : un clic et le tableau ne
#' montre plus que ce poste. Un graphe, lui, se regarde et c'est tout.
recap_postes <- function(g) {
  if (is.null(g) || !nrow(g)) return(NULL)
  g %>%
    group_by(GESTION) %>%
    summarise(SOLDE = sum(DEBIT) - sum(CREDIT), N = n(), .groups = "drop") %>%
    arrange(desc(abs(SOLDE)))
}

# La liste FERMÉE des postes de gestion. Fermée à dessein : les pastilles sont
# des boutons, et un bouton a besoin d'un identifiant stable. Les fabriquer à
# la volée obligerait à créer des observateurs dynamiques, qui s'empilent à
# chaque redessin ; ici, les huit existent depuis le démarrage et ne bougent
# plus, quoi que contienne la sélection.
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

# Un poste hors du plan de gestion (tout le bilan) n'a pas de couleur de
# poste : il prend le neutre, qui dit « classé nulle part » sans le crier.
couleur_poste <- function(poste) {
  c <- COUL_POSTES[poste]
  ifelse(is.na(c), COUL_NEUTRE, c)
}

# Les journaux n'ont pas de couleur dans le plan comptable : il n'y en a pas à
# connaître, ils sont propres à chaque installation d'Odoo. On en attribue
# donc une, STABLE pour un même jeu de journaux : l'ordre alphabétique décide,
# et le même journal garde sa couleur d'une session à l'autre. Une attribution
# au fil de l'arrivée des lignes en changerait à chaque filtre, et la couleur
# n'apprendrait plus rien.
PALETTE_JOURNAUX <- c("#5b7b9b", "#8d5b8c", "#6b8f7a", "#a8763f",
                      "#7a6b8f", "#8f6b6b", "#5f8f8f", "#8d7b68")

couleur_journal <- function(journal, tous = sort(unique(journal))) {
  i <- match(journal, tous)
  PALETTE_JOURNAUX[((i - 1L) %% length(PALETTE_JOURNAUX)) + 1L]
}

#### Le forage ####

# Aller d'une alerte aux écritures qui l'ont causée.
#
# UN SEUL canal pour tout le volet : un lien de forage pose sa consigne dans
# l'input `ctrl_forage`, et un unique observateur l'applique. L'alternative —
# un bouton Shiny par lien — demanderait des identifiants stables, donc des
# observateurs créés à la volée, qui s'empilent à chaque redessin de la liste
# d'alertes. Ici la liste peut changer à chaque import sans que rien ne bouge.
#
# La consigne est une chaîne « clé=valeur;clé=valeur », lisible dans le code
# source de la page. Elle ne porte que des filtres de lecture : au pire, un
# forage trafiqué affiche d'autres écritures que prévu.
INPUT_FORAGE <- "ctrl_forage"

# SCALAIRE, et c'est volontaire : une consigne désigne UNE destination. Pour
# en fabriquer plusieurs — une par ligne d'un tableau — on boucle à l'appel.
# La version vectorisée produisait « texte=A;texte=B;bloc=Tout », qu'aucun
# filtre ne sait lire, et le disait d'autant moins qu'elle ne plantait pas.
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

#' Un lien qui emmène aux écritures.
#'
#' Un <a> brut et non un actionLink : il n'a pas besoin d'identifiant, et
#' c'est précisément ce qui permet d'en poser autant qu'on veut.
lien_forage <- function(libelle, spec, classe = "lien-forage") {
  if (is.null(spec) || !nzchar(spec)) return(NULL)
  tags$a(
    class = classe, href = "#",
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
      INPUT_FORAGE, gsub("'", "", spec)),
    libelle, icon("arrow-right-long"))
}

# La même chose en HTML brut, pour une cellule de DataTable.
# Plusieurs consignes d'un coup, une par élément. C'est la forme dont a besoin
# une colonne de tableau.
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

#' Où mène chacun des contrôles d'import.
#'
#' Déduit du NOM du contrôle, pas stocké avec lui : ces tables sont produites
#' à l'import et sauvées dans le .RData, et leur ajouter une colonne casserait
#' la lecture des caches déjà écrits.
#'
#' @param detail le texte du contrôle, d'où l'on tire le numéro de compte ou
#'   la date quand il y en a un.
forage_import <- function(controle, detail, reprise = NULL) {
  if (is.null(reprise) && exists("REPRISE_ODOO")) reprise <- REPRISE_ODOO
  vapply(seq_along(controle), function(i) {
    ctl <- controle[i]; det <- detail[i]
    # Six chiffres au moins : les numéros de compte en ont six dans ce plan,
    # là où les montants du message en ont quatre (« 1477 € sans secteur »).
    # Sans ce seuil, le forage emmenait vers le montant pris pour un compte.
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
    # « Matières non ventilées » et « Débit = crédit » ne désignent pas un
    # compte : leur destination se construit côté serveur, qui a les données.
    NA_character_
  }, character(1))
}

#### Le tableau ####

#' Une ligne par écriture, telle qu'Odoo la montre.
#'
#' Débit et crédit SÉPARÉS, pas un solde signé : c'est la forme du grand
#' livre, et c'est elle qui permet de retrouver la ligne dans Odoo sans avoir
#' à refaire une soustraction de tête. Au centime, pour la même raison.
#'
#' Le compte et son intitulé tiennent dans une seule cellule — « 600100 ·
#' Achats de boissons » — parce que c'est ainsi qu'Odoo les affiche et qu'on
#' ne cherche jamais l'un sans l'autre. Journal et poste deviennent des
#' pastilles colorées : sur deux cents lignes, une couleur se repère d'un coup
#' d'œil là où un mot doit se lire.
#'
#' @param journaux tous les journaux connus, pour que la couleur d'un journal
#'   ne change pas quand un filtre en retire d'autres.
table_ecritures <- function(g, n_max = 20000, journaux = NULL) {
  if (is.null(g)) return(tibble(Date = character()))
  # Zéro ligne traverse le chemin NORMAL, et ne rend donc pas un tableau à une
  # seule colonne : les en-têtes restent les mêmes quand une recherche ne
  # trouve rien, et l'écran ne donne pas l'impression de s'être cassé.
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
    # Les jumelles : DT trie sur ce qu'il AFFICHE. « 31/01/2026 » se classerait
    # par son jour, et une pastille par son code HTML (cf. defs_tri dans
    # R/theme.R).
    !!col_tri("Date")    := as.numeric(g$DATE),
    !!col_tri("Journal") := g$JOURNAL,
    !!col_tri("Poste")   := g$GESTION,
    !!col_tri("Débit")   := g$DEBIT,
    !!col_tri("Crédit")  := g$CREDIT)
}

# Colonnes à aligner à droite : elles portent des nombres sous forme de texte.
COLONNES_CHIFFREES_ECR <- c("Débit", "Crédit")

#' Le nom du fichier d'export, horodaté.
#'
#' Horodaté parce qu'on en télécharge plusieurs, avec des filtres différents,
#' et qu'« ecritures.csv (3) » ne dit rien de ce qu'il contient.
nom_export_ecritures <- function(instant = Sys.time())
  paste0("ecritures_", format(instant, "%Y%m%d_%H%M"), ".csv")
