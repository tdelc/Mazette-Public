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

# Les trois périmètres proposés. « Résultat » par défaut, et ce n'est pas
# neutre : un compte bancaire voit passer dix fois le chiffre d'affaires, et
# dans un classement par montant il écrase tout le reste sans rien apprendre.
BLOCS_ECRITURES <- c("Résultat (classes 6 et 7)" = "Résultat",
                     "Bilan (classes 1 à 5)"     = "Bilan",
                     "Tout"                      = "Tout")

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
filtre_ecritures <- function(e, du = NULL, au = NULL, bloc = "Résultat",
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
#' sur une sélection entière il doit valoir zéro, et s'il ne le vaut pas, la
#' sélection coupe des pièces en deux — ce qui est normal quand on filtre par
#' compte, et inquiétant quand on ne filtre rien.
kpi_ecritures <- function(g, filtre_actif = FALSE) {
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
    bloc(euro_exact(sum(g$DEBIT)), "au débit", COUL_NEUTRE),
    tags$span("·"),
    bloc(euro_exact(sum(g$CREDIT)), "au crédit", COUL_NEUTRE),
    tags$span("·"),
    if (equilibre) bloc("équilibré", "", COUL_VERT)
    else tagList(
      bloc(euro_exact(ecart), "d'écart", COUL_AMBRE),
      tags$span(class = "text-muted",
                if (filtre_actif) "— normal : le filtre coupe des pièces en deux."
                else "— sur une sélection entière, c'est anormal.")))
}

#' Les comptes qui pèsent le plus dans la sélection.
#'
#' Classement sur la valeur ABSOLUE du solde : un compte très créditeur est
#' aussi intéressant qu'un compte très débiteur, et trier sur le solde signé
#' mettrait toutes les ventes en bas du graphe.
top_comptes <- function(g, n = 15) {
  if (is.null(g) || !nrow(g)) return(NULL)
  g %>%
    group_by(COMPTE, LIBELLE, GESTION) %>%
    summarise(SOLDE = sum(DEBIT) - sum(CREDIT), N = n(), .groups = "drop") %>%
    mutate(POIDS = abs(SOLDE)) %>%
    arrange(desc(POIDS)) %>%
    head(n) %>%
    arrange(POIDS)
}

# Un poste hors du plan de gestion (tout le bilan) n'a pas de couleur de
# poste : il prend le neutre, qui dit « classé nulle part » sans le crier.
couleur_poste <- function(poste) {
  c <- COUL_POSTES[poste]
  ifelse(is.na(c), COUL_NEUTRE, c)
}

graph_top_comptes <- function(t) {
  if (is.null(t) || !nrow(t))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune écriture", font = list(size = 13))))
  t <- t %>% mutate(
    ETIQ = paste0(COMPTE, " · ", tronque_nom(LIBELLE, 34)),
    SURVOL = paste0("<b>", COMPTE, " ", LIBELLE, "</b><br>",
                    GESTION, "<br>solde : ", euro_exact(SOLDE),
                    "<br>", N, " écriture", ifelse(N > 1, "s", "")))
  # hovertext et non text : sur une trace « bar », `text` s'IMPRIME sur la
  # barre. Une phrase de survol de trois lignes posée sur chaque barre rend le
  # graphe illisible — et c'est exactement ce qui s'est produit.
  plot_ly(t, x = ~SOLDE, y = ~factor(ETIQ, levels = ETIQ), type = "bar",
          orientation = "h", marker = list(color = couleur_poste(t$GESTION)),
          hoverinfo = "text", hovertext = ~SURVOL) %>%
    layout(xaxis = list(title = "", zeroline = TRUE, ticksuffix = " €"),
           yaxis = list(title = "", automargin = TRUE),
           margin = list(l = 10, r = 10, t = 10, b = 10),
           showlegend = FALSE) %>%
    change_cursor_plotly()
}

#' Le mouvement mois par mois, empilé par poste.
#'
#' Sert à répondre à « quand » après « combien ». Le solde comptable et non la
#' valeur de gestion : c'est le même nombre que dans le tableau du dessous, et
#' deux chiffres censés être le même doivent l'être.
solde_mensuel <- function(g) {
  if (is.null(g) || !nrow(g)) return(NULL)
  g %>%
    mutate(PERIODE = floor_date(DATE, "month")) %>%
    group_by(PERIODE, GESTION) %>%
    summarise(SOLDE = sum(DEBIT) - sum(CREDIT), .groups = "drop") %>%
    arrange(PERIODE)
}

graph_solde_mensuel <- function(m) {
  if (is.null(m) || !nrow(m))
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = list(text = "Aucune écriture", font = list(size = 13))))
  postes <- unique(m$GESTION)
  p <- plot_ly()
  for (ps in postes) {
    d <- m[m$GESTION == ps, , drop = FALSE]
    # hovertext, même raison que ci-dessus.
    p <- add_bars(p, x = d$PERIODE, y = d$SOLDE, name = ps,
                  marker = list(color = unname(couleur_poste(ps))),
                  hoverinfo = "text",
                  hovertext = paste0("<b>", ps, "</b><br>",
                                     etiquette_periode(d$PERIODE, "mois"), "<br>",
                                     euro_exact(d$SOLDE)))
  }
  p %>%
    layout(barmode = "relative",
           xaxis = list(title = "", type = "date"),
           yaxis = list(title = "", zeroline = TRUE, ticksuffix = " €"),
           margin = list(l = 10, r = 10, t = 10, b = 10),
           legend = list(orientation = "h", y = -0.18, font = list(size = 10))) %>%
    change_cursor_plotly()
}

#### Le tableau ####

#' Une ligne par écriture, telle qu'Odoo la montre.
#'
#' Débit et crédit séparés, pas un solde signé : c'est la forme du grand
#' livre, et c'est elle qui permet de retrouver la ligne dans Odoo sans avoir
#' à refaire une soustraction de tête.
table_ecritures <- function(g, n_max = 5000) {
  if (is.null(g)) return(tibble(Date = character()))
  # Zéro ligne traverse le chemin NORMAL, et ne rend donc pas un tableau à une
  # seule colonne : les en-têtes restent les mêmes quand une recherche ne
  # trouve rien, et l'écran ne donne pas l'impression de s'être cassé.
  g <- g %>% arrange(desc(DATE), PIECE, COMPTE)
  if (nrow(g) > n_max) g <- head(g, n_max)
  eur <- function(x) format_CA(x, -1)
  tibble(
    Date         = format(g$DATE, "%d/%m/%Y"),
    `Pièce`      = g$PIECE,
    Journal      = g$JOURNAL,
    Compte       = as.character(g$COMPTE),
    `Intitulé`   = tronque_nom(g$LIBELLE, 38),
    `Libellé`    = tronque_nom(replace_na(g$TEXTE, ""), 48),
    Partenaire   = tronque_nom(replace_na(g$PARTENAIRE, ""), 26),
    Poste        = g$GESTION,
    `Débit`      = eur(g$DEBIT),
    `Crédit`     = eur(g$CREDIT),
    # Les jumelles : DT trie sur ce qu'il AFFICHE, et « 31/01/2026 » se
    # classerait par son jour (cf. defs_tri dans R/theme.R).
    !!col_tri("Date")    := as.numeric(g$DATE),
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
