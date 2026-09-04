# Gestion des accès : catalogue des onglets, profils, et lecture des droits
# déclarés dans le Google Sheet « IMPORT PASS ».
#
# Trois idées, et une seule table qui fait autorité :
#
#   1. ONGLETS liste les onglets de l'application (clé, titre, icône, et le
#      nom du constructeur d'interface). ui.R en tire la barre de navigation,
#      l'accueil en tire ses cartes, et les droits s'expriment avec ses clés.
#      Ajouter un onglet = ajouter une ligne, ici et nulle part ailleurs.
#
#   2. PROFILS traduit un rôle (« salle », « gestion », …) en liste d'onglets.
#      C'est le vocabulaire qu'on écrit dans le Sheet : une colonne `role`
#      plutôt que quatorze colonnes de cases à cocher.
#
#   3. Le droit se joue à l'onglet entier, jamais à l'intérieur d'un onglet.
#      Un onglet interdit n'est pas masqué côté navigateur : il n'est tout
#      simplement jamais inséré dans la barre (cf. server.R), donc son HTML ne
#      quitte pas le serveur et ses sorties ne sont jamais calculées.
#
# CONSÉQUENCE À CONNAÎTRE — les inputs d'un onglet n'existent qu'une fois
# l'onglet inséré, c'est-à-dire après la connexion. Avant, ils valent NULL.
#
# Les sorties (render*) ne s'en aperçoivent pas : Shiny ne les calcule que
# lorsqu'elles sont visibles. Les observateurs, eux, tournent dès le premier
# flush, donc AVANT la connexion. D'où deux règles, selon le sens :
#
#   LIRE un input d'onglet dans un observe() -> le protéger : req(input$x), ou
#   une valeur de repli explicite comme le fait sim_periode_val(). Sans quoi le
#   calcul part sur NULL, ce qui donne des erreurs de longueur bien loin de
#   leur cause (`1 + NULL/100` vaut numeric(0), et non NA).
#
#   GARNIR un input d'onglet (update*Input) -> attendre ONGLETS_PRETS(), le
#   drapeau posé par server.R. Sinon le message vise un champ qui n'existe pas
#   encore : il est perdu SANS ERREUR, et l'observateur, n'ayant plus de raison
#   d'être invalidé, ne rejoue jamais — le sélecteur reste vide pour de bon.
#   Le drapeau ne bascule qu'au flush suivant l'insertion : côté navigateur,
#   Shiny traite « inputMessages » (3e gestionnaire) avant « shiny-insert-tab »
#   (19e), donc garnir dans le même flush arriverait encore trop tôt.
#
#   Dans un observeEvent, ce garde-fou va dans l'EXPRESSION DÉCLENCHANTE et non
#   dans le corps : le corps est isolé, une dépendance posée là ne rejouerait
#   jamais l'observateur quand le drapeau bascule.

#### Catalogue des onglets ####

ONGLETS <- tibble::tribble(
  ~CLE,               ~TITRE,               ~ICONE,                    ~UI,
  "tab_accueil",      "Accueil",            "home",                    "ui_accueil",
  "tab_maintenant",   "Maintenant",         "gauge-high",              "ui_maintenant",
  "tab_detail",       "Chiffre d'affaires", "magnifying-glass-chart",  "ui_detail",
  "tab_historique",   "Historique",         "chart-line",              "ui_historique",
  "tab_annee",        "Année",              "calendar-check",          "ui_annee",
  "tab_futs",         "Fûts",               "boxes-stacked",           "ui_futs",
  "tab_boissons",     "Boissons",           "beer-mug-empty",          "ui_conso_boissons",
  "tab_focaccias",    "Focaccias",          "bread-slice",             "ui_focaccias",
  "tab_pizzwanze",    "Pizzwanze",          "pizza-slice",             "ui_pizzwanze",
  "tab_simulation",   "Simulation",         "sliders",                 "ui_simulation",
  "tab_compta",       "Compta",             "calculator",              "ui_compta",
  "tab_travail",      "Travail",            "person-running",          "ui_travail",
  "tab_planning",     "Planning",           "calendar-days",           "ui_planning",
  "tab_reservations", "Réservations",       "calendar-check",          "ui_reservations",
  "tab_comparaison",  "Comparaison",        "code-compare",            "ui_comparaison"
)

# L'accueil est la page d'atterrissage : quiconque a un mot de passe valide y
# arrive. Ce sont ses cartes qui sont filtrées, pas l'onglet lui-même.
ONGLET_ACCUEIL <- "tab_accueil"

#### Cartes de l'accueil ####
# Une carte renvoie vers un onglet : elle n'a de sens que si cet onglet est
# autorisé. CLE fait donc référence à ONGLETS$CLE.

CARTES_ACCUEIL <- tibble::tribble(
  ~CLE,               ~TITRE,          ~ICONE,            ~SORTIE,            ~BOUTON,
  "tab_maintenant",   "Maintenant",    "gauge-high",      "acc_maintenant",   "go_maintenant",
  "tab_annee",        "Année",         "calendar-check",  "acc_annee",        "go_annee",
  "tab_futs",         "Fûts",          "boxes-stacked",   "acc_futs",         "go_futs",
  "tab_boissons",     "Bières",        "beer-mug-empty",  "acc_bieres",       "go_boissons",
  "tab_focaccias",    "Focaccias",     "bread-slice",     "acc_focaccias",    "go_focaccias",
  "tab_pizzwanze",    "Pizzwanze",     "pizza-slice",     "acc_pizzwanze",    "go_pizzwanze",
  "tab_reservations", "Réservations",  "calendar-check",  "acc_reservations", "go_reservations",
  "tab_compta",       "Compta",        "calculator",      "acc_compta",       "go_compta",
  "tab_planning",     "Planning",      "calendar-days",   "acc_planning",     "go_planning"
)

#### Profils ####
# "*" = tous les onglets. Un profil absent de cette liste ne donne aucun droit :
# une faute de frappe dans le Sheet ferme la porte, elle ne l'ouvre pas.
#
# C'est ici qu'on ajuste la politique d'accès — sans toucher au Sheet, et sans
# toucher au reste du code.

PROFILS <- list(
  # Les associés : tout, compta et coûts du personnel compris.
  admin     = "*",

  # Pilotage quotidien : tout l'opérationnel, sans la compta générale.
  gestion   = c("tab_maintenant", "tab_detail", "tab_historique", "tab_annee",
                "tab_futs", "tab_boissons", "tab_focaccias", "tab_pizzwanze",
                "tab_simulation", "tab_travail", "tab_planning",
                "tab_reservations", "tab_comparaison"),

  # L'équipe : l'activité, pas les coûts (ni compta, ni masse salariale).
  equipe    = c("tab_maintenant", "tab_detail", "tab_historique", "tab_annee",
                "tab_futs", "tab_boissons", "tab_focaccias", "tab_pizzwanze",
                "tab_reservations"),

  # Salle : ce qui se passe ce soir.
  salle     = c("tab_maintenant", "tab_futs", "tab_boissons",
                "tab_reservations"),

  # Brasserie : les fûts et ce qu'on en tire.
  brasserie = c("tab_futs", "tab_boissons", "tab_maintenant"),
  
  # Public : pour présenter à l'extérieur
  public = c("tab_historique", "tab_annee", "tab_maintenant"),

  # Un accès de courtoisie : l'accueil et rien d'autre.
  invite    = character(0)
)

#### Lecture des colonnes du Sheet ####

# Colonne éventuellement absente du Sheet. Tant que les nouvelles colonnes
# n'ont pas été ajoutées, `defaut` s'applique à toutes les lignes — ce qui
# permet de déployer le code avant de toucher au tableur.
colonne_optionnelle <- function(db, nom, defaut) {
  if (nom %in% names(db)) as.character(db[[nom]]) else rep(defaut, nrow(db))
}

# Case « oui / non » d'un tableur, écrite par un humain : on accepte les
# formes usuelles et on retient `defaut` pour une case vide.
oui_non <- function(x, defaut = TRUE) {
  v <- tolower(trimws(as.character(x)))
  vide <- is.na(v) | !nzchar(v)
  ifelse(vide, defaut, v %in% c("oui", "o", "x", "vrai", "true", "1", "yes", "y"))
}

#### Résolution des droits ####

# Remet une liste d'onglets dans l'ordre du catalogue, et écarte les clés
# inconnues : l'ordre de la barre de navigation ne dépend pas de l'ordre de
# saisie dans le tableur.
ordonne_onglets <- function(cles) ONGLETS$CLE[ONGLETS$CLE %in% cles]

# Onglets accordés par un rôle.
onglets_du_profil <- function(role) {
  role <- tolower(trimws(as.character(role)))
  if (is.na(role) || !nzchar(role) || !role %in% names(PROFILS))
    return(character(0))
  droits <- PROFILS[[role]]
  if (identical(droits, "*")) ONGLETS$CLE else droits
}

# Onglets accordés par la colonne `onglets` : une liste écrite à la main,
# séparée par des virgules ou des points-virgules. On accepte aussi bien
# "compta, travail" que "tab_compta ; tab_travail", et "*" pour tout.
onglets_de_la_liste <- function(liste) {
  liste <- trimws(as.character(liste))
  if (is.na(liste) || !nzchar(liste)) return(character(0))
  if (liste == "*") return(ONGLETS$CLE)
  cles <- trimws(strsplit(liste, "[,;]")[[1]])
  cles <- cles[nzchar(cles)]
  ordonne_onglets(ifelse(startsWith(cles, "tab_"), cles, paste0("tab_", cles)))
}

# Droits d'une ligne de DB_PASSWORD. Le rôle donne la base, la colonne
# `onglets` s'y ajoute : on écrit "equipe" + "compta" pour un profil standard
# augmenté d'un onglet, sans inventer un rôle pour une seule personne.
droits_ligne <- function(ligne) {
  ordonne_onglets(union(
    ONGLET_ACCUEIL,
    union(onglets_du_profil(ligne$ROLE), onglets_de_la_liste(ligne$ONGLETS_LISTE))
  ))
}

# Vérifie une saisie contre DB_PASSWORD à une date donnée.
# Renvoie NULL en cas de refus, sinon l'identité et les droits associés.
#
# La comparaison reste sensible à la casse (comme avant), mais on rogne les
# espaces : un clavier de téléphone en ajoute un derrière le dernier caractère.
verifie_acces <- function(db_password, saisie, date = today()) {
  if (is.null(saisie) || length(saisie) != 1 || is.na(saisie)) return(NULL)
  saisie <- trimws(as.character(saisie))
  if (!nzchar(saisie)) return(NULL)

  candidats <- db_password %>%
    filter(ACTIF, DATE_DEBUT <= date, DATE_FIN >= date, PASS == saisie)
  if (nrow(candidats) == 0) return(NULL)

  # Deux lignes pour un même mot de passe : on prend la première, en ayant
  # laissé une trace dans les logs plutôt que de choisir en silence.
  if (nrow(candidats) > 1)
    cli::cli_alert_warning(
      "Mot de passe partag\u00e9 par {nrow(candidats)} lignes d'IMPORT PASS ; la premi\u00e8re l'emporte.")

  ligne <- candidats[1, ]
  list(NOM     = if (is.na(ligne$NOM) || !nzchar(ligne$NOM)) "Mazette" else ligne$NOM,
       ROLE    = ligne$ROLE,
       ONGLETS = droits_ligne(ligne))
}

#### Construction des cartes d'accueil ####

# La grille des cartes, réduite aux onglets autorisés.
#
# On construit uniquement les cartes voulues, plutôt que les huit avec masquage
# des interdites : layout_columns() enveloppe chaque enfant dans son propre
# div.bslib-grid-item, et ce wrapper garde sa cellule même quand son contenu
# est masqué — d'où des trous dans la grille. Une carte absente, elle, ne
# réserve rien, et les suivantes remontent d'elles-mêmes.
#
# Vit ici, avec le catalogue : server.R doit pouvoir l'appeler, et ui.R lui est
# invisible (cf. le registre des constructeurs, plus bas).
grille_cartes_accueil <- function(cles_autorisees,
                                  libelle = "Aller plus loin") {
  lignes <- which(CARTES_ACCUEIL$CLE %in% cles_autorisees)

  # Un profil sans aucune carte (« invite ») aurait une grille vide : on dit
  # pourquoi, plutôt que de laisser une page blanche sans explication.
  if (length(lignes) == 0)
    return(div(class = "text-muted small p-3",
               "Aucun onglet n'est ouvert pour ce mot de passe."))

  carte <- function(i) card(
    class = "acc-card",
    card_header(span(icon(CARTES_ACCUEIL$ICONE[i]), " ", CARTES_ACCUEIL$TITRE[i])),
    card_body(uiOutput(CARTES_ACCUEIL$SORTIE[i])),
    card_footer(actionButton(CARTES_ACCUEIL$BOUTON[i], libelle,
                             class = "btn-sm btn-primary w-100")))

  # Responsive : 1 carte par ligne sur téléphone, 2 sur tablette, 4 sur grand
  # écran. Un col_widths fixe imposerait trois colonnes même sur mobile.
  do.call(layout_columns, c(
    list(col_widths = breakpoints(sm = 12, md = 6, lg = 4, xl = 3)),
    lapply(lignes, carte)
  ))
}

#### Construction des onglets ####
#
# ui.R et server.R sont évalués par Shiny dans deux environnements distincts :
# les constructeurs `ui_*()` définis dans ui.R ne sont pas visibles depuis
# server.R, qui doit pourtant les appeler pour insérer un onglet après la
# connexion. ui.R les dépose donc ici, dans un environnement du global.
CONSTRUCTEURS_ONGLETS <- new.env(parent = emptyenv())

# Appelé une fois depuis ui.R, avec son propre environnement.
enregistre_constructeurs_onglets <- function(envir) {
  for (nom in unique(ONGLETS$UI))
    assign(nom, get(nom, envir = envir), envir = CONSTRUCTEURS_ONGLETS)
  invisible(nrow(ONGLETS))
}

# Le nav_panel d'un onglet du catalogue.
panneau_onglet <- function(cle) {
  i <- match(cle, ONGLETS$CLE)
  if (is.na(i)) stop("Onglet inconnu : ", cle)
  constructeur <- get(ONGLETS$UI[i], envir = CONSTRUCTEURS_ONGLETS)
  nav_panel(value = ONGLETS$CLE[i], title = ONGLETS$TITRE[i],
            icon = icon(ONGLETS$ICONE[i]), constructeur())
}

#### Compatibilité des .RData déjà enregistrés ####

# Un .RData antérieur à la gestion des accès ne contient que DATE_DEBUT,
# DATE_FIN et PASS. On complète les colonnes manquantes au chargement plutôt
# que d'imposer un réimport complet : sans elles, plus personne ne se connecte.
# Les valeurs par défaut reproduisent l'ancien comportement (tous les droits).
normalise_password <- function(db) {
  if (is.null(db)) return(NULL)
  if (!"NOM"           %in% names(db)) db$NOM           <- NA_character_
  if (!"ROLE"          %in% names(db)) db$ROLE          <- "admin"
  if (!"ONGLETS_LISTE" %in% names(db)) db$ONGLETS_LISTE <- NA_character_
  if (!"ACTIF"         %in% names(db)) db$ACTIF         <- TRUE
  db
}
