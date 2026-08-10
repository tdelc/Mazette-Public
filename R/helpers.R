# Tronque un nom de produit trop long
tronque_nom <- function(x, max = 40) {
  ifelse(nchar(x) > max, paste0(substr(x, 1, max), "…"), x)
}

# Début de période d'une date selon la granularité.
debut_periode <- function(d, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
         semaine = floor_date(d, "week", week_start = 1),
         mois    = floor_date(d, "month"),
         annee   = floor_date(d, "year"))
}

# Dernier jour d'une période.
fin_periode <- function(periode, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
         semaine = periode + 6,
         mois    = ceiling_date(periode, "month") - 1,
         annee   = ceiling_date(periode, "year") - 1)
}

# Étiquette lisible d'une période (date = début de période).
label_periode <- function(periode, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  switch(unite,
         semaine = paste0("Sem. ", format(periode, "%d/%m/%y")),
         mois    = format(periode, "%B %Y"),
         annee   = format(periode, "%Y"))
}

# Ratio en % (NA si dénominateur nul)
#
# ifelse() renvoie un vecteur de la longueur de sa CONDITION : avec un
# dénominateur scalaire et un numérateur vectoriel, il ne rendait qu'une seule
# valeur, recyclée sur toute la colonne. On recycle donc explicitement.
ratio_pct <- function(num, den) {
  n <- max(length(num), length(den))
  if (n == 0) return(numeric(0))
  num <- rep_len(num, n); den <- rep_len(den, n)
  ifelse(!is.na(den) & den > 0, round(100 * num / den, 1), NA_real_)
}

# Étiquette d'unité de TVA, à accoler à un intitulé de CA.
#
# Le style est volontairement défini en CSS (.badge-tva) plutôt qu'en ligne :
# la pastille se pose sur des fonds très différents — brun, ambre, crème — et
# doit rester lisible sur les trois. Elle emprunte donc sa couleur au texte qui
# l'entoure via currentColor, au lieu de figer un couple fond/texte qui serait
# faux dans deux cas sur trois.
#
# Titre d'un value_box :
#   output$title_vb <- renderUI(titre_avec_tva("CA de la veille", input$unite_tva))
badge_tva <- function(unite) {
  if (is.null(unite) || !nzchar(unite)) return(NULL)
  span(class = "badge-tva", unite)
}

# Intitulé suivi de sa pastille d'unité.
titre_avec_tva <- function(libelle, unite) {
  tagList(libelle, badge_tva(unite))
}

# Bandeau d'avertissement, à utiliser dans un renderUI.
# Ne renvoie quelque chose que si `afficher` est vrai ; sinon NULL, donc rien
# ne s'affiche et l'espace n'est pas réservé.
bandeau_alerte <- function(afficher, texte,
                           titre   = "À lire attentivement",
                           couleur = COUL_ROUGE,
                           icone   = "triangle-exclamation") {
  if (!isTRUE(afficher)) return(NULL)
  
  div(
    class = "d-flex align-items-start gap-2", role = "alert",
    style = paste0("background:", couleur, "1a;",
                   "border-left:4px solid ", couleur, ";",
                   "border-radius:0.5rem;padding:0.7rem 0.9rem;",
                   "margin-bottom:0.9rem;"),
    span(style = paste0("color:", couleur, ";font-size:1.15rem;line-height:1.2;"),
         icon(icone)),
    div(
      div(style = paste0("font-weight:700;color:", couleur, ";"), titre),
      div(class = "small", texte)
    )
  )
}

# Périodes disponibles (avec du CA), de la plus récente à la plus ancienne.
liste_periodes_dispo <- function(db_kpi, unite = c("semaine", "mois", "annee")) {
  unite <- match.arg(unite)
  db_kpi %>%
    filter(ventes > 0) %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    distinct(PERIODE) %>%
    arrange(desc(PERIODE)) %>%
    pull(PERIODE)
}

# Périodes disponibles d'après la compta, de la plus récente à la plus ancienne.
liste_periodes_compta_dispo <- function(db_compta) {
  unite <- match.arg(unite)
  db_kpi %>%
    filter(ventes > 0) %>%
    mutate(PERIODE = debut_periode(DATE, unite)) %>%
    distinct(PERIODE) %>%
    arrange(desc(PERIODE)) %>%
    pull(PERIODE)
}