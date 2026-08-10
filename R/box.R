

#### Box Ventes ####

box_ventes_jour <- function(db_kpi,db_obj,date_debut,nb_jours,
                            format_date = "%d",titre = "",
                            is_semaine=FALSE,is_midi=TRUE,is_boisson=TRUE,
                            is_objectif=TRUE, width = "14%",
                            unite_tva = "HTVA", montrer_unite = NULL){
  plot_kpi <- db_kpi %>%
    left_join(db_obj%>%
                select(-starts_with("CA_")) %>%
                rename(ventes_obj = ventes)) %>%
    filter(DATE >= date_debut,DATE <= date_debut+days(nb_jours)) %>%
    # mutate(JOUR_SEMAINE = factor(JOUR_SEMAINE,levels=vecteur_jours_LOCAL,
    #                              labels = vecteur_jours)) %>%
    mutate(title = paste0(JOUR_SEMAINE," ",format(DATE,format = format_date)))
  
  if (titre != "") plot_kpi$title <- titre
  
  plot_kpi <- plot_kpi %>%
    table_kpi(fl_semaine = is_semaine,fl_midi = is_midi,
              fl_boisson = is_boisson,fl_objectif = is_objectif,
              width = width, unite_tva = unite_tva,
              montrer_unite = montrer_unite)
  
  return(
    div(class = "ventes-grid", do.call(tagList, plot_kpi))
  )
}

box_ventes_total <- function(db_kpi,db_obj,date_debut,nb_jours,
                             format_date = "%d",titre = "",
                             is_semaine=FALSE,is_midi=TRUE,is_boisson=TRUE,
                             is_objectif=TRUE, unite_tva = "HTVA",
                             montrer_unite = NULL){
  plot_kpi <- db_kpi %>%
    left_join(db_obj%>%
                select(-starts_with("CA_")) %>%
                rename(ventes_obj = ventes)) %>%
    mutate(ventes_obj = ventes_obj * (ventes>0)) %>%
    filter(DATE >= date_debut,DATE <= date_debut+days(nb_jours)) %>%
    summarise(ventes = sum(ventes,na.rm=TRUE),
              ventes_obj = sum(ventes_obj,na.rm=TRUE),
              Jour = sum(Jour),Soir = sum(Soir),
              Boisson = sum(Boisson),Nourriture = sum(Nourriture),
              Semaine = sum(Semaine),`Week-end` = sum(`Week-end`)) %>%
    mutate(title = titre) %>%
    table_kpi(fl_semaine = is_semaine,fl_midi = is_midi,
              fl_boisson = is_boisson,fl_objectif = is_objectif, width = "100%",
              unite_tva = unite_tva, montrer_unite = montrer_unite)
  
  return(
    div(class = "ventes-grid", do.call(tagList, plot_kpi))
  )
}


# `montrer_unite` : par défaut, la pastille d'unité n'apparaît que si la série
# ne compte qu'une carte. Sur les sept cartes d'une semaine elle se répéterait
# à l'identique sept fois — du bruit, alors que le total juste à côté porte
# déjà l'information. Passer TRUE pour forcer l'affichage partout.
table_kpi <- function(db,fl_midi=TRUE,fl_boisson=TRUE,
                      fl_semaine=TRUE,fl_objectif=TRUE,width = "14%",
                      unite_tva = "HTVA", montrer_unite = NULL){

  if (is.null(montrer_unite)) montrer_unite <- nrow(db) == 1

  list_kpi <- list()
  for (i in 1:nrow(db)){
    ligne <- db[i,]
    title <- ligne$title
    ca <- ligne$ventes
    objectif <- ligne$ventes_obj
    # Même convention que les barres de CA du reste du tableau de bord :
    # vert atteint, ambre à partir de 90 %, rouge en dessous.
    couleur <- couleur_objectif(ca, ligne$ventes_obj)
    percent_midi <- round(100 * ligne$Jour / (ligne$Jour+ligne$Soir))
    percent_soir <- 100 - percent_midi
    percent_boisson <- round(100 * ligne$Boisson / (ligne$Boisson+ligne$Nourriture))
    percent_nourriture <- 100 - percent_boisson
    percent_semaine <- round(100 * ligne$Semaine / (ligne$Semaine+ligne$`Week-end`))
    percent_weekend <- 100 - percent_semaine
    
    if (!fl_midi) {
      percent_midi <- 0
      percent_soir <- 0
    }
    if (!fl_boisson) {
      percent_boisson <- 0
      percent_nourriture <- 0
    }
    if (!fl_semaine) {
      percent_semaine <- 0
      percent_weekend <- 0
    }
    if (!fl_objectif){
      objectif <- NULL
    }
    
    list_kpi[[i]] <- tagList(caInfoBox(title,ca,percent_midi,
                                       percent_soir,percent_boisson,
                                       percent_nourriture,percent_semaine,
                                       percent_weekend,width,couleur,objectif,
                                       unite_tva, montrer_unite))
  }
  return(list_kpi)
}


# Fonction pour générer une infoBox avec une info-bulle

# Barre de répartition en deux segments. La mise en forme vit dans style.css
# (.ventes-barre), pour que la barre s'adapte à la largeur de sa carte.
generate_bar <- function(percent1, percent2, color1, color2, title) {
  if (is.na(percent1) || is.na(percent2) || percent1 + percent2 <= 0) return(NULL)
  
  # On n'inscrit le pourcentage que si le segment est assez large : en dessous,
  # le texte était rogné et illisible (« 7% » dans un filet de 10 px).
  etiquette <- function(p) if (p >= 18) paste0(p, "%") else ""
  
  div(
    div(
      class = "ventes-barre",
      span(style = paste0("flex:", percent1, ";background:", color1, ";"),
           etiquette(percent1)),
      span(style = paste0("flex:", percent2, ";background:", color2, ";"),
           etiquette(percent2))
    ),
    div(class = "ventes-legende", title)
  )
}

# Carte de ventes d'une journée (ou d'un total) : CA, atteinte de l'objectif
# et barres de répartition.
# `width` n'est plus utilisé : la largeur est pilotée par la grille responsive
# qui contient les cartes (.ventes-grid). L'argument reste pour ne pas casser
# les appels existants.
caInfoBox <- function(title, ca, percent_midi, percent_soir, percent_boisson,
                      percent_nourriture, percent_semaine, percent_weekend,
                      width = NULL, ca_color = NULL, objectif = NULL,
                      unite_tva = "HTVA", montrer_unite = TRUE) {

  couleur <- if (is.null(ca_color)) couleur_objectif(ca, objectif) else ca_color
  
  # Les barres n'ont pas de sens sur une journée sans activité
  barres <- if (!is.na(ca) && ca > 5) {
    div(
      class = "ventes-barres",
      generate_bar(percent_midi, percent_soir,
                   "#e67e22", "#9b59b6", "Midi / Soir"),
      generate_bar(percent_boisson, percent_nourriture,
                   "#d4ac0d", "#27ae60", "Boisson / Nourriture"),
      generate_bar(percent_semaine, percent_weekend,
                   "#2980b9", "#c0392b", "Semaine / Week-end")
    )
  } else NULL
  
  # L'unité était collée dans l'intitulé (« objectif HTVA 1 234 € ») : elle
  # remonte sur le chiffre de CA, qui est celui qu'on lit et qu'on recopie.
  objectif_ligne <- if (!is.null(objectif) && !is.na(objectif) && objectif > 0) {
    div(class = "ventes-obj",
        "objectif ", format_CA(objectif, -1), " · ",
        tags$b(paste0(round(100 * ca / objectif), " %")))
  } else NULL

  div(
    class = "ventes-card",
    div(class = "ventes-jour", title),
    div(class = "ventes-ca", style = paste0("color:", couleur, ";"),
        format_CA(ca, -1),
        if (isTRUE(montrer_unite)) badge_tva(unite_tva)),
    objectif_ligne,
    barres
  )
}
