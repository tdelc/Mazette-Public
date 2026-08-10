#-----------------------------
# Module server
#-----------------------------
comparaisonServer <- function(id, date_reactive, label_suff = "",
                                        secteur,db_kpi,db_obj,var_tva) {
  moduleServer(id, function(input, output, session) {
    
    if (secteur == "Total"){
      DB_COUT_TOTAL <- DB_COUT_TOTAL %>%
        mutate(SECTEUR = case_when(
          SECTEUR %in% c("Cuisine", "Boulangerie") ~ "Nourriture",
          SECTEUR %in% c("Service", "Brasserie") ~ "Boisson",
          TRUE ~ "Support"))
      DB_PRODUITS_JOURS_FULL <- DB_PRODUITS_JOURS_FULL %>% 
        mutate(SECTEUR = "Total")
    }
    
    sub_secteur <- reactive({
      if (secteur == "Nourriture") c("Cuisine", "Boulangerie")
      else if (secteur == "Boisson") c("Service", "Brasserie")
      else c("Boisson", "Nourriture","Support")
    })
    
    debut_mois <- reactive({
      floor_date(date_reactive(), unit = "month")
    })
    
    # box_ventes_all_mois
    output[[paste0("box_",secteur,"_ventes_all_mois", label_suff)]] <- renderUI({
      fin_mois <- ceiling_date(debut_mois(), unit = "month") - 1
      box_ventes_mois(
        db_kpi(),
        db_obj(),
        debut_mois(),
        fin_mois,
        "Tous les secteurs"
      )
    })
    
    # box_ventes_spe_mois
    if (secteur %in% c("Nourriture","Boisson")){
      output[[paste0("box_",secteur,"_ventes_spe_mois", label_suff)]] <- renderUI({
        kpi_special <- prepa_db(create_kpi_secteur(DB_KPI, secteur), var_tva())
        obj_special <- db_obj() %>% mutate(ventes = ventes/2)
        fin_mois <- ceiling_date(debut_mois(), unit = "month") - 1
        box_ventes_mois(
          kpi_special,
          obj_special,
          debut_mois(),
          fin_mois,
          secteur
        )
      })      
    }
    
    # table_produits_mois
    output[[paste0("table_",secteur,"_ventes_mois", label_suff)]] <- renderDataTable({
      table_produits_mois(DB_PRODUITS_JOURS_FULL %>% 
                          filter(SECTEUR %in% secteur),debut_mois())
    }, extensions = 'Scroller', options = list(deferRender = TRUE,
      scrollY = 200,scroller = TRUE))
    
    output[[paste0("table_",secteur,"_cout", label_suff)]] <- renderTable({
      table_cout_secteurs(
        DB_COUT_TOTAL %>%
          filter(
            PREMIER_JOUR_MOIS == debut_mois(),
            SECTEUR %in% sub_secteur()
          )
      )
    })
    
    switch_graph <- reactiveVal(FALSE)
    
    observeEvent(input[[paste0("graph_",secteur,"_cout","_click")]],{
      switch_graph(!switch_graph())
    })
    
    # graph_cout
    output[[paste0("graph_",secteur,"_cout", label_suff)]] <- renderPlot({
      DB <- DB_COUT_TOTAL %>%
        filter(
          PREMIER_JOUR_MOIS == debut_mois(),
          SECTEUR %in% sub_secteur()
        )
      if (switch_graph()) {
        graph_cout_secteurs(DB, "TYPE_COUT", "SECTEUR")
      } else {
        graph_cout_secteurs(DB, "SECTEUR", "TYPE_COUT")
      }
    })
    
   
    
    # valueBox_nourriture_all
    output[[paste0("valueBox_",secteur,"_all", label_suff)]] <- renderUI({
      
      if (secteur == "Nourriture"){
        valueBox_nourriture_all(
          DB_KPI_NOURRITURE_CA %>% filter(PREMIER_JOUR_MOIS == debut_mois())
        )
      }else if (secteur == "Boisson"){
        valueBox_boisson_all(
          DB_KPI_BOISSON_CA %>% filter(PREMIER_JOUR_MOIS == debut_mois())
        )
      }else{
        valueBox_total_all(
          DB_KPI_TOTAL_CA %>% filter(PREMIER_JOUR_MOIS == debut_mois())
        )
      }
     
    })
   
    output[[paste0("graph_",secteur,"_evo_cout", label_suff)]] <- renderPlot({
      graph_evo_cout(debut_mois(), secteur, sub_secteur())
    })
    
    output[[paste0("graph_",secteur,"_evo_kpi", label_suff)]] <- renderPlotly({
      graph_evo_kpi(debut_mois(), secteur, sub_secteur())
    })
    
  })
}

#-----------------------------
# Module UI
#-----------------------------
comparaisonUI <- function(id, label_suff = "", secteur) {
  ns <- NS(id)
  tagList(
    box(
      width = 12,status = "primary",solidHeader = TRUE,
      title = "Chiffres d'affaires",
      column(width = 6,
        uiOutput(ns(paste0("box_",secteur,"_ventes_all_mois", label_suff)))
      ),
      column(width = 6,
             uiOutput(ns(paste0("box_",secteur,"_ventes_spe_mois", label_suff)))
      ),
      dataTableOutput(ns(paste0("table_",secteur,"_ventes_mois", label_suff)))
    ),
    box(
      width = 12,status = "primary",solidHeader = TRUE,
      title = "Coûts par secteur",
      tableOutput(ns(paste0("table_",secteur,"_cout", label_suff))),
      plotOutput(ns(paste0("graph_",secteur,"_cout", label_suff)),
                 height = "200px",
                 click = ns(paste0("graph_",secteur,"_cout","_click")))
                 # click = ns(paste0("graph_",secteur,"_cout", label_suff,"_click")))
    ),
    box(
      width = 12,status = "primary",solidHeader = TRUE,
      title = "KPI",
      uiOutput(ns(paste0("valueBox_",secteur,"_all", label_suff)))
    )
  )
}


