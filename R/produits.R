from_product_to_boisson <- function(DB){
  DB %>%
    mutate(PRODUIT_VIDE = str_remove(PRODUIT," *[0-9]+ *[cC][lL]"),
           PRODUIT_VIDE = str_remove(PRODUIT_VIDE," verre"),
           PRODUIT_VIDE = str_remove(PRODUIT_VIDE," 1L"),
           VOLUME_CL = case_when(
             PRODUIT %in% c("Pépin blanc verre",
                            "Pépin rouge verre",
                            "Hurluberlu rouge verre") ~ 12.5,
             PRODUIT %in% c("Cidre Rhuys","Kefir") ~ 25,
             PRODUIT %in% c("Rhum Brussels") ~ 3,
             str_detect(PRODUIT,"1L") ~ 100,
             TRUE ~ as.numeric(str_extract(PRODUIT," *([0-9]+) *[cC]*[lL]",group= 1))
           ),
           BOISSON = case_when(
             is.na(VOLUME_CL) ~ "",
             TRUE ~ PRODUIT_VIDE
           )
    ) %>%
    rename(PRODUIT_FULL = PRODUIT,
           PRODUIT = PRODUIT_VIDE)
}