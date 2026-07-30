from_product_to_boisson <- function(DB){
  DB %>%
    mutate(PRODUCT_VIDE = str_remove(PRODUCT," *[0-9]+ *[cC][lL]"),
           PRODUCT_VIDE = str_remove(PRODUCT_VIDE," verre"),
           PRODUCT_VIDE = str_remove(PRODUCT_VIDE," 1L"),
           VOLUME_CL = case_when(
             PRODUCT %in% c("Pépin blanc verre",
                            "Pépin rouge verre",
                            "Hurluberlu rouge verre") ~ 12.5,
             PRODUCT %in% c("Cidre Rhuys","Kefir") ~ 25,
             PRODUCT %in% c("Rhum Brussels") ~ 3,
             str_detect(PRODUCT,"1L") ~ 100,
             TRUE ~ as.numeric(str_extract(PRODUCT," *([0-9]+) *[cC]*[lL]",group= 1))
           ),
           BOISSON = case_when(
             is.na(VOLUME_CL) ~ "",
             TRUE ~ PRODUCT_VIDE
           )
    ) %>%
    rename(PRODUCT_FULL = PRODUCT,
           PRODUCT = PRODUCT_VIDE)
}