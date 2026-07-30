sql_val <- function(x) {
  if (is.character(x)) ifelse(is.na(x), "NULL",
                              paste0("'", gsub("'", "''", x), "'"))
  else ifelse(is.na(x), "NULL", as.character(x))
}

sql_req <- function(sql, 
                    account_id = Sys.getenv("CF_ACCOUNT_ID"),
                    db_id = Sys.getenv("CF_D1_ID"),
                    api_token = Sys.getenv("CF_API_TOKEN")) {
  r <- httr::POST(
    sprintf("https://api.cloudflare.com/client/v4/accounts/%s/d1/database/%s/query",
            account_id, db_id),
    httr::add_headers(Authorization = paste("Bearer", api_token)),
    body = list(sql = sql), encode = "json")
  
  res <- httr::content(r)
  if (!isTRUE(res$success))
    stop("D1 a refusé la requête : ",
         paste(vapply(res$errors, `[[`, "", "message"), collapse = " | "))
  invisible(res)
}

sql_select <- function(sql) {
  r <- httr::POST(
    sprintf("https://api.cloudflare.com/client/v4/accounts/%s/d1/database/%s/query",
            Sys.getenv("CF_ACCOUNT_ID"), Sys.getenv("CF_D1_ID")),
    httr::add_headers(Authorization = paste("Bearer", Sys.getenv("CF_API_TOKEN"))),
    body = list(sql = sql), encode = "json"
  )
  httr::stop_for_status(r)
  dplyr::bind_rows(httr::content(r)$result[[1]]$results)
}

df_to_paquets <- function(db, taille = 200){
  lignes <- paste0("(", db %>% 
                     mutate(across(everything(), sql_val)) %>%
                     apply(1, paste, collapse = ", "), ")")
  
  # On insère par paquets : un INSERT géant dépasserait les limites de D1.
  split(lignes, ceiling(seq_along(lignes) / 200))
}