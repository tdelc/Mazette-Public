library(lubridate)

add_vacances_info <- function(df) {
  df %>%
    mutate(
      VACANCES_FR = case_when(
        # 2022-2023
        DATE >= ymd("2022-10-24") & DATE <= ymd("2022-11-06") ~ TRUE, # Toussaint
        DATE >= ymd("2022-12-26") & DATE <= ymd("2023-01-08") ~ TRUE, # Noël
        DATE >= ymd("2023-02-20") & DATE <= ymd("2023-03-05") ~ TRUE, # Carnaval
        DATE >= ymd("2023-05-01") & DATE <= ymd("2023-05-14") ~ TRUE, # Pâques
        DATE >= ymd("2023-07-08") & DATE <= ymd("2023-08-27") ~ TRUE, # Eté

        # 2023-2024
        DATE >= ymd("2023-10-23") & DATE <= ymd("2023-11-05") ~ TRUE, # Toussaint
        DATE >= ymd("2023-12-25") & DATE <= ymd("2024-01-07") ~ TRUE, # Noël
        DATE >= ymd("2024-02-26") & DATE <= ymd("2024-03-10") ~ TRUE, # Carnaval
        DATE >= ymd("2024-04-29") & DATE <= ymd("2024-05-12") ~ TRUE, # Pâques
        DATE >= ymd("2024-07-06") & DATE <= ymd("2024-08-25") ~ TRUE, # Eté

        # 2024-2025
        DATE >= ymd("2024-10-21") & DATE <= ymd("2024-11-03") ~ TRUE, # Toussaint
        DATE >= ymd("2024-12-23") & DATE <= ymd("2025-01-05") ~ TRUE, # Noël
        DATE >= ymd("2025-02-24") & DATE <= ymd("2025-03-09") ~ TRUE, # Carnaval
        DATE >= ymd("2025-04-28") & DATE <= ymd("2025-05-11") ~ TRUE, # Pâques
        DATE >= ymd("2025-07-05") & DATE <= ymd("2025-08-24") ~ TRUE, # Eté

        TRUE ~ FALSE
      ),
      VACANCES_NL = case_when(
        # 2022-2023
        DATE >= ymd("2022-10-31") & DATE <= ymd("2022-11-06") ~ TRUE, # Herfst
        DATE >= ymd("2022-12-26") & DATE <= ymd("2023-01-08") ~ TRUE, # Kerst
        DATE >= ymd("2023-02-20") & DATE <= ymd("2023-02-26") ~ TRUE, # Krokus
        DATE >= ymd("2023-04-03") & DATE <= ymd("2023-04-16") ~ TRUE, # Pasen
        DATE >= ymd("2023-07-01") & DATE <= ymd("2023-08-31") ~ TRUE, # Zomer

        # 2023-2024
        DATE >= ymd("2023-10-30") & DATE <= ymd("2023-11-05") ~ TRUE, # Herfst
        DATE >= ymd("2023-12-25") & DATE <= ymd("2024-01-07") ~ TRUE, # Kerst
        DATE >= ymd("2024-02-12") & DATE <= ymd("2024-02-18") ~ TRUE, # Krokus
        DATE >= ymd("2024-04-01") & DATE <= ymd("2024-04-14") ~ TRUE, # Pasen
        DATE >= ymd("2024-07-01") & DATE <= ymd("2024-08-31") ~ TRUE, # Zomer

        # 2024-2025
        DATE >= ymd("2024-10-28") & DATE <= ymd("2024-11-03") ~ TRUE, # Herfst
        DATE >= ymd("2024-12-23") & DATE <= ymd("2025-01-05") ~ TRUE, # Kerst
        DATE >= ymd("2025-03-03") & DATE <= ymd("2025-03-09") ~ TRUE, # Krokus
        DATE >= ymd("2025-04-07") & DATE <= ymd("2025-04-21") ~ TRUE, # Pasen
        DATE >= ymd("2025-07-01") & DATE <= ymd("2025-08-31") ~ TRUE, # Zomer

        TRUE ~ FALSE
      ),
      TYPE_VACANCES = case_when(
        VACANCES_FR & VACANCES_NL ~ "Communes",
        VACANCES_FR ~ "FR uniquement",
        VACANCES_NL ~ "NL uniquement",
        TRUE ~ "Hors vacances"
      )
    )
}
