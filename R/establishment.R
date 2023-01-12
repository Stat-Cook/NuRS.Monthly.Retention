# Tag:Allocate
# Tag:Establishment

get_establishment_by_year <- function(.year, .con = pkg_env$con) {
  #' Generate monthly establishment data sets
  #'
  #' @param .year Year of data to be processed
  #' @param .con A DBI connection object
  #'
  #'
  sql_table <- glue("jpuh_Allocate_Shifts_Worked_Demographics_Combined_{.year}")

  sw <- tbl(.con, sql_table) %>%
    select(.data$`Owning Unit`, .data$`Duty Date`) %>%
    collect() %>%
    mutate(
      `Duty Date` = as.Date(.data$`Duty Date`, format = "%d/%m/%Y"),
      Ward = .data$`Owning Unit`
    )

  establishment <- sw %>%
    group_by(.data$Ward, .data$`Duty Date`) %>%
    summarize(n = n())

  establishment %>%
    lag_data_frame("Duty Date", months = 3) %>%
    mutate(
      Year = year(.data$LagedDate),
      Month = month(.data$LagedDate)
    ) %>%
    group_by(.data$Ward, .data$Year, .data$Month) %>%
    summarize(`Ave Establishment Lag 3` = mean(n))
}


make_establishment_lag3 <- function(years = 2015:2020) {
  #' Calculate establishment at lag 3
  #'
  #' @param years vector of years to be querried across
  #'
  #' @export
  sw_list <- lapply(years, get_establishment_by_year)
  establishment_lag_3 <- do.call(rbind, sw_list)

  saveRDS(establishment_lag_3, "processed_data/Establishment_Lag_3.Rdata")
  establishment_lag_3
}
