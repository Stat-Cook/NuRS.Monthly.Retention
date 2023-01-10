# Tag:Allocate
# Tag:Establishment

get.establishment.by.year <- function(.year) {
  #' Generate monthly establishment data sets
  #'
  #' @param .year Year of data to be processed
  #'
  #'
  sql.table <- glue("jpuh_Allocate_Shifts_Worked_Demographics_Combined_{.year}")

  sw <- tbl(con, sql.table) %>%
    select(`Owning Unit`, `Duty Date`) %>%
    collect() %>%
    mutate(
      `Duty Date` = as.Date(`Duty Date`, format = "%d/%m/%Y"),
      Ward = `Owning Unit`
    )

  establishment <- sw %>%
    group_by(Ward, `Duty Date`) %>%
    summarize(n = n())

  establishment %>%
    lag.data.frame("Duty Date", months = 3) %>%
    mutate(Year = year(LagedDate), Month = month(LagedDate)) %>%
    group_by(Ward, Year, Month) %>%
    summarize(`Ave Establishment Lag 3` = mean(n))
}


make.establishment.lag3 <- function(years = 2015:2020) {
  #' Calculate establishment at lag 3
  #'
  #' @param years vector of years to be querried across
  #'
  #' @export
  sw.list <- lapply(years, get.establishment.by.year)
  establishment.lag.3 <- do.call(rbind, sw.list)

  saveRDS(establishment.lag.3, "processed_data/Establishment_Lag_3.Rdata")
  establishment.lag.3
}
