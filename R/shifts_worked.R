# Tag:ShiftsWorked
# Tag:Allocate

sw_to_date <- function(values) lubridate::as_date(values, format = "%d/%m/%Y")



fetch_sw <- function(year) {
  #' Query and clean Shifts Worked data
  #'
  #' @param year Numeric year of data to querry

  allocate.wards <- unlist(esr_to_allocate_list)

  tbl(
    pkg_env$con,
    glue::glue("jpuh_Allocate_Shifts_Worked_Demographics_Combined_{year}")
  ) %>%
    filter(`Owning Unit` %in% allocate.wards) %>%
    select(`Duty Date`, `Actual Work`, `Fulfilment Type`, `Owning Unit`) %>%
    collect() %>%
    mutate(`Duty Date` = sw_to_date(`Duty Date`))
}

sw.daily.f <- function(year) {
  #' Query and convert shifts worked data set to a report of quantity of work
  #' done each day by each staff group
  #'
  #' @param year Numeric year of data to look up

  s <- fetch_sw(year)

  daily.work <- s %>%
    mutate(Date = as.Date(`Duty Date`, format = "%d/%m/%Y")) %>%
    filter(`Owning Unit` %in% esr_to_allocate_list) %>%
    mutate(Ward = `Owning Unit`) %>%
    group_by(Ward, `Fulfilment Type`, `Date`) %>%
    summarise(`Total Work` = sum(`Actual Work`)) #

  daily.work <- expand.grid(
    Ward = unlist(esr_to_allocate_list),
    Date = seq(min(daily.work$Date), max(daily.work$Date), by = "d"),
    `Fulfilment Type` = unique(daily.work$`Fulfilment Type`)
  ) %>%
    as_tibble() %>%
    left_join(daily.work) %>%
    replace_na(list(`Total Work` = 0)) %>%
    arrange(Ward, `Fulfilment Type`, `Date`) %>%
    mutate(
      Year = year(Date),
      Month = month(Date)
    )

  daily.work
}

sum.to.proportion <- function(data) {
  #' Convert total work to proportion of work done by each staff group.
  #'
  #' @param data A data frame
  data %>%
    mutate(
      across(contains("sum(Total Work)")) / rowSums(across(contains("sum(Total Work)")))
    ) %>%
    rename_with(~ gsub("sum(Total Work)", "prop(Total Work)", .x, fixed = TRUE))
}

make.shifts.worked <- function() {
  #' Generate the shifts worked data sets (No Demographics)
  #'
  #' @export
  sw.daily <- lapply(2015:2020, sw.daily.f) %>% do.call(rbind, .)


  sw.daily.with.beds <- sw.daily %>% add_bed_size_all("Date")

  sw.daily.per.bed <- sw.daily.with.beds %>%
    filter(!is.na(Beds)) %>%
    mutate(
      `Total Work per Bed` = `Total Work` / Beds
    )

  sw.f <- function(window) {
    all.fts <- lag.data.frame(sw.daily.per.bed, "Date", window) %>%
      mutate(Year = year(LagedDate), Month = month(LagedDate)) %>%
      group_by(Ward, `Fulfilment Type`, Year, Month) %>%
      summarize(
        "Lag {min(window)}-{max(window)} mean(Total Work) per Bed" := mean(`Total Work per Bed`, na.rm = T),
        "Lag {min(window)}-{max(window)} sum(Total Work) per Bed" := sum(`Total Work per Bed`, na.rm = T),
        "Lag {min(window)}-{max(window)} var(Total Work) per Bed" := var(`Total Work per Bed`, na.rm = T),
        "Lag {min(window)}-{max(window)} IQR(Total Work) per Bed" := IQR(`Total Work per Bed`, na.rm = T)
      )

    fts <- unique(all.fts$`Fulfilment Type`)

    each.ft <- lapply(
      fts,
      function(ft) {
        all.fts %>%
          as_tibble() %>%
          filter(`Fulfilment Type` == ft) %>%
          select(-`Fulfilment Type`) %>%
          rename_with(function(i) paste(i, "Fulfilment:", ft), contains("Lag"))
      }
    )
    result <- reduce(
      each.ft,
      function(x, y) full_join(x, y, by = c("Ward", "Year", "Month"))
    )
    result[is.na(result)] <- 0

    result
  }

  sw.lagged <- lagged.process(sw.f)
  sw.lagged.proportions <- lapply(sw.lagged, sum.to.proportion)

  sw.lagged %>% saveRDS("processed_data/ShiftsWorked_Monthly_Lagged.RData")
  sw.lagged.proportions %>% saveRDS("processed_data/ShiftsWorkedProportions_Monthly_Lagged.RData")

  sw.lagged
}
