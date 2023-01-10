

check.mstart <- function(month.start) {
  #' Limit lower value
  #'
  #' Make a function to limit a date to a lower value
  #'
  #' @param month.start  Date or date-time limit
  function(vals) {
    data.table::fifelse(vals > month.start, vals, month.start)
  }
}

check.mend <- function(month.end) {
  #' Limit upper value
  #'
  #' Make a function to limit a date to an upper value
  #'
  #' @param month.end  Date or date-time limit
  function(vals) {
    data.table::fifelse(vals > month.end, month.end, vals)
  }
}
#
# monthly.sickness.f <- function(df, k=1){
#
#   sel <- (df$`Absence Start Date` <= month.ends[k]) &
#     (df$`Absence End Date` >= month.starts[k])
#
#   month.data <- df[sel,]
#
#   month.data <- month.data %>%
#     mutate(
#       `First Day` = check.mstart(month.starts[k])(`Absence Start Date`),
#       `Last Day` = check.mend(month.ends[k])(`Absence End Date`),
#       `Month Starting` = month.starts[k],
#       `Relative Days Lost` = `Last Day` - `First Day` + 1,
#       `Prorata FTE Lost` = `FTE Days Lost` * `Relative Days Lost` / `Absence Length (Days)`,
#       Year = year(`Month Starting`),
#       Month = month(`Month Starting`)
#     )
#
#   month.data
# }

monthly.sickness.f2 <- function(df, month.start, month.end) {
  #' Calculate proportion of sickness within a given time window
  #'
  #' Only intended for use with `ESR sickness` data set
  #'
  #' @param df Data frame
  #' @param month.start Start date of time window
  #' @param month.end End date of time window
  #'

  sel <- (df$`Absence Start Date` <= month.end) &
    (df$`Absence End Date` >= month.start)

  month.data <- df[sel, ]

  check.mstart.f <- check.mstart(month.start)
  check.mend.f <- check.mend(month.end)

  month.data <- month.data %>%
    mutate(
      `First Day` = check.mstart.f(`Absence Start Date`),
      `Last Day` = check.mend.f(`Absence End Date`),
      `Month Starting` = month.start,
      `Relative Days Lost` = `Last Day` - `First Day` + 1,
      `Prorata FTE Lost` = `FTE Days Lost` * `Relative Days Lost` / `Absence Length (Days)`,
      Year = year(`Month Starting`),
      Month = month(`Month Starting`)
    )

  month.data
}

sickness.windows <- function(data, month.starts, month.ends) {
  #' Produce month-ward sickenss data
  #'
  #' Run `monthly.sickness.f2` across pairs of month-start/-end values
  #'
  #' @param data A data frame containing `Absence Start Date` and
  #' `Absence End Date` fields
  #' @param month.starts A vector of month start dates.
  #' @param month.ends A vector of month end dates.

  sickness.list <- map2(month.starts, month.ends, ~ monthly.sickness.f2(data, .x, .y))
  do.call(rbind, sickness.list)
}

make.monthly.sickness <- function(sql.table = "jpuh_ESR_Sickness_processed") {
  #' Produce time lost to sickness features
  #'
  #' Convert data set into ward-month sickness fte lost reports.
  #'
  #' @param sql.table Sql database name.
  #'
  #' @export
  #'
  #'
  sick <- tbl(pkg.env$con, sql.table) %>%
    select(Organisation, `Absence End Date`, `Absence Start Date`, `FTE Days Lost`) %>%
    collect() %>%
    mutate(
      `Absence Start Date` = as.Date(`Absence Start Date`),
      `Absence End Date` = as.Date(`Absence End Date`),
      `Absence Length (Days)` = (`Absence End Date` - `Absence Start Date`) / ddays(),
      `Absence Length (Days)` = `Absence Length (Days)` + 1
    ) %>%
    filter(Organisation %in% names(esr_to_allocate_list))

  monthly.sickness <- sickness.windows(sick, pkg.env$month.starts, pkg.env$month.ends) %>%
    mutate(Ward = esr_to_allocate(Organisation)) %>%
    add.establishment(`Month Starting`) %>%
    mutate(
      `Prorata FTE Lost per Establishment` = as.double(`Prorata FTE Lost` / `Average Est`)
    )

  lagged.sickness.f <- function(window) {
    lagged.group(monthly.sickness, "Month Starting", window) %>%
      dplyr::summarize("Lag {min(window)}-{max(window)} Sickness N Spells" := n(),
        "Lag {min(window)}-{max(window)} Sickness Ave FTE Lost per Est" := mean(`Prorata FTE Lost per Establishment`),
        "Lag {min(window)}-{max(window)} Sickness SD FTE Lost per Est" := sd(`Prorata FTE Lost per Establishment`),
        "Lag {min(window)}-{max(window)} Sickness FTE Lost per Est" := sum(`Prorata FTE Lost per Establishment`),
        Year = mean(Year), Month = mean(Month)
      )
  }

  ms <- lagged.process(lagged.sickness.f)

  ms %>% saveRDS("processed_data/Sickness_Monthly_Lagged.RData")

  ms
}
