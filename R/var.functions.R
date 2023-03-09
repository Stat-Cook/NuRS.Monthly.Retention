var.functions <- function(data) {
  #' Calculate staffing variance
  #'
  #' Calculate:
  #' 1. Variance between average staffing of the shifts
  #' 2. Variance within shifts of staffing.
  #'
  #' @param data A data frame with fields: 'Ward', 'Year', 'Month' and 'Shift Type'
  #' 
  
  Ward <- Year <- Month <- `Shift Type` <- NULL

  averages <- data %>%
    group_by(Ward, Year, Month, `Shift Type`) %>%
    summarize(
      across(where(is.numeric), mean, .names = "{.col}")
    )

  merged.averages <- data %>%
    select("Ward", "Year", "Month", "Shift Type") %>%
    left_join(averages)

  ward.shift.month.ave <- tibble(merged.averages) %>%
    select(-c("Ward", "Year", "Month", "Shift Type"))

  ward.shift.month.ave.subtract <- select(
    tibble(data),
    colnames(ward.shift.month.ave)
  ) - ward.shift.month.ave

  de.meaned <- data %>%
    select("Ward", "Shift Type", "Date") %>%
    cbind(ward.shift.month.ave.subtract)

  list(
    data = data,
    averages = mutate(averages, Date = as.Date(paste(Year, Month, "01", sep = "-"))),
    `De-meaned` = de.meaned
  )
}
