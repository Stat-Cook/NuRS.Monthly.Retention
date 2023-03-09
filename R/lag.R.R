# Tag:Lag
# Tag:Date

lag_data_frame <- function(data, date_col, months) {
  #' Produce a data set where observations have been lagged in time
  #'   by a given value.
  #' If multiple lagging months are given - the data set produces duplicates.
  #'
  #' @param data The data frame to be lagged
  #' @param date_col The column of `data` to use for lagging
  #' @param months [Integer] The number of months to lag data by.
  #'   Expects integer value(s)
  #'
  #' @importFrom lubridate %m+%
  #' @importFrom dplyr mutate
  #'
  date_col <- sym(date_col)
  lagged_data <- lapply(
    months,
    function(i) mutate(data, LagedDate = {{ date_col }} %m+% months(i))
  )
  do.call(rbind, lagged_data)
}



lagged_group <- function(data, date_col, window = 3:4, ...) {
  #' Convert data set into a lagged, grouped version.
  #'
  #' @param data The data set to be converted
  #'   NB: if no grouping variables given (`...`)
  #'   data is expected to contain a column `Ward`.
  #' @param date_col The column of  data to treat as the date for lagging.
  #' @param window [optional] The number of months to lag the data by.
  #' @param ...  [optional] Columns of data to group by.  If none given -
  #'   defaults to `Ward` and the `Year`/`Month` produced by lagging `date.col`
  #'
  #' @importFrom lubridate year month
  #'
  frame <- lag_data_frame(data, date_col, window) %>%
    mutate(Year = year(.data$LagedDate), Month = month(.data$LagedDate))

  if (length(list(...)) == 0) {
    return(group_by(frame, .data$Ward, .data$Year, .data$Month))
  }

  return(group_by(frame, ...))
}
