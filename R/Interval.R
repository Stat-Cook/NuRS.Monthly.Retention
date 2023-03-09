# Tag:Date

truncate_start <- function(interval_start) {
  #' Function factory - limit vector to a minimum
  #' 
  #' @param interval_start Lower bound of interval
  #' @importFrom data.table fifelse
  function(vals) {
    fifelse(vals > interval_start, vals, interval_start)
  }
}

truncate_end <- function(interval_end) {
  #' Function factory - limit vector to a maximum
  #' 
  #' @param interval_end Upper bound of interval
  #' @importFrom data.table fifelse
  function(vals) {
    fifelse(vals > interval_end, interval_end, vals)
  }
}


interval_f <- function(start_date = "2015-01-01",
                       end_date = "2021-12-01",
                       by = "Month",
                       inclusive = TRUE) {
  
  #' Data to intervals function factory.
  #' 
  #' @param start_date Start of first interval
  #' @param end_date End of last interval
  #' @param by Width of intervals
  #' @param inclusive inclusive of end_date or not
  #' 
  #' @importFrom magrittr %>%
  #' @importFrom utils head tail 

  # Define start and end points as datetimes
  start_POSIXct <- as.POSIXct(start_date)
  end_POSIXct <- as.POSIXct(end_date)

  # Produce sequence of dates-times
  intervals <- seq(start_POSIXct, end_POSIXct,
    by = tolower(by)
  )
  if (inclusive) {
    intervals <- unique(c(start_POSIXct, intervals, end_POSIXct))
  }

  # Split sequence into start and end of `by` duration
  interval_starts <- head(intervals, -1)
  interval_ends <- tail(intervals, -1)
  k <- length(interval_starts)

  # Produce an internal function   to select appropriate data and
  # limit period to
  temp_f <- function(data, i = 1, start_col, end_col) {
    interval_data <- data %>% filter(
      {{ start_col }} <= interval_ends[i],
      {{ end_col }} >= interval_starts[i]
    )

    period_start_f <- truncate_start(interval_starts[i])
    period_end_f <- truncate_end(interval_ends[i])

    interval_data %>% mutate(
      `Period Start` = period_start_f({{ start_col }}),
      `Period End` = period_end_f({{ end_col }}),
      "{by} Starting" := interval_starts[i]
    )
  }

  # Produce a function to map a data set into the intervals defined, truncating
  # individual periods to interval start and end,
  function(data, start_col, end_col) {
    .list <- lapply(
      1:k,
      function(i) temp_f(data, i, {{ start_col }}, {{ end_col }})
    )

    do.call(rbind, .list)
  }
}

DEFAULT_INTERVAL_FUNC <- interval_f()
