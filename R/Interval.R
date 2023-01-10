# Tag:Date

truncate.start <- function(interval.start) {
  function(vals) {
    data.table::fifelse(vals > interval.start, vals, interval.start)
  }
}

truncate.end <- function(interval.end) {
  function(vals) {
    data.table::fifelse(vals > interval.end, interval.end, vals)
  }
}


interval.f <- function(start.date = "2015-01-01",
                       end.date = "2021-12-01",
                       by = "Month",
                       inclusive = TRUE) {
  #' @importFrom magrittr %>%

  # Define start and end points as datetimes
  start.POSIXct <- as.POSIXct(start.date)
  end.POSIXct <- as.POSIXct(end.date)

  # Produce sequence of dates-times
  intervals <- seq(start.POSIXct, end.POSIXct,
    by = tolower(by)
  )
  if (inclusive) {
    intervals <- unique(c(start.POSIXct, intervals, end.POSIXct))
  }

  # Split sequence into start and end of `by` duration
  interval.starts <- head(intervals, -1)
  interval.ends <- tail(intervals, -1)
  k <- length(interval.starts)

  # Produce an internal function   to select appropriate data and
  # limit period to
  temp.f <- function(data, i = 1, start.col, end.col) {
    interval.data <- data %>% filter(
      {{ start.col }} <= interval.ends[i],
      {{ end.col }} >= interval.starts[i]
    )

    period.start.f <- truncate.start(interval.starts[i])
    period.end.f <- truncate.end(interval.ends[i])

    interval.data %>% mutate(
      `Period Start` = period.start.f({{ start.col }}),
      `Period End` = period.end.f({{ end.col }}),
      "{by} Starting" := interval.starts[i]
    )
  }

  # Produce a function to map a data set into the intervals defined, truncating
  # individual periods to interval start and end,
  function(data, start.col, end.col) {
    lapply(
      1:k,
      function(i) temp.f(data, i, {{ start.col }}, {{ end.col }})
    ) %>% do.call(rbind, .)
  }
}

DEFAULT.INTERVAL.FUNC <- interval.f()
