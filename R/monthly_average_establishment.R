# Tag:Establishment


get.est.file <- function() {
  #' For generating Monthly Average Establishment
  #'
  #' @export

  est.file <- pkg.env$est.file

  if (!file.exists(est.file)) {
    est <- lapply(2015:2020, get.establishment)
    monthly.average.est <- do.call(rbind, est)
    saveRDS(monthly.average.est, est.file)
  } else {
    monthly.average.est <- readRDS(est.file)
  }

  monthly.average.est
}


add.establishment <- function(data, date.column) {
  #' Add 'Establishment' to a data set
  #'
  #' Aligns on 'date.column' and 'Ward'
  #'
  #'
  #' @param data Data frame to add est to.
  #' @param date.column Column of 'data' to be used as a date
  #' @export
  #'
  #' @importFrom dplyr mutate left_join
  #'
  monthly.average.est <- get.est.file()

  data %>%
    mutate(
      Year = year({{ date.column }}),
      Month = month({{ date.column }})
    ) %>%
    left_join(
      monthly.average.est,
      by = c(
        "Ward" = "Ward",
        "Year" = "Year",
        "Month" = "Month"
      )
    )
}
