

add_bed_size <- function(data, date_column, ward) {
  #' Add 'bed size' to data set
  #'
  #' Add a column listing number of beds a ward has to a data set -
  #' depending on ward and date.
  #'
  #' @param data A data frame to add column to
  #' @param date.column Name of the 'Date' reference column in data
  #' @param ward Name of a ward (string)
  #'
  ward_beds <- pkg_env$beds.frame %>% filter(.data$Ward == ward)
  merge.on.last(data, ward_beds, date_column, `Effective From`) %>%
    select(-.data$Ward)
}


add_bed_size_ws <- function(data, date_column, ward) {
  #' Add 'bed size' to data set
  #'
  #' Add a column listing number of beds a ward has to a data set -
  #' depending on ward and date (where the data is a POSIXct object).
  #'
  #' @param data A data frame to add column to
  #' @param date.column Name of the 'Date' reference column in data
  #' @param ward Name of a ward (string)
  #'
  ward_beds <- pkg_env$beds.frame %>%
    filter(.data$Ward == ward) %>%
    mutate(`Effective From` = as.POSIXct(.data$`Effective From`))
  merge.on.last(data, ward_beds, {{ date_column }}, .data$`Effective From`) %>%
    select(-.data$Ward)
}


add_bed_size_all <- function(data, date_column) {
  #' Add 'bed size' to data set
  #'
  #' Add a column listing number of beds a ward has to a data set -
  #' depending on ward and date.  Iterates across all 'Ward' in data set.
  #'
  #' @param data A data frame to add column to
  #' @param date.column Name of the 'Date' reference column in data
  #'

  n.ward <- length(unique(data$Ward))
  pb <- progress_bar_init(n.ward)

  data %>%
    group_by(.data$Ward) %>%
    group_modify(
      function(data, grp) {
        frm <- add_bed_size(data, {{ date_column }}, grp$Ward)
        pb$tick()
        frm
      }
    )
}


add_bed_size_all_ws <- function(data, date_column) {
  #' Add 'bed size' to data set
  #'
  #' Add a column listing number of beds a ward has to a data set -
  #' depending on ward and date (where the data is a POSIXct object).
  #' Iterates across all 'Ward' in data set.
  #'
  #' @param data A data frame to add column to
  #' @param date.column Name of the 'Date' reference column in data
  #'
  data %>%
    group_by(.data$Ward) %>%
    group_modify(
      function(data, grp) add_bed_size_ws(data, {{ date_column }}, grp$Ward)
    )
}
