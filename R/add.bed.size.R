

add.bed.size <- function(data, date.column, ward) {
  #' Add 'bed size' to data set
  #'
  #' Add a column listing number of beds a ward has to a data set -
  #' depending on ward and date.
  #'
  #' @param data A data frame to add column to
  #' @param date.column Name of the 'Date' reference column in data
  #' @param ward Name of a ward (string)
  #'
  ward.beds <- pkg.env$beds.frame %>% filter(Ward == ward)
  merge.on.last(data, ward.beds, date.column, `Effective From`) %>%
    select(-Ward)
}


add.bed.size.ws <- function(data, date.column, ward) {
  #' Add 'bed size' to data set
  #'
  #' Add a column listing number of beds a ward has to a data set -
  #' depending on ward and date (where the data is a POSIXct object).
  #'
  #' @param data A data frame to add column to
  #' @param date.column Name of the 'Date' reference column in data
  #' @param ward Name of a ward (string)
  #'
  ward.beds <- pkg.env$beds.frame %>%
    filter(Ward == ward) %>%
    mutate(`Effective From` = as.POSIXct(`Effective From`))
  merge.on.last(data, ward.beds, {{ date.column }}, `Effective From`) %>%
    select(-Ward)
}

add.bed.size.all <- function(data, date.column) {
  #' Add 'bed size' to data set
  #'
  #' Add a column listing number of beds a ward has to a data set -
  #' depending on ward and date.  Iterates across all 'Ward' in data set.
  #'
  #' @param data A data frame to add column to
  #' @param date.column Name of the 'Date' reference column in data
  #'
  data %>%
    group_by(Ward) %>%
    group_modify(
      function(data, grp) add.bed.size(data, {{ date.column }}, grp$Ward)
    )
}

add.bed.size.all.ws <- function(data, date.column) {
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
    group_by(Ward) %>%
    group_modify(
      function(data, grp) add.bed.size.ws(data, {{ date.column }}, grp$Ward)
    )
}
