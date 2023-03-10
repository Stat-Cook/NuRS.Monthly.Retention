# Tag:ShiftsWorked
# Tag:Allocate
# Tag:Establishment

sw_to_date <- function(values) {
  #' Convert char to date
  #'
  #' Cast data to date [specifically designed
  #'   for use with shifts worked data set].
  #'
  #' @param values Vector of character values
  #'
  #' @importFrom lubridate as_date
  as_date(values, format = "%d/%m/%Y")
}

shift_worked_table <- function(.year) {
  #' Get sql table string for  shifts worked table by year
  #'
  #' @param .year Year of data to be queried
  #' @importFrom glue glue
  #'
  glue("JPUH_Allocate_Shifts_Worked_Demographics_Combined_{.year}_tsv")
}

fetch_sw_est <- function(.year, .con = pkg_env$con) {
  #' Get shifts worked data by year
  #'
  #' @param .year Year of data to be queried
  #' @param .con SQL engine connection
  #' @importFrom dplyr mutate
  #'
  tbl_string <- as.character(shift_worked_table(.year))
  sql_query <- tbl(
    .con,
    tbl_string
  )

  raw_data <- collect(sql_query)

  mutate(raw_data, `Duty Date` = sw_to_date(.data$`Duty Date`))
}


get_establishment <- function(.year) {
  #' Fetch ward establishments
  #'
  #' @param .year Year of data to be queried and processed
  #'
  #' @importFrom magrittr %>%
  #' @importFrom dplyr mutate summarize group_by
  #'
  `Owning Unit` <- Ward <- `Duty Date` <- Count <- NULL
  
  fetch_sw_est(.year) %>%
    mutate(Ward = `Owning Unit`) %>%
    group_by(Ward, `Duty Date`) %>%
    summarize(Count = n()) %>%
    group_by(
      Ward,
      Year = year(`Duty Date`),
      Month = month(`Duty Date`)
    ) %>%
    summarize(`Average Est` = mean(Count))
}
