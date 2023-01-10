# Tag:ShiftsWorked
# Tag:Allocate
# Tag:Establishment

sw_to_date <- function(values) {
  #' Convert char to date 
  #' 
  #' Cast data to date [specifically designed for use with shifts worked data set].
  #' 
  #' @param value Vector of character values
  #' 
  #' @importFrom lubridate as_date
  as_date(values, format="%d/%m/%Y")
}

shift_worked_table <- function(.year){
  #' Get sql table string for  shifts worked table by year
  #' 
  #' @param .year Year of data to be queried
  #' @importFrom glue glue
  #'
  glue("JPUH_Allocate_Shifts_Worked_Demographics_Combined_{.year}_tsv")
} 

fetch_sw <- function(.year, .con=pkg.env$con) {
  #' Get shifts workled data by year
  #' 
  #' @param .year Year of data to be querried
  #' @param .con SQL engine connection
  #' @importFrom dplyr mutate
  #' 
  #' @param .year 
  tbl.string <- as.character(shift_worked_table(.year))
  sql.query <- tbl(
    .con, 
    tbl.string
  )
  
  # filter.to.acuity <- filter(sql.query, `Owning Unit` %in% allocate.wards) 
  raw.data <- collect(sql.query)

  mutate(raw.data, `Duty Date` = sw_to_date(`Duty Date`))
}


get.establishment <- function(.year) {
  #' Fetch ward establishments 
  #' 
  #' @param .year Year of data to be queried and processed
  #' 
  #' @importFrom magrittr %>% 
  #' @importFrom dplyr mutate summarize group_by
  #' 
  fetch_sw(.year) %>%
    mutate(Ward = `Owning Unit`)  %>%  
    group_by(Ward, `Duty Date`) %>% 
    summarize(Count = n()) %>% 
    group_by(
      Ward, 
      Year = year(`Duty Date`), 
      Month=month(`Duty Date`)) %>%
    summarize(`Average Est` = mean(Count))
}


