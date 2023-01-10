lag.data.frame <- function(data, date_col, months){
  #' Produce a data set where observations have been lagged in time by a given value. 
  #' If multiple lagging months are given - the data set produces duplicates.
  #'  
  #' @param data The data frame to be lagged
  #' @param date_col The column of `data` to use for lagging
  #' @param months [Integer] The number of months to lag data by.  Expects integer value(s)
  #' 
  #' @importFrom lubridate %m+% 
  #' 
  date_col <- sym(date_col)
  lagged.data <- lapply(
    months,
    function(i) data %>% dplyr::mutate(LagedDate = {{date_col}} %m+% months(i))
  )
  do.call(rbind, lagged.data)
}



lagged.group <- function(data, date.col, window=3:4, ...){
  
  #' Convert data set into a lagged, grouped version. 
  #' 
  #' @param data The data set to be converted NB: if no grouping variables given (`...`) 
  #'   data is expected to contain a column `Ward`. 
  #' @param date.col The column of  data to treat as the date for lagging.
  #' @param window [optional] The number of months to lag the data by.
  #' @param ...  [optional] Columns of data to group by.  If none given - 
  #'   defaults to `Ward` and the `Year`/`Month` produced by lagging `date.col`
  #' 
  #' @importFrom lubridate year month
  #' 
  frame <- lag.data.frame(data, date.col, window) %>% 
    mutate(Year = year(LagedDate), Month = month(LagedDate))
  
  if (length(list(...)) == 0){
    return(group_by(frame, Ward, Year, Month))   
  }
  
  return(group_by(frame, ...))   
}