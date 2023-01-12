# Tag:Complete


complete_f <- function(data) {
  #' Check which rows of a data frame are complete
  #'
  #' @param data A data frame
  !is.na(data) %>% apply(1, any)
}

get_complete <- function(data) {
  #' Select only complete rows of data set
  #'
  #' @param data A data frame
  #'
  #' @export
  index <- complete_f(data)
  data[index, ]
}
