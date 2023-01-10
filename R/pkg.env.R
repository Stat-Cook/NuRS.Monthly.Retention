# Tag:Environment

library(lubridate)

pkg.env <- new.env(parent = emptyenv())

pkg.env$start.date.inclusive <- as.Date("2016-08-01")
pkg.env$end.date.exclusive <- as.Date("2020-06-01")



pkg.env$raw_folder <- "raw_data"
pkg.env$processed_folder <- "processed_data"
pkg.env$result_folder <- "result_data"
pkg.env$con <- open.connection()

pkg.env$est.file <- "processed_data/Monthly_average_establishment.Rdata"
pkg.env$pas.file <- "processed_data/Ward_Stay_Offset.RData"
pkg.env$allocate_shit_aggregate_file <- "processed_data/Assignment_Shift_Aggregate.RData"

{
  beds.frame <- read.csv(glue("{pkg.env$raw_folder}/JP Beds by Date.csv"))
  colnames(beds.frame) <- c("Index", "Ward", "Effective From", "Beds")
  
  pkg.env$beds.frame <- beds.frame %>% 
    dplyr::select(-Index) %>% 
    dplyr::mutate(
      `Effective From` = as.Date(`Effective From`, format="%d/%m/%Y")
    )
}

pkg.env$default_functions <- list(
  "Mean" = function(i) mean(i, na.rm=T),
  "Na Ratio" = function(i) sum(is.na(i)) / length(i), 
  "Var" = function(i) var(i, na.rm=T),
  ".N." = length
)

pkg.env$all.months <- head(
  seq(pkg.env$start.date.inclusive, pkg.env$end.date.exclusive, by="month"), 
  -1
)

sickness.months <- seq(
  pkg.env$start.date.inclusive %m-% months(12), 
  pkg.env$end.date.exclusive, 
  by="month"
)

pkg.env$month.starts <- head(sickness.months, -1)
pkg.env$month.ends <- tail(sickness.months, -1) %m-% days(1)



