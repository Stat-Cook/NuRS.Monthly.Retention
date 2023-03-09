# Tag:Environment

library(lubridate)
library(readxl)

pkg_env <- new.env(parent = emptyenv())

pkg_env$start.date.inclusive <- as.Date("2016-08-01")
pkg_env$end.date.exclusive <- as.Date("2020-06-01")



pkg_env$raw_folder <- "raw_data"
pkg_env$processed_folder <- "processed_data"
pkg_env$result_folder <- "result_data"

pkg_env$con <- NA
class(pkg_env$con) <- "unconnected"

pkg_env$est.file <- "processed_data/Monthly_average_establishment.Rdata"
pkg_env$pas.file <- "processed_data/Ward_Stay_Offset.RData"
pkg_env$allocate_shit_aggregate_file <- "processed_data/Assignment_Shift_Aggregate.RData"

{
  beds.frame <- read.csv(glue("{pkg_env$raw_folder}/JP_Beds_by_Date.csv"))
  colnames(beds.frame) <- c("Index", "Ward", "Effective From", "Beds")

  pkg_env$beds.frame <- beds.frame %>%
    dplyr::select(-"Index") %>%
    dplyr::mutate(
      `Effective From` = as.Date(`Effective From`, format = "%d/%m/%Y")
    )
}

pkg_env$default_functions <- list(
  "Mean" = function(i) mean(i, na.rm = T),
  "Na Ratio" = function(i) sum(is.na(i)) / length(i),
  "Var" = function(i) var(i, na.rm = T),
  ".N." = length
)

pkg_env$all.months <- head(
  seq(pkg_env$start.date.inclusive, pkg_env$end.date.exclusive, by = "month"),
  -1
)

sickness.months <- seq(
  pkg_env$start.date.inclusive %m-% months(12),
  pkg_env$end.date.exclusive,
  by = "month"
)

pkg_env$month.starts <- head(sickness.months, -1)
pkg_env$month.ends <- tail(sickness.months, -1) %m-% days(1)

pkg_env$model.cols <- c("Owning Unit", "Duty Date")

pkg_env$demographic.cols <- c(
  "Gender", "Ethnic Origin", "Disability",
  "Marital Status", "Sexual Orientation",
  "Nationality",
  "Country of Birth",
  "Age Band", "Length of Service Band",
  "Religious Belief", "AfC Pay Band", "Ethnicity Group",
  "Disability Category"
)

#' @importFrom readxl read_excel

{
  # Load demographics cardinality reduction spreadsheet
  pkg_env$demo.list <- list()
  for (demo in pkg_env$demographic.cols) {
    frm <- read_excel("raw_data/DemographicAnalysisGroups.xlsx", demo)
    values <- frm$`For Analysis`
    names(values) <- frm[[demo]]
    pkg_env$demo.list[[demo]] <- values
  }
}