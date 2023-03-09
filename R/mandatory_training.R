# Tag:ESR
# Tag:MandatoryTraining

make_monthly_mand_training <- function(table = "jpuh_ESR_Mandatory_Training_2",
                                       fn = pkg_env$default_functions) {
  #'
  #' Steps:
  #' 1. Calculate Expiring soon %
  #' 2. Calculate aggregate metrics [Defaults: mean, var, Na ratio] at lag periods
  #' 3. Save to file
  #' 
  #' @param table Name of data set at database
  #' @param fn Functions for aggregating data
  #' 
  #' @export
  #' @importFrom magrittr %>%
  #'

  mt <- tbl(pkg_env$con, table) %>% collect()

  Organisation <- Required <- `Expiring Soon` <- 
    `Expiring Soon %` <- `Compliance %` <- NULL
  
  mt <- mt %>% mutate(
    Ward = esr_to_allocate(Organisation),
    `Expiring Soon %` = 
      100 * as.double(`Expiring Soon`) / as.double(Required)
  )

  mt.process.f <- process_across_f(
    mt,
    "Date_stamp",
    c(`Compliance %`, `Expiring Soon %`),
    fn
  )

  mt.laged <- lagged_process(mt.process.f)

  mt.laged %>% saveRDS("processed_data/MandTraining_Monthly_Lagged.RData")
  mt.laged
}
