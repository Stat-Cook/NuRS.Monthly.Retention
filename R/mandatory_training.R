# Tag:ESR
# Tag:MandatoryTraining

make.monthly.mand.training <- function(table = "jpuh_ESR_Mandatory_Training_2", 
                                       fn = pkg.env$default_functions) {
  #' 
  #' Steps:
  #' 1. Calculate Expiring soon %
  #' 2. Calcualte aggregate metrics [Defaults: mean, var, Na ratio] at lag periods 
  #' 3. Save to file
  #' @export
  #' @importFrom magrittr %>%
  #' 
  
  mt <- tbl(pkg.env$con, table) %>% collect()
  
  mt <- mt %>% mutate(
    Ward = esr_to_allocate(Organisation),
    `Expiring Soon %` = 100 * as.double(`Expiring Soon`) / as.double(Required)  
  )
  
  mt.process.f <- process.across.f(
    mt, 
    "Date_stamp", 
    c(`Compliance %`, `Expiring Soon %`), 
    fn
  )
  
  mt.laged <- lagged.process(mt.process.f)
  
  mt.laged %>% saveRDS("processed_data/MandTraining_Monthly_Lagged.RData")
}