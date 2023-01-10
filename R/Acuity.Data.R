
acuity.score.f <- function(L0, L1A, L1B, L2, L3){
  #' Calculate a weighted acuity score using the SNCT multipliers.
  L0*0.99 + L1A * 1.39 + L1B * 1.72 + L2 * 1.97 + L3 * 5.96
}

acuity.score <- Vectorize(acuity.score.f)


make.monthly.acuity <- function(sql.table = "jpuh_AllocateAcuity",
                                fn = pkg.env$default_functions) {
  #' Monthly Acuity data summary
  #' 
  #' Steps:
  #' 1. Get rows of data where `Name` like "Level"
  #' 2. Pivot data to ward-date vs Acuity  
  #' 3. Exclude rows where no data was entered
  #' 4. Fill in missing values from remaining cases with zero. 
  #' 5. Add bed sizes. 
  #' 6. Calculate Acuity frequencies per bed and Acuity Score (SNCT multipliers)
  #' 7. Calculate aggregate metrics (Defaults: mean, var, Na ratio) at lag periods 
  #' 8. Save to file
  #' 
  #' @param sql.table Name of relevant sql table
  #' @param fn List of functions to use for summarizing data
  #' 
  #' @export
  #' @importFrom magrittr %>%
  #' @importFrom reshape2 dcast
  #' @import dplyr
  #' 
  allocate <- tbl(pkg.env$con, sql.table) %>% 
    filter(Name %like% "Level%") %>%
    collect() %>% 
    mutate(
      ValidDate = as.Date(ValidDate, format="%d/%m/%y"),
      Ward = Unit
    )

  pivoted.allocate <- allocate %>% 
    dcast(Ward + ValidDate + CensusPeriod ~ Name, value.var = "Count")  # Ward - Day - Shift
  
  sel <- apply(
    is.na(pivoted.allocate[,c("Level 0", "Level 1A", "Level 1B", "Level 2", "Level 3")]),
    1,
    all
  )
  
  pivoted.allocate.some.entered <- pivoted.allocate[!sel, ]
  pivoted.allocate.all <- pivoted.allocate
  
  pivoted.allocate.some.entered[is.na(pivoted.allocate.some.entered)] <- 0
  
  pivoted.allocate.some.entered <- pivoted.allocate.some.entered %>% mutate(
    `Acuity Score` = acuity.score(`Level 0`, `Level 1A`, `Level 1B`, `Level 2`, `Level 3`)
  )

  pivoted.allocate.some.entered <- add.bed.size.all(
    pivoted.allocate.some.entered, 
    "ValidDate"
    )
  
  pivoted.allocate.some.entered.per.bed <- pivoted.allocate.some.entered %>% 
    mutate(
      across(
        contains("Level"),
        function(i) i / Beds,
        .names = "{.col} per Bed"
      )
    ) %>% 
    mutate(
      `Acuity Score per Bed` = `Acuity Score` / Beds
    )

  allo.process.f <- process.across.f(
    pivoted.allocate.some.entered.per.bed, "ValidDate",
    all_of(c("Level 0 per Bed", "Level 1A per Bed", "Level 1B per Bed", 
             "Level 2 per Bed", "Level 3 per Bed", "Acuity Score per Bed")),
    fn
  )
  
  monthly.allocate <- lagged.process(allo.process.f)
  
  monthly.allocate %>% saveRDS("processed_data/Allocate_Monthly_Lagged.RData")
  
  monthly.allocate
}