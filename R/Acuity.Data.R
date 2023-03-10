# Tag:Acuity
# Tag:SNCT


acuity_score_f <- function(l0, l1a, l1b, l2, l3) {
  #' Calculate a weighted acuity score using the SNCT multipliers.
  #' See the "Safer Nursing Care Tool - Implementation Resource Pack"
  #' [https://www.ulh.nhs.uk/content/uploads/2015/06/shelford_group_safety_care_nursing_tool.pdf]
  #' 
  #' @param l0 Count of `Level 0` acuity
  #' @param l1a Count of `Level 1A` acuity
  #' @param l1b Count of `Level 1B` acuity
  #' @param l2 Count of `Level 2` acuity
  #' @param l3 Count of `Level 3` acuity
  l0 * 0.99 + l1a * 1.39 + l1b * 1.72 + l2 * 1.97 + l3 * 5.96
}

acuity_score <- Vectorize(acuity_score_f)


make_monthly_acuity <- function(sql_table = "jpuh_AllocateAcuity",
                                fn = pkg_env$default_functions) {
  #' Monthly Acuity data summary
  #'
  #' Steps:
  #' 1. Get rows of data where `Name` like "Level"
  #' 2. Pivot data to ward-date vs Acuity
  #' 3. Exclude rows where no data was entered
  #' 4. Fill in missing values from remaining cases with zero.
  #' 5. Add bed sizes.
  #' 6. Calculate Acuity frequencies per bed and Acuity Score (SNCT multipliers)
  #' 7. Calculate aggregate metrics
  #'   (Defaults: mean, var, Na ratio) at lag periods
  #' 8. Save to file
  #'
  #' @param sql_table Name of relevant sql table
  #' @param fn List of functions to use for summarizing data
  #'
  #' @export
  #' @importFrom magrittr %>%
  #' @importFrom data.table %like%
  #' @importFrom reshape2 dcast
  #' @import dplyr
  #' @importFrom dplyr tbl collect mutate
  #'
  
  `ValidDate` <- Unit <- Name <-  NULL
  allocate <- tbl(pkg_env$con, sql_table) %>%
    filter(Name %like% "Level%") %>%
    collect() %>%
    mutate(
      ValidDate = as.Date(ValidDate, format = "%d/%m/%y"),
      Ward = Unit
    )

  pivoted_allocate <- allocate %>%
    dcast(
      Ward + ValidDate + CensusPeriod ~ Name,
      value.var = "Count"
    ) # Ward - Day - Shift

  acu_levels <- c("Level 0", "Level 1A", "Level 1B", "Level 2", "Level 3")

  sel <- apply(
    is.na(pivoted_allocate[, acu_levels]),
    1,
    all
  )

  pivoted_allocate_some_entered <- pivoted_allocate[!sel, ]

  pivoted_allocate_some_entered[is.na(pivoted_allocate_some_entered)] <- 0
  
  `Level 0` <- `Level 1A` <- `Level 1B` <- 
    `Level 2` <- `Level 3` <- `Acuity Score` <- NA

  pivoted_allocate_some_entered <- pivoted_allocate_some_entered %>% mutate(
    `Acuity Score` = acuity_score(
      `Level 0`, `Level 1A`, `Level 1B`,
      `Level 2`, `Level 3`
    )
  )

  pivoted_allocate_some_entered <- add_bed_size_all(
    pivoted_allocate_some_entered,
    "ValidDate"
  )
  
  Beds <- `Acuity Scores` <- NULL

  piv_allo_some_entered_per_bed <- pivoted_allocate_some_entered %>%
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

  allo_process_f <- process_across_f(
    piv_allo_some_entered_per_bed, "ValidDate",
    all_of(c(
      "Level 0 per Bed", "Level 1A per Bed", "Level 1B per Bed",
      "Level 2 per Bed", "Level 3 per Bed", "Acuity Score per Bed"
    )),
    fn
  )

  monthly_allocate <- lagged_process(allo_process_f)

  monthly_allocate %>% saveRDS("processed_data/Allocate_Monthly_Lagged.RData")

  monthly_allocate
}
