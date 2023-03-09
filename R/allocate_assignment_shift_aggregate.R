# Tag:Allocate
# Tag:Assignment

status_f <- function(delta_reg, delta_unreg, delta_other) {
  #' Determine shift status.
  #'
  #' Rules for deciding if a shift had 'Over', 'Under', 'Fully', or 'Mixed'
  #' staffing
  #'
  #' @param delta_reg Change in staffing from planned for registered staff
  #' @param delta_unreg Change in staffing from planned for unregistered staff
  #' @param delta_other Change in staffing from planned for other staff
  #'
  #'
  signs <- sapply(c(delta_reg, delta_unreg, delta_other), sign)

  all(signs == 0)

  if (all(signs == 0)) {
    return("Fully Staffed")
  }

  if (all(signs >= 0)) {
    return("Over Staffed")
  }

  if (all(signs <= 0)) {
    return("Under Staffed")
  }

  return("Mixed")
}

status_vec <- Vectorize(status_f)

make_assign_shift_agg <- function(sql_table = "JPUH_Allocate_Assignment_Combined_tsv") {
  #' Make assignment shift data
  #'
  #'
  #' Steps:
  #' 1. Filter data to only wards with an allocate mapping
  #' 2. Calculate diff between actual and planned staffing/
  #'    embed shift over/under staffing as one hot vec.
  #' 3. Calculate sum of staffing levels by ward, date and shift
  #' 4. Calculate over/under/mixed status [see status.f for logic]
  #' 5. Save data to file
  #'
  #' @param sql_table Name of data set in database
  #'
  #' @export
  #'
  #' @importFrom stringr str_detect

  `Unit Name` <- Ward <- NULL
  
  assignment <- tbl(pkg_env$con, sql_table) %>%
    collect() %>%
    mutate(Ward = `Unit Name`) %>%
    filter(Ward %in% esr_to_allocate_list)
  
  Date <- Status <- NULL
  
  `Actual Registered Count` <- `Planned Registered Count` <- NULL
  `Actual Unregistered Count` <- `Planned Unregistered Count` <- NULL
  `Actual Other Count` <- `Planned Other Count` <- NULL
  
  `Actual Registered Hours` <- `Planned Registered Hours` <- NULL
  `Actual Unregistered Hours` <- `Planned Unregistered Hours` <- NULL
  `Actual Other Hours` <- `Planned Other Hours` <- NULL

  assignment <- assignment %>%
    mutate(
      Date = as.Date(Date),
      `Over Staffed` = as.numeric(Status == "Over Staffed"),
      `Under Staffed` = as.numeric(Status == "Under Staffed"),
      `Mixed Staffed` = as.numeric(Status == "Mixed"),
      `Fully Staffed` = as.numeric(Status == "Full Staffed"),
      `Delta Registered Count` = 
        `Actual Registered Count` - `Planned Registered Count`,
      `Delta Unregistered Count` =
        `Actual Unregistered Count` - `Planned Unregistered Count`,
      `Delta Other Count` =
        `Actual Other Count` - `Planned Other Count`,
      `Delta Registered Hours` =
        `Actual Registered Hours` - `Planned Registered Hours`,
      `Delta Unregistered Hours` =
        `Actual Unregistered Hours` - `Planned Unregistered Hours`,
      `Delta Other Hours` =
        `Actual Other Hours` - `Planned Other Hours`
    ) %>%
    select(-"Red Flag Count")

  cols <- colnames(assignment)

  staff_cols <- c(
    cols[str_detect(cols, "Count")],
    cols[str_detect(cols, "Hours")]
  )
  Ward <- Date <- `Shift Type` <- NULL
  assignment_shift_f <- function(.year) {
    assignment %>%
      filter(year(Date) == .year) %>%
      group_by(Ward, Date, `Shift Type`) %>%
      summarize(
        across(
          all_of(staff_cols),
          sum
        )
      )
  }

  assignment_shift_overlap_list <- lapply(
    2015:2020,
    assignment_shift_f
  )
  
  `Delta Registered Hours` <- `Delta Unregistered Hours` <- 
    `Delta Other Hours` <-  NULL

  assignment_shift_overlap <- do.call(rbind, assignment_shift_overlap_list) %>%
    mutate(
      Status = status_vec(
        `Delta Registered Hours`,
        `Delta Unregistered Hours`,
        `Delta Other Hours`
      )
    ) %>%
    mutate(
      `Over Staffed` = (Status == "Over Staffed"),
      `Under Staffed` = (Status == "Under Staffed"),
      `Fully Staffed` = (Status == "Fully Staffed"),
      `Mixed` = (Status == "Mixed")
    ) %>%
    int.to.double()

  assignment_shift_overlap %>% saveRDS(pkg_env$allocate_shit_aggregate_file)
  assignment_shift_overlap
}
