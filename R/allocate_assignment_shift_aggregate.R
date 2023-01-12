# Tag:Allocate
# Tag:Assignment

status_f <- function(delta_reg, delta_unreg, delta_other) {
  #' Determine shift status.
  #'
  #' Rules for deciding if a shift had 'Over', 'Under', 'Fully', or 'Mixed'
  #' staffing
  #'
  #' @param deltra_reg Change in staffing from planned for registered staff
  #' @param deltra_runeg Change in staffing from planned for unregistered staff
  #' @param deltra_other Change in staffing from planned for other staff
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

  assignment <- tbl(pkg_env$con, sql_table) %>%
    collect() %>%
    mutate(Ward = .data$`Unit Name`) %>%
    filter(.data$Ward %in% esr_to_allocate_list)

  assignment <- assignment %>%
    mutate(
      Date = as.Date(.data$Date),
      `Over Staffed` = as.numeric(.data$Status == "Over Staffed"),
      `Under Staffed` = as.numeric(.data$Status == "Under Staffed"),
      `Mixed Staffed` = as.numeric(.data$Status == "Mixed"),
      `Fully Staffed` = as.numeric(.data$Status == "Full Staffed"),
      `Delta Registered Count` =
        .data$`Actual Registered Count` - .data$`Planned Registered Count`,
      `Delta Unregistered Count` =
        .data$`Actual Unregistered Count` - .data$`Planned Unregistered Count`,
      `Delta Other Count` =
        .data$`Actual Other Count` - .data$`Planned Other Count`,
      `Delta Registered Hours` =
        .data$`Actual Registered Hours` - .data$`Planned Registered Hours`,
      `Delta Unregistered Hours` =
        .data$`Actual Unregistered Hours` - .data$`Planned Unregistered Hours`,
      `Delta Other Hours` =
        .data$`Actual Other Hours` - .data$`Planned Other Hours`
    ) %>%
    select(-.data$`Red Flag Count`)

  cols <- colnames(assignment)

  staff_cols <- c(
    cols[str_detect(cols, "Count")],
    cols[str_detect(cols, "Hours")]
  )

  assignment_shift_f <- function(.year) {
    assignment %>%
      filter(year(.data$Date) == .year) %>%
      group_by(.data$Ward, .data$Date, .data$`Shift Type`) %>%
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

  assignment_shift_overlap <- do.call(rbind, assignment_shift_overlap_list) %>%
    mutate(
      Status = status_vec(
        .data$`Delta Registered Hours`,
        .data$`Delta Unregistered Hours`,
        .data$`Delta Other Hours`
      )
    ) %>%
    mutate(
      `Over Staffed` = (.data$Status == "Over Staffed"),
      `Under Staffed` = (.data$Status == "Under Staffed"),
      `Fully Staffed` = (.data$Status == "Fully Staffed"),
      `Mixed` = (.data$Status == "Mixed")
    ) %>%
    int.to.double()

  assignment_shift_overlap %>% saveRDS(pkg_env$allocate_shit_aggregate_file)
  assignment_shift_overlap
}
