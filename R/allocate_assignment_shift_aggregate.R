# Tag:Allocate
# Tag:Assignment


status.f <- function(delta_reg, delta_unreg, delta_other) {
  #' Determine shift status.
  #'
  #' Rules for deciding if a shift had 'Over', 'Under', 'Fully', or 'Mixed'
  #' staffing
  #'
  #' @param deltra_reg Change in staffing from planned for registered staff
  #' @param deltra_runeg Change in staffing from planned for unregistered staff
  #' @param deltra_other Change in staffing from planned for other staff
  #'
  over_staffed_reg <- delta_reg > 0
  over_staffed_unreg <- delta_unreg > 0
  over_staffed_other <- delta_other > 0

  under_staffed_reg <- delta_reg < 0
  under_staffed_unreg <- delta_unreg < 0
  under_staffed_other <- delta_other < 0

  exact_reg <- delta_reg == 0
  exact_unreg <- delta_unreg == 0
  exact_other <- delta_other == 0

  if (exact_reg & exact_unreg & exact_other) {
    return("Fully Staffed")
  }

  if (over_staffed_reg | over_staffed_unreg | over_staffed_other) {
    if (under_staffed_reg | under_staffed_unreg | under_staffed_other) {
      return("Mixed")
    }
    return("Over Staffed")
  }

  if (under_staffed_reg | under_staffed_unreg | under_staffed_other) {
    if (over_staffed_reg | over_staffed_unreg | over_staffed_other) {
      return("Mixed")
    }
    return("Under Staffed")
  }
}

status.vec <- Vectorize(status.f)

make.assignment.shift.aggregate <- function(sql.table = "JPUH_Allocate_Assignment_Combined_tsv") {
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
  #' @param sql.table Name of data set in database
  #'
  #' @export
  #'
  #' @importFrom stringr str_detect

  assignment <- tbl(pkg.env$con, sql.table) %>%
    collect() %>%
    mutate(Ward = `Unit Name`) %>%
    filter(Ward %in% esr_to_allocate_list)

  assignment <- assignment %>%
    mutate(
      Date = as.Date(Date),
      `Over Staffed` = as.numeric(Status == "Over Staffed"),
      `Under Staffed` = as.numeric(Status == "Under Staffed"),
      `Mixed Staffed` = as.numeric(Status == "Mixed"),
      `Fully Staffed` = as.numeric(Status == "Full Staffed"),
      `Delta Registered Count` = `Actual Registered Count` - `Planned Registered Count`,
      `Delta Unregistered Count` = `Actual Unregistered Count` - `Planned Unregistered Count`,
      `Delta Other Count` = `Actual Other Count` - `Planned Other Count`,
      `Delta Registered Hours` = `Actual Registered Hours` - `Planned Registered Hours`,
      `Delta Unregistered Hours` = `Actual Unregistered Hours` - `Planned Unregistered Hours`,
      `Delta Other Hours` = `Actual Other Hours` - `Planned Other Hours`
    ) %>%
    select(-`Red Flag Count`)

  cols <- colnames(assignment)

  staff.cols <- c(
    cols[str_detect(cols, "Count")],
    cols[str_detect(cols, "Hours")]
  )

  # assignment.overlap <- assignment %>% filter(Ward %in% esr_to_allocate_list)

  assignment.shift.f <- function(.year) {
    assignment %>%
      filter(year(Date) == .year) %>%
      group_by(Ward, Date, `Shift Type`) %>%
      summarize(
        across(
          all_of(staff.cols),
          sum
        )
      )
  }

  assignment.shift.overlap.list <- lapply(
    2015:2020,
    assignment.shift.f
  )

  assignment.shift.overlap <- do.call(rbind, assignment.shift.overlap.list) %>%
    mutate(
      Status = status.vec(
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

  assignment.shift.overlap %>% saveRDS(pkg.env$allocate_shit_aggregate_file)
  assignment.shift.overlap
}
