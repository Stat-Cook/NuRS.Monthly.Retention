# Tag:Allocate
# Tag:Assignments

make_monthly_assignment <- function(fn = pkg_env$default_functions) {
  #'
  #' Steps:
  #' 1. Limit analysis to shifts with at least one person in post.
  #' 2. Add bed sizes
  #' 3. Calculate shift staffing (Count and Hours) per bed
  #' 4. Calculate aggregate metrics for staffing and staff status columns.
  #'
  #' @param fn List of aggregate metrics
  #' @export
  if (!file.exists(pkg_env$allocate_shit_aggregate_file)) {
    make_assign_shift_agg()
  }

  assignment.shift.overlap <- readRDS(pkg_env$allocate_shit_aggregate_file) %>%
    mutate(
      Date = as.Date(.data$Date)
    )

  actual.total <- assignment.shift.overlap %>%
    select(contains("Actual")) %>%
    select(contains("Hours")) %>%
    as_tibble() %>%
    select(-c("Ward", "Date")) %>%
    apply(1, sum)


  asssignment.shift.overlap <- assignment.shift.overlap[actual.total != 0, ]

  assignment.shift.overlap.with.beds <- assignment.shift.overlap %>% add_bed_size_all("Date")

  cols <- colnames(assignment.shift.overlap.with.beds)
  count.cols <- cols[str_detect(cols, "Count")]
  hours.cols <- cols[str_detect(cols, "Hours")]

  assignment.shift.overlap.per.bed <- assignment.shift.overlap.with.beds %>%
    mutate(across(
      all_of(c(count.cols, hours.cols)),
      function(i) i / .data$Beds,
      .names = "{.col} per Bed"
    )) %>%
    select(-all_of(c(count.cols, hours.cols)))


  cols <- colnames(assignment.shift.overlap.per.bed)
  count.cols <- cols[str_detect(cols, "Count")]
  hours.cols <- cols[str_detect(cols, "Hours")]

  staff.cols <- c(
    count.cols, hours.cols,
    "Over Staffed", "Under Staffed", "Fully Staffed", "Mixed"
  )

  assignment.metrics.f <- process_across_f(
    assignment.shift.overlap.per.bed, "Date",
    all_of(staff.cols), fn
  )

  assignment.metrics <- lagged_process(assignment.metrics.f)


  var.data <- assignment.shift.overlap.per.bed %>%
    mutate(Year = year(.data$Date), Month = month(.data$Date)) %>%
    var.functions()

  temp <- var.data$averages %>%
    group_by(.data$Ward, .data$Year, .data$Month) %>%
    summarize(n = n())

  temp[temp$n == 5, ]
  mean(temp$n == 1)

  var.data$averages %>% filter(.data$Ward == "CDS", .data$Year == 2020)

  between.var.f <- process_across_f(
    var.data$averages, "Date",
    where(is.numeric),
    list("Between Shift Var" = var)
  )

  within.var.f <- process_across_f(
    var.data$`De-meaned`, "Date",
    where(is.numeric),
    list("Within Shift Var" = var)
  )

  assignment.between.var <- lagged_process(between.var.f)
  assignment.within.var <- lagged_process(within.var.f)

  assignment.merged.metrics <- merge.lists.by.name(
    assignment.metrics, assignment.between.var, assignment.within.var
  )

  assignment.merged.metrics %>%
    saveRDS("processed_data/Assignment_Monthly_Lagged.RData")

  assignment.merged.metrics
}
