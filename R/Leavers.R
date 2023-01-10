# Tag:Leavers
# Tag:Allocate

LEAVERS.SQL.TABLE <- "JPUH_Leavers_Monthly_Frequencies_StaffGroup"

get.leavers <- function(sql.table = LEAVERS.SQL.TABLE) {
  #' Query list of leavers from 'staging' data base
  #'
  #' @param sql.table Name of data set in database
  #'
  tbl(pkg.env$con, sql.table) %>%
    collect() %>%
    mutate(
      `Termination Month` = as.Date(paste(`Termination Month`, "01", sep = "-")),
      Ward = esr_to_allocate(Organisation),
      Year = year(`Termination Month`),
      Month = month(`Termination Month`)
    ) %>%
    add.establishment(`Termination Month`)
}

sql.table <- LEAVERS.SQL.TABLE
make.voluntary.outcome <- function(sql.table = LEAVERS.SQL.TABLE) {
  #'
  #' Step
  #' 1. Limit analysis to only voluntary leavers
  #' 2. Calculate Monthly Turnover
  #' 3. Add in ward-months with no turnover
  #' 4. Save to file
  #'
  #' @param sql.table Name of data set in database
  #'
  #' @export
  #' @importFrom tidyr replace_na
  leavers <- get.leavers(sql.table)

  voluntary.resignation <- leavers %>%
    filter(str_detect(`Leaving Reason`, "Voluntary Resignation")) %>%
    filter(`Staff Group` == "Nursing and Midwifery Registered") %>%
    group_by(Ward, Year, Month) %>%
    dplyr::summarize(Leavers = sum(Count))


  vr.all.months <- voluntary.resignation %>%
    mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    all.ward.month.merge() %>%
    mutate(Year = year(Date), Month = month(Date)) %>%
    replace_na(list("Leavers" = 0)) %>%
    select(-Date)

  saveRDS(vr.all.months, "processed_data/Voluntary_Resignation.Rdata")
  vr.all.months
}

make.voluntary.leavers <- function(sql.table = LEAVERS.SQL.TABLE) {
  #'
  #' Steps
  #' 1. Limit data to only wards in allocate data set
  #' 2. Calculate
  #'
  #' @param sql.table Name of data set in database
  #'
  #' @export
  leavers <- get.leavers(sql.table)

  ward.month.leavers <- leavers %>%
    group_by(Ward, `Termination Month`, `Leaving Reason`) %>%
    summarize(Count = sum(Count)) %>%
    add.establishment(`Termination Month`)


  leavers.count.f <- ward.month.leavers %>%
    filter(Ward != "Unknown") %>%
    mutate(`Leavers per Average Est` = Count / `Average Est`) %>%
    process.across.f("Termination Month", `Leavers per Average Est`, list(`per Month` = mean))

  {
    leavers.laged <- lagged.process(leavers.count.f)

    leavers.laged <- lapply(
      leavers.laged,
      function(i) gap.expand(i, "Ward", "Year", "Month")
    )
  }

  leavers.laged %>%
    saveRDS("processed_data/Leavers_Monthly_Lagged.RData")
  leavers.laged
}


make.all.leavers <- function(sql.table = LEAVERS.SQL.TABLE) {
  #' Make data set of all Leavers
  #'
  #' @param sql.table Name of data set in database
  #' @export
  leavers <- get.leavers(sql.table)

  ward.month.leavers <- leavers %>%
    group_by(Ward, `Termination Month`, `Leaving Reason`) %>%
    summarize(Count = sum(Count)) %>%
    add.establishment(`Termination Month`)

  leavers.voluntary.count.f <- ward.month.leavers %>%
    filter(Ward != "Unknown") %>%
    filter(str_detect(`Leaving Reason`, "Voluntary Resignation")) %>%
    mutate(`Voluntary Leavers per Average Est` = Count / `Average Est`) %>%
    process.across.f("Termination Month", `Voluntary Leavers per Average Est`, list(`per Month` = mean))

  {
    leavers.voluntary.laged <- lagged.process(leavers.voluntary.count.f)

    leavers.voluntary.laged <- lapply(
      leavers.voluntary.laged,
      function(i) gap.expand(i, "Ward", "Year", "Month")
    )
  }

  leavers.voluntary.laged %>%
    saveRDS("processed_data/Voluntary_Leavers_Monthly_Lagged.RData")
  leavers.voluntary.laged
}
