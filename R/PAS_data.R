# Tag:PAS
# Tag:WardStay
# Tag:Occupancy

load.pas.data <- function(sql.table = "pas_wardstays") {
  #' Read pas ward stay data
  #'
  #' @param sql.table PAS table name in sql engine
  pas.ws <- tbl(pkg_env$con, sql.table) %>%
    collect()
  colnames(pas.ws) <- c(
    "Hospital_Provider_Spell_No", "PatientID",
    "StartDate", "EndDate", "Ward"
  )
  pas.ws
}


make.admissions.discharges <- function(sql.table = "pas_wardstays") {
  #' Calculate monthly admissions and discharges
  #'
  #' Ward level time series.
  #'
  #' Steps:
  #' 1. Make data set on admissions date
  #' 2. Make data set on discharge date
  #' 3. Add beds to each
  #' 4. Group data sets by adet and calculate count per mean(Beds)
  #'
  #' @param sql.table PAS table name in sql engine
  #' @export
  pas.ws <- load.pas.data(sql.table)

  pas.start.frame <- pas.ws %>%
    filter(Ward %in% names(pas_to_allocate_list)) %>% #
    mutate(Ward = pas_to_allocate(Ward)) %>%
    mutate(StartDate = as.Date(`StartDate`)) %>%
    add_bed_size_all("StartDate")

  pas.end.frame <- pas.ws %>%
    filter(Ward %in% names(pas_to_allocate_list)) %>% #
    mutate(Ward = pas_to_allocate(Ward)) %>%
    mutate(EndDate = as.Date(`EndDate`)) %>%
    add_bed_size_all("EndDate")

  pas.start.end.f <- function(window) {
    pas.start <- pas.start.frame %>%
      lag.data.frame("StartDate", window) %>%
      group_by(Ward, Year = year(LagedDate), Month = month(LagedDate)) %>%
      summarize("Lag {min(window)}-{max(window)} Stay Start Per Bed" := n() / mean(Beds))

    pas.end <- pas.end.frame %>%
      lag.data.frame("EndDate", window) %>%
      group_by(Ward, Year = year(LagedDate), Month = month(LagedDate)) %>%
      summarize("Lag {min(window)}-{max(window)} Stay End Per Bed" := n() / mean(Beds))

    merge(pas.start, pas.end)
  }

  start.end <- lagged.process(pas.start.end.f)
  start.end %>% saveRDS("processed_data/WardStay_StartEnd_Monthly.RData")
}


make.ward.occupancy <- function(sql.table = "pas_wardstays") {
  #' @export
  pas.ws <- load.pas.data(sql.table)

  pas.by.month <- DEFAULT.INTERVAL.FUNC(pas.ws, StartDate, EndDate)

  pas.monthly <- pas.by.month %>%
    mutate(Ward = pas_to_allocate(Ward)) %>%
    filter(Ward %in% unlist(pas_to_allocate_list)) %>%
    mutate(
      `Relative Stay in Days` = as.double(`Period End` - `Period Start`, "days"),
      Year = year(`Month Starting`),
      Month = month(`Month Starting`)
    )

  pas.with.beds <- pas.monthly %>%
    mutate(`Period Start` = as.Date(`Period Start`)) %>%
    add_bed_size_all("Period Start")

  pas.per.bed <- pas.with.beds %>%
    mutate(
      `Stay Total Days per Bed` = `Relative Stay in Days` / Beds
    )


  lagged.pas.f <- function(window) {
    lagged.group(pas.per.bed, "Month Starting", window) %>%
      dplyr::summarize("Lag {min(window)}-{max(window)} Stay N" := n(),
        "Lag {min(window)}-{max(window)} Stay Total Days per Bed" := sum(`Stay Total Days per Bed`),
        "Lag {min(window)}-{max(window)} Stay Ave Days per Bed" := mean(`Stay Total Days per Bed`),
        Year = mean(Year), Month = mean(Month)
      )
  }

  lagged.pas <- lagged.process(lagged.pas.f)
  lagged.pas %>% saveRDS("processed_data/WardStay_Monthly.RData")
}
