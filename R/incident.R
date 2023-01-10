# Tag:Ulysses
# Tag:Incident

make.incident.data <- function(sql.table = "jpuh_ulysses_IncidentFile") {
  #' Querry incident data
  #'
  #' Steps:
  #' 1. Query data set
  #' 2. Define harmful and non-harmful incidents
  #' 3. Lag data and calculate monthly harmful/ non-harmful events per ward
  #' 4. Convert incidents to incidents per patient day
  #'
  #' @param sql.table Name of data set
  #'
  #' @export
  #' @importFrom purrr map2

  incident <- tbl(con, sql.table) %>%
    select(Department, `Incident Date`, `Actual Impact`) %>%
    collect()

  harmful <- c(
    "2 - Minor/Non Permanent Harm", "3 - Moderate/Semi Permanent Harm",
    "5 - Catastrophic/Death", "4 - Major/Permanent Harm"
  )
  non.harm <- c("1 - No Harm", "6 - Near Miss")

  harm.f <- function(values) sum(values %in% harmful)
  non.harm.f <- function(values) sum(values %in% non.harm)


  incident <- incident %>%
    mutate(
      Ward = ulys_to_allocate(Department),
      `Date` = as.Date(incident$`Incident Date`, format = "%d/%m/%Y")
    ) %>%
    mutate(Year = year(Date), Month = month(Date))

  pas.days.occupied <- get.pas.file()

  incident.summary.f <- function(window) {
    inc <- lagged.group(incident, "Date", window) %>%
      summarize(
        "Lagged Incidents" := n(),
        "Lagged Harmful Incidents" := harm.f(`Actual Impact`),
        "Lagged NoHarm Incidents" := non.harm.f(`Actual Impact`)
      )

    pas.days <- pas.days.occupied %>%
      lagged.group("Month Starting", window) %>%
      summarize(`Days Used` = sum(`Days Used`))

    left_join(inc, pas.days) %>%
      mutate(
        "Lag {min(window)}-{max(window)} Incidents per Patient Day" := `Lagged Incidents` / `Days Used`,
        "Lag {min(window)}-{max(window)} Harmful Incidents per Patient Day" := `Lagged Harmful Incidents` / `Days Used`,
        "Lag {min(window)}-{max(window)} NoHarm Incidents per Patient Day" := `Lagged NoHarm Incidents` / `Days Used`
      ) %>%
      filter(!is.na(`Days Used`)) %>%
      select(Ward, Year, Month, contains("per Patient Day"))
  }

  monthly.incident <- lagged.process(incident.summary.f)

  monthly.incident %>% saveRDS("processed_data/Incident_Monthly_Lagged.RData")
  monthly.incident
}
