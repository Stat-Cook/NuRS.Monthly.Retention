# Tag:Ulysses
# Tag:Incident

make_incident_data <- function(sql_table = "jpuh_ulysses_IncidentFile",
                               .con = pkg_env$con) {
  #' Querry incident data
  #'
  #' Steps:
  #' 1. Query data set
  #' 2. Define harmful and non-harmful incidents
  #' 3. Lag data and calculate monthly harmful/ non-harmful events per ward
  #' 4. Convert incidents to incidents per patient day
  #'
  #' @param sql_table Name of data set
  #'
  #' @export
  #' @importFrom purrr map2

  incident <- tbl(.con, sql_table) %>%
    select(
      .data$Department, .data$`Incident Date`,
      .data$`Actual Impact`
    ) %>%
    collect()

  harmful <- c(
    "2 - Minor/Non Permanent Harm", "3 - Moderate/Semi Permanent Harm",
    "5 - Catastrophic/Death", "4 - Major/Permanent Harm"
  )
  non_harm <- c("1 - No Harm", "6 - Near Miss")

  harm_f <- function(values) sum(values %in% harmful)
  non_harm_f <- function(values) sum(values %in% non_harm)


  incident <- incident %>%
    mutate(
      Ward = ulys_to_allocate(.data$Department),
      `Date` = as.Date(.data$`Incident Date`, format = "%d/%m/%Y")
    ) %>%
    mutate(Year = year(Date), Month = month(Date))

  pas_days_occupied <- get.pas.file()

  incident_summary_f <- function(window) {
    inc <- lagged_group(incident, "Date", window) %>%
      summarize(
        "Lagged Incidents" := n(),
        "Lagged Harmful Incidents" := harm_f(.data$`Actual Impact`),
        "Lagged NoHarm Incidents" := non_harm_f(.data$`Actual Impact`)
      )

    pas_days <- pas_days_occupied %>%
      lagged_group("Month Starting", window) %>%
      summarize(`Days Used` = sum(.data$`Days Used`))

    left_join(inc, pas_days) %>%
      mutate(
        "Lag {min(window)}-{max(window)} Incidents per Patient Day" :=
          .data$`Lagged Incidents` / .data$`Days Used`,
        "Lag {min(window)}-{max(window)} Harmful Incidents per Patient Day" :=
          .data$`Lagged Harmful Incidents` / .data$`Days Used`,
        "Lag {min(window)}-{max(window)} NoHarm Incidents per Patient Day" :=
          .data$`Lagged NoHarm Incidents` / .data$`Days Used`
      ) %>%
      filter(!is.na(`Days Used`)) %>%
      select(
        .data$Ward, .data$Year, .data$Month,
        contains("per Patient Day")
      )
  }

  monthly_incident <- lagged_process(incident_summary_f)

  monthly_incident %>% saveRDS("processed_data/Incident_Monthly_Lagged.RData")
  monthly_incident
}
