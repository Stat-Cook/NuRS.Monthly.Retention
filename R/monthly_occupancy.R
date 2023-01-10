# Tag:Occupancy

get.pas.file <- function(){
  
  #' For generating Monthly Average Establishment
  #' 
  #' @export
  #' 
  #' @importFrom magrittr %>%
  
  pas.file <- pkg.env$pas.file
  
  if (!file.exists(pas.file)) {
    pas.ws <- tbl(con, "pas_wardstays") %>% collect()
    colnames(pas.ws) <- c("Hospital_Provider_Spell_No", "PatientID", "StartDate", "EndDate", "Ward" )
    
    pas.by.month <- DEFAULT.INTERVAL.FUNC(pas.ws, StartDate, EndDate)
    
    pas.monthly <- pas.by.month %>%
      mutate(Ward = pas_to_allocate(Ward)) %>%
      filter(Ward %in% unlist(pas_to_allocate_list)) %>%
      mutate(
        `Relative Stay in Days` = as.double(`Period End` - `Period Start`, "days"),
        Year = year(`Month Starting`),
        Month = month(`Month Starting`)
      )
    
    pas.days.occupied <- pas.monthly %>%
      group_by(Ward, `Month Starting`) %>%
      summarise(`Days Used` = sum(`Relative Stay in Days`)) %>%
      mutate(Year = year(`Month Starting`), Month = month(`Month Starting`))
    
    saveRDS(pas.days.occupied, pas.file)
  } else {
    pas.days.occupied <- readRDS(pas.file)
  }
  
  pas.days.occupied
}


add.days.occupied <- function(data, date.column){
  #' Append 'pas.days.occupied' data set to another data set
  #' 
  #' Merge on Ward, Year and Month
  #' 
  #' @param data A data frame with 'Ward' and 'date.column' fields
  #' @param date.column A field of 'data' that is a data/ time
  #' 
  #' @export
  pas.days.occupied <- get.pas.file()
    
  data %>% 
    mutate(
      Year = year({{date.column}}),
      Month = month({{date.column}})
    ) %>%  
    left_join(
      pas.days.occupied, 
      by = c(
        "Ward" = "Ward",
        "Year" = "Year",
        "Month" = "Month"
      )
    )
}