# Tag:Allocate
# Tag:ShiftsWorked
# Tag:Demographics

model.cols <- c("Owning Unit", "Duty Date")

demographic.cols <- c("Gender", "Ethnic Origin", "Disability", 
               "Marital Status", "Sexual Orientation", 
               "Nationality", 
               "Country of Birth", 
               "Age Band", "Length of Service Band", 
               "Religious Belief", "AfC Pay Band", "Ethnicity Group", 
               "Disability Category")

one.hot.encode <- function(data, column) {
  #' Convert categorical variable to encoded matrix with dimensions 'nrow' x 'cardinality' 
  #' 
  #' @param data A data frame 
  #' @param column A column of data to encode.
  #' 
  #' @importFrom tidyr spread
  data %>% mutate(.value = 1)  %>% 
    mutate("{column}" := paste(column, .data[[column]], sep=".")) %>%
    spread(.data[[column]], .value,  fill = 0)
}

encode.all.demos <- function(data, factor.cols) {
  #' convert all given variables to a one hot encoding.
  #' 
  #' @param data A data frame
  #' @param factor.cols A vector of column in data
  #'
  
  loop.data <- data
  for (col in factor.cols){
    loop.data <- one.hot.encode(loop.data, col)
  }
  
  loop.data
}

to.chunks <- function(data, chunk.size = 1e4){
  
  #' Subdivide a data frame into a list of data frames for divide and conquer.
  #' 
  #' @param data A data frame
  #' @param chunk.size The max number of rows per item in list.
  #' 
  
  .nrow <- nrow(data)
  splits <- factor(floor(1:.nrow / chunk.size))
  data %>% 
    mutate(PK = 1:.nrow) %>% 
    split(splits)

}


fetch.swd <- function(year, .con=pkg.env$con){
  
  #' Get shifts worked data for a given year, and mark where values are missing due to 
  #' scrambler
  #' 
  #' @param year Numeric year of data to convert
  #' @param .con A DBI connection object

  new <- glue("JPUH_Allocate_Shifts_Worked_Demographics_Combined_{year}_tsv")
  frm <- tbl(.con, new) %>% 
    filter(`Shift Type` != "Rest") %>%
    select(`Duty Date`, `Shift Type`,`Owning Unit`, 
           all_of(demographic.cols)) %>% 
    collect()%>% 
    filter(`Owning Unit` %in% unlist(esr_to_allocate_list)) %>% 
    mutate(`Duty Date` = sw_to_date(`Duty Date`))
  
  wds.cnt <- frm  %>% 
    group_by(`Owning Unit`, `Duty Date`, `Shift Type`) %>%
    summarize(n = n())
  
  lone.shift <- wds.cnt %>% 
    filter(n == 1) %>% 
    mutate(Key = paste(`Owning Unit`, `Duty Date`, `Shift Type`))
  
  frm.key <- paste(frm$`Owning Unit`, frm$`Duty Date`, frm$`Shift Type`)
  
  lone.shift.sel <- frm.key %in% lone.shift$Key
  
  frm[lone.shift.sel, demographic.cols] <- "Lone Shift Missing"
  
  frm <- frm %>%
    filter(`Owning Unit` %in% unlist(esr_to_allocate_list)) %>% 
    replace(is.na(.), "Missing") %>%
    mutate(PK = 1:nrow(.))
  
  frm
}


{
  # Load demographics cardinality reduction spreadsheet
  demo.list <- list()
  for (demo in demographic.cols){
    frm <- readxl::read_excel("raw_data/DemographicAnalysisGroups.xlsx", demo)
    values <- frm$`For Analysis`
    names(values) <-   frm[[demo]]
    demo.list[[demo]] <- values
  }
}

clean.demographics <- function(data, .list = demo.list){
  
  #' Remap demographics in a data set based on "DemographicAnalysisGroups.xlsx"
  #' 
  #' @param data A data frame
  #' @param .list A mapped list of cardinality reduction.
  #' 
  for (demo in demographic.cols){
    lookup <- .list[[demo]]
    lookup["Lone Shift Missing"] <- "Lone Shift Missing"
    data <- data %>% 
      mutate(
        "{demo}" := lookup[.data[[demo]]]
      )
  }
  
  data
}

process.swd <- function(frm){
  
  #' Convert demographics in the shifts worked data set based on 'DemographicAnalysisGroups.xlsx' 
  #' 
  #' @param frm A data frame of shifts worked demographics
  #' 
  #' @importFrom purrr map
  
  chunked.frm <- frm  %>% to.chunks(5e3)
  
  chunked.encoded <- map(
    chunked.frm, 
    ~ encode.all.demos(., demographic.cols)
  )
  
  combined <- plyr::rbind.fill(chunked.encoded)
  combined[is.na(combined)] <- 0
  
  .cols <- combined %>% 
    select(-model.cols, -PK) %>% 
    colnames()
  combined <- combined[c(model.cols, sort(.cols))]
  
  combined
}


get.demographic.data <- function(){
  #' Fetch and clean demographics data and cache to file.
  
  swd.list <- lapply(2015:2020, fetch.swd)

  swd.clean.demos.list <- lapply(swd.list, clean.demographics)

  process.swd.list <- lapply(swd.clean.demos.list, process.swd)

  {
    process.swd <- plyr::rbind.fill(process.swd.list)
    process.swd[is.na(process.swd)] <- 0
  }

  saveRDS(process.swd, "processed_data/SWD_Raw.RData")
  
  process.swd
}


make.demographics <- function(){

  #' Produce the monthly lagged demographics data set having reduced demographic cardinality.
  #'
  #' @export
  
  if (!file.exists("processed_data/SWD_Raw.RData")){
    demos <- get.demographic.data()    
  }
  else{
    demos <-   readRDS("processed_data/SWD_Raw.RData")
  }

  
  lagged.demographic.f <- function(window){
    month.demos <- demos %>% 
      lag.data.frame("Duty Date", window) %>%
      mutate(Ward = `Owning Unit`, Year = year(LagedDate), Month = month(LagedDate)) %>%
      group_by(Ward, Year, Month) %>% summarize(
        across(where(is.numeric), sum)
      ) %>% as_tibble()
    
    month.prop.demos.list <- lapply(
      demographic.cols,
      function(demo){
        str <- paste(demo,".",sep="")
        month.demos %>% 
          select(contains(str))%>% 
          mutate(
            across(
              everything(), 
              .names="Lag {min(window)}-{max(window)} {.col} Est. Proportion"
            ) / rowSums(across(everything()))
          ) %>% 
          select(contains("Lag"))
      }
    )
    
    demo.proportions <- cbind(
      select(month.demos, Ward, Year, Month),
      do.call(cbind, month.prop.demos.list)
    )
    
    demo.proportions
  }
  
  lagged.demo.proportions <- lagged.process(lagged.demographic.f)

  saveRDS(lagged.demo.proportions, "processed_data/SWD_Monthly.RData")
  
  lagged.demo.proportions
}

# .all.cols <- colnames(lagged.demo.proportions$`Lag 3-4`) 
# 
# .demo.cols <- .all.cols[grepl("Lag", .all.cols)] %>% 
#   stringr::str_replace("Lag 3-4 ", "")

# min1 <- 3
# min2 <- 5
# max1 <- 4
# max2 <- 6
# merged <- merge(lagged.demo.proportions$`Lag 3-4`, lagged.demo.proportions$`Lag 5-6`)
# 
# lag.corr <- function(.col){
#   col1 <- glue("Lag {min1}-{max1} {.col}")
#   col2 <- glue("Lag {min2}-{max2} {.col}")
#   cor(merged[[col1]], merged[[col2]])
# }
# 
# lag.3.4.5.6.corrs <- sapply(.demo.cols, lag.corr)
# 
# min(sort(lag.3.4.5.6.corrs)[-c(1:15)])
# 
# .all.cols[grepl("Gender", .all.cols)]
# 
# cor(
#   merged$`Lag 3-4 Gender.Male Est. Proportion`, 
#   merged$`Lag 3-4 Gender.Female Est. Proportion`
# )

make.annual.demographics <- function(){
  #' Produce the annual lagged demographics data set having 
  #' reduced demographic cardinality.
  #'
  #' @export
  
  if (!file.exists("processed_data/SWD_Raw.RData")){
    demos <- get.demographic.data()    
  }
  else{
    demos <- readRDS("processed_data/SWD_Raw.RData")
  }
  
  
  lagged.demographic.f <- function(window){
    month.demos <- demos %>% 
      lag.data.frame("Duty Date", window) %>%
      mutate(Ward = `Owning Unit`, Year = year(LagedDate), Month = month(LagedDate)) %>%
      group_by(Ward, Year, Month) %>% summarize(
        across(where(is.numeric), sum)
      ) %>% as_tibble()
    
    month.prop.demos.list <- lapply(
      demographic.cols,
      function(demo){
        str <- paste(demo,".",sep="")
        month.demos %>% 
          select(contains(str))%>% 
          mutate(
            across(
              everything(), 
              .names="Lag {min(window)}-{max(window)} {.col} Est. Proportion"
            ) / rowSums(across(everything()))
          ) %>% 
          select(contains("Lag"))
      }
    )
    
    demo.proportions <- cbind(
      select(month.demos, Ward, Year, Month),
      do.call(cbind, month.prop.demos.list)
    )
    
    demo.proportions
  }
  
  annual.demo.proportions <- lagged.demographic.f(3:12)
  
  saveRDS(annual.demo.proportions, "processed_data/SWD_Annual.RData")
  
  annual.demo.proportions
}
