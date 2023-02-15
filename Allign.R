install.packages("devtools")
devtools::document()
devtools::load_all()

monthly.allocate <- readRDS("processed_data/Allocate_Monthly_Lagged.RData") # Allocate
mandtrain <- readRDS("processed_data/MandTraining_Monthly_Lagged.RData") # ESR

assignment.metrics <- readRDS("processed_data/Assignment_Monthly_Lagged.RData") # Allocate
voluntary.resignation <- readRDS("processed_data/Voluntary_Resignation.Rdata") # ESR

sickness <- readRDS("processed_data/Sickness_Monthly_Lagged.RData") # ESR
establishment.lag.3 <- readRDS("processed_data/Establishment_Lag_3.Rdata") # ESR

wardstays <- readRDS("processed_data/WardStay_Monthly.RData") # PAS
shifts.worked <- readRDS("processed_data/ShiftsWorkedProportions_Monthly_Lagged.RData") # Allocate

incidents <- readRDS("processed_data/Incident_Monthly_Lagged.RData") # Ulysses
leavers.monthly <- readRDS("processed_data/Leavers_Monthly_Lagged.RData") # ESR
voluntary.leavers.monthly <- readRDS("processed_data/Voluntary_Leavers_Monthly_Lagged.RData") # ESR
wardstays.start.end <- readRDS("processed_data/WardStay_StartEnd_Monthly.RData") # PAS

ward.annual.demos <- readRDS("processed_data/SWD_Annual.RData")



#shifts.worked$`Lag 3-4` %>% select(contains("prop"), contains("mean")) %>% View()

nurs.data <- list(
  voluntary.resignation,  #                                         Voluntary Resignation
  establishment.lag.3, #                                            Establishment 
  monthly.allocate$`Lag 3-4`,
  monthly.allocate$`Lag 5-6`, monthly.allocate$`Lag 7-12`, #        Allocate Acuity 
  mandtrain$`Lag 3-4`, mandtrain$`Lag 5-6`, mandtrain$`Lag 7-12`, # Mandatory Training 
  assignment.metrics$`Lag 3-4`, assignment.metrics$`Lag 5-6`, 
  assignment.metrics$`Lag 7-12`,  #                                 Allocate Assignment
  sickness$`Lag 3-4`, sickness$`Lag 5-6`, sickness$`Lag 7-12`, #    Sickness
  wardstays$`Lag 3-4`, wardstays$`Lag 5-6`, wardstays$`Lag 7-12`,#  Ward Stays 
  incidents$`Lag 3-4`, incidents$`Lag 5-6`, 
  incidents$`Lag 7-12`, #                                           Incidents
  shifts.worked$`Lag 3-4`, shifts.worked$`Lag 5-6`, 
  shifts.worked$`Lag 7-12`, #                                       Shifts Worked
  voluntary.leavers.monthly$`Lag 3-4`, voluntary.leavers.monthly$`Lag 5-6`, 
  voluntary.leavers.monthly$`Lag 7-12`, #                           Voluntary Levers
  leavers.monthly$`Lag 3-4`, leavers.monthly$`Lag 5-6`, 
  leavers.monthly$`Lag 7-12`, #                                     All Leavers
  wardstays.start.end$`Lag 3-4`,   wardstays.start.end$`Lag 5-6`,   
  wardstays.start.end$`Lag 7-12`,    #                                Ward Stay [Start and End Count]
  # ward.demos$`Lag 3-4`, ward.demos$`Lag 5-6`,
  # ward.demos$`Lag 7-12`
  ward.annual.demos
)




# nurs.join <- function(x, y) left_join(x, y,
#                                       by=c("Ward", "Year", "Month"))



joined <- reduce(nurs.data, nurs.join)%>%
  replace_na(list("Leavers" = 0))%>% 
  inc_exc_criteria()


library(stringr)
get_suffix <- function(data){
  
  .cols <- colnames(data)
  feature <- .cols %>% 
    str_replace("Lag 3-4 ", "") %>%
    str_replace("Lag 5-6 ", "") %>% 
    str_replace("Lag 7-12 ", "") %>% 
    table()
  names(feature)
}



joined.cols <- colnames(joined)


joined.filtered <- joined %>% 
  select(-contains("ratio")) %>% 
  select(-contains("Fulfilment: Junior Doctor")) %>% 
  select(-contains("Fulfilment: -"))  %>% 
  select(-contains("Fulfilment: Consultants & AS")) %>% 
  select(-contains("Achieved Mean")) %>% 
  select(-contains("Required Mean")) %>% 
  select(-contains("Actual ")) %>% 
  select(-contains("Sickness N")) %>% 
  select(-contains("Stay N")) %>% 
  select(-contains("Stay Ave")) %>% 
  select(-contains("sum(Total")) %>% 
  select(-contains("IQR(Total")) %>% 
  select(-contains("% var")) %>% 
  select(-contains("Combined Duty"))%>% 
  select(-contains("Outlier")) %>% 
  select(-contains(" Missing Charge Cover Count")) %>%
  select(-contains("Beds Between Shift Var")) %>%
  select(-(contains("Planned") & contains("Count per Bed var"))) %>% 
  select(-(contains("Planned") & contains("Hours per Bed var"))) %>% 
  select(-contains(".N.")) %>% 
  select(-contains("Ave FTE Lost")) %>% 
  replace_na(list(
    "Lag 3-4 Harmful Incidents" = 0,
    "Lag 3-4 NoHarm Incidents" = 0,
    "Lag 3-4 Incidents" = 0,
    "Lag 5-6 Harmful Incidents" = 0,
    "Lag 5-6 NoHarm Incidents" = 0,
    "Lag 5-6 Incidents" = 0,
    "Lag 7-12 Harmful Incidents" = 0,
    "Lag 7-12 NoHarm Incidents" = 0,
    "Lag 7-12 Incidents" = 0,
    "Lag 3-4 Sickness N Spells" = 0,
    "Lag 3-4 Sickness SD FTE Lost" = 0,
    "Lag 3-4 Sickness Ave FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 3-4 Sickness FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 5-6 Sickness N Spells" = 0,
    "Lag 5-6 Sickness SD FTE Lost" = 0,
    "Lag 5-6 Sickness Ave FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 5-6 Sickness FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 7-12 Sickness N Spells" = 0,
    "Lag 7-12 Sickness SD FTE Lost" = 0,
    "Lag 7-12 Sickness Ave FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 7-12 Sickness FTE Lost" = as.difftime(0, unit = "days")
  ))

staffing.feature.grid <- expand.grid(
  Window = c("3-4", "5-6", "7-12"),
  Staff = c("Registered", "Unregistered", "Other"),
  Type = c("Count", "Hours")
)


j.filt <- joined %>% 
  select(
    -contains("Fulfilment: -"),
    -contains("Fulfilment: Jun"),
    -contains("Fulfilment: Con")
  )%>% 
  select(
    `Leavers`, 
    Ward, Year,
    contains("Acuity Score") & contains("Mean"), 
    contains("Acuity Score") & contains("Var"), 
    contains("Level") & contains("Mean"),
    contains("Level") & contains("Var"),
    contains("prop(Total Work)"),
    contains("Stay Start"),   contains("Stay End"), 
    contains("Stay Total"), contains("Stay Ave"),
    contains("Staffed Mean"),   contains("Mixed Mean"),
    contains("Planned") & contains("Mean"),
    contains("Planned") & contains("Between"),
    contains("Actual") & contains("Mean"),
    contains("Actual") & contains("Within"),
    contains("Delta") & contains("Mean"),
    contains("Sickness FTE Lost"),
    contains("Incidents"),
    contains("% Mean"),
    contains("Leavers per"),
    contains("Ave Establish"),
    contains("Est. Prop")
  ) %>% 
  replace_na(list(
    "Lag 3-4 Harmful Incidents" = 0,
    "Lag 3-4 NoHarm Incidents" = 0,
    "Lag 3-4 Incidents" = 0,
    "Lag 5-6 Harmful Incidents" = 0,
    "Lag 5-6 NoHarm Incidents" = 0,
    "Lag 5-6 Incidents" = 0,
    "Lag 7-12 Harmful Incidents" = 0,
    "Lag 7-12 NoHarm Incidents" = 0,
    "Lag 7-12 Incidents" = 0,
    "Lag 3-4 Sickness N Spells" = 0,
    "Lag 3-4 Sickness SD FTE Lost" = 0,
    "Lag 3-4 Sickness Ave FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 3-4 Sickness FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 5-6 Sickness N Spells" = 0,
    "Lag 5-6 Sickness SD FTE Lost" = 0,
    "Lag 5-6 Sickness Ave FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 5-6 Sickness FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 7-12 Sickness N Spells" = 0,
    "Lag 7-12 Sickness SD FTE Lost" = 0,
    "Lag 7-12 Sickness Ave FTE Lost" = as.difftime(0, unit = "days"),
    "Lag 7-12 Sickness FTE Lost" = as.difftime(0, unit = "days")
  )) 

filt.suffix <- get_suffix(j.filt)
raw.suffix <- get_suffix(joined)

raw.suffix[!raw.suffix %in% filt.suffix]
filt.suffix


incident.harm.features.f <- function(data, window){
  harmful.string <- glue("Lag {window} Harmful Incidents per Patient Day")
  all.string <- glue("Lag {window} Incidents per Patient Day")
  mutate(
    data, 
    "Lag {window} Prevalence Harm per Patient Day" := .data[[harmful.string]],
    "Lag {window} Proportion Harm" := .data[[harmful.string]] / .data[[all.string]]
  )
}

incident.harm.features <- function(data){
  windows <- c("3-4", "5-6", "7-12")
  for (w in windows){
    data <- incident.harm.features.f(data, w)
  }
  data
}

acuity.grid <- expand.grid(
  Window = c("3-4", "5-6", "7-12"),
  Features = c("Level 0", "Level 1A", "Level 1B", "Level 2", "Level 3", "Acuity Score")
)


joined.processed <- j.filt %>% 
  actual.within.shift.var.per.mean.loop(staffing.feature.grid) %>%
  planned.between.shift.var.per.mean.loop(staffing.feature.grid) %>% 
  incident.harm.features() %>%
  acuity.var.features(acuity.grid) %>% 
  select(-contains("Actual")) %>%
  select(-(contains("Planned") & contains("Var"))) %>%
  select(-contains("Incident"))

joined.processed %>% select(contains("Acuity Score"))

joined.processed  %>%
  colnames()

cat(nrow(j.filt))
cat("\nCompared to 874")


complete.data <- joined.processed %>%
  get.complete()

nrow(complete.data)
ncol(complete.data)
.cols <- colnames(complete.data)

.cols[grepl("Eth", .cols)]

