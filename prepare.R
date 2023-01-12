######
install.packages("devtools")
devtools::document()
devtools::load_all()

con <- open.connection()
library(DBI)
tables <- dbListTables(con)

tables[grepl("Leavers", tables)]

leavers <- tbl(con, "JPUH_Leavers_Monthly_Frequencies_StaffGroup") %>% 
  select(Count) %>% collect()


est3 <- make.establishment.lag3()

acu <- make_monthly_acuity()

mt <- make.monthly.mand.training()

assign.shifts <- make.assignment.shift.aggregate()

month.assign <- make.monthly.assignment()

leaver.diff <- vo.old$Leavers - vo.new$Leavers

vo.new <- make.voluntary.outcome() 
vl.new <- make.voluntary.leavers()
al.new <- make.all.leavers()

inc <- make.incident.data()

ms <- make.monthly.sickness()

admin.dis <- make.admissions.discharges()
occup <- make.ward.occupancy()

shifts.worked <- make.shifts.worked()

temp <- get.demographic.data()

demos <- make.demographics()

demos
annual.demos <- make.annual.demographics()
ncol(annual.demos)
colnames(annual.demos)

demos$`Lag 5-6` %>% select(contains("NA")) %>% head()
  
 
con <- open.connection()
allo.head <- tbl(con, "JPUH_Allocate_Assignment_Combined_tsv")  %>% head(50000) %>% collect()




{
  month.demos <- demos %>% 
    mutate(Ward = `Owning Unit`, Year = year(`Duty Date`), Month = month(`Duty Date`)) %>%
    group_by(Ward, Year, Month) %>% summarize(
      across(where(is.numeric), sum)
    ) %>% as_tibble()
  
  
  month.prop.demos.list <- lapply(
    demographic.cols,
    function(demo){
      str <- paste(demo,".",sep="")
      month.demos %>% 
        select(contains(str))%>% 
        mutate(across(everything()) / rowSums(across(everything())))
    }
  )
  
  cbind(
    select(month.demos, Ward, Year, Month),
    do.call(cbind, month.prop.demos.list)
  )
}