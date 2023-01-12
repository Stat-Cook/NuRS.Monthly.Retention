######
install.packages("devtools")
devtools::document()
devtools::load_all()

library(DBI)

con <- open.connection()
tables <- dbListTables(con)

est3 <- make_establishment_lag3()

acu <- make_monthly_acuity()

mt <- make_monthly_mand_training()

assign_shifts <- make_assignment_shift_aggregate()

month_assign <- make_monthly_assignment()

vo_new <- make_voluntary_outcome()
vl_new <- make_voluntary_leavers()
al_new <- make_all_leavers()

inc <- make_incident_data()

ms <- make_monthly_sickness()

admin_dis <- make_admissions_discharges()
occup <- make_ward_occupancy()

shifts_worked <- make_shifts_worked()

temp <- get_demographic_data()

demos <- make_demographics()
annual_demos <- make_annual_demographics()
