######
source('db_setup.R')


install.packages("devtools")
devtools::document()
devtools::load_all()

# TODO: Add check in open connection that db and server are given
con <- open_connection()
tables <- DBI::dbListTables(con)

nurs.connect()

est3 <- make_establishment_lag3()

acu <- make_monthly_acuity()

mt <- make_monthly_mand_training()

assign_shifts <- make_assign_shift_agg()

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
