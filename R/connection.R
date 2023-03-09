# Generate DB connection object from config file.
# Tag:Connect
# Tag:odbc
# Tag:Key


credential_prompt <- function() {
  #' Ask user for SQL credentials
  #'
  #' @importFrom keyring key_set
  #' @export
  key_set("db_credentials_UID", prompt = "NuRS database username: ")
  key_set("db_credentials_PWD", prompt = "NuRS database password: ")

  pkg_env$con <- open_connection()
}

open_connection <- function() {
  #' Generate a DBI connection object
  #'
  #' @importFrom odbc odbc dbConnect
  #' @importFrom glue glue
  #' @importFrom config get
  #' @export
  dm <- tryCatch(
    config::get(),
    error = function(e) {
      stop("Error in config file. Run \"source('db_setup.R')\" and try again")
    }
  )
  
  for (name in names(dm)){
    if (is.null(dm[[name]])){
      warning(glue("Key `{name}` not defined.  Check config.yaml"))
    }
  }

  con <- tryCatch(
    odbc::dbConnect(
      odbc::odbc(),
      Driver = dm$Driver,
      Server = dm$Server,
      Database = dm$Database,
      UID = dm$UID,
      PWD = dm$PWD
    ),
    error = function(e) NA
  )

  con
}
