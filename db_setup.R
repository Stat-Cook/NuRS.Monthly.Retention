library(keyring)

if(!file.exists("config.yml")){
  
  config.deafults <- c(
    "default:",
    "  Driver: 'SQL Server'",
    "  Server: ",
    "  Database: ",
    "  UID: !expr keyring::key_get('db_credentials_UID')",
    "  PWD: !expr keyring::key_get('db_credentials_PWD')"
  )
  write(config.deafults, "config.yml")
}

key_set("db_credentials_UID", prompt = "NuRS database username: ")
key_set("db_credentials_PWD", prompt = "NuRS database password: ")
