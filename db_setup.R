library(keyring)

key_set("db_credentials_UID", prompt = "NuRS database username: ")
key_set("db_credentials_PWD", prompt = "NuRS database password: ")
