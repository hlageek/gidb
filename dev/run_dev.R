# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you want the app to be served on a random port
options(shiny.port = 6969, shiny.fullstacktrace = TRUE)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app(
  dbname = Sys.getenv("DB_NAME"),
  dbusername = Sys.getenv("DB_USER"),
  dbpassword = Sys.getenv("DB_PASS"),
  credentials_path = "gidb_users.sqlite",
  credentials_pass = Sys.getenv("GIDB_USERS_PASSPHRASE")
)
