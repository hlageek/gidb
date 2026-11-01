#' Run the Shiny Application
#'
#' @param credentials_path Path to the SQLite database for shinymanager.
#' @param credentials_pass Passphrase for the SQLite database.
#' @param onStart A function that will be called before the app is actually run.
#' @param options Named options that should be passed to the `runApp` call.
#' @param enableBookmarking Can be one of "url", "server", or "disable".
#' @param uiPattern A regular expression used to determine which UI should be served.
#' @param ... arguments to pass to golem_opts.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  dbhost = "localhost",
  dbport = 5432L,
  dbname = NULL,
  dbusername = NULL,
  dbpassword = NULL,
  credentials_path = NULL,
  credentials_pass = NULL,
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    golem_opts = list(
      credentials_path = credentials_path,
      credentials_pass = credentials_pass,
      ...
    ),

    {
      pool <- pool::dbPool(
        drv = RPostgres::Postgres(),
        host = dbhost,
        port = dbport,
        dbname = dbname,
        user = dbusername,
        password = dbpassword
      )

      shiny::onStop(function() {
        pool::poolClose(pool)
      })

      # Run the application
      shinyApp(
        ui = shinymanager::secure_app(
          app_ui,
          enable_admin = TRUE,
          fab_position = "bottom-left"
        ),
        server = function(input, output, session) {
          app_server(input, output, session, pool = pool)
        },
        onStart = onStart,
        options = options,
        enableBookmarking = enableBookmarking,
        uiPattern = uiPattern
      )
    }
  )
}
