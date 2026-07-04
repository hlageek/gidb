#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session, pool) {
  # Authentication
  glob <- reactiveValues(user = NULL)

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = golem::get_golem_options(which = "credentials_path"),
      passphrase = golem::get_golem_options(which = "credentials_pass")
    ),
    timeout = 60
  )
  observeEvent(req(res_auth), {
    glob$user <- reactiveValuesToList(res_auth)
    names(glob$user)[names(glob$user) == "user"] <- "name"
  })

  mod_data_editor_server("data_editor_1", pool = pool, user = glob$user)
}
