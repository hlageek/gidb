#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Authentication
  auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = golem::get_golem_options(which = "credentials_path"),
      passphrase = golem::get_golem_options(which = "credentials_pass")
    ),
    timeout = 60
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(auth)
  })
}
