#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Shinyjs
    shinyjs::useShinyjs(),
    # Your application UI logic
    bslib::page_fluid(
      # Header with title and user info
      div(
        class = "d-flex justify-content-between align-items-center mb-3",
        h1(class = "mb-0", "gidb"),
        uiOutput("user_display")
      ),
      shiny::tabsetPanel(
        id = "tabs",
        type = "tabs",
        tabPanel(
          title = "Data preview",
          value = "data_preview",
          mod_data_preview_ui("data_preview_1")
        ),
        tabPanel(
          title = "Data editor",
          value = "data_editor",
          mod_data_editor_ui("data_editor_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gidb"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
