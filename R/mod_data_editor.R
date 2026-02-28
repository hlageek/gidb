#' data_editor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_editor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    "Game",
    navlistPanel(
      id = ns("gametabs"),
      tabPanel("Game Info"), # title, year, url, parent game, external IDs
      tabPanel("Developer"), # dev selector (or inline if simple)
      tabPanel("Classification"), # group, genres, collections
      tabPanel("Notes"),
      widths = c(2, 10)
    )
  )
}

#' data_editor Server Functions
#'
#' @noRd
mod_data_editor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_data_editor_ui("data_editor_1")

## To be copied in the server
# mod_data_editor_server("data_editor_1")
