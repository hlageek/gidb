#' data_preview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_preview_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' data_preview Server Functions
#'
#' @noRd
mod_data_preview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_data_preview_ui("data_preview_1")

## To be copied in the server
# mod_data_preview_server("data_preview_1")
