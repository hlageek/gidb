#' game_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_game_item_ui <- function(id, type = NULL, label = NULL) {
  ns <- NS(id)
  ifelse(
    type == "integer",
    tagList(
      numericInput(
        inputId = ns("value"),
        label = label,
        value = NULL,
        updateOn = "blur"
      )
    ),
    tagList(
      textInput(
        inputId = ns("value"),
        label = label,
        updateOn = "blur"
      )
    )
  )
}

#' game_item Server Functions
#'
#' @noRd
mod_game_item_server <- function(
  id,
  table = NULL,
  column = NULL,
  type = NULL,
  gidb_id = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$value, {
      req(input$value)
      showNotification(paste0("Updating ", input$value))
    })
  })
}

## To be copied in the UI
# mod_game_item_ui("game_item_1")

## To be copied in the server
# mod_game_item_server("game_item_1")
