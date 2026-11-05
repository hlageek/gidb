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
  if (identical(type, "integer")) {
    numericInput(
      inputId = ns("value"),
      label = label,
      value = NA,
      updateOn = "blur"
    )
  } else {
    textInput(
      inputId = ns("value"),
      label = label,
      value = "",
      updateOn = "blur"
    )
  }
}

#' game_item Server Functions
#'
#' @param id Module ID.
#' @param table,column Database table and column the field maps to.
#' @param type Field type ("integer" or anything else for text).
#' @param gidb_id Either a reactive (recommended) or a plain value holding the
#'   id of the record being edited. In the app, you will almost always want to
#'   pass a reactive such as `reactive(input$game)`, otherwise the value is
#'   captured once at module setup and never updates.
#' @noRd
mod_game_item_server <- function(
  id,
  label = NULL,
  pool = NULL,
  user = NULL,
  table = NULL,
  column = NULL,
  type = NULL,
  gidb_id = NULL
) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(gidb_id))

    loc <- reactiveValues(gidb_id = NULL)

    # Mirror the reactive into loc so the rest of the module can read it as
    # plain state: loc$gidb_id. Clearing has to happen in an observer: a bare
    # reactive({...}) is lazy -- nothing reads it, so it never runs.
    observe({
      loc$gidb_id <- gidb_id()
    })
    # The field now lives in the UI from page load, so its first value is
    # NULL/empty and cannot trigger this. ignoreInit = TRUE is belt-and-braces.
    observeEvent(
      input$value,
      {
        req(loc$gidb_id, input$value)

        showNotification(
          glue::glue(
            "{user$display_name} updated {loc$gidb_id} to {input$value}"
          ),
          type = "message"
        )
      }
    )
  })
}

## To be copied in the UI
# mod_game_item_ui("game_item_1")

## To be copied in the server
# mod_game_item_server("game_item_1")
