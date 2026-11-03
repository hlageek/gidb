#' data_editor UI Function
#'
#' @description A shiny Module for adding/editing game records. Supports
#'   manual data entry and semi-automated entry via MobyGames API.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_editor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "editor-layout",

      uiOutput(ns("data_editor"))
    )
  )
}

#' data_editor Server Function
#'
#' @param id Module ID.
#' @param pool Database pool connection.
#' @param user User reactive values (with id field).
#'
#' @return A reactiveValues with `load_game` function to load a game by ID.
#' @noRd
mod_data_editor_server <- function(id, pool, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive state ────────────────────────────────────────────────────────

    loc <- reactiveValues()

    output$data_editor <- renderUI({
      mod_game_item_ui(
        ns(
          "mobygames_id"
        ),
        type = "integer",
        label = "MobyGames ID"
      )
    })

    mod_game_item_server("mobygames_id")
  })
}

## To be copied in the UI:
# mod_data_editor_ui("data_editor_1")

## To be copied in the server:
# mod_data_editor_server("data_editor_1")
