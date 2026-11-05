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
      id = ns("editor_layout"),
      div(
        "Select existing game",
        selectInput(
          inputId = ns("game"),
          label = "",
          choices = c("", "test", "retest")
        ),
        "or enter new game",
        wellPanel(
          actionButton(
            inputId = ns("new_game_manual"),
            label = "Manual entry"
          ),
          actionButton(
            inputId = ns("new_game_api"),
            label = "MobyGames ID"
          )
        )
      ),
      bslib::accordion(
        id = ns("game_editor"),
        open = "core",
        bslib::accordion_panel(
          class = "editor-panel",
          icon = bsicons::bs_icon("info-circle"),
          title = "Core Info",
          value = "core",
          mod_game_item_ui(
            ns("mobygames_id"),
            type = "integer",
            label = "MobyGames ID"
          ),
          mod_game_item_ui(
            ns("igdb_id"),
            type = "integer",
            label = "IGDB ID"
          )
        ),
        bslib::accordion_panel(
          icon = bsicons::bs_icon("upc"),
          title = "Identifiers",
          value = "identifiers"
        )
      )
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
    loc <- reactiveValues()

    mod_game_item_server(
      "mobygames_id",
      label = "MobyGames ID",
      pool = pool,
      user = user, # reactiveValues object
      gidb_id = reactive(input$game),
      table = "games",
      column = "mobygames_id",
      type = "integer"
    )

    mod_game_item_server(
      "igdb_id",
      label = "IGDB ID",
      pool = pool,
      user = user,
      gidb_id = reactive(input$game),
      table = "games",
      column = "mobygames_id",
      type = "integer"
    )
  })
}

## To be copied in the UI:
# mod_data_editor_ui("data_editor_1")

## To be copied in the server:
# mod_data_editor_server("data_editor_1")
