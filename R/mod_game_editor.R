#' game_editor UI Function
#'
#' @description A shiny Module for adding/editing game records. Supports
#'   manual data entry and semi-automated entry via MobyGames API.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_game_editor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "game-editor-layout",
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
        open = "identifiers",
        bslib::accordion_panel(
          icon = bsicons::bs_icon("upc"),
          title = "Identifiers",
          value = "identifiers",
          uiOutput(ns("panel_identifiers_ui"))
        ),
        bslib::accordion_panel(
          icon = bsicons::bs_icon("info-circle"),
          title = "Core info",
          value = "core"
        )
      )
    )
  )
}

#' Field definitions for the Identifiers panel
#'
#' One entry per field. Used by both loops below -- the server loop (one
#' mod_game_item_server() per entry) and the UI loop (one mod_game_item_ui()
#' per entry) -- so module id, label and DB column cannot drift apart.
#'
#' @noRd
game_editor_fields <- list(
  list(id = "mobygames_id", label = "MobyGames ID", column = "mobygames_id", type = "integer"),
  list(id = "igdb_id", label = "IGDB ID", column = "igdb_id", type = "integer")
)

#' game_editor Server Function
#'
#' @param id Module ID.
#' @param pool Database pool connection.
#' @param user User reactive values (with id field).
#'
#' @return A reactiveValues with `load_game` function to load a game by ID.
#' @noRd
mod_game_editor_server <- function(id, pool, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Start each field's server once, here. Server modules return observers,
    # not UI -- they must never be called from renderUI(). walk() is the
    # side-effect counterpart of map(); the returned observers are not needed.
    purrr::walk(
      game_editor_fields,
      function(field) {
        mod_game_item_server(
          field$id,
          label = field$label,
          pool = pool,
          user = user, # reactiveValues object
          gidb_id = reactive(input$game),
          table = "games",
          column = field$column,
          type = field$type
        )
      }
    )

    # renderUI() only builds UI, for modules that already exist above.
    # ns is session$ns(), so the ids line up with the module namespaces:
    # game_editor_1-mobygames_id-value.
    output$panel_identifiers_ui <- renderUI({
      req(input$game)

      tagList(
        purrr::map(
          game_editor_fields,
          function(field) {
            mod_game_item_ui(
              id = ns(field$id),
              type = field$type,
              label = field$label
            )
          }
        )
      )
    })
  })
}

## To be copied in the UI:
# mod_game_editor_ui("game_editor_1")

## To be copied in the server:
# mod_game_editor_server("game_editor_1")
