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
          choices = ""
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
      div(
        uiOutput(ns("game_title")),
        bslib::accordion(
          id = ns("game_editor"),
          open = "identifiers",
          bslib::accordion_panel(
            icon = bsicons::bs_icon("upc"),
            title = "Identifiers",
            value = "identifiers",
            uiOutput(ns("panel_identifiers_ui"), class = "game-identifiers")
          ),
          bslib::accordion_panel(
            icon = bsicons::bs_icon("info-circle"),
            title = "Core info",
            value = "core"
          )
        )
      )
    )
  )
}

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
    # The helpers live in mod_game_editor_fct.R but are evaluated in this frame,
    # so they can use input/session/loc/ns/... without argument plumbing.
    define_game_editor_helpers(environment())

    ns <- session$ns
    loc <- reactiveValues(observer_game_editor = 0, selected_game = NULL)

    # Fill the selector on launch with nothing selected; later runs refresh its
    # labels and keep something selected. loc$selected_game overrides the
    # current value -- a game that was just created is not in input$game yet --
    # and the handler is isolated, so input$game is read as it stands. With
    # nothing to select, the blank entry is used, which hides the editor UI
    # (its outputs req() on input$game) until another game is picked.
    observeEvent(loc$observer_game_editor, {
      updateSelectInput(
        session = session,
        inputId = "game",
        choices = c("", db_list_games(pool)),
        selected = loc$selected_game %||% input$game %||% ""
      )
      loc$selected_game <- NULL
    })

    # Title UI -----
    output$game_title <- renderUI({
      req(input$game)
      # Read as a trigger: the stored title is not reactive, so without this it
      # would keep its old text after a rename.
      loc$observer_game_editor

      div(
        class = "game-title-bar",
        h4(stored_title()),
        div(
          class = "game-title-meta",
          span(glue::glue("GIDB ID: {input$game}")),
          actionButton(
            ns("edit_game_title"),
            label = "",
            icon = bsicons::bs_icon("pencil")
          )
        )
      )
    })

    # New game ----

    # A title is all the games table requires, so a manual entry starts there
    # and the details are filled in through the editor of the game this creates.
    observeEvent(input$new_game_manual, {
      new_game_manual_modal()
    })

    observeEvent(input$confirm_new_game, {
      # game_title is NOT NULL; an empty value would reach SQL as NULL.
      req(nzchar(trimws(input$new_game_title)))
      new_game_db()
    })

    ## TODO ask for a MobyGames ID and fetch its metadata
    # observeEvent(input$new_game_api, {
    #   new_game_api_modal()
    # })

    # Edit title  ----

    # Open the modal, prefilled with the stored title
    observeEvent(input$edit_game_title, {
      edit_game_title_modal()
    })

    # Write the new title, then refresh the selector labels
    observeEvent(input$confirm_edit_game_title, {
      # An empty value would be coerced to SQL NULL by sql_null().
      req(nzchar(trimws(input$edited_game_title)))
      edit_game_title_db()
    })

    # Identifiers UI -----
    identifier_fields <- game_identifier_fields()

    purrr::walk(
      identifier_fields,
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

    output$panel_identifiers_ui <- renderUI({
      req(input$game)

      tagList(
        purrr::map(
          identifier_fields,
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
