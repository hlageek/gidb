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

#' Field definitions for the Identifiers panel
#'
#' One entry per field. Used by both loops below -- the server loop (one
#' mod_game_item_server() per entry) and the UI loop (one mod_game_item_ui()
#' per entry) -- so module id, label and DB column cannot drift apart.
#'
#' @noRd

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
    loc <- reactiveValues()
    loc$observer_game_editor <- 0
    loc$selected_game <- NULL

    # Fill the selector on launch with nothing selected; later runs only refresh
    # its labels. loc$selected_game carries the current selection through a
    # refresh; with none to carry, the blank entry is selected instead, which
    # hides the editor UI (its outputs req() on input$game) until another game
    # is picked.
    observeEvent(loc$observer_game_editor, {
      updateSelectInput(
        session = session,
        inputId = "game",
        choices = c("", db_list_games(pool)),
        selected = loc$selected_game %||% ""
      )
      loc$selected_game <- NULL
    })

    # Title UI -----
    output$game_title <- renderUI({
      req(input$game)
      # Read as a trigger: db_list_games() is not reactive, so without this the
      # title would keep its old text after a rename.
      loc$observer_game_editor

      games <- db_list_games(pool)
      title <- trimws(gsub(
        "(.*)\\(GIDB:.*\\)",
        "\\1",
        names(games[games == input$game])
      ))
      div(
        class = "game-title-bar",
        h4(title),
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

    # Edit title  ----

    # Open the modal, prefilled with the stored title
    observeEvent(input$edit_game_title, {
      current_title <- db_read_game_item(
        conn = pool,
        table = "games",
        column = "game_title",
        gidb_id = input$game
      )
      if (is.null(current_title) || is.na(current_title[1])) {
        current_title <- ""
      }

      showModal(
        modalDialog(
          textInput(
            inputId = ns("edited_game_title"),
            label = "Edit title",
            value = current_title
          ),
          footer = tagList(
            actionButton(
              inputId = ns("confirm_edit_game_title"),
              label = "Confirm",
              class = "btn-primary"
            ),
            modalButton("Cancel")
          )
        )
      )
    })

    # Write the new title, then refresh the selector labels
    observeEvent(input$confirm_edit_game_title, {
      # Empty input would be coerced to SQL NULL by sql_null() and wipe the title.
      req(nzchar(trimws(input$edited_game_title)))

      new_title <- trimws(input$edited_game_title)
      result <- tryCatch(
        db_update_game_item(
          conn = pool,
          table = "games",
          column = "game_title",
          value = new_title,
          user_id = user$editor_id,
          gidb_id = input$game
        ),
        error = function(e) e
      )

      # NULL means the UPDATE matched no row (e.g. the game was deleted).
      if (inherits(result, "error")) {
        showNotification(
          paste("Update failed:", conditionMessage(result)),
          type = "error"
        )
        return()
      }
      if (is.null(result)) {
        showNotification(
          "Update failed: no matching game row",
          type = "error"
        )
        return()
      }

      removeModal()
      loc$selected_game <- input$game
      loc$observer_game_editor <- update_observer(loc$observer_game_editor)
      showNotification(
        paste(user$display_name, "updated the title to", new_title),
        type = "message"
      )
    })

    # Identifiers UI -----
    game_editor_fields <- list(
      list(
        id = "mobygames_id",
        column = "mobygames_id",
        type = "integer",
        label = tags$a(
          "MobyGames ID",
          href = "https://www.mobygames.com/",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      list(
        id = "igdb_id",
        column = "igdb_id",
        type = "integer",
        label = tags$a(
          "IGDB ID",
          href = "https://www.igdb.com/",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      list(
        id = "steam_id",
        column = "steam_id",
        type = "integer",
        label = tags$a(
          "Steam ID",
          href = "https://store.steampowered.com/",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      list(
        id = "gog_id",
        column = "gog_id",
        type = "integer",
        label = tags$a(
          "GOG ID",
          href = "https://www.gog.com/",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      list(
        id = "itch_id",
        column = "itch_id",
        type = "text",
        label = tags$a(
          "itch.io slug",
          href = "https://itch.io/",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      ),
      list(
        id = "wikidata_id",
        column = "wikidata_id",
        type = "text",
        label = tags$a(
          "Wikidata ID",
          href = "https://www.wikidata.org/",
          target = "_blank",
          rel = "noopener noreferrer"
        )
      )
    )

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

    # Section placeholder ----
  })
}

## To be copied in the UI:
# mod_game_editor_ui("game_editor_1")

## To be copied in the server:
# mod_game_editor_server("game_editor_1")
