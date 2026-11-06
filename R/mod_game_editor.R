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

    # Set game selector
    observeEvent(loc$observer_game_editor, {
      req(loc$observer_game_editor)
      if (loc$observer_game_editor > 0) {
        req(loc$selected_game)
      }
      updateSelectInput(
        session = session,
        inputId = "game",
        choices = c("", db_list_games(pool)),
        selected = loc$selected_game
      )
      loc$selected_game <- NULL
    })

    # Title UI -----
    output$game_title <- renderUI({
      req(input$game)

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
            ns("edit_title"),
            label = "",
            icon = bsicons::bs_icon("pencil")
          )
        )
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
  })
}

## To be copied in the UI:
# mod_game_editor_ui("game_editor_1")

## To be copied in the server:
# mod_game_editor_server("game_editor_1")
