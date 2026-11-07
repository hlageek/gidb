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
        wellPanel(selectInput(
          inputId = ns("game"),
          label = "",
          choices = ""
        )),
        "or enter new game",
        wellPanel(
          no_autofill(textInput(
            inputId = ns("new_game_title"),
            label = "Manual entry",
            placeholder = "Game title"
          )),
          actionButton(
            inputId = ns("new_game_manual"),
            label = "Create"
          )
        ),
        # .no-steppers: the steppers would edit an external id by clicking,
        # which is never what the editor means to do.
        wellPanel(
          class = "no-steppers",
          # value = NULL, not NA: formatNoSci() would write value="NA" into the
          # tag, an invalid number the browser then marks.
          no_autofill(numericInput(
            inputId = ns("new_game_mobygames_id"),
            label = "MobyGames ID",
            value = NULL,
            min = 1,
            step = 1
          )),
          actionButton(
            inputId = ns("new_game_api"),
            label = "Fetch"
          )
        )
      ),
      div(
        uiOutput(ns("game_title")),
        # .game-editor-scroll: the panels grow past the screen once a game is
        # loaded, so the accordion scrolls inside the viewport; see custom.css.
        div(
          class = "game-editor-scroll",
          bslib::accordion(
            id = ns("game_editor"),
            open = TRUE,
            bslib::accordion_panel(
              icon = bsicons::bs_icon("upc"),
              title = "Identifiers",
              value = "identifiers",
              uiOutput(ns("panel_identifiers_ui"), class = "game-identifiers")
            ),
            bslib::accordion_panel(
              icon = bsicons::bs_icon("info-circle"),
              title = "Core info",
              value = "core",
              uiOutput(ns("panel_info_ui"), class = "game-identifiers")
            )
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
    define_game_editor_helpers(environment())
    ns <- session$ns
    loc <- reactiveValues(observer_game_editor = 0, selected_game = NULL)

    # Fill the selector on launch with nothing selected; later runs refresh its
    # labels and keep something selected. loc$selected_game overrides the
    # current value -- a game that was just created is not in input$game yet --
    # and the handler is isolated, so input$game is read as it stands. With
    # nothing to select, the blank entry is used, which hides the editor UI
    # (its outputs req() on input$game) until another game is picked.
    # add_gidb_ids() decorates the labels for display only; the ids stay the
    # values, so input$game is a plain gidb_id everywhere else.
    observeEvent(loc$observer_game_editor, {
      updateSelectInput(
        session = session,
        inputId = "game",
        choices = c("", add_gidb_ids(db_list_games(pool))),
        selected = loc$selected_game %||% input$game %||% ""
      )
      loc$selected_game <- NULL
    })

    # Title UI -----
    output$game_title <- renderUI({
      req(input$game)
      # Read as a trigger:
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

    # The panels hold the page's first inputs, so a password manager can drop a
    # stored username into the title before anyone types. A browser only reports
    # its inputs once it is connected, which is the first moment they can be
    # pushed back to empty -- ignoreNULL is what keeps this from firing earlier.
    observeEvent(
      input$new_game_title,
      {
        clear_new_game_inputs()
      },
      once = TRUE
    )

    # A title is all the games table requires, so a manual entry starts there
    # and the details are filled in through the editor of the game this creates.
    observeEvent(input$new_game_manual, {
      # game_title is NOT NULL; an empty value would reach SQL as NULL. Nothing
      # closes on a rejected click since the input lives in the panel, so the
      # reason has to be said out loud. isTRUE(): a cleared input can arrive as
      # NULL, which req() used to halt on and if() would choke on.
      if (!isTRUE(nzchar(trimws(input$new_game_title)))) {
        showNotification(
          "A title is required to create a game",
          type = "warning"
        )
        clear_new_game_inputs()
        return()
      }

      create_game_db()
    })

    # MobyGames ids are positive whole numbers without leading zeros. The
    # numeric input keeps letters out but not a 0, and a decimal can be typed.
    # as.character(): NULL (empty field) and NA both fail grepl, so a missing id
    # is rejected the same way as a malformed one.
    observeEvent(input$new_game_api, {
      mobygames_id <- as.character(input$new_game_mobygames_id)
      if (!isTRUE(grepl("^[1-9]\\d*$", mobygames_id))) {
        showNotification(
          paste(
            "A MobyGames ID is required:",
            "a whole number above 0, no leading zeros"
          ),
          type = "warning"
        )
        clear_new_game_inputs()
        return()
      }

      ## TODO fetch the metadata of the id and create the game from them
      showNotification(
        "Fetching from MobyGames is not wired up yet",
        type = "message"
      )
      clear_new_game_inputs()
    })

    # Edit title  ----

    # Open the modal, prefilled with the stored title
    observeEvent(input$edit_game_title, {
      edit_game_title_modal() # input$edited_game_title
    })

    # Write the edited title, then refresh the selector labels
    observeEvent(input$confirm_edit_game_title, {
      # An empty value would be coerced to SQL NULL by sql_null(). The modal
      # staying open is a weak signal on its own, so name the problem as well.
      if (!isTRUE(nzchar(trimws(input$edited_game_title)))) {
        showNotification("The title cannot be empty", type = "warning")
        return()
      }

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

    # Info UI -----
    info_fields <- game_info_fields()

    purrr::walk(
      info_fields,
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

    output$panel_info_ui <- renderUI({
      req(input$game)

      tagList(
        purrr::map(
          info_fields,
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

    # A rolled up accordion panel is display:none, and shiny does not render an
    # output while it is hidden. The fields would then only be created once the
    # editor unrolls the panel, which is after the game_item observers pushed
    # their values into inputs that did not exist yet -- an update the browser
    # never saw cannot be applied later, so the panel opened empty. Rendering
    # anyway keeps the fields in the DOM, hidden with their panel, where the
    # update reaches them and unrolling only reveals what they already hold.
    outputOptions(output, "panel_identifiers_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "panel_info_ui", suspendWhenHidden = FALSE)
  })
}

## To be copied in the UI:
# mod_game_editor_ui("game_editor_1")

## To be copied in the server:
# mod_game_editor_server("game_editor_1")
