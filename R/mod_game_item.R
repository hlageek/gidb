#' The widget a game item shows, checked once
#'
#' The name of the type is spelled out here so a typo cannot quietly fall
#' through to the plain text input, which the module used to do for everything
#' that was not "integer".
#'
#' @param type One of "integer", "character" or "text"; NULL means "character".
#' @return The type, as given or replaced by "character".
#' @noRd
game_item_type <- function(type) {
  type <- type %||% "character"

  if (!identical(type, "integer") &&
      !identical(type, "character") &&
      !identical(type, "text")) {
    stop(
      "type must be \"integer\", \"character\" or \"text\", not \"",
      as.character(type)[1],
      "\".",
      call. = FALSE
    )
  }

  type
}

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
  type <- game_item_type(type)

  # All three report on blur, so a field is written once the editor leaves it
  # instead of on every keystroke.
  input <- switch(
    type,
    # value = NULL, not NA: formatNoSci() would write value="NA" into the tag,
    # an invalid number the browser then marks.
    "integer" = numericInput(
      inputId = ns("value"),
      label = label,
      value = NULL,
      updateOn = "blur"
    ),
    # One line: ids, slugs, urls.
    "character" = textInput(
      inputId = ns("value"),
      label = label,
      value = "",
      updateOn = "blur"
    ),
    # Several lines: notes, descriptions.
    "text" = textAreaInput(
      inputId = ns("value"),
      label = label,
      value = "",
      rows = 3,
      updateOn = "blur"
    )
  )

  # .game-item-multiline asks the identifiers grid for a whole row; see the
  # rules in custom.css.
  div(
    class = paste(
      c(
        "game-item",
        "no-num-ticks",
        if (type == "text") "game-item-multiline"
      ),
      collapse = " "
    ),
    input
  )
}

#' game_item Server Functions
#'
#' @param id Module ID.
#' @param table,column Database table and column the field maps to.
#' @param type Field type: "integer" for a number field, "character" for a one
#'   line text field and "text" for a multi line one. Checked by
#'   [game_item_type()], so an unknown value is an error rather than a field of
#'   another shape.
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
    # The helpers live in mod_game_item_fct.R but are evaluated in this frame,
    # so they can use input/session/loc/pool/... without argument plumbing.
    define_game_item_helpers(environment())

    stopifnot(is.reactive(gidb_id))

    # Normalise a value for comparison across the client boundary: DB NULL,
    # numeric NA and "" compare equal, and as.character() bridges the
    # integer/double difference (12345L in R comes back as 12345).
    value_key <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x[1])) {
        return("")
      }
      as.character(x[1])
    }

    loc <- reactiveValues(gidb_id = NULL, stored_value = NULL)

    # Mirror the reactive into loc so the rest of the module can read it as
    # plain state: loc$gidb_id. Clearing has to happen in an observer: a bare
    # reactive({...}) is lazy -- nothing reads it, so it never runs.
    observe({
      loc$gidb_id <- gidb_id()
    })

    observeEvent(
      loc$gidb_id,
      {
        req(loc$gidb_id)

        value <- db_read_game_item(
          conn = pool,
          table = table,
          column = column,
          gidb_id = loc$gidb_id
        )

        if (is.null(value) || length(value) == 0 || is.na(value[1])) {
          value <- ""
        }

        # Remember what the DB returned before showing it, so the input event
        # that update*Input() triggers is recognised as an echo.
        loc$stored_value <- value
        update_game_item_ui()
      }
    )
    # The field now lives in the UI from page load, so its first value is
    # NULL/empty and cannot trigger this.
    observeEvent(
      input$value,
      {
        req(loc$gidb_id, input$value)

        # update*Input() re-fires input$value; ignore that echo.
        if (identical(value_key(input$value), value_key(loc$stored_value))) {
          return()
        }

        # Only remember the value once the DB accepted it.
        if (!is.null(update_game_item_db())) {
          loc$stored_value <- input$value
        }
      }
    )
  })
}

## To be copied in the UI
# mod_game_item_ui("game_item_1")

## To be copied in the server
# mod_game_item_server("game_item_1")
