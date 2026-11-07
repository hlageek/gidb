#' Define the game_item helpers inside the module environment
#'
#' @param env Environment to define the helpers in; defaults to the caller,
#'   i.e. the module function frame.
#' @return `env`, invisibly.
#' @noRd
define_game_item_helpers <- function(env = parent.frame()) {
  evalq(
    {
      # Show the stored value in the widget this item was built with
      update_game_item_ui <- function() {
        # The text widgets assign the value straight to el.value, so an empty
        # field has to arrive as ""; NA or NULL would read back as "NA"/"null".
        # The number input is the opposite: NA is what empties it, a NULL would
        # be dropped from the message and change nothing.
        value <- loc$stored_value
        if (is.null(value) || length(value) == 0 || is.na(value[1])) {
          value <- ""
        }

        switch(
          game_item_type(type),
          "integer" = updateNumericInput(
            session,
            "value",
            value = loc$stored_value
          ),
          "character" = updateTextInput(session, "value", value = value),
          "text" = updateTextAreaInput(session, "value", value = value)
        )
      }

      # Update database entry for the game item
      update_game_item_db <- function() {
        result <- tryCatch(
          db_update_game_item(
            conn = pool,
            table = table,
            column = column,
            value = input$value,
            user_id = user$editor_id,
            gidb_id = loc$gidb_id
          ),
          error = function(e) e
        )

        if (inherits(result, "error")) {
          showNotification(
            paste("Update failed:", conditionMessage(result)),
            type = "error"
          )
          return(invisible(NULL))
        }

        label_text <- if (is.null(label)) {
          if (is.null(column)) id else column
        } else {
          label
        }
        label_text <- strip_html(as.character(label_text))

        # NULL means the UPDATE matched no row (e.g. the game was deleted).
        if (is.null(result)) {
          showNotification(
            paste("Update failed:", label_text, "-- no matching game row"),
            type = "error"
          )
          return(invisible(NULL))
        }

        showNotification(
          glue::glue(
            "{user$display_name} updated {label_text} to {input$value}"
          ),
          type = "message"
        )

        invisible(result)
      }
    },
    envir = env
  )

  invisible(env)
}
