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

        display_value <- if (nchar(input$value) > 100) {
          paste0(substr(input$value, 1, 100), "...")
        } else {
          input$value
        }

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
            "{user$display_name} updated {label_text} to {display_value}"
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
