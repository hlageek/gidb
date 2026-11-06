#' Define the game_editor helpers inside the module environment
#'
#' Same trick as define_game_item_helpers(): the helpers are evaluated in the
#' module frame, so they can use input/session/loc/ns/... without argument
#' plumbing. Has to be called at the top of the module server function.
#'
#' @param env Environment to define the helpers in; defaults to the caller,
#'   i.e. the module function frame.
#' @return `env`, invisibly.
#' @noRd
define_game_editor_helpers <- function(env = parent.frame()) {
  evalq(
    {
      # Stored title, read on demand: once for the title bar, once to prefill
      # the edit modal. Falls back to "" when the column is SQL NULL.
      stored_title <- function(gidb_id = input$game) {
        title <- db_read_game_item(
          conn = pool,
          table = "games",
          column = "game_title",
          gidb_id = gidb_id
        )
        if (is.null(title) || length(title) == 0 || is.na(title[1])) {
          return("")
        }
        title[1]
      }

      # Modal for editing the game title
      edit_game_title_modal <- function() {
        showModal(
          modalDialog(
            textInput(
              inputId = ns("edited_game_title"),
              label = "Edit title",
              value = stored_title()
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
      }

      # Write the new title, then refresh the selector labels
      edit_game_title_db <- function() {
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
        loc$observer_game_editor <- update_observer(loc$observer_game_editor)
        showNotification(
          paste(user$display_name, "updated the title to", new_title),
          type = "message"
        )
      }
    },
    envir = env
  )

  invisible(env)
}

# Identifier fields of the game editor. One entry per field, used by both loops
# in the module server: the server loop (one mod_game_item_server() per entry)
# and the UI loop (one mod_game_item_ui() per entry), so module id, label and DB
# column cannot drift apart.

#' @noRd
game_identifier_fields <- function() {
  list(
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
}
