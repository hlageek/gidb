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

      # ── LEFT: Preview panel ────────────────────────────────────────────────

      div(
        class = "preview-panel",
        mod_game_card_ui(ns("game_card_1")),
      ),

      # ── RIGHT: Editor panel (flat accordion, no wrapper card) ─────────────
      div(
        class = "editor-panel",
        radioButtons(
          ns("entry_mode"),
          label = "Entry mode",
          choices = c(
            "Manual" = "manual",
            "MobyGames" = "auto",
            "Edit existing" = "edit"
          ),
          selected = "manual",
          inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'auto'", ns("entry_mode")),
          bslib::layout_columns(
            col_widths = c(8, 4),
            textInput(
              ns("mobygames_id"),
              "MobyGames ID",
              placeholder = "e.g. 366",
              width = "100%"
            ),
            div(
              style = "padding-top: 1.85rem;",
              shinyjs::disabled(actionButton(
                ns("retrieve_mobygames"),
                "Fetch",
                icon = icon("cloud-download-alt"),
                class = "btn-primary w-100"
              ))
            )
          )
        ),
        uiOutput(ns("editor"))
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

    # ── Reactive state ────────────────────────────────────────────────────────

    loc <- reactiveValues(
      current_tags_token = NULL,
      current_originators_token = NULL,
      current_notes_token = NULL,
      gidb_id = NULL,  # Track the gidb_id of the currently loaded game
      edit_loaded = FALSE,  # Track if a game has been loaded in edit mode
      pending_mode = NULL,  # Store requested mode during confirmation
      games_refresh_trigger = 1  # Trigger to refresh games list (start at 1 for initial load)
    )

    game_data <- do.call(reactiveValues, default_game_data())

    # Helper function to check if there are unsaved changes
    has_unsaved_changes <- function() {
      # Check core info
      if (!is.null(game_data$info$title) && nzchar(game_data$info$title)) return(TRUE)
      if (!is.null(game_data$info$release_year) && !is.na(game_data$info$release_year)) return(TRUE)
      if (!is.null(game_data$info$official_url) && nzchar(game_data$info$official_url)) return(TRUE)

      # Check identifiers
      ids <- game_data$identifiers
      if (!is.null(ids$moby_id) && nzchar(ids$moby_id)) return(TRUE)
      if (!is.null(ids$id_igdb) && nzchar(ids$id_igdb)) return(TRUE)
      if (!is.null(ids$steam_id) && nzchar(ids$steam_id)) return(TRUE)
      if (!is.null(ids$gog_id) && nzchar(ids$gog_id)) return(TRUE)

      # Check tags, originators, notes
      if (length(game_data$game_tags) > 0) return(TRUE)
      if (length(game_data$originators) > 0) return(TRUE)
      if (length(game_data$notes) > 0 && any(nzchar(game_data$notes))) return(TRUE)

      FALSE
    }

    # Function to clear all game data
    clear_game_data <- function() {
      game_data$info <- list(title = NULL, release_year = NULL, official_url = NULL)
      game_data$identifiers <- list(moby_id = NULL, id_igdb = NULL, steam_id = NULL, gog_id = NULL)
      game_data$game_tags <- list()
      game_data$originators <- list()
      game_data$notes <- character(0)
      loc$gidb_id <- NULL
      loc$edit_loaded <- FALSE
      loc$mobygames_called <- FALSE
    }

    # Internal function to load game data
    load_game_internal <- function(gidb_id) {
      req(gidb_id)
      loaded <- load_game_data(pool, as.integer(gidb_id))
      if (!is.null(loaded)) {
        # Update game_data with loaded values
        game_data$info <- loaded$info
        game_data$identifiers <- loaded$identifiers
        game_data$game_tags <- loaded$game_tags
        game_data$originators <- loaded$originators
        game_data$notes <- loaded$notes
        loc$gidb_id <- as.integer(gidb_id)
        loc$edit_loaded <- TRUE  # Track that a game was loaded for edit mode

        # Switch to edit mode
        updateRadioButtons(session, "entry_mode", selected = "edit")

        showNotification(paste("Loaded game:", loaded$info$title %||% "Untitled"), type = "message")
      } else {
        showNotification("Game not found", type = "error")
      }
    }

    # Reactive to track last saved game ID (for triggering table refresh)
    rv_saved_game_id <- reactiveVal(NULL)

    # Expose functions and values via return value
    return_values <- reactiveValues()
    return_values$load_game <- load_game_internal
    return_values$saved_game_id <- rv_saved_game_id

    # Reactive games list for the selector - refreshes on demand
    games_list <- reactive({
      loc$games_refresh_trigger
      db_read_games(pool)
    })

    # Load all dropdown options from database
    all_categories <- db_read_all_categories(pool)
    all_tags <- db_read_all_tags(pool)
    all_originators <- db_read_all_originators(pool)
    all_roles <- db_read_all_roles(pool)

    # Reactive values for tag options (computed from loaded data + global options)
    tags_opts <- reactive({
      list(
        category = unique(c(all_categories, unlist(lapply(game_data$game_tags, `[[`, "category")))),
        name = unique(c(all_tags, unlist(lapply(game_data$game_tags, `[[`, "name"))))
      )
    })

    # Helper to get tags for a specific category (lazy evaluation)
    get_tags_for_category <- function(category) {
      if (is.null(category) || !nzchar(category)) {
        return(character(0))
      }
      db_read_all_tags(pool, category = category)
    }

    # Track previous entry mode (starts as NULL until first user interaction)
    prev_mode <- reactiveVal(NULL)
    has_user_changed_mode <- reactiveVal(FALSE)

    # Handle entry mode switching with confirmation
    observeEvent(input$entry_mode, {
      req(input$entry_mode)  # Wait for actual value

      current_mode <- isolate(prev_mode())

      # If this is the first time we have a mode, just record it
      if (is.null(current_mode)) {
        prev_mode(input$entry_mode)
        return()
      }

      # If user hasn't changed mode yet, just update prev_mode
      if (!has_user_changed_mode() && current_mode == input$entry_mode) {
        return()
      }

      # Mark that we now have an initial mode
      if (!has_user_changed_mode() && current_mode != input$entry_mode) {
        has_user_changed_mode(TRUE)
        prev_mode(input$entry_mode)
        return()
      }

      # User is actively switching modes - show confirmation
      if (current_mode != input$entry_mode) {
        showModal(
          modalDialog(
            title = "Switch Mode",
            p("Switching modes will clear any unsaved changes. Continue?"),
            easyClose = FALSE,
            footer = tagList(
              actionButton(ns("mode_switch_cancel"), "Cancel", class = "btn-default"),
              actionButton(ns("mode_switch_confirm"), "Continue", class = "btn-danger")
            )
          )
        )
        # Store the requested mode temporarily
        loc$pending_mode <- input$entry_mode
      }
    })

    # Handle mode switch cancellation
    observeEvent(input$mode_switch_cancel, {
      removeModal()
      # Revert to previous mode
      updateRadioButtons(session, "entry_mode", selected = isolate(prev_mode()))
    })

    # Handle mode switch confirmation
    observeEvent(input$mode_switch_confirm, {
      removeModal()
      # Clear game data and update mode
      clear_game_data()
      prev_mode(isolate(loc$pending_mode))
      showNotification("Mode switched, data cleared", type = "message")
    })

    observeEvent(input$mobygames_id, {
      if (isTruthy(input$mobygames_id)) {
        shinyjs::enable("retrieve_mobygames")
      } else {
        shinyjs::disable("retrieve_mobygames")
      }
    })

    # Update load button state based on selection
    observeEvent(input$load_existing_select, {
      if (isTruthy(input$load_existing_select)) {
        shinyjs::enable("load_existing_btn")
      } else {
        shinyjs::disable("load_existing_btn")
      }
    })

    # Handle loading game by selector when in "edit" mode
    observeEvent(input$load_existing_btn, {
      req(input$load_existing_select)
      loaded <- load_game_data(pool, as.integer(input$load_existing_select))
      if (!is.null(loaded)) {
        game_data$info <- loaded$info
        game_data$identifiers <- loaded$identifiers
        game_data$game_tags <- loaded$game_tags
        game_data$originators <- loaded$originators
        game_data$notes <- loaded$notes
        loc$gidb_id <- as.integer(input$load_existing_select)
        loc$edit_loaded <- TRUE  # Track that a game was loaded
        showNotification(paste("Loaded game:", loaded$info$title %||% "Untitled"), type = "message")
      } else {
        showNotification("Game not found", type = "error")
      }
    })

    # Observer to refresh games list when entering edit mode
    observeEvent(input$entry_mode, {
      if (input$entry_mode == "edit") {
        loc$games_refresh_trigger <- loc$games_refresh_trigger + 1
      }
    }, ignoreInit = TRUE)

    output$editor <- renderUI({
      # Get current games list for the selector
      current_games <- games_list()

      # MobyGames API mode - show editor only after successful fetch
      if (input$entry_mode == "auto" && isTruthy(loc$mobygames_called)) {
        bslib::accordion(
          id = ns("game_accordion"),
          open = TRUE,
          multiple = TRUE,
          panel_core(ns = ns, info = game_data$info),
          panel_identifiers(
            ns = ns,
            identifiers = game_data$identifiers
          ),
          panel_tags(ns = ns),
          panel_originators(ns = ns),
          panel_notes(ns = ns)
        )
      } else if (input$entry_mode == "edit" && isTruthy(loc$edit_loaded)) {
        # Edit mode - show editor only after a game has been loaded
        tagList(
          load_existing_panel(ns = ns, games = current_games),
          bslib::accordion(
            id = ns("game_accordion"),
            open = TRUE,
            multiple = TRUE,
            panel_core(ns = ns, info = game_data$info),
            panel_identifiers(
              ns = ns,
              identifiers = game_data$identifiers
            ),
            panel_tags(ns = ns),
            panel_originators(ns = ns),
            panel_notes(ns = ns)
          )
        )
      } else if (input$entry_mode == "edit") {
        # Edit mode - no game loaded yet, just show the selector
        load_existing_panel(ns = ns, games = current_games)
      } else if (input$entry_mode == "manual") {
        bslib::accordion(
          id = ns("game_accordion"),
          open = TRUE,
          multiple = TRUE,
          panel_core(ns = ns, info = game_data$info),
          panel_identifiers(
            ns = ns,
            identifiers = game_data$identifiers
          ),
          panel_tags(ns = ns),
          panel_originators(ns = ns),
          panel_notes(ns = ns)
        )
      }
    })

    # Render delete button container (only when a game is loaded)
    output$delete_game_btn_container <- renderUI({
      if (isTruthy(loc$gidb_id) && input$entry_mode == "edit") {
        # Find the game title from games_list
        current_games <- games_list()
        game_row <- current_games[current_games$gidb_id == loc$gidb_id, ]
        game_title <- if (nrow(game_row) > 0) game_row$title[1] else NULL
        render_delete_button(ns, loc$gidb_id, game_title)
      } else {
        tagList()
      }
    })

    # Handle delete button click - show confirmation modal
    observeEvent(input$delete_game_btn, {
      req(loc$gidb_id)
      current_games <- games_list()
      game_row <- current_games[current_games$gidb_id == loc$gidb_id, ]
      game_title <- if (nrow(game_row) > 0) game_row$title[1] else "Untitled"

      showModal(
        modalDialog(
          title = "Confirm Delete",
          p(sprintf("Are you sure you want to delete '%s' (ID: %s)?", game_title, loc$gidb_id)),
          p(class = "text-danger small", tags$strong("This action cannot be undone.")),
          easyClose = FALSE,
          footer = tagList(
            actionButton(ns("delete_cancel"), "Cancel", class = "btn-default"),
            actionButton(ns("delete_confirm"), "Delete", class = "btn-danger")
          )
        )
      )
    })

    # Handle delete cancellation
    observeEvent(input$delete_cancel, {
      removeModal()
    })

    # Handle delete confirmation
    observeEvent(input$delete_confirm, {
      req(loc$gidb_id)
      removeModal()

      result <- tryCatch({
        db_delete_game(pool, as.integer(loc$gidb_id))
      }, error = function(e) {
        FALSE
      })

      if (result) {
        showNotification(paste("Game deleted successfully"), type = "message")
        # Clear the editor and refresh the preview table
        clear_game_data()
        prev_mode("manual")
        updateRadioButtons(session, "entry_mode", selected = "manual")
        # Refresh games list so deleted game disappears from selector
        loc$games_refresh_trigger <- loc$games_refresh_trigger + 1
        # Trigger table refresh via the saved_game_id signal
        rv_saved_game_id(-1)  # Special value to indicate delete
      } else {
        showNotification(paste("Failed to delete game"), type = "error")
      }
    })

    # ── Tags ──────────────────────────────────────────────────────────────────

    output$panel_tags_ui <- shiny::renderUI({
      loc$current_tags_token <- as.numeric(Sys.time()) * 1000
      # Build category-to-tags mapping for filtered suggestions
      cats <- tags_opts()$category
      cat_to_tags <- setNames(
        lapply(cats, function(cat) get_tags_for_category(cat)),
        cats
      )
      panel_tags_ui(
        ns = ns,
        game_tags = game_data$game_tags,
        game_tags_opts = tags_opts(),
        token = loc$current_tags_token,
        cat_to_tags = cat_to_tags
      )
    })

    observeEvent(input$rm_tag_clicked, {
      game_data$game_tags <- game_data$game_tags[-input$rm_tag_clicked]
    })

    observeEvent(input$add_tag, {
      game_data$game_tags <- c(
        game_data$game_tags,
        list(list(name = "", category = ""))
      )
    })

    observe({
      req(loc$current_tags_token)
      n <- length(shiny::isolate(game_data$game_tags))

      purrr::walk(seq_len(n), function(i) {
        name_id <- paste0("tag_name_", loc$current_tags_token, "_", i)
        cat_id <- paste0("tag_cat_", loc$current_tags_token, "_", i)

        observeEvent(
          input[[name_id]],
          {
            tags <- game_data$game_tags
            if (i <= length(tags)) {
              tags[[i]]$name <- input[[name_id]]
              game_data$game_tags <- tags
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )

        observeEvent(
          input[[cat_id]],
          {
            tags <- game_data$game_tags
            if (i <= length(tags)) {
              tags[[i]]$category <- input[[cat_id]]
              game_data$game_tags <- tags
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      })
    })

    # ── Originators ───────────────────────────────────────────────────────────

    output$panel_originators_ui <- renderUI({
      loc$current_originators_token <- as.numeric(Sys.time()) * 1000
      panel_originators_ui(
        ns = ns,
        originators = game_data$originators,
        originators_opts = list(
          name = all_originators,
          role = all_roles,
          location = character(0)  # Location not yet supported from DB
        ),
        token = loc$current_originators_token
      )
    })

    observeEvent(input$rm_originator_clicked, {
      game_data$originators <- game_data$originators[
        -input$rm_originator_clicked
      ]
    })

    observeEvent(input$add_originator, {
      game_data$originators <- c(
        game_data$originators,
        list(list(name = "", role = "", location = ""))
      )
    })

    observe({
      req(loc$current_originators_token)
      n <- length(isolate(game_data$originators))

      purrr::walk(seq_len(n), function(i) {
        name_id <- paste0("originator_", loc$current_originators_token, "_", i)
        role_id <- paste0(
          "originator_role_",
          loc$current_originators_token,
          "_",
          i
        )
        location_id <- paste0(
          "originator_location_",
          loc$current_originators_token,
          "_",
          i
        )

        observeEvent(
          input[[name_id]],
          {
            originators <- game_data$originators
            if (i <= length(originators)) {
              originators[[i]]$name <- input[[name_id]]
              game_data$originators <- originators
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )

        observeEvent(
          input[[role_id]],
          {
            originators <- game_data$originators
            if (i <= length(originators)) {
              originators[[i]]$role <- input[[role_id]]
              game_data$originators <- originators
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )

        observeEvent(
          input[[location_id]],
          {
            originators <- game_data$originators
            if (i <= length(originators)) {
              originators[[i]]$location <- input[[location_id]]
              game_data$originators <- originators
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      })
    })

    # ── Notes ─────────────────────────────────────────────────────────────────

    output$panel_notes_ui <- renderUI({
      loc$current_notes_token <- as.numeric(Sys.time()) * 1000
      panel_notes_ui(
        ns = ns,
        notes = game_data$notes,
        token = loc$current_notes_token
      )
    })

    observeEvent(input$rm_note_clicked, {
      game_data$notes <- game_data$notes[-input$rm_note_clicked]
    })

    observeEvent(input$add_note, {
      game_data$notes <- c(game_data$notes, "")
    })

    observe({
      req(loc$current_notes_token)
      n <- length(isolate(game_data$notes))

      purrr::walk(seq_len(n), function(i) {
        note_id <- paste0("note_", loc$current_notes_token, "_", i)

        observeEvent(
          input[[note_id]],
          {
            notes <- game_data$notes
            if (i <= length(notes)) {
              notes[[i]] <- input[[note_id]]
              game_data$notes <- notes
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      })
    })

    # ── Core info / Identifiers (unchanged — scalar fields, no token needed) ──

    shiny::observeEvent(
      list(
        input$id_mobygames,
        input$id_igdb,
        input$id_steam,
        input$id_gog
      ),
      {
        # Convert empty strings to NULL for proper database handling
        game_data$identifiers <- list(
          moby_id = if (nzchar(trimws(input$id_mobygames %||% ""))) input$id_mobygames else NULL,
          id_igdb = if (nzchar(trimws(input$id_igdb %||% ""))) input$id_igdb else NULL,
          steam_id = if (nzchar(trimws(input$id_steam %||% ""))) input$id_steam else NULL,
          gog_id = if (nzchar(trimws(input$id_gog %||% ""))) input$id_gog else NULL
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    shiny::observeEvent(
      list(
        input$title,
        input$release_year,
        input$official_url
      ),
      {
        # Convert empty strings to NULL for proper database handling
        game_data$info <- list(
          title = if (nzchar(trimws(input$title %||% ""))) input$title else NULL,
          release_year = suppressWarnings(as.integer(input$release_year)),
          official_url = if (nzchar(trimws(input$official_url %||% ""))) input$official_url else NULL
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # MobyGames API ----------------------

    observeEvent(input$retrieve_mobygames, {
      req(input$mobygames_id)
      showNotification("Initiating API call", type = "message")

      result <- tryCatch(
        moby_get_game_metadata(input$mobygames_id),
        error = function(e) {
          showNotification(
            paste("Fetch failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )

      req(result)

      game_data$info <- list(
        title = result$moby_title,
        release_year = result$release_year,
        official_url = result$official_url
      )

      game_data$identifiers <- list(
        moby_id = result$moby_id
      )

      # tags: nested by category (basic_genres, perspective, ...) -> flatten to
      # list(list(name, category), ...)
      flat_tags <- purrr::imap(result$tags, function(entries, category) {
        category_label <- category |>
          gsub("_", " ", x = _) |>
          tools::toTitleCase()
        purrr::map(entries, function(t) {
          list(name = t$name %||% "", category = category_label)
        })
      }) |>
        purrr::flatten()

      game_data$game_tags <- if (length(flat_tags) > 0) {
        unname(flat_tags)
      } else {
        list(list(name = "", category = ""))
      }

      # originators: no location field from MobyGames — left blank for manual fill-in
      game_data$originators <- if (length(result$originators) > 0) {
        purrr::map(result$originators, function(o) {
          list(
            name = o$company_name %||% "",
            role = o$role %||% "",
            location = ""
          )
        })
      } else {
        list(list(name = "", role = "", location = ""))
      }

      showNotification(paste("Loaded:", result$moby_title), type = "message")
      loc$mobygames_called <- TRUE
    })

    # ── Preview module ────────────────────────────────────────────────────────
    game_card_server <- mod_game_card_server(
      "game_card_1",
      game_data = game_data
    )
    observeEvent(game_card_server$reset_flag, {
      if (game_card_server$reset_flag > 0) {
        # Reset all game data to default empty state
        game_data$info <- list(title = NULL, release_year = NULL, official_url = NULL)
        game_data$identifiers <- list(moby_id = NULL, id_igdb = NULL, steam_id = NULL, gog_id = NULL)
        game_data$game_tags <- list()
        game_data$originators <- list()
        game_data$notes <- character(0)
        loc$gidb_id <- NULL
        loc$edit_loaded <- FALSE
        loc$mobygames_called <- FALSE
        showNotification("Game data reset", type = "message")
      }
    })

    observeEvent(game_card_server$saved_flag, {
      req(user$editor_id)  # Ensure editor_id is available
      if (game_card_server$saved_flag > 0) {
        result <- tryCatch({
          saved_id <- save_game_data(
            pool = pool,
            game_data = game_data,
            user_id = user$editor_id,
            gidb_id = loc$gidb_id
          )
          showNotification(paste("Saved game (ID:", saved_id, ")"), type = "message")
          loc$gidb_id <- saved_id
          rv_saved_game_id(saved_id)
          # Refresh games list so selector shows the newly saved game
          loc$games_refresh_trigger <- loc$games_refresh_trigger + 1
          saved_id
        }, error = function(e) {
          showNotification(paste("Save failed:", e$message), type = "error")
          NULL
        })
      }
    })

    return(return_values)
  })
}

## To be copied in the UI:
# mod_data_editor_ui("data_editor_1")

## To be copied in the server:
# mod_data_editor_server("data_editor_1")
