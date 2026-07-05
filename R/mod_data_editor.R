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
#' @noRd
mod_data_editor_server <- function(id, pool, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive state ────────────────────────────────────────────────────────

    loc <- reactiveValues(
      current_tags_token = NULL,
      current_platforms_token = NULL,
      current_originators_token = NULL,
      current_notes_token = NULL
    )

    game_data <- do.call(reactiveValues, default_game_data())

    observeEvent(input$mobygames_id, {
      if (isTruthy(input$mobygames_id)) {
        shinyjs::enable("retrieve_mobygames")
      } else {
        shinyjs::disable("retrieve_mobygames")
      }
    })

    output$editor <- renderUI({
      if (input$entry_mode == "auto") {
        NULL
      } else if (input$entry_mode == "edit") {
        NULL
      } else if (input$entry_mode == "manual") {
        bslib::accordion(
          id = ns("game_accordion"),
          open = TRUE,
          multiple = TRUE,
          panel_identifiers(
            ns = ns,
            identifiers = game_data$identifiers
          ),
          panel_core(ns = ns, info = game_data$info),
          panel_tags(ns = ns),
          panel_platforms(ns = ns),
          panel_originators(ns = ns),
          panel_notes(ns = ns)
        )
      }
    })

    # ── Tags ──────────────────────────────────────────────────────────────────

    output$panel_tags_ui <- shiny::renderUI({
      loc$current_tags_token <- as.numeric(Sys.time()) * 1000
      panel_tags_ui(
        ns = ns,
        game_tags = game_data$game_tags,
        game_tags_opts = list(
          name = c("Action", "Third-person", "Puzzle-solving", "test"),
          category = c("Basic Genres", "Perspective", "Gameplay", "test")
        ),
        token = loc$current_tags_token
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

    # ── Platforms ─────────────────────────────────────────────────────────────

    output$panel_platforms_ui <- renderUI({
      loc$current_platforms_token <- as.numeric(Sys.time()) * 1000
      panel_platforms_ui(
        ns = ns,
        platforms = game_data$platforms,
        platforms_opts = c("Mac"),
        token = loc$current_platforms_token
      )
    })

    observeEvent(input$rm_platform_clicked, {
      game_data$platforms <- game_data$platforms[-input$rm_platform_clicked]
    })

    observeEvent(input$add_platform, {
      game_data$platforms <- c(
        game_data$platforms,
        list(list(name = "", year = NULL))
      )
    })

    observe({
      req(loc$current_platforms_token)
      n <- length(isolate(game_data$platforms))

      purrr::walk(seq_len(n), function(i) {
        name_id <- paste0("platform_", loc$current_platforms_token, "_", i)
        year_id <- paste0("platform_year_", loc$current_platforms_token, "_", i)

        observeEvent(
          input[[name_id]],
          {
            platforms <- game_data$platforms
            if (i <= length(platforms)) {
              platforms[[i]]$name <- input[[name_id]]
              game_data$platforms <- platforms
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )

        observeEvent(
          input[[year_id]],
          {
            platforms <- game_data$platforms
            if (i <= length(platforms)) {
              platforms[[i]]$year <- input[[year_id]]
              game_data$platforms <- platforms
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
          name = c("Glass Studios"),
          role = c("maker"),
          location = c("Boston, United States, US")
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
        input$id_gog,
        input$id_itch
      ),
      {
        game_data$identifiers <- list(
          moby_id = input$id_mobygames,
          id_igdb = input$id_igdb,
          steam_id = input$id_steam,
          gog_id = input$id_gog,
          itch_id = input$id_itch
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    shiny::observeEvent(
      list(
        input$title,
        input$release_year,
        input$official_url,
        input$description
      ),
      {
        game_data$info <- list(
          title = input$title,
          release_year = input$release_year,
          official_url = input$official_url,
          description = strip_html(input$description)
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # ── Preview module ────────────────────────────────────────────────────────
    preview <- mod_game_card_server(
      "game_card_1",
      values = game_data
    )
  })
}

## To be copied in the UI:
# mod_data_editor_ui("data_editor_1")

## To be copied in the server:
# mod_data_editor_server("data_editor_1")
