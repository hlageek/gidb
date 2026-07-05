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
      n_game_tags = 3
    )

    game_data <- reactiveValues(
      info = list(
        title = "Test game title",
        release_year = 7L,
        description = "my desc",
        official_url = "https://example.com"
      ),
      identifiers = list(
        moby_id = 7L
      ), # named list label → value
      game_tags = list(
        list(name = "Action", category = "Basic Genres"),
        list(name = "Third-person", category = "Perspective"),
        list(name = "Puzzle-solving", category = "Gameplay")
      ), # tags: list of list(name, category)
      platforms = list(
        list(name = "Windows", year = "1998"),
        list(name = "PlayStation", year = "1999")
      ), # list of list(name, year) — structured, not strings
      originators = list(
        list(
          name = "Looking Glass Studios",
          role = "developer",
          location = "Cambridge, United States, US"
        ),
        list(
          name = "Eidos Interactive",
          role = "publisher",
          location = "London, United Kingdom, GB"
        )
      ), # list of list(name, role, location)
      notes = c("Great atmosphere", "Check for a widescreen patch") # character vector
    )

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

    output$panel_tags_ui <- renderUI({
      panel_tags_ui(
        ns = ns,
        game_tags = game_data$game_tags,
        game_tags_opts = list(
          name = c("Action", "Third-person", "Puzzle-solving", "test"),
          category = c("Basic Genres", "Perspective", "Gameplay", "test")
        )
      )
    })
    observeEvent(input$rm_tag_clicked, {
      game_data$game_tags <- game_data$game_tags[
        -input$rm_tag_clicked
      ]
    })

    output$panel_platforms_ui <- renderUI({
      panel_platforms_ui(
        ns = ns,
        platforms = game_data$platforms,
        platforms_opts = c(
          "Mac"
        )
      )
    })
    observeEvent(input$rm_platform_clicked, {
      game_data$platforms <- game_data$platforms[
        -input$rm_platform_clicked
      ]
    })

    output$panel_originators_ui <- renderUI({
      panel_originators_ui(
        ns = ns,
        originators = game_data$originators,
        originators_opts = list(
          name = c("Glass Studios"),
          role = c("maker"),
          location = c("Boston, United States, US")
        )
      )
    })
    observeEvent(input$rm_originator_clicked, {
      game_data$originators <- game_data$originators[
        -input$rm_originator_clicked
      ]
    })

    output$panel_notes_ui <- renderUI({
      panel_notes_ui(
        ns = ns,
        notes = game_data$notes
      )
    })
    observeEvent(input$rm_note_clicked, {
      game_data$notes <- game_data$notes[-input$rm_note_clicked]
    })

    shiny::observeEvent(input$add_tag, loc$n_game_tags <- loc$n_game_tags + 1L)
    shiny::observeEvent(
      input$rm_tag_clicked,
      loc$n_game_tags <- loc$n_game_tags - 1L
    )
    # Observers for update -------------------------
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
    observe({
      n <- length(isolate(game_data$game_tags))
      purrr::walk(seq_len(loc$n_game_tags), function(i) {
        name_id <- paste0("tag_name_", i)
        cat_id <- paste0("tag_cat_", i)

        if (!is.null(input[[name_id]])) {
          game_data$game_tags[[i]]$name <- input[[name_id]]
        }

        if (!is.null(input[[cat_id]])) {
          game_data$game_tags[[i]]$category <- input[[cat_id]]
        }
      })
    })
    observe({
      n <- length(isolate(game_data$platforms))
      purrr::walk(seq_len(n), function(i) {
        name_id <- paste0("platform_", i)
        year_id <- paste0("platform_year_", i)
        if (!is.null(input[[name_id]])) {
          game_data$platforms[[i]]$name <- input[[name_id]]
        }
        if (!is.null(input[[year_id]])) {
          game_data$platforms[[i]]$year <- input[[year_id]]
        }
      })
    })
    observe({
      n <- length(isolate(game_data$originators))
      purrr::walk(seq_len(n), function(i) {
        name_id <- paste0("originator_", i)
        role_id <- paste0("originator_role_", i)
        location_id <- paste0("originator_location_", i) # matches your current UI id (see note below)
        if (!is.null(input[[name_id]])) {
          game_data$originators[[i]]$name <- input[[name_id]]
        }
        if (!is.null(input[[role_id]])) {
          game_data$originators[[i]]$role <- input[[role_id]]
        }
        if (!is.null(input[[location_id]])) {
          game_data$originators[[i]]$location <- input[[location_id]]
        }
      })
    })
    observe({
      n <- length(isolate(game_data$notes))
      purrr::walk(seq_len(n), function(i) {
        note_id <- paste0("note_", i)
        if (!is.null(input[[note_id]])) {
          game_data$notes[[i]] <- input[[note_id]]
        }
      })
    })

    # ── Preview module ────────────────────────────────────────────────────────
    preview <- mod_game_card_server(
      "game_card_1",
      values = game_data
    )

    # reactive(game_data)
  })
}

## To be copied in the UI:
# mod_data_editor_ui("data_editor_1")

## To be copied in the server:
# mod_data_editor_server("data_editor_1")
