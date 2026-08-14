#' data_preview UI Function
#'
#' @description A shiny Module displaying a datatable of games from the database.
#' Supports clicking on rows to edit games in the Data Editor tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_preview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex align-items-center justify-content-between",
        div(
          class = "d-flex align-items-center gap-2",
          bsicons::bs_icon("table"),
          strong("Games Database")
        ),
        div(
          class = "d-flex gap-2 align-items-center",
          actionButton(
            ns("refresh"),
            "Refresh",
            icon = icon("sync"),
            class = "btn-outline-secondary btn-sm"
          )
        )
      ),
      bslib::card_body(
        class = "p-0",
        style = "overflow-x: auto; min-height: 450px; max-height: 600px;",
        DT::dataTableOutput(ns("games_table"))
      )
    ),
    # CSS for data badges
    tags$style(
      HTML(
        "
        .data-badge {
          display: inline-block;
          padding: 2px 8px;
          border-radius: 12px;
          font-size: 0.75rem;
          font-weight: 500;
          white-space: nowrap;
          margin: 2px;
        }
        .data-badge-tag {
          background-color: #e3f2fd;
          color: #1976d2;
          border: 1px solid #bbdefb;
        }
        .data-badge-tag {
          background-color: #fff3e0;
          color: #f57c00;
          border: 1px solid #ffe0b2;
        }
        .data-badge-identifier {
          background-color: #f3e5f5;
          color: #7b1fa2;
          border: 1px solid #e1bee7;
        }
        .data-badge-role {
          background-color: #eceff1;
          color: #546e7a;
          border: 1px solid #cfd8dc;
          font-style: italic;
        }
        .data-empty {
          color: #9e9e9e;
          font-style: italic;
          font-size: 0.8rem;
        }
        .edit-cell {
          text-align: center;
        }
        .edit-link {
          color: #1976d2;
          text-decoration: none;
          cursor: pointer;
          font-weight: 500;
        }
        .edit-link:hover {
          text-decoration: underline;
        }
        /* Make row highlight on hover */
        table.dataTable tbody tr:hover {
          background-color: #f5f5f5;
        }
        table.dataTable td {
          vertical-align: middle;
        }
      "
      )
    )
  )
}

#' data_preview Server Functions
#'
#' @param id Module ID.
#' @param pool Database pool connection.
#'
#' @noRd
mod_data_preview_server <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    loc <- reactiveValues(click_count = 0, refresh_trigger = 0)

    # Fetch game data - invalidate when refresh is clicked
    fetch_games <- reactive({
      req(pool)
      loc$refresh_trigger # Dependency for refresh button

      # Get core game data
      games_df <- db_read_games(pool)

      if (nrow(games_df) == 0) {
        return(data.frame(
          Edit = character(0),
          Title = character(0),
          Year = character(0),
          Tags = character(0),
          Originators = character(0),
          Identifiers = character(0),
          stringsAsFactors = FALSE
        ))
      }

      # Build the display table
      result_list <- lapply(seq_len(nrow(games_df)), function(i) {
        game <- games_df[i, ]
        gidb_id <- game$gidb_id

        # Edit link - ActionLink with onclick to set games_table_cell_click with gidb_id
        edit_link <- actionLink(
          inputId = ns(paste0("edit_", gidb_id)),
          label = "✏️ Edit",
          class = "edit-link",
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("games_table_cell_click"),
            gidb_id
          )
        )
        edit_html <- tags$div(edit_link, class = "edit-cell")

        # Title with year
        title_val <- game$title %||% ""
        title_html <- if (is.na(title_val) || nchar(trimws(title_val)) > 0) {
          title_val
        } else {
          "<em>No title</em>"
        }

        # Release year
        rel_year <- game$release_year
        release_year <- if (!is.null(rel_year) && !is.na(rel_year)) {
          format_single_html(as.character(rel_year), "identifier")
        } else {
          "<span class=\"data-empty\">—</span>"
        }

        # Tags - show tag value with category
        tags_df <- db_read_game_tags(pool, gidb_id)
        tags_html <- if (nrow(tags_df) > 0) {
          tag_labels <- ifelse(
            nchar(tags_df$category_value %||% "") > 0,
            paste0(tags_df$tag_value, " (", tags_df$category_value, ")"),
            tags_df$tag_value
          )
          format_cell_html(tag_labels, "tag")
        } else {
          "<span class=\"data-empty\">—</span>"
        }

        # Originators (name + role)
        originators_df <- db_read_game_originators(pool, gidb_id)
        originators_html <- if (nrow(originators_df) > 0) {
          originator_labels <- paste0(
            originators_df$originator_name,
            ifelse(
              nchar(originators_df$originator_role %||% "") > 0,
              paste0(" (", originators_df$originator_role, ")"),
              ""
            )
          )
          format_cell_html(originator_labels, "originator")
        } else {
          "<span class=\"data-empty\">—</span>"
        }

        # Identifiers
        identifiers <- db_read_game_identifiers(pool, gidb_id)
        ident_parts <- c()
        add_ident <- function(val, prefix) {
          if (
            !is.null(val) && !is.na(val) && nzchar(trimws(as.character(val)))
          ) {
            paste0(prefix, ": ", val)
          }
        }
        ident_parts <- c(
          ident_parts,
          add_ident(identifiers$mobygames_id, "Moby"),
          add_ident(identifiers$igdb_id, "IGDB"),
          add_ident(identifiers$steam_id, "Steam"),
          add_ident(identifiers$gog_id, "GOG")
        )
        ident_parts <- ident_parts[!vapply(ident_parts, is.null, logical(1))]
        identifiers_html <- if (length(ident_parts) > 0) {
          format_cell_html(ident_parts, "identifier")
        } else {
          "<span class=\"data-empty\">—</span>"
        }

        list(
          Edit = edit_html,
          Title = title_html,
          `Release Year` = release_year,
          Tags = tags_html,
          Originators = originators_html,
          Identifiers = identifiers_html,
          .gidb_id = gidb_id # Store for click handler
        )
      })

      result_df <- do.call(
        rbind,
        lapply(result_list, function(row) {
          df <- data.frame(
            Edit = as.character(row$Edit),
            Title = shiny::HTML(row$Title),
            `Release Year` = shiny::HTML(row$`Release Year`),
            Tags = shiny::HTML(row$Tags),
            Originators = shiny::HTML(row$Originators),
            Identifiers = shiny::HTML(row$Identifiers),
            stringsAsFactors = FALSE
          )
          rownames(df) <- NULL # Explicitly remove row names
          df
        })
      )

      result_df
    })

    # Render the datatable
    output$games_table <- DT::renderDT(
      expr = fetch_games(),
      rownames = FALSE, # Don't show row numbers on left
      selection = "none", # Disable row selection
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        order = list(list(1, 'desc')), # Sort by Title descending
        paging = TRUE,
        searching = TRUE,
        info = TRUE,
        autoWidth = FALSE,
        colnames = TRUE, # Show column names
        deferRender = TRUE,
        scrollY = "400px", # Enable vertical scrolling with fixed header
        lengthChange = TRUE,
        columnDefs = list(
          list(className = 'dt-left', targets = '_all'),
          list(width = '60px', targets = 0), # Edit column
          list(width = '150px', targets = 1), # Title
          list(width = '100px', targets = 2), # Release Year
          list(orderable = FALSE, targets = 0) # Edit column not sortable
        )
      ),
      server = TRUE,
      escape = FALSE # Allow HTML in cells
    )

    # Handle clicking on the edit link - games_table_cell_click now contains gidb_id directly
    observeEvent(input$games_table_cell_click, {
      req(input$games_table_cell_click)
      gidb_id <- input$games_table_cell_click
      req(gidb_id)

      # Update click count to trigger the edit flow
      loc$click_count <- loc$click_count + 1
    })

    # Expose the edit_game_id as an observable value
    edit_game_id <- reactive({
      req(input$games_table_cell_click)
      input$games_table_cell_click
    })

    # Refresh button
    observeEvent(input$refresh, {
      loc$refresh_trigger <- loc$refresh_trigger + 1
    })

    # Expose a refresh function for external triggers (e.g., after save/delete)
    refresh_table <- function() {
      loc$refresh_trigger <- loc$refresh_trigger + 1
    }

    # Return reactive values and functions
    returnValues <- reactiveValues(
      click_count = reactive({
        loc$click_count
      }),
      edit_game_id = edit_game_id,
      refresh_table = refresh_table
    )

    returnValues
  })
}

## To be copied in the UI:
# mod_data_preview_ui("data_preview_1")

## To be copied in the server:
# mod_data_preview_server("data_preview_1", pool)
