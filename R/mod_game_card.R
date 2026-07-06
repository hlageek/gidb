#' game_card UI Function
#'
#' @description A shiny Module rendering the read-only preview panel for a
#'   game record. Purely presentational — reads from a `game_data`
#'   reactivegame_data object supplied by the caller.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_game_card_ui <- function(id) {
  ns <- NS(id)

  bslib::card(
    bslib::card_header(
      class = "d-flex align-items-center justify-content-between py-2",
      div(
        class = "d-flex align-items-center gap-2",
        bsicons::bs_icon("eye"),
        strong("Preview")
      ),
      div(
        class = "d-flex gap-2",
        actionButton(
          ns("reset"),
          "Reset",
          icon = icon("rotate-left"),
          class = "btn-outline-secondary btn-sm"
        ),
        actionButton(
          ns("save"),
          "Save",
          icon = icon("floppy-disk"),
          class = "btn-success btn-sm",
          disabled = NA
        )
      )
    ),
    bslib::card_body(
      class = "p-3",
      style = "overflow-y: visible;",
      uiOutput(ns("preview_content"), style = "display: block;")
    )
  )
}

# ── Helpers (preview-only) ───────────────────────────────────────────────────

#' Pill row.
#' @noRd
.pill_row <- function(game_data, pill_class = "preview-pill") {
  game_data <- game_data[!is.na(game_data) & nchar(game_data) > 0]
  if (length(game_data) == 0) {
    return(span(class = "preview-empty", "—"))
  }
  div(
    class = "pill-wrap",
    lapply(game_data, function(v) {
      span(class = paste("preview-pill", pill_class), title = v, v)
    })
  )
}

#' Originator mini-table.
#' @noRd
.originator_table <- function(originators) {
  if (length(originators) == 0) {
    return(span(class = "preview-empty", "—"))
  }
  tags$table(
    class = "orig-table",
    !!!lapply(originators, function(o) {
      tags$tr(
        tags$td(
          class = "orig-name",
          title = o$name %||% "",
          o$name %||% ""
        ),
        tags$td(
          class = "orig-role",
          title = o$role %||% "",
          o$role %||% ""
        ),
        tags$td(
          class = "orig-loc",
          title = o$location %||% "",
          o$location %||% ""
        )
      )
    })
  )
}

#' Labelled preview section.
#' @noRd
.preview_section <- function(label, ...) {
  tagList(div(class = "preview-section-label", label), ...)
}

#' game_card Server Function
#'
#' @param id Module id.
#' @param game_data A `reactivegame_data` object (e.g. the parent module's `loc`)
#'   with fields: title, release_year, description, official_url,
#'   identifiers, tags, platforms, originators, notes. Passed by reference,
#'   so changes made by the caller are reflected reactively here.
#'
#' @noRd
mod_game_card_server <- function(id, game_data) {
  moduleServer(id, function(input, output, session) {
    return_values <- reactiveValues(
      reset_flag = 0,
      saved_flag = 0
    )

    output$preview_content <- renderUI({
      # Title — linked if official_url is set
      title_el <- if (
        !is.null(game_data$info$title) && nchar(game_data$info$title) > 0
      ) {
        url <- game_data$info$official_url %||% ""
        if (nchar(url) > 0) {
          tags$a(
            class = "preview-title-link",
            href = url,
            target = "_blank",
            game_data$info$title
          )
        } else {
          game_data$info$title
        }
      } else {
        span(class = "preview-empty", "No title yet")
      }
      tagList(
        div(
          h5(class = "mb-0", title_el),
          span(
            class = "small text-muted",
            bsicons::bs_icon("calendar3"),
            " ",
            game_data$info$release_year %||% "—"
          )
        ),
        # Description
        {
          desc <- game_data$info$description %||% ""
          if (nchar(desc) > 0) {
            tagList(
              hr(class = "my-2"),
              div(
                class = "preview-description",
                onclick = "this.classList.toggle('expanded')",
                desc
              )
            )
          }
        },
        hr(class = "my-2"),
        # Identifiers
        # Identifiers
        {
          ids <- game_data$identifiers
          ids <- ids[
            !vapply(
              ids,
              function(v) is.null(v) || !nzchar(as.character(v)),
              logical(1)
            )
          ]
          if (length(ids) > 0) {
            .preview_section(
              "Identifiers",
              div(
                class = "pill-wrap",
                lapply(names(ids), function(nm) {
                  span(
                    class = "preview-pill pill-id",
                    title = paste0(nm, ": ", ids[[nm]]),
                    paste0(nm, " ", ids[[nm]])
                  )
                })
              )
            )
          }
        },
        # Tags — single "Tags" label, each category inline as plain text
        # before its pills
        {
          tags <- game_data$game_tags
          tags <- tags[purrr::map_lgl(tags, \(t) nchar(t$name %||% "") > 0)]
          if (length(tags) > 0) {
            cats <- unique(purrr::map_chr(tags, \(t) t$category %||% ""))
            cats_with_names <- cats[nchar(cats) > 0]
            cats_no_cat <- tags[purrr::map_lgl(tags, \(t) {
              nchar(t$category %||% "") == 0
            })]
            cat_rows <- purrr::map(cats_with_names, function(cat) {
              names_in_cat <- purrr::keep(tags, \(t) {
                identical(t$category %||% "", cat)
              }) |>
                purrr::map_chr("name")
              div(
                class = "d-flex flex-wrap align-items-center gap-1 mb-1",
                span(
                  class = "small text-muted me-1",
                  style = "white-space: nowrap; font-size: 0.72rem;",
                  paste0(cat, ":")
                ),
                lapply(names_in_cat[nchar(names_in_cat) > 0], function(v) {
                  span(class = "preview-pill pill-tag", title = v, v)
                })
              )
            })
            uncategorised <- if (length(cats_no_cat) > 0) {
              div(
                class = "pill-wrap",
                lapply(purrr::map_chr(cats_no_cat, "name"), function(v) {
                  span(class = "preview-pill pill-tag", title = v, v)
                })
              )
            }
            .preview_section("Tags", !!!cat_rows, uncategorised)
          } else {
            .preview_section("Tags", span(class = "preview-empty", "—"))
          }
        },
        # Platforms
        .preview_section(
          "Platforms",
          .pill_row(
            purrr::map_chr(game_data$platforms %||% list(), function(p) {
              yr <- p$year
              has_year <- !is.null(yr) &&
                !is.na(yr) &&
                nchar(as.character(yr)) > 0
              if (has_year) {
                paste0(p$name %||% "", " (", yr, ")")
              } else {
                p$name %||% ""
              }
            }),
            "pill-platform"
          )
        ),
        # Originators
        .preview_section(
          "Originators",
          .originator_table(game_data$originators)
        ),
        # Notes
        {
          notes <- game_data$notes %||% character(0)
          if (length(notes) > 0) {
            .preview_section("Notes", .pill_row(notes, "pill-note"))
          }
        }
      )
    })

    observeEvent(input$reset, {
      return_values$reset_flag <- return_values$reset_flag + 1
    })

    observeEvent(input$save, {
      return_values$saved_flag <- return_values$saved_flag + 1
    })

    return(
      return_values
    )
  })
}

## To be copied in the UI:
# mod_game_card_ui("game_card_1")

## To be copied in the server:
# mod_game_card_server("game_card_1", loc)
