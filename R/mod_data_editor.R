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
    # ── Blur-only JS binding (text inputs AND textareas) ─────────────────────
    # Both <input class="blur-input"> and <textarea class="blur-textarea"> only
    # report their value to the server on blur (focus lost), not on each keystroke.
    shiny::tags$script(shiny::HTML(
      "
      $(document).on('shiny:connected', function() {

        // ── blur binding for <input> ──────────────────────────────────────────
        var blurInputBinding = new Shiny.InputBinding();
        $.extend(blurInputBinding, {
          find:      function(scope) { return $(scope).find('input.blur-input'); },
          getValue:  function(el)    { return el.value; },
          setValue:  function(el, v) { el.value = v; },
          subscribe: function(el, cb) {
            $(el).on('blur.blurInput', function() { cb(true); });
          },
          unsubscribe:    function(el)    { $(el).off('.blurInput'); },
          receiveMessage: function(el, d) { if ('value' in d) el.value = d.value; },
          getState:       function(el)    { return { value: el.value }; },
          getRatePolicy:  function()      { return { policy: 'direct' }; }
        });
        Shiny.inputBindings.register(blurInputBinding, 'shiny.blurInput');

        // ── blur binding for <textarea> ───────────────────────────────────────
        var blurTextareaBinding = new Shiny.InputBinding();
        $.extend(blurTextareaBinding, {
          find:      function(scope) { return $(scope).find('textarea.blur-textarea'); },
          getValue:  function(el)    { return el.value; },
          setValue:  function(el, v) { el.value = v; },
          subscribe: function(el, cb) {
            $(el).on('blur.blurTextarea', function() { cb(true); });
          },
          unsubscribe:    function(el)    { $(el).off('.blurTextarea'); },
          receiveMessage: function(el, d) { if ('value' in d) el.value = d.value; },
          getState:       function(el)    { return { value: el.value }; },
          getRatePolicy:  function()      { return { policy: 'direct' }; }
        });
        Shiny.inputBindings.register(blurTextareaBinding, 'shiny.blurTextarea');
      });
    "
    )),

    # ── Inline CSS ───────────────────────────────────────────────────────────
    shiny::tags$style(shiny::HTML(
      "
      /* ── Layout ── */
      .editor-layout {
        display: grid;
        grid-template-columns: 360px 1fr;
        gap: 1rem;
        align-items: start;
      }
      .preview-panel {
        position: sticky;
        top: 1rem;
      }
      .editor-panel {
        max-height: calc(100vh - 4rem);
        overflow-y: auto;
        padding-right: 0.25rem;
      }

      /* ── Accordion headers ── */
      .accordion-button {
        background-color: #f8f9fa !important;
        font-size: 0.875rem;
        font-weight: 500;
      }
      .accordion-button:not(.collapsed) {
        background-color: #e9ecef !important;
        color: inherit;
        box-shadow: none;
      }

      /* ── Preview typography ── */
      .preview-section-label {
        font-size: 0.68rem;
        text-transform: uppercase;
        letter-spacing: 0.07em;
        color: #6c757d;
        margin-bottom: 0.3rem;
        margin-top: 0.8rem;
      }
      .preview-title-link {
        color: inherit;
        text-decoration: none;
      }
      .preview-title-link:hover {
        text-decoration: underline;
        color: #0d6efd;
      }
      .preview-description {
        font-size: 0.8rem;
        color: #495057;
        display: -webkit-box;
        -webkit-line-clamp: 3;
        -webkit-box-orient: vertical;
        overflow: hidden;
        cursor: pointer;
        line-height: 1.4;
        transition: all 0.2s ease;
      }
      .preview-description.expanded {
        -webkit-line-clamp: unset;
        overflow: visible;
      }

      /* ── Pills ── */
      .pill-wrap {
        display: flex;
        flex-wrap: wrap;
        gap: 0.3rem;
        min-height: 1.6rem;
      }
      .preview-pill {
        display: inline-block;
        max-width: 160px;
        padding: 0.2rem 0.55rem;
        border-radius: 999px;
        font-size: 0.75rem;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        background: #e9ecef;
        color: #343a40;
        cursor: default;
        transition: max-width 0.25s ease, background 0.15s ease;
        position: relative;
      }
      .preview-pill:hover {
        max-width: 400px;
        z-index: 10;
      }
      .preview-pill.pill-tag      { background: #cfe2ff; color: #084298; }
      .preview-pill.pill-tag:hover { background: #9ec5fe; }
      .preview-pill.pill-platform  { background: #d1e7dd; color: #0a3622; }
      .preview-pill.pill-platform:hover { background: #a3cfbb; }
      .preview-pill.pill-note      { background: #fff3cd; color: #664d03; }
      .preview-pill.pill-note:hover { background: #ffe69c; }
      .preview-pill.pill-id        { background: #f8d7da; color: #58151c; font-family: monospace; }
      .preview-pill.pill-id:hover  { background: #f1aeb5; }

      /* ── Originator mini-table ── */
      .orig-table {
        width: 100%;
        font-size: 0.78rem;
        border-collapse: collapse;
      }
      .orig-table td {
        padding: 0.18rem 0.4rem 0.18rem 0;
        vertical-align: top;
        max-width: 110px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        transition: max-width 0.2s ease;
      }
      .orig-table td:hover {
        white-space: normal;
        max-width: 300px;
      }
      .orig-table .orig-name { font-weight: 500; color: #212529; }
      .orig-table .orig-role { color: #6c757d; font-style: italic; }
      .orig-table .orig-loc  { color: #6c757d; font-size: 0.72rem; }
      .orig-table tr + tr td { border-top: 1px solid #f0f0f0; padding-top: 0.3rem; }

      .preview-empty {
        color: #adb5bd;
        font-size: 0.8rem;
        font-style: italic;
      }
    "
    )),

    shiny::div(
      class = "editor-layout",

      # ── LEFT: Preview panel ────────────────────────────────────────────────
      shiny::div(
        class = "preview-panel",
        bslib::card(
          bslib::card_header(
            class = "d-flex align-items-center justify-content-between py-2",
            shiny::div(
              class = "d-flex align-items-center gap-2",
              bsicons::bs_icon("eye"),
              shiny::strong("Preview")
            ),
            shiny::div(
              class = "d-flex gap-2",
              shiny::actionButton(
                ns("reset"),
                "Reset",
                icon = shiny::icon("rotate-left"),
                class = "btn-outline-secondary btn-sm"
              ),
              shiny::actionButton(
                ns("confirm"),
                "Save",
                icon = shiny::icon("floppy-disk"),
                class = "btn-success btn-sm",
                disabled = NA
              )
            )
          ),
          bslib::card_body(
            class = "p-3",
            style = "max-height: calc(100vh - 8rem); overflow-y: auto;",
            shiny::uiOutput(ns("preview_content"))
          )
        )
      ),

      # ── RIGHT: Editor panel (flat accordion, no wrapper card) ─────────────
      shiny::div(
        class = "editor-panel",
        shiny::uiOutput(ns("editor"))
      )
    )
  )
}


# ── Helpers ──────────────────────────────────────────────────────────────────

#' textInput that fires only on blur.
#' @noRd
.text_input_blur <- function(
  inputId,
  label,
  value = "",
  placeholder = NULL,
  width = "100%"
) {
  tag <- shiny::textInput(
    inputId,
    label,
    value = value,
    placeholder = placeholder,
    width = width
  )
  tag$children[[2]] <- shiny::tagAppendAttributes(
    tag$children[[2]],
    class = "blur-input"
  )
  tag
}

#' textAreaInput that fires only on blur.
#' @noRd
.text_area_blur <- function(
  inputId,
  label,
  value = "",
  placeholder = NULL,
  width = "100%",
  rows = 3
) {
  tag <- shiny::textAreaInput(
    inputId,
    label,
    value = value,
    placeholder = placeholder,
    width = width,
    rows = rows
  )
  # The <textarea> is the second child of the wrapper div
  tag$children[[2]] <- shiny::tagAppendAttributes(
    tag$children[[2]],
    class = "blur-textarea"
  )
  tag
}

#' Strip HTML tags from a string.
#' @noRd
.strip_html <- function(x) {
  if (is.null(x) || nchar(x) == 0) {
    return("")
  }
  gsub("\\s+", " ", trimws(gsub("<[^>]+>", " ", x)))
}

#' Pill row.
#' @noRd
.pill_row <- function(values, pill_class = "preview-pill") {
  values <- values[!is.na(values) & nchar(values) > 0]
  if (length(values) == 0) {
    return(shiny::span(class = "preview-empty", "—"))
  }
  shiny::div(
    class = "pill-wrap",
    lapply(values, function(v) {
      shiny::span(class = paste("preview-pill", pill_class), title = v, v)
    })
  )
}

#' Originator mini-table.
#' @noRd
.originator_table <- function(originators) {
  if (length(originators) == 0) {
    return(shiny::span(class = "preview-empty", "—"))
  }
  shiny::tags$table(
    class = "orig-table",
    !!!lapply(originators, function(o) {
      shiny::tags$tr(
        shiny::tags$td(
          class = "orig-name",
          title = o$name %||% "",
          o$name %||% ""
        ),
        shiny::tags$td(
          class = "orig-role",
          title = o$role %||% "",
          o$role %||% ""
        ),
        shiny::tags$td(
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
  shiny::tagList(shiny::div(class = "preview-section-label", label), ...)
}

#' Dynamic rows UI helper.
#' @noRd
.dynamic_rows_ui <- function(ns, n, add_btn_id, add_label, row_fn) {
  shiny::tagList(
    !!!purrr::map(seq_len(n), row_fn),
    shiny::actionButton(
      ns(add_btn_id),
      add_label,
      icon = shiny::icon("plus"),
      class = "btn-outline-secondary btn-sm mt-2"
    )
  )
}


#' data_editor Server Function
#'
#' @noRd
mod_data_editor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive state ────────────────────────────────────────────────────────

    game_data <- shiny::reactiveVal(NULL)

    # loc: single source of truth for preview. All caches write into it.
    loc <- shiny::reactiveValues(
      title = NULL,
      release_year = NULL,
      description = NULL,
      official_url = NULL,
      identifiers = list(), # named list label → value
      # tags: list of list(name, category) — one entry per tag row
      tags = list(),
      platforms = list(), # list of list(name, year) — structured, not strings
      originators = list(), # list of list(name, role, location)
      notes = NULL # character vector
    )

    geonames_results <- shiny::reactiveValues() # key: "geo_{i}"
    geo_keys <- character(0) # track all keys for reliable reset

    n_originators <- shiny::reactiveVal(1L)
    n_platforms <- shiny::reactiveVal(1L)
    n_notes <- shiny::reactiveVal(1L)

    # ── Per-row caches (survive renderUI re-renders) ──────────────────────────
    plat_cache <- shiny::reactiveValues() # plat_name_i, plat_year_i
    orig_cache <- shiny::reactiveValues() # orig_name_i, orig_role_i, orig_moby_id_i
    note_cache <- shiny::reactiveValues() # note_i

    # Tag categories available in the selectize.
    # Starts with defaults; can be seeded from DB at init; grows as user types.
    .default_tag_cats <- c(
      "Basic Genres",
      "Perspective",
      "Gameplay",
      "Interface",
      "Setting",
      "Pacing",
      "Visual",
      "Narrative / Theme / Topic"
    )
    tag_cats <- shiny::reactiveVal(.default_tag_cats)

    # ── Rebuild helpers ───────────────────────────────────────────────────────
    # Each reads from the cache (not from input[[]] directly when n changes),
    # so values always survive re-renders.

    .rebuild_platforms <- function() {
      n <- n_platforms()
      loc$platforms <- purrr::map(seq_len(n), function(i) {
        list(
          name = plat_cache[[paste0("plat_name_", i)]] %||% "",
          year = plat_cache[[paste0("plat_year_", i)]] %||% ""
        )
      }) |>
        purrr::keep(\(x) nchar(x$name) > 0)
    }

    .rebuild_originators <- function() {
      n <- n_originators()
      loc$originators <- purrr::map(seq_len(n), function(i) {
        nm <- orig_cache[[paste0("orig_name_", i)]] %||% ""
        if (nchar(nm) == 0) {
          return(NULL)
        }
        geo <- geonames_results[[paste0("geo_", i)]]
        list(
          name = nm,
          role = orig_cache[[paste0("orig_role_", i)]] %||% "",
          location = if (!is.null(geo)) geo$display else ""
        )
      }) |>
        purrr::compact()
    }

    .rebuild_notes <- function() {
      n <- n_notes()
      vec <- purrr::map_chr(seq_len(n), \(i) {
        note_cache[[paste0("note_", i)]] %||% ""
      })
      loc$notes <- vec[nchar(vec) > 0]
    }

    .rebuild_identifiers <- function() {
      ids <- list()
      fields <- c(
        id_mobygames = "MobyGames",
        id_igdb = "IGDB",
        id_steam = "Steam",
        id_gog = "GOG",
        id_itch = "itch.io"
      )
      for (field in names(fields)) {
        val <- input[[field]] %||% ""
        if (nchar(val) > 0) ids[[fields[[field]]]] <- val
      }
      loc$identifiers <- ids
    }

    .clear_caches <- function() {
      for (k in ls(plat_cache)) {
        plat_cache[[k]] <- NULL
      }
      for (k in ls(orig_cache)) {
        orig_cache[[k]] <- NULL
      }
      for (k in ls(note_cache)) {
        note_cache[[k]] <- NULL
      }
      for (k in geo_keys) {
        geonames_results[[k]] <- NULL
      }
      geo_keys <<- character(0)
    }

    # ── MobyGames fetch ───────────────────────────────────────────────────────

    shiny::observeEvent(input$retrieve_mobygames, {
      shiny::req(input$mobygames_id)
      game_data(NULL)

      id_int <- suppressWarnings(as.integer(trimws(input$mobygames_id)))
      if (is.na(id_int)) {
        output$fetch_status <- shiny::renderUI(shiny::div(
          class = "alert alert-danger py-2 d-flex gap-2",
          bsicons::bs_icon("exclamation-triangle"),
          paste("Invalid ID:", input$mobygames_id)
        ))
        return()
      }

      output$fetch_status <- shiny::renderUI(shiny::div(
        class = "text-muted d-flex gap-2",
        shiny::icon("spinner", class = "fa-spin"),
        "Fetching..."
      ))

      result <- tryCatch(moby_get_game_metadata(id_int), error = function(e) e)

      if (inherits(result, "error")) {
        output$fetch_status <- shiny::renderUI(shiny::div(
          class = "alert alert-danger py-2 d-flex gap-2",
          bsicons::bs_icon("x-circle"),
          conditionMessage(result)
        ))
        return()
      }

      game_data(result)
      .clear_caches()

      # ── Row counts ──
      n_platforms(max(1L, length(result$platforms)))
      n_originators(max(1L, length(result$originators)))
      n_notes(1L)

      # Flatten MobyGames tag buckets → loc$tags list(list(name, category), ...)
      all_tags <- purrr::imap(result$tags, function(entries, bucket) {
        cat_label <- tools::toTitleCase(gsub("_", " ", bucket))
        purrr::map(entries, \(e) list(name = e$name, category = cat_label))
      }) |>
        purrr::flatten()

      # Extend available categories with anything new from this result
      new_cats <- unique(purrr::map_chr(all_tags, \(t) t$category %||% ""))
      tag_cats(union(.default_tag_cats, new_cats))

      # ── Seed platform and originator caches ──
      purrr::iwalk(result$platforms, function(p, i) {
        plat_cache[[paste0("plat_name_", i)]] <- p$platform_name
        plat_cache[[paste0("plat_year_", i)]] <- p$release_year %||% ""
      })
      purrr::iwalk(result$originators, function(o, i) {
        orig_cache[[paste0("orig_name_", i)]] <- o$company_name
        orig_cache[[paste0("orig_role_", i)]] <- o$role
        orig_cache[[paste0("orig_moby_id_", i)]] <- as.character(
          o$company_id %||% ""
        )
      })

      # ── Seed loc ──
      loc$title <- result$moby_title
      loc$description <- .strip_html(result$description)
      loc$official_url <- result$official_url %||% ""
      loc$identifiers <- list(MobyGames = as.character(result$moby_id))
      loc$notes <- NULL

      years <- purrr::map_chr(result$platforms, \(p) p$release_year %||% "")
      loc$release_year <- if (any(nchar(years) > 0)) {
        min(years[nchar(years) > 0])
      } else {
        ""
      }

      loc$platforms <- purrr::map(result$platforms, \(p) {
        list(name = p$platform_name, year = p$release_year %||% "")
      })
      loc$originators <- purrr::map(result$originators, \(o) {
        list(name = o$company_name, role = o$role, location = "")
      })
      loc$tags <- all_tags

      output$fetch_status <- shiny::renderUI(shiny::div(
        class = "alert alert-success py-2 d-flex gap-2",
        bsicons::bs_icon("check-circle"),
        paste("Loaded:", result$moby_title)
      ))
      shinyjs::enable("confirm")
    })

    # ── Reset ─────────────────────────────────────────────────────────────────

    shiny::observeEvent(input$reset, {
      game_data(NULL)
      .clear_caches()
      n_originators(1L)
      n_platforms(1L)
      n_notes(1L)
      tag_cats(.default_tag_cats)
      loc$title <- loc$release_year <- loc$description <- loc$official_url <- loc$notes <- NULL
      loc$originators <- list()
      loc$tags <- list()
      loc$platforms <- list()
      loc$identifiers <- list()
      output$fetch_status <- shiny::renderUI(NULL)
      shinyjs::disable("confirm")
    })

    # ── Preview ───────────────────────────────────────────────────────────────

    output$preview_content <- shiny::renderUI({
      # Title — linked if official_url is set
      title_el <- if (!is.null(loc$title) && nchar(loc$title) > 0) {
        url <- loc$official_url %||% ""
        if (nchar(url) > 0) {
          shiny::tags$a(
            class = "preview-title-link",
            href = url,
            target = "_blank",
            loc$title
          )
        } else {
          loc$title
        }
      } else {
        shiny::span(class = "preview-empty", "No title yet")
      }

      shiny::tagList(
        shiny::div(
          shiny::h5(class = "mb-0", title_el),
          shiny::span(
            class = "small text-muted",
            bsicons::bs_icon("calendar3"),
            " ",
            loc$release_year %||% "—"
          )
        ),

        # Description
        {
          desc <- loc$description %||% ""
          if (nchar(desc) > 0) {
            shiny::tagList(
              shiny::hr(class = "my-2"),
              shiny::div(
                class = "preview-description",
                onclick = "this.classList.toggle('expanded')",
                desc
              )
            )
          }
        },

        shiny::hr(class = "my-2"),

        # Identifiers
        {
          ids <- loc$identifiers
          if (length(ids) > 0) {
            .preview_section(
              "Identifiers",
              shiny::div(
                class = "pill-wrap",
                lapply(names(ids), function(nm) {
                  shiny::span(
                    class = "preview-pill pill-id",
                    title = paste0(nm, ": ", ids[[nm]]),
                    paste0(nm, " ", ids[[nm]])
                  )
                })
              )
            )
          }
        },

        # Tags — single "Tags" label, each category inline as plain text before its pills
        {
          tags <- loc$tags
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
              shiny::div(
                class = "d-flex flex-wrap align-items-center gap-1 mb-1",
                shiny::span(
                  class = "small text-muted me-1",
                  style = "white-space: nowrap; font-size: 0.72rem;",
                  paste0(cat, ":")
                ),
                lapply(names_in_cat[nchar(names_in_cat) > 0], function(v) {
                  shiny::span(class = "preview-pill pill-tag", title = v, v)
                })
              )
            })

            uncategorised <- if (length(cats_no_cat) > 0) {
              shiny::div(
                class = "pill-wrap",
                lapply(purrr::map_chr(cats_no_cat, "name"), function(v) {
                  shiny::span(class = "preview-pill pill-tag", title = v, v)
                })
              )
            }

            .preview_section("Tags", !!!cat_rows, uncategorised)
          } else {
            .preview_section("Tags", shiny::span(class = "preview-empty", "—"))
          }
        },

        # Platforms
        .preview_section(
          "Platforms",
          .pill_row(
            purrr::map_chr(loc$platforms %||% list(), function(p) {
              if (nchar(p$year) > 0) {
                paste0(p$name, " (", p$year, ")")
              } else {
                p$name
              }
            }),
            "pill-platform"
          )
        ),

        # Originators
        .preview_section("Originators", .originator_table(loc$originators)),

        # Notes
        {
          notes <- loc$notes %||% character(0)
          if (length(notes) > 0) {
            .preview_section("Notes", .pill_row(notes, "pill-note"))
          }
        }
      )
    })

    # ── Editor (server-rendered accordion, flat) ──────────────────────────────

    output$editor <- shiny::renderUI({
      d <- game_data()
      is_auto <- !is.null(d)
      val <- function(x) if (is_auto && !is.null(x)) as.character(x) else ""

      release_year_val <- {
        years <- if (is_auto) {
          purrr::map_chr(d$platforms, \(p) p$release_year %||% "")
        } else {
          character(0)
        }
        if (any(nchar(years) > 0)) min(years[nchar(years) > 0]) else ""
      }

      # ── Entry mode + MobyGames fetch ───────────────────────────────────────
      panel_entry <- bslib::accordion_panel(
        title = shiny::tagList(
          bsicons::bs_icon("pencil-square"),
          " Entry Mode"
        ),
        value = "entry",
        shiny::radioButtons(
          ns("entry_mode"),
          label = "Entry mode",
          choices = c("Automated (MobyGames)" = "auto", "Manual" = "manual"),
          selected = "auto",
          inline = TRUE
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'auto'", ns("entry_mode")),
          bslib::layout_columns(
            col_widths = c(8, 4),
            shiny::textInput(
              ns("mobygames_id"),
              "MobyGames ID",
              placeholder = "e.g. 366",
              width = "100%"
            ),
            shiny::div(
              style = "padding-top: 1.85rem;",
              shiny::actionButton(
                ns("retrieve_mobygames"),
                "Fetch",
                icon = shiny::icon("cloud-download-alt"),
                class = "btn-primary w-100"
              )
            )
          ),
          shiny::uiOutput(ns("fetch_status"))
        )
      )

      # ── Core Info ──────────────────────────────────────────────────────────
      panel_core <- bslib::accordion_panel(
        title = shiny::tagList(bsicons::bs_icon("info-circle"), " Core Info"),
        value = "core",
        bslib::layout_columns(
          col_widths = c(8, 4),
          .text_input_blur(
            ns("title"),
            "Title",
            value = loc$title %||% val(d$moby_title)
          ),
          .text_input_blur(
            ns("release_year"),
            "Release year",
            value = loc$release_year %||% release_year_val
          )
        ),
        .text_input_blur(
          ns("official_url"),
          "Official URL",
          value = loc$official_url %||% val(d$official_url)
        ),
        .text_area_blur(
          ns("description"),
          "Description",
          value = loc$description %||%
            .strip_html(if (is_auto) d$description else NULL),
          rows = 4
        )
      )

      # ── Identifiers ────────────────────────────────────────────────────────
      panel_identifiers <- bslib::accordion_panel(
        title = shiny::tagList(bsicons::bs_icon("upc"), " Identifiers"),
        value = "identifiers",
        shiny::p(
          class = "text-muted small mb-2",
          "External IDs. MobyGames ID pre-filled from fetch."
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          .text_input_blur(
            ns("id_mobygames"),
            "MobyGames ID",
            value = val(d$moby_id)
          ),
          .text_input_blur(ns("id_igdb"), "IGDB ID", value = "")
        ),
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          .text_input_blur(
            ns("id_steam"),
            "Steam App ID",
            value = val(d$other_ids$steam_id)
          ),
          .text_input_blur(
            ns("id_gog"),
            "GOG ID",
            value = val(d$other_ids$gog_id)
          ),
          .text_input_blur(ns("id_itch"), "itch.io slug", value = "")
        )
      )

      # ── Tags ───────────────────────────────────────────────────────────────
      panel_tags <- bslib::accordion_panel(
        title = shiny::tagList(bsicons::bs_icon("tags"), " Tags"),
        value = "tags",
        shiny::uiOutput(ns("tag_rows"))
      )

      # ── Platforms ──────────────────────────────────────────────────────────
      panel_platforms <- bslib::accordion_panel(
        title = shiny::tagList(bsicons::bs_icon("display"), " Platforms"),
        value = "platforms",
        shiny::uiOutput(ns("platform_rows"))
      )

      # ── Originators ────────────────────────────────────────────────────────
      panel_originators <- bslib::accordion_panel(
        title = shiny::tagList(bsicons::bs_icon("building"), " Originators"),
        value = "originators",
        shiny::uiOutput(ns("originator_rows"))
      )

      # ── Notes ──────────────────────────────────────────────────────────────
      panel_notes <- bslib::accordion_panel(
        title = shiny::tagList(bsicons::bs_icon("journal-text"), " Notes"),
        value = "notes",
        shiny::uiOutput(ns("note_rows"))
      )

      bslib::accordion(
        id = ns("game_accordion"),
        open = "entry",
        multiple = TRUE,
        panel_entry,
        panel_core,
        panel_identifiers,
        panel_tags,
        panel_platforms,
        panel_originators,
        panel_notes
      )
    })

    # ── Tag rows ──────────────────────────────────────────────────────────────
    # loc$tags is the source of truth: list of list(name, category).
    # The renderUI reads from it to prefill; observers write back to it.
    # Selectize fires on render but the identity guard below makes it a no-op.

    output$tag_rows <- shiny::renderUI({
      tags <- if (length(loc$tags) > 0) {
        loc$tags
      } else {
        list(list(name = "", category = ""))
      }
      cats <- tag_cats()
      .dynamic_rows_ui(ns, length(tags), "add_tag", "Add tag", function(i) {
        bslib::layout_columns(
          col_widths = c(5, 6, 1),
          .text_input_blur(
            ns(paste0("tag_name_", i)),
            label = if (i == 1) "Tag name" else NULL,
            value = tags[[i]]$name %||% ""
          ),
          shiny::selectizeInput(
            ns(paste0("tag_cat_", i)),
            label = if (i == 1) "Category" else NULL,
            choices = cats,
            selected = tags[[i]]$category %||% NULL,
            multiple = FALSE,
            width = "100%",
            options = list(create = TRUE, placeholder = "Select or type…")
          ),
          shiny::div(
            style = if (i == 1) "padding-top: 1.85rem;" else "",
            shiny::actionButton(
              ns(paste0("rm_tag_", i)),
              NULL,
              icon = shiny::icon("minus"),
              class = "btn-outline-danger btn-sm"
            )
          )
        )
      })
    })

    # Observers: each writes one field of one row back into loc$tags.
    # tag_name uses blur so it only fires on focus-out.
    # tag_cat (selectize) fires on render too, but the identity check makes
    # that a no-op — it only writes when the value genuinely changed.
    # The outer observe re-runs only when the number of rows changes so that
    # observers for new rows get registered. loc$tags length is read via isolate
    # inside observeEvent bodies to avoid re-registering on every tag value change.
    shiny::observe({
      n <- max(1L, length(shiny::isolate(loc$tags)))
      # Re-run this observe when a row is added or removed
      input$add_tag
      purrr::walk(seq_len(n), function(i) {
        shiny::observeEvent(
          input[[paste0("tag_name_", i)]],
          {
            if (i > length(loc$tags)) {
              return()
            } # guard: list may have shrunk
            val <- input[[paste0("tag_name_", i)]] %||% ""
            if (!identical(val, loc$tags[[i]]$name %||% "")) {
              tags <- loc$tags
              tags[[i]]$name <- val
              loc$tags <- tags
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )

        shiny::observeEvent(
          input[[paste0("tag_cat_", i)]],
          {
            if (i > length(loc$tags)) {
              return()
            } # guard: list may have shrunk
            val <- input[[paste0("tag_cat_", i)]] %||% ""
            if (!identical(val, loc$tags[[i]]$category %||% "")) {
              tags <- loc$tags
              tags[[i]]$category <- val
              loc$tags <- tags
              if (nchar(val) > 0 && !val %in% tag_cats()) {
                tag_cats(c(tag_cats(), val))
              }
            }
          },
          ignoreInit = FALSE,
          ignoreNULL = TRUE
        )
      })
    })

    shiny::observeEvent(input$add_tag, {
      loc$tags <- c(loc$tags, list(list(name = "", category = "")))
    })

    shiny::observe({
      n <- max(1L, length(shiny::isolate(loc$tags)))
      input$add_tag # re-register when rows change
      purrr::walk(seq_len(n), function(i) {
        shiny::observeEvent(
          input[[paste0("rm_tag_", i)]],
          {
            tags <- loc$tags
            loc$tags <- if (length(tags) > 1L) {
              tags[-i]
            } else {
              list(list(name = "", category = ""))
            }
          },
          ignoreInit = TRUE,
          once = FALSE
        )
      })
    })

    # ── Platform rows ─────────────────────────────────────────────────────────

    output$platform_rows <- shiny::renderUI({
      n <- n_platforms()
      .dynamic_rows_ui(ns, n, "add_platform", "Add platform", function(i) {
        bslib::layout_columns(
          col_widths = c(7, 4, 1),
          .text_input_blur(
            ns(paste0("plat_name_", i)),
            if (i == 1) "Platform" else NULL,
            value = plat_cache[[paste0("plat_name_", i)]] %||% ""
          ),
          .text_input_blur(
            ns(paste0("plat_year_", i)),
            if (i == 1) "Release year" else NULL,
            value = plat_cache[[paste0("plat_year_", i)]] %||% ""
          ),
          shiny::div(
            style = if (i == 1) "padding-top: 1.85rem;" else "",
            shiny::actionButton(
              ns(paste0("rm_plat_", i)),
              NULL,
              icon = shiny::icon("minus"),
              class = "btn-outline-danger btn-sm"
            )
          )
        )
      })
    })

    # ── Originator rows ───────────────────────────────────────────────────────

    output$originator_rows <- shiny::renderUI({
      n <- n_originators()
      rows <- purrr::map(seq_len(n), function(i) {
        shiny::tagList(
          if (i > 1) shiny::hr(class = "my-2"),
          bslib::layout_columns(
            col_widths = c(5, 3, 3, 1),
            .text_input_blur(
              ns(paste0("orig_name_", i)),
              if (i == 1) "Company name" else NULL,
              value = orig_cache[[paste0("orig_name_", i)]] %||% ""
            ),
            shiny::selectizeInput(
              ns(paste0("orig_role_", i)),
              label = if (i == 1) "Role" else NULL,
              choices = c("developer", "publisher", "porter", "distributor"),
              selected = orig_cache[[paste0("orig_role_", i)]] %||% "developer",
              width = "100%"
            ),
            .text_input_blur(
              ns(paste0("orig_moby_id_", i)),
              if (i == 1) "MobyGames co. ID" else NULL,
              value = orig_cache[[paste0("orig_moby_id_", i)]] %||% ""
            ),
            shiny::div(
              style = if (i == 1) "padding-top: 1.85rem;" else "",
              shiny::actionButton(
                ns(paste0("rm_orig_", i)),
                NULL,
                icon = shiny::icon("minus"),
                class = "btn-outline-danger btn-sm"
              )
            )
          ),
          bslib::layout_columns(
            col_widths = c(4, 3, 5),
            .text_input_blur(
              ns(paste0("geonames_id_", i)),
              label = shiny::tagList(
                "Location (",
                shiny::a(
                  "Geonames ID",
                  href = "https://www.geonames.org/",
                  target = "_blank"
                ),
                ")"
              )
            ),
            shiny::div(
              style = "padding-top: 1.85rem;",
              shiny::actionButton(
                ns(paste0("retrieve_geonames_", i)),
                "Look up",
                icon = shiny::icon("location-dot"),
                class = "btn-outline-secondary btn-sm w-100"
              )
            ),
            shiny::div(
              style = "padding-top: 1.9rem;",
              shiny::textOutput(ns(paste0("geonames_result_", i)))
            )
          )
        )
      })
      shiny::tagList(
        !!!rows,
        shiny::actionButton(
          ns("add_originator"),
          "Add originator",
          icon = shiny::icon("plus"),
          class = "btn-outline-secondary btn-sm mt-2"
        )
      )
    })

    # ── Note rows ────────────────────────────────────────────────────────────
    # Uses blur-textarea so editing is not disrupted by reactive re-renders.

    output$note_rows <- shiny::renderUI({
      n <- n_notes()
      .dynamic_rows_ui(ns, n, "add_note", "Add note", function(i) {
        bslib::layout_columns(
          col_widths = c(11, 1),
          .text_area_blur(
            ns(paste0("note_", i)),
            label = if (i == 1) "Note" else NULL,
            value = note_cache[[paste0("note_", i)]] %||% "",
            rows = 2
          ),
          shiny::div(
            style = if (i == 1) "padding-top: 1.85rem;" else "",
            shiny::actionButton(
              ns(paste0("rm_note_", i)),
              NULL,
              icon = shiny::icon("minus"),
              class = "btn-outline-danger btn-sm"
            )
          )
        )
      })
    })

    # ── Input observers: write to cache then rebuild loc ──────────────────────
    # Pattern: observe n_X(), register per-row observers that write cache → loc.
    # Using ignoreInit = TRUE prevents spurious fires on first render.

    # ── Scalar fields ────────────────────────────────────────────────────────

    shiny::observeEvent(
      input$title,
      {
        loc$title <- input$title
        shinyjs::enable("confirm")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$release_year,
      {
        loc$release_year <- input$release_year
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$official_url,
      {
        loc$official_url <- input$official_url
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$description,
      {
        loc$description <- .strip_html(input$description)
      },
      ignoreInit = TRUE
    )

    purrr::walk(
      c("id_mobygames", "id_igdb", "id_steam", "id_gog", "id_itch"),
      function(field) {
        shiny::observeEvent(
          input[[field]],
          {
            .rebuild_identifiers()
          },
          ignoreInit = TRUE
        )
      }
    )

    # ── Platform rows ─────────────────────────────────────────────────────────

    shiny::observe({
      n_platforms()
      purrr::walk(seq_len(n_platforms()), function(i) {
        shiny::observeEvent(
          input[[paste0("plat_name_", i)]],
          {
            plat_cache[[paste0("plat_name_", i)]] <- input[[paste0(
              "plat_name_",
              i
            )]]
            .rebuild_platforms()
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
        shiny::observeEvent(
          input[[paste0("plat_year_", i)]],
          {
            plat_cache[[paste0("plat_year_", i)]] <- input[[paste0(
              "plat_year_",
              i
            )]]
            .rebuild_platforms()
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      })
    })

    shiny::observeEvent(input$add_platform, {
      n_platforms(n_platforms() + 1L)
    })

    shiny::observe({
      purrr::walk(seq_len(n_platforms()), function(i) {
        shiny::observeEvent(
          input[[paste0("rm_plat_", i)]],
          {
            if (n_platforms() > 1L) {
              plat_cache[[paste0("plat_name_", i)]] <- NULL
              plat_cache[[paste0("plat_year_", i)]] <- NULL
              n_platforms(n_platforms() - 1L)
              .rebuild_platforms()
            }
          },
          ignoreInit = TRUE,
          once = FALSE
        )
      })
    })

    # ── Originator rows ───────────────────────────────────────────────────────

    shiny::observe({
      n_originators()
      purrr::walk(seq_len(n_originators()), function(i) {
        shiny::observeEvent(
          input[[paste0("orig_name_", i)]],
          {
            orig_cache[[paste0("orig_name_", i)]] <- input[[paste0(
              "orig_name_",
              i
            )]]
            .rebuild_originators()
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
        shiny::observeEvent(
          input[[paste0("orig_role_", i)]],
          {
            orig_cache[[paste0("orig_role_", i)]] <- input[[paste0(
              "orig_role_",
              i
            )]]
            .rebuild_originators()
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      })
    })

    shiny::observeEvent(input$add_originator, {
      n_originators(n_originators() + 1L)
    })

    shiny::observe({
      purrr::walk(seq_len(n_originators()), function(i) {
        shiny::observeEvent(
          input[[paste0("rm_orig_", i)]],
          {
            if (n_originators() > 1L) {
              orig_cache[[paste0("orig_name_", i)]] <- NULL
              orig_cache[[paste0("orig_role_", i)]] <- NULL
              orig_cache[[paste0("orig_moby_id_", i)]] <- NULL
              n_originators(n_originators() - 1L)
              .rebuild_originators()
            }
          },
          ignoreInit = TRUE,
          once = FALSE
        )
      })
    })

    # ── Note rows ─────────────────────────────────────────────────────────────

    shiny::observe({
      n_notes()
      purrr::walk(seq_len(n_notes()), function(i) {
        shiny::observeEvent(
          input[[paste0("note_", i)]],
          {
            note_cache[[paste0("note_", i)]] <- input[[paste0("note_", i)]]
            .rebuild_notes()
          },
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      })
    })

    shiny::observeEvent(input$add_note, {
      n_notes(n_notes() + 1L)
    })

    shiny::observe({
      purrr::walk(seq_len(n_notes()), function(i) {
        shiny::observeEvent(
          input[[paste0("rm_note_", i)]],
          {
            if (n_notes() > 1L) {
              note_cache[[paste0("note_", i)]] <- NULL
              n_notes(n_notes() - 1L)
              .rebuild_notes()
            }
          },
          ignoreInit = TRUE,
          once = FALSE
        )
      })
    })

    # ── Geonames lookup ───────────────────────────────────────────────────────

    shiny::observe({
      purrr::walk(seq_len(n_originators()), function(i) {
        key <- paste0("geo_", i)
        if (!key %in% geo_keys) {
          geo_keys <<- c(geo_keys, key)
        }

        shiny::observeEvent(
          input[[paste0("retrieve_geonames_", i)]],
          {
            shiny::req(input[[paste0("geonames_id_", i)]])
            place <- tryCatch(
              get_geonames_place(
                as.integer(input[[paste0("geonames_id_", i)]]),
                "pgps"
              ),
              error = function(e) NULL
            )
            geonames_results[[key]] <- if (!is.null(place)) {
              list(
                name = place$name,
                country_name = place$countryName,
                country_code = place$countryCode,
                display = paste(
                  place$name,
                  place$countryName,
                  place$countryCode,
                  sep = ", "
                )
              )
            } else {
              list(display = "Not found")
            }
            .rebuild_originators()
          },
          ignoreInit = TRUE
        )

        output[[paste0("geonames_result_", i)]] <- shiny::renderText({
          geonames_results[[key]]$display %||% ""
        })
      })
    })

    # ── Confirm ───────────────────────────────────────────────────────────────

    shiny::observeEvent(input$confirm, {
      shiny::showNotification(
        "Data confirmed — DB write not yet implemented.",
        type = "message"
      )
    })

    shiny::reactive(game_data())
  })
}

## To be copied in the UI:
# mod_data_editor_ui("data_editor_1")

## To be copied in the server:
# mod_data_editor_server("data_editor_1")
