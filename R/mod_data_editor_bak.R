# #' data_editor UI Function
# #'
# #' @description A shiny Module for adding/editing game records. Supports
# #'   manual data entry and semi-automated entry via MobyGames API.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd
# #'
# #' @importFrom shiny NS tagList
# mod_data_editor_ui <- function(id) {
#   ns <- NS(id)

#   tagList(
#     div(
#       class = "editor-layout",

#       # ── LEFT: Preview panel ────────────────────────────────────────────────
#       div(
#         class = "preview-panel",
#         mod_game_card_ui(ns("game_card_1"))
#       ),

#       # ── RIGHT: Editor panel (flat accordion, no wrapper card) ─────────────
#       div(
#         class = "editor-panel",
#         uiOutput(ns("editor"))
#       )
#     )
#   )
# }

# # ── Helpers ──────────────────────────────────────────────────────────────────

# #' textInput that fires only on blur.
# #' @noRd
# .text_input_blur <- function(
#   inputId,
#   label,
#   value = "",
#   placeholder = NULL,
#   width = "100%"
# ) {
#   tag <- textInput(
#     inputId,
#     label,
#     value = value,
#     placeholder = placeholder,
#     width = width
#   )
#   tag$children[[2]] <- tagAppendAttributes(
#     tag$children[[2]],
#     class = "blur-input"
#   )
#   tag
# }

# #' textAreaInput that fires only on blur.
# #' @noRd
# .text_area_blur <- function(
#   inputId,
#   label,
#   value = "",
#   placeholder = NULL,
#   width = "100%",
#   rows = 3
# ) {
#   tag <- textAreaInput(
#     inputId,
#     label,
#     value = value,
#     placeholder = placeholder,
#     width = width,
#     rows = rows
#   )
#   # The <textarea> is the second child of the wrapper div
#   tag$children[[2]] <- tagAppendAttributes(
#     tag$children[[2]],
#     class = "blur-textarea"
#   )
#   tag
# }

# #' Dynamic rows UI helper.
# #' @noRd
# .dynamic_rows_ui <- function(ns, n, add_btn_id, add_label, row_fn) {
#   tagList(
#     !!!purrr::map(seq_len(n), row_fn),
#     actionButton(
#       ns(add_btn_id),
#       add_label,
#       icon = icon("plus"),
#       class = "btn-outline-secondary btn-sm mt-2"
#     )
#   )
# }

# #' data_editor Server Function
# #'
# #' @noRd
# mod_data_editor_server <- function(id, pool, user) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     # ── Reactive state ────────────────────────────────────────────────────────

#     loc <- reactiveValues()

#     # loc: single source of truth for preview. All caches write into it.
#     game_data <- reactiveValues(
#       title = NULL,
#       release_year = NULL,
#       description = NULL,
#       official_url = NULL,
#       identifiers = list(), # named list label → value
#       tags = list(), # tags: list of list(name, category)
#       platforms = list(), # list of list(name, year) — structured, not strings
#       originators = list(), # list of list(name, role, location)
#       notes = NULL # character vector
#     )

#     # ── Preview module ────────────────────────────────────────────────────────
#     preview <- mod_game_card_server(
#       "preview",
#       values = game_data
#     )

#     geonames_results <- reactiveValues() # key: "geo_{i}"
#     geo_keys <- character(0) # track all keys for reliable reset

#     n_originators <- reactiveVal(1L)
#     n_platforms <- reactiveVal(1L)
#     n_notes <- reactiveVal(1L)

#     # ── Per-row caches (survive renderUI re-renders) ──────────────────────────
#     plat_cache <- reactiveValues() # plat_name_i, plat_year_i
#     orig_cache <- reactiveValues() # orig_name_i, orig_role_i, orig_moby_id_i
#     note_cache <- reactiveValues() # note_i

#     # Tag categories available in the selectize.
#     # Starts with defaults; can be seeded from DB at init; grows as user types.
#     .default_tag_cats <- c(
#       "Basic Genres",
#       "Perspective",
#       "Gameplay",
#       "Interface",
#       "Setting",
#       "Pacing",
#       "Visual",
#       "Narrative / Theme / Topic"
#     )
#     tag_cats <- reactiveVal(.default_tag_cats)

#     # ── Rebuild helpers ───────────────────────────────────────────────────────
#     # Each reads from the cache (not from input[[]] directly when n changes),
#     # so values always survive re-renders.

#     .rebuild_platforms <- function() {
#       n <- n_platforms()
#       loc$platforms <- purrr::map(seq_len(n), function(i) {
#         list(
#           name = plat_cache[[paste0("plat_name_", i)]] %||% "",
#           year = plat_cache[[paste0("plat_year_", i)]] %||% ""
#         )
#       }) |>
#         purrr::keep(\(x) nchar(x$name) > 0)
#     }

#     .rebuild_originators <- function() {
#       n <- n_originators()
#       loc$originators <- purrr::map(seq_len(n), function(i) {
#         nm <- orig_cache[[paste0("orig_name_", i)]] %||% ""
#         if (nchar(nm) == 0) {
#           return(NULL)
#         }
#         geo <- geonames_results[[paste0("geo_", i)]]
#         list(
#           name = nm,
#           role = orig_cache[[paste0("orig_role_", i)]] %||% "",
#           location = if (!is.null(geo)) geo$display else ""
#         )
#       }) |>
#         purrr::compact()
#     }

#     .rebuild_notes <- function() {
#       n <- n_notes()
#       vec <- purrr::map_chr(seq_len(n), \(i) {
#         note_cache[[paste0("note_", i)]] %||% ""
#       })
#       loc$notes <- vec[nchar(vec) > 0]
#     }

#     .rebuild_identifiers <- function() {
#       ids <- list()
#       fields <- c(
#         id_mobygames = "MobyGames",
#         id_igdb = "IGDB",
#         id_steam = "Steam",
#         id_gog = "GOG",
#         id_itch = "itch.io"
#       )
#       for (field in names(fields)) {
#         val <- input[[field]] %||% ""
#         if (nchar(val) > 0) ids[[fields[[field]]]] <- val
#       }
#       loc$identifiers <- ids
#     }

#     .clear_caches <- function() {
#       for (k in ls(plat_cache)) {
#         plat_cache[[k]] <- NULL
#       }
#       for (k in ls(orig_cache)) {
#         orig_cache[[k]] <- NULL
#       }
#       for (k in ls(note_cache)) {
#         note_cache[[k]] <- NULL
#       }
#       for (k in geo_keys) {
#         geonames_results[[k]] <- NULL
#       }
#       geo_keys <<- character(0)
#     }

#     # ── MobyGames fetch ───────────────────────────────────────────────────────

#     observeEvent(input$retrieve_mobygames, {
#       req(input$mobygames_id)
#       game_data(NULL)

#       id_int <- suppressWarnings(as.integer(trimws(input$mobygames_id)))
#       if (is.na(id_int)) {
#         output$fetch_status <- renderUI(div(
#           class = "alert alert-danger py-2 d-flex gap-2",
#           bsicons::bs_icon("exclamation-triangle"),
#           paste("Invalid ID:", input$mobygames_id)
#         ))
#         return()
#       }

#       output$fetch_status <- renderUI(div(
#         class = "text-muted d-flex gap-2",
#         icon("spinner", class = "fa-spin"),
#         "Fetching..."
#       ))

#       result <- tryCatch(moby_get_game_metadata(id_int), error = function(e) e)

#       if (inherits(result, "error")) {
#         output$fetch_status <- renderUI(div(
#           class = "alert alert-danger py-2 d-flex gap-2",
#           bsicons::bs_icon("x-circle"),
#           conditionMessage(result)
#         ))
#         return()
#       }

#       game_data(result)
#       .clear_caches()

#       # ── Row counts ──
#       n_platforms(max(1L, length(result$platforms)))
#       n_originators(max(1L, length(result$originators)))
#       n_notes(1L)

#       # Flatten MobyGames tag buckets → loc$tags list(list(name, category), ...)
#       all_tags <- purrr::imap(result$tags, function(entries, bucket) {
#         cat_label <- tools::toTitleCase(gsub("_", " ", bucket))
#         purrr::map(entries, \(e) list(name = e$name, category = cat_label))
#       }) |>
#         purrr::flatten()

#       # Extend available categories with anything new from this result
#       new_cats <- unique(purrr::map_chr(all_tags, \(t) t$category %||% ""))
#       tag_cats(union(.default_tag_cats, new_cats))

#       # ── Seed platform and originator caches ──
#       purrr::iwalk(result$platforms, function(p, i) {
#         plat_cache[[paste0("plat_name_", i)]] <- p$platform_name
#         plat_cache[[paste0("plat_year_", i)]] <- p$release_year %||% ""
#       })
#       purrr::iwalk(result$originators, function(o, i) {
#         orig_cache[[paste0("orig_name_", i)]] <- o$company_name
#         orig_cache[[paste0("orig_role_", i)]] <- o$role
#         orig_cache[[paste0("orig_moby_id_", i)]] <- as.character(
#           o$company_id %||% ""
#         )
#       })

#       # ── Seed loc ──
#       loc$title <- result$moby_title
#       loc$description <- .strip_html(result$description)
#       loc$official_url <- result$official_url %||% ""
#       loc$identifiers <- list(MobyGames = as.character(result$moby_id))
#       loc$notes <- NULL

#       years <- purrr::map_chr(result$platforms, \(p) p$release_year %||% "")
#       loc$release_year <- if (any(nchar(years) > 0)) {
#         min(years[nchar(years) > 0])
#       } else {
#         ""
#       }

#       loc$platforms <- purrr::map(result$platforms, \(p) {
#         list(name = p$platform_name, year = p$release_year %||% "")
#       })
#       loc$originators <- purrr::map(result$originators, \(o) {
#         list(name = o$company_name, role = o$role, location = "")
#       })
#       loc$tags <- all_tags

#       output$fetch_status <- renderUI(div(
#         class = "alert alert-success py-2 d-flex gap-2",
#         bsicons::bs_icon("check-circle"),
#         paste("Loaded:", result$moby_title)
#       ))
#     })

#     # ── Reset ─────────────────────────────────────────────────────────────────

#     observeEvent(preview$reset(), {
#       game_data(NULL)
#       .clear_caches()
#       n_originators(1L)
#       n_platforms(1L)
#       n_notes(1L)
#       tag_cats(.default_tag_cats)
#       loc$title <- loc$release_year <- loc$description <- loc$official_url <- loc$notes <- NULL
#       loc$originators <- list()
#       loc$tags <- list()
#       loc$platforms <- list()
#       loc$identifiers <- list()
#       output$fetch_status <- renderUI(NULL)
#     })

#     # ── Editor (server-rendered accordion, flat) ──────────────────────────────

#     output$editor <- renderUI({
#       d <- game_data()
#       is_auto <- !is.null(d)
#       val <- function(x) if (is_auto && !is.null(x)) as.character(x) else ""

#       release_year_val <- {
#         years <- if (is_auto) {
#           purrr::map_chr(d$platforms, \(p) p$release_year %||% "")
#         } else {
#           character(0)
#         }
#         if (any(nchar(years) > 0)) min(years[nchar(years) > 0]) else ""
#       }

#       # ── Entry mode + MobyGames fetch ───────────────────────────────────────
#       panel_entry <- bslib::accordion_panel(
#         title = tagList(
#           bsicons::bs_icon("pencil-square"),
#           " Entry Mode"
#         ),
#         value = "entry",
#         radioButtons(
#           ns("entry_mode"),
#           label = "Entry mode",
#           choices = c("Automated (MobyGames)" = "auto", "Manual" = "manual"),
#           selected = "auto",
#           inline = TRUE
#         ),
#         conditionalPanel(
#           condition = sprintf("input['%s'] === 'auto'", ns("entry_mode")),
#           bslib::layout_columns(
#             col_widths = c(8, 4),
#             textInput(
#               ns("mobygames_id"),
#               "MobyGames ID",
#               placeholder = "e.g. 366",
#               width = "100%"
#             ),
#             div(
#               style = "padding-top: 1.85rem;",
#               actionButton(
#                 ns("retrieve_mobygames"),
#                 "Fetch",
#                 icon = icon("cloud-download-alt"),
#                 class = "btn-primary w-100"
#               )
#             )
#           ),
#           uiOutput(ns("fetch_status"))
#         )
#       )

# # ── Core Info ──────────────────────────────────────────────────────────
# panel_core <- bslib::accordion_panel(
#   title = tagList(bsicons::bs_icon("info-circle"), " Core Info"),
#   value = "core",
#   bslib::layout_columns(
#     col_widths = c(8, 4),
#     .text_input_blur(
#       ns("title"),
#       "Title",
#       value = loc$title %||% val(d$moby_title)
#     ),
#     .text_input_blur(
#       ns("release_year"),
#       "Release year",
#       value = loc$release_year %||% release_year_val
#     )
#   ),
#   .text_input_blur(
#     ns("official_url"),
#     "Official URL",
#     value = loc$official_url %||% val(d$official_url)
#   ),
#   .text_area_blur(
#     ns("description"),
#     "Description",
#     value = loc$description %||%
#       .strip_html(if (is_auto) d$description else NULL),
#     rows = 4
#   )
# )

# # ── Identifiers ────────────────────────────────────────────────────────
# panel_identifiers <- bslib::accordion_panel(
#   title = tagList(bsicons::bs_icon("upc"), " Identifiers"),
#   value = "identifiers",
#   p(
#     class = "text-muted small mb-2",
#     "External IDs. MobyGames ID pre-filled from fetch."
#   ),
#   bslib::layout_columns(
#     col_widths = c(6, 6),
#     .text_input_blur(
#       ns("id_mobygames"),
#       "MobyGames ID",
#       value = val(d$moby_id)
#     ),
#     .text_input_blur(ns("id_igdb"), "IGDB ID", value = "")
#   ),
#   bslib::layout_columns(
#     col_widths = c(4, 4, 4),
#     .text_input_blur(
#       ns("id_steam"),
#       "Steam App ID",
#       value = val(d$other_ids$steam_id)
#     ),
#     .text_input_blur(
#       ns("id_gog"),
#       "GOG ID",
#       value = val(d$other_ids$gog_id)
#     ),
#     .text_input_blur(ns("id_itch"), "itch.io slug", value = "")
#   )
# )

# # ── Tags ───────────────────────────────────────────────────────────────
# panel_tags <- bslib::accordion_panel(
#   title = tagList(bsicons::bs_icon("tags"), " Tags"),
#   value = "tags",
#   uiOutput(ns("tag_rows"))
# )

# # ── Platforms ──────────────────────────────────────────────────────────
# panel_platforms <- bslib::accordion_panel(
#   title = tagList(bsicons::bs_icon("display"), " Platforms"),
#   value = "platforms",
#   uiOutput(ns("platform_rows"))
# )

# # ── Originators ────────────────────────────────────────────────────────
# panel_originators <- bslib::accordion_panel(
#   title = tagList(bsicons::bs_icon("building"), " Originators"),
#   value = "originators",
#   uiOutput(ns("originator_rows"))
# )

# # ── Notes ──────────────────────────────────────────────────────────────
# panel_notes <- bslib::accordion_panel(
#   title = tagList(bsicons::bs_icon("journal-text"), " Notes"),
#   value = "notes",
#   uiOutput(ns("note_rows"))
# )

#       bslib::accordion(
#         id = ns("game_accordion"),
#         open = "entry",
#         multiple = TRUE,
#         panel_entry,
#         panel_core,
#         panel_identifiers,
#         panel_tags,
#         panel_platforms,
#         panel_originators,
#         panel_notes
#       )
#     })

#     # ── Tag rows ──────────────────────────────────────────────────────────────
#     # loc$tags is the source of truth: list of list(name, category).
#     # The renderUI reads from it to prefill; observers write back to it.
#     # Selectize fires on render but the identity guard below makes it a no-op.

#     output$tag_rows <- renderUI({
#       tags <- if (length(loc$tags) > 0) {
#         loc$tags
#       } else {
#         list(list(name = "", category = ""))
#       }
#       cats <- tag_cats()
#       .dynamic_rows_ui(ns, length(tags), "add_tag", "Add tag", function(i) {
#         bslib::layout_columns(
#           col_widths = c(5, 6, 1),
#           .text_input_blur(
#             ns(paste0("tag_name_", i)),
#             label = if (i == 1) "Tag name" else NULL,
#             value = tags[[i]]$name %||% ""
#           ),
#           selectizeInput(
#             ns(paste0("tag_cat_", i)),
#             label = if (i == 1) "Category" else NULL,
#             choices = cats,
#             selected = tags[[i]]$category %||% NULL,
#             multiple = FALSE,
#             width = "100%",
#             options = list(create = TRUE, placeholder = "Select or type…")
#           ),
#           div(
#             style = if (i == 1) "padding-top: 1.85rem;" else "",
#             actionButton(
#               ns(paste0("rm_tag_", i)),
#               NULL,
#               icon = icon("minus"),
#               class = "btn-outline-danger btn-sm"
#             )
#           )
#         )
#       })
#     })

#     # Observers: each writes one field of one row back into loc$tags.
#     # tag_name uses blur so it only fires on focus-out.
#     # tag_cat (selectize) fires on render too, but the identity check makes
#     # that a no-op — it only writes when the value genuinely changed.
#     # The outer observe re-runs only when the number of rows changes so that
#     # observers for new rows get registered. loc$tags length is read via isolate
#     # inside observeEvent bodies to avoid re-registering on every tag value change.
#     observe({
#       n <- max(1L, length(isolate(loc$tags)))
#       # Re-run this observe when a row is added or removed
#       input$add_tag
#       purrr::walk(seq_len(n), function(i) {
#         observeEvent(
#           input[[paste0("tag_name_", i)]],
#           {
#             if (i > length(loc$tags)) {
#               return()
#             } # guard: list may have shrunk
#             val <- input[[paste0("tag_name_", i)]] %||% ""
#             if (!identical(val, loc$tags[[i]]$name %||% "")) {
#               tags <- loc$tags
#               tags[[i]]$name <- val
#               loc$tags <- tags
#             }
#           },
#           ignoreInit = TRUE,
#           ignoreNULL = FALSE
#         )

#         observeEvent(
#           input[[paste0("tag_cat_", i)]],
#           {
#             if (i > length(loc$tags)) {
#               return()
#             } # guard: list may have shrunk
#             val <- input[[paste0("tag_cat_", i)]] %||% ""
#             if (!identical(val, loc$tags[[i]]$category %||% "")) {
#               tags <- loc$tags
#               tags[[i]]$category <- val
#               loc$tags <- tags
#               if (nchar(val) > 0 && !val %in% tag_cats()) {
#                 tag_cats(c(tag_cats(), val))
#               }
#             }
#           },
#           ignoreInit = FALSE,
#           ignoreNULL = TRUE
#         )
#       })
#     })

#     observeEvent(input$add_tag, {
#       loc$tags <- c(loc$tags, list(list(name = "", category = "")))
#     })

#     observe({
#       n <- max(1L, length(isolate(loc$tags)))
#       input$add_tag # re-register when rows change
#       purrr::walk(seq_len(n), function(i) {
#         observeEvent(
#           input[[paste0("rm_tag_", i)]],
#           {
#             tags <- loc$tags
#             loc$tags <- if (length(tags) > 1L) {
#               tags[-i]
#             } else {
#               list(list(name = "", category = ""))
#             }
#           },
#           ignoreInit = TRUE,
#           once = FALSE
#         )
#       })
#     })

#     # ── Platform rows ─────────────────────────────────────────────────────────

#     output$platform_rows <- renderUI({
#       n <- n_platforms()
#       .dynamic_rows_ui(ns, n, "add_platform", "Add platform", function(i) {
#         bslib::layout_columns(
#           col_widths = c(7, 4, 1),
#           .text_input_blur(
#             ns(paste0("plat_name_", i)),
#             if (i == 1) "Platform" else NULL,
#             value = plat_cache[[paste0("plat_name_", i)]] %||% ""
#           ),
#           .text_input_blur(
#             ns(paste0("plat_year_", i)),
#             if (i == 1) "Release year" else NULL,
#             value = plat_cache[[paste0("plat_year_", i)]] %||% ""
#           ),
#           div(
#             style = if (i == 1) "padding-top: 1.85rem;" else "",
#             actionButton(
#               ns(paste0("rm_plat_", i)),
#               NULL,
#               icon = icon("minus"),
#               class = "btn-outline-danger btn-sm"
#             )
#           )
#         )
#       })
#     })

#     # ── Originator rows ───────────────────────────────────────────────────────

#     output$originator_rows <- renderUI({
#       n <- n_originators()
#       rows <- purrr::map(seq_len(n), function(i) {
#         tagList(
#           if (i > 1) hr(class = "my-2"),
#           bslib::layout_columns(
#             col_widths = c(5, 3, 3, 1),
#             .text_input_blur(
#               ns(paste0("orig_name_", i)),
#               if (i == 1) "Company name" else NULL,
#               value = orig_cache[[paste0("orig_name_", i)]] %||% ""
#             ),
#             selectizeInput(
#               ns(paste0("orig_role_", i)),
#               label = if (i == 1) "Role" else NULL,
#               choices = c("developer", "publisher", "porter", "distributor"),
#               selected = orig_cache[[paste0("orig_role_", i)]] %||% "developer",
#               width = "100%"
#             ),
#             .text_input_blur(
#               ns(paste0("orig_moby_id_", i)),
#               if (i == 1) "MobyGames co. ID" else NULL,
#               value = orig_cache[[paste0("orig_moby_id_", i)]] %||% ""
#             ),
#             div(
#               style = if (i == 1) "padding-top: 1.85rem;" else "",
#               actionButton(
#                 ns(paste0("rm_orig_", i)),
#                 NULL,
#                 icon = icon("minus"),
#                 class = "btn-outline-danger btn-sm"
#               )
#             )
#           ),
#           bslib::layout_columns(
#             col_widths = c(4, 3, 5),
#             .text_input_blur(
#               ns(paste0("geonames_id_", i)),
#               label = tagList(
#                 "Location (",
#                 a(
#                   "Geonames ID",
#                   href = "https://www.geonames.org/",
#                   target = "_blank"
#                 ),
#                 ")"
#               )
#             ),
#             div(
#               style = "padding-top: 1.85rem;",
#               actionButton(
#                 ns(paste0("retrieve_geonames_", i)),
#                 "Look up",
#                 icon = icon("location-dot"),
#                 class = "btn-outline-secondary btn-sm w-100"
#               )
#             ),
#             div(
#               style = "padding-top: 1.9rem;",
#               textOutput(ns(paste0("geonames_result_", i)))
#             )
#           )
#         )
#       })
#       tagList(
#         !!!rows,
#         actionButton(
#           ns("add_originator"),
#           "Add originator",
#           icon = icon("plus"),
#           class = "btn-outline-secondary btn-sm mt-2"
#         )
#       )
#     })

#     # ── Note rows ────────────────────────────────────────────────────────────
#     # Uses blur-textarea so editing is not disrupted by reactive re-renders.

#     output$note_rows <- renderUI({
#       n <- n_notes()
#       .dynamic_rows_ui(ns, n, "add_note", "Add note", function(i) {
#         bslib::layout_columns(
#           col_widths = c(11, 1),
#           .text_area_blur(
#             ns(paste0("note_", i)),
#             label = if (i == 1) "Note" else NULL,
#             value = note_cache[[paste0("note_", i)]] %||% "",
#             rows = 2
#           ),
#           div(
#             style = if (i == 1) "padding-top: 1.85rem;" else "",
#             actionButton(
#               ns(paste0("rm_note_", i)),
#               NULL,
#               icon = icon("minus"),
#               class = "btn-outline-danger btn-sm"
#             )
#           )
#         )
#       })
#     })

#     # ── Input observers: write to cache then rebuild loc ──────────────────────
#     # Pattern: observe n_X(), register per-row observers that write cache → loc.
#     # Using ignoreInit = TRUE prevents spurious fires on first render.

#     # ── Scalar fields ────────────────────────────────────────────────────────

#     observeEvent(
#       input$title,
#       {
#         loc$title <- input$title
#       },
#       ignoreInit = TRUE
#     )

#     observeEvent(
#       input$release_year,
#       {
#         loc$release_year <- input$release_year
#       },
#       ignoreInit = TRUE
#     )

#     observeEvent(
#       input$official_url,
#       {
#         loc$official_url <- input$official_url
#       },
#       ignoreInit = TRUE
#     )

#     observeEvent(
#       input$description,
#       {
#         loc$description <- .strip_html(input$description)
#       },
#       ignoreInit = TRUE
#     )

#     purrr::walk(
#       c("id_mobygames", "id_igdb", "id_steam", "id_gog", "id_itch"),
#       function(field) {
#         observeEvent(
#           input[[field]],
#           {
#             .rebuild_identifiers()
#           },
#           ignoreInit = TRUE
#         )
#       }
#     )

#     # ── Platform rows ─────────────────────────────────────────────────────────

#     observe({
#       n_platforms()
#       purrr::walk(seq_len(n_platforms()), function(i) {
#         observeEvent(
#           input[[paste0("plat_name_", i)]],
#           {
#             plat_cache[[paste0("plat_name_", i)]] <- input[[paste0(
#               "plat_name_",
#               i
#             )]]
#             .rebuild_platforms()
#           },
#           ignoreInit = TRUE,
#           ignoreNULL = FALSE
#         )
#         observeEvent(
#           input[[paste0("plat_year_", i)]],
#           {
#             plat_cache[[paste0("plat_year_", i)]] <- input[[paste0(
#               "plat_year_",
#               i
#             )]]
#             .rebuild_platforms()
#           },
#           ignoreInit = TRUE,
#           ignoreNULL = FALSE
#         )
#       })
#     })

#     observeEvent(input$add_platform, {
#       n_platforms(n_platforms() + 1L)
#     })

#     observe({
#       purrr::walk(seq_len(n_platforms()), function(i) {
#         observeEvent(
#           input[[paste0("rm_plat_", i)]],
#           {
#             if (n_platforms() > 1L) {
#               plat_cache[[paste0("plat_name_", i)]] <- NULL
#               plat_cache[[paste0("plat_year_", i)]] <- NULL
#               n_platforms(n_platforms() - 1L)
#               .rebuild_platforms()
#             }
#           },
#           ignoreInit = TRUE,
#           once = FALSE
#         )
#       })
#     })

#     # ── Originator rows ───────────────────────────────────────────────────────

#     observe({
#       n_originators()
#       purrr::walk(seq_len(n_originators()), function(i) {
#         observeEvent(
#           input[[paste0("orig_name_", i)]],
#           {
#             orig_cache[[paste0("orig_name_", i)]] <- input[[paste0(
#               "orig_name_",
#               i
#             )]]
#             .rebuild_originators()
#           },
#           ignoreInit = TRUE,
#           ignoreNULL = FALSE
#         )
#         observeEvent(
#           input[[paste0("orig_role_", i)]],
#           {
#             orig_cache[[paste0("orig_role_", i)]] <- input[[paste0(
#               "orig_role_",
#               i
#             )]]
#             .rebuild_originators()
#           },
#           ignoreInit = TRUE,
#           ignoreNULL = FALSE
#         )
#       })
#     })

#     observeEvent(input$add_originator, {
#       n_originators(n_originators() + 1L)
#     })

#     observe({
#       purrr::walk(seq_len(n_originators()), function(i) {
#         observeEvent(
#           input[[paste0("rm_orig_", i)]],
#           {
#             if (n_originators() > 1L) {
#               orig_cache[[paste0("orig_name_", i)]] <- NULL
#               orig_cache[[paste0("orig_role_", i)]] <- NULL
#               orig_cache[[paste0("orig_moby_id_", i)]] <- NULL
#               n_originators(n_originators() - 1L)
#               .rebuild_originators()
#             }
#           },
#           ignoreInit = TRUE,
#           once = FALSE
#         )
#       })
#     })

#     # ── Note rows ─────────────────────────────────────────────────────────────

#     observe({
#       n_notes()
#       purrr::walk(seq_len(n_notes()), function(i) {
#         observeEvent(
#           input[[paste0("note_", i)]],
#           {
#             note_cache[[paste0("note_", i)]] <- input[[paste0("note_", i)]]
#             .rebuild_notes()
#           },
#           ignoreInit = TRUE,
#           ignoreNULL = FALSE
#         )
#       })
#     })

#     observeEvent(input$add_note, {
#       n_notes(n_notes() + 1L)
#     })

#     observe({
#       purrr::walk(seq_len(n_notes()), function(i) {
#         observeEvent(
#           input[[paste0("rm_note_", i)]],
#           {
#             if (n_notes() > 1L) {
#               note_cache[[paste0("note_", i)]] <- NULL
#               n_notes(n_notes() - 1L)
#               .rebuild_notes()
#             }
#           },
#           ignoreInit = TRUE,
#           once = FALSE
#         )
#       })
#     })

#     # ── Geonames lookup ───────────────────────────────────────────────────────

#     observe({
#       purrr::walk(seq_len(n_originators()), function(i) {
#         key <- paste0("geo_", i)
#         if (!key %in% geo_keys) {
#           geo_keys <<- c(geo_keys, key)
#         }

#         observeEvent(
#           input[[paste0("retrieve_geonames_", i)]],
#           {
#             req(input[[paste0("geonames_id_", i)]])
#             place <- tryCatch(
#               get_geonames_place(
#                 as.integer(input[[paste0("geonames_id_", i)]]),
#                 "pgps"
#               ),
#               error = function(e) NULL
#             )
#             geonames_results[[key]] <- if (!is.null(place)) {
#               list(
#                 name = place$name,
#                 country_name = place$countryName,
#                 country_code = place$countryCode,
#                 display = paste(
#                   place$name,
#                   place$countryName,
#                   place$countryCode,
#                   sep = ", "
#                 )
#               )
#             } else {
#               list(display = "Not found")
#             }
#             .rebuild_originators()
#           },
#           ignoreInit = TRUE
#         )

#         output[[paste0("geonames_result_", i)]] <- renderText({
#           geonames_results[[key]]$display %||% ""
#         })
#       })
#     })

#     # ── Confirm ───────────────────────────────────────────────────────────────

#     observeEvent(preview$confirm(), {
#       showNotification(
#         "Data confirmed — DB write not yet implemented.",
#         type = "message"
#       )
#     })

#     reactive(game_data())
#   })
# }

# ## To be copied in the UI:
# # mod_data_editor_ui("data_editor_1")

# ## To be copied in the server:
# # mod_data_editor_server("data_editor_1")
