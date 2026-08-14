#── Helpers ──────────────────────────────────────────────────────────────────

#' textInput that fires only on blur.
#' @noRd
text_input_blur <- function(
  inputId,
  label,
  value = "",
  placeholder = NULL,
  width = "100%"
) {
  tag <- textInput(
    inputId,
    label,
    value = value,
    placeholder = placeholder,
    width = width
  )
  tag$children[[2]] <- tagAppendAttributes(
    tag$children[[2]],
    class = "blur-input"
  )
  tag
}

#' SelectizeInput that fires only on blur.
#' @noRd
selectize_input_blur <- function(
  inputId,
  label,
  choices = NULL,
  selected = NULL,
  multiple = FALSE,
  placeholder = NULL,
  width = "100%",
  options = NULL
) {
  tag <- selectizeInput(
    inputId,
    label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    placeholder = placeholder,
    width = width,
    options = options
  )
  # Add class for JS handling if needed
  tag
}


#' textAreaInput that fires only on blur.
#' @noRd
text_area_blur <- function(
  inputId,
  label,
  value = "",
  placeholder = NULL,
  width = "100%",
  rows = 3
) {
  tag <- textAreaInput(
    inputId,
    label,
    value = value,
    placeholder = placeholder,
    width = width,
    rows = rows
  )
  # The <textarea> is the second child of the wrapper div
  tag$children[[2]] <- tagAppendAttributes(
    tag$children[[2]],
    class = "blur-textarea"
  )
  tag
}



# ── Identifiers ────────────────────────────────────────────────────────
panel_identifiers <- function(
  ns = ns,
  identifiers
) {
  bslib::accordion_panel(
    title = tagList(bsicons::bs_icon("upc"), " Identifiers"),
    value = "identifiers",
    p(
      class = "text-muted small mb-2",
      "External IDs."
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      text_input_blur(
        ns("id_mobygames"),
        "MobyGames ID",
        value = identifiers$moby_id
      ),
      text_input_blur(ns("id_igdb"), "IGDB ID", value = identifiers$id_igdb)
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      text_input_blur(
        ns("id_steam"),
        "Steam App ID",
        value = identifiers$steam_id
      ),
      text_input_blur(
        ns("id_gog"),
        "GOG ID",
        value = identifiers$gog_id
      )
    )
  )
}

# ── Core Info ──────────────────────────────────────────────────────────
panel_core <- function(
  ns = NULL,
  info = NULL
) {
  bslib::accordion_panel(
    title = tagList(bsicons::bs_icon("info-circle"), " Core Info"),
    value = "core",
    bslib::layout_columns(
      col_widths = c(8, 4),
      text_input_blur(
        ns("title"),
        "Title",
        value = info$title
      ),
      text_input_blur(
        ns("release_year"),
        "Release year",
        value = info$release_year
      )
    ),
    text_input_blur(
      ns("official_url"),
      "Official URL",
      value = info$official_url
    )
    # Note: description field removed - not in database schema
  )
}

# ── Tags ───────────────────────────────────────────────────────────────
panel_tags <- function(ns = ns) {
  bslib::accordion_panel(
    title = tagList(bsicons::bs_icon("tags"), " Tags"),
    value = "game_tags",
    uiOutput(ns("panel_tags_ui"))
  )
}

panel_tags_ui <- function(ns, game_tags = NULL, game_tags_opts = NULL, token, cat_to_tags = NULL) {
  tagList(
    !!!purrr::imap(game_tags, function(game_tag, i) {
      # Get tags for this row's category (if any) for pre-filtering display
      cat_value <- game_tag$category %||% ""
      if (nzchar(cat_value) && !is.null(cat_to_tags)) {
        cat_tags <- cat_to_tags[[cat_value]] %||% character(0)
        # Show category-specific tags first, then all others
        other_tags <- setdiff(game_tags_opts$name, cat_tags)
        tag_choices <- c("", cat_tags[cat_tags != ""], other_tags[other_tags != ""])
      } else {
        tag_choices <- c("", union(game_tags_opts$name, game_tag$name))
      }

      bslib::layout_columns(
        col_widths = c(5, 6, 1),
        selectizeInput(
          ns(paste0("tag_cat_", token, "_", i)),
          label = if (i == 1) "Category" else NULL,
          choices = c("", union(game_tags_opts$category, game_tag$category)),
          selected = game_tag$category %||% "",
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        selectizeInput(
          ns(paste0("tag_name_", token, "_", i)),
          label = if (i == 1) "Tag name" else NULL,
          choices = tag_choices,
          selected = game_tag$name %||% "",
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        div(
          style = if (i == 1) "padding-top: 1.85rem;" else "",
          actionButton(
            ns(paste0("rm_tag_", token, "_", i)),
            NULL,
            icon = icon("minus"),
            class = "btn-outline-danger btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'})",
              ns("rm_tag_clicked"),
              i
            )
          )
        )
      )
    }),
    actionButton(
      ns("add_tag"),
      "Add tag",
      icon = icon("plus"),
      class = "btn-outline-secondary btn-sm mt-2"
    )
  )
}

# # ── Originators ────────────────────────────────────────────────────────
panel_originators <- function(ns = ns) {
  bslib::accordion_panel(
    title = tagList(bsicons::bs_icon("building"), " Originators"),
    value = "originators",
    uiOutput(ns("panel_originators_ui"))
  )
}

panel_originators_ui <- function(
  ns,
  originators = NULL,
  originators_opts = NULL,
  token
) {
  tagList(
    !!!purrr::imap(originators, function(originator, i) {
      bslib::layout_columns(
        col_widths = c(5, 2, 4, 1),
        selectizeInput(
          ns(paste0("originator_", token, "_", i)),
          label = if (i == 1) "Originator" else NULL,
          choices = c("", union(originators_opts$name, originator$name)),
          selected = originator$name %||% "",
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        selectizeInput(
          ns(paste0("originator_role_", token, "_", i)),
          label = if (i == 1) "Role" else NULL,
          choices = c("", union(originators_opts$role, originator$role)),
          selected = originator$role %||% "",
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        selectizeInput(
          ns(paste0("originator_location_", token, "_", i)),
          label = if (i == 1) "Location" else NULL,
          choices = c(
            "",
            union(originators_opts$location, originator$location)
          ),
          selected = originator$location %||% "",
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),

        div(
          style = if (i == 1) "padding-top: 1.85rem;" else "",
          actionButton(
            ns(paste0("rm_originator_", token, "_", i)),
            NULL,
            icon = icon("minus"),
            class = "btn-outline-danger btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'})",
              ns("rm_originator_clicked"),
              i
            )
          )
        )
      )
    }),
    actionButton(
      ns("add_originator"),
      "Add originator",
      icon = icon("plus"),
      class = "btn-outline-secondary btn-sm mt-2"
    )
  )
}

# ── Load Existing Game ───────────────────────────────────────────────────
load_existing_panel <- function(ns = ns, games = NULL) {
  div(
    class = "mb-4",
    p(class = "text-muted small mb-2", "Select a game from the database to edit"),
    selectizeInput(
      ns("load_existing_select"),
      "Game",
      choices = if (is.null(games) || nrow(games) == 0) {
        c("")  # Always have empty option
      } else {
        c("", setNames(as.character(games$gidb_id), sprintf("%s (ID: %s)", games$title, games$gidb_id)))
      },
      selected = "",  # Default to empty selection
      multiple = FALSE,
      width = "100%",
      options = list(
        placeholder = "Search or select a game..."
      )
    ),
    div(
      style = "margin-top: 1rem;",
      shinyjs::disabled(actionButton(ns("load_existing_btn"), "Load Game", class = "btn-primary"))
    ),
    # Delete button - only visible after a game is loaded
    uiOutput(ns("delete_game_btn_container"))
  )
}

# Helper to render delete button conditionally
render_delete_button <- function(ns, gidb_id, game_title) {
  if (is.null(gidb_id)) {
    return(tagList())
  }
  div(
    class = "mt-3 pt-3 border-top",
    actionButton(
      ns("delete_game_btn"),
      "Delete Game",
      icon = icon("trash"),
      class = "btn-outline-danger btn-sm"
    ),
    p(class = "text-muted small mt-2",
      tags$em(sprintf("Deleting game '%s' (ID: %s)", game_title %||% "Untitled", gidb_id))
    )
  )
}

# ── Notes ──────────────────────────────────────────────────────────────
panel_notes <- function(ns = ns) {
  bslib::accordion_panel(
    title = tagList(bsicons::bs_icon("journal-text"), " Notes"),
    value = "notes",
    uiOutput(ns("panel_notes_ui"))
  )
}

panel_notes_ui <- function(ns, notes, token) {
  tagList(
    !!!purrr::imap(notes, function(note, i) {
      bslib::layout_columns(
        col_widths = c(11, 1),
        text_area_blur(
          ns(paste0("note_", token, "_", i)),
          "Notes",
          value = note
        ),
        div(
          style = if (i == 1) "padding-top: 1.85rem;" else "",
          actionButton(
            ns(paste0("rm_note_", token, "_", i)),
            NULL,
            icon = icon("minus"),
            class = "btn-outline-danger btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'})",
              ns("rm_note_clicked"),
              i
            )
          )
        )
      )
    }),
    actionButton(
      ns("add_note"),
      "Add note",
      icon = icon("plus"),
      class = "btn-outline-secondary btn-sm mt-2"
    )
  )
}
