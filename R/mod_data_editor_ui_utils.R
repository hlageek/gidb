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
      col_widths = c(4, 4, 4),
      text_input_blur(
        ns("id_steam"),
        "Steam App ID",
        value = identifiers$steam_id
      ),
      text_input_blur(
        ns("id_gog"),
        "GOG ID",
        value = identifiers$gog_id
      ),
      text_input_blur(
        ns("id_itch"),
        "itch.io slug",
        value = identifiers$itch_id
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
    ),
    text_area_blur(
      ns("description"),
      "Description",
      value = info$description,
      rows = 4
    )
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

panel_tags_ui <- function(ns, game_tags = NULL, game_tags_opts = NULL) {
  tagList(
    !!!purrr::imap(game_tags, function(game_tag, i) {
      bslib::layout_columns(
        col_widths = c(5, 6, 1),
        selectizeInput(
          ns(paste0("tag_cat_", i)),
          label = if (i == 1) "Category" else NULL,
          choices = union(game_tags_opts$category, game_tag$category) %||% NULL,
          selected = game_tag$category %||% NULL,
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        selectizeInput(
          ns(paste0("tag_name_", i)),
          label = if (i == 1) "Tag name" else NULL,
          choices = game_tags_opts$name %||% NULL,
          selected = game_tag$name %||% NULL,
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        div(
          style = if (i == 1) "padding-top: 1.85rem;" else "",
          actionButton(
            ns(paste0("rm_tag_", i)),
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

# # ── Platforms ──────────────────────────────────────────────────────────
panel_platforms <- function(ns = ns) {
  bslib::accordion_panel(
    title = tagList(bsicons::bs_icon("display"), " Platforms"),
    value = "platforms",
    uiOutput(ns("panel_platforms_ui"))
  )
}

panel_platforms_ui <- function(ns, platforms = NULL, platforms_opts = NULL) {
  tagList(
    !!!purrr::imap(platforms, function(platform, i) {
      bslib::layout_columns(
        col_widths = c(5, 6, 1),
        selectizeInput(
          ns(paste0("platform_", i)),
          label = if (i == 1) "Platform" else NULL,
          choices = union(platforms_opts, platform$name) %||% NULL,
          ,
          selected = platform$name %||% NULL,
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        numericInput(
          ns(paste0("platform_year_", i)),
          label = if (i == 1) "Release year" else NULL,
          value = as.numeric(platform$year %||% NA),
          min = 1950,
          max = as.numeric(format(Sys.Date(), "%Y")) + 5,
          step = 1,
          width = "100%"
        ),
        div(
          style = if (i == 1) "padding-top: 1.85rem;" else "",
          actionButton(
            ns(paste0("rm_platform_", i)),
            NULL,
            icon = icon("minus"),
            class = "btn-outline-danger btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %d, {priority: 'event'})",
              ns("rm_platform_clicked"),
              i
            )
          )
        )
      )
    }),
    actionButton(
      ns("add_platform"),
      "Add platform",
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
  originators_opts = NULL
) {
  tagList(
    !!!purrr::imap(originators, function(originator, i) {
      bslib::layout_columns(
        col_widths = c(5, 2, 4, 1),
        selectizeInput(
          ns(paste0("originator_", i)),
          label = if (i == 1) "Originator" else NULL,
          choices = union(originators_opts$name, originator$name) %||% NULL,
          ,
          selected = originator$name %||% NULL,
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        selectizeInput(
          ns(paste0("originator_role_", i)),
          label = if (i == 1) "Role" else NULL,
          choices = union(originators_opts$role, originator$role) %||% NULL,
          ,
          selected = originator$role %||% NULL,
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),
        selectizeInput(
          ns(paste0("originator_location_", i)),
          label = if (i == 1) "Location" else NULL,
          choices = union(originators_opts$location, originator$location) %||%
            NULL,
          ,
          selected = originator$location %||% NULL,
          multiple = FALSE,
          width = "100%",
          options = list(create = TRUE, placeholder = "Select or type…")
        ),

        div(
          style = if (i == 1) "padding-top: 1.85rem;" else "",
          actionButton(
            ns(paste0("rm_originator_", i)),
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

# ── Notes ──────────────────────────────────────────────────────────────
panel_notes <- function(ns = ns) {
  bslib::accordion_panel(
    title = tagList(bsicons::bs_icon("journal-text"), " Notes"),
    value = "notes",
    uiOutput(ns("panel_notes_ui"))
  )
}

panel_notes_ui <- function(ns, notes) {
  tagList(
    !!!purrr::imap(notes, function(note, i) {
      bslib::layout_columns(
        col_widths = c(11, 1),
        text_area_blur(
          ns(paste0("note_", i)),
          "Notes",
          value = note
        ),
        div(
          style = if (i == 1) "padding-top: 1.85rem;" else "",
          actionButton(
            ns(paste0("rm_originator_", i)),
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
