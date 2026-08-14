# R/db_read_games.R

# ── Editor functions ────────────────────────────────────────────────────────

#' Get or create an editor record based on login
#' If the editor doesn't exist, returns NULL and the caller should show registration modal.
#'
#' @param pool Database pool connection.
#' @param login The login username from authentication.
#' @return The editor_id if found, NULL if not found.
#' @noRd
db_get_editor_by_login <- function(pool, login) {
  tryCatch({
    query <- glue::glue_sql(
      "SELECT editor_id FROM editors WHERE editor_login = {login}",
      .con = pool
    )
    result <- DBI::dbGetQuery(pool, query)
    if (nrow(result) == 0) {
      NULL
    } else {
      result$editor_id[1]
    }
  }, error = function(e) {
    NULL
  })
}

#' Create a new editor record
#'
#' @param pool Database pool connection.
#' @param login The login username.
#' @param first_name Editor's first name.
#' @param last_name Editor's last name.
#' @return The newly created editor_id.
#' @noRd
db_create_editor <- function(pool, login, first_name, last_name) {
  tryCatch({
    query <- glue::glue_sql(
      "INSERT INTO editors (editor_login, editor_name_first, editor_name_last)
       VALUES ({login}, {first_name}, {last_name})
       RETURNING editor_id",
      .con = pool
    )
    result <- DBI::dbGetQuery(pool, query)
    result$editor_id[1]
  }, error = function(e) {
    NULL
  })
}

#' Get editor full name by editor_id
#'
#' @param pool Database pool connection.
#' @param editor_id The editor ID.
#' @return A string with "First Last" or just the available parts.
#' @noRd
db_get_editor_name <- function(pool, editor_id) {
  tryCatch({
    query <- glue::glue_sql(
      "SELECT editor_name_first, editor_name_last FROM editors WHERE editor_id = {editor_id}",
      .con = pool
    )
    result <- DBI::dbGetQuery(pool, query)
    if (nrow(result) == 0) {
      NULL
    } else {
      first <- result$editor_name_first[1]
      last <- result$editor_name_last[1]
      if (!is.null(first) && nzchar(first) && !is.null(last) && nzchar(last)) {
        paste(first, last)
      } else if (!is.null(first) && nzchar(first)) {
        first
      } else if (!is.null(last) && nzchar(last)) {
        last
      } else {
        NULL
      }
    }
  }, error = function(e) {
    NULL
  })
}

# ── Game functions ──────────────────────────────────────────────────────────

#' Look up a game by MobyGames ID
#'
#' @param pool Database pool connection.
#' @param mobygames_id The MobyGames ID to search for.
#' @return The gidb_id if found, otherwise NA_integer_.
#' @noRd
db_find_game_by_mobygames <- function(pool, mobygames_id) {
  if (is.null(mobygames_id) || is.na(mobygames_id) || !nzchar(as.character(mobygames_id))) {
    return(NA_integer_)
  }
  tryCatch({
    query <- glue::glue_sql(
      "SELECT gidb_id FROM games WHERE mobygames_id = {mobygames_id} LIMIT 1",
      .con = pool
    )
    result <- DBI::dbGetQuery(pool, query)
    if (nrow(result) == 0) NA_integer_ else result$gidb_id[1]
  }, error = function(e) {
    NA_integer_
  })
}

#' Look up a game by any available identifier
#'
#' @param pool Database pool connection.
#' @param identifiers A list of identifiers to search for (moby_id, id_igdb, steam_id, gog_id).
#' @return The gidb_id if found, otherwise NA_integer_.
#' @noRd
db_find_game_by_identifiers <- function(pool, identifiers) {
  # Try each identifier in order of reliability
  # Map internal names to database column names
  field_map <- list(
    moby_id = "mobygames_id",
    id_igdb = "igdb_id",
    steam_id = "steam_id",
    gog_id = "gog_id"
  )

  for (internal_name in names(field_map)) {
    db_column <- field_map[[internal_name]]
    if (!is.null(identifiers[[internal_name]]) && !is.na(identifiers[[internal_name]]) &&
        nzchar(as.character(identifiers[[internal_name]]))) {
      tryCatch({
        query <- glue::glue_sql(
          "SELECT gidb_id FROM games WHERE ", .con = pool
        )
        query <- paste0(query, db_column, " = '", identifiers[[internal_name]], "' LIMIT 1")
        result <- DBI::dbGetQuery(pool, query)
        if (nrow(result) > 0) {
          return(result$gidb_id[1])
        }
      }, error = function(e) {
        # Try next identifier
      })
    }
  }
  NA_integer_
}

#' Read all games from the database
#'
#' @param pool Database pool connection.
#' @return A data frame with gidb_id, title, and release_year.
#' @noRd
db_read_games <- function(pool) {
  query <- "
    SELECT
      gidb_id,
      game_title AS title,
      game_year AS release_year
    FROM games
    ORDER BY gidb_id DESC
  "
  DBI::dbGetQuery(pool, query)
}

#' Read tags for a specific game
#'
#' @param pool Database pool connection.
#' @param gidb_id The game ID to fetch tags for.
#' @return A data frame with tag_value and category_value.
#' @noRd
db_read_game_tags <- function(pool, gidb_id) {
  tryCatch({
    query <- glue::glue_sql(
      "SELECT t.tag_value, c.category_value
       FROM tags_games_map tgm
       JOIN tags t ON tgm.tag_id = t.tag_id
       LEFT JOIN categories c ON t.category_id = c.category_id
       WHERE tgm.gidb_id = {gidb_id}
       ORDER BY c.category_value, t.tag_value",
      .con = pool
    )
    result <- DBI::dbGetQuery(pool, query)
    if (nrow(result) == 0) {
      return(data.frame(tag_value = character(0), category_value = character(0)))
    }
    result
  }, error = function(e) {
    # Table doesn't exist or other error - return empty data frame
    data.frame(tag_value = character(0), category_value = character(0))
  })
}

#' Read originators for a specific game
#'
#' @param pool Database pool connection.
#' @param gidb_id The game ID to fetch originators for.
#' @return A data frame with originator_name and originator_role.
#' @noRd
db_read_game_originators <- function(pool, gidb_id) {
  tryCatch({
    query <- glue::glue_sql(
      "SELECT o.originator_name, r.originator_role
       FROM originators_games_map ogm
       JOIN originators o ON ogm.originator_id = o.originator_id
       JOIN originator_roles r ON ogm.originator_role = r.originator_role_id
       WHERE ogm.gidb_id = {gidb_id}
       ORDER BY o.originator_name",
      .con = pool
    )
    result <- DBI::dbGetQuery(pool, query)
    if (nrow(result) == 0) {
      return(data.frame(originator_name = character(0), originator_role = character(0)))
    }
    result
  }, error = function(e) {
    # Table doesn't exist or other error - return empty data frame
    data.frame(originator_name = character(0), originator_role = character(0))
  })
}

#' Read identifiers for a specific game
#'
#' @param pool Database pool connection.
#' @param gidb_id The game ID to fetch identifiers for.
#' @return A list with mobygames_id, igdb_id, steam_id, gog_id.
#' @noRd
db_read_game_identifiers <- function(pool, gidb_id) {
  tryCatch({
    query <- glue::glue_sql(
      "SELECT mobygames_id, igdb_id, steam_id, gog_id
       FROM games
       WHERE gidb_id = {gidb_id}",
      .con = pool
    )
    result <- DBI::dbGetQuery(pool, query)
    if (nrow(result) == 0) {
      return(list())
    }
    as.list(result[1, ])
  }, error = function(e) {
    list()
  })
}

# ── Options for dropdowns ───────────────────────────────────────────────────

#' Read all categories from the database
#'
#' @param pool Database pool connection.
#' @return A character vector of category values.
#' @noRd
db_read_all_categories <- function(pool) {
  tryCatch({
    query <- "SELECT DISTINCT category_value FROM categories ORDER BY category_value"
    result <- DBI::dbGetQuery(pool, query)
    result$category_value
  }, error = function(e) {
    character(0)
  })
}

#' Read all tags from the database (optionally filtered by category)
#'
#' @param pool Database pool connection.
#' @param category Optional category filter.
#' @return A character vector of tag values.
#' @noRd
db_read_all_tags <- function(pool, category = NULL) {
  tryCatch({
    if (is.null(category)) {
      query <- "SELECT DISTINCT tag_value FROM tags ORDER BY tag_value"
    } else {
      query <- glue::glue_sql(
        "SELECT DISTINCT t.tag_value FROM tags t
         JOIN categories c ON t.category_id = c.category_id
         WHERE c.category_value = {category}
         ORDER BY t.tag_value",
        .con = pool
      )
    }
    result <- DBI::dbGetQuery(pool, query)
    result$tag_value
  }, error = function(e) {
    character(0)
  })
}

#' Read all originators from the database
#'
#' @param pool Database pool connection.
#' @return A character vector of originator names.
#' @noRd
db_read_all_originators <- function(pool) {
  tryCatch({
    query <- "SELECT DISTINCT originator_name FROM originators ORDER BY originator_name"
    result <- DBI::dbGetQuery(pool, query)
    result$originator_name
  }, error = function(e) {
    character(0)
  })
}

#' Read all originator roles from the database
#'
#' @param pool Database pool connection.
#' @return A character vector of role names.
#' @noRd
db_read_all_roles <- function(pool) {
  tryCatch({
    query <- "SELECT DISTINCT originator_role FROM originator_roles ORDER BY originator_role"
    result <- DBI::dbGetQuery(pool, query)
    result$originator_role
  }, error = function(e) {
    character(0)
  })
}

# ── Formatting helpers ──────────────────────────────────────────────────────

#' Format multiple values as HTML badges/tags
#'
#' @param values A character vector of values to format.
#' @param type The type of badge for CSS styling ("tag", "originator", "identifier").
#' @param tooltip_values Optional vector of tooltip text (defaults to values).
#' @return A character string of HTML (wrap with shiny::HTML() when rendering).
#' @noRd
format_cell_html <- function(values, type = "tag", tooltip_values = NULL) {
  if (is.null(values) || length(values) == 0 || all(is.na(values))) {
    return("<span class=\"data-empty\">—</span>")
  }

  values <- values[!is.na(values) & nchar(trimws(values)) > 0]
  if (length(values) == 0) {
    return("<span class=\"data-empty\">—</span>")
  }

  tooltip_values <- tooltip_values %||% values

  badge_class <- paste0("data-badge data-badge-", type)

  html_values <- mapply(function(val, tip) {
    sprintf(
      '<span class="%s" title="%s">%s</span>',
      badge_class,
      tip,
      val
    )
  }, values, tooltip_values, SIMPLIFY = TRUE)

  paste(html_values, collapse = " ")
}

#' Format a single value as an HTML badge
#'
#' @param value A single value to format.
#' @param type The type of badge for CSS styling.
#' @return A character string of HTML.
#' @noRd
format_single_html <- function(value, type = "tag") {
  if (is.null(value) || is.na(value) || !nzchar(trimws(value))) {
    return("<span class=\"data-empty\">—</span>")
  }

  sprintf(
    '<span class="data-badge data-badge-%s" title="%s">%s</span>',
    type,
    value,
    value
  )
}
