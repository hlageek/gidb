# R/db_creates.R

#' @noRd
db_create_category <- function(pool, category_value, category_source) {
  query <- glue::glue_sql(
    "INSERT INTO categories (category_value, category_source) VALUES ({category_value}, {category_source}) RETURNING category_id",
    .con = pool
  )
  DBI::dbGetQuery(pool, query)$category_id[1]
}

#' @noRd
db_create_tag <- function(pool, tag_value, category_id) {
  query <- glue::glue_sql(
    "INSERT INTO tags (tag_value, category_id) VALUES ({tag_value}, {category_id}) RETURNING tag_id",
    .con = pool
  )
  DBI::dbGetQuery(pool, query)$tag_id[1]
}

#' @noRd
db_create_originator_role <- function(pool, role_name) {
  query <- glue::glue_sql(
    "INSERT INTO originator_roles (originator_role) VALUES ({role_name}) RETURNING originator_role_id",
    .con = pool
  )
  DBI::dbGetQuery(pool, query)$originator_role_id[1]
}

#' @noRd
db_create_originator <- function(
  pool,
  originator_name,
  originator_mobygames_id = NA_integer_,
  originator_igdb_id = NA_integer_,
  originator_description = NULL
) {
  # Use sql_null helper for proper NULL handling
  moby_val <- if (is.na(originator_mobygames_id)) DBI::SQL("NULL") else originator_mobygames_id
  igdb_val <- if (is.na(originator_igdb_id)) DBI::SQL("NULL") else originator_igdb_id
  desc_val <- sql_null(originator_description)

  query <- glue::glue_sql(
    "INSERT INTO originators (originator_mobygames_id, originator_igdb_id, originator_name, originator_description)
     VALUES ({moby_val}, {igdb_val}, {originator_name}, {desc_val})
     RETURNING originator_id",
    .con = pool
  )
  DBI::dbGetQuery(pool, query)$originator_id[1]
}


# R/db_write_game.R

#' @noRd
db_write_game <- function(
  conn,
  info,
  identifiers,
  user_id = NULL,
  gidb_id = NULL
) {
  release_year <- suppressWarnings(as.integer(info$release_year))

  if (is.null(gidb_id)) {
    # INSERT new game
    query <- glue::glue_sql(
      "INSERT INTO games (
       mobygames_id, igdb_id, steam_id, gog_id,
       game_title, game_year, game_url, status,
       created_by, modified_by, created_at, modified_at
     ) VALUES (
       {sql_null(identifiers$moby_id)},
       {sql_null(identifiers$id_igdb)},
       {sql_null(identifiers$steam_id)},
       {sql_null(identifiers$gog_id)},
       {sql_null(info$title)},
       {sql_null(release_year)},
       {sql_null(info$official_url)},
       'draft',
       {user_id},
       {user_id},
       NOW(),
       NOW()
     ) RETURNING gidb_id",
      .con = conn
    )

    result <- DBI::dbGetQuery(conn, query)
    return(result$gidb_id[1])
  }

  # UPDATE existing game
  query <- glue::glue_sql(
    "UPDATE games SET
       mobygames_id = {sql_null(identifiers$moby_id)}, igdb_id = {sql_null(identifiers$id_igdb)},
       steam_id = {sql_null(identifiers$steam_id)}, gog_id = {sql_null(identifiers$gog_id)},
       game_title = {sql_null(info$title)}, game_year = {sql_null(release_year)}, game_url = {sql_null(info$official_url)},
       modified_by = {user_id}, modified_at = now()
     WHERE gidb_id = {gidb_id}",
    .con = conn
  )
  DBI::dbExecute(conn, query)
  gidb_id
}

# R/db_write_maps.R

#' Resolve each tag to an id, reshape to a df, bulk-write the join table.
#' @noRd
db_write_game_tags <- function(pool, conn, gidb_id, game_tags) {
  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "DELETE FROM tags_games_map WHERE gidb_id = {gidb_id}",
      .con = conn
    )
  )

  game_tags <- purrr::keep(game_tags, \(t) nzchar(t$name %||% ""))
  if (length(game_tags) == 0) {
    return(invisible(NULL))
  }

  tag_ids <- purrr::map_int(game_tags, function(t) {
    category_id <- db_resolve_category_id(pool, t$category)
    db_resolve_tag_id(pool, t$name, category_id)
  })

  map_df <- data.frame(tag_id = tag_ids, gidb_id = gidb_id)
  map_df <- map_df[!is.na(map_df$tag_id), ]

  # Remove duplicates (same tag_id + gidb_id combination)
  if (nrow(map_df) > 0) {
    map_df <- map_df[!duplicated(map_df[c("tag_id", "gidb_id")]), ]
  }

  if (nrow(map_df) == 0) {
    return(invisible(NULL))
  }

  DBI::dbWriteTable(
    conn,
    "tags_games_map",
    map_df,
    append = TRUE,
    row.names = FALSE
  )
  invisible(NULL)
}

#' Resolve each originator + role to ids, reshape to a df, bulk-write the join table.
#' NOTE: location intentionally not written yet (see earlier caveat re: geonames_id).
#' @noRd
db_write_game_originators <- function(pool, conn, gidb_id, originators) {
  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "DELETE FROM originators_games_map WHERE gidb_id = {gidb_id}",
      .con = conn
    )
  )

  originators <- purrr::keep(originators, \(o) nzchar(o$name %||% ""))
  if (length(originators) == 0) {
    return(invisible(NULL))
  }

  map_df <- purrr::map_dfr(originators, function(o) {
    data.frame(
      originator_id = db_resolve_originator_id(pool, o$name),
      gidb_id = gidb_id,
      originator_role = db_resolve_originator_role_id(pool, o$role)
    )
  })
  map_df <- map_df[!is.na(map_df$originator_id), ]

  # Remove duplicates (same originator_id + gidb_id combination)
  if (nrow(map_df) > 0) {
    map_df <- map_df[!duplicated(map_df[c("originator_id", "gidb_id")]), ]
  }

  if (nrow(map_df) == 0) {
    return(invisible(NULL))
  }

  DBI::dbWriteTable(
    conn,
    "originators_games_map",
    map_df,
    append = TRUE,
    row.names = FALSE
  )
  invisible(NULL)
}


#' @noRd
db_write_game_notes <- function(conn, gidb_id, notes, user_id) {
  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "DELETE FROM notes USING notes_games_map
       WHERE notes.note_id = notes_games_map.note_id AND notes_games_map.gidb_id = {gidb_id}",
      .con = conn
    )
  )

  notes <- notes[nzchar(notes %||% character(0))]
  if (length(notes) == 0) {
    return(invisible(NULL))
  }

  values_clause <- glue::glue_sql_collapse(
    glue::glue_sql(
      "({notes}, {user_id}, {user_id}, now(), now())",
      .con = conn
    ),
    sep = ", "
  )

  query <- glue::glue_sql(
    "INSERT INTO notes (note, created_by, modified_by, created_at, modified_at)
     VALUES {values_clause} RETURNING note_id",
    .con = conn
  )
  note_ids <- DBI::dbGetQuery(conn, query)$note_id

  map_df <- data.frame(note_id = note_ids, gidb_id = gidb_id)
  DBI::dbWriteTable(
    conn,
    "notes_games_map",
    map_df,
    append = TRUE,
    row.names = FALSE
  )
  invisible(NULL)
}

#' @noRd
db_save_game <- function(pool, game_data, user_id, gidb_id = NULL) {
  # Validate user_id
  if (is.null(user_id) || is.na(user_id) || !nzchar(as.character(user_id))) {
    stop("user_id must be provided and cannot be NULL or NA")
  }

  # If no gidb_id provided, try to find existing game by identifiers
  if (is.null(gidb_id) || is.na(gidb_id)) {
    gidb_id <- db_find_game_by_identifiers(pool, game_data$identifiers)
  }

  # Determine if we're inserting or updating
  should_insert <- is.null(gidb_id) || is.na(gidb_id)

  pool::poolWithTransaction(pool, function(conn) {
    gid <- db_write_game(
      conn = conn,
      info = game_data$info,
      identifiers = game_data$identifiers,
      user_id = user_id,
      gidb_id = if (should_insert) NULL else gidb_id
    )

    db_write_game_tags(pool, conn, gid, game_data$game_tags)
    db_write_game_originators(pool, conn, gid, game_data$originators)
    db_write_game_notes(conn, gid, game_data$notes, user_id)
    gid
  })
}

#' Delete a game and its mappings from the database
#' Does NOT delete related entities (tags, originators, roles, categories) as they may be used by other games.
#'
#' @param pool Database pool connection.
#' @param gidb_id The gidb_id to delete.
#' @return TRUE if deletion was successful, FALSE otherwise.
#' @noRd
db_delete_game <- function(pool, gidb_id) {
  tryCatch({
    pool::poolWithTransaction(pool, function(conn) {
      # Delete notes_games_map entries
      DBI::dbExecute(
        conn,
        glue::glue_sql(
          "DELETE FROM notes_games_map WHERE gidb_id = {gidb_id}",
          .con = conn
        )
      )

      # Delete tags_games_map entries
      DBI::dbExecute(
        conn,
        glue::glue_sql(
          "DELETE FROM tags_games_map WHERE gidb_id = {gidb_id}",
          .con = conn
        )
      )

      # Delete originators_games_map entries
      DBI::dbExecute(
        conn,
        glue::glue_sql(
          "DELETE FROM originators_games_map WHERE gidb_id = {gidb_id}",
          .con = conn
        )
      )

      # Finally, delete the game itself
      DBI::dbExecute(
        conn,
        glue::glue_sql(
          "DELETE FROM games WHERE gidb_id = {gidb_id}",
          .con = conn
        )
      )

      TRUE
    })
  }, error = function(e) {
    FALSE
  })
}
