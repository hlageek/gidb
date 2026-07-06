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
  originator_mobygames_id = NA,
  originator_igdb_id = NA,
  originator_description = NA
) {
  query <- glue::glue_sql(
    "INSERT INTO originators (originator_mobygames_id, originator_igdb_id, originator_name, originator_description)
     VALUES ({originator_mobygames_id}, {originator_igdb_id}, {originator_name}, {originator_description})
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
  game_id = NULL
) {
  release_year <- suppressWarnings(as.integer(info$release_year))

  if (is.null(game_id)) {
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
     {'draft'},
     {user_id},
     now(),
     now()
   ) RETURNING game_id",
      .con = conn
    )
    return(DBI::dbGetQuery(conn, query)$game_id[1])
  }

  query <- glue::glue_sql(
    "UPDATE games SET
       mobygames_id = {identifiers$moby_id}, igdb_id = {identifiers$id_igdb},
       steam_id = {identifiers$steam_id}, gog_id = {identifiers$gog_id},
       game_title = {info$title}, game_year = {release_year}, game_url = {info$official_url},
       modified_by = {user_id}, modified_at = now()
     WHERE game_id = {game_id}",
    .con = conn
  )
  DBI::dbExecute(conn, query)
  game_id
}

# R/db_write_maps.R

#' Resolve each tag to an id, reshape to a df, bulk-write the join table.
#' @noRd
db_write_game_tags <- function(pool, conn, game_id, game_tags) {
  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "DELETE FROM tags_games_map WHERE game_id = {game_id}",
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

  map_df <- data.frame(tag_id = tag_ids, game_id = game_id)
  map_df <- map_df[!is.na(map_df$tag_id), ]
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
db_write_game_originators <- function(pool, conn, game_id, originators) {
  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "DELETE FROM originators_games_map WHERE game_id = {game_id}",
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
      game_id = game_id,
      originator_role = db_resolve_originator_role_id(pool, o$role)
    )
  })
  map_df <- map_df[!is.na(map_df$originator_id), ]
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
db_write_game_notes <- function(conn, game_id, notes, user_id) {
  DBI::dbExecute(
    conn,
    glue::glue_sql(
      "DELETE FROM notes USING notes_games_map
       WHERE notes.note_id = notes_games_map.note_id AND notes_games_map.game_id = {game_id}",
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

  map_df <- data.frame(note_id = note_ids, game_id = game_id)
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
db_save_game <- function(pool, game_data, user_id, game_id = NULL) {
  browser()

  pool::poolWithTransaction(pool, function(conn) {
    gid <- db_write_game(
      conn = pool,
      info = game_data$info,
      identifiers = game_data$identifiers,
      user_id = user_id,
      game_id = game_id
    )
    db_write_game_tags(pool, conn, gid, game_data$game_tags)
    db_write_game_originators(pool, conn, gid, game_data$originators)
    db_write_game_notes(conn, gid, game_data$notes, user_id)
    gid
  })
}
