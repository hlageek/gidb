#' Default game data structure for new entries
#' @noRd
default_game_data <- function() {
  list(
    info = list(
      title = NULL,
      release_year = NULL,
      # description not in schema yet
      official_url = NULL
    ),
    identifiers = list(
      moby_id = NULL,
      id_igdb = NULL,
      steam_id = NULL,
      gog_id = NULL
      # itch_id not in schema yet
    ),
    game_tags = list(),
    # platforms table doesn't exist yet
    originators = list(),
    notes = character(0),
    mobygames_called = FALSE
  )
}

#' Save game data to the database
#' Auto-detects whether to insert or update based on existing identifiers.
#'
#' @param pool Database pool connection.
#' @param game_data Game data reactive values.
#' @param user_id User ID performing the save.
#' @param gidb_id Optional existing gidb_id (if NULL, will auto-detect).
#' @return The gidb_id of the saved/updated game.
#' @noRd
save_game_data <- function(pool, game_data, user_id, gidb_id = NULL) {
  db_save_game(
    pool = pool,
    game_data = game_data,
    user_id = user_id,
    gidb_id = gidb_id
  )
}

#' Load game data from the database
#'
#' @param pool Database pool connection.
#' @param gidb_id The gidb_id to load.
#' @return A list with the same structure as default_game_data(), populated with DB data.
#' @noRd
load_game_data <- function(pool, gidb_id) {
  # Validate gidb_id first
  if (is.null(gidb_id) || is.na(gidb_id)) {
    return(NULL)
  }

  # Get core game info (only columns that exist in schema)
  query <- glue::glue_sql(
    "SELECT mobygames_id, igdb_id, steam_id, gog_id, game_title, game_year, game_url
     FROM games WHERE gidb_id = {gidb_id}",
    .con = pool
  )
  game <- DBI::dbGetQuery(pool, query)
  if (nrow(game) == 0) {
    return(NULL)
  }

  result <- default_game_data()

  # Core info
  result$info$title <- game$game_title[1]
  result$info$release_year <- game$game_year[1]
  result$info$official_url <- game$game_url[1]

  # Identifiers - convert empty strings and NA to NULL
  result$identifiers$moby_id <- if (is.null(game$mobygames_id[1]) || is.na(game$mobygames_id[1]) || game$mobygames_id[1] == "") NULL else game$mobygames_id[1]
  result$identifiers$id_igdb <- if (is.null(game$igdb_id[1]) || is.na(game$igdb_id[1]) || game$igdb_id[1] == "") NULL else game$igdb_id[1]
  result$identifiers$steam_id <- if (is.null(game$steam_id[1]) || is.na(game$steam_id[1]) || game$steam_id[1] == "") NULL else game$steam_id[1]
  result$identifiers$gog_id <- if (is.null(game$gog_id[1]) || is.na(game$gog_id[1]) || game$gog_id[1] == "") NULL else game$gog_id[1]

  # Tags - each section has its own error handling
  result$game_tags <- tryCatch({
    tags_df <- db_read_game_tags(pool, gidb_id)
    if (nrow(tags_df) > 0) {
      purrr::pmap(tags_df, function(tag_value, category_value) {
        list(name = tag_value, category = category_value)
      })
    } else {
      list()
    }
  }, error = function(e) {
    list()
  })

  # Platforms table doesn't exist in schema yet - skip

  # Originators
  result$originators <- tryCatch({
    originators_df <- db_read_game_originators(pool, gidb_id)
    if (nrow(originators_df) > 0) {
      purrr::pmap(originators_df, function(originator_name, originator_role) {
        list(name = originator_name, role = originator_role, location = NULL)
      })
    } else {
      list()
    }
  }, error = function(e) {
    list()
  })

  # Notes - need to query separately
  result$notes <- tryCatch({
    notes_query <- glue::glue_sql(
      "SELECT n.note FROM notes n
       JOIN notes_games_map ngm ON n.note_id = ngm.note_id
       WHERE ngm.gidb_id = {gidb_id}",
      .con = pool
    )
    notes_result <- DBI::dbGetQuery(pool, notes_query)
    if (nrow(notes_result) > 0) {
      notes_result$note
    } else {
      character(0)
    }
  }, error = function(e) {
    character(0)
  })

  result
}
