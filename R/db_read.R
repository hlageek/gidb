#' List all games as a named vector of gidb_ids
#'
#' Useful as `selectInput()` choices: the values are `gidb_id`s, the names
#' are the game titles.
#'
#' @param pool Database pool connection.
#' @return A named integer vector (names = `game_title`), sorted by title.
#'   Empty vector when the games table is empty.
#' @noRd
db_list_games <- function(pool) {
  # COLLATE: the database is initialised with the C collation, which compares
  # byte by byte -- uppercase, digits and punctuation would all come before
  # lowercase, and accented letters after z. The ICU collation compares the way
  # the alphabet reads instead (Postgres >= 12). Without ICU, lower(game_title)
  # is the fallback; it fixes the case, not the accents.
  result <- DBI::dbGetQuery(
    pool,
    'SELECT gidb_id, game_title
     FROM games
     ORDER BY game_title COLLATE "und-x-icu", gidb_id'
  )

  ids <- result$gidb_id
  names(ids) <- paste0(result$game_title, " (GIDB: ", ids, ")")
  ids
}

#' Read a single game field from the database
#'
#' @param conn Database pool connection.
#' @param table,column Table and column to read from.
#' @param gidb_id Row to read.
#' @return The stored value (length-1 vector; `NA` when the column is SQL
#'   `NULL`), or `NULL` when no row matches.
#' @noRd
db_read_game_item <- function(
  conn,
  table,
  column,
  gidb_id = NULL
) {
  # glue_sql() cannot interpolate NULL; nothing to read without an id anyway.
  if (is.null(gidb_id) || length(gidb_id) != 1 || is.na(gidb_id)) {
    return(NULL)
  }

  query <- glue::glue_sql(
    "SELECT {`column`}
       FROM {`table`}
      WHERE gidb_id = {gidb_id}",
    .con = conn
  )

  result <- DBI::dbGetQuery(conn, query)

  # SQL returns a single column, whatever it is called.
  if (nrow(result) == 0) NULL else result[[1]][1]
}
