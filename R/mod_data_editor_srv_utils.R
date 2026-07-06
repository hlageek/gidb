default_game_data <- function() {
  list(
    info = list(
      title = NULL,
      release_year = NULL,
      description = NULL,
      official_url = NULL
    ),
    identifiers = list(
      moby_id = NULL
    ),
    game_tags = list(
      list(name = NULL, category = NULL)
    ),
    platforms = list(
      list(name = NULL, year = NULL)
    ),
    originators = list(
      list(
        name = NULL,
        role = NULL,
        location = NULL
      )
    ),
    notes = NULL,
    mobygames_called = NULL
  )
}

save_game_data <- function(
  pool = NULL,
  game_data = NULL,
  user_id = NULL,
  game_id = NULL
) {
  db_save_game(
    pool = pool,
    game_data = game_data,
    user_id = user_id,
    game_id = game_id
  )
  {
    db_write_game(
      conn = pool,
      info = game_data$info,
      identifiers = game_data$identifiers,
      user_id = user_id,
      game_id = game_id
    )
  }
  db_write_game_tags(pool = pool, game_data = game_data)
  db_write_game_platforms(pool = pool, game_data = game_data)
  db_write_game_originators(pool = pool, game_data = game_data)
  db_write_game_notes(pool = pool, game_data = game_data)
}
