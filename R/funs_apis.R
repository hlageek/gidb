get_geonames_place <- function(geonames_id, username) {
  httr2::request("http://api.geonames.org/getJSON") |>
    httr2::req_url_query(
      geonameId = geonames_id,
      username = username
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# 3067696

# place <- get_geonames_place(3067696, "pgps")
# place$name
# place$countryName
# place$countryCode

# credentials register at dev.twitch.tv, create an app, get  client_id and client_secret.
get_igdb_token <- function(client_id, client_secret) {
  httr2::request("https://id.twitch.tv/oauth2/token") |>
    httr2::req_url_query(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = "client_credentials"
    ) |>
    httr2::req_method("POST") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# token <- get_igdb_token("your_client_id", "your_client_secret")
# access_token <- token$access_token
get_igdb_game <- function(igdb_id, client_id, access_token) {
  httr2::request("https://api.igdb.com/v4/games") |>
    httr2::req_headers(
      `Client-ID` = client_id,
      `Authorization` = paste("Bearer", access_token)
    ) |>
    httr2::req_body_raw(
      paste0(
        "fields name, first_release_date, genres, themes, involved_companies, url; where id = ",
        igdb_id,
        ";"
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# game <- get_igdb_game(902, "t6hs7slsu8b07ejiebhr2updw1sqs5", access_token)
#game
