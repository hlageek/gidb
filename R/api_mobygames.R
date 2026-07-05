# ============================================================
# MobyGames API v2 — Game Metadata Retrieval
# ============================================================
# For use inside a golem package — all functions fully namespaced.
# Add to DESCRIPTION Imports: httr2, purrr
#
# All data is fetched in TWO requests:
#   1. GET /v2/games?id={id}&include=genres,platforms,moby_score,description
#   2. GET /v2/games?id={id}&include=developers,publishers
#
# Splitting into two calls because the API silently drops unknown
# include fields — keeping them separate makes failures easier to debug.
#
# v2 field names (differ from v1):
#   genres[]:   category, category_id, id, name
#   platforms[]: name, platform_id, release_date
#   developers[]/publishers[]: id, name, url, platforms (list of name strings)
#
# Hobbyist tier available include fields:
#   covers, description, developers, game_id, genres, moby_score, moby_url,
#   official_url, platforms, publishers, release_date, screenshots, title
# identifiers (Steam, wiki etc.) require Silver tier.
# Company location is not available via the public API.
# ============================================================

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

.MOBY_BASE <- "https://api.mobygames.com/v2"


# ------------------------------------------------------------
# Internal: base request factory
# ------------------------------------------------------------

.moby_req <- function(endpoint, ...) {
  api_key <- Sys.getenv("MOBYGAMES_APIKEY")
  if (!nzchar(api_key)) {
    stop(
      "MOBYGAMES_APIKEY not found. ",
      "Add it to your .Renviron: usethis::edit_r_environ()"
    )
  }

  httr2::request(.MOBY_BASE) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(api_key = api_key, ...) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent("ShinyGameDB/0.1") |>
    httr2::req_throttle(rate = 1) |>
    httr2::req_retry(max_tries = 3, backoff = ~5)
}


# ------------------------------------------------------------
# Internal: perform request with consistent error handling
# ------------------------------------------------------------

.moby_perform <- function(req) {
  resp <- req |>
    httr2::req_error(body = function(r) {
      tryCatch(httr2::resp_body_json(r)$error, error = function(e) NULL)
    }) |>
    httr2::req_perform()

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}


# ------------------------------------------------------------
# Internal: normalise a MobyGames category name to a snake_case key
#
# Examples:
#   "Basic Genres"          -> "basic_genres"
#   "Interface/Control"     -> "interface_control"
#   "Narrative Theme/Topic" -> "narrative_theme_topic"
# ------------------------------------------------------------

.normalise_bucket <- function(category) {
  x <- tolower(category)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}


# ------------------------------------------------------------
# Internal: parse genres into named tag buckets
#
# v2 genre fields: category, category_id, id, name
#
# Buckets are derived dynamically from whatever `category` values
# MobyGames returns — no hardcoded list, so new categories are
# picked up automatically.
#
# Returns a named list of buckets, each a list of list(id, name).
# ------------------------------------------------------------

.parse_tags <- function(genres_list) {
  buckets <- list()

  for (g in genres_list) {
    bucket <- .normalise_bucket(g$category %||% "other")
    entry <- list(id = g$id, name = g$name)
    buckets[[bucket]] <- c(buckets[[bucket]], list(entry))
  }

  buckets
}


# ------------------------------------------------------------
# Internal: parse platforms list
#
# v2 platform fields: name, platform_id, release_date ("YYYY-MM-DD")
#
# Returns a list of list(platform_id, platform_name, release_year)
# ------------------------------------------------------------

.parse_platforms <- function(platforms_list) {
  purrr::map(platforms_list, function(p) {
    raw_date <- p$release_date %||% NA_character_
    year <- if (!is.na(raw_date)) substr(raw_date, 1, 4) else NA_character_

    list(
      platform_id = p$platform_id,
      platform_name = p$name,
      release_year = year
    )
  })
}


# ------------------------------------------------------------
# Internal: parse developers and publishers into a unified
# originators list with a `role` field.
#
# v2 company fields: id, name, url, platforms (list of name strings)
#
# Returns a list of list(company_id, company_name, role, moby_url, platforms)
# ------------------------------------------------------------

.parse_originators <- function(developers, publishers) {
  parse_one <- function(company, role) {
    list(
      company_id = company$id,
      company_name = company$name,
      role = role,
      moby_url = company$url,
      platforms = unlist(company$platforms %||% list())
    )
  }

  devs <- purrr::map(developers %||% list(), parse_one, role = "developer")
  pubs <- purrr::map(publishers %||% list(), parse_one, role = "publisher")

  c(devs, pubs)
}


# ============================================================
# moby_get_game_metadata()
#
# Fetches a complete metadata record for one game using two API calls:
#   call 1 — core fields: description, genres, platforms, moby_score
#   call 2 — originators: developers, publishers
#
# @param game_id             Integer or character MobyGames game ID
# @param include_originators Logical. Set FALSE to skip the second API
#                            call when dev/pub data is not needed.
#
# @return Named list:
#   $moby_id      integer
#   $moby_title   character
#   $moby_url     character
#   $official_url character (may be NULL)
#   $release_date character YYYY-MM-DD of earliest release (may be NULL)
#   $description  character (HTML, may be NULL)
#   $moby_score   numeric   (may be NULL)
#   $tags         list of buckets, each a list of list(id, name);
#                   bucket names are snake_case of MobyGames category,
#                   e.g. basic_genres, perspective, gameplay, setting ...
#   $platforms    list of list(platform_id, platform_name, release_year)
#   $originators  list of list(company_id, company_name, role, moby_url, platforms)
#                   role is "developer" or "publisher"
#                   platforms is a character vector of platform names
#   NOTE: identifiers (Steam ID, wiki etc.) require Silver API tier.
# ============================================================

moby_get_game_metadata <- function(game_id, include_originators = TRUE) {
  message("Fetching core metadata for game_id: ", game_id)

  # --- Call 1: core fields (all Hobbyist-tier fields) ---
  core_result <- .moby_req(
    "games",
    id = game_id,
    include = "genres,platforms,moby_score,description,official_url,release_date"
  ) |>
    .moby_perform()

  if (length(core_result$games) == 0) {
    stop("No game found for game_id: ", game_id)
  }

  game <- core_result$games[[1]]

  tags <- .parse_tags(game$genres %||% list())
  platforms <- .parse_platforms(game$platforms %||% list())

  # --- Call 2: originators ---
  originators <- list()
  if (include_originators) {
    message("Fetching originators for game_id: ", game_id)
    Sys.sleep(1)

    orig_result <- .moby_req(
      "games",
      id = game_id,
      include = "developers,publishers"
    ) |>
      .moby_perform()

    orig_game <- orig_result$games[[1]]
    originators <- .parse_originators(
      orig_game$developers,
      orig_game$publishers
    )
  }

  res <- list(
    moby_id = game$game_id,
    moby_title = game$title,
    moby_url = game$moby_url,
    official_url = game$official_url,
    release_date = game$release_date,
    description = game$description,
    moby_score = game$moby_score,
    tags = tags,
    platforms = platforms,
    originators = originators
  )
}


# ============================================================
# moby_print_metadata()
# Pretty-print for interactive dev / testing.
# ============================================================

moby_print_metadata <- function(meta) {
  cat("=== MobyGames Metadata ===\n")
  cat("ID:    ", meta$moby_id, "\n")
  cat("Title: ", meta$moby_title, "\n")
  cat("URL:   ", meta$moby_url, "\n")
  cat("Score: ", meta$moby_score %||% "N/A", "\n")
  cat("Release:", meta$release_date %||% "N/A", "\n")
  cat("URL:   ", meta$official_url %||% "N/A", "\n\n")

  cat("--- Tags ---\n")
  for (bucket in names(meta$tags)) {
    entries <- meta$tags[[bucket]]
    if (length(entries) == 0) {
      next
    }
    labels <- paste(purrr::map_chr(entries, "name"), collapse = ", ")
    cat(sprintf("  %-14s %s\n", paste0(bucket, ":"), labels))
  }

  cat("\n--- Platforms ---\n")
  for (p in meta$platforms) {
    cat(sprintf(
      "  [%d] %-30s %s\n",
      p$platform_id,
      p$platform_name,
      p$release_year %||% "?"
    ))
  }

  cat("\n--- Originators ---\n")
  if (length(meta$originators) == 0) {
    cat("  (not fetched or none found)\n")
  } else {
    for (o in meta$originators) {
      plat_str <- if (length(o$platforms) > 0) {
        paste0(" [", paste(o$platforms, collapse = ", "), "]")
      } else {
        ""
      }
      cat(sprintf(
        "  [%d] %-35s (%s)%s\n",
        o$company_id,
        o$company_name,
        o$role,
        plat_str
      ))
    }
  }

  invisible(meta)
}
