# R/db_resolve.R

#' @noRd
db_resolve_category_id <- function(
  pool,
  category_value,
  category_source = "MobyGames"
) {
  if (is.null(category_value) || !nzchar(category_value)) {
    return(NA_integer_)
  }
  id <- db_get_category_id(pool, category_value, category_source)
  if (is.na(id)) {
    id <- db_create_category(pool, category_value, category_source)
  }
  id
}

#' @noRd
db_resolve_tag_id <- function(pool, tag_value, category_id = NA_integer_) {
  if (is.null(tag_value) || !nzchar(tag_value)) {
    return(NA_integer_)
  }
  id <- db_get_tag_id(pool, tag_value)
  if (is.na(id)) {
    id <- db_create_tag(pool, tag_value, category_id)
  }
  id
}

#' @noRd
db_resolve_originator_role_id <- function(pool, role_name) {
  if (is.null(role_name) || !nzchar(role_name)) {
    return(NA_integer_)
  }
  id <- db_get_originator_role_id(pool, role_name)
  if (is.na(id)) {
    id <- db_create_originator_role(pool, role_name)
  }
  id
}

#' @noRd
db_resolve_originator_id <- function(
  pool,
  originator_name,
  originator_mobygames_id = NA
) {
  if (is.null(originator_name) || !nzchar(originator_name)) {
    return(NA_integer_)
  }
  id <- if (!is.na(originator_mobygames_id)) {
    db_get_originator_id_by_mobygames(pool, originator_mobygames_id)
  } else {
    db_get_originator_id_by_name(pool, originator_name)
  }
  if (is.na(id)) {
    id <- db_create_originator(pool, originator_name, originator_mobygames_id)
  }
  id
}


# R/db_lookups.R

#' @noRd
db_get_category_id <- function(pool, category_value, category_source) {
  query <- glue::glue_sql(
    "SELECT category_id FROM categories WHERE category_value = {category_value} AND category_source = {category_source}",
    .con = pool
  )
  result <- DBI::dbGetQuery(pool, query)
  if (nrow(result) == 0) NA_integer_ else result$category_id[1]
}

#' @noRd
db_get_tag_id <- function(pool, tag_value) {
  query <- glue::glue_sql(
    "SELECT tag_id FROM tags WHERE tag_value = {tag_value}",
    .con = pool
  )
  result <- DBI::dbGetQuery(pool, query)
  if (nrow(result) == 0) NA_integer_ else result$tag_id[1]
}

#' @noRd
db_get_originator_role_id <- function(pool, role_name) {
  query <- glue::glue_sql(
    "SELECT originator_role_id FROM originator_roles WHERE originator_role = {role_name}",
    .con = pool
  )
  result <- DBI::dbGetQuery(pool, query)
  if (nrow(result) == 0) NA_integer_ else result$originator_role_id[1]
}

#' @noRd
db_get_originator_id_by_mobygames <- function(pool, mobygames_id) {
  query <- glue::glue_sql(
    "SELECT originator_id FROM originators WHERE originator_mobygames_id = {mobygames_id}",
    .con = pool
  )
  result <- DBI::dbGetQuery(pool, query)
  if (nrow(result) == 0) NA_integer_ else result$originator_id[1]
}

#' @noRd
db_get_originator_id_by_name <- function(pool, originator_name) {
  query <- glue::glue_sql(
    "SELECT originator_id FROM originators WHERE originator_name = {originator_name}",
    .con = pool
  )
  result <- DBI::dbGetQuery(pool, query)
  if (nrow(result) == 0) NA_integer_ else result$originator_id[1]
}
