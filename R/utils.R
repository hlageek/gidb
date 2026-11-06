#' Strip HTML tags from a string.
#' @noRd
strip_html <- function(x) {
  if (is.null(x) || nchar(x) == 0) {
    return("")
  }
  gsub("\\s+", " ", trimws(gsub("<[^>]+>", " ", x)))
}

sql_null <- function(x) {
  if (is.null(x) || is.na(x) || x == "" || length(x) == 0) {
    DBI::SQL("NULL")
  } else {
    x
  }
}

#' Update an internal count observer.
#' @noRd
update_observer <- function(observer) {
  observer <- observer + 1
  return(observer)
}
