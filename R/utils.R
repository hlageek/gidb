#' Strip HTML tags from a string.
#' @noRd
strip_html <- function(x) {
  if (is.null(x) || nchar(x) == 0) {
    return("")
  }
  gsub("\\s+", " ", trimws(gsub("<[^>]+>", " ", x)))
}
