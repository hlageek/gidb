#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "gidb")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  # Modify this if your config file is somewhere else
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

#' Get media base path for file uploads
#'
#' Returns the configured base path for media files set via run_app().
#' If not set, returns NULL indicating the app directory should be used.
#'
#' @return Character string with base path, or NULL if not configured
#' @noRd
get_media_base_path <- function() {
  val <- golem::get_golem_options(which = "media_base_path")
  if (!is.null(val) && nzchar(trimws(as.character(val)))) {
    return(path.expand(as.character(val)))
  }
  NULL
}

#' Construct full media path from relative path
#'
#' @param rel_path Relative path within media directory
#' @return Full absolute path
#' @noRd
media_path <- function(rel_path) {
  base <- get_media_base_path()
  if (is.null(base)) {
    base <- app_sys("")
  }
  file.path(base, rel_path)
}
