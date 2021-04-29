#' Install COPASI libraries
#'
#' \code{getCopasi} downloads COPASI libraries or retrieves them from given path.
#' 
#' This function is deprecated as COPASI libraries will now always be included during package installation.
#' 
#' No replacement function is needed.
#' @name getCopasi-deprecated
#' @family deprecated
#' @keywords internal
NULL

#' @rdname getCopasi-deprecated
#' @export
getCopasi <- function(...) {
  .Deprecated(msg = paste0("'", getPackageName(), "::getCopasi' is deprecated.\nLibraries are now installed on package installation. No replacement function is needed.\nSee help(\"Deprecated\")"))
  invisible()
}
