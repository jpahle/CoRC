#' @title String matching engines from stringr
#'
#' @details
#' CoRC makes use of the string matching engines from the stringr package.
#' 
#' By default matching is done with \code{coll}.
#' @name regex
NULL

#' @export
#' @rdname regex
stringr::fixed

#' @export
#' @rdname regex
stringr::coll

#' @export
#' @rdname regex
stringr::regex

# apply an engine for a character vector
# do nothing if engine is already applied
apply_eng <- function(x, engine = coll) {
  if (!inherits(x, c("fixed", "coll", "regex")))
    x <- engine(x)
  x
}

# Subsetting engines (x[1]) usually clears all attributes
# This method prevents that
subset_eng <- function(x, i) {
  attrs <- attributes(x)
  x <- x[i]
  attributes(x) <- attrs
  x
}
