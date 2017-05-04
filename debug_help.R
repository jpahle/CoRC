## Simple helper functions

library(pryr, warn.conflicts = FALSE)
library(assertthat, warn.conflicts = FALSE)

ls_d <- function(namex = "") {
    namex <- substitute(namex)
    if (!is.string(namex)) namex <- deparse(namex)

    return(ls(all.names = TRUE, pattern = namex, envir = .GlobalEnv))
}
