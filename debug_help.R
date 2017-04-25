## Simple helper functions

library(pryr, warn.conflicts = FALSE)
library(assertthat, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

ls_d <- function(namex = "") {
    return(ls(all.names = TRUE, pattern = namex, envir = .GlobalEnv))
}
