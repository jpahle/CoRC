## Simple helper functions

library(pryr)
library(assertthat)

ls_d <- function(namex = "") {
    return(ls(all.names = TRUE, pattern = namex, envir = .GlobalEnv))
}
