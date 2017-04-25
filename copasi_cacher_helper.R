## Load the COPASI swig package and save the R session to enable the use of this session as a cache for the COPASI.R file.

library(methods)

source("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI.R")

enumToInteger <- function(name,type) {
    if (is.character(name)) {
        gettype <- paste(".__E__", type, sep = "")
        if (!exists(gettype)) gettype <- paste(".__E__", substr(type, 3, nchar(type)), sep = "")
        ans <- as.integer(get(gettype)[name])
        if (is.na(ans)) {warning("enum not found ", name, " ", type)}
        ans
    } 
}

enumFromInteger <- function(i,type) {
    gettype <- paste(".__E__", type, sep = "")
    if (!exists(gettype)) gettype <- paste(".__E__", substr(type, 3, nchar(type)), sep = "")
    itemlist <- get(gettype)
    names(itemlist)[match(i, itemlist)]
}

save(list = ls(all.names = TRUE), file = "copasi_cache.RData")
