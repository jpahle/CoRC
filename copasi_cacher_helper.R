source("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI.R")

enumToInteger <- function(name,type) {
    if (is.character(name)) {
        ans <- as.integer(get0(paste0(".__E__", type))[name])
        if (length(ans) == 0) {ans <- as.integer(get(paste0(".__E__", substr(type, 3, nchar(type))))[name])}
        if (is.na(ans)) {warning("enum not found ", name, " ", type)}
        ans
    }
}

enumFromInteger <- function(i,type) {
    itemlist <- get0(paste0(".__E__", type))
    if (length(itemlist) == 0) {itemlist <- get(paste0(".__E__", substr(type, 3, nchar(type))))}
    names(itemlist)[match(i, itemlist)]
}

save(list = ls(all.names = TRUE), file = "copasi_cache.RData", envir = .GlobalEnv)
