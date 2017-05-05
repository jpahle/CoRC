# Use this if you recompile Copasi.
# It takes the swig files from the build dir and deletes the copasi cache file.

buildpath <- file.path("copasi-dev", "build_copasi_r_bindings", "copasi", "bindings", "R")
pkgpath <- file.path("rCopasi")
cachepath <- "copasi_cache.RData"

# enable use from rCopasi project too
if (!dir.exists(buildpath)) {
    buildpath <- file.path("..", buildpath)
    pkgpath <- file.path("..", pkgpath)
    cachepath <- file.path("..", cachepath)
}

if (!dir.exists(buildpath)) stop("Missing swig builds. Did you compile copasi?")

stopifnot(
    file.copy(file.path(buildpath, "COPASI.R"), file.path(pkgpath, "R", "swig_wrapper.R"), overwrite = TRUE)
)

libpath <- file.path(pkgpath, "inst", "libs")
if (!dir.exists(libpath)) dir.create(libpath, recursive = TRUE)
stopifnot(
    file.copy(file.path(buildpath, paste0("COPASI", .Platform$dynlib.ext)), file.path(libpath, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)
)

if (file.exists(cachepath)) file.remove(cachepath)

rm(buildpath, pkgpath, cachepath, libpath)
