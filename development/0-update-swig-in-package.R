# Use this if you recompile Copasi.
# It takes the swig files from the build dir and deletes the copasi cache file.

buildpath <- file.path("development", "copasi-source", "build_copasi_r_bindings", "copasi", "bindings", "R")

if (!dir.exists(buildpath)) stop("Missing swig builds. Did you compile copasi?")

stopifnot(
    file.copy(file.path(buildpath, "COPASI.R"), file.path("R", "swig_wrapper.R"), overwrite = TRUE)
)

libpath <- file.path("inst", "libs")

if (!dir.exists(libpath)) dir.create(libpath, recursive = TRUE)

stopifnot(
    file.copy(file.path(buildpath, paste0("COPASI", .Platform$dynlib.ext)), file.path(libpath, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)
)

rm(buildpath, libpath)
