# Use this if you recompile Copasi.
# It takes the swig files from the build dir and deletes the copasi cache file.

buildpath <- file.path("copasi-source", "build_copasi_r_bindings", "copasi", "bindings", "R")

if (!dir.exists(buildpath)) stop("Missing swig builds. Did you compile copasi?")

# Copy the swig wrapper (COPASI.R)
stopifnot(
    file.copy(file.path(buildpath, "COPASI.R"), file.path("..", "R", "swig_wrapper.R"), overwrite = TRUE)
)

libpath <- file.path("..", "inst", "libs")

if (!dir.exists(libpath)) dir.create(libpath, recursive = TRUE)

# Copy the binaries (COPASI.SO)
stopifnot(
    file.copy(file.path(buildpath, paste0("COPASI", .Platform$dynlib.ext)), file.path(libpath, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)
)
# Also copy binaries to the installed package
if (system.file(package = "CoRC") != "")
  stopifnot(
      file.copy(file.path(buildpath, paste0("COPASI", .Platform$dynlib.ext)), system.file("libs", paste0("COPASI", .Platform$dynlib.ext), package = "CoRC"), overwrite = TRUE)
  )

rm(buildpath, libpath)
