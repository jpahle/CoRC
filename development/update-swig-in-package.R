# Use this if you recompile Copasi.
# It takes the swig files from the build dir and deletes the copasi cache file.

buildpath <- file.path("copasi-source", "build_copasi_r_bindings", "copasi", "bindings", "R")
rfile <- file.path(buildpath, "COPASI.R")
binfile <- file.path(buildpath, paste0("COPASI", .Platform$dynlib.ext))

if (!dir.exists(buildpath))
  stop("Missing swig builds. Did you compile copasi?")

# Copy the swig wrapper (COPASI.R)
stopifnot(
  file.copy(rfile, file.path("..", "R", "swig_wrapper.R"), overwrite = TRUE)
)

libpath <- file.path("..", "inst", "libs")

if (!dir.exists(libpath))
  dir.create(libpath, recursive = TRUE)

# Copy the binaries (COPASI.xx)
stopifnot(
  file.copy(binfile, file.path(libpath, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)
)

libpath <- file.path("..", "inst", "libs", "x64")

if (!dir.exists(libpath))
  dir.create(libpath, recursive = TRUE)

# Copy the binaries (COPASI.xx)
stopifnot(
  file.copy(binfile, file.path(libpath, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)
)

# Also copy binaries to the installed package
rlibpath <- system.file("libs", paste0("COPASI", .Platform$dynlib.ext), package = "CoRC")
if (rlibpath != "")
  stopifnot(
    file.copy(binfile, rlibpath, overwrite = TRUE)
  )

rlibpath <- system.file("libs", "x64", paste0("COPASI", .Platform$dynlib.ext), package = "CoRC")
if (rlibpath != "")
  stopifnot(
    file.copy(binfile, rlibpath, overwrite = TRUE)
  )

rm(buildpath, rfile, binfile, libpath, rlibpath)
