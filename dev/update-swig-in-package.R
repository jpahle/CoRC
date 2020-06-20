# Use this if you recompile COPASI.
# It takes the swig files from the libs folder in copasi-source and puts them into the package.
# It also patches the swig wrapper as per patch-swig-wrapper.R

pkgname <- "CoRC"
buildpath <- file.path("copasi-source", "libs")
rfile <- file.path(buildpath, "swig_wrapper.R")
binfile <- file.path(buildpath, paste0("COPASI", .Platform$dynlib.ext))

if (!dir.exists(buildpath))
  stop("Missing swig builds. Did you compile COPASI?")

# Copy the swig wrapper (COPASI.R)
stopifnot(
  file.copy(rfile, file.path("..", "R", "swig_wrapper.R"), overwrite = TRUE)
)

# apply the wrapper patches
source("patch-swig-wrapper.R", chdir = TRUE)

libpath <- file.path("..", "libs")

if (.Platform$OS.type == "windows")
  libpath <- file.path(libpath, "x64")

if (!dir.exists(libpath))
  dir.create(libpath, recursive = TRUE)

# Copy the binaries (COPASI.xx)
stopifnot(
  file.copy(binfile, file.path(libpath, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)
)

instlibpath <- file.path("..", "inst", "libs")

if (.Platform$OS.type == "windows")
  instlibpath <- file.path(instlibpath, "x64")

if (!dir.exists(instlibpath))
  dir.create(instlibpath, recursive = TRUE)

# Copy the binaries (COPASI.xx)
stopifnot(
  file.copy(binfile, file.path(instlibpath, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)
)
