# THIS FILE DOESNT SEEM TO GET CALLED
# buildpath <- file.path(R_PACKAGE_SOURCE, "..", "copasi-dev", "build_copasi_r_bindings", "copasi", "bindings", "R")
# stop("buildpath is ", buildpath)
#
# libdest <- file.path(R_PACKAGE_DIR, paste0('libs', R_ARCH))
#
# dir.create(libdest, recursive = TRUE, showWarnings = FALSE)
#
# file.copy(file.path(buildpath, "COPASI.R"), file.path(R_PACKAGE_SOURCE, "R", "swig_wrapper.R"), overwrite = TRUE)
# file.copy(file.path(buildpath, paste0("COPASI", SHLIB_EXT)), file.path(libdest, paste0("COPASI", SHLIB_EXT)), overwrite = TRUE)
