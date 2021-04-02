
# R_LIBS <- strsplit(Sys.getenv("R_LIBS"), ";", fixed = TRUE)[[1]]
R_PACKAGE_DIR <- Sys.getenv("R_PACKAGE_DIR")
# R_OSTYPE <- Sys.getenv("R_OSTYPE")
# R_ARCH <- Sys.getenv("R_ARCH")
R_ARCH_BIN <- Sys.getenv("R_ARCH_BIN")
COPASI_LIB_PATH <- Sys.getenv("COPASI_LIB_PATH")
# R_PACKAGE_NAME <- Sys.getenv("R_PACKAGE_NAME")

os <- tolower(Sys.info()["sysname"])
arch <- R.Version()$arch

print0 <- function(...) cat(..., "\n", sep = "")
print_env <- function(env) print0(env, " = ", Sys.getenv(env))

print0("platform = ", os, "_", arch)

# print0(R_LIBS)
# print_env("R_PACKAGE_DIR")
# print_env("R_OSTYPE")
# print_env("R_ARCH")
# print_env("R_ARCH_BIN")
print_env("COPASI_LIB_PATH")
# print_env("R_PACKAGE_NAME")
# print0("os = ", os)
# print0("arch = ", arch)

# print(Sys.getenv())

# R_ARCH
# windows64 = /x64
# ubunut64 = 
# maxos64 = 

# R_ARCH_BIN
# windows64 = /x64
# ubunut64 = 
# maxos64 = 

# R_OSTYPE
# windows64 = windows
# ubunut64 = unix
# maxos64 = unix

# Sys.info()["sysname"]
# windows64 = Windows
# ubunut64 = linux
# maxos64 = darwin

# R.Version()$arch
# windows64 = x86_64
# ubunut64 = x86_64
# maxos64 = x86_64

assert_that <- function(...) invisible(assertthat::assert_that(...))
pluck <- purrr::pluck
partial <- purrr::partial
possibly <- purrr::possibly
quietly <- purrr::quietly
digest <- partial(digest::digest, algo = "sha256", file = TRUE)

# read in lib metadata and url tools
source(file = "tools/lib_metadata.R", local = TRUE)

libsdir <- file.path(R_PACKAGE_DIR, "libs", R_ARCH_BIN)

assert_that(dir.create(libsdir, recursive = TRUE))

libfile <- file.path(libsdir, paste0("COPASI", .Platform$dynlib.ext))
lib_hash <- pluck(COPASI_BIN_HASHES, arch, os)

# if no path is given we download the COAPSI libraries
if (is.null(COPASI_LIB_PATH) || COPASI_LIB_PATH == "" || is.na(COPASI_LIB_PATH)) {
  assert_that(
    !is.null(lib_hash),
    msg = paste0("There are currently no COAPSI libraries available for your platform (", os, "_", arch, ").")
  )
  
  # Don't do anything if the current lib file is good.
  if (file.exists(libfile)) {
    current_file_hash <- digest(libfile)
    if (current_file_hash == lib_hash) {
      cat("Keeping present COPASI libraries.\n")
      return(invisible())
    }
  }
  
  cat("Downloading COPASI libraries for your system.\n")
  
  assert_that(
    capabilities(what = "http/ftp"),
    msg = paste0(
      "R doesn't have internet capabilities. Can't download COPASI libraries for your platform (",
      os, "_", arch,
      "). Consider providing libraries via `configure.vars` 'COPASI_LIB_PATH'."
    )
  )
  
  # this was used in some examples. I am not sure whether I need this.
  if (os == "windows" && getRversion() < "3.3.0")
    setInternet2()
  
  dlfun <- quietly(possibly(utils::download.file, otherwise = 1))
  
  dlurl <- dl_url_former_pahle(os = os, arch = arch)
  
  # download the binaries from pahle url first
  dlresult_pahle <- dlfun(url = dlurl, destfile = libfile, method = "auto", quiet = FALSE, mode = "wb")
  dlsuccess_pahle <- dlresult_pahle$result == 0
  
  dl_is_valid <- FALSE
  
  if (dlsuccess_pahle)
    dl_is_valid <- digest(libfile) == lib_hash
  
  if (!dl_is_valid) {
    dlurl <- dl_url_former_github(os = os, arch = arch)
    
    # download the binaries from github
    dlresult_github <- dlfun(url = dlurl, destfile = libfile, method = "auto", quiet = FALSE, mode = "wb")
    dlsuccess_github <- dlresult_github$result == 0
    
    if (dlsuccess_github)
      dl_is_valid <- digest(libfile) == lib_hash
  }
  
  if (!dlsuccess_pahle && !dlsuccess_github) {
    warning(dlresult_pahle$warnings)
    warning(dlresult_github$warnings)
    stop("Downloading the COPASI libraries failed. Your platform is ", os, "_", arch, ". Consider providing libraries via `configure.vars` 'COPASI_LIB_PATH'.")
  }
  
  # Check if the hash matches
  assert_that(
    dl_is_valid,
    msg = "Downloaded COPASI libraries are corrupted."
  )
} else {
  cat("Including externally supplied COPASI libraries.\n")
  if (!is.null(lib_hash) && lib_hash != digest(COPASI_LIB_PATH))
    warning("The externally supplied COPASI libraries do not conform to expected libaries. Please ensure that the correct libraries were supplied. Your platform is ", os, "_", arch, ".")
    
  assert_that(file.copy(COPASI_LIB_PATH, libfile, overwrite = TRUE))
}
