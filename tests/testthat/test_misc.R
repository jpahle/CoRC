context("Miscellaneous Tests")

is_url_readable <- function(url = "http://www.google.com") {
  # test connection by trying to read first line of url
  test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
  
  # return FALSE if test inherits 'try-error' class
  !inherits(test, "try-error")
}

# test if online
is_online <- function() {
  # test the http capabilities of the current R build
  if (!capabilities(what = "http/ftp"))
    return(FALSE)
  
  is_url_readable()
}

test_that("getVersion() is correct", {
  v <- getVersion()
  expect_type(v, "integer")
  expect_length(v, 3)
  expect_identical(v[["major"]], 4L)
  expect_identical(v[["minor"]], 28L)
  expect_identical(v[["build"]], 226L)
})

test_that("COPASI_BIN_VERSION is count", {
  expect_type(COPASI_BIN_VERSION, "integer")
  expect_length(COPASI_BIN_VERSION, 1L)
  expect_gt(COPASI_BIN_VERSION, 0L)
})

test_that("COPASI_BIN_HASHES are valid hashes", {
  hashes <- unlist(COPASI_BIN_HASHES)
  # for now expect 3 entries
  expect_length(hashes, 3L)
  expect_match(hashes, "^[A-Fa-f0-9]{64}$")
})

test_that("libs on server are accessible", {
  skip_if_not(is_online())
  
  for (arch in names(COPASI_BIN_HASHES)) {
    for (os in names(COPASI_BIN_HASHES[[arch]])) {
      if (os == "windows")
        ext <- ".dll"
      else
        ext <- ".so"
        
      expect_true(
        is_url_readable(dl_url_former_pahle(os = os, arch = arch, ext = ext)),
        paste0("Cannot access file for os '", os, "' and arch '", arch, "' on pahle server.")
      )
      expect_true(
        is_url_readable(dl_url_former_github(os = os, arch = arch, ext = ext)),
        paste0("Cannot access file for os '", os, "' and arch '", arch, "' oh github server.")
      )
    }
  }
})
