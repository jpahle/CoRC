PKG_VERSION <- "0.0.0"
COPASI_BIN_VERSION <- 21L
COPASI_BIN_HASHES <- list(
  x86_64 = c(
    darwin  = "b0f4043302bd59b65d21e94505e1b61e907457af318cb45896a40a4cfbb04e38",
    linux   = "4c385b21276ee2e1184ba16bac8e8e613a3839b86bb1897f3a030e6e2be8383c",
    windows = "f173b2e0567c31d9f0830e171af88843a568617ced0c770d7c993619fd80a119"
  )
)

PKG_VERSION <- package_version(PKG_VERSION)

COPASI_BIN_BASE_URL_PAHLE <- "http://juergen.pahle.de/CoRC_libs/"
dl_url_former_pahle <- function(base = COPASI_BIN_BASE_URL_PAHLE, version = COPASI_BIN_VERSION, os, arch, ext = .Platform$dynlib.ext) {
  paste0(
    base,
    "v", version, "/",
    "COPASI_", os,
    "_", arch,
    ext
  )
}

COPASI_BIN_BASE_URL_GITHUB <- "https://github.com/jpahle/CoRC/"
dl_url_former_github <- function(base = COPASI_BIN_BASE_URL_GITHUB, version = PKG_VERSION, os, arch, ext = .Platform$dynlib.ext) {
  paste0(
    base,
    "releases/download/",
    "v", version$major, ".", version$minor, ".", version$patchlevel, "/",
    "COPASI_", os,
    "_", arch,
    ext
  )
}
