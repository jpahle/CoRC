if (!file.exists("copasi_cache.RData")) {
    system2("Rscript", c("--vanilla", "--default-packages=methods", "copasi_cacher_helper.R"))
}