## If no cache exists, run a seperate R session to generate it.

if (!file.exists("copasi_cache.RData")) {
    system2("Rscript", c("--vanilla", "copasi_cacher_helper.R"))
}