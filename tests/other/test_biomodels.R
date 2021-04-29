# This file will test loading all biomodels in sequence.
# It will also test saving and running time courses.
# A summary is printed in the end.

options(warn = 1)
library(purrr)
library(dplyr)
library(CoRC)

max_biomodels <- 1000

message("getting biomodels... this may take a while")
if (file.exists("bmdls.rds")) {
  biomodels <- readRDS("bmdls.rds")
} else {
  dl_bmdl <- function(id, url) {
    message(id)
    tryCatch(CoRC:::con_to_string(url), error = function(e) "")
  }
  
  biomodels <-
    tibble(
      id = 1:max_biomodels,
      url = map_chr(id, biomodels_url, format = "sbml"),
      sbml_string = map2_chr(id, url, dl_bmdl),
      exists = sbml_string != "",
      loadable = NA,
      taskable = NA,
      cop_string = NA_character_
    )

  saveRDS(biomodels, "bmdls.rds")
}

message("Available Biomodels: ", sum(biomodels$exists))

message("Testing model loading...")
for (id in biomodels$id) {
  if (!biomodels$exists[id])
    next()
  message(id)
  
  mdl <- try(loadSBMLFromString(biomodels$sbml_string[id]))
  
  if (is(mdl, "try-error")) {
    biomodels$loadable[id] <- FALSE
    next()
  }
  
  biomodels$loadable[id] <- TRUE
  unloadModel(model = mdl)
  clearCustomKineticFunctions()
}

message("Testing model saving...")
for (id in biomodels$id) {
  if (!isTRUE(biomodels$loadable[id]))
    next()
  message(id)
  
  mdl <- suppressWarnings(loadSBMLFromString(biomodels$sbml_string[id]))
  
  biomodels$cop_string[id] <- saveModelToString(model = mdl)
  unloadModel(model = mdl)
  clearCustomKineticFunctions()
}

message("Testing model time courses...")
for (id in biomodels$id) {
  if (!isTRUE(biomodels$loadable[id]))
    next()
  message(id)
  
  mdl <- suppressWarnings(loadSBMLFromString(biomodels$sbml_string[id]))
  
  tc <- try(runTimeCourse(duration = 1e-6, intervals = 1, method = "deterministic", model = mdl))

  biomodels$taskable[id] <- !is(tc, "try-error")
  unloadModel(model = mdl)
  clearCustomKineticFunctions()
}

message(
  "unloadable: ",
  biomodels$loadable %>%
    `!`() %>%
    which() %>%
    paste0(collapse = ", ")
)

message(
  "unsavable: ",
  biomodels %>%
    filter(loadable) %>%
    .$cop_string %>%
    grepl("^<\\?xml.*<COPASI.*</COPASI>(\n)?$", .) %>%
    `!`() %>%
    which() %>%
    paste0(collapse = ", ")
)

message(
  "cant do time course: ",
  biomodels$taskable %>%
    `!`() %>%
    which() %>%
    paste0(collapse = ", ")
)
