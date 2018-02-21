# This file will test loading all biomodels in sequence.
# It will also test saving and running time courses.
# A summary is printed in the end.

options(warn = 1)
library(tidyverse)
library(CoRC)

max_biomodels <- 700

if (file.exists("bmdls.rds")) {
  biomodels <- readRDS("bmdls.rds")
} else {
  biomodels <-
    tibble(
      id = 1:max_biomodels,
      url = paste0("http://www.ebi.ac.uk/biomodels-main/download?mid=BIOMD0000000", sprintf("%03d", id)),
      sbml_string =
        url %>%
        map(possibly(readLines, character(), quiet = TRUE)) %>%
        map_chr(paste0, collapse = "\n"),
      exists = sbml_string != "",
      loadable = rep_along(id, NA),
      taskable = rep_along(id, NA),
      cop_string = rep_along(id, NA_character_)
    )

  saveRDS(biomodels, "bmdls.rds")
}

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

for (id in biomodels$id) {
  if (!isTRUE(biomodels$loadable[id]))
    next()
  message(id)
  
  mdl <- suppressWarnings(loadSBMLFromString(biomodels$sbml_string[id]))
  
  biomodels$cop_string[id] <- saveModelToString(model = mdl)
  unloadModel(model = mdl)
  clearCustomKineticFunctions()
}

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
    "!"() %>%
    which() %>%
    paste0(collapse = ", ")
)

message(
  "unsavable: ",
  biomodels %>%
    filter(loadable) %>%
    .$cop_string %>%
    grepl("^<\\?xml.*<COPASI.*</COPASI>(\n)?$", .) %>%
    "!"() %>%
    which() %>%
    paste0(collapse = ", ")
)

message(
  "cant do time course: ",
  biomodels$taskable %>%
    "!"() %>%
    which() %>%
    paste0(collapse = ", ")
)
