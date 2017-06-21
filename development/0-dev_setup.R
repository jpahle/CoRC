# devtools::document(); devtools::install()

library(purrr)
library(ggplot2)
.data <- rlang::.data
library(assertthat)
devtools::load_all()

setCurrentModel(
  loadExamples()[[2]]
)

datamodel <- getCurrentModel()
setTimeCourseSettings(duration = 100, dt = 1)

ls_corc <- function(pattern) {ls(pattern = pattern, name = "package:CoRC", all.names = TRUE)}
