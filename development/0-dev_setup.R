library(purrr)
library(ggplot2)
.data <- rlang::.data
library(assertthat)
library(devtools)
load_all()

setCurrentModel(
  loadExamples()[[2]]
)

datamodel <- getCurrentModel()
setTimeCourseSettings(duration = 100, dt = 1)
# devtools::document(); devtools::install()

ls_corc <- function(pattern) {ls(pattern = pattern, name = "package:CoRC", all.names = TRUE)}
