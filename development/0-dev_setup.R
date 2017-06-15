library(purrr)
autoplot <- ggplot2::autoplot
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
