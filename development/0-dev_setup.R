library(tidyverse)
library(assertthat)
library(devtools)
load_all()

setCurrentModel(
  loadExamples()[[2]]
)

# devtools::document(); devtools::install()
