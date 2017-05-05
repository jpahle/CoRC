rm(list = ls(all.names = T))
source("copasi_funs.R")

filename <- "examples/brusselator_1.cps"

message("File: ", filename)

dataModel <- CRootContainer_addDatamodel()
invisible(dataModel$loadModel(filename))
model <- dataModel$getModel()

nMetab <- model$getMetabolites()$size()
message("Number of Metabolites: " , nMetab)

rm(list = ls(all.names = T))
library(rCopasi)

filename <- "examples/brusselator_1.cps"

message("File: ", filename)

model2 <- loadModel(filename)
nMetab2 <- model2$getModel()$getMetabolites()$size()
message("Number of Metabolites: " , nMetab2)
