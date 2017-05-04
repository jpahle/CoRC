rm(list = ls(all.names = T))
source("copasi_wrapper.R")

filename <- "examples/brusselator_1.cps"

message("File: ", filename)

dataModel <- cop$bind$CRootContainer_addDatamodel()
invisible(dataModel$loadModel(filename))
model <- dataModel$getModel()

nMetab <- model$getMetabolites()$size()
message("Number of Metabolites: " , nMetab)

rm(list = ls(all.names = T))
source("copasi_wrapper.R")

filename <- "examples/brusselator_1.cps"

message("File: ", filename)

model2 <- cop$loadModel(filename)
nMetab2 <- model2$getMetabolites()$size()
message("Number of Metabolites: " , nMetab2)
