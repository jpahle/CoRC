source("copasi_wrapper.R")

filename <- "examples/brusselator.cps"

message("File: ", filename)

dataModel <- cop$bind$CCopasiRootContainer_addDatamodel()
invisible(cop$bind$CCopasiDataModel_loadModel(dataModel,filename))
model <- cop$bind$CCopasiDataModel_getModel(dataModel)

nMetab <- cop$bind$MetabVector_size(cop$bind$CModel_getMetabolites(model))
message("Number of Metabolites: " , nMetab)

filename <- "example1.cps"

message("File: ", filename)

model2 <- cop$loadModel(filename)
nMetab2 <- cop$bind$MetabVector_size(cop$bind$CModel_getMetabolites(model2))
message("Number of Metabolites: " , nMetab2)
