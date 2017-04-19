rm(list = ls(all.names = T))
source("copasi_funs.R")

filename <- "examples/brusselator.cps"

message("File: ", filename)

dataModel <- CCopasiRootContainer_addDatamodel()
invisible(CCopasiDataModel_loadModel(dataModel,filename))
model <- CCopasiDataModel_getModel(dataModel)

nMetab <- MetabVector_size(CModel_getMetabolites(model))
message("Number of Metabolites: " , nMetab)

rm(list = ls(all.names = T))
source("copasi_funs.R")

filename <- "examples/brusselator.cps"

message("File: ", filename)

model2 <- loadModel(filename)
nMetab2 <- MetabVector_size(CModel_getMetabolites(model2))
message("Number of Metabolites: " , nMetab2)
