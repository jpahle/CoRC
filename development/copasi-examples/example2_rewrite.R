if (!exists("CRootContainer_getRoot")) devtools::load_all()

stopifnot(!is.null(CRootContainer_getRoot()))
# create a datamodel
dataModel <- CRootContainer_addDatamodel()
stopifnot(CRootContainer_getDatamodelList()$size() == 1)
# the only argument to the main routine should be the name of a CPS file
args <- "development/copasi-examples/brusselator.cps"
if (length(args) == 1) {
    filename <- args[1]
    # load the model without progress report
   
    # I have no clue how exception handling in R works
    tryCatch(dataModel$loadModel(filename), error = function(e) {
      write(paste("Error while loading the model from file named \"" , filename , "\"."), stderr())
      quit(save = "default", status = 1, runLast = TRUE)
    } )

    model <- dataModel$getModel()
    stopifnot(!is.null(model))
    cat('Model statistics for model "' , model$getObjectName() , '".\n', sep="")

    # output number and names of all compartments
    iMax <- model$getCompartments()$size()
    cat("Number of Compartments: " , iMax, "\n", sep="")
    cat("Compartments: \n")
    i <- 0
    while ( i < iMax) {
        compartment <- model$getCompartment(i)
        stopifnot(!is.null(compartment))
        cat("    " , compartment$getObjectName(), "\n", sep = "")
        i <- i + 1
    }
    # output number and names of all metabolites
    iMax <- model$getMetabolites()$size()
    cat("Number of Metabolites: " , iMax, "\n", sep = "")
    cat("Metabolites: \n")
    i <- 0
    while (i < iMax) {
        metab <- model$getMetabolite(i)
        stopifnot(!is.null(metab))
        cat("    " , metab$getObjectName(), "\n", sep="")
        i <- i + 1
    }
    # output number and names of all reactions
    iMax <- model$getReactions()$size()
    cat("Number of Reactions: " , iMax, "\n", sep = "")
    cat("Reactions: \n")
    i <- 0
    while ( i < iMax) {
        reaction <- model$getReaction(i)
        stopifnot(!is.null(reaction))
        cat("    " , reaction$getObjectName() , "\n", sep="")
        i <- i + 1
    }
} else {
    write( "Usage: example2 CPSFILE", stderr())
    quit(save = "default", status = 1, runLast = TRUE)
}


