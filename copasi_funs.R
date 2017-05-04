## Load the COPASI swig package and initialize API.
source("debug_help.R")
library(purrr, warn.conflicts = FALSE)

source("copasi_cacher.R")
load("copasi_cache.RData")

# eg. example9 only works with cacheMetaData
cacheMetaData(1)

dyn.load(paste0("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI", .Platform$dynlib.ext))

# Using any function with the datamodel parameter set will redefine the currently active datamodel
API_CURRENT_DATAMODEL <- NULL

# Format function for the CDataModel class which is used as a basis for the print method
format._p_CDataModel <- function(x, ...) {
    model <- x$getModel()
    string <- ""
    string <- paste0(string, 'Model name: "' , model$getObjectName() , '"\n')
    print(x@ref)
    # string <- paste0(string, '@ref is set to: "' , datamodel@ref , '"\n')
    n <- model$getCompartments()$size()
    string <- paste0(string, "Number of Compartments: " , n, "\n")
    n <- model$getMetabolites()$size()
    string <- paste0(string, "Number of Metabolites: " , n, "\n")
    n <- model$getReactions()$size()
    paste0(string, "Number of Reactions: " , n, "\n")
}

setMethod("print",
          "_p_CDataModel",
          function(x, ...) {
              cat(format(x, ...), "\n")
          }
)

# Works like seq_along for CDataVectors (0 based index)
seq_along_cv <- function(copasivector) {0L:(copasivector$size() - 1)}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_from_cv <- function(copasivector, index) {
    type <- is(copasivector)[1]
    
    # exise the items class from the classname of the vector
    type <- paste0("_p_", stringr::str_sub(type, 17L, -3L))
    
    # typecasting the result
    as(copasivector$get(index), type)
}

# Checks whether the datamodel parameter is valid
confirmDatamodel <- function(datamodel) {
    success <- is(datamodel, "_p_CDataModel")
    
    if (success) API_CURRENT_DATAMODEL <- datamodel
    
    success
}

getCurrentModel <- function() {
    assert_that(!is_null(API_CURRENT_DATAMODEL), msg = "No model currently in use.")
    
    API_CURRENT_DATAMODEL
}

loadModel <- function(filename) {
    assert_that(is.readable(filename))
    
    datamodel <- CRootContainer_addDatamodel()
    success <- datamodel$loadModel(filename)
    
    if (!success) {
        CRootContainer_removeDatamodel(datamodel)
        stop("Couldn't load model file.")
    }
    
    API_CURRENT_DATAMODEL <<- datamodel
    datamodel
}

saveCPS <- function(filename, overwrite = FALSE, datamodel = API_CURRENT_DATAMODEL) {
    assert_that(is.string(filename))
    
    if (!has_extension(filename, "cps")) {
        filename <- paste0(filename, ".cps")
    }
    
    success <- datamodel$saveModel(filename, overwriteFile = overwrite)
    
    assert_that(success, msg = paste0("Model failed to save at: ", filename))
    
    invisible(success)
}

unloadModel <- function(datamodel) {
    assert_that(confirmDatamodel(datamodel))
    
    datamodel <- CRootContainer_removeDatamodel(datamodel)
}

getSpecies <- function(datamodel = API_CURRENT_DATAMODEL) {
    assert_that(confirmDatamodel(datamodel))
    
    metabs <- datamodel$getModel()$getMetabolites()
    
    # assemble output dataframe
    seq_along_cv(metabs) %>%
        map_df(function(x) {
            metab <- get_from_cv(metabs, x)
            list(
                key = metab$getKey(),
                name = metab$getObjectName(),
                concentration = metab$getInitialConcentration()
            )
        })
}

setSpecies <- function(species, datamodel = API_CURRENT_DATAMODEL) {
    assert_that(confirmDatamodel(datamodel), is(species, "tbl_df"), all(!is.na(species$key), is_character(species$name), is_double(species$concentration)))
    
    metabs <- datamodel$getModel()$getMetabolites()
    
    # assemble dataframe with the models species
    metab_df <-
        tibble::tibble(
            object = seq_along_cv(metabs) %>% map(~ get_from_cv(metabs, .x))
        ) %>%
        dplyr::mutate(key = object %>% map_chr(~ .x$getKey()))
    
    # add an id column to species, so I dont lose the sorting order
    species <- species %>% tibble::add_column(id = seq_len(nrow(species)) - 1)
    
    # join both dataframes but only accept rows with key that exists in the model
    metab_df <-
        metab_df %>%
        dplyr::left_join(species, by = "key")
    
    # apply values to all species
    metab_df %>%
        invoke_rows(.f = function(name, concentration, object, ...) {
            if (!is.na(name)) object$setObjectName(name)
            if (!is.na(concentration)) object$setInitialConcentration(concentration)
        })
    
    # if all species were given, accept the new sorting order by reshuffeling the copasi vector
    if (all(!is.na(metab_df$id))) {
        metab_df %>%
            invoke_rows(.f = function(id, object, ...) {
                old_id <- metabs$getIndex(object)
                metabs$swap(id, old_id)
            })
    }
    
    # apparently I need to give changedObjects because I cant update initial values without
    # for now it just adds all species
    changedObjects <- ObjectStdVector()
    metab_df$object %>% walk(~ changedObjects$push_back(.x$getObject(CCommonName("Reference=InitialConcentration"))))
    
    datamodel$getModel()$updateInitialValues(changedObjects)
    
    # datamodel$getModel()$compileIfNecessary()
    
    # datamodel$getModel()$initializeMetabolites()
    
    invisible()
}

runTimecourse <- function(duration, steps = 1000, initialtime = datamodel$getModel()$getInitialTime(), datamodel = API_CURRENT_DATAMODEL) {
    assert_that(confirmDatamodel(datamodel))
    
    trajectoryTask <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
    assert_that(!is_null(trajectoryTask))
    
    trajectoryTask$setMethodType("deterministic")
    
    problem <- as(trajectoryTask$getProblem(), "_p_CTrajectoryProblem")
    
    # Not sure if this is needed
    problem$setModel(datamodel$getModel())
    
    problem$setDuration(duration)
    problem$setStepNumber(steps)
    datamodel$getModel()$setInitialTime(initialtime)
    problem$setTimeSeriesRequested(TRUE)
    
    result <- trajectoryTask$process(TRUE)
    timeSeries <- trajectoryTask$getTimeSeries()
    
    # assemble output dataframe
    1L:timeSeries$getNumVariables() %>%
        map(function(i_var) {
            1L:timeSeries$getRecordedSteps() %>%
                map_dbl(function(i_step) {
                    timeSeries$getConcentrationData(i_step - 1L, i_var - 1L)
                }) %>%
                list() %>%
                magrittr::set_names(timeSeries$getTitle(i_var - 1L))
        }) %>%
        dplyr::bind_cols()
}
