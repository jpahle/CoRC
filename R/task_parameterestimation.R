#' Run parameter estimation
#'
#' \code{runParamEst} runs parameter estimation and returns results in a list.
#'
#' @param calculateJacobian boolean
#' @param performStabilityAnalysis boolean
#' @param calculateStatistics boolean
#' @param updateModel boolean
#' @param method  character or list
#' @param datamodel a model object
#' @return a list of results
#' @export
runParamEst <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, parameters = NULL, experiments = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  # use the worker function to apply all given arguments
  # the worker function returns all args needed to restore previous settings
  restorationCall <- pe_settings_worker(
    .type = "temporary",
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    parameters = parameters,
    experiments = experiments,
    method = method,
    datamodel = datamodel
  )
  
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  
  success <- grab_msg(task$initializeRaw(OUTPUTFLAG))
  if (success)
    success <- grab_msg(task$processRaw(TRUE))
  if (success)
    ret <- try(pe_result_worker(datamodel))
  
  # Call the worker again to restore previous settings.
  do.call(pe_settings_worker, restorationCall)
  
  # Assertions only after restoration of settings
  assert_that(
    success,
    msg = paste0("Processing the task failed.")
  )
  assert_that(
    !is.error(ret),
    msg = paste0("Result readout failed.")
  )
  
  ret
}

#' Set parameter estimation settings
#'
#' \code{setParamEstSettings} sets parameter estimation settings including method options.
#'
#' @param randomizeStartValues boolean
#' @param createParameterSets boolean
#' @param calculateStatistics boolean
#' @param updateModel boolean
#' @param executable boolean
#' @param method character or list
#' @param datamodel a model object
#' @export
setParamEstSettings <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, executable = NULL, parameters = NULL, experiments = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.null(executable) || is.flag(executable) && !is.na(executable))
  
  # Call the worker to set most settings
  pe_settings_worker(
    .type = "permanent",
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    parameters = parameters,
    experiments = experiments,
    method = method,
    datamodel = datamodel
  )
  
  if (!is.null(executable)) {
    datamodel$getTask("Parameter Estimation")$setScheduled(executable)
  }
  
  invisible()
}

#' @export
defineParameter <- function(key = NULL, lower.bound = 1e-6, upper.bound = 1e6, start.value = (lower.bound + upper.bound) / 2) {
  assert_that(
    is.string(key),
    is.number(lower.bound),
    is.number(upper.bound), lower.bound <= upper.bound,
    is.number(start.value), start.value >= lower.bound, start.value <= upper.bound
  )
  
  list(
    key = key,
    lower = lower.bound,
    upper = upper.bound,
    start = start.value
  )
}

#' @export
addParameter <- function(param_struct, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  fititem <- problem$addFitItem(CCommonName(param_struct$key))
  fititem$setLowerBound(CCommonName(as.character(param_struct$lower)))
  fititem$setUpperBound(CCommonName(as.character(param_struct$upper)))
  fititem$setStartValue(param_struct$start)
}

#' @export
clearParameters <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  seq_len_0(problem$getOptItemSize()) %>% walk(~ problem$removeOptItem(0))
}

#' @export
defineExperiments <- function(experiment_type = c("Time Course", "Steady State"), data = NULL, types = NULL, mappings = NULL, weights = NULL, filename = NULL) {
  experiment_type <- rlang::arg_match(experiment_type, c("Steady State", "Time Course"))
  experiment_type <- c("Steady State" = "steadyState", "Time Course" = "timeCourse")[experiment_type]
  
  if (is.data.frame(data)) data <- list(data)
  assert_that(every(data, is.data.frame))

  experiment_names <- names(data) %||% paste0("Experiment_", seq_along(data))
  experiment_names[is.na(experiment_names)] <- "Experiment"
  experiment_lengths <- map_int(data, nrow)
  experiment_lastrows <- cumsum(experiment_lengths)
  
  data <- dplyr::bind_rows(data)
  data_cols <- names(data)
  
  assert_that(
    is.character(types),
    is.null(names(types)) && length(types) == length(data_cols) ||
    all(names(mappings) %in% data_cols)
  )
  
  types <- stringr::str_to_lower(types)
  names(types) <- names(types) %||% data_cols
  types %>% map_chr(function(types) {rlang::arg_match(types, c("time", "independent", "dependent", "ignore"))})
  types <- types[types != "ignore"]
  
  assert_that(
    !(experiment_type == "timeCourse" && length(which(types == "time")) != 1L),
    msg = 'Time course experiements need exactly one "time" mapping.'
  )
  assert_that(
    !(experiment_type == "steadyState" && any(types == "time")),
    msg = 'Steady state experiements cannot have a "time" mapping.'
  )
  
  mappings <- mappings %||% data_cols
  assert_that(
    is.character(mappings),
    is.null(names(mappings)) && length(mappings) == length(data_cols) ||
    all(names(mappings) %in% data_cols)
  )
  
  names(mappings) <- names(mappings) %||% data_cols
  
  mappings[names(types)[types %in% c("time", "ignore")]] <- ""
  
  if (!is.null(filename)) {
    assert_that(is.string(filename) && !is.na(filename))
    if (!has_extension(filename, ".txt")) filename <- paste0(filename, ".txt")
  }
  
  ret <- list()
  
  ret$experiment_type <- experiment_type
  
  ret$data <- data
  
  ret$experiments <-
    tibble::tibble(
      name = experiment_names,
      first_row = c(1L, head(experiment_lastrows + 1L, -1L)),
      last_row = experiment_lastrows
    )
  
  ret$types <- types
  
  ret$mappings <- mappings
  
  ret$weights <- weights
  
  ret$filename <- filename
  
  ret
}

#' @export
addExperiments <- function(exp_struct, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  experiment_set <- problem$getExperimentSet()
  
  # Create experiment file
  model_dir <- normalizePathC(datamodel$getReferenceDirectory())
  filepath <- file.path(model_dir, exp_struct$filename)
  readr::write_tsv(exp_struct$data, filepath)
  
  # Construct individual experiments
  experiments <-
    exp_struct$experiments %>%
    pmap(function(name, first_row, last_row, ...) {
      exp <- CExperiment(experiment_set, name)
      exp$setFirstRow(first_row)
      exp$setLastRow(last_row)
      exp
    })
  
  col_count <- ncol(exp_struct$data)
  col_names <- names(exp_struct$data)

  # Set all experiment's settings
  walk_swig(experiments, "setHeaderRow", 1)
  walk_swig(experiments, "setFileName", exp_struct$filename)
  walk_swig(experiments, "setExperimentType", exp_struct$experiment_type)
  walk_swig(experiments, "setNumColumns", col_count)
  # walk_swig(experiments, "readColumnNames")
  
  # Get the object maps and assign roles and mappings
  object_maps <- map_swig(experiments, "getObjectMap")
  walk_swig(object_maps, "setNumCols", col_count)
  
  # Transfer values from named vectors to a full representation of all data columns
  types <- rep("ignore", col_count)
  types[match(names(exp_struct$types), col_names)] <- exp_struct$types
  mappings <- rep("", col_count)
  mappings[match(names(exp_struct$mappings), col_names)] <- exp_struct$mappings
  
  types %>% iwalk(~ walk_swig(object_maps, "setRole", .y - 1L, .x))
  mappings %>% iwalk(~ walk_swig(object_maps, "setObjectCN", .y - 1L, .x))
  
  # Add all experiments to copasi
  experiments %>% walk(~ experiment_set$addExperiment(.x))
  
  # possibly compile
  # experiment_set$compile(problem$getMathContainer())
}

#' @export
clearExperiments <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  experiment_set <- problem$getExperimentSet()
  
  seq_len_0(experiment_set$getExperimentCount()) %>% walk(~ experiment_set$removeExperiment(0))
}

# .type can help in some situations to determine assertions etc
# can be "temporary", "permanent" or "restore"
pe_settings_worker <- function(.type, randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, parameters = NULL, experiments = NULL, method = NULL, datamodel) {
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  assert_that(
    is.null(randomizeStartValues)   || is.flag(randomizeStartValues) && !is.na(randomizeStartValues),
    is.null(createParameterSets)    || is.flag(createParameterSets)  && !is.na(createParameterSets),
    is.null(calculateStatistics)    || is.flag(calculateStatistics)  && !is.na(calculateStatistics),
    is.null(updateModel)            || is.flag(updateModel)          && !is.na(updateModel),
    is.null(parameters)             || is.list(parameters),
    is.null(experiments)            || is.list(experiments),
    is.null(method)                 || is.string(method)             && !is.na(method) || is.list(method) && is.string(method$method) && !is.na(method$method)
  )
  
  if (!is_null(parameters) && .type != "restore") {
    assert_that(
      problem$getOptItemSize() == 0L,
      msg = "This function can not set parameters if there are already parameters set in copasi."
    )
    
    if (has_name(parameters, "key"))
      parameters <- list(parameters)
    
    parameters %>% walk(~
      assert_that(
        all(names(.x) %in% c("key", "lower", "upper", "start")),
        msg = "Invalid parameters submitted."
      )
    )
  }
    
  if (!is_null(experiments) && .type != "restore") {
    assert_that(
      problem$getExperimentSet()$getExperimentCount() == 0L,
      msg = "This function can not set experiments if there are already experiments set in copasi."
    )
    
    if (has_name(experiments, "experiment_type"))
      experiments <- list(experiments)
    
    experiments %>% walk(~
      assert_that(
        all(names(.x) %in% c("experiment_type", "data", "experiments", "types", "mappings", "weights", "filename")),
        msg = "Invalid experiments submitted."
      )
    )
    
    no_filename <- experiments %>% map("filename") %>% map_lgl(is_null)
    if (any(no_filename)) {
      assert_that(.type != "permanent", msg = "If setting experiments permanently, all experiments need a defined filename.")
      experiments <- experiments %>% modify_if(
        no_filename,
        ~ {
          .x$filename <- paste0("CoRC_temp_", digest::digest(.x), ".txt")
          .x
        }
      )
    }
  }
  
  errors <- FALSE
  
  restorationCall <- list(
    .type = "restore",
    datamodel = datamodel
  )
  
  if (!is.null(method)) {
    if (is_scalar_character(method)) method <- list(method = method)
    # hack to get nice error message if method string is not accepted.
    with(method, rlang::arg_match(method, names(.__E___CTaskEnum__Method)[task$getValidMethods() + 1L]))
  }
  
  if (!is.null(randomizeStartValues)) {
    restorationCall$randomizeStartValues <- as.logical(problem$getRandomizeStartValues())
    problem$setRandomizeStartValues(randomizeStartValues)
  }
  
  if (!is.null(createParameterSets)) {
    restorationCall$createParameterSets <- as.logical(problem$getCreateParameterSets())
    problem$setCreateParameterSets(createParameterSets)
  }
  
  if (!is.null(calculateStatistics)) {
    restorationCall$calculateStatistics <- as.logical(problem$getCalculateStatistics())
    problem$setCalculateStatistics(calculateStatistics)
  }
  
  if (!is.null(updateModel)) {
    restorationCall$updateModel <- as.logical(task$isUpdateModel())
    task$setUpdateModel(updateModel)
  }
  
  if (!is.null(parameters)) {
    if (.type != "restore") {
      restorationCall$parameters <- parameters
      parameters %>% walk(~ {
        e <- try(addParameter(.x, datamodel = datamodel))
        if (is.error(e)) errors <<- TRUE
      })
    } else {
      clearParameters(datamodel = datamodel)
    }
  }
  
  if (!is.null(experiments)) {
    if (.type != "restore") {
      restorationCall$experiments <- experiments
        
      experiments %>% walk(~ {
        e <- try(addExperiments(.x, datamodel = datamodel))
        if (is.error(e)) errors <<- TRUE
      })
    } else {
      # delete the file
      e <- try(experiments %>% map("filename") %>% {file.path(normalizePathC(datamodel$getReferenceDirectory()), .)} %>% file.remove())
      if (is.error(e)) errors <<- TRUE
      clearExperiments(datamodel = datamodel)
    }
  }
  
  if (!is.null(method) && !is_empty(method)) {
    # We need to keep track of the previously set method
    restorationCall$method_old = task$getMethod()$getSubType()
    
    task$setMethodType(method$method)
    restorationCall$method <- list(method = method$method)
    method_cop = as(task$getMethod(), "_p_CFitMethod")
    
    method <- method[names(method) != "method"]
    
    if (!is_empty(method)) {
      # get some info on what parameters the method has
      methodstruct <- methodstructure(method_cop) %>% tibble::rowid_to_column()
      
      method <-
        tibble::tibble(value = method) %>%
        dplyr::mutate(rowid = pmatch(names(value), methodstruct$name))
      
      # all names that are not names of method parameters
      bad_names <- names(method$value)[is.na(method$rowid)]
      
      # merge method with relevant lines from methodstruct and check if the given values is allowed
      method <-
        method %>%
        dplyr::filter(!is.na(rowid), map_lgl(value, negate(is_null))) %>%
        dplyr::left_join(methodstruct, by = "rowid") %>%
        dplyr::mutate(
          allowed = map2_lgl(control_fun, value, ~ {
            if (!is_null(.x)) .x(.y)
            # No control function defined means just pass
            else TRUE
          })
        )
      
      # all parameters that did not satisfy the tests in methodstruct$control_fun
      forbidden <- dplyr::filter(method, !allowed)
      
      method <- dplyr::filter(method, allowed)
      
      # gather old value and then set new value
      method <-
        method %>%
        dplyr::mutate(
          oldval = map2(get_fun, object, ~ .x(.y)),
          success = pmap_lgl(., function(set_fun, object, value, ...) {set_fun(object, value)})
        )
      
      # parameters where trying to set it somehow failed as per feedback from copasi
      failures <- dplyr::filter(method, !success)
      
      method <- dplyr::filter(method, success)
      
      # Overwritten parameters need to be in the restorationCall
      if (nrow(method) != 0L) {
        restorationCall$method <- method %>% dplyr::select(name, oldval) %>% tibble::deframe()
      }
      
      # Give complete feedback error.
      errmsg <- ""
      if (!is_empty(bad_names)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(bad_names, collapse = "\", \""), "\" invalid. Should be one of : \"", paste0(methodstruct$name, collapse = "\", \""), "\"\n")
      if (nrow(forbidden) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(forbidden$name, collapse = "\", \""), "\" have to be of type(s) ", paste0(forbidden$type, collapse = "\", \""), ".\n")
      if (nrow(failures) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(failures$name, collapse = "\", \""), "\" could not be set.\n")
      if (nchar(errmsg) != 0L) {
        try(stop(errmsg))
        errors <- TRUE
      }
    }
  }
  
  if (errors && .type != "restore") {
    do.call(pe_settings_worker, restorationCall)
    stop("Rolled back task due to errors during setup.")
  }
  
  restorationCall
}

pe_result_worker <- function(datamodel) {
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  # items <- get_sv(problem$getOptItemList()) %>% map(as, Class = "_p_CFitItem")
  items <- seq_len_0(problem$getOptItemSize()) %>% map(~ as(problem$getOptItem(.x), Class = "_p_CFitItem"))
  experiment_set <- problem$getExperimentSet()
  experiments <-
    seq_len_0(experiment_set$getExperimentCount()) %>%
    map(~ experiment_set$getExperiment(.x))
  dependent_obj <- swigfix_resolve_obj_cvector(experiment_set, CExperimentSet_getDependentObjects, "CObjectInterface")
  
  ret <- list()
  
  evals <- problem$getFunctionEvaluations()
  evaltime <- problem$getExecutionTime()
  
  ret$main <-
    list(
      "Objective Value" = problem$getSolutionValue(),
      "Root Mean Square" = problem$getRMS(),
      "Standard Deviation" = problem$getStdDeviation(),
      "Validation Objective Value" = problem$getCrossValidationSolutionValue(),
      "Validation Root Mean Square" = problem$getCrossValidationRMS(),
      "Validation Standard Deviation" = problem$getCrossValidationSD(),
      "Function Evaluations" = evals,
      "CPU Time [s]" = evaltime,
      "Evaluations/second [1/s]" = evals / evaltime
    ) %>%
    transform_names()
  
  ret$parameters <-
    tibble::tibble(
      "Parameter" = map_chr(items, ~ .x$getObjectDisplayName()),
      "Lower Bound" = map_dbl(items, ~ .x$getLowerBoundValue()),
      "Start Value" = map_dbl(items, ~ .x$getStartValue()),
      "Value" = get_cv(problem$getSolutionVariables()),
      "Upper Bound" = map_dbl(items, ~ .x$getUpperBoundValue()),
      "Std. Deviation" = get_cv(problem$getVariableStdDeviations()),
      "Coeff. of Variation [%]" = NaN, # TODO
      "Gradient" = get_cv(problem$getVariableGradients())
    ) %>%
    transform_names()
  
  ret$experiments <-
    tibble::tibble(
      "Experiment" = map_chr(experiments, ~ .x$getObjectName()),
      "Objective Value" = map_dbl(experiments, ~ .x$getObjectiveValue()),
      "Root Mean Square" = map_dbl(experiments, ~ .x$getRMS()),
      "Error Mean" = map_dbl(experiments, ~ .x$getErrorMean()),
      "Error Mean Std. Deviation" = map_dbl(experiments, ~ .x$getErrorMeanSD())
    ) %>%
    transform_names()
  
  ret$fitted.values <-
    tibble::tibble(
      "Fitted Value" = map_chr(dependent_obj, ~ .x$getObjectDisplayName()),
      "Objective Value" = get_cv(experiment_set$getDependentObjectiveValues()),
      "Root Mean Square" = get_cv(experiment_set$getDependentRMS()),
      "Error Mean" = get_cv(experiment_set$getDependentErrorMean()),
      "Error Mean Std. Deviation" = get_cv(experiment_set$getDependentErrorMeanSD())
    ) %>%
    transform_names()
  
  ret$correlation <- get_annotated_matrix(problem$getCorrelations())
  
  ret$fim <- get_annotated_matrix(problem$getFisherInformation())
  ret$fim.eigenvalues <- get_annotated_matrix(problem$getFisherInformationEigenvalues())
  ret$fim.eigenvectors <- get_annotated_matrix(problem$getFisherInformationEigenvectors())
  
  ret$fim.scaled <- get_annotated_matrix(problem$getScaledFisherInformation())
  ret$fim.scaled.eigenvalues <- get_annotated_matrix(problem$getScaledFisherInformationEigenvalues())
  ret$fim.scaled.eigenvectors <- get_annotated_matrix(problem$getScaledFisherInformationEigenvectors())
  
  ret
}
