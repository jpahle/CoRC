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
  
  if (!is_null(executable)) {
    datamodel$getTask("Parameter Estimation")$setScheduled(executable)
  }
  
  invisible()
}

new_copasi_parm <- function(x, lower, upper, start) {
  assert_that(
    is.string(x),
    is.number(lower),
    is.number(upper),
    is.number(start),
    lower <= start,
    start <= upper
  )
  
  structure(
    list(
      key = x,
      lower = lower,
      upper = upper,
      start = start
    ),
    class = "copasi_parm"
  )
}

#' @export
validate_copasi_parm <- function(x) {
  assert_that(
    is.string(x$key),
    is.number(x$lower),
    is.number(x$upper),
    is.number(x$start),
    x$lower <= x$start,
    x$start <= x$upper
  )
}

#' @export
is.copasi_parm <- function(x) {
  inherits(x, "copasi_parm")
}

#' @export
copasi_parm <- function(key = NULL, lower.bound = 1e-6, upper.bound = 1e6, start.value = (lower.bound + upper.bound) / 2) {
  new_copasi_parm(
    key,
    lower = lower.bound,
    upper = upper.bound,
    start = start.value
  )
}

#' @export
defineParameter <- copasi_parm

#' @export
addParameter <- function(copasi_parm, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.copasi_parm(copasi_parm)
  )
  validate_copasi_parm(copasi_parm)
  
  # Test if the key is valid
  # This can probably be a more elaborate and safe test (by using dn_to_object(accepted_types))
  assert_that(
    !is.null(dn_to_object(copasi_parm$key, datamodel)),
    msg = "The given parameter is invalid for this datamodel"
  )
  
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  fititem <- problem$addFitItem(CCommonName(copasi_parm$key))
  fititem$setLowerBound(CCommonName(as.character(copasi_parm$lower)))
  fititem$setUpperBound(CCommonName(as.character(copasi_parm$upper)))
  fititem$setStartValue(copasi_parm$start)
}

#' @export
clearParameters <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  seq_len_0(problem$getOptItemSize()) %>% walk(~ problem$removeOptItem(0))
}

new_copasi_exp <- function(x, experiment_type, experiments, types, mappings, weight_method, filename) {
  assert_that(
    is.string(experiment_type),
    is.character(types), length(types) == ncol(x),
    is.character(mappings), length(mappings) == ncol(x),
    is.string(weight_method),
    is.null(filename) || is.string(filename)
  )
  
  x <- tibble::as_tibble(x)
  experiments <- tibble::as_tibble(experiments)
  
  structure(
    x,
    class = c("copasi_exp", class(x)),
    experiment_type = experiment_type,
    experiments = experiments,
    types = types,
    mappings = mappings,
    weight_method = weight_method,
    filename = filename %||% NA_character_
  )
}

#' @export
is.copasi_exp <- function(x) {
  inherits(x, "copasi_exp")
}

#' @export
format.copasi_exp <- function (x, ...) {
  c(
    NextMethod(),
    "# Experiment Type:",
    format(x %@% "experiment_type"),
    "# Experiments:",
    format(x %@% "experiments"),
    "# Types:",
    format(x %@% "types") %>% paste0(names(.), ": ", .),
    "# Mappings:",
    format(x %@% "mappings") %>% paste0(names(.), ": ", .),
    "# Weight Method:",
    format(x %@% "weight_method"),
    "# Filename:",
    format(x %@% "filename")
  )
}

#' @export
copasi_exp <- function(experiment_type = c("Time Course", "Steady State"), data = NULL, types = NULL, mappings = NULL, weight_method = NULL, filename = NULL) {
  # experiment_type
  experiment_type <- rlang::arg_match(experiment_type)
  experiment_type <- c("Steady State" = "steadyState", "Time Course" = "timeCourse")[experiment_type]
  
  # data
  if (is.data.frame(data)) data <- list(data)
  assert_that(every(data, is.data.frame))
  data <- map(data, tibble::as_tibble)

  # get experiment names from names of data or use list position ("Experiment_x")
  experiment_names <- names(data) %||% rep(NA_character_, length(data))
  missing_names <- which(is.na(experiment_names))
  experiment_names[missing_names] <- paste0("Experiment_", missing_names)
  
  # gather info on where the individual experiments start because the all get merged
  experiment_lengths <- map_int(data, nrow)
  experiment_lastrows <- cumsum(experiment_lengths)
  
  data <- dplyr::bind_rows(data)
  data_cols <- names(data)
  
  # types
  assert_that(
    is.character(types),
    is.null(names(types)) && length(types) == length(data_cols) ||
    all(names(mappings) %in% data_cols)
  )
  
  types <- stringr::str_to_lower(types)
  names(types) <- names(types) %||% data_cols
  types <- map_chr(types, function(types) {rlang::arg_match(types, c("time", "independent", "dependent", "ignore"))})
  types <- types[types != "ignore"]
  
  assert_that(
    !(experiment_type == "timeCourse" && sum(types == "time") != 1L),
    msg = 'Time course experiements need exactly one "time" mapping.'
  )
  assert_that(
    !(experiment_type == "steadyState" && any(types == "time")),
    msg = 'Steady state experiements cannot have a "time" mapping.'
  )
  
  # mappings
  mappings <- mappings %||% data_cols
  assert_that(
    is.character(mappings),
    is.null(names(mappings)) && length(mappings) == length(data_cols) ||
    all(names(mappings) %in% data_cols)
  )
  
  names(mappings) <- names(mappings) %||% data_cols
  
  # all mappings to time and ignore are forced to be blank
  mappings[names(types)[types %in% c("time", "ignore")]] <- ""
  
  # allowed weight methods are taken from the copasi enumeration
  weight_methods <- names(.__E___CExperiment__WeightMethod)
  weight_methods_lower <- stringr::str_to_lower(weight_methods)
  if (is_null(weight_method)) {
    weight_method <- weight_methods[1]
  } else {
    weight_method <- rlang::arg_match(weight_method, weight_methods_lower)
    weight_method <- weight_methods[weight_method == weight_methods_lower]
  }
  
  # filename
  if (!is_null(filename)) {
    assert_that(is.string(filename) && !is.na(filename))
    if (!has_extension(filename, ".txt")) filename <- paste0(filename, ".txt")
  }
  
  new_copasi_exp(
    data,
    experiment_type = experiment_type,
    experiments = tibble::tibble(
      name = experiment_names,
      first_row = c(1L, head(experiment_lastrows + 1L, -1L)),
      last_row = experiment_lastrows
    ),
    types = types,
    mappings = mappings,
    weight_method = weight_method,
    filename = filename
  )
}

#' @export
defineExperiments <- copasi_exp

#' @export
addExperiments <- function(copasi_exp, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.copasi_exp(copasi_exp))
  
  experiment_type <- copasi_exp %@% "experiment_type"
  experiments <- copasi_exp %@% "experiments"
  types <- copasi_exp %@% "types"
  mappings <- copasi_exp %@% "mappings"
  weight_method <- copasi_exp %@% "weight_method"
  filename <- copasi_exp %@% "filename"
  
  c_task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  c_experiment_set <- c_problem$getExperimentSet()
  
  # Create experiment file
  model_dir <- datamodel$getReferenceDirectory()
  if (model_dir == "") model_dir <- getwd()
  model_dir <- normalizePathC(model_dir)
  filepath <- file.path(model_dir, filename)
  readr::write_tsv(copasi_exp, filepath)
  
  # Construct individual experiments
  c_experiments <-
    experiments %>%
    pmap(function(name, first_row, last_row, ...) {
      exp <- avert_gc(CExperiment(c_experiment_set, name))
      exp$setFirstRow(first_row)
      exp$setLastRow(last_row)
      exp
    })
  
  col_count <- ncol(copasi_exp)
  col_names <- colnames(copasi_exp)

  # Set all experiment's settings
  walk_swig(c_experiments, "setHeaderRow", 1)
  walk_swig(c_experiments, "setFileName", filename)
  walk_swig(c_experiments, "setExperimentType", experiment_type)
  walk_swig(c_experiments, "setNumColumns", col_count)
  walk_swig(c_experiments, "setWeightMethod", weight_method)
  # walk_swig(experiments, "readColumnNames")
  
  # Get the object maps and assign roles and mappings
  c_object_maps <- map_swig(c_experiments, "getObjectMap")
  walk_swig(c_object_maps, "setNumCols", col_count)
  
  # Transfer values from named vectors to a full representation of all data columns
  types_ordered <- rep("ignore", col_count)
  types_ordered[match(names(types), col_names)] <- types
  mappings_ordered <- rep("", col_count)
  mappings_ordered[match(names(mappings), col_names)] <- mappings
  
  # We have a list of object_maps and all need the same types and mappings
  types_ordered %>% iwalk(~ walk_swig(c_object_maps, "setRole", .y - 1L, .x))
  mappings_ordered %>% iwalk(~ walk_swig(c_object_maps, "setObjectCN", .y - 1L, .x))
  
  # Add all experiments to copasi
  c_experiments %>% walk(~ c_experiment_set$addExperiment(.x))
  
  # possibly compile
  # c_experiment_set$compile(problem$getMathContainer())
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
pe_settings_worker <- function(.type, randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, parameters = NULL, experiments = NULL, method = NULL, method_old = NULL, datamodel) {
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  assert_that(
    is.null(randomizeStartValues)   || is.flag(randomizeStartValues) && !is.na(randomizeStartValues),
    is.null(createParameterSets)    || is.flag(createParameterSets)  && !is.na(createParameterSets),
    is.null(calculateStatistics)    || is.flag(calculateStatistics)  && !is.na(calculateStatistics),
    is.null(updateModel)            || is.flag(updateModel)          && !is.na(updateModel),
    is.null(parameters)             || is.list(parameters)           && every(parameters, is.copasi_parm) || is.copasi_parm(parameters),
    is.null(experiments)            || is.list(experiments)          && every(experiments, is.copasi_exp) || is.copasi_exp(experiments),
    is.null(method)                 || is.string(method)             && !is.na(method) || is.list(method) && is.string(method$method) && !is.na(method$method)
  )
  
  if (!is_null(parameters) && .type != "restore") {
    assert_that(
      problem$getOptItemSize() == 0L,
      msg = "This function can not set parameters if there are already parameters set in copasi."
    )
    
    if (is.copasi_parm(parameters))
      parameters <- list(parameters)
    
    parameters %>% walk(validate_copasi_parm)
  }
    
  if (!is_null(experiments) && .type != "restore") {
    assert_that(
      problem$getExperimentSet()$getExperimentCount() == 0L,
      msg = "This function can not set experiments if there are already experiments set in copasi."
    )
    
    if (is.copasi_exp(experiments))
      experiments <- list(experiments)
    
    no_filename <- experiments %>% map(attr_getter("filename")) %>% map_lgl(is.na)
    if (any(no_filename)) {
      assert_that(.type != "permanent", msg = "If setting experiments permanently, all experiments need a defined filename.")
      experiments <- experiments %>% modify_if(
        no_filename,
        ~ {
          attr(.x, "filename") <- paste0("CoRC_temp_", digest::digest(.x), ".txt")
          .x
        }
      )
    }
  }
  
  if (!is_null(method)) {
    if (is_scalar_character(method)) method <- list(method = method)
    # hack to get nice error message if method string is not accepted.
    with(method, rlang::arg_match(method, names(.__E___CTaskEnum__Method)[task$getValidMethods() + 1L]))
  }
  
  errors <- FALSE
  
  restorationCall <- list(
    .type = "restore",
    datamodel = datamodel
  )
  
  if (!is_null(randomizeStartValues)) {
    restorationCall$randomizeStartValues <- as.logical(problem$getRandomizeStartValues())
    problem$setRandomizeStartValues(randomizeStartValues)
  }
  
  if (!is_null(createParameterSets)) {
    restorationCall$createParameterSets <- as.logical(problem$getCreateParameterSets())
    problem$setCreateParameterSets(createParameterSets)
  }
  
  if (!is_null(calculateStatistics)) {
    restorationCall$calculateStatistics <- as.logical(problem$getCalculateStatistics())
    problem$setCalculateStatistics(calculateStatistics)
  }
  
  if (!is_null(updateModel)) {
    restorationCall$updateModel <- as.logical(task$isUpdateModel())
    task$setUpdateModel(updateModel)
  }
  
  if (!is_null(parameters)) {
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
  
  if (!is_null(experiments)) {
    if (.type != "restore") {
      restorationCall$experiments <- experiments
        
      experiments %>% walk(~ {
        e <- try(addExperiments(.x, datamodel = datamodel))
        if (is.error(e)) errors <<- TRUE
      })
    } else {
      clearExperiments(datamodel = datamodel)
      # delete the file
      model_dir <- datamodel$getReferenceDirectory()
      if (model_dir == "") model_dir <- getwd()
      model_dir <- normalizePathC(model_dir)
      e <- try(experiments %>% map_chr(attr_getter("filename")) %>% file.path(model_dir, .) %>% file.remove())
      if (is.error(e)) errors <- TRUE
    }
  }
  
  if (!is_null(method)) {
    # We need to keep track of the previously set method
    restorationCall$method_old = task$getMethod()$getSubType()
    
    task$setMethodType(method$method)
    restorationCall$method <- list(method = method$method)
    method_cop = as(task$getMethod(), "_p_COptMethod")
    
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
  
  # method_old is only set if the purpose of calling the function was a restorationCall
  if (!is_null(method_old)) {
    task$setMethodType(method_old)
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
  method <- as(task$getMethod(), "_p_COptMethod")
  
  items <- get_sv(problem$getOptItemList()) %>% map(as, Class = "_p_CFitItem")
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
  
  ret$protocol <- method$getMethodLog()$getPlainLog()
  
  ret
}
