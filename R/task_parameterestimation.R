#' Run parameter estimation
#'
#' \code{runParameterEstimation} runs parameter estimation and returns results in a list.
#'
#' @param randomizeStartValues flag
#' @param createParameterSets flag
#' @param calculateStatistics flag
#' @param updateModel flag
#' @param executable flag
#' @param parameters copasi_param or list of copasi_param objects
#' @param experiments copasi_exp or list of copasi_exp objects
#' @param method string or list
#' @param model a model object
#' @return a list of results
#' @export
runParameterEstimation <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, executable = NULL, parameters = NULL, experiments = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- pe_assemble_settings(
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  
  # does assertions
  method_settings <- pe_assemble_method(method, c_task)
  
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  # does assertions
  parameter_list <- pe_assemble_parameters(parameters, c_problem)
  experiment_list <- pe_assemble_experiments(experiments, c_problem, temp_filenames = TRUE)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  do_parameters <- !is_empty(parameter_list)
  do_experiments <- !is_empty(experiment_list)
  
  # save all previous settings
  if (do_settings)
    pre_settings <- pe_get_settings(c_task)
  if (do_method) {
    # keep track of the originally set method
    pre_method <- c_task$getMethod()$getSubType()
    # change the method first, then save the settings for the new method
    if (!is_null(method_settings$method)) c_task$setMethodType(method_settings$method)
    c_method <- as(c_task$getMethod(), "_p_CTrajectoryMethod")
    pre_method_settings <- get_method_settings(c_method, with_name = TRUE)
  } else {
    c_method <- as(c_task$getMethod(), "_p_CTrajectoryMethod")
  }
  
  # apply settings
  success <- !is.error(try(pe_set_settings(settings, c_task)))
  if (success)
    success <- !is.error(try(set_method_settings(method_settings, c_method)))
  if (success)
    success <- !is.error(try(walk(parameter_list, addParameter, c_datamodel)))
  if (success)
    success <- !is.error(try(walk(experiment_list, addExperiments, c_datamodel)))
  # initialize task
  if (success)
    success <- grab_msg(c_task$initializeRaw(OUTPUTFLAG))
  # run task and save current settings
  if (success) {
    full_settings <- pe_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method, with_name = TRUE)
    success <- grab_msg(c_task$processRaw(TRUE))
  }
  # get results
  if (success)
    ret <- pe_get_results(c_datamodel, full_settings)
  
  # revert all settings
  if (do_settings)
    pe_set_settings(pre_settings, c_task)
  if (do_method) {
    set_method_settings(pre_method_settings, c_method)
    c_task$setMethodType(pre_method)
  }
  if (do_parameters)
    clearParameters()
  if (do_experiments) {
    clearExperiments()
    model_dir <- c_datamodel$getReferenceDirectory()
    if (model_dir == "") model_dir <- getwd()
    model_dir <- normalizePathC(model_dir)
    try(experiment_list %>% map_chr(attr_getter("filename")) %>% file.path(model_dir, .) %>% file.remove())
  }
  
  # assertions only after restoration of settings
  assert_that(
    success,
    msg = paste0("Processing the task failed.")
  )
  
  ret
}

#' Set parameter estimation settings
#'
#' \code{setParameterEstimationSettings} sets parameter estimation task settings including parameters, experiments and method options.
#'
#' @param randomizeStartValues flag
#' @param createParameterSets flag
#' @param calculateStatistics flag
#' @param updateModel flag
#' @param executable flag
#' @param parameters copasi_param or list of copasi_param objects
#' @param experiments copasi_exp or list of copasi_exp objects
#' @param method string or list
#' @param model a model object
#' @export
setParameterEstimationSettings <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, executable = NULL, parameters = NULL, experiments = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- pe_assemble_settings(
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  
  # does assertions
  method_settings <- pe_assemble_method(method, c_task)
  
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  # does assertions
  parameter_list <- pe_assemble_parameters(parameters, c_problem)
  experiment_list <- pe_assemble_experiments(experiments, c_problem)
  
  # experiments and parameters get rolled back when not setting them properly
  tryCatch(
    walk(parameter_list, addParameter, c_datamodel),
    error = {
      clearParameters()
      stop("Failed when applying parameters.")
    }
  )
  tryCatch(
    walk(experiment_list, addExperiments, c_datamodel),
    error = {
      clearExperiments()
      stop("Failed when applying experiments.")
    }
  )
  
  # switch to given method
  if (!is_null(method_settings$method))
    c_task$setMethodType(method_settings$method)
  
  c_method <- as(c_task$getMethod(), "_p_COptMethod")
  
  pe_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method)
  
  invisible()
}

#' Set parameter estimation settings
#'
#' \code{getParameterEstimationSettings} gets parameter estimation task settings including method options.
#'
#' @param model a model object
#' @return A list of parameter estimation task settings including method options.
#' @export
getParameterEstimationSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_method <- as(c_task$getMethod(), "_p_COptMethod")
  
  ret <- pe_get_settings(c_task)
  ret$method <- get_method_settings(c_method, with_name = TRUE)
  
  ret
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
addParameter <- function(copasi_parm, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.copasi_parm(copasi_parm)
  )
  validate_copasi_parm(copasi_parm)
  
  c_obj <- xn_to_object(copasi_parm$key, c_datamodel)
  # Test if the key is valid
  # This can probably be a more elaborate and safe test (by using dn_to_object(accepted_types))
  assert_that(
    !is.null(c_obj),
    msg = "The given parameter is invalid for this model"
  )
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  c_fititem <- c_problem$addFitItem(c_obj$getCN())
  c_fititem$setLowerBound(CCommonName(as.character(copasi_parm$lower)))
  c_fititem$setUpperBound(CCommonName(as.character(copasi_parm$upper)))
  c_fititem$setStartValue(copasi_parm$start)
}

#' @export
clearParameters <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  seq_len_0(c_problem$getOptItemSize()) %>% walk(~ c_problem$removeOptItem(0))
}

new_copasi_exp <- function(x, experiment_type, experiments, types, mappings, weight_method, filename) {
  assert_that(
    is.string(experiment_type),
    is.character(types), length(types) == ncol(x),
    is.character(mappings), length(mappings) == ncol(x),
    is.string(weight_method),
    is.string(filename)
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
    filename = filename
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
  if (is.data.frame(data))
    data <- list(data)
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
  assert_that(is.null(filename) || is.string(filename) && noNA(filename))
  if (is.null(filename))
    # if no filename given, just use random one.
    filename <- paste0("CoRC_exp_", digest::digest(runif(1)))
  if (!has_extension(filename, ".txt"))
    filename <- paste0(filename, ".txt")
  
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
addExperiments <- function(copasi_exp, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.copasi_exp(copasi_exp))
  
  experiment_type <- copasi_exp %@% "experiment_type"
  experiments <- copasi_exp %@% "experiments"
  types <- copasi_exp %@% "types"
  mappings <- copasi_exp %@% "mappings"
  weight_method <- copasi_exp %@% "weight_method"
  filename <- copasi_exp %@% "filename"
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  c_experiment_set <- c_problem$getExperimentSet()
  
  # Create experiment file
  model_dir <- c_datamodel$getReferenceDirectory()
  if (model_dir == "")
    model_dir <- getwd()
  model_dir <- normalizePathC(model_dir)
  filepath <- file.path(model_dir, filename)
  assert_that(
    # If the user has set a manual filename, try to be safe and not overwrite anything
    !file.exists(filepath) || grepl("^CoRC_exp_", filename),
    msg = paste0('Experiment file path "', filepath, '" already exists.')
  )
  readr::write_tsv(copasi_exp, filepath)
  
  # Construct individual experiments
  cl_experiments <-
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
  walk_swig(cl_experiments, "setHeaderRow", 1)
  walk_swig(cl_experiments, "setFileName", filename)
  walk_swig(cl_experiments, "setExperimentType", experiment_type)
  walk_swig(cl_experiments, "setNumColumns", col_count)
  walk_swig(cl_experiments, "setWeightMethod", weight_method)
  # walk_swig(cl_experiments, "readColumnNames")
  
  # Get the object maps and assign roles and mappings
  cl_object_maps <- map_swig(cl_experiments, "getObjectMap")
  walk_swig(cl_object_maps, "setNumCols", col_count)
  
  # Transfer values from named vectors to a full representation of all data columns
  types_ordered <- rep("ignore", col_count)
  types_ordered[match(names(types), col_names)] <- types
  mappings_ordered <- rep("", col_count)
  mappings_ordered[match(names(mappings), col_names)] <-
    map(mappings, xn_to_object, c_datamodel) %>%
    map_chr(get_cn) %>%
    replace(is.na(.), "")
  
  # We have a list of object_maps and all need the same types and mappings
  types_ordered %>% iwalk(~ walk_swig(cl_object_maps, "setRole", .y - 1L, .x))
  mappings_ordered %>% iwalk(~ walk_swig(cl_object_maps, "setObjectCN", .y - 1L, .x))
  
  # Add all experiments to copasi
  cl_experiments %>% walk(~ c_experiment_set$addExperiment(.x))
  
  # possibly compile
  # c_experiment_set$compile(problem$getMathContainer())
}

#' @export
clearExperiments <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  c_experiment_set <- c_problem$getExperimentSet()
  
  seq_len_0(c_experiment_set$getExperimentCount()) %>% walk(~ c_experiment_set$removeExperiment(0))
}

pe_assemble_parameters <- function(parameters, c_problem) {
  assert_that(is.null(parameters) || is.list(parameters) && every(parameters, is.copasi_parm) || is.copasi_parm(parameters))
  
  if (is.copasi_parm(parameters))
    parameters <- list(parameters)
  
  if (is_empty(parameters))
    return(list())
  
  assert_that(
    c_problem$getOptItemSize() == 0L,
    msg = "This function can not set parameters if there are already parameters set in copasi."
  )
  
  walk(parameters, validate_copasi_parm)
  
  parameters
}

pe_assemble_experiments <- function(experiments, c_problem, temp_filenames = FALSE) {
  assert_that(is.null(experiments) || is.list(experiments) && every(experiments, is.copasi_exp) || is.copasi_exp(experiments))
  
  if (is.copasi_exp(experiments))
    experiments <- list(experiments)
  
  if (is_empty(experiments))
    return(list())
  
  assert_that(
    c_problem$getExperimentSet()$getExperimentCount() == 0L,
    msg = "This function can not set experiments if there are already experiments set in copasi."
  )
  
  # force temporary experiment file names so they can be deleted safely
  if (temp_filenames)
    experiments <-
      experiments %>%
      map(~ {
        attr(.x, "filename") <- paste0("CoRC_exp_", digest::digest(runif(1)), ".txt")
        .x
      })
  
  experiments
}

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
pe_assemble_settings <- function(randomizeStartValues, createParameterSets, calculateStatistics, updateModel, executable) {
  assert_that(
    is.null(randomizeStartValues) || noNA(randomizeStartValues) && is.flag(randomizeStartValues),
    is.null(createParameterSets)  || noNA(createParameterSets)  && is.flag(createParameterSets),
    is.null(calculateStatistics)  || noNA(calculateStatistics)  && is.flag(calculateStatistics),
    is.null(updateModel)          || noNA(updateModel)          && is.flag(updateModel),
    is.null(executable)           || noNA(executable)           && is.flag(executable)
  )
  
  list(
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    executable = executable
  ) %>%
    discard(is_null)
}

# does assertions
# returns a list of method settings
pe_assemble_method <- function(method, c_task) {
  if (is_null(method))
    return(list())
  
  assert_that(is.string(method) || is.list(method))
  
  if (is_scalar_character(method))
    method <- list(method = method)
  
  if (has_name(method, "method"))
    # hack to get nice error message if method string is not accepted.
    with(method, rlang::arg_match(method, names(.__E___CTaskEnum__Method)[c_task$getValidMethods() + 1L]))
  
  method
}

# gets full list of settings
pe_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  list(
    randomizeStartValues = as.logical(c_problem$getRandomizeStartValues()),
    createParameterSets =  as.logical(c_problem$getCreateParameterSets()),
    calculateStatistics =  as.logical(c_problem$getCalculateStatistics()),
    updateModel =          as.logical(c_task$isUpdateModel()),
    executable =           as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
pe_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  if (!is_null(data$randomizeStartValues))
    c_problem$setRandomizeStartValues(data$randomizeStartValues)
  
  if (!is_null(data$createParameterSets))
    c_problem$setCreateParameterSets(data$createParameterSets)
  
  if (!is_null(data$calculateStatistics))
    c_problem$setCalculateStatistics(data$calculateStatistics)
  
  if (!is_null(data$updateModel))
    c_task$setUpdateModel(data$updateModel)
  
  if (!is_null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
pe_get_results <- function(c_datamodel, settings) {
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  c_method <- as(c_task$getMethod(), "_p_COptMethod")
  
  cl_items <- get_sv(c_problem$getOptItemList()) %>% map(as, Class = "_p_CFitItem")
  c_experiment_set <- c_problem$getExperimentSet()
  cl_experiments <-
    seq_len_0(c_experiment_set$getExperimentCount()) %>%
    map(~ c_experiment_set$getExperiment(.x))
  cl_dependent_obj <- swigfix_resolve_obj_cvector(c_experiment_set, CExperimentSet_getDependentObjects, "CObjectInterface")
  
  evals <- c_problem$getFunctionEvaluations()
  evaltime <- c_problem$getExecutionTime()
  
  main <-
    list(
      "Objective Value" = c_problem$getSolutionValue(),
      "Root Mean Square" = c_problem$getRMS(),
      "Standard Deviation" = c_problem$getStdDeviation(),
      "Validation Objective Value" = c_problem$getCrossValidationSolutionValue(),
      "Validation Root Mean Square" = c_problem$getCrossValidationRMS(),
      "Validation Standard Deviation" = c_problem$getCrossValidationSD(),
      "Function Evaluations" = evals,
      "CPU Time [s]" = evaltime,
      "Evaluations/second [1/s]" = evals / evaltime
    ) %>%
    transform_names()
  
  parameters <-
    tibble::tibble(
      "Parameter" = map_swig_chr(cl_items, "getObjectDisplayName"),
      "Lower Bound" = map_swig_dbl(cl_items, "getLowerBoundValue"),
      "Start Value" = map_swig_dbl(cl_items, "getStartValue"),
      "Value" = get_cv(c_problem$getSolutionVariables()),
      "Upper Bound" = map_swig_dbl(cl_items, "getUpperBoundValue"),
      "Std. Deviation" = get_cv(c_problem$getVariableStdDeviations()),
      "Coeff. of Variation [%]" = NaN, # TODO
      "Gradient" = get_cv(c_problem$getVariableGradients())
    ) %>%
    transform_names()
  
  experiments <-
    tibble::tibble(
      "Experiment" = map_swig_chr(cl_experiments, "getObjectName"),
      "Objective Value" = map_swig_dbl(cl_experiments, "getObjectiveValue"),
      "Root Mean Square" = map_swig_dbl(cl_experiments, "getRMS"),
      "Error Mean" = map_swig_dbl(cl_experiments, "getErrorMean"),
      "Error Mean Std. Deviation" = map_swig_dbl(cl_experiments, "getErrorMeanSD")
    ) %>%
    transform_names()
  
  fitted.values <-
    tibble::tibble(
      "Fitted Value" = map_swig_chr(cl_dependent_obj, "getObjectDisplayName"),
      "Objective Value" = get_cv(c_experiment_set$getDependentObjectiveValues()),
      "Root Mean Square" = get_cv(c_experiment_set$getDependentRMS()),
      "Error Mean" = get_cv(c_experiment_set$getDependentErrorMean()),
      "Error Mean Std. Deviation" = get_cv(c_experiment_set$getDependentErrorMeanSD())
    ) %>%
    transform_names()
  
  correlation <- get_annotated_matrix(c_problem$getCorrelations())
  
  fim <- get_annotated_matrix(c_problem$getFisherInformation())
  fim.eigenvalues <- get_annotated_matrix(c_problem$getFisherInformationEigenvalues())
  fim.eigenvectors <- get_annotated_matrix(c_problem$getFisherInformationEigenvectors())
  
  fim.scaled <- get_annotated_matrix(c_problem$getScaledFisherInformation())
  fim.scaled.eigenvalues <- get_annotated_matrix(c_problem$getScaledFisherInformationEigenvalues())
  fim.scaled.eigenvectors <- get_annotated_matrix(c_problem$getScaledFisherInformationEigenvectors())
  
  protocol <- c_method$getMethodLog()$getPlainLog()
  
  list(
    settings = settings,
    main = main,
    parameters = parameters,
    experiments = experiments,
    fitted.values = fitted.values,
    correlation = correlation,
    fim = fim,
    fim.eigenvalues = fim.eigenvalues,
    fim.eigenvectors = fim.eigenvectors,
    fim.scaled = fim.scaled,
    fim.scaled.eigenvalues = fim.scaled.eigenvalues,
    fim.scaled.eigenvectors = fim.scaled.eigenvectors,
    protocol = protocol
  )
}

