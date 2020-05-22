#' Run parameter estimation
#'
#' \code{runParameterEstimation} runs parameter estimation and returns the results in a list.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param randomize_start_values flag
#' @param create_parameter_sets flag
#' @param calculate_statistics flag
#' @param update_model flag
#' @param executable flag
#' @param parameters corc_opt_parm or list of corc_opt_parm objects
#' @param experiments copasi_exp or list of copasi_exp objects
#' @eval rox_method_param("Parameter Estimation", "_p_CFitTask")
#' @param model A model object.
#' @return A list of results.
#' @family parameter estimation
#' @export
runParameterEstimation <- function(randomize_start_values = NULL, create_parameter_sets = NULL, calculate_statistics = NULL, update_model = NULL, executable = NULL, parameters = NULL, experiments = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- pe_assemble_settings(
    randomize_start_values = randomize_start_values,
    create_parameter_sets  = create_parameter_sets,
    calculate_statistics   = calculate_statistics,
    update_model           = update_model,
    executable             = executable
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
  
  c_model <- c_datamodel$getModel()
  
  tryCatch({
    # save all previous settings
    if (do_settings)
      pre_settings <- pe_get_settings(c_task)
    if (do_method) {
      # keep track of the originally set method
      pre_method <- c_task$getMethod()$getSubType()
      # change the method first, then save the settings for the new method
      if (!is.null(method_settings$method))
        c_task$setMethodType(method_settings$method)
      c_method <- as(c_task$getMethod(), "_p_COptMethod")
      pre_method_settings <- get_method_settings(c_method, with_name = TRUE)
    } else {
      c_method <- as(c_task$getMethod(), "_p_COptMethod")
    }
    
    # apply settings
    if (do_settings)
      pe_set_settings(settings, c_task)
    if (do_method)
      set_method_settings(method_settings, c_method)
    if (do_parameters)
      addParameterEstimationParameter(parameter_list, model = c_datamodel)
    if (do_experiments)
      addExperiments(experiment_list, model = c_datamodel)
    
    compile_and_check(c_model)
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # save current settings
    full_settings <- pe_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method, with_name = TRUE)
    
    # run task
    process_task(c_task)
    
    # get results
    ret <- pe_get_results(c_task, full_settings)
  },
  finally = {
    # revert all settings
    if (do_settings)
      pe_set_settings(pre_settings, c_task)
    if (do_method) {
      set_method_settings(pre_method_settings, c_method)
      c_task$setMethodType(pre_method)
    }
    if (do_parameters)
      clearParameterEstimationParameters()
    if (do_experiments) {
      clearExperiments()
      
      # delete all experiment files
      # pe_assemble_experiments makes sure the are all tempfiles
      try(
        experiment_list %>%
          map_chr(attr_getter("filename")) %>%
          file.remove(),
        silent = TRUE
      )
    }
  })
  
  ret
}

#' Set parameter estimation settings
#'
#' \code{setParameterEstimationSettings} sets parameter estimation task settings including parameters, experiments and method options.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param randomize_start_values flag
#' @param create_parameter_sets flag
#' @param calculate_statistics flag
#' @param update_model flag
#' @param executable flag
#' @param parameters corc_opt_parm or list of corc_opt_parm objects
#' @param experiments copasi_exp or list of copasi_exp objects
#' @eval rox_method_param("Parameter Estimation", "_p_CFitTask")
#' @param model a model object
#' @family parameter estimation
#' @export
setParameterEstimationSettings <- function(randomize_start_values = NULL, create_parameter_sets = NULL, calculate_statistics = NULL, update_model = NULL, executable = NULL, parameters = NULL, experiments = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- pe_assemble_settings(
    randomize_start_values = randomize_start_values,
    create_parameter_sets  = create_parameter_sets,
    calculate_statistics   = calculate_statistics,
    update_model           = update_model,
    executable             = executable
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
    addParameterEstimationParameter(parameter_list, model = c_datamodel),
    error = function(e) {
      clearParameterEstimationParameters(c_datamodel)
      base::stop(e)
      # stop("Failed when applying parameters.")
    }
  )
  tryCatch(
    addExperiments(experiment_list, model = c_datamodel),
    error = function(e) {
      clearExperiments(c_datamodel)
      base::stop(e)
      # stop("Failed when applying experiments.")
    }
  )
  
  # switch to given method
  if (!is.null(method_settings$method))
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
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param model a model object
#' @return A list of parameter estimation task settings including method options.
#' @family parameter estimation
#' @export
getParameterEstimationSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_method <- as(c_task$getMethod(), "_p_COptMethod")
  
  ret <- pe_get_settings(c_task)
  ret$method <- get_method_settings(c_method, with_name = TRUE)
  
  ret
}

#' @rdname runParameterEstimation
#' @export
runPE <- runParameterEstimation
#' @rdname setParameterEstimationSettings
#' @export
setPE<- setParameterEstimationSettings
#' @rdname getParameterEstimationSettings
#' @export
getPE <- getParameterEstimationSettings

#' Define a parameter estimation parameter
#' 
#' @param ref value reference
#' @param start_value start value
#' @param lower_bound lower value bound
#' @param upper_bound upper value bound
#' @seealso \code{\link{addParameterEstimationParameter}} \code{\link{clearParameterEstimationParameters}}
#' @return corc_opt_parm object for input into related functions
#' @export
defineParameterEstimationParameter <- corc_opt_parm

#' Add a parameter estimation parameter
#' 
#' @param ... objects as returned by \code{\link{defineParameterEstimationParameter}}.
#' Alternatively, the same parameters as used by \code{\link{defineParameterEstimationParameter}}.
#' @param model a model object
#' @family parameter estimation
#' @seealso \code{\link{defineParameterEstimationParameter}} \code{\link{clearParameterEstimationParameters}}
#' @export
addParameterEstimationParameter <- function(..., model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # flatten all args into a single list
  # this list can be used to check if the user gave only corc_opt_parm
  arglist_compact <- rlang::squash(unname(list(...)))
  
  # if not all are corc_opt_parm, try handing the args to define... so we get corc_opt_parm
  if (!every(arglist_compact, is.corc_opt_parm))
    arglist_compact <- list(defineParameterEstimationParameter(...))
  
  walk(arglist_compact, validate_corc_opt_parm)
  
  cl_obj <-
    map_chr(arglist_compact, "ref") %>%
    map(xn_to_object, c_datamodel = c_datamodel)
  
  # Test if all refs are valid
  # This can probably be a more elaborate and safe test (by using dn_to_object(accepted_types))
  invalid_refs <- map_lgl(cl_obj, is.null)
  assert_that(
    !any(invalid_refs),
    msg = paste0("Given reference(s) ", paste0(which(invalid_refs), collapse = ", "), " are invalid for this model.")
  )
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  walk2(
    arglist_compact, cl_obj,
    ~ {
      c_fititem <- c_problem$addFitItem(.y$getCN())
      c_fititem$setStartValue(.x$start)
      c_fititem$setLowerBound(CCommonName(tolower(as.character(.x$lower))))
      c_fititem$setUpperBound(CCommonName(tolower(as.character(.x$upper))))
    }
  )
  
  invisible()
}

#' Clear all parameter estimation parameters
#' 
#' @param model a model object
#' @seealso \code{\link{addParameterEstimationParameter}} \code{\link{defineParameterEstimationParameter}}
#' @family parameter estimation
#' @export
clearParameterEstimationParameters <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  walk(
    seq_len_0(c_problem$getOptItemSize()),
    ~ c_problem$removeOptItem(0L)
  )
  
  invisible()
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

exp_weight_methods <- tolower(names(.__E___CExperiment__WeightMethod))
exp_allowed_types <- c("time", "independent", "dependent", "ignore")

#' @export
copasi_exp <- function(experiment_type = c("time_course", "steady_state"), data = NULL, types = NULL, mappings = NULL, weight_method = NULL, filename = NULL) {
  types <- to_param_vector(types, "character")
  mappings <- to_param_vector(mappings, "character")
  
  # experiment_type
  experiment_type <- rlang::arg_match(experiment_type)
  experiment_type <- c("time_course" = "timeCourse", "steady_state" = "steadyState")[experiment_type]
  
  # data
  if (is.data.frame(data))
    data <- list(data)
  assert_that(every(data, is.data.frame))
  data <- map(data, tibble::as_tibble, validate = TRUE)

  # get experiment names from names of data or use list position ("Experiment_x")
  experiment_names <- names(data) %||% rep_along(data, NA_character_)
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
  
  types <- tolower(types)
  names(types) <- names(types) %||% data_cols
  types <- args_match(types, exp_allowed_types)
  types <- types[types != "ignore"]
  
  if (experiment_type == "timeCourse")
    assert_that(sum(types == "time") == 1L, msg = 'Time course experiements need exactly one "time" mapping.')
  else if (experiment_type == "steadyState")
    assert_that(!any(types == "time"), msg = 'Steady state experiements cannot have a "time" mapping.')
  
  # mappings
  mappings <- mappings %||% data_cols
  assert_that(
    is.null(names(mappings)) && length(mappings) == length(data_cols) ||
    all(names(mappings) %in% data_cols)
  )
  
  names(mappings) <- names(mappings) %||% data_cols
  
  # all mappings to time and ignore are forced to be blank
  mappings[names(types)[types %in% c("time", "ignore")]] <- ""
  
  if (is.null(weight_method)) {
    weight_method <- exp_weight_methods[1]
  } else {
    weight_method <- tolower(weight_method)
    weight_method <- rlang::arg_match(weight_method, exp_weight_methods)
    weight_method <- toupper(weight_method)
  }
  
  # filename
  assert_that(is.null(filename) || is.string(filename) && noNA(filename))
  if (is.null(filename))
    # if no filename given, just use random one.
    filename <- stringr::str_sub(tempfile("CoRC_exp_", ""), 2L)
  if (!has_extension(filename, ".txt"))
    filename <- paste0(filename, ".txt")
  
  new_copasi_exp(
    data,
    experiment_type = experiment_type,
    experiments = tibble::tibble(
      name = experiment_names,
      first_row = c(1L, head(experiment_lastrows + 1L, -1L)),
      last_row = experiment_lastrows,
      .rows = length(experiment_names)
    ),
    types = types,
    mappings = mappings,
    weight_method = weight_method,
    filename = filename
  )
}

#' Define a parameter estimation experiment
#' 
#' \code{defineExperiments} defines a set of experiments given as tidy data frame to the given model.
#' 
#' CoRC uses it's own methodology for defining experimental data for use with a COPASI model.
#' To this end it is required that experimental data be imported to R by the user and transformed to tidy data.
#' For help on data import and tidying see: \code{vignette("tidy-data", "tidyr")}.
#' This function adds required metadata to experimental data for use with CoRC.
#' 
#' @param experiment_type string
#' @param data list of tidy data frames
#' @eval paste0("@param types data column types as character vector
#' 
#' Allowed types of columns are: ", rox_print_v(exp_allowed_types), ".
#' 
#' Type 'time' is only allowed for time course experiments.")
#' @param mappings data column mappings as character vector
#' 
#' Expects value references.
#' 
#' If no mappings are given, column names can serve as mappings.
#' @eval paste0("@param weight_method string
#' 
#' Allowed methods: ", rox_print_v(exp_weight_methods), ".")
#' @param filename optional string
#' 
#' When adding the experiments to a COPASI model, this filename will be used.
#' In use cases, where experiments are only used temporarily, the filename is ignored.
#' @return copasi_exp object for input into related functions
#' @seealso \code{\link{addExperiments}} \code{\link{clearExperiments}}
#' @family parameter estimation
#' @export
defineExperiments <- copasi_exp

#' Add a parameter estimation experiment
#' 
#' @param ... objects as returned by \code{\link{defineExperiments}}.
#' Alternatively, the same parameters as used by \code{\link{defineExperiments}}.
#' @param model a model object
#' @seealso \code{\link{defineExperiments}} \code{\link{clearExperiments}}
#' @family parameter estimation
#' @export
addExperiments <- function(..., model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # flatten all args into a single list
  # this list can be used to check if the user gave only corc_opt_parm
  arglist_compact <- rlang::squash(unname(list(...)))
  
  # if not all are corc_opt_parm, try handing the args to define... so we get corc_opt_parm
  if (!every(arglist_compact, is.copasi_exp))
    arglist_compact <- list(defineExperiments(...))
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  c_experiment_set <- c_problem$getExperimentSet()
  
  map(
    arglist_compact,
    ~ {
      experiment_type <- .x %@% "experiment_type"
      experiments <- .x %@% "experiments"
      types <- .x %@% "types"
      mappings <- .x %@% "mappings"
      weight_method <- .x %@% "weight_method"
      filename <- .x %@% "filename"
      
      # Create experiment file
      assert_that(
        # If the user has set a manual filename, try to be safe and not overwrite anything
        !file.exists(filename) || grepl("CoRC_exp_[0-9a-f]+\\.txt$", filename),
        msg = paste0('Experiment file "', filename, '" already exists.')
      )
      assert_that(file.create(filename))
      readr::write_tsv(.x, filename)
      
      # make sure the file gets deleted on error
      tryCatch({
        # Construct individual experiments
        cl_experiments <- pmap(experiments, function(name, first_row, last_row, ...) {
          exp <- avert_gc(CExperiment(c_experiment_set, name))
          exp$setFirstRow(first_row)
          exp$setLastRow(last_row)
          exp
        })
        
        col_count <- ncol(.x)
        col_names <- colnames(.x)
        
        # Set all experiment's settings
        walk_swig(cl_experiments, "setHeaderRow", 1L)
        walk_swig(cl_experiments, "setFileName", normalizePathC(filename))
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
          map(mappings, xn_to_object, c_datamodel = c_datamodel) %>%
          map_chr(get_cn) %>%
          replace(is.na(.), "")
        
        # We have a list of object_maps and all need the same types and mappings
        types_ordered %>% iwalk(~ walk_swig(cl_object_maps, "setRole", .y - 1L, .x))
        mappings_ordered %>% iwalk(~ walk_swig(cl_object_maps, "setObjectCN", .y - 1L, .x))
        
        # Add all experiments to COPASI
        cl_experiments %>% walk(~ c_experiment_set$addExperiment(.x))
        
        # possibly compile
        # c_experiment_set$compile(problem$getMathContainer())
      },
      error = function(e) {
        file.remove(filename)
        base::stop(e)
      })
    }
  )

  invisible()
}

#' Clear all parameter estimation experiments
#' 
#' @param model a model object
#' @seealso \code{\link{addExperiments}}
#' @family parameter estimation
#' @export
clearExperiments <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  c_experiment_set <- c_problem$getExperimentSet()
  
  replicate(
    n = c_experiment_set$getExperimentCount(),
    expr = c_experiment_set$removeExperiment(0L)
  )
  
  invisible()
}

#' Clear all parameter estimation validation data.
#' 
#' @param model a model object
#' @family parameter estimation
#' @export
clearValidations <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  c_crossvalidation_set <- c_problem$getCrossValidationSet()
  
  replicate(
    n = c_crossvalidation_set$getExperimentCount(),
    expr = c_crossvalidation_set$removeExperiment(0L)
  )
  
  invisible()
}

pe_assemble_parameters <- function(parameters, c_problem) {
  assert_that(is.null(parameters) || is.list(parameters) && every(parameters, is.corc_opt_parm) || is.corc_opt_parm(parameters))
  
  if (is.corc_opt_parm(parameters))
    parameters <- list(parameters)
  
  if (is_empty(parameters))
    return(list())
  
  assert_that(
    c_problem$getOptItemSize() == 0L,
    msg = "This function can not set parameters if there are already parameters set in COPASI."
  )
  
  walk(parameters, validate_corc_opt_parm)
  
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
    msg = "This function can not set experiments if there are already experiments set in COPASI."
  )
  
  # force temporary experiment file names so they can be deleted safely
  if (temp_filenames)
    experiments <-
      experiments %>%
      map(~ {
        attr(.x, "filename") <- tempfile("CoRC_exp_", fileext = ".txt")
        # attr(.x, "filename") <- stringr::str_sub(tempfile("CoRC_exp_", "", ".txt"), 2L)
        .x
      })
  
  experiments
}

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
pe_assemble_settings <- function(randomize_start_values, create_parameter_sets, calculate_statistics, update_model, executable) {
  assert_that(
    is.null(randomize_start_values) || is.flag(randomize_start_values) && noNA(randomize_start_values),
    is.null(create_parameter_sets)  || is.flag(create_parameter_sets)  && noNA(create_parameter_sets),
    is.null(calculate_statistics)   || is.flag(calculate_statistics)   && noNA(calculate_statistics),
    is.null(update_model)           || is.flag(update_model)           && noNA(update_model),
    is.null(executable)             || is.flag(executable)             && noNA(executable)
  )
  
  list(
    randomize_start_values = randomize_start_values,
    create_parameter_sets  = create_parameter_sets,
    calculate_statistics   = calculate_statistics,
    update_model           = update_model,
    executable             = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
pe_assemble_method <- function(method, c_task) {
  if (is.null(method))
    return(list())
  
  assert_that(
    is.string(method) || is.list(method) && (is_empty(method) || !is.null(names(method))),
    msg = "method must be a string (a length one character vector) or a named list."
  )
  
  if (is_scalar_character(method))
    method <- list(method = method)
  
  if (hasName(method, "method")) {
    valid_methods <- names(.__E___CTaskEnum__Method)[c_task$getValidMethods() + 1L]
    # hack to get nice error message if method string is not accepted.
    method$method <- args_match(method$method, name = "method", valid_methods)
  }
  
  method
}

# gets full list of settings
pe_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  list(
    randomize_start_values = as.logical(c_problem$getRandomizeStartValues()),
    create_parameter_sets  = as.logical(c_problem$getCreateParameterSets()),
    calculate_statistics   = as.logical(c_problem$getCalculateStatistics()),
    update_model           = as.logical(c_task$isUpdateModel()),
    executable             = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
pe_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  if (!is.null(data$randomize_start_values))
    c_problem$setRandomizeStartValues(data$randomize_start_values)
  
  if (!is.null(data$create_parameter_sets))
    c_problem$setCreateParameterSets(data$create_parameter_sets)
  
  if (!is.null(data$calculate_statistics))
    c_problem$setCalculateStatistics(data$calculate_statistics)
  
  if (!is.null(data$update_model))
    c_task$setUpdateModel(data$update_model)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
pe_get_results <- function(c_task, settings) {
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
      "Objective Value"               = c_problem$getSolutionValue(),
      "Root Mean Square"              = c_problem$getRMS(),
      "Standard Deviation"            = c_problem$getStdDeviation(),
      "Validation Objective Value"    = c_problem$getCrossValidationSolutionValue(),
      "Validation Root Mean Square"   = c_problem$getCrossValidationRMS(),
      "Validation Standard Deviation" = c_problem$getCrossValidationSD(),
      "Function Evaluations"          = evals,
      "CPU Time [s]"                  = evaltime,
      "Evaluations/second [1/s]"      = evals / evaltime
    ) %>%
    transform_names()
  
  vals <- get_cv(c_problem$getSolutionVariables())
  std_dev <- get_cv(c_problem$getVariableStdDeviations())
  
  parameters <-
    tibble::tibble(
      "Parameter"               = get_key(cl_items),
      "Lower Bound"             = map_swig_dbl(cl_items, "getLowerBoundValue"),
      "Start Value"             = map_swig_dbl(cl_items, "getStartValue"),
      "Value"                   = vals,
      "Upper Bound"             = map_swig_dbl(cl_items, "getUpperBoundValue"),
      "Std. Deviation"          = std_dev,
      "Coeff. of Variation [%]" = abs(100 * std_dev / vals),
      "Gradient"                = get_cv(c_problem$getVariableGradients()),
      .rows = length(cl_items),
      .name_repair = transform_names_worker
    )
  
  experiments <-
    tibble::tibble(
      "Experiment"                = map_swig_chr(cl_experiments, "getObjectName"),
      "Objective Value"           = map_swig_dbl(cl_experiments, "getObjectiveValue"),
      "Root Mean Square"          = map_swig_dbl(cl_experiments, "getRMS"),
      "Error Mean"                = map_swig_dbl(cl_experiments, "getErrorMean"),
      "Error Mean Std. Deviation" = map_swig_dbl(cl_experiments, "getErrorMeanSD"),
      .rows = length(cl_experiments),
      .name_repair = transform_names_worker
    )
  
  fitted_values <-
    tibble::tibble(
      "Fitted Value"              = get_key(cl_dependent_obj),
      "Objective Value"           = get_cv(c_experiment_set$getDependentObjectiveValues()),
      "Root Mean Square"          = get_cv(c_experiment_set$getDependentRMS()),
      "Error Mean"                = get_cv(c_experiment_set$getDependentErrorMean()),
      "Error Mean Std. Deviation" = get_cv(c_experiment_set$getDependentErrorMeanSD()),
      .rows = length(cl_dependent_obj),
      .name_repair = transform_names_worker
    )
  
  correlation <- get_annotated_matrix(c_problem$getCorrelations())
  
  fim <- get_annotated_matrix(c_problem$getFisherInformation())
  fim_eigenvalues <- get_annotated_matrix(c_problem$getFisherInformationEigenvalues())
  fim_eigenvectors <- get_annotated_matrix(c_problem$getFisherInformationEigenvectors())
  
  fim_scaled <- get_annotated_matrix(c_problem$getScaledFisherInformation())
  fim_scaled_eigenvalues <- get_annotated_matrix(c_problem$getScaledFisherInformationEigenvalues())
  fim_scaled_eigenvectors <- get_annotated_matrix(c_problem$getScaledFisherInformationEigenvectors())
  
  protocol <- c_method$getMethodLog()$getPlainLog()
  
  list(
    settings                = settings,
    main                    = main,
    parameters              = parameters,
    experiments             = experiments,
    fitted_values           = fitted_values,
    correlation             = correlation,
    fim                     = fim,
    fim_eigenvalues         = fim_eigenvalues,
    fim_eigenvectors        = fim_eigenvectors,
    fim_scaled              = fim_scaled,
    fim_scaled_eigenvalues  = fim_scaled_eigenvalues,
    fim_scaled_eigenvectors = fim_scaled_eigenvectors,
    protocol                = protocol
  )
}
