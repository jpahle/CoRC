#' Run to steady state
#'
#' \code{runSteadyState} calculates the steady state and returns the results in a list.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param calculate_jacobian flag
#' @param perform_stability_analysis flag
#' @param update_model flag
#' @param executable flag
#' @param method list
#' @param model A model object.
#' @eval paste0("@return A list of results.
#' \\itemize{
#'   \\item \\code{$result} can be one of ", rox_print_v(names(.__E___CSteadyStateMethod__ReturnCode)), ".
#' }")
#' @family steady state
#' @export
runSteadyState <- function(calculate_jacobian = NULL, perform_stability_analysis = NULL, update_model = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- ss_assemble_settings(
    calculate_jacobian         = calculate_jacobian,
    perform_stability_analysis = perform_stability_analysis,
    update_model               = update_model,
    executable                 = executable
  )
  
  # does assertions
  method_settings <- ss_assemble_method(method)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
  c_model <- c_datamodel$getModel()
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  # save all previous settings
  if (do_settings)
    pre_settings <- ss_get_settings(c_task)
  if (do_method)
    pre_method_settings <- get_method_settings(c_method)
  
  tryCatch({
    # apply settings
    if (do_settings)
      ss_set_settings(settings, c_task)
    if (do_method)
      set_method_settings(method_settings, c_method)
    
    compile_and_check(c_model)
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # save current settings
    full_settings <- ss_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method)
    
    # run task
    process_task(c_task)
    
    # get results
    ret <- ss_get_results(c_task, full_settings)
  },
  finally = {
    # revert all settings
    if (do_settings)
      ss_set_settings(pre_settings, c_task)
    if (do_method)
      set_method_settings(pre_method_settings, c_method)
  })
  
  ret
}

#' Set steady state settings
#'
#' \code{setSteadyStateSettings} sets steady state task settings including method options.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param calculate_jacobian flag
#' @param perform_stability_analysis flag
#' @param update_model flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @family steady state
#' @export
setSteadyStateSettings <- function(calculate_jacobian = NULL, perform_stability_analysis = NULL, update_model = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- ss_assemble_settings(
    calculate_jacobian         = calculate_jacobian,
    perform_stability_analysis = perform_stability_analysis,
    update_model               = update_model,
    executable                 = executable
  )
  
  # does assertions
  method_settings <- ss_assemble_method(method)
  
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  ss_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method)
  
  invisible()
}

#' Get steady state settings
#'
#' \code{getSteadyStateSettings} gets steady state task settings including method options.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param model a model object
#' @return A list of steady state task settings including method options.
#' @family steady state
#' @export
getSteadyStateSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  ret <- ss_get_settings(c_task)
  ret$method <- get_method_settings(c_method)
  
  ret
}

#' @rdname runSteadyState
#' @export
runSS <- runSteadyState
#' @rdname setSteadyStateSettings
#' @export
setSS <- setSteadyStateSettings
#' @rdname getSteadyStateSettings
#' @export
getSS <- getSteadyStateSettings

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
ss_assemble_settings <- function(calculate_jacobian, perform_stability_analysis, update_model, executable) {
  assert_that(
    is.null(calculate_jacobian)         || is.flag(calculate_jacobian)         && noNA(calculate_jacobian),
    is.null(perform_stability_analysis) || is.flag(perform_stability_analysis) && noNA(perform_stability_analysis),
    is.null(update_model)               || is.flag(update_model)               && noNA(update_model),
    is.null(executable)                 || is.flag(executable)                 && noNA(executable)
  )
  
  list(
    calculate_jacobian = calculate_jacobian,
    perform_stability_analysis = perform_stability_analysis,
    update_model = update_model,
    executable = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
ss_assemble_method <- function(method) {
  if (is.null(method))
    return(list())
  
  assert_that(
    is.list(method) && (is_empty(method) || !is.null(names(method))),
    msg = "method must be a named list."
  )
  assert_that(!hasName(method, "method"))
  
  method
}

# gets full list of settings
ss_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CSteadyStateProblem")
  
  list(
    calculate_jacobian         = as.logical(c_problem$isJacobianRequested()),
    perform_stability_analysis = as.logical(c_problem$isStabilityAnalysisRequested()),
    update_model               = as.logical(c_task$isUpdateModel()),
    executable                 = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
ss_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CSteadyStateProblem")
  
  calculate_jacobian <- data$calculate_jacobian
  if (!is.null(calculate_jacobian)) {
    c_problem$setJacobianRequested(calculate_jacobian)
    if (!calculate_jacobian)
      c_problem$setStabilityAnalysisRequested(FALSE)
  }
  
  
  perform_stability_analysis <- data$perform_stability_analysis
  if (!is.null(perform_stability_analysis)) {
    if (perform_stability_analysis) {
      assert_that(
        as.logical(c_problem$isJacobianRequested()),
        msg = "`perform_stability_analysis` can only be set to `TRUE` in combination with `calculate_jacobian`."
      )
    }
    c_problem$setStabilityAnalysisRequested(perform_stability_analysis)
  }
  
  if (!is.null(data$update_model))
    c_task$setUpdateModel(data$update_model)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
ss_get_results <- function(c_task, settings) {
  c_model <- c_task$getObjectDataModel()$getModel()
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  result <- c_task$getResult()
  
  cl_metabs <- get_cdv(c_model$getMetabolites())
  # if no transition time, metabs aren't listed
  cl_metabs_ss <- discard(cl_metabs, is.na(map_swig_dbl(cl_metabs, "getTransitionTime")))
  
  species <-
    tibble::tibble(
      key               = get_key(cl_metabs_ss, is_species = TRUE),
      "Name"            = map_swig_chr(cl_metabs_ss, "getObjectName"),
      "Type"            = cl_metabs_ss %>% map_swig_chr("getStatus") %>% tolower(),
      "Concentration"   = map_swig_dbl(cl_metabs_ss, "getConcentration"),
      "Number"          = map_swig_dbl(cl_metabs_ss, "getValue"),
      "Rate"            = map_swig_dbl(cl_metabs_ss, "getConcentrationRate"),
      "Number Rate"     = map_swig_dbl(cl_metabs_ss, "getRate"),
      "Transition Time" = map_swig_dbl(cl_metabs_ss, "getTransitionTime"),
      .rows = length(cl_metabs_ss),
      .name_repair = transform_names_worker
    )
  
  cl_comps <-
    get_cdv(c_model$getCompartments()) %>%
    discard(map_swig_chr(., "getStatus") == "FIXED")
  
  compartments <-
    tibble::tibble(
      key                  = get_key(cl_comps),
      "Name"               = map_swig_chr(cl_comps, "getObjectName"),
      "Type"               = cl_comps %>% map_swig_chr("getStatus") %>% tolower(),
      "Size"               = map_swig_dbl(cl_comps, "getValue"),
      "Rate"               = map_swig_dbl(cl_comps, "getRate"),
      .rows = length(cl_comps),
      .name_repair = transform_names_worker
    )
  
  cl_quants <- get_cdv(c_model$getModelValues()) %>%
    discard(map_swig_chr(., "getStatus") == "FIXED")
  
  quantities <-
    tibble::tibble(
      key                  = get_key(cl_quants),
      "Name"               = map_swig_chr(cl_quants, "getObjectName"),
      "Type"               = cl_quants %>% map_swig_chr("getStatus") %>% tolower(),
      "Value"              = map_swig_dbl(cl_quants, "getValue"),
      "Rate"               = map_swig_dbl(cl_quants, "getRate"),
      .rows = length(cl_quants),
      .name_repair = transform_names_worker
    )
  
  cl_reacts <- get_cdv(c_model$getReactions())
  
  reactions <-
    tibble::tibble(
      key           = get_key(cl_reacts),
      "Name"        = map_swig_chr(cl_reacts, "getObjectName"),
      "Flux"        = map_swig_dbl(cl_reacts, "getFlux"),
      "Number Flux" = map_swig_dbl(cl_reacts, "getParticleFlux"),
      .rows = length(cl_reacts),
      .name_repair = transform_names_worker
    )
  
  jacobian_complete <- NULL
  jacobian_reduced <- NULL
  
  if (settings$calculate_jacobian) {
    jacobian_complete <- get_annotated_matrix(c_task$getJacobianAnnotated())
    jacobian_reduced <- get_annotated_matrix(c_task$getJacobianXAnnotated())
  }
  
  protocol <- c_method$getMethodLog()
  
  list(
    settings          = settings,
    result            = result,
    species           = species,
    compartments      = compartments,
    global_quantities = quantities,
    reactions         = reactions,
    jacobian_complete = jacobian_complete,
    jacobian_reduced  = jacobian_reduced,
    protocol          = protocol
  )
}
