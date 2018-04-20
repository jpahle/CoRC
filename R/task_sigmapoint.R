#' Run sigma point method
#'
#' \code{runSigmaPoint} runs parallel parameter estimations as dictated by the sigma point method and returns the results in a list.
#'
#' WARNING: TODO: This implementation of the sigma point method is almost completely untested.
#' 
#' Uses the current parameter estimation settings as defined in the model, with the exception of experiments.
#' 
#' As described in:\cr
#' Schenkendorf, R., Kremling, A., & Mangold, M. (2009).\cr
#' Optimal experimental design with the sigma point method.\cr
#' IET Systems Biology, 3(1), 10–23.\cr
#' https://doi.org/10.1049/iet-syb:20080094
#'
#' @param alpha number
#' @param beta number
#' @param kappa count
#' @param var Measurement variance, as numeric.
#' Can be a scalar value for variance on all measurements.
#' A vector for variance per dependent experimental column or all measurements individually.
#' A matrix for full covariance matrix.
#' @param experiments Object of type copasi_exp as returned by \code{\link{defineExperiments}}.
#' If it contains a single experiment, the values are understood to be mean values.
#' If it contains multiple experiments, the experiments have to have identical independent columns.
#' @param mean_fit_as_basis Whether to initially fit to the mean experimental data and set those fitted parameter values as starting values.
#' Given as flag.
#' @param cl Either object of type 'cluster' as used in the parallel package, or a count of threads to use, or \code{NULL} to automatically use all cores on current machine.
#' @param model A model object.
#' @return A list of results.
#' \itemize{
#'   \item \code{$fit_task_results} is a list of all parameter estimation results gathered in this assay.
#'   \item \code{$sp_means} is the result of term (28).
#'   \item \code{$sp_cov_matrix} is the result of term (29).
#'   \item \code{$param_bias} is the result of term (33).
#'   \item \code{$param_cov_matrix} is the result of term (34).
#' }
#' @family sigma point method
#' @export
runSigmaPoint <- function(alpha = 0.5, beta = 2, kappa = 3, var = NULL, experiments, mean_fit_as_basis = TRUE, cl = NULL, model = getCurrentModel()) {
  # TODO
  warning("This implementation of the sigma point method is almost completely untested.")
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.number(alpha),
    is.number(beta),
    is.count(kappa),
    is.null(var) || is.numeric(var),
    is.copasi_exp(experiments),
    is.flag(mean_fit_as_basis)
  )
  
  c_task <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_problem <- as(c_task$getProblem(), "_p_CFitProblem")
  
  data <- experiments
  types <- data %@% "types"
  experiments <- data %@% "experiments"
  dep_cols <- which(types == "dependent")
  ign_cols <- which(types == "ignore")
  dep_col_count <- length(dep_cols)
  data_dep <- data[dep_cols]
  data_indep <- data[-c(dep_cols, ign_cols)]
  exp_rows <- pmap(experiments, function(first_row, last_row, ...) first_row:last_row)
  exp_count <- nrow(experiments)
  
  assert_that(every(data_dep, is.numeric), msg = "All experiment variables of type `dependent` must be numeric.")
  assert_that(noNA(data_dep), msg = "Experiment variables of type `dependent` must not contain <NA> values.")
  
  if (exp_count > 1L) {
    assert_that(is.null(var), msg = "Argument `var` can only be supplied with a single set of experimental data.")
    
    # control, that all non-dependent data is identical for each experiment
    first_exp_indep <- data_indep[exp_rows[[1L]], ]
    for (i in 2L:exp_count) {
      assert_that(
        isTRUE(dplyr::all_equal(
          first_exp_indep,
          data_indep[exp_rows[[i]], ]
        )),
        msg = paste0("Independent data of experiment `", experiments$name[i], "` has to be equal to those of other experiments within the set.")
      )
    }
  } else {
    assert_that(!is.null(var), msg = "Argument `var` must be given if experimental data is only mean data.")
  }
  
  # prepare the cluster
  close_cl <- FALSE
  if (is.null(cl) || is.count(cl)) {
    close_cl <- TRUE
    cl <- parallel::makeCluster(cl %||% parallel::detectCores())
  } else if (!inherits(cl, "cluster")) {
    stop("Argument `cl` has to be a number of threads or a parallel cluster as created with the parallel package.")
  }
  
  if (c_problem$getExperimentSet()$getExperimentCount() != 0L) {
    warnings("Experimental data present in COPASI. Ignoring it for this task.")
  }
  if (c_problem$getCrossValidationSet()$getExperimentCount() != 0L) {
    warnings("Cross-validation data present in COPASI. Ignoring it for this task.")
  }
  
  # clean up model for the task
  # use temporary model for that
  # -> clean_model string
  c_temp_datamodel <- loadModelFromString(saveModelToString(model = c_datamodel))
  clearExperiments(model = c_temp_datamodel)
  clearValidations(model = c_temp_datamodel)
  setParameterEstimationSettings(update_model = FALSE, executable = FALSE, model = c_temp_datamodel)
  clean_model <- saveModelToString(model = c_temp_datamodel)
  unloadModel(model = c_temp_datamodel)
  
  # generate a full matrix of dependent data
  # extract dep colums from data frame and then procedurally merge data to matrix format
  data_dep_split <- map(exp_rows, ~ data_dep[.x, ])
  data_dep_matrix <-
    data_dep_split %>%
    map(flatten_dbl) %>%
    do.call(what = rbind)
  
  datapoint_count <- ncol(data_dep_matrix)
  
  data_dep_mean <- colMeans(data_dep_matrix)
  # generate cov matrix (Cy)
  if (exp_count > 1L) {
    data_dep_cov <- calc_cov_from_data(data_dep_matrix)
  } else {
    data_dep_cov <- calc_cov_from_var(data_dep_matrix, dep_col_count, var)
  }
  
  lambd <- alpha ^ 2 * (datapoint_count + kappa) - datapoint_count
  lambdterm <- sqrt(datapoint_count + lambd)
  
  # (24) - (26)
  sigma_points_matrix <- gen_sigma_points(data_dep_mean, data_dep_cov, lambdterm)
  assay_count <- nrow(sigma_points_matrix)
  
  # replicate the original data frame into a list of assays and replace the
  # dependent data with new dependent data from sigma points
  data_list <- perturb_data(data, dep_cols, sigma_points_matrix)

  # set up base for fitting
  # -> base_model string
  if (mean_fit_as_basis) {
    c_temp_datamodel <- loadModelFromString(clean_model)
    runParameterEstimation(update_model = TRUE, experiments = data_list[[1L]], model = c_temp_datamodel)
    
    base_model <- saveModelToString(model = c_temp_datamodel)
    unloadModel(model = c_temp_datamodel)
  } else {
    base_model <- clean_model
  }
  
  # see if CoRC is ready on cluster
  tryCatch(
    parallel::clusterEvalQ(cl = cl, CoRC:::assert_binaries()),
    error = function(e) stop("Can't load ", getPackageName(), " on cluster instances.")
  )
  
  # load model on all clusters
  parallel::clusterCall(
    cl = cl,
    function(model_string) {
      CoRC::loadModelFromString(model_string)
    },
    model_string = base_model
  )
  
  # use the full experiment list to acquire a result
  # use cluster because it has loaded base model ready
  result_init <-
    parallel::clusterApply(
      cl = cl,
      x = list(data),
      function(experiments) {
        CoRC::runParameterEstimation(experiments = experiments)
      }
    )
  result_init <- result_init[[1]]
  
  # do the full assay
  results <-
    map_parallel(
      cl = cl,
      .x = data_list,
      .f = ~ CoRC::runParameterEstimation(experiments = .x),
      chunk.size = 1
    )
  
  if (close_cl) {
    parallel::stopCluster(cl = cl)
  } else {
    # unload model on all clusters
    parallel::clusterEvalQ(cl = cl, CoRC::unloadModel())
  }
  
  # gather all fitted parameter values as matrix
  param_vals <-
    results %>%
    map(~
      .x$parameters %>%
        dplyr::select(.data$parameter, .data$value) %>%
        tidyr::spread(.data$parameter, .data$value)
    ) %>%
    dplyr::bind_rows()
  param_vals_matrix <- as.matrix(param_vals)
  
  # 0th mean weight in (30)
  m_weight_0 = lambd / (datapoint_count + lambd)
  # 0th covariance weight in (31)
  c_weight_0 = lambd / (datapoint_count + lambd) + 1L - (alpha ^ 2) + beta
  # ith weight in (32)
  i_weight = 1 / (2 * (datapoint_count + lambd))
  
  m_weight_vec <- c(m_weight_0, rep(i_weight, datapoint_count * 2L))
  c_weight_vec <- c(c_weight_0, rep(i_weight, datapoint_count * 2L))
  
  # term (28)
  param_vals_means <- colSums(m_weight_vec * param_vals_matrix)
  
  # rowwise subtraction of vector
  # subterm of (29)
  param_vals_matrix_sub_means <- sweep(param_vals_matrix, 2L, param_vals_means)
  
  # term (29)
  param_vals_cov_matrix <-
    # t(c_weight_vec * param_vals_matrix_sub_means) %*% param_vals_matrix_sub_means
    crossprod(sqrt(c_weight_vec) * param_vals_matrix_sub_means)
  
  # rownames(param_vals_cov_matrix) <- colnames(param_vals_cov_matrix)
  
  bias <- param_vals_means - param_vals_matrix[1L, ]
  
  cov_matrix <- tcrossprod(bias) + param_vals_cov_matrix
  
  list(
    fit_task_results = results,
    sp_means = param_vals_means,
    sp_cov_matrix = param_vals_cov_matrix,
    param_bias = bias,
    param_cov_matrix = cov_matrix
  )
}

#' @rdname runSigmaPoint
#' @export
runSP <- runSigmaPoint

# as in condor COPASI but not really anymore
# calc_cov_hoops <- function(data_dep_matrix, measurement_error) {
#   # Noise added to the data will be assumed to normally distributed within the measurement error with 95% confidence.
#   generate_noise_factor <- function() {
#     # noise_factor <- Inf
#     # Ensures that the generated data is not negative
#     # while (noise_factor > 1)
#       # noise_factor <- lambdterm * abs(rnorm(1L, mean = 0, sd = 1/1.96) * measurement_error)
#       (stats::rnorm(1L, mean = 0, sd = 1/1.96) * measurement_error) ^ 2
#     
#     # noise_factor
#   }
#   
#   datapoint_count <- ncol(data_dep_matrix)
#   
#   noise_matrix <-
#     matrix(
#       replicate(
#         datapoint_count ^ 2,
#         generate_noise_factor()
#       ),
#       nrow = datapoint_count
#     )
# }

# calc cov matrix form var arg
calc_cov_from_var <- function(data_dep_matrix, dep_col_count, var) {
  datapoint_count <- ncol(data_dep_matrix)
  
  if (is.matrix(var)) {
    assert_that(
      ncol(var) == datapoint_count,
      isSymmetric(var, tol = 0),
      msg = paste0("Matrices given in `var` must be symmetric and of size ", datapoint_count, " ^ 2 for the given experimental data.")
    )
    
    cov_matrix <- var
  } else {
    dim(var) <- NULL
    
    assert_that(
      is.scalar(var) ||
      length(var) == datapoint_count ||
      length(var) == dep_col_count,
      msg = "Vector given in `var` is of incompatible dimensions to the experimental data."
    )
    
    if (is.scalar(var))
      var <- rep(var, datapoint_count)
    else if (length(var) == dep_col_count)
      var <- rep(var, each = datapoint_count / dep_col_count)
    
    cov_matrix <- diag(var)
  }
  
  cov_matrix
}

# take list of exp dependent data and calc cov matrix from data
calc_cov_from_data <- function(data_dep_matrix) {
  stats::cov(data_dep_matrix)
}

gen_sigma_points <- function(means, cov_m, lambdterm) {
  datapoint_count <- length(means)
  
  cov_m_sqrt <- sqrt(abs(cov_m))
  
  assay_count <- datapoint_count * 2L + 1L
  
  # generate matrix of deltas (24-26)
  # cols: datapoints, rows: deltas
  sigma_point_delta <-
    rbind(
      t(rep(0, datapoint_count)),
      lambdterm * cov_m_sqrt,
      -lambdterm * cov_m_sqrt
    )
  
  # add means vector to each row to get final data
  sigma_points <- sweep(sigma_point_delta, 2L, means, "+")
  
  sigma_points
}

# for each row in sigma_points, replicate the original data once
# and perturb its dependent data columns according to the sigma points.
perturb_data <- function(data, dep_cols, sigma_points) {
  sigma_points %>%
    nrow() %>%
    seq_len() %>%
    # seperate all rows into individual list entries
    map(~ sigma_points[.x, ]) %>%
    # reshape those rows to matrices
    map(matrix, ncol = dep_cols) %>%
    # merge each of the matrices back into the original data
    map(replace, x = data, list = dep_cols)
}
