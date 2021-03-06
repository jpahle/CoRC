% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/task_parameterestimation.R
\name{defineParameterEstimationParameter}
\alias{defineParameterEstimationParameter}
\title{Define a parameter estimation parameter}
\usage{
defineParameterEstimationParameter(
  ref,
  start_value = (lower_bound + upper_bound)/2,
  lower_bound = 1e-06,
  upper_bound = 1e+06
)
}
\arguments{
\item{ref}{value reference}

\item{start_value}{start value}

\item{lower_bound}{lower value bound}

\item{upper_bound}{upper value bound}
}
\value{
corc_opt_parm object for input into related functions
}
\description{
Define a parameter estimation parameter
}
\seealso{
\code{\link{addParameterEstimationParameter}} \code{\link{clearParameterEstimationParameters}}
}
