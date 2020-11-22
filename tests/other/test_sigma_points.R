library(tidyverse)
library(CoRC)

alpha <- 0.5
beta <- 2
kappa <- 3
measurement_error <- 0.1


# Generate a fully fitted model

loadSBML(biomodels_url(id = 10, version = 2, format = "sbml"))

# define the parameters for COPASI
walk(
  parameter_strict(regex(c("V1$", "V2$", "V5$", "V6$", "V9$", "V10$"))),
  ~ {
    val <- getParameters(.x)$value
    addParameterEstimationParameter(parameter(.x, "Value"), start_value = val, lower_bound = val * 0.1, upper_bound = val * 1.9)
  }
)

setParameterEstimationSettings(
  method = "LevenbergMarquardt"
)

# read experimental data
data_experimental <-
  "data/MAPKdata.txt" %>%
  read_tsv(col_types = cols(
    time = col_double(),
    `MAPKKK-P` = col_double(),
    `MAPK-P` = col_double()
  )) %>%
  rename(Time = time, `Mos-P`= "MAPKKK-P", `Erk2-P` = "MAPK-P")

# define the experiments for COPASI
fit_experiments <- defineExperiments(
  data = data_experimental,
  type = c("time", "dependent", "dependent"),
  mapping = c(NA, "{[Mos-P]}", "{[Erk2-P]}"),
  weight_method = "mean_square"
)

result <-
  runSigmaPoint(
    alpha = alpha,
    beta = beta,
    kappa = kappa,
    var = measurement_error,
    experiments = fit_experiments,
    mean_fit_as_basis = TRUE
  )

result
