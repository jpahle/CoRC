context("Task Metabolic Control Analysis")

loadExamples(1)
setMetabolicControlAnalysisSettings(perform_steady_state_analysis = TRUE)

test_that("runMetabolicControlAnalysis()", {
  MCA <- runMetabolicControlAnalysis()
  expect_length(MCA, 8)
  
  expect_is(MCA$settings, "list")
  
  expect_identical(MCA$result_ss, "found")
  
  expect_is(MCA$elasticities_scaled, "matrix")
  expect_is(MCA$elasticities_unscaled, "matrix")
  expect_is(MCA$flux_control_coefficients_scaled, "matrix")
  expect_is(MCA$flux_control_coefficients_unscaled, "matrix")
  expect_is(MCA$concentration_control_coefficients_scaled, "matrix")
  expect_is(MCA$concentration_control_coefficients_unscaled, "matrix")
  
  var_count <- ncol(MCA$elasticities_scaled)
  react_count <- nrow(MCA$elasticities_scaled)
  
  expect_equal(ncol(MCA$elasticities_unscaled), var_count)
  expect_equal(nrow(MCA$elasticities_unscaled), react_count)
  
  expect_equal(ncol(MCA$flux_control_coefficients_scaled), react_count + 1L)
  expect_equal(nrow(MCA$flux_control_coefficients_scaled), react_count)
  
  expect_equal(ncol(MCA$flux_control_coefficients_unscaled), react_count)
  expect_equal(nrow(MCA$flux_control_coefficients_unscaled), react_count)
  
  expect_equal(ncol(MCA$concentration_control_coefficients_scaled), react_count + 1L)
  expect_equal(nrow(MCA$concentration_control_coefficients_scaled), var_count)
  
  expect_equal(ncol(MCA$concentration_control_coefficients_unscaled), react_count)
  expect_equal(nrow(MCA$concentration_control_coefficients_unscaled), var_count)
})

test_that("setMetabolicControlAnalysisSettings() method", {
  expect_error(setMetabolicControlAnalysisSettings(method = "..."))
  setMetabolicControlAnalysisSettings(method = list())
  expect_error(setMetabolicControlAnalysisSettings(method = list(".")))
})

unloadAllModels()
clearCustomKineticFunctions()
