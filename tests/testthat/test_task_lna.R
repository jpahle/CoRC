context("Task Linear Noise Approximation")

loadExamples(3)
deleteEvent(event())
setLinearNoiseApproximationSettings(method = list(resolution = 1e-3))

test_that("runLinearNoiseApproximation()", {
  LNA <- runLinearNoiseApproximation()
  expect_length(LNA, 6)
  
  expect_is(LNA$settings, "list")
  
  expect_identical(LNA$result_ss, "found")
  expect_identical(LNA$result_lna, "allNeg")
  
  expect_is(LNA$covariance_matrix, "matrix")
  expect_is(LNA$covariance_matrix_reduced, "matrix")
  expect_is(LNA$b_matrix_reduced, "matrix")
})

test_that("setLinearNoiseApproximationSettings() method", {
  expect_error(setLinearNoiseApproximationSettings(method = "..."))
  setLinearNoiseApproximationSettings(method = list())
  expect_error(setLinearNoiseApproximationSettings(method = list(".")))
})

unloadAllModels()
clearCustomKineticFunctions()
