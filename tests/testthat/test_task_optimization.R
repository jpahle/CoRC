context("Task Optimization")

loadExamples(4)
setOptimizationSettings(update_model = FALSE, method = "LevenbergMarquardt")

test_that("runOptimization()", {
  OPT <- runOptimization()
  expect_length(OPT, 4)
  
  expect_is(OPT$settings, "list")
  
  expect_is(OPT$main, "list")
  expect_gt(OPT$main$objective_value, 0)
  expect_gt(OPT$main$function_evaluations, 0)
  expect_gt(OPT$main$cpu_time_s, 0)
  expect_gt(OPT$main$evaluations_second_1_s, 0)
  
  expect_length(OPT$parameters, 6)
  expect_equal(nrow(OPT$parameters), 2L)
  
  expect_is(OPT$protocol, "character")
})

test_that("setOptimizationSettings() method", {
  expect_error(setOptimizationSettings(method = "..."))
  setOptimizationSettings(method = list())
  expect_error(setOptimizationSettings(method = list(".")))
})

unloadAllModels()
clearCustomKineticFunctions()
