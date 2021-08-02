context("Task TimeCourse")

loadExamples(1)
setTimeCourseSettings(duration = 1e-6, intervals = 1, update_model = FALSE, method = "deterministic")

test_that("runTimeCourse()", {
  TC <- runTimeCourse()
  expect_length(TC, 6)
  
  expect_is(TC$settings, "list")
  
  expect_length(TC$result, 3)
  expect_identical(nrow(TC$result), 2L)
  
  expect_identical(length(TC$result), length(TC$result_number))
  expect_identical(nrow(TC$result), nrow(TC$result_number))
  
  expect_length(TC$column_keys, length(TC$result))
  
  expect_type(TC$units$time, "character")
  expect_type(TC$units$concentration, "character")
  
  expect_true(hasName(TC, "task_error"))
  expect_null(TC$task_error)
})

test_that("runTimeCourse() failure", {
  expect_error(expect_warning(runTimeCourse(duration = 3000, intervals = 3)))
  
  expect_warning(TC <- runTimeCourse(duration = 3000, intervals = 3, soft_error = TRUE))
  expect_length(TC, 6)
  
  expect_length(TC$result, 3)
  expect_identical(nrow(TC$result), 2L)
  
  expect_identical(length(TC$result), length(TC$result_number))
  expect_identical(nrow(TC$result), nrow(TC$result_number))
  
  expect_length(TC$column_keys, length(TC$result))
  
  expect_match(TC$task_error, ".+")
})

test_that("runTimeCourse() automatic iterations", {
  intervals <- 1L
  TC <- runTimeCourse(intervals = intervals, automatic_intervals = FALSE)
  
  expect_identical(TC$setting$automatic_intervals, FALSE)
  expect_equal(nrow(TC$result), intervals + 1L)
  
  TC <- runTimeCourse(intervals = intervals, automatic_intervals = TRUE)
  
  expect_identical(TC$setting$automatic_intervals, TRUE)
  expect_gt(nrow(TC$result), intervals + 1L)
})

test_that("setTimeCourseSettings() method", {
  expect_error(setTimeCourseSettings(method = "..."))
  setTimeCourseSettings(method = list())
  expect_error(setTimeCourseSettings(method = list(".")))
})

test_that("autoplot.copasi_ts()", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tidyr")
  
  TC <- runTimeCourse()
  expect_s3_class(autoplot.copasi_ts(TC), "ggplot")
  expect_s3_class(autoplot.copasi_ts(TC, use_concentrations = FALSE), "ggplot")
  expect_error(autoplot.copasi_ts(TC, "failure"))
})

test_that("runaway stochastic timecourse abort", {
  newModel()
  
  comp <- newCompartment("A")
  newSpecies("A", compartment = "A", initial_number = 1)
  newReaction("A -> 2 A", mappings = list(k1 = 1))
  
  setTimeCourseSettings(duration = 30, dt = 1, method = list(method = "directMethod", max_internal_steps = 1e5))
  
  expect_warning(expect_error(runTimeCourse()), regexp = "Internal step limit exceeded\\.")
  
  abort_time <- 1e9
  newEvent("abort", trigger_expression = "{A.ParticleNumber} > 2000", assignment_target = "Time", assignment_expression = abort_time)
  tc <- runTimeCourse()
  
  expect_identical(tail(tc$result_number$Time, n = 1), abort_time)
})

unloadAllModels()
clearCustomKineticFunctions()
