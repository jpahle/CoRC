context("Task TimeCourse")

loadExamples(1)
setTimeCourseSettings(duration = 1e-6, intervals = 1, update_model = FALSE, method = "deterministic")

test_that("runTimeCourse()", {
  TC <- runTimeCourse()
  expect_length(TC, 5)
  
  expect_is(TC$settings, "list")
  
  expect_length(TC$result, 3)
  expect_identical(nrow(TC$result), 2L)
  
  expect_identical(length(TC$result), length(TC$result_number))
  expect_identical(nrow(TC$result), nrow(TC$result_number))
  
  expect_length(TC$column_keys, length(TC$result))
  
  expect_type(TC$units$time, "character")
  expect_type(TC$units$concentration, "character")
})

test_that("setTimeCourseSettings() method", {
  expect_error(setTimeCourseSettings(method = "..."))
  setTimeCourseSettings(method = list())
  expect_error(setTimeCourseSettings(method = list(".")))
})

test_that("autoplot.copasi_ts()", {
  TC <- runTimeCourse()
  if (requireNamespace("ggplot2", quietly = T)) {
    expect_s3_class(autoplot.copasi_ts(TC), "ggplot")
  } else {
    expect_error(autoplot.copasi_ts(TC))
    warning("Skipping autoplot test because package 'ggplot2' is missing")
  }
})

unloadAllModels()
clearCustomKineticFunctions()
