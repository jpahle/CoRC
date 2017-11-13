context("Task SteadyState")

loadExamples(1)
setSteadyStateSettings(update_model = FALSE, method = list(resolution = 1e6))

test_that("runSteadyState()", {
  SS <- runSteadyState()
  expect_length(SS, 9)
  
  expect_is(SS$settings, "list")
  
  expect_identical(SS$result, "foundEquilibrium")
  
  expect_length(SS$species, 8)
  expect_identical(nrow(SS$species), 2L)
  
  expect_length(SS$compartments, 5)
  expect_identical(nrow(SS$compartments), 0L)
  
  expect_length(SS$global_quantities, 5)
  expect_identical(nrow(SS$global_quantities), 0L)
  
  expect_length(SS$reactions, 4)
  expect_identical(nrow(SS$reactions), 4L)
  
  expect_is(SS$jacobian_complete, "matrix")
  expect_is(SS$jacobian_reduced, "matrix")
  expect_type(SS$protocol, "character")
})

test_that("setSteadyStateSettings() method", {
  expect_error(setSteadyStateSettings(method = "..."))
  setSteadyStateSettings(method = list())
  expect_error(setSteadyStateSettings(method = list(".")))
})

unloadAllModels()
clearCustomKineticFunctions()
