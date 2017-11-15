context("Entity accessors")

newModel()
test_that("get*() with empty model", {
  expect_equal(nrow(getSpecies()), 0L)
  expect_equal(nrow(getCompartments()), 0L)
  expect_equal(nrow(getGlobalQuantities()), 0L)
  expect_equal(nrow(getReactions()), 0L)
  expect_equal(nrow(getParameters()), 0L)
})

loadExamples(1)
setTimeCourseSettings(duration = 1e-6, intervals = 1, update_model = FALSE, method = "deterministic")
runTimeCourse()

test_that("getSpecies()", {
  species_df <- getSpecies()
  expect_length(species_df, 12)
  expect_true(nrow(species_df) == length(species()))
  
  species_df_b <- getSpecies("B")
  expect_equal(species_df_b$initial_concentration, 3)
  expect_equal(species_df_b$concentration, 3)
})

test_that("setSpecies()", {
  setSpecies("B", initial_concentration = 1.1)
  expect_equal(getSpecies("B")$initial_concentration, 1.1)
  
  setSpecies(regex("^B"), initial_concentration = 2.2)
  expect_equal(getSpecies("B")$initial_concentration, 2.2)
})

test_that("setSpecies() vectorization", {
  setSpecies(c("B", "B"), initial_concentration = c(3.3, 4.4))
  expect_equal(getSpecies("B")$initial_concentration, 4.4)
  
  nms <- rev(species())
  vls <- seq_along(nms)
  setSpecies(nms, initial_concentration = vls)
  expect_equal(getSpecies(nms)$initial_concentration, vls)
})

test_that("setSpecies() persistence", {
  setSpecies("B", initial_concentration = 5.5)
  runTimeCourse()
  expect_equal(getSpecies("B")$initial_concentration, 5.5)
})

loadExamples(3)
setTimeCourseSettings(duration = 1e-6, intervals = 1, update_model = FALSE, method = "deterministic")
runTimeCourse()

test_that("getCompartments()", {
  compartments_df <- getCompartments()
  expect_length(compartments_df, 8)
  expect_true(nrow(compartments_df) == length(compartment()))
  
  compartments_df_c <- getCompartments("Ca")
  expect_equal(compartments_df_c$initial_size, 1)
  expect_equal(compartments_df_c$size, 1)
})

test_that("setCompartments()", {
  setCompartments("Ca", initial_size = 1.1)
  expect_equal(getCompartments("Ca")$initial_size, 1.1)
  
  setCompartments(regex(".*a$"), initial_size = 2.2)
  expect_equal(getCompartments("Ca")$initial_size, 2.2)
})

test_that("setCompartments() vectorization", {
  setCompartments(c("Ca", "Ca"), initial_size = c(3.3, 4.4))
  expect_equal(getCompartments("Ca")$initial_size, 4.4)
  
  nms <- rev(compartment())
  vls <- seq_along(nms)
  setCompartments(nms, initial_size = vls)
  expect_equal(getCompartments(nms)$initial_size, vls)
})

test_that("setCompartments() persistence", {
  setCompartments("Ca", initial_size = 5.5)
  runTimeCourse()
  expect_equal(getCompartments("Ca")$initial_size, 5.5)
})

test_that("getGlobalQuantities()", {
  quantities_df <- getGlobalQuantities()
  expect_length(quantities_df, 8)
  expect_true(nrow(quantities_df) == length(quantity()))
  
  quantities_df_b <- getGlobalQuantities("Ca")
  expect_equal(quantities_df_b$initial_value, 1)
  expect_identical(quantities_df_b$value, 1)
})

test_that("setGlobalQuantities()", {
  setGlobalQuantities("Ca", initial_value = 1.1)
  expect_equal(getGlobalQuantities("Ca")$initial_value, 1.1)
  
  setGlobalQuantities(regex("Ca$"), initial_value = 2.2)
  expect_equal(getGlobalQuantities("Ca")$initial_value, 2.2)
})

test_that("setGlobalQuantities() vectorization", {
  setGlobalQuantities(c("Ca", "Ca"), initial_value = c(3.3, 4.4))
  expect_equal(getGlobalQuantities("Ca")$initial_value, 4.4)
  
  nms <- rev(quantity())
  vls <- seq_along(nms)
  setGlobalQuantities(nms, initial_value = vls)
  expect_equal(getGlobalQuantities(nms)$initial_value, vls)
})

test_that("setGlobalQuantities() persistence", {
  setGlobalQuantities("Ca", initial_value = 5.5)
  runTimeCourse()
  expect_equal(getGlobalQuantities("Ca")$initial_value, 5.5)
})

test_that("getParameters()", {
  parameters_df <- getParameters()
  expect_length(parameters_df, 5)
  expect_true(nrow(parameters_df) == length(parameter()))
  
  parameters_df_b <- getParameters("v")
  expect_equal(parameters_df_b$value, 0.1)
  expect_identical(parameters_df_b$mapping, NA_character_)
})

test_that("setParameters()", {
  setParameters("v", value = 1.1)
  expect_equal(getParameters("v")$value, 1.1)
  
  setParameters(regex("^.Ca.\\.k\\d$"), value = 2.2)
  expect_equal(getParameters("(Ca).k1")$value, 2.2)
})

test_that("setParameters() vectorization", {
  setParameters(c("v", "v"), value = c(3.3, 4.4))
  expect_equal(getParameters("v")$value, 4.4)
  
  nms <- rev(parameter())
  vls <- seq_along(nms)
  setParameters(nms, value = vls)
  expect_equal(getParameters(nms)$value, vls)
})

test_that("setParameters() persistence", {
  setParameters("v", value = 5.5)
  runTimeCourse()
  expect_equal(getParameters("v")$value, 5.5)
  
  setParameters("-> Ca).k1", value = 6.6)
  runTimeCourse()
  expect_equal(getParameters("-> Ca).k1")$value, 6.6)
})

unloadAllModels()
clearCustomKineticFunctions()
