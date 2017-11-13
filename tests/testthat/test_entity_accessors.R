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

test_that("getSpecies()", {
  species_df <- getSpecies()
  expect_length(species_df, 12)
  expect_true(nrow(species_df) == length(species()))
  
  species_df_b <- getSpecies("B")
  expect_equal(species_df_b$initial_concentration, 3)
  expect_identical(species_df_b$concentration, NaN)
})

test_that("setSpecies()", {
  setSpecies("B", initial_concentration = 1.1)
  expect_equal(getSpecies("B")$initial_concentration, 1.1)
  
  setSpecies(regex("^B"), initial_concentration = 2.2)
  expect_equal(getSpecies("B")$initial_concentration, 2.2)
})

test_that("setSpecies() persistence", {
  setSpecies("B", initial_concentration = 3.3)
  runTimeCourse()
  expect_equal(getSpecies("B")$initial_concentration, 3.3)
})

test_that("getCompartments()", {
  compartments_df <- getCompartments()
  expect_length(compartments_df, 8)
  expect_true(nrow(compartments_df) == length(compartment()))
  
  compartments_df_c <- getCompartments("compartment")
  expect_equal(compartments_df_c$initial_size, 1)
  expect_equal(compartments_df_c$size, 1)
})

test_that("setCompartments()", {
  setCompartments("compartment", initial_size = 1.1)
  expect_equal(getCompartments("compartment")$initial_size, 1.1)
  
  setCompartments(regex("^com.*ent$"), initial_size = 2.2)
  expect_equal(getCompartments("compartment")$initial_size, 2.2)
})

test_that("setCompartments() persistence", {
  setCompartments("compartment", initial_size = 3.3)
  runTimeCourse()
  expect_equal(getCompartments("compartment")$initial_size, 3.3)
})

loadExamples(3)
setTimeCourseSettings(duration = 1e-6, intervals = 1, update_model = FALSE, method = "deterministic")

test_that("getGlobalQuantities()", {
  quantities_df <- getGlobalQuantities()
  expect_length(quantities_df, 8)
  expect_true(nrow(quantities_df) == length(quantity()))
  
  quantities_df_b <- getGlobalQuantities("Ca")
  expect_equal(quantities_df_b$initial_value, 1)
  expect_identical(quantities_df_b$value, NaN)
})

test_that("setGlobalQuantities()", {
  setGlobalQuantities("Ca", initial_value = 1.1)
  expect_equal(getGlobalQuantities("Ca")$initial_value, 1.1)
  
  setGlobalQuantities(regex("Ca$"), initial_value = 2.2)
  expect_equal(getGlobalQuantities("Ca")$initial_value, 2.2)
})

test_that("setGlobalQuantities() persistence", {
  setGlobalQuantities("Ca", initial_value = 3.3)
  runTimeCourse()
  expect_equal(getGlobalQuantities("Ca")$initial_value, 3.3)
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

test_that("setParameters() persistence", {
  setParameters("v", value = 4.4)
  runTimeCourse()
  expect_equal(getParameters("v")$value, 4.4)
  
  setParameters("-> Ca).k1", value = 5.5)
  runTimeCourse()
  expect_equal(getParameters("-> Ca).k1")$value, 5.5)
})

unloadAllModels()
clearCustomKineticFunctions()
