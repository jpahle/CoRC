context("Entity accessors")

newModel()
test_that("get*() with empty model", {
  expect_equal(nrow(getSpecies()), 0L)
  expect_equal(nrow(getCompartments()), 0L)
  expect_equal(nrow(getGlobalQuantities()), 0L)
  expect_equal(nrow(getReactions()), 0L)
  expect_equal(nrow(getParameters()), 0L)
  expect_equal(nrow(getEvents()), 0L)
})

loadExamples(1)
setTimeCourseSettings(duration = 1e-6, intervals = 1, update_model = FALSE, method = "deterministic")
runTimeCourse()

test_that("getSpecies()", {
  species_df <- getSpecies()
  expect_length(species_df, 13)
  expect_true(nrow(species_df) == length(species()))
  
  species_df_b <- getSpecies("B")
  expect_equal(species_df_b$initial_concentration, 3)
  expect_equal(species_df_b$concentration, 3)
  expect_identical(species_df_b$unit, "mmol/ml")
})

test_that("setSpecies() concentrations", {
  setSpecies("B", initial_concentration = 1.1)
  expect_equal(getSpecies("B")$initial_concentration, 1.1)
  
  setSpecies("B", initial_concentration = NaN)
  expect_equal(getSpecies("B")$initial_concentration, NaN)
  
  setSpecies("B", initial_concentration = Inf)
  expect_equal(getSpecies("B")$initial_concentration, Inf)
  
  setSpecies(regex("^B"), initial_concentration = 2.2)
  expect_equal(getSpecies("B")$initial_concentration, 2.2)
  
  setSpecies("B", type = "assignment", expression = "1.1")
  expect_equal(getSpecies("B")$expression, "1.1")
  expect_error(setSpecies("B", type = "failure"))
  setSpecies("B", type = "reactions", initial_concentration = 1.1)
})

test_that("setSpecies() numbers", {
  setSpecies("B", initial_number = 1)
  expect_equal(getSpecies("B")$initial_number, 1)
  
  setSpecies("B", initial_number = NaN)
  expect_equal(getSpecies("B")$initial_number, NaN)
  
  setSpecies("B", initial_number = Inf)
  expect_equal(getSpecies("B")$initial_number, Inf)
  
  setSpecies(regex("^B"), initial_number = 2)
  expect_equal(getSpecies("B")$initial_number, 2)
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
  expect_length(compartments_df, 10)
  expect_true(nrow(compartments_df) == length(compartment()))
  
  compartments_df_c <- getCompartments("Ca1")
  expect_equal(compartments_df_c$initial_size, 1)
  expect_equal(compartments_df_c$size, 1)
  expect_equal(compartments_df_c$dimensionality, 2)
  expect_identical(charToRaw(compartments_df_c$unit), charToRaw("m²"))
})

test_that("setCompartments()", {
  setCompartments("Ca", initial_size = 1.1)
  expect_equal(getCompartments("Ca")$initial_size, 1.1)
  
  setCompartments("Ca", initial_size = NaN)
  expect_equal(getCompartments("Ca")$initial_size, NaN)
  
  setCompartments("Ca", initial_size = Inf)
  expect_equal(getCompartments("Ca")$initial_size, Inf)
  
  setCompartments(regex(".*a$"), initial_size = 2.2)
  expect_equal(getCompartments("Ca")$initial_size, 2.2)
  
  setCompartments("Ca", type = "assignment", expression = "1.1")
  expect_equal(getCompartments("Ca")$expression, "1.1")
  expect_error(setCompartments("Ca", type = "failure"))
  setCompartments("Ca", type = "fixed", initial_size = 1.1)
  
  setCompartments("Ca", dimensionality = 1)
  expect_equal(getCompartments("Ca")$dimensionality, 1)
  
  expect_error(setCompartments("Ca", dimensionality = NaN))
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
  expect_length(quantities_df, 9)
  expect_true(nrow(quantities_df) == length(quantity()))
  
  quantities_df_c <- getGlobalQuantities("Ca")
  expect_equal(quantities_df_c$initial_value, 1)
  expect_equal(quantities_df_c$value, 1)
  expect_identical(quantities_df_c$unit, "m^3")
})

test_that("setGlobalQuantities()", {
  setGlobalQuantities("Ca", initial_value = 1.1)
  expect_equal(getGlobalQuantities("Ca")$initial_value, 1.1)
  
  setGlobalQuantities("Ca", initial_value = NaN)
  expect_equal(getGlobalQuantities("Ca")$initial_value, NaN)
  
  setGlobalQuantities("Ca", initial_value = Inf)
  expect_equal(getGlobalQuantities("Ca")$initial_value, Inf)
  
  setGlobalQuantities(regex("Ca$"), initial_value = 2.2)
  expect_equal(getGlobalQuantities("Ca")$initial_value, 2.2)
  
  setGlobalQuantities("Ca", type = "assignment", expression = "1.1")
  expect_equal(getGlobalQuantities("Ca")$expression, "1.1")
  expect_error(setGlobalQuantities("Ca", type = "failure"))
  setGlobalQuantities("Ca", type = "fixed", initial_value = 1.1)
  
  setGlobalQuantities("Ca", unit = "s")
  expect_equal(getGlobalQuantities("Ca")$unit, "s")
  
  expect_error(expect_warning(setGlobalQuantities("Ca", unit = "failure")))
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
  
  parameters_df_v <- getParameters("v")
  expect_equal(parameters_df_v$value, 0.1)
  expect_identical(parameters_df_v$mapping, NA_character_)
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

test_that("getEvents()", {
  events_df <- getEvents()
  expect_length(events_df, 10)
  expect_true(nrow(events_df) == length(event()))
  
  events_df_c <- getEvents("Ca")
  expect_identical(events_df_c$fire_at_initial_time, FALSE)
  expect_identical(events_df_c$trigger_must_remain_true, TRUE)
  expect_identical(events_df_c$priority_expression, "")
  expect_identical(events_df_c$delayed, "no")
  expect_identical(events_df_c$delay_expression, "")
  expect_identical(events_df_c$assignment_target, list(c("Ca{Ca}", "Ca{inside}", "Ca{}")))
  expect_identical(events_df_c$assignment_expression, list(c("1", "1", "2.5")))
})

test_that("setEvents()", {
  setEvents("Ca", trigger_expression = FALSE)
  expect_identical(getEvents("Ca")$trigger_expression, "FALSE")
  expect_error(setEvents("Ca", trigger_expression = "1.1"))
  
  setEvents(regex("Ca$"), trigger_expression = "2.2 < 0")
  expect_identical(getEvents("Ca")$trigger_expression, "2.2 < 0")
  
  setEvents("Ca", delay = "assignment", delay_expression = "1.1")
  expect_identical(getEvents("Ca")$delay_expression, "1.1")
  expect_identical(getEvents("Ca")$delayed, "assignment")
  expect_error(setEvents("Ca", delay = "failure"))
  setEvents("Ca", delay = "no", delay_expression = "failure")
  expect_identical(getEvents("Ca")$delayed, "no")
  expect_identical(getEvents("Ca")$delay_expression, "")
})

test_that("setEvents() list columns", {
  events_old <- getEvents()
  events_empty <- events_old
  events_empty$assignment_target <- rep_along(event(), list(character()))
  events_empty$assignment_expression <- rep_along(event(), list(character()))
  setEvents(data = events_empty)
  expect_identical(events_empty, getEvents())
  setEvents(data = events_old)
  expect_identical(events_old, getEvents())
})

test_that("setEvents() vectorization", {
  setEvents(c("Ca", "Ca"), priority_expression = c(3.3, 4.4))
  expect_identical(getEvents("Ca")$priority_expression, "4.4")
  
  nms <- rev(event())
  vls <- seq_along(nms)
  setEvents(nms, priority_expression = vls)
  expect_identical(getEvents(nms)$priority_expression, as.character(vls))
})

test_that("setEvents() persistence", {
  setEvents("Ca", trigger_expression = "5.5 < 0")
  runTimeCourse()
  expect_identical(getEvents("Ca")$trigger_expression,  "5.5 < 0")
})

loadExamples(3)
setTimeCourseSettings(duration = 1e-6, intervals = 1, update_model = FALSE, method = "deterministic")
runTimeCourse()

test_that("data argument compatibility", {
  # TODO do all colums have to match exactly?
  species_old <- getSpecies()
  setSpecies(data = species_old)
  expect_identical(species_old[-9:-10], getSpecies()[-9:-10])
  
  compartments_old <- getCompartments()
  setCompartments(data = compartments_old)
  expect_identical(compartments_old, getCompartments())
  
  quantities_old <- getGlobalQuantities()
  setGlobalQuantities(data = quantities_old)
  expect_identical(quantities_old, getGlobalQuantities())
  
  reactions_old <- getReactions()
  setReactions(data = reactions_old)
  expect_identical(reactions_old, getReactions())
  
  parameters_old <- getParameters()
  setParameters(data = parameters_old)
  expect_identical(parameters_old, getParameters())
  
  events_old <- getEvents()
  setEvents(data = events_old)
  expect_identical(events_old, getEvents())
})

unloadAllModels()
clearCustomKineticFunctions()
