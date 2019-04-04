context("Model building")

newModel()

test_that("basic building", {
  newCompartment("a")
  newCompartment("b")
  newCompartment("c")
  expect_warning(newSpecies("a"))
  newSpecies("b", compartment = "b")
  newSpecies("c", compartment = "c")
  newGlobalQuantity("a")
  newGlobalQuantity("b")
  newGlobalQuantity("c")
  newReaction("a -> b")
  newReaction("b -> c")
  newReaction("c -> a")
  newEvent("a", trigger_expression = FALSE)
  newEvent("b", trigger_expression = FALSE)
  newEvent("c", trigger_expression = FALSE)
  
  expect_length(compartment(), 3)
  expect_length(species(), 3)
  expect_length(quantity(), 3)
  expect_length(reaction(), 3)
  expect_length(event(), 3)
})

test_that("newSpecies() success", {
  newSpecies("s1", compartment = compartment()[1])
  expect_equal(getSpecies("s1")$initial_concentration, 1)
  
  newSpecies("s2", compartment = compartment()[1], initial_concentration = 1.1)
  expect_equal(getSpecies("s2")$initial_concentration, 1.1)
  
  newSpecies("s3", compartment = compartment()[1], initial_concentration = NaN)
  expect_equal(getSpecies("s3")$initial_concentration, NaN)
  
  newSpecies("s4", compartment = compartment()[1], initial_concentration = Inf)
  expect_equal(getSpecies("s4")$initial_concentration, Inf)
  
  newSpecies("s5", compartment = compartment()[1], initial_number = 2.2)
  expect_equal(getSpecies("s5")$initial_number, 2.2)
  
  newSpecies("s6", compartment = compartment()[1], initial_expression = 3.3)
  expect_equal(getSpecies("s6")$initial_concentration, 3.3)
  
  newSpecies("s7", compartment = compartment()[1], initial_concentration = 4.4, initial_number = 5.5)
  expect_equal(getSpecies("s7")$initial_number, 5.5)
  
  newSpecies("s8", compartment = compartment()[1], initial_concentration = 6.6, initial_number = 7.7, initial_expression = 8.8)
  expect_equal(getSpecies("s8")$initial_concentration, 8.8)
  
  deleteSpecies(paste0("s", 1:8))
})

test_that("newCompartment() success", {
  newCompartment("s1")
  expect_equal(getCompartments("s1")$initial_size, 1)
  
  newCompartment("s2", dimensionality = 2)
  compartments_df_2 <- getCompartments("s2")
  expect_equal(compartments_df_2$dimensionality, 2)
  expect_identical(charToRaw(compartments_df_2$unit), charToRaw("m²"))
  
  newCompartment("s3", initial_size = 1.1)
  expect_equal(getCompartments("s3")$initial_size, 1.1)
  
  newCompartment("s4", initial_size = NaN)
  expect_equal(getCompartments("s4")$initial_size, NaN)
  
  newCompartment("s5", initial_size = Inf)
  expect_equal(getCompartments("s5")$initial_size, Inf)
  
  newCompartment("s6", initial_expression = 2.2)
  expect_equal(getCompartments("s6")$initial_size, 2.2)
  
  newCompartment("s7", initial_size = 3.3, initial_expression = 4.4)
  expect_equal(getCompartments("s7")$initial_size, 4.4)
  
  deleteCompartment(paste0("s", 1:7))
})

test_that("newGlobalQuantity() success", {
  newGlobalQuantity("s1")
  expect_equal(getGlobalQuantities("s1")$initial_value, 0)
  
  newGlobalQuantity("s2", unit = "ps")
  expect_identical(getGlobalQuantities("s2")$unit, "ps")
  
  newGlobalQuantity("s3", initial_value = 1.1)
  expect_equal(getGlobalQuantities("s3")$initial_value, 1.1)
  
  newGlobalQuantity("s4", initial_value = NaN)
  expect_equal(getGlobalQuantities("s4")$initial_value, NaN)
  
  newGlobalQuantity("s5", initial_value = Inf)
  expect_equal(getGlobalQuantities("s5")$initial_value, Inf)
  
  newGlobalQuantity("s6", initial_expression = 2.2)
  expect_equal(getGlobalQuantities("s6")$initial_value, 2.2)
  
  newGlobalQuantity("s7", initial_value = 3.3, initial_expression = 4.4)
  expect_equal(getGlobalQuantities("s7")$initial_value, 4.4)
  
  deleteGlobalQuantity(paste0("s", 1:7))
})

test_that("newKineticFunction() success", {
  newKineticFunction(
    "manual MM",
    "((Vf * substrate) / Kms - Vr * product / Kmp) / (1 + substrate / Kms + product / Kms)",
    parameters = list(substrate = "substrate", product = "product")
  )
  expect_identical(kinfunction("manual MM"), "FunctionDB.Functions[manual MM]")
})

test_that("newReaction() success", {
  newReaction("a = b", name = "r1", fun = "Reversible Michaelis-Menten")
  expect_identical(getReactions("r1")$rate_law, "FunctionDB.Functions[Reversible Michaelis-Menten]")
  
  newReaction(
    "a = b",
    name = "r2",
    fun = "manual MM",
    mapping = list(
      substrate = "a",
      product = "b",
      Vf = 0.1,
      Kms = 0.2,
      Vr = 0.3,
      Kmp = 0.4
    )
  )
  expect_identical(getReactions("r2")$rate_law, "FunctionDB.Functions[manual MM]")
  r2_mappings <- getReactionMappings("r2")
  expect_identical(r2_mappings$substrate, "a")
  expect_identical(r2_mappings$product, "b")
  expect_equal(r2_mappings$Vf, 0.1)
  expect_equal(r2_mappings$Kms, 0.2)
  expect_equal(r2_mappings$Vr, 0.3)
  expect_equal(r2_mappings$Kmp, 0.4)
  
  deleteReaction(paste0("r", 1:2))
})

test_that("newEvent() success", {
  newEvent("s1", trigger_expression = FALSE)
  newEvent("s2", trigger_expression = FALSE, assignment_target = character(), assignment_expression = character())
  newEvent("s3", trigger_expression = FALSE, assignment_target = character(), assignment_expression = NULL)
  newEvent("s4", trigger_expression = FALSE, assignment_target = NULL, assignment_expression = character())
  newEvent("s5", trigger_expression = FALSE, assignment_target = quantity()[1], assignment_expression = "1")
  newEvent("s6", trigger_expression = FALSE, assignment_target = quantity()[1:2], assignment_expression = c("1", "2"))
  expect_null(deleteEvent(paste0("s", 1:6)))
})

# this is an edge case where delayed will be set to "no" afterwards because of the empty expression. fine for now.
test_that("newEvent() empty delay_expression case", {
  e1 <- newEvent("e1", trigger_expression = FALSE, delayed = "assignment", delay_expression = "")
  expect_identical(getEvents(e1)$delayed, "no")
  setEvents(e1, delay_expression = "1")
  expect_identical(getEvents(e1)$delayed, "assignment")
  
  e2 <- newEvent("e2", trigger_expression = FALSE, delayed = "calculation", delay_expression = "")
  expect_identical(getEvents(e2)$delayed, "no")
  setEvents(e2, delay_expression = "1")
  expect_identical(getEvents(e2)$delayed, "calculation")
  deleteEvent(c(e1, e2))
})
  
test_that("newEvent() errors", {
  expect_error(newEvent("failure"))
  expect_error(newEvent("failure", trigger_expression = FALSE, delayed = "no", delay_expression = "1"))
  expect_error(newEvent("failure", trigger_expression = FALSE, assignment_target = quantity()[c(1,1)], assignment_expression = c("1", "2")))
})

# I want to perform model deletion twice
currentModel <- getCurrentModel()
loadModelFromString(saveModelToString())

test_that("non-recursive destruction", {
  deleteEvent(event())
  deleteReaction(reaction())
  deleteGlobalQuantity(quantity())
  deleteSpecies(species())
  deleteCompartment(compartment())

  expect_length(compartment(), 0)
  expect_length(species(), 0)
  expect_length(quantity(), 0)
  expect_length(reaction(), 0)
  expect_length(event(), 0)
})

setCurrentModel(currentModel)
rm(currentModel)

test_that("recursive destruction", {
  expect_gt(length(species()), 0)
  
  deleteCompartment(compartment())
  deleteSpecies(species())
  deleteGlobalQuantity(quantity())
  deleteReaction(reaction())
  deleteEvent(event())

  expect_length(compartment(), 0)
  expect_length(species(), 0)
  expect_length(quantity(), 0)
  expect_length(reaction(), 0)
  expect_length(event(), 0)
})

unloadAllModels()
clearCustomKineticFunctions()
