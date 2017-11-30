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

test_that("newEvent() success", {
  newEvent("s1", trigger_expression = FALSE)
  newEvent("s2", trigger_expression = FALSE, assignment_target = character(), assignment_expression = character())
  newEvent("s3", trigger_expression = FALSE, assignment_target = character(), assignment_expression = NULL)
  newEvent("s4", trigger_expression = FALSE, assignment_target = NULL, assignment_expression = character())
  newEvent("s5", trigger_expression = FALSE, assignment_target = quantity()[1], assignment_expression = "1")
  newEvent("s6", trigger_expression = FALSE, assignment_target = quantity()[1:2], assignment_expression = c("1", "2"))
  deleteEvent(paste0("s", 1:6))
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
