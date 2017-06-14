context("Quantity accessors")

models <- loadExamples()

test_that("getSpecies gives expected results", {
  species <- getSpecies(models$brusselator)
  expect_equal_to_reference(species, "brusselator_species.rds")
  species <- getSpecies(models$chemotaxis)
  expect_equal_to_reference(species, "chemotaxis_species.rds")
})

test_that("setSpecies functions", {
  expect_silent(setSpecies(getSpecies(models$brusselator)))
  expect_silent(setSpecies(getSpecies(models$chemotaxis)))
})

unloadAllModels()
