context("Quantity accessors")

loadExamples(1)

test_that("getSpecies gives expected results", {
  species <- getSpecies()
  expect_length(species, 12L)
  expect_true(nrow(species) == 6L)
  species_b <- getSpecies("B")
  expect_equal(species_b$initial_concentration, 3)
  expect_identical(species_b$concentration, NaN)
})

test_that("setSpecies works", {
  setSpecies("B", initial_concentration = 5.5)
  expect_equal(getSpecies("B")$initial_concentration, 5.5)
})

unloadAllModels()
