context("Quantity accessors")

loadModel(system.file("extdata", "brusselator.cps", package = "rCopasi"))

test_that("getSpecies gives expected results", {
  expect_is(getSpecies(), "tbl_df")
})

test_that("setSpecies functions", {
  expect_silent(setSpecies(getSpecies()))
})
