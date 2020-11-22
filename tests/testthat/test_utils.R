context("Utility Tests")

test_that("map_parallel works", {
  cl <- parallel::makeCluster(1)
  expect_identical(map_parallel(cl = cl, .x = 1:10, .f = force, chunk.size = 0), as.list(1:10))
  expect_identical(map_parallel(cl = cl, .x = 1:10, .f = force, chunk.size = 1), as.list(1:10))
  expect_identical(map_parallel(cl = cl, .x = 1:10, .f = force, chunk.size = 20), as.list(1:10))
  parallel::stopCluster(cl)
})

test_that("biomodels_url success", {
  expect_equal(
    biomodels_url(id = 1, format = "sbml"),
    "https://www.ebi.ac.uk/biomodels/model/download/BIOMD0000000001?filename=BIOMD0000000001_url.xml"
  )
  expect_equal(
    biomodels_url(id = 329, version = 2, format = "sbml"),
    "https://www.ebi.ac.uk/biomodels/model/download/BIOMD0000000329.2?filename=BIOMD0000000329_url.xml"
  )
  expect_equal(
    biomodels_url(id = 1, format = "omex"),
    "https://www.ebi.ac.uk/biomodels/model/download/BIOMD0000000001"
  )
  expect_equal(
    biomodels_url(id = 329, version = 2, format = "omex"),
    "https://www.ebi.ac.uk/biomodels/model/download/BIOMD0000000329.2"
  )
})

test_that("biomodels_url failure", {
  expect_error(biomodels_url(id = NA))
  expect_error(biomodels_url(id = 329, format = "cps"))
  expect_error(biomodels_url(id = 329, version = -2))
  expect_error(biomodels_url(id = -329))
})
