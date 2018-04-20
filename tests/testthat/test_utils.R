context("Utility Tests")

test_that("map_parallel works", {
  cl <- parallel::makeCluster(1)
  expect_identical(map_parallel(cl = cl, .x = 1:10, .f = force, chunk.size = 0), as.list(1:10))
  expect_identical(map_parallel(cl = cl, .x = 1:10, .f = force, chunk.size = 1), as.list(1:10))
  expect_identical(map_parallel(cl = cl, .x = 1:10, .f = force, chunk.size = 20), as.list(1:10))
  parallel::stopCluster(cl)
})
