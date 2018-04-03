context("Model properties")

loadExamples(2)

test_that("setModelName success", {
  setModelName("test")
  expect_identical(getModelName(), "test")
})

test_that("setTimeUnit success", {
  setTimeUnit("1")
  expect_identical(getTimeUnit(), "1")
  
  setTimeUnit("ms")
  expect_identical(getTimeUnit(), "ms")
  
  setTimeUnit("m/m * ps")
  expect_identical(getTimeUnit(), "ps")
  
  expect_error(expect_warning(setTimeUnit("mol")))
  expect_error(expect_warning(setTimeUnit("failure")))
})

test_that("setQuantityUnit success", {
  setQuantityUnit("1")
  expect_identical(getQuantityUnit(), "1")
  
  setQuantityUnit("#")
  expect_identical(getQuantityUnit(), "#")
  
  setQuantityUnit("mol")
  expect_identical(getQuantityUnit(), "mol")
  
  setQuantityUnit("m/m * pmol")
  expect_identical(getQuantityUnit(), "pmol")
  
  expect_error(expect_warning(setQuantityUnit("s")))
  expect_error(expect_warning(setQuantityUnit("failure")))
})

test_that("setVolumeUnit success", {
  setVolumeUnit("1")
  expect_identical(getVolumeUnit(), "1")
  
  setVolumeUnit("l")
  expect_identical(getVolumeUnit(), "l")
  
  setVolumeUnit("m^3")
  expect_identical(charToRaw(getVolumeUnit()), charToRaw("m³"))
  
  setVolumeUnit("mm³")
  expect_identical(charToRaw(getVolumeUnit()), charToRaw("mm³"))
  
  setVolumeUnit("m/m * pm^3")
  expect_identical(charToRaw(getVolumeUnit()), charToRaw("pm³"))
  
  expect_error(expect_warning(setVolumeUnit("m")))
  expect_error(expect_warning(setVolumeUnit("failure")))
})

test_that("setAreaUnit success", {
  setAreaUnit("1")
  expect_identical(getAreaUnit(), "1")
  
  setAreaUnit("m^2")
  expect_identical(charToRaw(getAreaUnit()), charToRaw("m²"))
  
  setAreaUnit("mm²")
  expect_identical(charToRaw(getAreaUnit()), charToRaw("mm²"))
  
  setAreaUnit("m/m * pm^2")
  expect_identical(charToRaw(getAreaUnit()), charToRaw("pm²"))
  
  expect_error(expect_warning(setAreaUnit("m")))
  expect_error(expect_warning(setAreaUnit("failure")))
})

test_that("setLengthUnit success", {
  setLengthUnit("1")
  expect_identical(getLengthUnit(), "1")
  
  setLengthUnit("m")
  expect_identical(getLengthUnit(), "m")
  
  setLengthUnit("mm")
  expect_identical(getLengthUnit(), "mm")
  
  setLengthUnit("m/m * pm")
  expect_identical(getLengthUnit(), "pm")
  
  expect_error(expect_warning(setLengthUnit("m²")))
  expect_error(expect_warning(setLengthUnit("failure")))
})

test_that("setInitialTime failure", {
  expect_error(setInitialTime(1))
})

test_that("getStoichiometryMatrix success", {
  m <- getStoichiometryMatrix()
  m_names <- dimnames(m)
  
  expect_equal(dim(m), c(11, 14))
  expect_equal(m[1,1], 0)
  expect_equal(m[1,5], 1)
  expect_identical(m_names[[1]][2], "T3")
  expect_identical(m_names[[2]][2], "(Che B phosph)")
})

test_that("getReducedStoichiometryMatrix success", {
  m <- getReducedStoichiometryMatrix()
  m_names <- dimnames(m)
  
  expect_equal(dim(m), c(7, 14))
  expect_equal(m[1,1], 0)
  expect_equal(m[1,5], 1)
  expect_identical(m_names[[1]][2], "T3")
  expect_identical(m_names[[2]][2], "(Che B phosph)")
})

test_that("getLinkMatrix success", {
  m <- getLinkMatrix()
  m_names <- dimnames(m)
  
  expect_equal(dim(m), c(11, 7))
  expect_equal(m[1,1], 1)
  expect_equal(m[1,2], 0)
  expect_identical(m_names[[1]][2], "T3")
  expect_identical(m_names[[2]][3], "CheY")
})

unloadAllModels()
clearCustomKineticFunctions()
