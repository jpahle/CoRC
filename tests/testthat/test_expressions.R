context("Expressions")

loadExamples(1)

test_that("getValue()", {
  expect_identical(getValue(""), NaN)
  expect_equal(getValue("1"), 1)
  expect_equal(getValue("1.1"), 1.1)
  expect_error(suppressWarnings(getValue("1,1")))
  expect_error(suppressWarnings(getValue("A")))
  expect_error(suppressWarnings(getValue("[A]")))
  expect_identical(getValue("{[A]}"), NaN)
  expect_equal(getValue("{[A]_0}"), 0.5)
  expect_equal(getValue("{[A]_0} + {[B]_0}"), 3.5)
})

test_that("getValue() boolean", {
  expect_equal(getValue("1 > 0"), 1)
  expect_equal(getValue("0 > 0"), 0)
  expect_equal(getValue("0 > 1"), 0)
})

test_that("getInitialValue()", {
  expect_identical(getInitialValue(""), NaN)
  expect_equal(getInitialValue("1"), 1)
  expect_equal(getInitialValue("1.1"), 1.1)
  expect_error(suppressWarnings(getInitialValue("1,1")))
  expect_error(suppressWarnings(getInitialValue("A")))
  expect_error(suppressWarnings(getInitialValue("[A]")))
  expect_equal(getInitialValue("{[A]}"), getInitialValue("{[A]_0}"))
  expect_equal(getInitialValue("{[A]} + {[B]_0}"), 3.5)
})

test_that("getInitialValue() boolean", {
  expect_equal(getInitialValue("1 > 0"), 1)
  expect_equal(getInitialValue("0 > 0"), 0)
  expect_equal(getInitialValue("0 > 1"), 0)
})

unloadAllModels()
clearCustomKineticFunctions()
