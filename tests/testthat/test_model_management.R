context("Model management")

model_path <- function(x) {
  system.file("extdata", x, package = "CoRC", mustWork = TRUE)
}

test_that("loadExamples success", {
  examples <- loadExamples()
  expect_length(examples, 4)
  expect_s4_class(examples[[1]], "_p_CDataModel")
  walk(examples, unloadModel)
})

test_that("loadModel local file success", {
  expect_s4_class(loadModel(model_path("brusselator.cps")), "_p_CDataModel")
  unloadModel()
})

test_that("loadModel local file failure", {
  expect_error(loadModel("__fake_file.cps"))
})

test_that("loadModelFromString success", {
  model_string <- paste0(readLines(model_path("brusselator.cps")), collapse = "\n")
  expect_s4_class(loadModelFromString(model_string), "_p_CDataModel")
  unloadModel()
})

test_that("loadModelFromString failure", {
  expect_error(suppressWarnings(loadModelFromString("1\n2\n3")))
})

test_that("saveModelToString successs", {
  loadExamples(1)
  model_string <- saveModelToString()
  expect_type(model_string, "character")
  expect_false(is.na(model_string))
  expect_true(model_string != "")
  unloadModel()
})

test_that("loadSBML local file succcess", {
  expect_s4_class(loadSBML(model_path("Kummer2000_Ca.xml")), "_p_CDataModel")
  unloadModel()
})

test_that("loadSBML local file failure", {
  expect_error(loadSBML("__fake_file.xml"))
})

test_that("loadSBMLFromString success", {
  sbml_string <- paste0(readLines(model_path("Kummer2000_Ca.xml")), collapse = "\n")
  expect_s4_class(loadSBMLFromString(sbml_string), "_p_CDataModel")
  unloadModel()
})

test_that("loadSBMLFromString failure", {
  expect_error(suppressWarnings(loadSBMLFromString("1\n2\n3")))
})

test_that("saveSBMLToString successs", {
  loadExamples(1)
  sbml_string <- suppressWarnings(saveSBMLToString(3, 1))
  expect_type(sbml_string, "character")
  expect_false(is.na(sbml_string))
  expect_true(sbml_string != "")
  unloadModel()
})

unloadAllModels()
clearCustomKineticFunctions()
