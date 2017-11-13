context("Entity finders")

newModel()
test_that("entity finders with empty model", {
  expect_length(species(), 0)
  expect_length(compartment(), 0L)
  expect_length(quantity(), 0L)
  expect_length(reaction(), 0L)
  expect_length(parameter(), 0L)
})

loadExamples(3)

test_that("species()", {
  expect_length(species(), 6)
  expect_length(species("Ca"), 6)
  expect_equal(species("inside"), "Ca{inside}")
})

test_that("species() regex", {
  expect_length(species(regex("^Ca")), 4)
  expect_length(species(regex("e\\}$")), 1)
})

test_that("species_strict()", {
  expect_error(species_strict())
  expect_error(species_strict("Ca"))
  expect_equal(species_strict("Ca1"), "Ca1{Ca}")
  expect_equal(species_strict("inside"), "Ca{inside}")
  expect_equal(species_strict("1"), "1{Ca}")
})

test_that("species_strict() regex", {
  expect_equal(species_strict(regex("^Ca\\{\\w\\w\\}$")), "Ca{Ca}")
  expect_equal(species_strict(regex("e\\}$")), "Ca{inside}")
  expect_equal(species_strict(regex("^\\d$")), "1{Ca}")
  expect_error(species_strict(regex("^Ca$")))
})

test_that("species_strict() vectorization", {
  expect_length(species_strict(species()), length(species()))
  expect_equal(species_strict(rev(species())), rev(species()))
  expect_error(species_strict(getSpecies()$name))
})

test_that("compartment()", {
  expect_length(compartment(), 5)
  expect_length(compartment("Ca"), 2)
  expect_equal(compartment("inside"), "Compartments[inside]")
})

test_that("compartment() regex", {
  expect_length(compartment(regex("^Ca")), 0)
  expect_length(compartment(regex("^.*Ca.*$")), 2)
})

test_that("compartment_strict()", {
  expect_error(compartment_strict())
  expect_equal(compartment_strict("Ca"), "Compartments[Ca]")
  expect_equal(compartment_strict("a1"), "Compartments[Ca1]")
  expect_equal(compartment_strict("1"), "Compartments[1]")
})

test_that("compartment_strict() regex", {
  expect_equal(compartment_strict(regex(".*\\[\\d\\]$")), "Compartments[1]")
  expect_equal(compartment_strict(regex("^\\d$")), "Compartments[1]")
  expect_error(compartment_strict(regex(".*\\d$")))
})

test_that("compartment_strict() vectorization", {
  expect_length(compartment_strict(compartment()), length(compartment()))
  expect_equal(compartment_strict(rev(compartment())), rev(compartment()))
  expect_equal(compartment_strict(getCompartments()$name), compartment())
})

test_that("quantity()", {
  expect_length(quantity(), 4)
  expect_length(quantity("Ca"), 2)
})

test_that("quantity() regex", {
  expect_length(quantity(regex("^Ca")), 0)
  expect_length(quantity(regex("^.*Ca.*$")), 2)
})

test_that("quantity_strict()", {
  expect_error(quantity_strict())
  expect_equal(quantity_strict("Ca"), "Values[Ca]")
  expect_equal(quantity_strict("a1"), "Values[Ca1]")
  expect_equal(quantity_strict("1"), "Values[1]")
})

test_that("quantity_strict() regex", {
  expect_equal(quantity_strict(regex(".*\\[\\d\\]$")), "Values[1]")
  expect_equal(quantity_strict(regex("^\\d$")), "Values[1]")
  expect_error(quantity_strict(regex(".*\\d$")))
})

test_that("quantity_strict() vectorization", {
  expect_length(quantity_strict(quantity()), length(quantity()))
  expect_equal(quantity_strict(rev(quantity())), rev(quantity()))
  expect_equal(quantity_strict(getGlobalQuantities()$name), quantity())
})

test_that("reaction()", {
  expect_length(reaction(), 6)
  expect_length(reaction("Ca"), 4)
})

test_that("reaction() regex", {
  expect_length(reaction(regex("^\\(Ca")), 3)
  expect_length(reaction(regex("^.*Ca.*$")), 4)
})

test_that("reaction_strict()", {
  expect_error(reaction_strict())
  expect_equal(reaction_strict("Ca"), "(Ca)")
  expect_equal(reaction_strict("a1"), "(Ca1)")
  expect_equal(reaction_strict("1"), "(1)")
})

test_that("reaction_strict() regex", {
  expect_equal(reaction_strict(regex("^\\(\\d\\)$")), "(1)")
  expect_equal(reaction_strict(regex("^\\d$")), "(1)")
  expect_error(reaction_strict(regex(".*\\d\\)$")))
})

test_that("reaction_strict() vectorization", {
  expect_length(reaction_strict(reaction()), length(reaction()))
  expect_equal(reaction_strict(rev(reaction())), rev(reaction()))
  expect_equal(reaction_strict(getReactions()$name), reaction())
})

test_that("parameter()", {
  expect_length(parameter(), 3)
  expect_length(parameter("Ca"), 3)
  expect_length(parameter("inside"), 2)
  expect_equal(parameter(".v"), "(-> Ca{inside}).v")
})

test_that("parameter() regex", {
  expect_length(parameter(regex("k$")), 0)
  expect_equal(parameter(regex("Ca\\{.*\\.k1")), "(Ca{inside} -> Ca).k1")
})

test_that("parameter_strict()", {
  expect_error(parameter_strict())
  expect_equal(parameter_strict("v"), "(-> Ca{inside}).v")
  expect_equal(parameter_strict(".v"), "(-> Ca{inside}).v")
  expect_error(parameter_strict(".k1"))
})

test_that("parameter_strict() regex", {
  expect_error(parameter_strict(regex(".*\\.k\\d$")))
  expect_equal(parameter_strict(regex("^v$")), "(-> Ca{inside}).v")
  expect_equal(parameter_strict(regex(".*v.*")), "(-> Ca{inside}).v")
})

test_that("parameter_strict() vectorization", {
  expect_length(parameter_strict(parameter()), length(parameter()))
  expect_equal(parameter_strict(rev(parameter())), rev(parameter()))
  expect_error(parameter_strict(getParameters()$name))
})

test_that("kinfunction()", {
  expect_gt(length(kinfunction()), 30)
  expect_equal(kinfunction("Catalytic activation (irrev)"), "FunctionDB.Functions[Catalytic activation (irrev)]")
  expect_length(kinfunction("Catalytic activation"), 2)
})

test_that("kinfunction() regex", {
  expect_length(kinfunction(regex("^Cata")), 0)
  expect_length(kinfunction(regex("^.*Catalytic activation.*$")), 2)
})

test_that("kinfunction_strict()", {
  expect_error(kinfunction_strict())
  expect_error(kinfunction_strict("Catalytic activation"))
  expect_equal(kinfunction_strict("Catalytic activation (irrev)"), "FunctionDB.Functions[Catalytic activation (irrev)]")
})

test_that("kinfunction_strict() regex", {
  expect_error(kinfunction_strict(regex("^Cata")))
  expect_error(kinfunction_strict(regex("^.*Catalytic activation.*$")))
  expect_equal(kinfunction_strict(regex("^.*Catalytic activation.*irrev.*$")), "FunctionDB.Functions[Catalytic activation (irrev)]")
})

test_that("kinfunction_strict() vectorization", {
  expect_length(kinfunction_strict(kinfunction()), length(kinfunction()))
  expect_equal(kinfunction_strict(rev(kinfunction())), rev(kinfunction()))
})

unloadAllModels()
clearCustomKineticFunctions()
