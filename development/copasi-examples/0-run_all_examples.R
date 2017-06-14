## Run all examples to find regressions.

library(assertthat)
library(purrr)
devtools::load_all()

categories <- c(
    std = T,
    rewrite = T,
    api = F
)

testfiles <- list(
    std = c(
        "example1",
        "example2",
        "example3",
        "example4",
        "example5",
        "example6",
        "example7",
        "example8",
        "example9"
    ),
    rewrite = c(
        "example1_rewrite",
        "example2_rewrite",
        "example3_rewrite",
        "example4_rewrite",
        "example5_rewrite",
        "example6_rewrite",
        "example7_rewrite",
        "example8_rewrite",
        "example9_rewrite"
    ),
    api = c(

    )
)

validations = list(
    example1 = quote({
        validate_that(!(FALSE %in% file.exists("example1.cps", "example1.xml")))
        file.remove("example1.cps", "example1.xml")
    }),
    example1_rewrite = quote({
        validate_that(!(FALSE %in% file.exists("example1.cps", "example1.xml")))
        file.remove("example1.cps", "example1.xml")
    }),
    example3 = quote({
        validate_that(file.exists("development/copasi-examples/example3.txt"))
        file.remove("development/copasi-examples/example3.txt")
    }),
    example3_rewrite = quote({
        validate_that(file.exists("development/copasi-examples/example3.txt"))
        file.remove("development/copasi-examples/example3.txt")
    }),
    example4 = quote({
        validate_that(file.exists("example4.txt"))
        file.remove("example4.txt")
    }),
    example4_rewrite = quote({
        validate_that(file.exists("example4.txt"))
        file.remove("example4.txt")
    }),
    example5 = quote({
        validate_that(file.exists("example5.txt"))
        file.remove("example5.txt")
    }),
    example5_rewrite = quote({
        validate_that(file.exists("example5.txt"))
        file.remove("example5.txt")
    }),
    example6 = quote({
        validate_that(file.exists("fakedata_example6.txt"))
        file.remove("fakedata_example6.txt")
    }),
    example6_rewrite = quote({
        validate_that(file.exists("fakedata_example6.txt"))
        file.remove("fakedata_example6.txt")
    }),
    example7 = quote({
        validate_that(!(FALSE %in% file.exists("example7.cps", "example7.xml")))
        file.remove("example7.cps", "example7.xml")
    }),
    example7_rewrite = quote({
        validate_that(!(FALSE %in% file.exists("example7.cps", "example7.xml")))
        file.remove("example7.cps", "example7.xml")
    })
)

for (cat in names(categories)) {
  if (categories[[cat]]) {
    for (file in testfiles[[cat]]) {
      message("###START### ", file, " ###START###")
      source(paste0("development/copasi-examples/", file, ".R"), echo = F, local = rlang::env())
      unloadAllModels()
      
      if (file %in% names(validations)) eval(validations[[file]])
      message("####END#### ", file, " ####END####")
    }
  }
}

rm(cat, file)
