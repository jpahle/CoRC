## Run all examples to find regressions.

library(assertthat)

rm(list = ls(all.names = T))

t <- list()

t$runtest <- c(
    std = F,
    rewrite = T,
    api = F
)

t$tests <- list(
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

t$validations = list(
    example1 = quote({
        validate_that(!(FALSE %in% file.exists("example1.cps", "example1.xml")))
        file.remove("example1.cps", "example1.xml")
    }),
    example1_rewrite = quote({
        validate_that(!(FALSE %in% file.exists("example1.cps", "example1.xml")))
        file.remove("example1.cps", "example1.xml")
    }),
    example3 = quote({
        validate_that(file.exists("examples/example3.txt"))
        file.remove("examples/example3.txt")
    }),
    example3_rewrite = quote({
        validate_that(file.exists("examples/example3.txt"))
        file.remove("examples/example3.txt")
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

for (t_test in names(t$runtest)) {
    if (t$runtest[t_test]) {
        t$testfiles <- t$tests[[t_test]]
        
        for (t_testfile in t$testfiles) {
            del <- ls(all.names = T)
            del <- del[del != "t" & del != "t_test" & del != "t_testfile"]
            rm(list = del)
            
            message("###START### ", t_testfile, " ###START###")
            source(paste0("examples/", t_testfile, ".R"), echo = F)
            
            if (t_testfile %in% names(t$validations)) eval(t$validations[[t_testfile]])
            message("####END#### ", t_testfile, " ####END####")
        }
    }
}

rm(list = ls(all.names = T))
