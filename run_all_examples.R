## Run all examples to find regressions.

library(assertthat)

rm(list = ls(all.names = T))

### Example 1
try(
    source("examples/example1.R")
)
validate_that(!(FALSE %in% file.exists("example1.cps", "example1.xml")))
file.remove("example1.cps", "example1.xml")
###

rm(list = ls(all.names = T))

### Example 2
try(
    source("examples/example2.R")
)
###

rm(list = ls(all.names = T))

### Example 3
try(
    source("examples/example3.R")
)
validate_that(file.exists("examples/example3.txt"))
file.remove("examples/example3.txt")
###

rm(list = ls(all.names = T))

### Example 4
try(
    source("examples/example4.R")
)
validate_that(file.exists("example4.txt"))
file.remove("example4.txt")
###

rm(list = ls(all.names = T))

### Example 5
try(
    source("examples/example5.R")
)
validate_that(file.exists("example5.txt"))
file.remove("example5.txt")
###

rm(list = ls(all.names = T))

### Example 6
try(
    source("examples/example6.R")
)
validate_that(file.exists("fakedata_example6.txt"))
file.remove("fakedata_example6.txt")
###

rm(list = ls(all.names = T))

### Example 7
try(
    source("examples/example7.R")
)
validate_that(!(FALSE %in% file.exists("example7.cps", "example7.xml")))
file.remove("example7.cps", "example7.xml")
###

rm(list = ls(all.names = T))

### Example 8
try(
    source("examples/example8.R")
)
###

rm(list = ls(all.names = T))

### Example 9
try(
    source("examples/example9.R")
)
###

rm(list = ls(all.names = T))
