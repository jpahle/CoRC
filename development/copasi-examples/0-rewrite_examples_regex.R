# Use these regex commands to rewrite examples from old to new syntax

pattern_t <- "TRUE"
subs_t <- "%TRUE"

pattern_f <- "FALSE"
subs_f <- "FALSE"

pattern_long <- "\\w+?_(\\w+)\\(([\\w$^]+(\\(\\))?),\\s?(.+?)\\)"
subs_long <- "\\2$\\1^^^^^^(\\4)"

pattern_short <- "\\w+?_(\\w+)\\(([\\w$^]+(\\(\\))?)\\)"
subs_short <- "\\2$\\1^^^^^^()"

pattern_invis <- "^(\\s*)invisible\\((.*)\\)(\\s*)$"
subs_invis <- "\\1\\2\\3"

pattern_getitem <- "\\$__getitem__\\((\\w+)\\)"
subs_getitem <- "[\\1][[1]]"

pattern_fix <- "^^^^^^"
subs_fix <- ""

examples <- c(
    "example1",
    "example2",
    "example3",
    "example4",
    "example5",
    "example6",
    "example7",
    "example8",
    "example9"
)

fixes <- list(
    example3 = c(
        'trajectoryTask <- dataModel$getTask("Time-Course")',
        'trajectoryTask <- as(dataModel$getTask("Time-Course"), "_p_CTrajectoryTask")',
        'problem <- trajectoryTask$getProblem()',
        'problem <- as(trajectoryTask$getProblem(), "_p_CTrajectoryProblem")'
    ),
    example4 = c(
        'problem <- trajectoryTask$getProblem()',
        'problem <- as(trajectoryTask$getProblem(), "_p_CTrajectoryProblem")',
        'scanProblem <- scanTask$getProblem()',
        'scanProblem <- as(scanTask$getProblem(), "_p_CScanProblem")'
    ),
    example5 = c(
        'problem <- timeCourseTask$getProblem()',
        'problem <- as(timeCourseTask$getProblem(), "_p_CTrajectoryProblem")',
        'optProblem <- optTask$getProblem()',
        'optProblem <- as(optTask$getProblem(), "_p_COptProblem")'
    ),
    example6 = c(
        'trajectoryTask <- dataModel$getTask("Time-Course")',
        'trajectoryTask <- as(dataModel$getTask("Time-Course"), "_p_CTrajectoryTask")',
        'problem <- trajectoryTask$getProblem()',
        'problem <- as(trajectoryTask$getProblem(), "_p_CTrajectoryProblem")',
        'fitProblem <- fitTask$getProblem()',
        'fitProblem <- as(fitTask$getProblem(), "_p_CFitProblem")'
    ),
    example9 = c(
        'task <- dataModel$getTask("Steady-State")',
        'task <- as(dataModel$getTask("Steady-State"), "_p_CSteadyStateTask")'
    )
)

for (ex in examples) {
    fileName <- paste0("development/copasi-examples/", ex, ".R")
    newFileName <- paste0("development/copasi-examples/", ex, "_rewrite.R")
    
    exFile <- readLines(fileName)
    
    exFile <- gsub(
        pattern = pattern_t,
        replacement = subs_t,
        x = exFile,
        fixed = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_f,
        replacement = subs_f,
        x = exFile,
        fixed = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_long,
        replacement = subs_long,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_short,
        replacement = subs_short,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_long,
        replacement = subs_long,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_short,
        replacement = subs_short,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_long,
        replacement = subs_long,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_short,
        replacement = subs_short,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_long,
        replacement = subs_long,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_short,
        replacement = subs_short,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_long,
        replacement = subs_long,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_short,
        replacement = subs_short,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_fix,
        replacement = subs_fix,
        x = exFile,
        fixed = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_invis,
        replacement = subs_invis,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = pattern_getitem,
        replacement = subs_getitem,
        x = exFile,
        perl = TRUE
    )
    
    exFile <- gsub(
        pattern = subs_t,
        replacement = pattern_t,
        x = exFile,
        fixed = TRUE
    )
    
    exFile <- gsub(
        pattern = subs_f,
        replacement = pattern_f,
        x = exFile,
        fixed = TRUE
    )
    
    if (ex %in% names(fixes)) {
        for (job in seq(1, length(fixes[[ex]]), 2)) {
            exFile <- gsub(
                pattern = fixes[[ex]][job],
                replacement = fixes[[ex]][job + 1],
                x = exFile,
                fixed = TRUE
            )
        }
    }
    
    write(exFile, newFileName)
}
