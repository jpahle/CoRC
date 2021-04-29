# Apply my patches to the swig_wrapper
# These patches are likely specific for the CoRC packages
# which is why I did not commit them upstream to COPASI

wrapper_path <- file.path("..", "R", "swig_wrapper.R")

wrapper_lines <- readLines(wrapper_path)

# replace all checks for null pointers
# previously this was done by an awkward workaround and string comparison
# now use isnullptr::isnullptr
wrapper_lines <- stringr::str_replace_all(
  wrapper_lines,
  'capture\\.output\\((\\w+?)\\) %in% c\\("<pointer: 0x0>", "<pointer: \\(nil\\)>"\\)',
  'isnullptr(\\1)'
)
wrapper_lines <- stringr::str_replace_all(
  wrapper_lines,
  '!\\(isnullptr\\((\\w+?)\\)\\)',
  '!isnullptr(\\1)'
)

# Make all .Call use the COPASI DLLInfo for addressing
wrapper_lines <- stringr::str_replace_all(
  wrapper_lines,
  "\\.Call\\('(.*?)'(.*), PACKAGE='COPASI'\\);",
  ".Call(COPASI$`\\1`\\2);"
)

# remove R_SWIG_getCallbackFunctionStack which is called out in R CMD check
i_empty_lines <- which(wrapper_lines == "")
i_start <- which(wrapper_lines == "R_SWIG_getCallbackFunctionStack =")
stopifnot(length(i_start) == 1)
i_end <- i_empty_lines[i_empty_lines > i_start][1]
wrapper_lines <- wrapper_lines[-(i_start:i_end)]

# remove R_SWIG_addCallbackFunctionStack which is called out in R CMD check
i_empty_lines <- which(wrapper_lines == "")
i_start <- which(wrapper_lines == "R_SWIG_addCallbackFunctionStack =")
stopifnot(length(i_start) == 1)
i_end <- i_empty_lines[i_empty_lines > i_start][1]
wrapper_lines <- wrapper_lines[-(i_start:i_end)]

# # rename COPASI lib to CoRC
# wrapper_lines <- stringr::str_replace_all(
#   wrapper_lines,
#   "\\.Call\\((.*), PACKAGE='COPASI'\\);",
#   paste0(".Call(\\1, PACKAGE='", pkgname, "');")
# )

# remove swig debug info containing personal folder structure
wrapper_lines <- stringr::str_replace_all(
  wrapper_lines,
  '^##.*R.i$',
  ""
)

# using wb write mode allows to use lf line ending on windows
wrapper_con <- file(wrapper_path, open = "wb")
writeLines(wrapper_lines, wrapper_con)
close(wrapper_con)

rm(wrapper_path, wrapper_con, wrapper_lines)
