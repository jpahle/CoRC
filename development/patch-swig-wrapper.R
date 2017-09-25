# Apply my patches to the swig_wrapper
# These patches are likely specific for the CoRC packages
# which is why I did not commit them upstream to Copasi

wrapper_path <- file.path("..", "R", "swig_wrapper.R")

wrapper_con <- file(wrapper_path)

wrapper_lines <- readLines(wrapper_con)

wrapper_lines <-
  wrapper_lines %>%
  # replace all checks for null pointers
  # previously this was done by an awkward workaround and string comparison
  # now use isnullptr::isnullptr
  stringr::str_replace_all('capture\\.output\\((\\w+?)\\) %in% c\\("<pointer: 0x0>", "<pointer: \\(nil\\)>"\\)', 'isnullptr(\\1)') %>%
  stringr::str_replace_all('!\\(isnullptr\\((\\w+?)\\)\\)', '!isnullptr(\\1)')

writeLines(wrapper_lines, wrapper_con)

close(wrapper_con)

rm(wrapper_path, wrapper_con, wrapper_lines)
