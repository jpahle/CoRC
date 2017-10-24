# cluster helper function
# behaves like purrr::map
# I dont use parallel::parLapplyLB because I have noticed issues with
# unexpected behavior of load balancing.
# Workaround as described here: https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16792
map_parallel <- function(cl, .x, .f, ...) {
  flatten(
    parallel::clusterApplyLB(
      cl = cl,
      x = split_list(.x),
      fun = lapply,
      as_mapper(.f),
      ...
    )
  )
}

# like parallel:::splitList(x, length(x))
split_list <- function(x) {
  imap(x, ~ set_names(list(.x), .y))
}
