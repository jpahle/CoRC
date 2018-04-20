# cluster helper function
# behaves like purrr::map
map_parallel <- function(cl, .x, .f, ..., chunk.size = NULL) {
  # before R 3.5, parLapplyLB doesn't actually load balance
  if (getRversion() < "3.5") {
    parallel::parLapplyLB(
      cl = cl,
      X = .x,
      fun = as_mapper(.f),
      ...
    )
  } else {
    parallel::parLapplyLB(
      cl = cl,
      X = .x,
      fun = as_mapper(.f),
      ...,
      chunk.size = chunk.size
    )
  }
}
