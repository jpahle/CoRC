# like seq_len but starts from 0L so it can be used for iterating through c vectors
seq_len_0 <- function(len) {seq_len(len) - 1L}

# Works like seq_along for many copasi vectors (0 based index)
seq_along_v <- function(copasivector) {
  len <- copasivector$size()
  
  if (len == 0L) return(integer())
  
  seq_len_0(len)
}

# get items of std_vector in list
get_sv <- function(vector, indices = seq_along_v(vector) + 1L) {
  vector[indices]
}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_cdv_index <- function(copasivector, index) {
  type <- is(copasivector)[1L]
  
  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_match(type, "^_p_CDataVector\\w+_(\\w+)_t$")[2L])
  
  # typecasting the result
  as(copasivector$get(index), type)
}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_cdv <- function(copasivector, indices = seq_along_v(copasivector)) {
  type <- is(copasivector)[1L]
  
  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_match(type, "^_p_CDataVector\\w+_(\\w+)_t$")[2L])
  
  # typecasting the result
  map(indices, ~ as(copasivector$get(.x), type))
}

# get items of C vectors
get_cv <- function(vector, indices = seq_along_v(vector)) {
  assert_that(inherits(vector, "_p_CVectorT_double_t"))
  map_dbl(indices, unclass(FloatVectorCore_get), self = vector)
}
