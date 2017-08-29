# like seq_len but starts from 0L so it can be used for iterating through c vectors
seq_len_0 <- function(len) {seq_len(len) - 1L}

# Works like seq_along for many copasi vectors (0 based index)
seq_along_v <- function(c_copasivector) {
  len <- c_copasivector$size()
  
  if (len == 0L) return(integer())
  
  seq_len_0(len)
}

# get items of std_vector in list
get_sv <- function(c_vector, indices = seq_along_v(c_vector) + 1L) {
  c_vector[indices]
}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_cdv_index <- function(c_copasivector, index) {
  type <- is(c_copasivector)[1L]
  
  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_match(type, "^_p_CDataVector\\w+_(\\w+)_t$")[2L])
  
  # typecasting the result
  as(c_copasivector$get(index), type)
}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_cdv <- function(c_copasivector, indices = seq_along_v(c_copasivector)) {
  type <- is(c_copasivector)[1L]
  
  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_match(type, "^_p_CDataVector\\w+_(\\w+)_t$")[2L])
  
  # typecasting the result
  map(indices, ~ as(c_copasivector$get(.x), type))
}

# get items of C vectors
get_cv <- function(c_vector, indices = seq_along_v(c_vector)) {
  assert_that(inherits(c_vector, "_p_CVectorT_double_t"))
  map_dbl(indices, unclass(FloatVectorCore_get), self = c_vector)
}
