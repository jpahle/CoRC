# Works like seq_along for CDataVectors (0 based index)
seq_along_cv <- function(copasivector) {
  len <- copasivector$size()
  
  if (len == 0L) return(integer())
  
  0L:(len - 1L)
}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_cv_index <- function(copasivector, index) {
  type <- is(copasivector)[1L]
  
  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_match(type, "^_p_CDataVector\\w+_(\\w+)_t$")[2L])
  
  # typecasting the result
  as(copasivector$get(index), type)
}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_cv <- function(copasivector, indices = seq_along_cv(copasivector)) {
  type <- is(copasivector)[1L]
  
  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_match(type, "^_p_CDataVector\\w+_(\\w+)_t$")[2L])
  
  # typecasting the result
  map(indices, ~ as(copasivector$get(.x), type))
}
