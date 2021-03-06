# This file contains functions that work around bugs or help deal with quirks in the R implementation of swig.
# There are numerous bugs and the workarounds are certainly error prone too.

# Read arbitrary CVectors
# Sometimes R tries to create classes like _p_CVectorT_CObjectInterface_const_p_t which it has never defined.
# This functions skips those vectors classes and gives the contents as list.
# @param self object which the method is called on
# @param fun symbol of the function that gives a bad vector
# @param class name or symbol for the class that is forced on all list members
swigfix_resolve_obj_cvector <- function(self, fun, class) {
  fun <- getNativeSymbolInfo(paste0("R_swig_", as.character(substitute(fun))), COPASI, withRegistrationInfo = TRUE)
  class <- paste0("_p_", as.character(substitute(class)))
  
  # args: self@ref, bool
  vector <- .Call(fun, self@ref, FALSE)
  # args: self@ref, bool
  vectorsize <- .Call(COPASI$`R_swig_ObjectVectorCore_size`, vector, FALSE)

  map(seq_len_0(vectorsize), ~
    new(
      class,
      # args: self@ref, int
      ref = .Call(COPASI$`R_swig_ObjectVectorCore_get`, vector, .x)
    )
  )
}

# @param self slot "ref" of the object which the method is called on
# @param fun symbol of the function that gives a bad vector
# @param class name or symbol for the class that is forced on all list members
swigfix_resolve_int_stdvector <- function(self, fun) {
  fun <- getNativeSymbolInfo(paste0("R_swig_", as.character(substitute(fun))), COPASI, withRegistrationInfo = TRUE)
  
  # args: self@ref, bool
  vector <- .Call(fun, self@ref, FALSE)
  # args: self@ref, bool
  vectorsize <- .Call(COPASI$`R_swig_IntStdVector_size`, vector, FALSE)
  
  # args: self@ref, int, bool
  map_int(seq_len_0(vectorsize), ~ .Call(COPASI$`R_swig_IntStdVector___getitem__`, vector, .x, FALSE))
}

# Apply a $function of a swig object to a list of objects
# e.g.:
# metab_list %>% map_swig("getObjectDisplayName")
# equals (in most cases)
# metab_list %>% map(~ .x$getObjectDisplayName())
# Function will always be the one which gets resolved from the first object
# Thus, unexpected results might happen with inhomogenous lists
map_swig <- function(x, fun, ..., .mapfun = map) {
  # if list is empty, use force as dummy function to return empty vector/list
  if (is_empty(x))
    return(.mapfun(logical(), force))
  
  # Find the actual function and strip its class attribute
  f <- unclass(environment(eval(call("$", x[[1L]], fun)))$f)
  
  .mapfun(x, f, ...)
}

map_swig_lgl <- partial(map_swig, .mapfun = map_lgl)
map_swig_chr <- partial(map_swig, .mapfun = map_chr)
map_swig_int <- partial(map_swig, .mapfun = map_int)
map_swig_dbl <- partial(map_swig, .mapfun = map_dbl)
map_swig_dfr <- partial(map_swig, .mapfun = map_dfr)
map_swig_dfc <- partial(map_swig, .mapfun = map_dfc)

walk_swig <- partial(map_swig, .mapfun = walk)
