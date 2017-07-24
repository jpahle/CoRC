# This file contains functions that work around bugs or help deal with quirks in the R implementation of swig.
# There are numerous bugs and the workarounds are certainly error prone too.

# Read arbitrary CVectors
# Sometimes R tries to create classes like _p_CVectorT_CObjectInterface_const_p_t which it has never defined.
# This functions skips those vectors classes and gives the contents as list.
# @param self slot "ref" of the object which the method is called on
# @param fun symbol of the function that gives a bad vector
# @param class name or symbol for the class that is forced on all list members
swigfix_resolve_vector <- function(self, fun, class) {
  fun <- paste0("R_swig_", as.character(substitute(fun)))
  class <- paste0("_p_", as.character(substitute(class)))
  
  # args: self@ref, bool
  vector <- .Call(fun, self@ref, FALSE, PACKAGE='COPASI')
  # args: self@ref, bool
  vectorsize <- .Call('R_swig_FloatVectorCore_size', vector, FALSE, PACKAGE='COPASI')
  
  seq_len_0(vectorsize) %>%
    map(~ {
      new(
        class,
        # args: self@ref, int
        ref = .Call('R_swig_ObjectVectorCore_get', vector, .x, PACKAGE='COPASI')
      )
    })
}

# Apply a function to a list of objects
# Function will always be the one which gets resolved from the first object
# Thus, unexpected results might happen with inhomogenous lists
map_swig <- function(x, fun, ...) {
  if (is_empty(x)) return(list())
  x_q <- quote(x)
  map(
    .x = x,
    # Find the actual function and strip its class attribute
    .f = rlang::set_attrs(environment(eval(substitute(x_q[[1]]$fun)))$f, class = NULL),
    ...
  )
}

# Apply a function to a list of objects
# Function will always be the one which gets resolved from the first object
# Thus, unexpected results might happen with inhomogenous lists
walk_swig <- function(x, fun, ...) {
  if (is_empty(x)) return(list())
  x_q <- quote(x)
  walk(
    .x = x,
    # Find the actual function and strip its attributes
    .f = rlang::set_attrs(environment(eval(substitute(x_q[[1]]$fun)))$f, NULL),
    ...
  )
}
