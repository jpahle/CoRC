# This file contains functions that work around bugs in the R implementation of swig.
# There are numerous bugs and the workarounds are certainly error prone too.

# Read arbitrary CVectors
# Sometimes R tries to create classes like _p_CVectorT_CObjectInterface_const_p_t which it has never defined.
# This functions skips those vectors classes and gives the contents as list.
# @param fun symbol of the function that gives a bad vector
# @param self slot "ref" of the object which the method is called on
# @param class name or symbol for the class that is forced on all list members
swigfix_resolve_vector <- function(fun, self, class) {
  fun <- paste0("R_swig_", as.character(substitute(fun)))
  class <- paste0("_p_", as.character(substitute(class)))
  
  vector <- .Call(fun, self@ref, FALSE, PACKAGE='COPASI')
  vectorsize <- .Call('R_swig_FloatVectorCore_size', vector, FALSE, PACKAGE='COPASI')
  
  seq_len_from0(vectorsize) %>%
    map(~ {
      new(
        class,
        ref = .Call('R_swig_ObjectVectorCore_get', vector, .x, PACKAGE='COPASI')
      )
    })
}