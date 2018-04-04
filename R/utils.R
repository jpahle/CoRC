# stub for backports
hasName <- function (...) {}

# like is.na but ignores NaN
is_pure_na <- function(x) {
  is.na(x) & !is.nan(x)
}

# like noNA but ignores NaN
noPureNA <- function(x) {
  !(any(is_pure_na(x)))
}
on_failure(noPureNA) <- function(call, env) {
  n <- sum(is_pure_na(eval(call$x, env)))
  paste0(deparse(call$x), " contains ", n, " missing values")
}

# Format function for the CDataModel class which is used as a basis for the print method
#' @include swig_wrapper.R
#' @export
setMethod("format",
  "_p_CDataModel",
  function(x, ...) {
    string <- "# A COPASI model reference:\n"
    
    if (has_null_pointer(x))
      return(paste0(string, "Model has been unloaded."))
    
    c_model <- x$getModel()
    string <- paste0(string, "Model name: \"" , c_model$getObjectName() , "\"\n")
    # string <- paste0(string, "@ref is set to: " , utils::capture.output(x@ref) , "\n")
    string <- paste0(string, "Number of compartments: " , c_model$getNumCompartments(), "\n")
    string <- paste0(string, "Number of species: " , c_model$getNumMetabs(), "\n")
    string <- paste0(string, "Number of reactions: " , c_model$getNumReactions())

    string
  }
)

#' @include swig_wrapper.R
#' @export
setMethod("print",
  "_p_CDataModel",
  function(x, ...) {
    cat(format(x, ...), "\n")
    invisible(x)
  }
)

#' @include swig_wrapper.R
#' @export
setMethod("show",
  "_p_CDataModel",
  function(object) {
    print(object)
    invisible(object)
  }
)

# Checks whether the given c_datamodel is valid / loaded
# Whenever it's called, it will also set the model as the currently active one
assert_datamodel <- function(model) {
  assert_that(inherits(model, "_p_CDataModel"))
  c_datamodel <- model
  
  assert_that(
    !has_null_pointer(c_datamodel),
    msg = "The model is not loaded in CoRC. Did you unload it?"
  )
  
  pkg_env$c_curr_dm <- c_datamodel
  
  c_datamodel
}

# Make c_datamodel variables safe to pass trough assert_datamodel.
# expernal pointers in R are always copied by reference.
# This is a major exception to typical copying rules of R.
# This fact is crucial for CoRC to work safely because unloading a datamodel
# from anywhere will invalidate the users references to it.
# If I generate another reference to a datamodel by $getObjectDatamodel() I have
# to make sure it never leaks back to the user.
# For this reason, CoRC keeps track of references passed to users with the
# pkg_env$cl_loaded_dms list.
make_dm_safe <- function(c_datamodel) {
  cl_loaded_dms <- pkg_env$cl_loaded_dms
  
  i_dm_in_list <-
    cl_loaded_dms %>%
    map(attr_getter("ref")) %>%
    map_lgl(identical, c_datamodel@ref) %>%
    which()
  
  cl_loaded_dms[[i_dm_in_list[1]]]
}

compile_and_check <- function(c_model) {
  if (!grab_msg(c_model$compileIfNecessary()))
    warning("The model failed to compile in it's current state. Some CoRC features might fail unexpectedly.")
}

# transforms names from how they appear in the GUI to the preferred format in CoRC
# e.g. "Initial Concentration" -> "initial_concentration" 
# e.g. "Rate (1/s)" -> "rate_1_s"
transform_names <- function(x) {
  set_names(
    x,
    transform_names_worker(names(x))
  )
}

transform_names_worker <- function(x) {
  # x %>% make.names(unique = TRUE) %>% tolower()
  x %>%
    make.names() %>%
    stringr::str_replace_all("\\.+", "_") %>%
    stringr::str_replace("_+$", "") %>%
    tolower() %>%
    make.unique(sep = "_")
}

# Convert lists that are equivalent to vectors to vectors
# This function is a helper for functions that take (named) vectors as argument.
# It generates helpful error messages by using substitute.
# In R, users often like to give named lists instead of named vectors which is an easy mistake.
# This function is very specific to R and can likely be cut completely in other languages.
to_param_vector <- function(x, type) {
  x_val <- x
  
  type_fun <- get(type)
  x_val <- try(as_vector(x_val, .type = type_fun(1)), silent = TRUE)
  
  assert_that(
    !is.error(x_val),
    msg = paste0(deparse(substitute(x)), " cannot be converted to vector of type ", type, ".")
  )
  
  x_val
}

# depending on dimensionality of the compartment, the units vary
get_dimension_units <- function(c_model) {
  c(
    1,
    CUnit_prettyPrint(c_model$getLengthUnit()),
    CUnit_prettyPrint(c_model$getAreaUnit()),
    CUnit_prettyPrint(c_model$getVolumeUnit())
  )
}

# wrapper for getReferenceDirectory
# if no ref dir is set in COPASI, getReferenceDirectory returns ""
# in that case, use getwd()
get_ref_dir <- function(c_datamodel) {
  dir <- c_datamodel$getReferenceDirectory()
  
  if (dir == "")
    dir <- getwd()
  
  dir
}

# finds COPASI messages and helps purge known annoying messages
# because of lazy evaluation, x will not be evaluated on the function
# call but be evaluated with force(x) and then messages are checked
grab_msg <- function(x, purge = character()) {
  purge_by_default <- c(
    ": No output file defined for report of task ",
    "The MIRIAM resource '.*' is unknown to COPASI."
  )
  purge <- c(purge, purge_by_default)
  
  if (CCopasiMessage_size() > 0L) {
    warning(
      "\n",
      "==========================================\n",
      "Uncaptured COPASI Message(s):\n",
      CCopasiMessage_getAllMessageText(), "\n",
      "==========================================\n"
    )
  }
  
  force(x)
  
  if (CCopasiMessage_size() > 0L) {
    messages <- map_chr(seq_len(CCopasiMessage_size()), ~ CCopasiMessage_getFirstMessage()$getText())
    
    # filter all messages that match a purge pattern
    messages <- messages[map_lgl(messages, ~ !any(stringr::str_detect(.x, pattern = purge)))]
    
    if (!is_empty(messages)) {
      warning(
        "Process generated COPASI Message(s):\n",
        paste0(messages, collapse = "\n"), "\n"
      )
    }
  }
  
  x
}

# Check if object@ref pointer is NULL
# This happens whenever an object is destructed from R (e.g. garbage collection)
# or when the object has been saved and loaded from file (not supported)
#' @importFrom isnullptr isnullptr
has_null_pointer <- function(c_object) {
  isnullptr(c_object@ref)
}

# Hack to allow me to use swig constructors and hand the objects to C
# Usually swig makes sure all constructed objects will be up for garbage collection upon dereference in R
pkg_env$avert_gc <- FALSE
reg.finalizer <- function(...) {
  if (!pkg_env$avert_gc) base::reg.finalizer(...)
}
avert_gc <- function(expr) {
  pkg_env$avert_gc <- TRUE
  force(expr)
  pkg_env$avert_gc <- FALSE
  expr
}

# Cleaner error messages
warning <- partial(base::warning, call. = FALSE)
stop <- partial(base::stop, call. = FALSE)

# Force the winslash to be the same applied by file.path etc
normalizePathC <- partial(normalizePath, winslash = .Platform$file.sep)

# Convert annotated matrices to data frames
# Is not sufficiently tested for col/row; colnames; rownames consistency
get_annotated_matrix <- function(c_matrix) {
  dims <- c_matrix$dimensionality()
  
  assert_that(dims == 2, msg = "Only two dimensional annotated matrices can be read for now.")
  
  col_headers <- get_sv(c_matrix$getAnnotationsString(1L))
  row_headers <- get_sv(c_matrix$getAnnotationsString(0L))
  
  cols <- length(col_headers)
  rows <- length(row_headers)
  
  array <- c_matrix$getArray()
  
  # Timecritical step optimization
  array_ref <- array@ref
  R_swig_CArrayInterface_get__SWIG_1 <- getNativeSymbolInfo("R_swig_CArrayInterface_get__SWIG_1", "COPASI")[["address"]]
  
  # assemble output dataframe
  # Iterates over all cols and rows
  # Inner loops creates numeric()
  # Outer loop creates list of columns for conversion to matrix
  seq_len_0(cols) %>%
    map(function(i_col) {
      seq_len_0(rows) %>%
        map_dbl(function(i_row) {
          # Timecritical step optimization
          # array$get(i_row, i_col)
          # CArrayInterface_get(array, i_row, i_col)
          # args: self@ref, int, int, bool
          .Call(R_swig_CArrayInterface_get__SWIG_1, array_ref, i_row, i_col, FALSE)
        })
    }) %>%
    flatten_dbl() %>%
    matrix(rows, cols, dimnames = list(row_headers, col_headers))
}
