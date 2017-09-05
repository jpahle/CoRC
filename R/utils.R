# Format function for the CDataModel class which is used as a basis for the print method
#' @include swig_wrapper.R
#' @export
setMethod("format",
  "_p_CDataModel",
  function(x, ...) {
    string <- "# A copasi model reference:\n"
    
    if (has_null_pointer(x))
      return(paste0(string, "Model has been unloaded."))
    
    model <- x$getModel()
    string <- paste0(string, "Model name: \"" , model$getObjectName() , "\"\n")
    # string <- paste0(string, "@ref is set to: " , utils::capture.output(x@ref) , "\n")
    string <- paste0(string, "Number of compartments: " , model$getCompartments()$size(), "\n")
    string <- paste0(string, "Number of species: " , model$getMetabolites()$size(), "\n")
    string <- paste0(string, "Number of reactions: " , model$getReactions()$size())

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
assert_datamodel <- function(c_datamodel) {
  assert_that(inherits(c_datamodel, "_p_CDataModel"))
  
  assert_that(
    !has_null_pointer(c_datamodel),
    msg = "The model is not loaded in CoRC. Did you unload it?"
  )
  
  pkg_env$c_curr_dm <- c_datamodel
  
  c_datamodel
}

# transforms names from how they appear in the GUI to the preferred format in CoRC
# e.g. "Initial Concentration" -> "initial.concentration" 
transform_names <- function(x) {
  set_names(
    x,
    transform_names_worker(names(x))
  )
}

transform_names_worker <- function(x) {
  x %>% make.names(unique = TRUE) %>% stringr::str_to_lower()
}

# finds copasi messages and helps purge known annoying messages
# because of lazy evaluation, x will evaluated with force(x) and then messages are checked
grab_msg <- function(x, purge = character(0)) {
  purge_by_default <- c(
    ": No output file defined for report of task "
  )
  purge <- c(purge, purge_by_default)
  
  if (CCopasiMessage_size() > 0L) {
    warning(
      "\n",
      "==========================================\n",
      "Uncaptured Copasi Message(s):\n",
      CCopasiMessage_getAllMessageText(), "\n",
      "==========================================\n"
    )
  }
  
  force(x)
  
  if (CCopasiMessage_size() > 0L) {
    messages <- map_chr(1L:CCopasiMessage_size(), ~ CCopasiMessage_getFirstMessage()$getText())
    
    # filter all messages that match a purge pattern
    messages <- messages[map_lgl(messages, ~ !any(stringr::str_detect(.x, pattern = purge)))]
    
    if (!is_empty(messages)) {
      warning(
        "Process generated Copasi Message(s):\n",
        paste0(messages, collapse = "\n"), "\n"
      )
    }
  }
  
  x
}

# Check if object@ref pointer is NULL
# This happens whenever an object is destructed from R (e.g. garbage collection)
# or when the object has been saved and loaded from file (not supported)
has_null_pointer <- function(c_object) {
  capture.output(c_object@ref) %in% c("<pointer: 0x0>", "<pointer: (nil)>")
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

#' Autoplot method for copasi timeseries objects.
#'
#' Uses ggplot2 to plot timeseries.
#' The plot has a geom_line layer.
#'
#' @param object A copasi timeseries object
#' @param \dots Species names selected for plotting
#' @return A ggplot2 plot
#' @importFrom ggplot2 autoplot
#' @export
autoplot.copasi_ts <- function(object, ...) {
  validate_copasi_ts(object)
  
  tc <- object$result
  
  # use partial matching with ... args
  selected <- flatten_chr(list(...)) %>% pmatch(names(tc))
  
  if (anyNA(selected)) {
    warning("Partial matching failed for some species")
    selected <- selected[!is.na(selected)]
  }
  
  # only add species selected in ...
  if (!is_empty(selected))
    tc <- tc %>% dplyr::select(Time, !!!selected)
  
  units <- object$units
  
  # reshape data frame for ggplot and define the plot
  tc %>%
    tidyr::gather(Species, Concentration, -Time) %>%
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Concentration, group = Species, color = Species)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = paste0("Time (", units$time, ")"),
      y = paste0("Concentration (", units$concentration, ")")
    )
}

# CParameter gives us only feedback on what kind of value it needs.
# It is our responsibility to call the right function which is why this list is needed.
cparameter_control_functions <-
  list(
    DOUBLE = function(x) {is_scalar_integer(x) || is_scalar_double(x)},
    UDOUBLE = function(x) {(is_scalar_integer(x) || is_scalar_double(x)) && x >= 0},
    INT = rlang::is_scalar_integerish,
    UINT = function(x) {rlang::is_scalar_integerish(x) && x >= 0},
    BOOL = function(x) {length(x) != 0L && (x == 1L || x == 0L)},
    STRING = is_scalar_character,
    GROUP = NULL,
    CN = is_scalar_character,
    KEY = is_scalar_character,
    FILE = is_scalar_character,
    EXPRESSION = NULL,
    INVALID = NULL
  )

cparameter_get_functions <-
  list(
    DOUBLE = CCopasiParameter_getDblValue,
    UDOUBLE = CCopasiParameter_getUDblValue,
    INT = CCopasiParameter_getIntValue,
    UINT = CCopasiParameter_getUIntValue,
    BOOL = CCopasiParameter_getBoolValue,
    STRING = CCopasiParameter_getStringValue,
    # GROUP = CCopasiParameter_getGroupValue,
    GROUP = function(self) {"UNSUPPORTED_PARAMETER"},
    CN = function(self) {
      CCopasiParameter_getCNValue(self)$getString()
    },
    KEY = CCopasiParameter_getKeyValue,
    FILE = CCopasiParameter_getFileValue,
    EXPRESSION = function(self) {"UNSUPPORTED_PARAMETER"},
    INVALID = function(self) {"UNSUPPORTED_PARAMETER"}
  )

cparameter_set_functions <-
  list(
    DOUBLE = CCopasiParameter_setDblValue,
    UDOUBLE = CCopasiParameter_setUDblValue,
    INT = CCopasiParameter_setIntValue,
    UINT = CCopasiParameter_setUIntValue,
    BOOL = CCopasiParameter_setBoolValue,
    STRING = CCopasiParameter_setStringValue,
    GROUP = CCopasiParameter_setGroupValue,
    CN = function(self, x) {
      CCopasiParameter_setCNValue(self, CCommonName(x))
    },
    KEY = CCopasiParameter_setKeyValue,
    FILE = CCopasiParameter_setFileValue,
    EXPRESSION = NULL,
    INVALID = NULL
  )

methodstructure <- function(c_method) {
  struct <-
    tibble::tibble(
      object = seq_along_v(c_method) %>% map(~ c_method$getParameter(.x)),
      name = object %>% map_chr(~ .x$getObjectName()) %>% transform_names_worker(),
      type = object %>% map_chr(~ .x$getType()),
      control_fun = cparameter_control_functions[type],
      get_fun = cparameter_get_functions[type],
      set_fun = cparameter_set_functions[type]
    )
  
  if (has_element(struct$control_fun, NULL))
    warning("Internal: Unknown type found with parameters: ", paste(struct$name[map_lgl(struct$control_fun, is.null)], collapse = "; "))
  
  struct
}

get_method_settings <- function(c_method, with_name = FALSE) {
  struct <- methodstructure(c_method)
  ret <- list()
  
  if (with_name) ret$method <- c_method$getSubType()
  
  params <- map2(struct$get_fun, struct$object, ~ .x(.y))
  names(params) <- struct$name
  
  c(ret, params)
}

set_method_settings <- function(values, c_method) {
  # parameter "method" is reserved for CoRC
  # if a parameter "method" is ever created in CoRC, adjust methodstructure() to call it "method_"
  values <- values[names(values) != "method"]
  
  if (is_empty(values)) return()
  
  struct <- methodstructure(c_method) %>% tibble::rowid_to_column()
  
  data <-
    tibble::tibble(value = values) %>%
    dplyr::mutate(rowid = pmatch(names(values), struct$name))
  
  matched <- !is.na(data$rowid)
  
  assert_that(
    all(matched),
    msg = paste0(
      "Bad method parameter names.\n",
      '"', names(values)[!matched][1], '" should be one of "',
      paste0(struct$name, collapse = '", "'), '".'
    )
  )
  
  data <-
    data %>%
    dplyr::filter(map_lgl(value, negate(is.null))) %>%
    dplyr::left_join(struct, by = "rowid")
  
  skipped <- map_lgl(data$control_fun, is.null)
  
  skipped %>%
    which() %>%
    walk(~
      warning('Parameter "', data$name[.x], '" was skipped because it is of unsupported type "', data$struct[.x], '".')
    )
  
  data <- data %>% dplyr::filter(!skipped)
  
  allowed <- map2_lgl(data$control_fun, data$value, ~ .x(.y))
  
  assert_that(
    all(allowed),
    msg = paste0(
      'Method parameter "', data$name[!allowed][1], '" has to be of type "', data$type[!allowed][1], '".'
    )
  )
  
  success <- pmap_lgl(
    data,
    function(set_fun, object, value, ...) {
      set_fun(object, value)
    }
  )
  
  assert_that(
    all(success),
    msg = paste0('Method parameter "', data$name[!success][1], '" could not be set.')
  )
}

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
