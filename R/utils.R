# Format function for the CDataModel class which is used as a basis for the print method
#' @include swig_wrapper.R
#' @export
setMethod("format",
  "_p_CDataModel",
  function(x, ...) {
    model <- x$getModel()

    string <- "# A copasi model reference:\n"
    string <- paste0(string, "Model name: \"" , model$getObjectName() , "\"\n")
    string <- paste0(string, "@ref is set to: " , utils::capture.output(x@ref) , "\n")
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

# Checks whether the datamodel parameter is valid
assert_datamodel <- function(datamodel) {
  assert_that(inherits(datamodel, "_p_CDataModel"))
  
  pkg_env$curr_dm <- datamodel
}

transform_names <- function(x) {
  set_names(
    x,
    names(x) %>% make.names(unique = TRUE) %>% stringr::str_to_lower()
  )
}

# finds copasi messages and helps purge known annoying messages
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
  # use partial matching with ... args
  selected <- flatten_chr(list(...)) %>% pmatch(names(object))
  
  if (anyNA(selected)) {
    warning("Partial matching failed for some species")
    selected <- selected[!is.na(selected)]
  }
  
  # only add species selected in ...
  if (!is_empty(selected)) object <- object %>% dplyr::select(Time, !!!selected)
  
  units <- object %@% "units"
  
  # reshape data frame for ggplot and define the plot
  object %>%
    tidyr::gather(Species, Concentration, -Time) %>%
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Concentration, group = Species, color = Species)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = paste0("Time (", units[["Time"]], ")"),
      y = paste0("Concentration (", units[["Concentration"]], ")")
    )
}

# CParameter gives us only feedback on what kind of value it needs.
# It is our responsibility to call the right function which is why this list is needed.
cparameter_control_functions <-
  list(
    DOUBLE = is_scalar_numeric,
    UDOUBLE = (function(x) {is_scalar_numeric(x) && x >= 0}),
    INT = rlang::is_scalar_integerish,
    UINT = (function(x) {rlang::is_scalar_integerish(x) && x >= 0}),
    BOOL = (function(x) {length(x) != 0L && (x == 1L || x == 0L)}),
    STRING = is_scalar_character,
    GROUP = NULL,
    CN = NULL,
    KEY = NULL,
    FILE = NULL,
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
    GROUP = CCopasiParameter_setGroupValue,
    CN = CCopasiParameter_setCNValue,
    KEY = CCopasiParameter_setKeyValue,
    FILE = CCopasiParameter_setFileValue,
    EXPRESSION = NULL,
    INVALID = NULL
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
    CN = CCopasiParameter_setCNValue,
    KEY = CCopasiParameter_setKeyValue,
    FILE = CCopasiParameter_setFileValue,
    EXPRESSION = NULL,
    INVALID = NULL
  )

methodstructure <- function(method) {
  struct <-
    tibble::tibble(
      object = seq_along_v(method) %>% map(~ method$getParameter(.x)),
      name = object %>% map_chr(~ .x$getObjectName()) %>% make.names(unique = TRUE),
      type = object %>% map_chr(~ .x$getType()),
      control_fun = cparameter_control_functions[type],
      get_fun = cparameter_get_functions[type],
      set_fun = cparameter_set_functions[type]
    )
  
  if (has_element(struct$control_fun, NULL)) warning("Unknown type found with parameters: ", paste(struct$name[map_lgl(struct$control_fun, is_null)], collapse = "; "))
  
  struct
}

# Convert annotated matrices to data frames
# Is not sufficiently tested for col/row; colnames; rownames consistency
get_annotated_matrix <- function(matrix) {
  dims <- matrix$dimensionality()
  
  assert_that(dims == 2, msg = "Only two dimensional annotated matrices can be read for now.")
  
  col_headers <- get_sv(matrix$getAnnotationsString(1L))
  row_headers <- get_sv(matrix$getAnnotationsString(0L))
  
  cols <- length(col_headers)
  rows <- length(row_headers)
  
  array <- matrix$getArray()
  
  # Timecritical step optimization
  array_ref <- array@ref
  R_swig_CArrayInterface_get__SWIG_1 <- getNativeSymbolInfo("R_swig_CArrayInterface_get__SWIG_1", "COPASI")[["address"]]
  
  # assemble output dataframe
  # Iterates over all cols and rows
  # Inner loops creates numeric()
  # Outer loop binds all vectors to a data frame
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
    set_names(col_headers) %>%
    dplyr::bind_cols(list(rowname = row_headers), .)
}
