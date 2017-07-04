pkg_env <- new.env(parent = emptyenv())

# Using any function with the datamodel parameter set will redefine the currently active datamodel
pkg_env$curr_dm <- NULL

# Format function for the CDataModel class which is used as a basis for the print method
#' @include swig_wrapper.R
#' @export
setMethod("format",
  "_p_CDataModel",
  function(x, ...) {
    model <- x$getModel()

    string <- "# A copasi model reference:\n"
    string <- paste0(string, "Model name: \"" , model$getObjectName() , "\"\n")
    string <- paste0(string, "@ref is set to: " , capture.output(x@ref) , "\n")
    string <- paste0(string, "Number of compartments: " , model$getCompartments()$size(), "\n")
    string <- paste0(string, "Number of species: " , model$getMetabolites()$size(), "\n")
    string <- paste0(string, "Number of reactions: " , model$getReactions()$size(), "\n")

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
confirmDatamodel <- function(datamodel) {
  success <- is(datamodel, "_p_CDataModel")

  if (success) pkg_env$curr_dm <- datamodel

  success
}

# finds copasi messages and helps purge known annoying messages
grab_msg <- function(x, purge = NULL) {
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
    if (!is_null(purge))
      messages <- messages[map_lgl(messages, ~ !any(stringr::str_detect(.x, pattern = purge)))]
    
    if (!is_empty(messages)) {
      warning(
        "Process generated Copasi Message(s):\n",
        paste0(messages, collapse = "\n")
      )
    }
  }
  
  x
}

# Better error message for assert_that
assertthat::on_failure(confirmDatamodel) <- function(call, env) {
  paste0(deparse(call$datamodel), " is not a datamodel")
}

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
  
  # reshape data frame for ggplot and define the plot
  object %>%
    tidyr::gather(Species, Concentration, -Time) %>%
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Concentration, group = Species, color = Species)) +
    ggplot2::geom_line()
}

# CParameter gives us only feedback on what kind of value it needs.
# It is our responsibility to call the right function which is why this list is needed.
cparameter_control_functions <-
  list(
    DOUBLE = is_scalar_numeric,
    UDOUBLE = (function(x) {is_scalar_numeric(x) && x >= 0}),
    INT = rlang::is_scalar_integerish,
    UINT = (function(x) {rlang::is_scalar_integerish && x >= 0}),
    BOOL = (function(x) {length(x) == 1 && (x == 1 || x == 0)}),
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
      object = seq_along_cv(method) %>% map(~ method$getParameter(.x)),
      name = object %>% map_chr(~ .x$getObjectName()) %>% make.names(unique = TRUE),
      type = object %>% map_chr(~ .x$getType()),
      control_fun = cparameter_control_functions[type],
      get_fun = cparameter_get_functions[type],
      set_fun = cparameter_set_functions[type]
    )
  
  if (contains(struct$control_fun, NULL)) warning("Unknown type found with parameters: ", paste(struct$name[map_lgl(struct$control_fun, is_null)], collapse = "; "))
  
  struct
}
