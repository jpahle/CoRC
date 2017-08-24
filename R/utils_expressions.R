# check if an entity has an expression set
# return NA_character_ or the expression string
expr_to_str <- function(c_entity, pretty = FALSE) {
  c_expression <- c_entity$getExpressionPtr()
  
  if (is_null(c_expression)) {
    NA_character_
  } else if (pretty) {
    c_expression$getDisplayString()
  } else {
    c_expression$getInfix()
  }
}

# check if an entity has an expression set
# return NA_character_ or the expression DisplayName
expr_to_ref_str <- function(c_entity) {
  c_expression <- c_entity$getExpressionPtr()
  
  if (is_null(c_expression))
    NA_character_
  else
    c_expression$getObjectDisplayName()
}

#' @export
prettify <- function(expression, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(is.string(expression))
  map_chr(
    expression,
    ~ {
      c_expression <- avert_gc(CExpression("CoRC_transl_expr", datamodel))
      grab_msg(c_expression$setInfix(.x))
      c_expression$compile()
      str <- c_expression$getDisplayString()
      delete(c_expression)
      str
    }
  )
}

#' @export
defineExpression <- function(template, ..., validate = TRUE, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(
    is.string(template),
    is.flag(validate)
  )
  
  args <- as.list(...)
  arg_count = length(args)
  
  repfun <- function(x) {
    x <- as.integer(stringr::str_sub(x, 2L, -2L))
    assert_that(x <= arg_count, msg = paste0("No value supplied for {", x, "}."))
    
    c_obj <- dn_to_object(args[[x]], datamodel)
    assert_that(!is_null(c_obj), msg = paste0("Could not resolve {", x, "}."))
    
    paste0("<", c_obj$getCN()$getString(), ">")
  }
  
  expression <- stringr::str_replace_all(
    template,
    "\\{\\d+\\}",
    repfun
  )
  
  if (validate)
    assert_that(
      !is.nan(get_expr_val(expression, datamodel)),
      msg = "The generated expression is invalid."
    )
  
  expression
}

#' @export
getExpressionValue <- function(expression, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(is.string(expression))
  
  get_expr_val(expression, datamodel)
}

get_expr_val <- function(x, datamodel) {
  c_expression <- avert_gc(CExpression("CoRC_validate_expr", datamodel))
  grab_msg(c_expression$setInfix(x))
  c_expression$compile()
  val <- c_expression$calcValue()
  delete(c_expression)
  
  val
}
