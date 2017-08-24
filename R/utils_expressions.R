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
  map_chr(
    expression,
    ~ {
      c_expression <- avert_gc(CExpression("CoRC_transl_expr", datamodel))
      c_expression$setInfix(.x)
      # c_expression$compile()
      c_expression$getDisplayString()
      delete(c_expression)
    }
  )
}
