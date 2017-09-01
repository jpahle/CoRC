#' Calculate the value of an expression or reference
#'
#' \code{getValue} calculates the value of a given expression or reference.
#'
#' @param expression an character vector of expressions
#' @param model a model object
#' @return a numeric vector of values
#' @export
getValue <- function(expression, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.character(expression))
  
  expression %>%
    map_chr(write_expr, c_datamodel) %>%
    map_dbl(get_expr_val, c_datamodel)
}

#' Calculate the initial value of an expression or reference
#'
#' \code{getValue} calculates the initial value of a given expression or reference.
#'
#' @param expression an character vector of expressions
#' @param model a model object
#' @return a numeric vector of initial values
#' @export
getInitialValue <- function(expression, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.character(expression))
  
  expression %>%
    map_chr(write_expr, c_datamodel) %>%
    map_dbl(get_expr_init_val, c_datamodel)
}

# check if an entity has an expression set
# return NA_character_ or the expression string
expr_to_str <- function(c_entity, raw = FALSE) {
  c_expression <- c_entity$getExpressionPtr()
  c_datamodel <- c_expression$getObjectDataModel()
  assert_that(!is.null(c_datamodel))
  
  if (is.null(c_expression))
    NA_character_
  else if (raw)
    c_expression$getInfix()
  else
    read_expr(c_expression$getInfix(), c_datamodel)
}

# check if an entity has an expression set
# return NA_character_ or the expression DisplayName
expr_to_ref_str <- function(c_entity) {
  c_expression <- c_entity$getExpressionPtr()
  c_datamodel <- c_expression$getObjectDataModel()
  assert_that(!is.null(c_datamodel))
  
  if (is.null(c_expression))
    NA_character_
  else
    as_ref(list(c_expression), c_datamodel)
}

# Copasi -> R
# In copasi, expressions consist of <CN>, in R they should be mostly {DN}
read_expr <- function(x, c_datamodel) {
  stringr::str_replace_all(
    x,
    "<CN=.*?[^\\\\]>",
    function(x) {
      x <- stringr::str_sub(x, 2L, -2L)
      c_obj <- cn_to_object(x, c_datamodel)
      assert_that(!is.null(c_obj), msg = paste0("Failure in expression readout"))
      
      escape_ref(c_obj$getObjectDisplayName())
    }
  )
}

# R -> Copasi
# In copasi, expressions consist of <CN>, in R they should be mostly {DN}
write_expr <- function(x, c_datamodel) {
  # <CN=Root,Model=mouse Anoctamin-1 (ac variant) model according to Contreras-Vite et. al. (2016),Reference=Avogadro Constant>
  # <CN=Root,Model=mouse Anoctamin-1 (ac variant) model according to Contreras-Vite et. al. (2016),Reference=Quantity Conversion Factor>
  # <CN=Root,Model=mouse Anoctamin-1 (ac variant) model according to Contreras-Vite et. al. (2016),Reference=Time>
  # {Quantity Conversion Factor} <- 
  # {Avogadro Constant}
  # {Time}
  
  stringr::str_replace_all(
    x,
    "\\{.*?[^\\\\]\\}",
    function(x) {
      c_obj <- dn_to_object(unescape_ref(x), c_datamodel)
      assert_that(!is.null(c_obj), msg = paste0("Cannot resolve ", x, "."))
      
      if (c_obj$getObjectType() != "Reference")
        c_obj <- c_obj$getValueReference()
      
      paste0("<", get_cn(c_obj), ">")
    }
  )
}

# calculate the value of an expression
get_expr_val <- function(x, c_datamodel) {
  # avert_gc so i can delete it right away
  # CExpressions get destructed on model unloading and
  # are therefore unsafe to keep around until next gc
  c_expression <- avert_gc(CExpression("CoRC_value_expr", c_datamodel))
  
  grab_msg(c_expression$setInfix(x))
  c_expression$compile()
  val <- c_expression$calcValue()
  
  delete(c_expression)
  
  val
}

# calculate the initial value of an expression
get_expr_init_val <- function(x, c_datamodel) {
  # avert_gc so i can delete it right away
  # CExpressions get destructed on model unloading and
  # are therefore unsafe to keep around until next gc
  c_expression <- avert_gc(CExpression("CoRC_value_expr", c_datamodel))
  c_init_expression <- grab_msg(CExpression_createInitialExpression(c_expression, c_datamodel))
  delete(c_expression)
  
  grab_msg(c_init_expression$setInfix(x))
  c_init_expression$compile()
  val <- c_init_expression$calcValue()
  
  delete(c_init_expression)
  
  val
}
