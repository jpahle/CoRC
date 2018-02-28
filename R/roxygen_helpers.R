# Format vector for roxygen documentation
rox_print_v <- function(vec) {
  paste0("'", paste0(vec, collapse = "', '"), "'")
}

# Create an @param line for roxygen documentation
# Specifically for "@param method ..."
# The line lists allowed task methods for given task
rox_method_param <- function(task, class) {
  c_datamodel <- CRootContainer_addDatamodel()
  
  c_task <- as(c_datamodel$getTask(task), class)
  
  values <- names(.__E___CTaskEnum__Method)[c_task$getValidMethods() + 1L]
  
  delete(c_datamodel)
  
  c(
    "@param method string or list",
    "",
    "Set the method directly with a string or with a list containing the entry named `method`.",
    "",
    "The list may contain further method arguments and their values.",
    "",
    paste0("Available methods: ", rox_print_v(values), ".")
  )
}