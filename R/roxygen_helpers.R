# Create an @param line for roxygen documentation
# The line lists allowed values given by vec
rox_param <- function(name, type, vec) {
  c(
    paste0("@param ", name, " ", type),
    "",
    paste0("Allowed values: ", paste0(vec, collapse = ", "))
  )
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
    paste0("@param method string or list"),
    "",
    "Set method with a string or a list with the entry `method`. The list may contain further method args and their values.",
    paste0("Allowed methods: ", paste0(values, collapse = ", "))
  )
}