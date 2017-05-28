#'  Get species
#'
#' \code{getSpecies} returns all species as a data frame.
#'
#' @param datamodel a model object
#' @return a data frame with species and associated information
#' @export
getSpecies <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))

  metabs <- datamodel$getModel()$getMetabolites()

  # assemble output dataframe
  seq_along_cv(metabs) %>%
    map_df(~ {
      metab <- get_from_cv(metabs, .x)
      list(
        key = metab$getKey(),
        name = metab$getObjectName(),
        concentration = metab$getInitialConcentration()
      )
    })
}

#' Set species
#'
#' \code{setSpecies} accepts a data frame of species and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param species a data frame as given from getSpecies()
#' @param datamodel a model object
#' @export
setSpecies <- function(species, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel), is.data.frame(species), !anyNA(species$key), is_character(species$name), is_double(species$concentration))

  model <- datamodel$getModel()
  metabs <- model$getMetabolites()

  # assemble data frame with the model's species
  metab_df <-
    tibble::tibble(
      object = seq_along_cv(metabs) %>% map(~ get_from_cv(metabs, .x))
    ) %>%
    dplyr::mutate(key = object %>% map_chr(~ .x$getKey()))

  # add an id column to species, so I dont lose the sorting order
  species <- species %>% dplyr::mutate(id = row_number() - 1)

  # join both dataframes but only accept rows with key that exists in the model
  metab_df <-
    metab_df %>%
    dplyr::left_join(species, by = "key")

  # if all species were given, accept the new sorting order by reshuffeling the copasi vector
  if (!anyNA(metab_df$id)) {
    metab_df %>%
      pwalk(function(id, object, ...) {
        old_id <- metabs$getIndex(object)
        metabs$swap(id, old_id)
      })
  }

  # apparently I need to give changedObjects because I cant update initial values without
  changedObjects <- ObjectStdVector()

  # apply values to all species
  metab_df %>%
    pwalk(function(name, concentration, object, ...) {
      if (!is.na(name)) object$setObjectName(name)
      if (!is.na(concentration)) {
        object$setInitialConcentration(concentration)
        changedObjects$push_back(object$getObject(CCommonName("Reference=InitialConcentration")))
      }
    })

  model$updateInitialValues(changedObjects)
  delete_ObjectStdVector(changedObjects)

  # model$compileIfNecessary()

  # model$initializeMetabolites()

  invisible()
}

#'  Get Global Quantities
#'
#' \code{getGlobalQuantities} returns all species as a data frame.
#'
#' @param datamodel a model object
#' @return a data frame with global quantities and associated information
#' @export
getGlobalQuantities <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))
  
  quantities <- datamodel$getModel()$getModelValues()
  
  # assemble output dataframe
  seq_along_cv(quantities) %>%
    map_df(~ {
      quantity <- get_from_cv(quantities, .x)
      list(
        key = quantity$getKey(),
        name = quantity$getObjectName(),
        concentration = quantity$getInitialValue()
      )
    })
}

#' Set Global Quantities
#'
#' \code{setGlobalQuantities} accepts a data frame of global quantities and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param species a data frame as given from getGlobalQuantities()
#' @param datamodel a model object
#' @export
setGlobalQuantities <- function(quantities, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel), is.data.frame(quantities), !anyNA(quantities$key), is_character(quantities$name), is_double(quantities$concentration))
  
  model <- datamodel$getModel()
  quants <- model$getModelValues()
  
  # assemble data frame with the model's quantities
  quantity_df <-
    tibble::tibble(
      object = seq_along_cv(quants) %>% map(~ get_from_cv(quants, .x))
    ) %>%
    dplyr::mutate(key = object %>% map_chr(~ .x$getKey()))
  
  # add an id column to quantities, so I dont lose the sorting order
  quantities <- quantities %>% dplyr::mutate(id = row_number() - 1)
  
  # join both data frames but only accept rows with key that exists in the model
  quantity_df <-
    quantity_df %>%
    dplyr::left_join(quantities, by = "key")
  
  # if all quantities were given, accept the new sorting order by reshuffeling the copasi vector
  if (!anyNA(quantity_df$id)) {
    quantity_df %>%
      pwalk(function(id, object, ...) {
        old_id <- quants$getIndex(object)
        quants$swap(id, old_id)
      })
  }
  
  # apparently I need to give changedObjects because I cant update initial values without
  changedObjects <- ObjectStdVector()
  
  # apply values to all quantities
  quantity_df %>%
    pwalk(function(name, concentration, object, ...) {
      if (!is.na(name)) object$setObjectName(name)
      if (!is.na(concentration)) {
        object$setInitialValue(concentration)
        changedObjects$push_back(object$getObject(CCommonName("Reference=InitialValue")))
      }
    })
  
  model$updateInitialValues(changedObjects)
  delete_ObjectStdVector(changedObjects)
  
  # model$compileIfNecessary()
  
  # model$initializeMetabolites()
  
  invisible()
}
