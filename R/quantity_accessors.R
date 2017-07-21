#' Get species
#'
#' \code{getSpecies} returns species information as a data frame.
#'
#' @param key a character vector uniquely identifying species
#' @param datamodel a model object
#' @return a data frame with species and associated information
#' @export
getSpecies <- function(key = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  key <- species(key = key %||% character(), datamodel = datamodel)
  
  if (is_empty(key))
    metabs <- get_cdv(datamodel$getModel()$getMetabolites())
  else
    metabs <- cn_to_object(key, datamodel)
  
  # assemble output dataframe
  metabs %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        "Name" = .x$getObjectName(),
        "Compartment" = .x$getCompartment()$getObjectName(),
        "Type" = stringr::str_to_lower(.x$getStatus()),
        "Initial Concentration" = .x$getInitialConcentration(),
        "Initial Number" = .x$getInitialValue(),
        "Concentration" = .x$getInitialConcentration(),
        "Number" = .x$getInitialValue()
      )
    }) %>%
    transform_names() %>%
    dplyr::select(-key, key)
}

#' Get species references
#'
#' \code{getSpeciesReferences} returns species attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying species
#' @param datamodel a model object
#' @return a data frame with species and associated references
#' @export
getSpeciesReferences <- function(key = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  key <- species(key = key %||% character(), datamodel = datamodel)
  
  if (is_empty(key))
    metabs <- get_cdv(datamodel$getModel()$getMetabolites())
  else
    metabs <- cn_to_object(key, datamodel)
  
  # assemble output dataframe
  metabs %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        "Name" = .x$getObjectName(),
        "Compartment" = .x$getCompartment()$getObjectName(),
        "Type" = stringr::str_to_lower(.x$getStatus()),
        "Initial Concentration" = .x$getInitialConcentrationReference()$getCN()$getString(),
        "Initial Number" = .x$getInitialValueReference()$getCN()$getString(),
        "Concentration" = .x$getConcentrationReference()$getCN()$getString(),
        "Number" = .x$getValueReference()$getCN()$getString()
      )
    }) %>%
    transform_names() %>%
    dplyr::select(-key, key)
}

#' Set species
#'
#' \code{setSpecies} applies given values to species of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying species
#' @param name a character vector of names to set
#' @param initial.concentration a numeric vector of concentrations to set
#' @param initial.number a numeric vector of particle numbers to set
#' @param data a data frame as given by getSpecies which will be applied before the other arguments.
#' @param datamodel a model object
#' @export
setSpecies <- function(key = NULL, name = NULL, initial.concentration = NULL, initial.number = NULL, data = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.null(key) || is_character(key) && !anyNA(key),
    is.null(name) || is_character(name) && length(name) == length(key),
    is.null(initial.concentration) || is_numeric(initial.concentration) && length(initial.concentration) == length(key),
    is.null(initial.number) || is_numeric(initial.number) && length(initial.number) == length(key),
    is.null(data) || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  key <- species(key = key %||% character(), datamodel = datamodel)
  
  if (!is_null(data)) do.call(setSpecies, data[names(data) %in% c("key", "name", "concentration", "particlenum")])
  
  if (is_empty(key)) return(invisible())
  
  metabs <- cn_to_object(key, datamodel, "_p_CMetab")
  
  # metabs <- model$getMetabolites()
  
  # # assemble data frame with the model's species
  # metab_df <-
  #   tibble::tibble(
  #     object = get_cdv(metabs)
  #   ) %>%
  #   dplyr::mutate(key = .data$object %>% map_chr(~ .x$getCN()$getString()))
  
  # # add an id column to species, so I dont lose the sorting order
  # species <-
  #   species %>%
  #   dplyr::mutate(
  #     id = row_number() - 1L,
  #     key = as.character(key)
  #   )
  
  # # join both dataframes but only accept rows with key that exists in the model
  # metab_df <-
  #   metab_df %>%
  #   dplyr::left_join(species, by = "key")
  
  # # if all species were given, accept the new sorting order by reshuffeling the copasi vector
  # if (!anyNA(metab_df$id)) {
  #   metab_df %>%
  #     pwalk(function(id, object, ...) {
  #       old_id <- metabs$getIndex(object)
  #       metabs$swap(id, old_id)
  #     })
  # }
  
  # apparently I need to give changedObjects because I cant update initial values without
  changedObjects <- ObjectStdVector()
  
  # apply names
  if (!is_null(name)) {
    walk2(
      metabs, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply concentrations
  if (!is_null(initial.concentration)) {
    walk2(
      metabs, initial.concentration,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialConcentration(.y)
          changedObjects$push_back(.x$getInitialConcentrationReference())
        }
      }
    )
  }
  
  # apply particlenum
  if (!is_null(initial.number)) {
    walk2(
      metabs, initial.number,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialValue(.y)
          changedObjects$push_back(.x$getInitialValueReference())
        }
      }
    )
  }
  
  datamodel$getModel()$updateInitialValues(changedObjects)
  delete_ObjectStdVector(changedObjects)
  
  # model$compileIfNecessary()
  
  # model$initializeMetabolites()
  
  invisible()
}

#'  Get global quantities
#'
#' \code{getGlobalQuantities} returns global quantities as a data frame.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param datamodel a model object
#' @return a data frame with global quantities and associated information
#' @export
getGlobalQuantities <- function(key = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  key <- quantity(key = key %||% character(), datamodel = datamodel)
  
  if (is_empty(key))
    quantities <- get_cdv(datamodel$getModel()$getModelValues())
  else
    quantities <- cn_to_object(key, datamodel)
  
  # assemble output dataframe
  quantities %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        "Name" = .x$getObjectName(),
        "Type" = stringr::str_to_lower(.x$getStatus()),
        "Initial Value" = .x$getInitialValue()
      )
    }) %>%
    transform_names() %>%
    dplyr::select(-key, key)
}

#'  Get global quantitiy references
#'
#' \code{getGlobalQuantityReferences} returns global quantity attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param datamodel a model object
#' @return a data frame with global quantities and associated references
#' @export
getGlobalQuantityReferences <- function(key = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  key <- quantity(key = key %||% character(), datamodel = datamodel)
  
  if (is_empty(key))
    quantities <- get_cdv(datamodel$getModel()$getModelValues())
  else
    quantities <- cn_to_object(key, datamodel)
  
  # assemble output dataframe
  quantities %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        "Name" = .x$getObjectName(),
        "Type" = stringr::str_to_lower(.x$getStatus()),
        "Initial Value" = .x$getInitialValueReference()$getCN()$getString()
      )
    }) %>%
    transform_names() %>%
    dplyr::select(-key, key)
}

#' Set global quantities
#'
#' \code{setGlobalQuantities} applies given values to global quantities of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param name a character vector of names to set
#' @param initial.volume a numeric vector of values to set
#' @param data a data frame as given by getGlobalQuantities which will be applied before the other arguments.
#' @param datamodel a model object
#' @export
setGlobalQuantities <- function(key = NULL, name = NULL, initial.value = NULL, data = NULL, datamodel = pkg_env$curr_dm) {  assert_datamodel(datamodel)
  assert_datamodel(datamodel)
  assert_that(
    is.null(key) || is_character(key) && !anyNA(key),
    is.null(name) || is_character(name) && length(name) == length(key),
    is.null(initial.value) || is_numeric(initial.value) && length(initial.value) == length(key),
    is.null(data) || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  key <- quantity(key = key %||% character(), datamodel = datamodel)
  
  if (!is_null(data)) do.call(setGlobalQuantities, data[names(data) %in% c("key", "name", "initial.value")])
  
  if (is_empty(key)) return(invisible())
  
  quants <- cn_to_object(key, datamodel, "_p_CModelValue")
  
  # apparently I need to give changedObjects because I cant update initial values without
  changedObjects <- ObjectStdVector()
  
  # apply names
  if (!is_null(name)) {
    walk2(
      quants, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply value
  if (!is_null(initial.value)) {
    walk2(
      quants, initial.value,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialValue(.y)
          changedObjects$push_back(.x$getInitialValueReference())
        }
      }
    )
  }
  
  datamodel$getModel()$updateInitialValues(changedObjects)
  delete_ObjectStdVector(changedObjects)
  
  # model$compileIfNecessary()
  
  # model$initializeMetabolites()
  
  invisible()
}

#'  Get compartments
#'
#' \code{getCompartments} returns compartments as a data frame.
#'
#' @param key a character vector uniquely identifying compartments
#' @param datamodel a model object
#' @return a data frame with compartments and associated information
#' @export
getCompartments <- function(key = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  key <- compartment(key = key %||% character(), datamodel = datamodel)
  
  if (is_empty(key))
    comps <- get_cdv(datamodel$getModel()$getCompartments())
  else
    comps <- cn_to_object(key, datamodel)
  
  # assemble output dataframe
  comps %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        "Name" = .x$getObjectName(),
        "Initial Volume" = .x$getInitialValue()
      )
    }) %>%
    transform_names() %>%
    dplyr::select(-key, key)
}

#'  Get compartment references
#'
#' \code{getCompartmentReferences} returns compartment attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying compartments
#' @param datamodel a model object
#' @return a data frame with compartments and associated references
#' @export
getCompartmentReferences <- function(key = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  key <- compartment(key = key %||% character(), datamodel = datamodel)
  
  if (is_empty(key))
    comps <- get_cdv(datamodel$getModel()$getCompartments())
  else
    comps <- cn_to_object(key, datamodel)
  
  # assemble output dataframe
  comps %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        "Name" = .x$getObjectName(),
        "Initial Volume" = .x$getInitialValueReference()$getCN()$getString()
      )
    }) %>%
    transform_names() %>%
    dplyr::select(-key, key)
}

#' Set compartments
#'
#' \code{setCompartments} applies given values to compartments of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying compartments
#' @param name a character vector of names to set
#' @param initial.volume a numeric vector of values to set
#' @param data a data frame as given by getCompartments which will be applied before the other arguments.
#' @param datamodel a model object
#' @export
setCompartments <- function(key = NULL, name = NULL, initial.volume = NULL, data = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.null(key) || is_character(key) && !anyNA(key),
    is.null(name) || is_character(name) && length(name) == length(key),
    is.null(initial.volume) || is_numeric(initial.volume) && length(initial.volume) == length(key),
    is.null(data) || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  key <- compartment(key = key %||% character(), datamodel = datamodel)
  
  if (!is_null(data)) do.call(setCompartments, data[names(data) %in% c("key", "name", "initial.volume")])
  
  if (is_empty(key)) return(invisible())
  
  comps <- cn_to_object(key, datamodel, "_p_CCompartment")
  
  # apparently I need to give changedObjects because I cant update initial values without
  changedObjects <- ObjectStdVector()
  
  # apply names
  if (!is_null(name)) {
    walk2(
      comps, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply volume
  if (!is_null(initial.volume)) {
    walk2(
      comps, initial.volume,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialValue(.y)
          changedObjects$push_back(.x$getInitialValueReference())
        }
      }
    )
  }
  
  datamodel$getModel()$updateInitialValues(changedObjects)
  delete_ObjectStdVector(changedObjects)
  
  # model$compileIfNecessary()
  
  # model$initializeMetabolites()
  
  invisible()
}

#'  Get reactions
#'
#' \code{getReactions} returns reactions as a data frame.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param datamodel a model object
#' @return a data frame with reactions and associated information
#' @export
getReactions <- function(key = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  key <- reaction(key = key %||% character(), datamodel = datamodel)
  
  if (is_empty(key))
    reactions <- get_cdv(datamodel$getModel()$getReactions())
  else
    reactions <- cn_to_object(key, datamodel)
  
  # assemble output dataframe
  reactions %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        "Name" = .x$getObjectName()
      )
    }) %>%
    transform_names() %>%
    dplyr::select(-key, key)
}

#' Set reactions
#'
#' \code{setReactions} applies given values to reactions of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying reactions
#' @param name a character vector of names to set
#' @param data a data frame as given by getReactions which will be applied before the other arguments.
#' @param datamodel a model object
#' @export
setReactions <- function(key = NULL, name = NULL, data = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.null(key) || is_character(key) && !anyNA(key),
    is.null(name) || is_character(name) && length(name) == length(key),
    is.null(data) || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  key <- reaction(key = key %||% character(), datamodel = datamodel)
  
  if (!is_null(data)) do.call(setReactions, data[names(data) %in% c("key", "name")])
  
  if (is_empty(key)) return(invisible())
  
  reactions <- cn_to_object(key, datamodel, "_p_CReaction")
  
  # apply names
  if (!is_null(name)) {
    walk2(
      reactions, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  invisible()
}