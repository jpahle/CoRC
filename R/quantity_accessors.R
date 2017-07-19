#' Get species
#'
#' \code{getSpecies} returns all species as a data frame.
#'
#' @param datamodel a model object
#' @return a data frame with species and associated information
#' @export
getSpecies <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  metabs <- get_cdv(datamodel$getModel()$getMetabolites())

  # assemble output dataframe
  metabs %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        name = .x$getObjectName(),
        compartment = .x$getCompartment()$getObjectName(),
        type = stringr::str_to_lower(.x$getStatus()),
        concentration = .x$getInitialConcentration(),
        particlenum = .x$getInitialValue()
      )
    }) %>%
    dplyr::select(-key, key)
}

#' Set species
#'
#' \code{setSpecies} accepts a data frame of species and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param key a character vector uniquely identifying species
#' @param name a character vector of names to set
#' @param concentration a character vector of concentrations to set
#' @param particlenum a character vector of particle numbers to set
#' @param data a data frame as given by getSpecies which will be applied before the other arguments
#' @param datamodel a model object
#' @export
setSpecies <- function(key = NULL, name = NULL, concentration = NULL, particlenum = NULL, data = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.null(key) || is_character(key) && !anyNA(key),
    is.null(name) || is_character(name) && length(name) == length(key),
    is.null(concentration) || is_numeric(concentration) && length(concentration) == length(key),
    is.null(particlenum) || is_numeric(particlenum) && length(particlenum) == length(key),
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
  if (!is_null(concentration)) {
    walk2(
      metabs, concentration,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialConcentration(.y)
          changedObjects$push_back(.x$getInitialConcentrationReference())
        }
      }
    )
  }
  
  # apply particlenum
  if (!is_null(particlenum)) {
    walk2(
      metabs, particlenum,
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
#' \code{getGlobalQuantities} returns all global quantities as a data frame.
#'
#' @param datamodel a model object
#' @return a data frame with global quantities and associated information
#' @export
getGlobalQuantities <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  quantities <- datamodel$getModel()$getModelValues()
  
  # assemble output dataframe
  get_cdv(quantities) %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        name = .x$getObjectName(),
        type = stringr::str_to_lower(.x$getStatus()),
        value = .x$getInitialValue()
      )
    })
}

#' Set global quantities
#'
#' \code{setGlobalQuantities} accepts a data frame of global quantities and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param quantities a data frame as given by getGlobalQuantities()
#' @param datamodel a model object
#' @export
setGlobalQuantities <- function(quantities, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.data.frame(quantities),
    has_name(quantities, "key"), is_character(quantities$key), !anyNA(quantities$key),
    !has_name(quantities, "name") || is_character(quantities$name),
    !has_name(quantities, "value") || is_numeric(quantities$value)
  )
  
  quantities <-
    quantities %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      object = cn_to_object(key, datamodel, "_p_CModelValue")
    )
  
  assert_that(
    !any(map_lgl(quantities$object, is_null)),
    msg = paste0("Keys in row(s) ", paste0(which(map_lgl(quantities$object, is_null)), collapse = ", "), " are invalid.")
  )
  
  # apparently I need to give changedObjects because I cant update initial values without
  changedObjects <- ObjectStdVector()
  
  # apply names
  if (has_name(quantities, "name")) {
    walk2(
      quantities$object, quantities$name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply value
  if (has_name(quantities, "value")) {
    walk2(
      quantities$object, quantities$value,
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
#' \code{getReactions} returns all reactions as a data frame.
#'
#' @param datamodel a model object
#' @return a data frame with reactions and associated information
#' @export
getReactions <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  reactions <- datamodel$getModel()$getReactions()
  
  # assemble output dataframe
  get_cdv(reactions) %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        name = .x$getObjectName()
      )
    })
}

#' Set reactions
#'
#' \code{setReactions} accepts a data frame of reactions and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param reactions a data frame as given by getReactions()
#' @param datamodel a model object
#' @export
setReactions <- function(reactions, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.data.frame(reactions),
    has_name(reactions, "key"), is_character(reactions$key), !anyNA(reactions$key),
    !has_name(reactions, "name") || is_character(reactions$name)
  )
  
  reactions <-
    reactions %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      object = cn_to_object(key, datamodel, "_p_CReaction")
    )
  
  assert_that(
    !any(map_lgl(reactions$object, is_null)),
    msg = paste0("Keys in row(s) ", paste0(which(map_lgl(reactions$object, is_null)), collapse = ", "), " are invalid.")
  )
  
  # apply names
  if (has_name(reactions, "name")) {
    walk2(
      reactions$object, reactions$name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  invisible()
}

#'  Get compartments
#'
#' \code{getCompartments} returns all compartments as a data frame.
#'
#' @param datamodel a model object
#' @return a data frame with compartments and associated information
#' @export
getCompartments <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  compartments <- datamodel$getModel()$getCompartments()
  
  # assemble output dataframe
  get_cdv(compartments) %>%
    map_df(~ {
      list(
        key = .x$getCN()$getString(),
        name = .x$getObjectName(),
        volume = .x$getInitialValue()
      )
    })
}

#' Set compartments
#'
#' \code{setCompartments} accepts a data frame of compartments and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param compartments a data frame as given by getCompartments()
#' @param datamodel a model object
#' @export
setCompartments <- function(compartments, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.data.frame(compartments),
    has_name(compartments, "key"), is_character(compartments$key), !anyNA(compartments$key),
    !has_name(compartments, "name") || is_character(compartments$name),
    !has_name(compartments, "volume") || is_numeric(compartments$value)
  )
  
  compartments <-
    compartments %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      object = cn_to_object(key, datamodel, "_p_CCompartment")
    )
  
  assert_that(
    !any(map_lgl(compartments$object, is_null)),
    msg = paste0("Keys in row(s) ", paste0(which(map_lgl(compartments$object, is_null)), collapse = ", "), " are invalid.")
  )
  
  # apparently I need to give changedObjects because I cant update initial values without
  changedObjects <- ObjectStdVector()
  
  # apply names
  if (has_name(compartments, "name")) {
    walk2(
      compartments$object, compartments$name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply volume
  if (has_name(compartments, "volume")) {
    walk2(
      compartments$object, compartments$volume,
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