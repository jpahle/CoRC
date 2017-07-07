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
  get_cdv(metabs) %>%
    map_df(~ {
      list(
        key = list(as.copasi_key(.x$getCN()$getString())),
        name = .x$getObjectName(),
        compartment = .x$getCompartment()$getObjectName(),
        type = stringr::str_to_lower(.x$getStatus()),
        concentration = .x$getInitialConcentration(),
        particlenum = .x$getInitialValue()
      )
    })
}

#' Set species
#'
#' \code{setSpecies} accepts a data frame of species and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param species a data frame as given by getSpecies()
#' @param datamodel a model object
#' @export
setSpecies <- function(species, datamodel = pkg_env$curr_dm) {
  assert_that(
    confirmDatamodel(datamodel),
    is.data.frame(species),
    has_name(species, "key"), !anyNA(species$key),
    !has_name(species, "name") || is_character(species$name),
    !has_name(species, "concentration") || is_numeric(species$concentration),
    !has_name(species, "particlenum") || rlang::is_integerish(species$particlenum)
  )

  species <-
    species %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      object = cn_to_object(key, datamodel, "_p_CMetab")
    )
  
  assert_that(
    !any(map_lgl(species$object, is_null)),
    msg = paste0("Keys in row(s) ", paste0(which(map_lgl(species$object, is_null)), collapse = ", "), " are invalid.")
  )
  
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
  if (has_name(species, "name")) {
    walk2(
      species$object, species$name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply concentrations
  if (has_name(species, "concentration")) {
    walk2(
      species$object, species$concentration,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialConcentration(.y)
          changedObjects$push_back(.x$getInitialConcentrationReference())
        }
      }
    )
  }
  
  # apply particlenum
  if (has_name(species, "particlenum")) {
    walk2(
      species$object, species$particlenum,
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
  assert_that(confirmDatamodel(datamodel))
  
  quantities <- datamodel$getModel()$getModelValues()
  
  # assemble output dataframe
  get_cdv(quantities) %>%
    map_df(~ {
      list(
        key = list(as.copasi_key(.x$getCN()$getString())),
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
  assert_that(
    confirmDatamodel(datamodel),
    is.data.frame(quantities),
    has_name(quantities, "key"), !anyNA(quantities$key),
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
  assert_that(confirmDatamodel(datamodel))
  
  reactions <- datamodel$getModel()$getReactions()
  
  # assemble output dataframe
  get_cdv(reactions) %>%
    map_df(~ {
      list(
        key = list(as.copasi_key(.x$getCN()$getString())),
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
  assert_that(
    confirmDatamodel(datamodel),
    is.data.frame(reactions),
    has_name(reactions, "key"), !anyNA(reactions$key),
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
  assert_that(confirmDatamodel(datamodel))
  
  compartments <- datamodel$getModel()$getCompartments()
  
  # assemble output dataframe
  get_cdv(compartments) %>%
    map_df(~ {
      list(
        key = list(as.copasi_key(.x$getCN()$getString())),
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
  assert_that(
    confirmDatamodel(datamodel),
    is.data.frame(compartments),
    has_name(compartments, "key"), !anyNA(compartments$key),
    !has_name(compartments, "name") || is_character(compartments$name),
    !has_name(compartments, "volume") || is_numeric(compartments$value)
  )
  
  compartments <-
    compartments %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      object = cn_to_object(key, datamodel, "_p_CModelValue")
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