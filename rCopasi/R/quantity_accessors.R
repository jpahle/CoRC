#'  Get species
#'
#' \code{getSpecies} returns all species as a dataframe.
#'
#' @param datamodel a model object
#' @return a dataframe with species and associated information
#' @export
getSpecies <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))

  metabs <- datamodel$getModel()$getMetabolites()

  # assemble output dataframe
  seq_along_cv(metabs) %>%
    map_df(function(x) {
      metab <- get_from_cv(metabs, x)
      list(
        key = metab$getKey(),
        name = metab$getObjectName(),
        concentration = metab$getInitialConcentration()
      )
    })
}

#' Set species
#'
#' \code{setSpecies} accepts a dataframe of species and attempts to apply given values to the model depending on the 'key' column.
#'
#' @param species a dataframe as given from getSpecies()
#' @param datamodel a model object
#' @export
setSpecies <- function(species, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel), is(species, "tbl_df"), !anyNA(species$key), is_character(species$name), is_double(species$concentration))

  metabs <- datamodel$getModel()$getMetabolites()

  # assemble dataframe with the models species
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

  # apply values to all species
  metab_df %>%
    pwalk(function(name, concentration, object, ...) {
      if (!is.na(name)) object$setObjectName(name)
      if (!is.na(concentration)) object$setInitialConcentration(concentration)
    })

  # if all species were given, accept the new sorting order by reshuffeling the copasi vector
  if (!anyNA(metab_df$id)) {
    metab_df %>%
      pwalk(function(id, object, ...) {
        old_id <- metabs$getIndex(object)
        metabs$swap(id, old_id)
      })
  }

  # apparently I need to give changedObjects because I cant update initial values without
  # for now it just adds all species
  changedObjects <- ObjectStdVector()
  metab_df$object %>% walk(~ changedObjects$push_back(.x$getObject(CCommonName("Reference=InitialConcentration"))))

  datamodel$getModel()$updateInitialValues(changedObjects)

  # datamodel$getModel()$compileIfNecessary()

  # datamodel$getModel()$initializeMetabolites()

  invisible()
}
