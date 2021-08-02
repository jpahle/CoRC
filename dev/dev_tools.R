# Various helpers to make up for the fact that the swig wrapper prevents decent integration with any IDE.

# quickly reloads all of CoRC and the copasi backed for development.
# clears the workspace.
# set ´current_example´ to have it ready for testing.
reset_copasi <- function() {
  rm(list = ls(envir = .GlobalEnv) %>% .[!(. %in% c("reset_copasi", "current_example", "ls_corc", "inspect"))], envir = .GlobalEnv)
  gc()
  
  if (exists("unloadAllModels")) unloadAllModels()
  if (exists("clearCustomKineticFunctions")) clearCustomKineticFunctions()
  
  # It seems that sometimes CoRC is randomly loaded already (knitr?).
  # If so, load_all will fail due to a locked import environment
  # If import env is locked, unload CoRC
  if (isNamespaceLoaded("CoRC")) {
      import_env <- parent.env(getNamespace("CoRC"))
      stopifnot(environmentName(import_env) == "imports:CoRC")
      if (environmentIsLocked(import_env))
        unloadNamespace("CoRC")
  }
  
  pkgload::load_all(reset = FALSE)
  
  examples <- loadExamples()
  
  # current_example is typically defined in global_env
  if (exists("current_example"))
    setCurrentModel(examples[[current_example]])
  
  assign("c_datamodel", getCurrentModel(), envir = .GlobalEnv)
}

ls_corc <- function(pattern) {ls(pattern = pattern, name = "package:CoRC", all.names = TRUE)}

# inspect swig functions and objects
# e.g.
# c_datamodel <- CRootContainer_addDatamodel()
# c_datamodel %>% inspect()
# c_datamodel$addModel %>% inspect()
inspect <- function(object) {
  if (is.function(object)) {
    funtext <- object %>% environment() %>% .$f %>% format()
    funinfo <- funtext %>% paste0(collapse = "\n") %>% stringr::str_match(stringr::regex("\\n\\s*(?:ans = )?(\\.Call\\(COPASI\\$`?([^,]+)`?,?.*\\))\\n"))
    if (!all(is.na(funinfo))) {
      cat(
        funinfo[,3] %>% stringr::str_sub(8),
        funtext[1] %>% stringr::str_sub(11, -2),
        "",
        funinfo[,3],
        funinfo[,2] %>% stringr::str_replace("\\n\\s*", ""),
        sep = "\n"
      )
    } else {
      funinfo <- funtext %>% stringr::str_match("^\\s*f <- ((\\w+)__SWIG\\w*)$") %>% na.omit()
      cat(
        funinfo[1,3],
        "",
        paste0(funinfo[,2], collapse = "\n"),
        sep = "\n"
      )
    }
  } else {
    classes <- is(object)
    
    functions <-
      classes %>%
      map(safely(getMethod, quiet = TRUE), f = "$") %>%
      transpose() %>%
      .$result
    
    to_keep <- map_lgl(functions, negate(is_null))
    
    classes <- classes[to_keep]
    functions <- functions[to_keep]
    
    functions %>%
      map(~ {
        funtext <- format(.x)
        liststart <- stringr::str_which(funtext, "accessorFuns =")
        listend <- stringr::str_which(funtext %>% tail(-liststart + 1L), "\\)$")
        listtext <- paste0(funtext[seq(liststart, listend[1] + liststart - 1L)], collapse = "\n")
        
        parse(text = paste0(listtext, "\n", "names(accessorFuns)")) %>% eval()
      }) %>%
      set_names(classes) %>%
      {.[length(.):1L]}
  }
}
