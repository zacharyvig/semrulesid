# use lavaanify() to convert lavaan syntax to a parameter table, and classify
# the model type
#' @noRd
lavaan_syntax_to_partable <- function(x, lav_fun, ...) {
  dotdotdot <- list(...)
  if (grepl("\\.inp$", x, ignore.case = TRUE)) {
    id_stop(gettext(
      paste("This looks like an Mplus input file.\n",
            "Did you mean to use id_mplus() instead?")
    ))
  }
  if (isTRUE(dotdotdot$model.type == "efa")) {
    dotdotdot[["model.type"]] <- NULL
    id_warn(gettext("Only model.type='sem' is currently supported"))
  }
  if (isTRUE(dotdotdot$debug)) {
    dotdotdot[["debug"]] <- NULL
    id_warn(gettextf("Ignoring debug=%s", dotdotdot$debug))
  }
  if (is.na(lav_fun)) {
    id_warn("lav_fun= is set to NA. Defaulting to lav_fun='sem'")
    lav_fun <- "sem"
  }
  if (is.null(dotdotdot$auto)) {
    dotdotdot$auto <- (lav_fun != "lavaan")
  }
  args <- c(
    list(
      model = x,
      warn = TRUE,
      debug = FALSE,
      model.type = "sem"
    ),
    dotdotdot
  )
  partable <- do.call(lavaan::lavaanify, args)
  id_model_type <- classify_model(partable) # errors are handled in this function
  if (lav_fun == "cfa" && id_model_type != "cfa") {
    id_warn(gettext("sem() or lavaan() may be more appropriate functions for this type of model"))
  }
  list(partable = partable, lav_fun = lav_fun, id_model_type = id_model_type)
}

# convert a fitted lavaan object to a parameter table and extract the function
# used to fit the model
#' @noRd
lavaan_obj_to_partable <- function(x, lav_fun, ...) {
  dotdotdot <- list(...)
  if (length(dotdotdot) > 0) {
    id_warn(gettext("Additional arguments are ignored when a fitted lavaan object is supplied"))
  }
  lav_fun_orig <- get_lavaan_cmd(x)
  if (!is.na(lav_fun) && lav_fun_orig != lav_fun) {
    id_warn(
      gettextf("The fitted lavaan object was created with %s, but you specified lav_fun='%s'",
               format_lavaan_fun(lav_fun_orig), lav_fun),
      gettext("This may lead to unexpected results.")
    )
  }
  partable <- as.data.frame(
    x@ParTable,
    stringsAsFactors = FALSE
  )
  id_model_type <- classify_model(partable)
  return(list(partable = partable, lav_fun = lav_fun, id_model_type = id_model_type))
}

# internal validation function for lav_fun argument
#' @noRd
validate_lav_fun_arg <- function(lav_fun, options = c("lavaan", "sem", "cfa")) {
  valid <- (length(lav_fun) == 1 && (is.character(lav_fun) && lav_fun %in% options)) || is.na(lav_fun)
  if (!valid) {
    id_stop(gettextf("lav_fun= must be one of %s or NA",
      paste0("'", options, "'", collapse = ", ")))
  }
  lav_fun <- ifelse(is.na(lav_fun), NA_character_, lav_fun)
  lav_fun
}

#' Format a function call for printing for internal use
#' @keywords internal
#' @param fun A character string of the function call
#' @return A formatted character string of the function call
format_lavaan_fun <- function(fun) {
  if (length(fun) != 1) {
    id_stop(gettext("fun= must be a character string."), internal = TRUE)
  }
  if (is.null(fun) || !is.character(fun) || is.na(fun)) {
    return(NULL)
  }
  fun <- paste0("lavaan::", fun, "()")
  fun
}

#' Extracts the cmd object from a lavaan fitted object for internal use
#' @keywords internal
#' @param obj A fitted lavaan object
#' @return The function/command used to fit the model
get_lavaan_cmd <- function(obj) {
  if (!inherits(obj, "lavaan")) {
    id_stop(gettext("obj= must be a fitted lavaan object."), internal = TRUE)
  }
  cmd <- obj@call$cmd
  if (is.null(cmd)) {
    cmd <- "lavaan"
  }
  cmd
}