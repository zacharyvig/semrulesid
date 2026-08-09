#' Classify a model using the parameter table for internal use
#' @keywords internal
classify_model <- function(partable = NULL) {
  # retrieve attributes and variable names
  lavpta <- lavaan::lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # check for MLM
  if (lavpta$nblocks > 1 || "~*~" %in% partable$op || "|" %in% partable$op) {
    stop(gettext("This model type is not currently supported."))
  }
  # tally variables
  nlv <- lapply(vnames$lv, length) # latent vars
  nov.nox <- lapply(vnames$ov.nox, length) # non-exogenous observed vars
  neqs.y <- lapply(vnames$eqs.y, length) # dependent regression vars
  nov.cind <- lapply(vnames$ov.cind, length) # causal indicators
  # check a list for counts greater than zero
  nonzero <- function(x) {
    isTRUE(x > 0)
  }
  # classify model
  if (any(sapply(nlv, nonzero))) {
    if (any(sapply(neqs.y, nonzero)) || any(sapply(nov.cind, nonzero))) {
      # lvs present and regressions/causal indicators present
      return("sem")
    } else {
      # lvs present but no regressions
      return("cfa")
    }
  } else if (any(sapply(nov.nox, nonzero))) {
    # no lvs but regressions present
    return("reg")
  } else {
    stop(gettext("Cannot classify model. This is an internal error. Please report this issue to the package maintainer."))
  }
}

#' Extracts the cmd object from a lavaan fitted object for internal use
#' @keywords internal
#' @param obj A fitted lavaan object
#' @return The function/command used to fit the model
get_lavaan_cmd <- function(obj) {
  if (!inherits(obj, "lavaan")) {
    stop(gettextf("'%s' must be a fitted lavaan object", "obj"))
  }
  cmd <- obj@call$cmd
  if (is.null(cmd)) {
    cmd <- "lavaan"
  }
  cmd
}

#' Format a function call for printing for internal use
#' @keywords internal
#' @param fun A character string of the function call
#' @return A formatted character string of the function call
format_lavaan_fun <- function(fun) {
  if (is.null(fun) || !is.character(fun)) {
    return(NULL)
  }
  fun <- paste0("`lavaan::", fun, "()`")
  fun
}