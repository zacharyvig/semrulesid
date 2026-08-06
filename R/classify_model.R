#' Classify a model using the parameter table for internal use
#' @keywords internal
classify_model <- function(partable = NULL) {
  model_type <- NA
  # retrieve attributes and variable names
  lavpta <- lavaan::lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # check for MLM
  if (lavpta$nblocks > 1 | "~*~" %in% partable$op | "|" %in% partable$op) {
    stop("This model type is not currently supported")
  }
  # tally variables
  nlv <- lapply(vnames$lv, length) # latent vars
  nov.nox <- lapply(vnames$ov.nox, length) # non-exogenous observed vars
  neqs.y <- lapply(vnames$eqs.y, length) # dependent regression vars
  nov.cind <- lapply(vnames$ov.cind, length) # causal indicators
  # check a list for counts greater than zero
  nonzero <- function(x) {
    return(isTRUE(x > 0))
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
    stop("Cannot classify model!")
  }
}

#' Extracts the call from a lavaan fitted object for internal use
#' @keywords internal
#' @param obj A fitted lavaan object
#' @return The call used to fit the model
get_lavaan_call <- function(obj) {
  if (!inherits(obj, "lavaan")) {
    stop("`obj` must be a fitted lavaan object")
  }
  call <- obj@call$cmd
  if (is.null(call)) {
    call <- "lavaan"
  }
  return(call)
}

#' Format a call for printing for internal use
#' @keywords internal
#' @param call A character string of the function call
#' @return A formatted character string of the function call
format_lavaan_call <- function(call) {
  if (is.null(call) || !is.character(call)) {
    return(NULL)
  }
  call <- paste0("`lavaan::", call, "()`")
  return(call)
}