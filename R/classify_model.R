#' Classify a model using the parameter table for internal use
#' @keywords internal
classify_model <- function(partable = NULL) {
  # retrieve attributes and variable names
  lavpta <- lavaan::lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # check for MLM
  if (lavpta$nblocks > 1 || "~*~" %in% partable$op || "|" %in% partable$op) {
    id_stop(gettext("This model type is not currently supported."))
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
    id_stop(gettext("Cannot classify model. This is an internal error. Please report this issue to the package maintainer."))
  }
}

# internal function to convert model type abbrevation to full name
#' @noRd
get_model_type_name <- function(model_type, capitalize = TRUE, plural = FALSE, long = TRUE) {
  model_type <- match.arg(model_type, c("reg", "cfa", "sem"))
  if (long) {
    title <- switch(
      model_type,
      reg = "simultaneous equations model",
      cfa = "confirmatory factor analysis model",
      sem = "general structural equation model"
    )
  } else {
    title <- switch(
      model_type,
      sem = "general SEM",
      cfa = "CFA model",
      reg = "simultaneous equations model"
    )
  }
  if (capitalize) {
    title <- tools::toTitleCase(title)
  }
  if (plural) {
    title <- paste0(title, "s")
  }
  title
}