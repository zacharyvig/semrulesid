#' Rules for simultaneous equations/regression models
#'
#' Simultaneous equations models are structural models without any latent
#' variables. Regression models are a subset of these models with a single
#' outcome, while the general simultaneous equations model can accomodate
#' multiple outcomes
#'
#' \describe{
#'  \item{Null B_YY Rule}{No endogenous variable is the predictor of another
#'  endogenous variable. Sufficient but not necessary.}
#'  \item{Fully Recursive Model Rule}{The model has no feedback loops (i.e.,
#'  is recursive) and there are no correlated errors ("fully" recursive).
#'  Sufficient but not necessary.}
#'  \item{Recursive with Correlated Errors Rule}{The model has no feedback loops
#'  (i.e., is recursive) but can have correlated errors as long as the errors are
#'  not for terms between which exists a direct structural/directional path.
#'  Sufficient but not necessary.}
#' }
#'
#' @name reg_rules
#'
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Brito, C., & Pearl, J. (2002). A new identification condition
#' for recursive models with correlated errors.
#' @keywords internal
NULL

# Null B_YY Rule
#' @rdname reg_rules
#' @keywords internal
rule_reg_null_byy <- function(partable) {
  rule <- "Null B_YY Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov", "eqs.x", "eqs.y"))
  ov.ox <- intersect(vars$eqs.x, vars$ov)
  ov.nox <- intersect(vars$eqs.y, vars$ov)
  if (length(vars$lv) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      cond = NA_character_,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when there are no latent variables in the model",
        levels = "1"
      )
    )
    return(out)
  }
  # check if endogenous vars appear as exogenous vars
  nox_ox <- ov.nox %in% ov.ox; names(nox_ox) <- ov.nox
  # build output
  if (any(nox_ox[!is.na(nox_ox)])) {
    pass <- FALSE
    msgs <- add_rule_msgs(
      new_msgs = paste("One or more endogenous variables appear as regression predictors:",
                       paste(ov.nox[nox_ox], collapse = ", ")),
      levels = 2
    )
  } else {
    pass <- TRUE
    msgs <- NA_character_
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "S"
  )
  return(out)
}

# Fully Recursive model rule
#' @rdname reg_rules
#' @keywords internal
rule_reg_fully_recursive <- function(partable) {
  rule <- "Fully Recursive Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "eqs.x", "ov.nox"))
  if (length(vars$lv) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      cond = NA_character_,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when there are no latent variables in the model",
        levels = "1"
      )
    )
    return(out)
  }
  # check recursion
  recursive <- check_recursion(partable, start = vars$eqs.x)
  # check uncorrelated errors of endogenous variables
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.ov.nox <- with(covs, lhs %in% vars$ov.nox & rhs %in% vars$ov.nox)
  # build output
  if (isFALSE(recursive)) {
    pass <- FALSE
    msgs <- add_rule_msgs(
      new_msgs = "Feedback loops exist in the model, i.e., the model is non-recursive",
      levels = "2"
    )
  } else if (any(cor_err.ov.nox)) {
    pass <- FALSE
    viol <- paste(covs$lhs[cor_err.ov.nox], covs$rhs[cor_err.ov.nox], sep = "/")
    msgs <- add_rule_msgs(
      new_msgs = paste("The model is recursive but some endogenous variables have correlated errors:",
                   paste(viol, collapse = ", ")),
      levels = "2"
    )
  } else {
    pass <- TRUE
    msgs <- NA_character_
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "S"
  )
  return(out)
}

# Recursive model with correlated errors rule
#' @rdname reg_rules
#' @keywords internal
rule_reg_recursive_corr_err <- function(partable) {
  rule <- "Recur/Corr Err Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "eqs.x"))
  if (length(vars$lv) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      cond = NA_character_,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when there are no latent variables in the model",
        levels = "1"
      )
    )
    return(out)
  }
  # check recursion
  recursive <- check_recursion(partable, start = vars$eqs.x)
  # check uncorrelated errors of regression variables
  regs <- subset(partable, op == "~" & free > 0)
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.eqs <- unname(
    apply(covs, 1, function(row) {
      any(with(regs, lhs %in% row["lhs"] & rhs %in% row["rhs"] |
                 lhs %in% row["rhs"] & rhs %in% row["lhs"]))
    }, simplify = TRUE)
  )
  # build output
  if (isFALSE(recursive)) {
    pass <- FALSE
    msgs <- add_rule_msgs(
      new_msgs = "Feedback loops exist in the model, i.e., the model is non-recursive",
      levels = "2"
    )
  } else if (any(cor_err.eqs)) {
    pass <- FALSE
    viol <- paste(covs$lhs[cor_err.eqs], covs$rhs[cor_err.eqs], sep = "/")
    msgs <- add_rule_msgs(
      new_msgs = paste("The model is recursive but some directly related variables have correlated errors:",
                   paste(viol, collapse = ", ")),
      levels = "2"
    )
  } else {
    pass <- TRUE
    msgs <- NA_character_
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "S"
  )
  return(out)
}
