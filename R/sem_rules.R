#' Rules for all structural equation models
#'
#' Structural Equation Models (SEM) refer to the general class of models
#' consisting of structural/directional relations, latent variables, or both.
#'
#' \describe{
#'  \item{N_theta Rule (a.k.a. t-Rule)}{The number of free parameters must be
#'  less than or equal to the number of sample means, variances, and
#'  covariances. Necessary but not sufficient}
#'  \item{Latent Scaling Rule}{In a model with latent variables, all latent
#'  variables must be correctly scaled (see \link[semrulesid]{scaling}).
#'  Necessary but not sufficient.}
#'  \item{Two Emitted Paths Rule}{In a model with latent variables, all latent
#'  variables must emit two paths, either to other latent variables or to
#'  observed variables. This rule only applies to latent variables that have
#'  free variances and whose downstream variables have free error/disturbance
#'  variance. Necessary but not sufficient.}
#'  \item{Exogenous X Rule/MIMIC Rules}{These rules apply to models in which one
#'  or more latent variables have a causal indicator (or generally, are
#'  downstream of an observed variable), in addition to effect
#'  indicators. In such a model with a single latent variable, there only need
#'  to be one (or more) causal indicators as long as there are at least two
#'  effect indicators. This rule is not currently implemented for models with
#'  multiple latent variables. Sufficient but not necessary.}
#' }
#'
#' @name sem_rules
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen, K. A., Lilly, A. G., & Luo, L. (2024). Selecting scaling
#' indicators in structural equation models (SEMs).
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Bollen & Davis (2009). Two Rules of Identification for Structural
#' Equation Models.
#' @keywords internal
NULL


# N_theta rule (or t-Rule; compares parameters to observed statistics)
#' @rdname sem_rules
#' @keywords internal
rule_sem_ntheta <- function(partable) {
  rule <- "N_theta Rule (t-Rule)"
  # number of parameters (internal function)
  ntheta <- get_ntheta(partable)
  # number of means, variances, and covariances
  ndat <- get_ndata(partable)
  pass <- isTRUE(ntheta <= ndat)
  if (pass) {
    msgs <- NA
  } else {
    msgs <- add_rule_msgs(
      new_msgs = sprintf("The number of free parameters (=%s) exceeds the number of means/variances/covariances (=%s)", ntheta, ndat),
      levels = "identification_failure"
    )
  }
  build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "N"
  )
}
attr(rule_sem_ntheta, "applies_to") <- c("reg", "cfa", "sem")

# Latent Scaling rule
#' @rdname sem_rules
#' @keywords internal
rule_sem_latent_scaling <- function(partable) {
  rule <- "Latent Scaling Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, "lv")
  if (length(vars$lv) == 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when there are latent variables in the model",
        levels = "not_applicable"
      ),
      cond = NA_character_
    )
    return(out)
  }
  # build output
  scaled <- scaling(partable, lv = vars$lv, return_type = "logical", lav_fun = NA)
  pass <- isTRUE(all(scaled))
  cond <- "N"
  if (!pass) {
    msgs <- add_rule_msgs(
      new_msgs = paste("Some latent variables are not scaled:",
                paste(vars$lv[!scaled], collapse = ", ")),
      levels = "identification_failure"
    )
  } else {
    msgs <- NA_character_
  }
  build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
}
attr(rule_sem_latent_scaling, "applies_to") <- c("cfa", "sem")

# 2+ Emitted Paths rule
#' @rdname sem_rules
#' @keywords internal
rule_sem_two_emitted_paths <- function(partable) {
  rule <- "2+ Emitted Paths Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, "lv")
  if (length(vars$lv) == 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when there are latent variables in the model",
        levels = "not_applicable"
      ),
      cond = NA_character_
    )
    return(out)
  }
  # build output
  free_var <- vapply(vars$lv, function(var) {
    any(
      with(partable, free[lhs == var & rhs == var & op == "~~"]) > 0
    )
  }, FUN.VALUE = logical(1))
  free_var.nox <- sapply(vars$lv, function(var) {
    nox <- c(
      with(partable, rhs[lhs == var & op == "=~"]),
      with(partable, lhs[rhs == var & op == "~"])
    )
    c2 <- vapply(nox, function(var.nox) {
      any(
        with(
          partable,
          free[lhs == var.nox & rhs == var.nox & op == "~~"]) > 0
      )
    }, FUN.VALUE = logical(1))
    return(all(c2))
  })
  two_path.lv <- sapply(vars$lv, function(var) {
    c3 <- sum(
      with(
        partable,
        (lhs == var & op == "=~" & rhs != var) | (rhs == var & op == "~")
      )
    )
    return(c3 >= 2)
  })
  if (all(!free_var | !free_var.nox)) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "There are no variables (with free variance & free downstream disturbance variances) to which to apply the rule",
        levels = "not_applicable"
      ),
      cond = NA_character_
    )
    return(out)
  } else {
    pass <- isTRUE(all(two_path.lv[free_var & free_var.nox]))
    cond <- "N"
  }
  # messages
  msgs <- NA_character_
  if (!pass) {
    msgs <- add_rule_msgs(
      msgs,
      new_msgs = paste(
        "Some variables do not have two emitted paths:",
        paste(vars$lv[free_var & free_var.nox & !two_path.lv], collapse = ", ")
      ),
      levels = "identification_failure"
    )
  }
  if (any(!free_var)) {
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = paste(
        "This rule ignores latent variables without free variance:",
        paste(vars$lv[!free_var], collapse = ", ")
      ),
      levels = "not_applicable"
    )
  }
  if (any(!free_var.nox)) {
    msgs <- c(
      msgs,
      add_rule_msgs(
        new_msgs = paste(
          "This rule ignores latent variables with downstream variables that do not have free disturbance variances:",
          paste(vars$lv[!free_var.nox], collapse = ", ")
        ),
        levels = "not_applicable"
      )
    )
  }
  if (length(msgs) == 0) msgs <- NA_character_
  build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
}
attr(rule_sem_two_emitted_paths, "applies_to") <- c("cfa", "sem")

# Exogenous X rule/MIMIC rules
#' @rdname sem_rules
#' @keywords internal
rule_sem_exogenous_x <- function(partable) {
  rule <- "Exogenous X Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov", "ov.ind"))
  if (length(vars$lv) == 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when there are latent variables in the model",
        levels = "not_applicable"
      ),
      cond = NA_character_
    )
    return(out)
  }
  # build output
  n.ind <- sapply(vars$lv, function(var) {
    sum(
      with(partable, lhs == var & rhs %in% vars$ov.ind)
    )
  }, simplify = TRUE)
  n.exogx <- sapply(vars$lv, function(var) {
    sum(
      with(partable, lhs == var & rhs %in% vars$ov & (op == "~" | op == "<~"))
    )
  }, simplify = TRUE)
  if (all(n.exogx == 0)) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when causal indicators or exogenous observed variables are in the model",
        levels = "not_applicable"
      ),
      cond = NA_character_
    )
    return(out)
  }
  if (length(vars$lv) == 1) {
    pass <- isTRUE(n.ind >= 2)
    cond <- "S"
    if (pass) {
      msgs <- NA_character_
    } else {
      msgs <- add_rule_msgs(
        new_msgs = "The latent variable must have at least two effect indicators",
        levels = "suff_cond_not_satisfied"
      )
    }
  } else {
    # most likely won't implement multiple latent variable case because it requires
    # checking if the structural model among latent variables is identified
    pass <- NA
    cond <- NA_character_
    msgs <- add_rule_msgs(
      new_msgs = "This rule is currently not supported for multiple latent variables",
      levels = "not_applicable"
    )
  }
  build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
}
attr(rule_sem_exogenous_x, "applies_to") <- c("cfa", "sem")
