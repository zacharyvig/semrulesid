#' Rules for confirmatory factor analysis models
#' 
#' Confirmatory Factor Analysis (CFA) models are models with latent variables
#' but no structural paths between latent variables. CFA rules assume each 
#' latent variable is correctly scaled (see \link[semidentify]{scaling}).
#' 
#' \describe{
#'  \item{Two Indicator Rule}{In a model with more than one latent variable, each
#'  latent variable can have just two indicators if each indicator loads on exactly
#'  one variable, none of the errors of the indicators are correlated, and each
#'  latent variable correlates with at least one other latent variable. Sufficient
#'  but not necessary.}
#'  \item{Three Indicator Rule}{In a model with one or more latent variables, each
#'  latent variable can have just three indicators if each indicator loads on exactly
#'  one variable and none of the errors of the indicators are correlated. Sufficient
#'  but not necessary.}
#' }
#' 
#' @name cfa_rules
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Kenny, D. A. (1979). Correlation and causality.
#' 
#' @keywords internal
NULL


# Three indicator rules
#' @rdname cfa_rules
#' @keywords internal
rule_cfa_three_indicator <- function(partable) {
  rule <- "Three Indicator Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov.ind", "eqs.y"))
  if (length(vars$eqs.y) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies to confirmatory factor analysis models",
        levels = "1"
      ),
      cond = NA_character_
    )
    return(out)
  }
  # tally indicators per lv
  nov.ind <- sapply(vars$lv, function(var){
    out <- with(partable, rhs[lhs == var & rhs %in% vars$ov.ind])
    return(length(out))
  }, simplify = TRUE)
  # check for higher order cfa
  nlv.ind <- sapply(vars$lv, function(var){
    out <- with(partable, rhs[lhs == var & op == "=~" & rhs %in% vars$lv])
    return(length(out))
  }, simplify = TRUE)
  # build output
  if(any(nov.ind[nlv.ind == 0] < 3)) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = paste("This rule only applies when all latent variables have more than three indicators:",
                   paste(vars$lv[nov.ind < 3 & nlv.ind == 0], collapse = ", ")),
        levels = "1"
      ),
      cond = NA_character_
    )
    return(out)
  }
  # first order factors
  idx.fof <- which(nlv.ind == 0)
  ind.fof <- unique(with(partable, rhs[lhs %in% vars$lv[idx.fof] & op == "=~"]))
  # factor complexity 1
  fc1.fof <- sapply(ind.fof, function(ind) {
    length(
      with(partable, lhs[rhs == ind & op == "=~"])
    ) == 1
  }, simplify = TRUE)
  # uncorrelated errors
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.ind <- sapply(ind.fof, function(ind) {
    with(covs, any(lhs == ind & rhs %in% ind.fof | rhs == ind & lhs %in% ind.fof))
  }, simplify = TRUE)
  # messages
  msgs <- NA_character_
  if (any(nlv.ind > 0)) {
    cond <- NA_character_
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = "Cannot establish sufficiency when higher order factors are present; pass/fail applies to first order factors only",
      levels = "1"
    )
  } else {
    cond <- "S"
  }
  pass <- isTRUE(all(fc1.fof) && all(!cor_err.ind))
  if (any(!fc1.fof)) {
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = paste("Some indicators have a factor complexity greater than one:",
                paste(ind.fof[!fc1.fof], collapse = ", ")),
      levels = "2"
    )
  }
  if (any(cor_err.ind)) {
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = paste("Some indicators have correlated errors:",
              paste(ind.fof[cor_err.ind], collapse = ", ")),
      levels = "2"
    )
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
  return(out)
}


# Two indicator rules
#' @rdname cfa_rules
#' @keywords internal
rule_cfa_two_indicator <- function(partable) {
  rule <- "Two Indicator Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov.ind", "eqs.y"))
  if (length(vars$eqs.y) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies to confirmatory factor analysis models",
        levels = "1"
      ),
      cond = NA_character_
    )
    return(out)
  }
  if (length(vars$lv) < 2) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = "This rule only applies when more than one latent variable is in the model",
        levels = "1"
      ),
      cond = NA_character_
    )
    return(out)
  }
  # tally indicators per lv
  nov.ind <- sapply(vars$lv, function(var){
    out <- with(partable, rhs[lhs == var & rhs %in% vars$ov.ind])
    return(length(out))
  }, simplify = TRUE)
  # check for higher order cfa
  nlv.ind <- sapply(vars$lv, function(var){
    out <- with(partable, rhs[lhs == var & op == "=~" & rhs %in% vars$lv])
    return(length(out))
  }, simplify = TRUE)
  # build output
  if(any(nov.ind[nlv.ind == 0] < 2)) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = add_rule_msgs(
        new_msgs = paste("This rule only applies when all latent variables have more than two indicators:",
                   paste(vars$lv[nov.ind < 2 & nlv.ind == 0], collapse = ", ")),
        levels = "1"
      ),
      cond = NA_character_
    )
    return(out)
  }
  # first order factors
  idx.fof <- which(nlv.ind == 0)
  ind.fof <- unique(with(partable, rhs[lhs %in% vars$lv[idx.fof] & op == "=~"]))
  # factor complexity 1
  fc1.fof <- sapply(ind.fof, function(ind) {
    length(with(partable, lhs[rhs == ind & op == "=~"])) == 1
  }, simplify = TRUE)
  # uncorrelated errors
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.ind <- sapply(ind.fof, function(ind) {
    with(covs, any(lhs == ind & rhs %in% ind.fof | rhs == ind & lhs %in% ind.fof))
  }, simplify = TRUE)
  # lv variances/correlations -- each lv is correlated with at least one other lv
  cor.lv <- sapply(vars$lv[idx.fof], function(var) {
    with(covs, any(lhs == var & rhs %in% vars$lv | rhs == var & lhs %in% vars$lv))
  }, simplify = TRUE)
  # messages
  msgs <- NA_character_
  if (any(nlv.ind > 0)) {
    cond <- NA_character_
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = "Cannot establish sufficiency when higher order factors are present; pass/fail applies to first order factors only",
      levels = "1"
    )
  } else {
    cond <- "S"
  }
  pass <- isTRUE(all(fc1.fof) && all(!cor_err.ind) && all(cor.lv))
  if (any(!fc1.fof)) {
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = paste("Some indicators have a factor complexity greater than one:",
                          paste(ind.fof[!fc1.fof], collapse = ", ")),
      levels = "2"
    )
  }
  if (any(cor_err.ind)) {
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = paste("Some indicators have correlated errors:",
                          paste(ind.fof[cor_err.ind], collapse = ", ")),
      levels = "2"
    )
  }
  if (any(!cor.lv)) {
    msgs <- add_rule_msgs(
      msgs = msgs,
      new_msgs = paste("Some latent variables are not correlated with another latent variable:",
              paste(vars$lv[idx.fof & !cor.lv] , collapse = ", ")),
      levels = "2"
    )
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
  return(out)
}
