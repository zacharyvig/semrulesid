#' Check recursion using Depth-First Search algorithm
#' @param partable A lavaan parameter table
#' @param start A character vector of variable names from which to start the
#'              algorithm.
#' @return A logical value indicating whether the model is recursive (TRUE) or
#' not (FALSE).
#' @keywords internal
check_recursion <- function(partable, start) {
  if (!is.character(start)) {
    id_stop(gettext("start= must be a character vector. This is an internal error. Please report this issue to the package maintainer."))
  }

  # regressions
  regs <- subset(partable, op == "~" & free > 0)
  ds_full <- split(regs$lhs, regs$rhs)

  loop <- function(start, tally) {
    tally <- c(tally, start)
    ds <- ds_full[[start]]
    if (length(ds)) {
      for (var in ds) {
        if (var %in% tally) {
          return(FALSE)
        } else {
          out <- loop(var, tally)
          if (isFALSE(out)) {
            return(FALSE)
          }
        }
      }
    }
    return(TRUE)
  }

  for (var in start) {
    out <- loop(var, tally = c())
    if (isFALSE(out)) {
      return(FALSE)
    }
  }

  return(TRUE)
  
}

#' Convert a SEM into a Confirmatory Factor Analysis for the two-step rule
#' @param partable A lavaan parameter table
#' @return A lavaan parameter table that has been converted to a CFA model.
#' @noRd
sem_to_cfa <- function(partable) {
  vars <- get_partable_vars(partable, c("lv"))
  lv.regs <- with(partable, op == "~" & lhs %in% vars$lv & rhs %in% vars$lv)
  # replace directional arrows with double-sided ones
  partable$op[lv.regs] <- "~~"
  type <- classify_model(partable)
  if (type != "cfa") {
    id_stop(gettext("sem_to_cfa() failed. This is an internal error. Please report this issue to the package maintainer."))
  }
  partable
}

#' Convert a SEM into a Simultaneous Equations Model for the two-step rule
#' @param partable A lavaan parameter table
#' @return A lavaan parameter table that has been converted to a simultaneous
#' equations model.
#' @noRd
sem_to_reg <- function(partable) {
  vars <- get_partable_vars(partable, c("lv"))
  lv.paths <- with(partable, lhs %in% vars$lv & rhs %in% vars$lv)
  partable <- partable[lv.paths, , drop = FALSE]
  # handle higher order factors
  hof <- which(partable$op == "=~")
  # switch lhs and rhs
  lhs <- partable$rhs[hof]
  rhs <- partable$lhs[hof]
  partable$lhs[hof] <- lhs
  partable$rhs[hof] <- rhs
  # then change the operator to "~"
  partable$op[hof] <- "~"
  # handle causal indicators (for future use)
  # partable$op[partable$op == "<~"] <- "~"
  type <- classify_model(partable)
  if (type != "reg") {
    id_stop(gettext("sem_to_reg() failed. This is an internal error. Please report this issue to the package maintainer."))
  }
  partable
}

#' Gather identification rules as a list
#'
#' The `semidentify` package stores identification rule functions internally.
#' This function makes them available to the user as a list.
#'
#' @param rule A character vector specifying the name of the rule as it's
#'        defined in the package. Use "*" to get all rules (or all rules of the
#'        defined model type). Partial matches are acceptable.
#' @param model_type A character vector specifying the model sub-type from which
#'        to get rules. Sub-types include "reg" (simultaneous equations
#'        models/regression models) and "cfa" (confirmatory factor analysis
#'        models). Use "sem" to get rules that apply to all structural equation
#'        models. Use "*" to get all rules in the package.
#'
#' @return A list object with the rule function(s) specified by the user.
#'
#' @export
#'
#' @examples
#' # Get all rules for a CFA model
#' rules <- get_rules(rule = "*", model_type = "cfa")
#' # Get a specific rule for a SEM model
#' rules <- get_rules(rule = "latent_scaling", model_type = "sem")
#' latent_scaling_rule <- rules[[1]]
#'
get_rules <- function(rule = "*", model_type = "*") {
  if (!all(is.character(rule))) {
    id_stop(gettext("rule= must be a character vector"))
  }
  if (!all(is.character(model_type))) {
    id_stop(gettext("model_type= must be a character vector"))
  }
  if ("*" %in% model_type) model_type <- "all"
  model_type <- unique(model_type)
  if (!all(model_type %in% c("all", "reg", "cfa", "sem"))) {
    id_stop(gettext("model_type= must be one of 'all', 'reg', 'cfa', or 'sem'"))
  }
  pull_fns <- function(fns) {
    mget(fns, envir = asNamespace("semidentify"), mode = "function")
  }
  rules <- get_rule_names(model_type)
  if (any(rule %in% c("*", "all"))) {
    return(pull_fns(rules))
  } else {
    rule <- grep(rule, rules, value = TRUE)
    if (length(rule) == 0) {
      id_stop(gettext("Specified rule does not exist"))
    }
    if (length(rule) > 1) {
      id_warn(gettext("Multiple rules matched the specified rule. Returning all matches."))
    }
    return(pull_fns(rule))
  }
}

# internal function for extracting rule function names
#' @noRd
get_rule_names <- function(model_type = c("all", "reg", "cfa", "sem")) {
  model_type <- match.arg(model_type, several.ok = TRUE)
  prefix <- if ("all" %in% model_type) {
    "^rule_"
  } else {
    sprintf("^rule_%s", model_type)
  }
  ns <- asNamespace("semidentify")
  objs <- ls(envir = ns, all.names = TRUE)
  out <- sapply(prefix, function(p) {
    grep(p, objs, value = TRUE)
  })
  unname(c(out, recursive = TRUE))
}

# internal function for building rule output lists
#' @noRd
build_rule_out <- function(rule, pass, msgs = NA_character_,
                           cond = c("N", "S", "NS", NA_character_)) {
  cond <- match.arg(cond)
  msgs <- if (isTRUE(pass) || any(!is.na(msgs))) msgs else NA_character_
  list(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
}

# internal function to get certain variables from a lavaan parameter table
#' @noRd
get_partable_vars <- function(partable, vars, var_names = NA) {
  lavpta <- lavaan::lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  if (lavpta$nblocks > 1) {
    id_stop(gettext("This function currently only supports single-block models."))
  }
  # tally variables assuming one block
  out <- lapply(vars, function(var) {
    vnames[[var]][[1]]
  })
  if (!all(is.na(var_names))) {
    names(out) <- var_names
  } else {
    names(out) <- vars
  }
  out
}

# internal function for adding messages to rule output
#' @noRd
add_rule_msgs <- function(msgs = NA_character_, new_msgs, levels = NULL) {
  if (length(msgs) == 1L && is.na(msgs)) {
    msgs <- character(0)
  }
  if (is.null(levels)) {
    levels <- rep(NA_character_, length(new_msgs))
  }
  stopifnot(length(new_msgs) == length(levels))
  level_labels <- c(
    "1" = "Info",
    "2" = "Reason",
    "3" = "WARNING"
  )
  new_msgs <- ifelse(
    is.na(levels),
    new_msgs,
    sprintf("[%s] %s", level_labels[as.character(levels)], new_msgs)
  )
  out <- c(msgs[!is.na(msgs)], new_msgs)
  out
}

# More advanced version of lavaan::lav_partable_npar() that
# accounts for equality constraints
#' @noRd
get_ntheta <- function(partable) {
  # select rows with free parameters
  free_rows <- partable$free > 0
  # extract user-supplied labels (matching ones indicate equality constraints)
  keys <- partable$label[free_rows]
  # select internal labels for remaining parameters
  na_key_plabels <- partable$plabel[free_rows][is.na(keys) | keys == ""]
  # fill in missing keys with internal labels
  keys[is.na(keys) | keys == ""] <- na_key_plabels
  # count unique keys only--user supplied equalities are not double-counted
  length(unique(keys))
}

# wrapper for lavaan::lav_partable_ndat() in case it ever needs to be modified
#' @noRd
get_ndata <- function(partable) {
  lavaan::lav_partable_ndat(partable)
}

# internal function for ordering rules in id() output
#' @noRd
order_rules <- function(rule_names) {
  # preferred order is to start with ntheta, scaling, and all sem rules
  ntheta_rule <- grep("ntheta", rule_names, value = TRUE)
  scaling_rule <- grep("scaling", rule_names, value = TRUE)
  sem_rules <- grep("_sem", rule_names, value = TRUE)
  rule_names_ord <- c(
    ntheta_rule,
    scaling_rule,
    setdiff(sem_rules, c(ntheta_rule, scaling_rule)),
    setdiff(rule_names, sem_rules)
  )
  rule_names_ord
}

# internal function for checking if an object is a lavaan parameter table
#' @noRd
is_lavaan_partable <- function(x) {
   is.list(x) && !is.null(x$lhs) && is.null(x$mod.idx)
}