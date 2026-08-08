#' Evaluate common Structural Equation Model (SEM) identification rules
#'
#' This is the "workhorse" function of the \code{semidentify} package. The user supplies
#' a model string in \code{lavaan} syntax (see \link[lavaan]{model.syntax} for more
#' details) and the function prints an informative table to the console about the
#' status of the model on a variety of common identification rules.
#'
#' The primary output of \code{id()} is a table printed to the console, where rows
#' correspond to rules, and columns include "Pass" (did the rule pass?), "Necessary"
#' (is the rule necessary for identification?), and "Sufficient" (is the rule
#' sufficient for identification?). These columns can take values "Yes", "No",
#' or be left blank in the case of NA values.
#'
#' If the user set the \code{include.msgs} argument to "TRUE" (which is the default),
#' a column labeled "Messages" is appended to the table, and an output section called
#' "Messages" is printed below the table. Messages include why a rule failed, why
#' a rule is not applicable to the current model, or why the necessary/sufficient
#' conditions may not apply as usual.
#'
#' Messages are identified by a number, and corresponding message numbers are listed
#' in the "Messages" column of the table.
#'
#' \code{lav_fun} takes character values "lavaan", "sem", or "cfa", specifying which
#' \code{lavaan} function the user intends to call (and thus which defaults should)
#' be used) when fitting the model in the case a model string is supplied. Supplying
#' a parameter table or fitted model object ignores the \code{lav_fun} argument since
#' defaults will have already been implemented.
#' 
#' \code{id2} is a wrapper function for calling \code{id} with argument \code{twostep}
#' set to \code{TRUE}. The two-step method parses an SEM into a CFA model and a latent
#' variable/structural model, and evaluates the identification rules on each. If both
#' parts are identified, the whole model is identified.
#'
#' @param x A character string model in \code{lavaan} syntax, a
#'        \code{lavaan} parameter table, or a fitted \code{lavaan} object.
#' @param include.msgs Logical. If \code{TRUE}, the output will include why a rule 
#'        does not pass or is not applicable, along with any other helpful information.
#'        Default: \code{TRUE}.
#' @param lav_fun A character string specifying the lavaan function you intend to use to fit
#'        the model. This will ensure the correct model defaults are specified. Options
#'        currently include "lavaan", "sem", or "cfa". If a parameter table for fit
#'        object are supplied, this argument is ignored. Default: "sem".
#' @param twostep A logical indicating whether to use the two-step identification rule
#'        instead of the usual one-step. See details. Default: \code{FALSE}.
#' @param ... Additional arguments passed to the \code{lavaanify} function from
#'        \code{lavaan}. See \link[lavaan]{lavaanify} for more information. If parameter
#'        tables or fitted model objects are supplied, these arguments are ignored.
#'
#' @return An object of class \code{semid} or \code{semid2} (if \code{twostep = TRUE}
#'         or \code{id2} is called. See details.)
#'
#' @examples
#' my_model <- ' L1 =~ x1 + x2 + x3
#'               L2 =~ x4 + x5 + x6
#'               L3 =~ x7 + x8 + x9
#'               L2 ~ L1
 #'              L3 ~ L2 '
#' id(my_model, include.msgs = TRUE, lav_fun = "sem", 
#'    meanstructure = FALSE)
#' id2(my_model, include.msgs = TRUE, lav_fun = "sem",
#'    meanstructure = FALSE)
#' @name id
#' @export
id <- function(x, include.msgs = TRUE, lav_fun = "sem", twostep = FALSE, ...) {
  stopifnot(
    "Argument `include.msgs` must be a logical" =
      is.logical(include.msgs)
  )
  stopifnot(
    "Argument `lav_fun` must be a character string" =
      is.character(lav_fun)
  )
  stopifnot(
    "Unknown `lav_fun` or `lav_fun` currently not supported" =
      lav_fun %in% c("lavaan", "sem", "cfa")
  )
  stopifnot(
    "Argument `twostep` must be a logical" =
      is.logical(twostep)
  )
  UseMethod("id")
}

#' @export
id.semscale <- function(x, include.msgs = TRUE, lav_fun = "sem", twostep = FALSE, ...) {
  print(x)
  return(id.data.frame(x$partable, include.msgs = include.msgs, lav_fun = lav_fun, twostep = twostep, ...))
}

#' @export
id.lavaan <- function(x, include.msgs = TRUE, lav_fun = "sem", twostep = FALSE, ...) {
  dotdotdot <- list(...)
  if (length(dotdotdot) > 0) {
    warning("Additional arguments are ignored when a fitted lavaan object is supplied")
  }
  lav_fun.orig <- get_lavaan_cmd(x)
  if (lav_fun.orig != lav_fun) {
    warning(
      paste0("The fitted lavaan object was created with ", format_lavaan_fun(lav_fun.orig),
             ", but you specified `lav_fun = '", lav_fun,
             "'`. This may lead to unexpected results.")
    )
  }
  partable <- as.data.frame(
    x@ParTable,
    stringsAsFactors = FALSE
  )
  return(id.data.frame(partable, include.msgs = include.msgs, lav_fun = lav_fun, twostep = twostep, ...))
}

#' @export
id.character <- function(x, include.msgs = TRUE, lav_fun = "sem", twostep = FALSE, ...) {
  dotdotdot <- list(...)
  if (grepl("\\.inp$", x, ignore.case = TRUE)) {
    stop("This looks like an Mplus input file. Did you mean to use `id_mplus()` instead?")
  }
  if (isTRUE(dotdotdot$model.type == "efa")) {
    dotdotdot[["model.type"]] <- NULL
    warning("Only `model.type = 'sem'` is currently supported")
  }
  if (isTRUE(dotdotdot$debug)) {
    dotdotdot[["debug"]] <- NULL
    warning("Ignoring `debug`")
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
  return(id.data.frame(partable, include.msgs = include.msgs, lav_fun = lav_fun, twostep = twostep, ...))
}

#' @export
id.data.frame <- function(x, include.msgs = TRUE, lav_fun = "sem", twostep = FALSE, ...) {
  if (is.list(x) && !is.null(x$lhs) && is.null(x$mod.idx)) {
    partable <- x
  } else {
    stop("Unknown list format. Please supply a lavaan parameter table or fitted model object.")
  }

  # STEP 1 - Classify model
  model.type <- classify_model(partable) # errors are handled in this function
  if (lav_fun == "cfa" && model.type != "cfa") {
      warning("`sem()` or `lavaan()` may be more appropriate functions for this type of model")
  }

  if (twostep) {

    if (model.type != "sem") {
      stop("The two-step identification rule is only applicable to full SEMs.")
    }

    partable.cfa <- sem_to_cfa(partable)
    partable.reg <- sem_to_reg(partable)

    out <- list(
      id.cfa = id(partable.cfa),
      id.reg = id(partable.reg),
      partable = partable,
      lav_fun = lav_fun,
      print.options = list(
        include.msgs = include.msgs
      )
    )

    # alternate class for two-step id
    class(out) <- "semid2"

    return(out)

  }

  # STEP 2 - Evaluate rules
  rule_names <- get_rule_names("all")
  rule_names_ord <- order_rules(rule_names)
  # apply rules to partable
  rules <- c(
    lapply(
      rule_names_ord,
      function(rule) do.call(rule, list(partable))
    )
  )

  out <- list( 
    model.type = model.type,
    Rules = rules,
    partable = partable,
    lav_fun = lav_fun,
    print.options = list(
      include.msgs = include.msgs
    )
  )

  class(out) <- "semid"

  return(out)

}

#' @export
id.default <- function(x, include.msgs = TRUE, lav_fun = "sem", ...) {
  stop("Unknown model format. Please supply a model string, lavaan parameter table, or fitted model object.")
}

#' @rdname id
#' @export
id2 <- function(x, include.msgs = TRUE, lav_fun = "sem", ...) {
  id(x, include.msgs = include.msgs, lav_fun = lav_fun, twostep = TRUE, ...)
}

#' Evaluate identification rules for an Mplus model
#'
#' This is a wrapper function for \code{\link{id}} that accomodates Mplus model syntax
#' (as a string) or Mplus input files (with extension ".inp"). It internally converts
#' the Mplus model to a lavaan model, then calls \code{\link{id}} using the specified
#' arguments.
#' 
#' @param x A character string model in Mplus syntax, or a path to an Mplus input file.
#' @inheritParams id
#' 
#' @return An object of class \code{semid} or \code{semid2} (if \code{twostep = TRUE}).
#' 
#' @examples
#' my_model <- ' L1 BY x1 x2 x3;
#'               L2 BY x4 x5 x6;
#'               L3 BY x7 x8 x9;
#'               L2 ON L1;
#'               L3 ON L2; '
#' @examples
#' id_mplus(my_model, include.msgs = TRUE, lav_fun = "sem", 
#'    meanstructure = FALSE)
#' 
#' @export
id_mplus <- function(x, include.msgs = TRUE, lav_fun = "sem", twostep = FALSE, ...) {
  if (grepl("\\.inp$", x, ignore.case = TRUE)) {
    lav <- lavaan::lav_mplus_lavaan(x)
  } else {
    lav <- lavaan::lav_mplus_syntax_model(x)
  }
  id(lav, include.msgs = include.msgs, lav_fun = lav_fun, twostep = twostep, ...)
}