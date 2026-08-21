#' Evaluate common Structural Equation Model (SEM) identification rules
#'
#' This is the "workhorse" function of the \code{semrulesid} package. The user
#' supplies a model string in \code{lavaan} syntax (see
#' \link[lavaan]{model.syntax} for more details) and the function prints an
#' informative table to the console about the status of the model on a variety
#' of common identification rules.
#'
#' The primary output of \code{id()} is a table printed to the console, where
#' rows correspond to rules, and columns include "Pass" (did the rule pass?),
#' "Necessary" (is the rule necessary for identification?), and "Sufficient" (is
#' the rule sufficient for identification?). These columns can take values
#' "Yes", "No", or be left blank in the case of \code{NA} values.
#'
#' If the user set the \code{print_msgs} argument to "TRUE" (which is the
#' default), a column labeled "Messages" is appended to the table, and an output
#' section called "Messages" is printed below the table. Messages include why a
#' rule failed, why a rule is not applicable to the current model, or why the
#' necessary/sufficient conditions may not apply as usual.
#'
#' Messages are identified by a number, and corresponding message numbers are
#' listed in the "Messages" column of the table.
#'
#' \code{lav_fun} takes character values "lavaan", "sem", or "cfa", specifying
#' which \code{lavaan} function the user intends to call (and thus which
#' defaults should be used) when fitting the model in the case a model string
#' is supplied. Supplying a parameter table or fitted model object ignores the
#' \code{lav_fun} argument since defaults will have already been implemented. In
#' these cases, you can set \code{lav_fun = NA} to avoid warnings about the
#' argument being ignored. For \code{id_mplus()}, \code{lav_fun} is only needed
#' if the user supplies an Mplus model string due to how
#' \code{lavaan::lav_mplus_syntax_model()} works. If an Mplus input file is
#' supplied, \code{lav_fun} is ignored since \code{lavaan::lav_mplus_lavaan()}
#' always uses \code{sem()} defaults.
#'
#' \code{id2} is a wrapper function for calling \code{id} with argument
#' \code{twostep} set to \code{TRUE}. The two-step method parses an SEM into a
#' CFA model and a latent variable/structural model, and evaluates the
#' identification rules on each. If both parts are identified, the whole model
#' is identified.
#'
#' @param x A character string model in \code{lavaan} syntax, a \code{lavaan}
#'        parameter table, or a fitted \code{lavaan} object.
#' @param print_msgs Logical. If \code{TRUE}, the output will include why a rule
#'        does not pass or is not applicable, along with any other helpful
#'        information. Default: \code{TRUE}.
#' @param lav_fun A character string specifying the lavaan function you intend
#'        to use to fit the model. This will ensure the correct model defaults
#'        are specified. Options currently include "lavaan", "sem", or "cfa". If
#'        a parameter table or fit object are supplied, this argument is
#'        ignored. Default: "sem".
#' @param twostep A logical indicating whether to use the two-step
#'        identification rule instead of the usual one-step. See details.
#'        Default: \code{FALSE}.
#' @param ... Additional arguments passed to the \code{lavaanify} function from
#'        \code{lavaan}. See \link[lavaan]{lavaanify} for more information. If
#'        parameter tables or fitted model objects are supplied, these arguments
#'        are ignored.
#'
#' @return An object of class \code{semid} or \code{semid2} (if \code{twostep =
#'         TRUE} or \code{id2} is called. See details.)
#'
#' @examples
#' my_model <- ' L1 =~ x1 + x2 + x3
#'               L2 =~ x4 + x5 + x6
#'               L3 =~ x7 + x8 + x9
#'               L2 ~ L1
#'               L3 ~ L2 '
#' id(my_model, print_msgs = TRUE, lav_fun = "sem",
#'    meanstructure = FALSE)
#' id2(my_model, print_msgs = TRUE, lav_fun = "sem",
#'    meanstructure = FALSE)
#' @name id
#' @export
id <- function(x, print_msgs = TRUE, lav_fun = "sem", twostep = FALSE, ...) {
  lav_fun <- validate_lav_fun_arg(lav_fun)
  if (!is.logical(print_msgs) || length(print_msgs) != 1) {
    id_stop(gettext("print_msgs= must be a logical"))
  }
  if (!is.logical(twostep) || length(twostep) != 1) {
    id_stop(gettext("twostep= must be a logical"))
  }
  if (!is.na(lav_fun) && is_lavaan_partable(x)) {
    id_warn(
      gettext("lav_fun= is ignored when a parameter table is supplied.")
    )
  }
  UseMethod("id")
}

#' @export
id.semscale <- function(x, print_msgs = TRUE, lav_fun = "sem",
                        twostep = FALSE, ...) {
  out <- id.data.frame(
    x = x$partable,
    print_msgs = print_msgs,
    lav_fun = lav_fun,
    twostep = twostep,
    ...
  )
  out$scaling <- x
  out
}

#' @export
id.lavaan <- function(x, print_msgs = TRUE, lav_fun = "sem",
                      twostep = FALSE, ...) {
  out <- lavaan_obj_to_partable(x, lav_fun = lav_fun, ...)
  id.data.frame(
    x = out$partable,
    print_msgs = print_msgs,
    lav_fun = out$lav_fun,
    twostep = twostep,
    id_model_type = out$id_model_type
  )
}

#' @export
id.character <- function(x, print_msgs = TRUE, lav_fun = "sem",
                         twostep = FALSE, ...) {
  out <- lavaan_syntax_to_partable(x, lav_fun = lav_fun, ...)
  id.data.frame(
    x = out$partable,
    print_msgs = print_msgs,
    lav_fun = out$lav_fun,
    twostep = twostep,
    id_model_type = out$id_model_type
  )
}

#' @export
id.data.frame <- function(x, print_msgs = TRUE, lav_fun = "sem",
                          twostep = FALSE, id_model_type = NULL, ...) {
  if (is_lavaan_partable(x)) {
    partable <- x
  } else {
    id_stop(gettext("Unknown input type."))
  }

  # classify model
  if (is.null(id_model_type)) {
    id_model_type <- classify_model(partable)
  }

  if (twostep) {

    if (id_model_type != "sem") {
      id_stop(
        gettext("The two-step identification rule is only applicable to full SEMs.")
      )
    }

    vars <- get_partable_vars(partable, c("ov.cind"))
    if (length(vars$ov.cind) > 0) {
      id_stop(
        gettext("The two-step identification rule is currently not supported for models with causal indicators.")
      )
    }

    partable_cfa <- sem_to_cfa(partable)
    partable_reg <- sem_to_reg(partable)

    out <- list(
      id_model_type = id_model_type,
      id_cfa = id.data.frame(
        x = partable_cfa,
        print_msgs = print_msgs,
        lav_fun = lav_fun,
        twostep = FALSE,
        id_model_type = "cfa"
      ),
      id_reg = id.data.frame(
        x = partable_reg,
        print_msgs = print_msgs,
        lav_fun = lav_fun,
        twostep = FALSE,
        id_model_type = "reg"
      ),
      partable = partable,
      lav_fun = lav_fun,
      print_options = list(
        print_msgs = print_msgs,
        applicable_rules_policy = "hide"
      )
    )

    # alternate class for two-step id
    class(out) <- "semid2"

    return(out)

  }

  # evaluate rules
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
    id_model_type = id_model_type,
    Rules = rules,
    partable = partable,
    lav_fun = lav_fun,
    scaling = NULL,
    print_options = list(
      print_msgs = print_msgs,
      applicable_rules_policy = "footnote"
    )
  )

  class(out) <- "semid"
  out

}

#' @export
id.default <- function(x, print_msgs = TRUE, lav_fun = "sem", ...) {
  id_stop(gettext("Unknown input type."))
}

#' @rdname id
#' @export
id2 <- function(x, print_msgs = TRUE, lav_fun = "sem", ...) {
  id(x, print_msgs = print_msgs, lav_fun = lav_fun, twostep = TRUE, ...)
}

#' Evaluate identification rules for an Mplus model
#'
#' This is a wrapper function for \code{\link{id}} that handles Mplus model
#' syntax (as a string) or Mplus input files (with extension ".inp"). It
#' internally converts the Mplus model to a lavaan model, then calls
#' \code{\link{id}} using the specified arguments.
#'
#' @param x A character string model in Mplus syntax, or a path to an Mplus
#' input file.
#' @inheritParams id
#'
#' @return An object of class \code{semid} or \code{semid2} (if \code{twostep =
#' TRUE}).
#'
#' @examples
#' my_model <- ' L1 BY x1 x2 x3;
#'               L2 BY x4 x5 x6;
#'               L3 BY x7 x8 x9;
#'               L2 ON L1;
#'               L3 ON L2; '
#' @examples
#' id_mplus(my_model, print_msgs = TRUE, lav_fun = "sem",
#'    meanstructure = FALSE)
#'
#' @export
id_mplus <- function(x, print_msgs = TRUE, lav_fun = "sem",
                     twostep = FALSE, ...) {
  if (length(x) == 1 && is.character(x) && grepl("\\.inp$", x, ignore.case = TRUE)) {
    if (length(lav_fun) == 1 && (is.na(lav_fun) || lav_fun != "sem")) {
      id_warn(gettextf("Ignoring lav_fun='%s';lavaan::lav_mplus_lavaan() always uses sem() defaults.", lav_fun))
    }
    lav <- lavaan::lav_mplus_lavaan(x)
    id.lavaan(
      x = lav,
      print_msgs = print_msgs,
      lav_fun = "sem",
      twostep = twostep,
      ...
    )
  } else {
    lav <- lavaan::lav_mplus_syntax_model(x)
    id.character(
      x = lav,
      print_msgs = print_msgs,
      lav_fun = lav_fun,
      twostep = twostep,
      ...
    )
  }
}