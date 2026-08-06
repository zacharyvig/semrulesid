#' Check if latent variables are correctly scaled
#'
#' This function checks whether latent variables in the model are scaled,
#' which is a necessary condition for model identification. A latent variable is
#' scaled if it has a fixed loading on a scaling indicator or the latent variable
#' has a fixed variance. When mean structure is present, the scaling indicator must
#' also have a fixed intercept or the latent variable must also have a fixed mean.
#' If any latent variable is not scaled, the model is not identified.
#'
#' @param x \code{lavaan} model syntax, a \code{lavaan} parameter table or a
#'        \code{semidentify} ID object.
#' @param call A character string specifying the call you intend to use to fit
#'        the model. This will ensure the correct model defaults are specified. Options
#'        currently include "lavaan", "sem", or "cfa". If a parameter table or fitted model
#'        object are supplied, this argument is ignored.
#' @param include.msgs Logical. If \code{TRUE} the output will print why a 
#'        latent variable is not scaled when applicable and the method or methods
#'        by which it is scaled when it is scaled. If \code{FALSE}, the output will
#'        only print whether the latent variable is scaled or not and a few other
#'        descriptives.
#' @param lv An optional character vector of the specific latent variables you
#'        would like to check. Otherwise, all latent variables are extracted from the
#'        parameter table
#' @param return.type A character string specifying the type of output. Options
#'        include "logical" (a logical vector specifying whether each latent variable is
#'        correctly scaled) or "object" (an object with additional information about the
#'        scaling of each latent variable). The latter is of type \code{semscale} and
#'        has a custom print method for additional diagnosis of identification issues.
#' @param ... Additional arguments passed to the \code{lavaanify} function from
#'        \code{lavaan}. See \link[lavaan]{lavaanify} for more information. These are
#'        only used when a model string is supplied. Otherwise, they are ignored.
#'
#' @return An object of class \code{semscale} with all scaling information
#' (if \code{return.type = "object"}), or a logical vector (if \code{return.type = "logical"})
#' indicating whether each latent variable is correctly scaled.
#'
#' @export
#' @name scaling
#' @examples
#' my_model <- ' L1 =~ x1 + x2 + x3
#'               L2 =~ x4 + x5 + x6
#'               L3 =~ x7 + x8 + x9
#'               L2 ~ L1
#'               L3 ~ L2 '
#' scaling(my_model, include.msgs = TRUE, call = "sem",
#'         meanstructure = FALSE)
#'
scaling <- function(x, call = "sem", include.msgs = TRUE, lv = NULL, 
                    return.type = c("object", "logical"), ...) {                  
  stopifnot(
    "Argument `lv` must be a character vector or NULL" =
      is.character(lv) || is.null(lv)
  )
  stopifnot(
    "Argument `include.msgs` must be a logical" =
      is.logical(include.msgs)
  )
  stopifnot(
    "Unknown `call` or `call` currently not supported" =
      call %in% c("lavaan", "sem", "cfa")
  )
  UseMethod("scaling")
}

#' @export
scaling.semid <- function(x, call = "sem", include.msgs = TRUE, lv = NULL, 
                          return.type = c("object", "logical"), ...) {
  print.semid(x)
  return(scaling.data.frame(x$partable, call = call, include.msgs = include.msgs, lv = lv, return.type = return.type))
}

#' @rdname scaling
#' @export
scaling.lavaan <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                           return.type = c("object", "logical"), ...) {
  dotdotdot <- list(...)
  if (length(dotdotdot) > 0) {
    warning("Additional arguments are ignored when a fitted lavaan object is supplied")
  }
  call.orig <- get_lavaan_call(x)
  if (call.orig != call) {
    warning(
      paste0("The fitted lavaan object was created with ", format_call(call.orig),
             ", but you specified `call = '", call,
             "'`. This may lead to unexpected results.")
    )
  }
  partable <- as.data.frame(
    x@ParTable,
    stringsAsFactors = FALSE
  )
  return(scaling.data.frame(partable, call = call, include.msgs = include.msgs, lv = lv, return.type = return.type))
}

#' @export
scaling.character <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                              return.type = c("object", "logical"), ...) {
  dotdotdot <- list(...)
  if (isTRUE(dotdotdot$model.type == "efa")) {
    dotdotdot[["model.type"]] <- NULL
    warning("Only `model.type='sem'` is currently supported")
  }
  if (isTRUE(dotdotdot$debug)) {
    dotdotdot[["debug"]] <- NULL
    warning("Ignoring `debug`")
  }
  if (is.null(dotdotdot$auto)) {
    dotdotdot$auto <- (call != "lavaan")
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
  return(scaling.data.frame(partable, call = call, include.msgs = include.msgs, lv = lv, return.type = return.type))
}

#' @export
scaling.data.frame <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                               return.type = c("object", "logical"), ...) {
  return.type <- match.arg(return.type) 
  if (!(is.list(x) && !is.null(x$lhs) && is.null(x$mod.idx))) {
    stop("Unknown list format. Please supply a lavaan parameter table or fitted model object.")
  }
  
  if (is.null(lv)) {
    # retrieve attributes and variable names
    vars <- get_partable_vars(x, "lv")
    lv <- vars$lv
  }
  if (length(lv) == 0) {
    stop("Scaling only applies to models with latent variables")
  }

  if (return.type == "object") {
    scaling.table <- list(
      lv = NA_character_,
      scaled = NA,
      n.indicators = NA_integer_,
      scaling.indicator = NA_character_,
      mean.structure = NA,
      fail.reason = NA_character_,
      scaling.method = NA_character_
    )
    scaling.tables <- replicate(length(lv), scaling.table, simplify = FALSE)
  } else {
    out <- vector(length = length(lv))
    names(out) <- lv
  }

  for (i in seq_along(lv)) {
    var <- lv[i]
    if (return.type == "object") {
      scaling.tables[[i]]$lv <- var
    }

    mean.structure <- any(with(x, lhs == var & op == "~1"))

    scale.ind.idx <- with(
      x,
      lhs == var &
        op == "=~" &
        free == 0 &
        rhs != var &
        !is.na(ustart) &
        ustart != 0
    )
    has.scale.ind <- any(scale.ind.idx)
    scale.ind <- if (has.scale.ind) with(x, rhs[scale.ind.idx]) else character(0)

    scale.ind.intercept.fixed <- if (has.scale.ind && mean.structure) {
      any(with(x, lhs %in% scale.ind & op == "~1" & free == 0))
    } else {
      TRUE
    }
    latent.var.fixed <- any(
      with(
        x,
        lhs == var &
          rhs == var &
          op == "~~" &
          free == 0 &
          !is.na(ustart) &
          ustart > 0
      )
    )
    latent.mean.fixed <- if (mean.structure) {
      any(with(x, lhs == var & op == "~1" & free == 0))
    } else {
      TRUE
    }

    units.assigned <- has.scale.ind || latent.var.fixed
    origin.assigned <- if (mean.structure) latent.mean.fixed || (has.scale.ind && scale.ind.intercept.fixed) else TRUE
    scaled <- units.assigned && origin.assigned
    if (return.type == "logical") {
      out[var] <- scaled
      next
    }

    scaling.tables[[i]]$mean.structure <- mean.structure
    scaling.tables[[i]]$scaled <- scaled
    scaling.tables[[i]]$n.indicators <- sum(with(x, lhs == var & op == "=~" & rhs != var))
    if (has.scale.ind) {
      scaling.tables[[i]]$scaling.indicator <- scale.ind
    }

    if (scaled) {
      scaling.method <- c(
        if (has.scale.ind) "scaling indicator",
        if (latent.var.fixed) "fixed latent-variable variance",
        if (mean.structure && has.scale.ind && scale.ind.intercept.fixed) {
          "fixed scaling-indicator intercept"
        },
        if (mean.structure && latent.mean.fixed) "fixed latent-variable mean"
      )
      scaling.method <- scaling.method[nzchar(scaling.method)]
      scaling.method <- paste(scaling.method, collapse = ", ")

      # Capitalize only the first character
      scaling.method <- paste0(
        toupper(substr(scaling.method, 1, 1)),
        substr(scaling.method, 2, nchar(scaling.method))
      )
      scaling.tables[[i]]$scaling.method <- scaling.method
    } else {
      fail.reason <- c(
        if (!units.assigned) {
          "neither scaling indicator nor fixed latent variance"
        },
        if (mean.structure && !origin.assigned) {
          "neither fixed scaling indicator intercept nor fixed latent variable mean"
        }
      )
      fail.reason <- fail.reason[nzchar(fail.reason)]
      fail.reason <- paste(fail.reason, collapse = ", and ")
      # capitalize first character
      fail.reason <- paste0(
        toupper(substr(fail.reason, 1, 1)),
        substr(fail.reason, 2, nchar(fail.reason))
      )
      scaling.tables[[i]]$fail.reason <- fail.reason
    }

  }

  if (return.type == "logical") {
    return(out)
  } else {
    out <- list(
      Scaling = scaling.tables,
      partable = x,
      call = call,
      print.options = list(
        include.msgs = include.msgs
      )
    )
    class(out) <- c("semscale")
    return(out)
  }
}

#' @export
scaling.default <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                            return.type = c("table", "logical"), ...) {
  stop("Unknown object type. Please supply a model string, lavaan parameter table, or fitted model object.")
}


#' @export
scaling.semid2 <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                           return.type = c("object", "logical"), ...) {
  print.semid2(x)
  cat("\n")
  scaling(x$partable, call = call, include.msgs = include.msgs, lv = lv, return.type = return.type)
}