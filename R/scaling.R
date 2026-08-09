#' Check if latent variables are correctly scaled
#'
#' This function checks whether latent variables in the model are scaled, which
#' is a necessary condition for model identification. A latent variable is
#' scaled if it has a fixed loading on a scaling indicator or the latent
#' variable has a fixed variance. When mean structure is present, the scaling
#' indicator must also have a fixed intercept or the latent variable must also
#' have a fixed mean. If any latent variable is not scaled, the model is not
#' identified.
#'
#' @param x \code{lavaan} model syntax, a \code{lavaan} parameter table or a
#'        \code{semidentify} ID object.
#' @param lav_fun A character string specifying the function you intend to use
#'        to fit the model. This will ensure the correct model defaults are
#'        specified. Options currently include "lavaan", "sem", or "cfa". If a
#'        parameter table or fitted model object are supplied, this argument is
#'        ignored.
#' @param print_msgs Logical. If \code{TRUE} the output will print why a latent
#'        variable is not scaled when applicable and the method or methods by
#'        which it is scaled when it is scaled. If \code{FALSE}, the output will
#'        only print whether the latent variable is scaled or not and a few
#'        other descriptives.
#' @param lv An optional character vector of the specific latent variables you
#'        would like to check. Otherwise, all latent variables are extracted
#'        from the parameter table
#' @param return_type A character string specifying the type of output. Options
#'        include "logical" (a logical vector specifying whether each latent
#'        variable is correctly scaled) or "object" (an object with additional
#'        information about the scaling of each latent variable). The latter is
#'        of type \code{semscale} and has a custom print method for additional
#'        diagnosis of identification issues.
#' @param ... Additional arguments passed to the \code{lavaanify} function from
#'        \code{lavaan}. See \link[lavaan]{lavaanify} for more information.
#'        These are only used when a model string is supplied. Otherwise, they
#'        are ignored.
#'
#' @return An object of class \code{semscale} with all scaling information (if
#' \code{return_type = "object"}), or a logical vector (if \code{return_type =
#' "logical"}) indicating whether each latent variable is correctly scaled.
#'
#' @export
#' @name scaling
#' @examples
#' my_model <- ' L1 =~ x1 + x2 + x3
#'               L2 =~ x4 + x5 + x6
#'               L3 =~ x7 + x8 + x9
#'               L2 ~ L1
#'               L3 ~ L2 '
#' scaling(my_model, print_msgs = TRUE, lav_fun = "sem",
#'         meanstructure = FALSE)
#'
scaling <- function(x, lav_fun = "sem", print_msgs = TRUE, lv = NULL,
                    return_type = c("object", "logical"), ...) {
  return_type <- match.arg(return_type)
  if (!is.character(lv) && !is.null(lv)) {
    stop(gettext("lv= must be a character vector or NULL"))
  }
  if (!is.logical(print_msgs) || length(print_msgs) != 1) {
    stop(gettext("print_msgs= must be a logical"))
  }
  if (!is.character(return_type) || length(return_type) != 1 ||
  !(return_type %in% c("object", "logical"))) {
    stop(
      gettext("return_type= must be a character string and one of 'object' or 'logical'")
    )
  }
  lav_fun <- validate_lav_fun(lav_fun)
  lav_fun <- validate_lav_fun(lav_fun)
  UseMethod("scaling")
}

#' @export
scaling.semid <- function(x, lav_fun = "sem", print_msgs = TRUE, lv = NULL, 
                          return_type = c("object", "logical"), ...) {
  print.semid(x)
  scaling.data.frame(
    x = x$partable,
    lav_fun = lav_fun,
    print_msgs = print_msgs,
    lv = lv,
    return_type = return_type,
    ...
  )
}

#' @rdname scaling
#' @export
scaling.lavaan <- function(x, lav_fun = "sem", print_msgs = TRUE, lv = NULL,
                           return_type = c("object", "logical"), ...) {
  out <- lavaan_obj_to_partable(x, lav_fun = lav_fun, ...)
  scaling.data.frame(
    x = out$partable,
    lav_fun = out$lav_fun,
    print_msgs = print_msgs,
    lv = lv,
    return_type = return_type,
    ...
  )
}

#' @export
scaling.character <- function(x, lav_fun = "sem", print_msgs = TRUE, lv = NULL,
                              return_type = c("object", "logical"), ...) {
  out <- lavaan_syntax_to_partable(x, lav_fun = lav_fun, ...)
  scaling.data.frame(
    x = out$partable,
    lav_fun = out$lav_fun,
    print_msgs = print_msgs,
    lv = lv,
    return_type = return_type,
    ...
  )
}

#' @export
scaling.data.frame <- function(x, lav_fun = "sem", print_msgs = TRUE, lv = NULL,
                               return_type = c("object", "logical"), ...) {
  return_type <- match.arg(return_type)
  if (is_lavaan_partable(x)) {
  partable <- x
  } else {
    stop(gettext("Unknown input type."))
  }

  if (is.null(lv)) {
    # retrieve attributes and variable names
    vars <- get_partable_vars(partable, "lv")
    lv <- vars$lv
  }
  if (length(lv) == 0) {
    stop(gettext("Scaling only applies to models with latent variables."))
  }

  if (return_type == "object") {
    scaling_table <- list(
      lv = NA_character_,
      scaled = NA,
      n_indicators = NA_integer_,
      scaling_indicator = NA_character_,
      mean_structure = NA,
      fail_reason = NA_character_,
      scaling_method = NA_character_
    )
    scaling_tables <- replicate(length(lv), scaling_table, simplify = FALSE)
  } else {
    out <- vector(length = length(lv))
    names(out) <- lv
  }

  for (i in seq_along(lv)) {
    var <- lv[i]
    if (return_type == "object") {
      scaling_tables[[i]]$lv <- var
    }

    mean_structure <- any(with(x, lhs == var & op == "~1"))

    scale_ind_idx <- with(
      partable,
      lhs == var &
        op == "=~" &
        free == 0 &
        rhs != var &
        !is.na(ustart) &
        ustart != 0
    )
    has_scale_ind <- any(scale_ind_idx)
    scale.ind <- if (has_scale_ind) {
      with(partable, rhs[scale_ind_idx])
    } else {
      character(0)
    }

    scale_ind_intercept_fixed <- if (has_scale_ind && mean_structure) {
      any(with(partable, lhs %in% scale.ind & op == "~1" & free == 0))
    } else {
      TRUE
    }
    latent_variance_fixed <- any(
      with(
        partable,
        lhs == var &
          rhs == var &
          op == "~~" &
          free == 0 &
          !is.na(ustart) &
          ustart > 0
      )
    )
    latent_mean_fixed <- if (mean_structure) {
      any(with(partable, lhs == var & op == "~1" & free == 0))
    } else {
      TRUE
    }

    units_assigned <- has_scale_ind || latent_variance_fixed
    origin_assigned <- if (mean_structure) {
      latent_mean_fixed || (has_scale_ind && scale_ind_intercept_fixed)
      } else {
        TRUE
      }
    scaled <- units_assigned && origin_assigned
    if (return_type == "logical") {
      out[var] <- scaled
      next
    }

    # populate scaling table
    scaling_tables[[i]]$mean_structure <- mean_structure
    scaling_tables[[i]]$scaled <- scaled
    indicator_rows <- with(partable, lhs == var & op == "=~" & rhs != var)
    scaling_tables[[i]]$n_indicators <- sum(indicator_rows)
    if (has_scale_ind) {
      scaling_tables[[i]]$scaling_indicator <- scale.ind
    }

    if (scaled) {
      scaling_method <- c(
        if (has_scale_ind) "scaling indicator",
        if (latent_variance_fixed) "fixed latent-variable variance",
        if (mean_structure && has_scale_ind && scale_ind_intercept_fixed) {
          "fixed scaling-indicator intercept"
        },
        if (mean_structure && latent_mean_fixed) "fixed latent-variable mean"
      )
      scaling_method <- scaling_method[nzchar(scaling_method)]
      scaling_method <- paste(scaling_method, collapse = ", ")

      # capitalize only the first character
      scaling_method <- paste0(
        toupper(substr(scaling_method, 1, 1)),
        substr(scaling_method, 2, nchar(scaling_method))
      )
      scaling_tables[[i]]$scaling_method <- scaling_method
    } else {
      fail_reason <- c(
        if (!units_assigned) {
          "neither scaling indicator nor fixed latent variance"
        },
        if (mean_structure && !origin_assigned) {
          "neither fixed scaling indicator intercept nor fixed latent variable mean"
        }
      )
      fail_reason <- fail_reason[nzchar(fail_reason)]
      fail_reason <- paste(fail_reason, collapse = ", and ")
      # capitalize first character
      fail_reason <- paste0(
        toupper(substr(fail_reason, 1, 1)),
        substr(fail_reason, 2, nchar(fail_reason))
      )
      scaling_tables[[i]]$fail_reason <- fail_reason
    }

  }

  if (return_type == "logical") {
    out
  } else {
    out <- list(
      Scaling = scaling_tables,
      partable = partable,
      lav_fun = lav_fun,
      print_options = list(
        print_msgs = print_msgs
      )
    )
    class(out) <- c("semscale")
    out
  }
}

#' @export
scaling.default <- function(x, lav_fun = "sem", print_msgs = TRUE, lv = NULL,
                            return_type = c("table", "logical"), ...) {
  stop(gettext("Unknown input type."))
}


#' @export
scaling.semid2 <- function(x, lav_fun = "sem", print_msgs = TRUE, lv = NULL,
                           return_type = c("object", "logical"), ...) {
  print.semid2(x)
  cat("\n")
  scaling(
    x = x$partable,
    lav_fun = lav_fun,
    print_msgs = print_msgs,
    lv = lv,
    return_type = return_type
  )
}