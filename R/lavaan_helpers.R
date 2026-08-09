lavaan_syntax_to_partable <- function(x, lav_fun, ...) {
  dotdotdot <- list(...)
  if (grepl("\\.inp$", x, ignore.case = TRUE)) {
    stop(gettext(
      paste("This looks like an Mplus input file.\n",
            "Did you mean to use id_mplus() instead?")
    ))
  }
  if (isTRUE(dotdotdot$model_type == "efa")) {
    dotdotdot[["model_type"]] <- NULL
    warning(gettext("Only model_type='sem' is currently supported"))
  }
  if (isTRUE(dotdotdot$debug)) {
    dotdotdot[["debug"]] <- NULL
    warning(gettextf("Ignoring debug=%s", dotdotdot$debug))
  }
  if (is.na(lav_fun)) {
    warning("lav_fun= is set to NA. Defaulting to lav_fun='sem'")
    lav_fun <- "sem"
  }
  if (is.null(dotdotdot$auto)) {
    dotdotdot$auto <- (lav_fun != "lavaan")
  }
  args <- c(
    list(
      model = x,
      warn = TRUE,
      debug = FALSE,
      model_type = "sem"
    ),
    dotdotdot
  )
  partable <- do.call(lavaan::lavaanify, args)
  id_model_type <- classify_model(partable) # errors are handled in this function
  if (lav_fun == "cfa" && id_model_type != "cfa") {
    warning(gettext("sem() or lavaan() may be more appropriate functions for this type of model"))
  }
  list(partable = partable, lav_fun = lav_fun, id_model_type = id_model_type)
}

lavaan_obj_to_partable <- function(x, lav_fun, ...) {
  dotdotdot <- list(...)
  if (length(dotdotdot) > 0) {
    warning(gettext("Additional arguments are ignored when a fitted lavaan object is supplied"))
  }
  lav_fun_orig <- get_lavaan_cmd(x)
  if (!is.na(lav_fun) && lav_fun_orig != lav_fun) {
    warning(
      gettextf("The fitted lavaan object was created with %s, but you specified lav_fun='%s'. This may lead to unexpected results.",
               format_lavaan_fun(lav_fun_orig), lav_fun)
    )
  }
  partable <- as.data.frame(
    x@ParTable,
    stringsAsFactors = FALSE
  )
  return(list(partable = partable, lav_fun = lav_fun))
}