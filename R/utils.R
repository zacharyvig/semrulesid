#' internal validation function for lav_fun argument
#' @param lav_fun The input value for the lav_fun argument
#' @param options The valid options for the lav_fun argument
#' @noRd
validate_lav_fun_arg <- function(lav_fun, options = c("lavaan", "sem", "cfa")) {
  valid <- (length(lav_fun) == 1 && (is.character(lav_fun) && lav_fun %in% options)) || is.na(lav_fun)
  if (!valid) {
    stop(gettextf("lav_fun= must be one of %s or NA",
      paste0("'", options, "'", collapse = ", ")))
  }
  lav_fun <- ifelse(is.na(lav_fun), NA_character_, lav_fun)
  lav_fun
}