#' Internal validation function for lav_fun argument
#' @param lav_fun The input value for the lav_fun argument
#' @param options The valid options for the lav_fun argument
#' @noRd
validate_lav_fun <- function(lav_fun, options = c("lavaan", "sem", "cfa", NA)) {
  if (length(lav_fun) != 1 || !(is.character(lav_fun) || is.na(lav_fun)) ||
  !(lav_fun %in% options)) {
    stop(gettextf("lav_fun= must be one of %s",
      paste0("'", options, "'", collapse = ", ")))
  }
  lav_fun <- ifelse(is.na(lav_fun), NA_character_, lav_fun)
  lav_fun
}