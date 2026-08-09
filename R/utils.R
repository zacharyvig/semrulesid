# internal warning function for controlled messages
#' @noRd
id_warn <- function(...) {
  msg <- paste0(...)
  warning(msg, call. = FALSE)
}

# internal stop function for controlled messages
#' @noRd
id_stop <- function(...) {
  msg <- paste0(...)
  stop(msg, call. = FALSE)
}