# internal warning function for controlled messages
#' @noRd
id_warn <- function(..., internal = FALSE) {
  msgs <- c(...)
  if (internal) {
    msgs <- c(msgs, gettext("This is an internal warning. Please report this issue to the package maintainer."))
  }
  msg <- paste0(msgs, collapse = "\n")
  warning(msg, call. = FALSE)
}

# internal stop function for controlled messages
#' @noRd
id_stop <- function(..., internal = FALSE) {
  msgs <- c(...)
  if (internal) {
    msgs <- c(msgs, gettext("This is an internal error. Please report this issue to the package maintainer."))
  }
  msg <- paste0(msgs, collapse = "\n")
  stop(msg, call. = FALSE)
}