.onAttach <- function(libname, pkgname) {
  version <- read.dcf(
    file = system.file("DESCRIPTION", package = pkgname),
    fields = "Version"
  )[1]
  packageStartupMessage(
    paste(pkgname, version, "is still in the development phase.\nPlease report any bugs or edge cases to the GitHub repository.")
  )
}

utils::globalVariables(c("op", "free", "lhs", "rhs")) # for compatibility with columns in lavaan parameter tables
