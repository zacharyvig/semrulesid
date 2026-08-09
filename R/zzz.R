.onAttach <- function(libname, pkgname) {
  version <- read.dcf(
    file = system.file("DESCRIPTION", package = pkgname),
    fields = "Version"
  )[1]
  bugreport <- read.dcf(
    file = system.file("DESCRIPTION", package = pkgname),
    fields = "BugReports"
  )[1]
  packageStartupMessage(
    pkgname, " ", version, "\nPlease report any bugs or edge cases at:\n",
    bugreport
  )
}

# for compatibility with columns in lavaan parameter tables
utils::globalVariables(c("op", "free", "lhs", "rhs"))
