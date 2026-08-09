.onAttach <- function(libname, pkgname) {
  desc <- utils::packageDescription(pkgname)

  packageStartupMessage(
    desc$Package, " ", desc$Version,
    "\nPlease report bugs, unexpected results, or edge cases at:\n",
    desc$BugReports
  )
}

# for compatibility with columns in lavaan parameter tables
utils::globalVariables(c("op", "free", "lhs", "rhs"))
