.onAttach <- function(libname, pkgname) {
  msg <- sprintf("Welcome to %s v%s! Happy multiverse analysis :)", pkgname, utils::packageVersion(pkgname))
  packageStartupMessage(msg)
}