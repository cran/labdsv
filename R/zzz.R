.First.lib <- function(lib, pkg) {
  library.dynam("labdsv", pkg, lib)
  require(MASS)
  require(mgcv)
  require(akima)
}
