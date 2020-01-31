.onAttach <- function(libname, pkgname) {

}


#' @export
np <- NULL

.onLoad <- function(libname, pkgname) {

  delayedAssign("np", reticulate::import("numpy", delay_load = TRUE, convert = FALSE),
                eval.env = baseenv(), assign.env = getNamespace(pkgname))

}
