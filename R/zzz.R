#' RiverSatUtils: Utilities for RiverSAT data workflows
#'
#' Includes utilities for downloading, loading, and processing river network datasets
#' (SWORD, GRWL, and WBD), converting river reaches to graph/polygon products,
#' and creating project-level RiverSAT data bundles.
#'
#' @keywords internal
#' @import sf
#' @import terra
#' @import data.table
"_PACKAGE"


#' Initialize package defaults on load
#'
#' Called when the package is loaded. Sets the package cache path to the
#' current working directory by calling [setCachePath()].
#'
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  setCachePath(getwd())
  invisible(NULL)
}
