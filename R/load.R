#' Load SWORD reaches or nodes
#'
#' Loads SWORD v17c from either an active RiverSAT project (`reaches.gpkg`) or
#' from cache by continent/layer.
#'
#' When `load_from_project = TRUE`, reads the file configured in package state.
#' Otherwise reads `sword_<CONTINENT>_v17c.gpkg` from SWORD cache.
#'
#' @param version SWORD version; currently only `"17c"` is supported.
#' @param load_from_project Logical; read from active project file when `TRUE`.
#' @param continent Continent code used for cache file selection (`na`, `sa`,
#'   `oc`, `eu`, `as`, `af`) when not loading from project.
#' @param layer Layer name in geopackage (`"reaches"` or `"nodes"`); ignored
#'   when loading from project.
#' @param download Logical; auto-download SWORD if not found locally (ignored
#'   when loading from project).
#' @return An `sf` object from [sf::read_sf()].
#' @export
load_sword <- function(version = "17c",
                       load_from_project = !is.null(getRiversatProject()),
                       continent = c("na", "sa", "oc", "eu", "as", "af")[1], # continents: na = north america, sa = south america, oc = oceania, eu = europe, as = asia, af = africa. This determines which file to load if project_dir = NULL.
                       layer = "reaches", # reaches or nodes. ignored if load_from_project = TRUE
                       download = FALSE# automatically download the dataset if it's not found in the cache? (~4GB size) Ignored if load_from_project = TRUE

) {
  if (version != "17c") stop("Only version = '17c' currently supported, sorry.")
  if (load_from_project) {
    if (file.exists(getSwordFilepath())) {
      fp <- getSwordFilepath()
      layer = NULL
    } else {
      stop("could not find reaches file in project dir.")
    }
  } else {
    fp <- file.path(getSwordCacheDir(), paste0("sword_", toupper(continent), "_v", version, ".gpkg"))
    if (!file.exists(fp)) {
      if (download) {
        message("Downloading SWORD dataset to ", getSwordCacheDir())
        download_sword()
      } else {
        stop("Could not find SWORD dataset locally. Set download = TRUE to download or update sword cache path via setSwordCachePath() to the correct location.")
      }
    }
  }
  read_sf(fp, layer = layer)
}
