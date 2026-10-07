#' Create a RiverSAT project bundle
#'
#' Creates a project directory, writes project boundary geometry, subsets SWORD
#' reaches to the AOI, optionally adds dam-distance summaries, optionally builds
#' polygons, and optionally appends GRWL summaries.
#'
#' If `use_huc = TRUE`, AOI is expanded to intersecting HUC units via
#' [nhdplusTools::get_huc()].
#'
#' @param project_name Project name used to create output directory
#'   (`file.path(base_dir, project_name)`).
#' @param aoi AOI geometry (`sf` point or polygon).
#' @param base_dir Directory in which the project folder will be created.
#' @param use_huc Logical; use intersecting HUC polygon(s) as project AOI.
#' @param huc_level Character HUC level string (e.g., `"huc08"`) passed to
#'   [nhdplusTools::get_huc()] when `use_huc = TRUE`.
#' @param activate Logical; activate created directory as current RiverSAT
#'   project in package state.
#' @param download Logical; download datasets if missing from cache.
#' @param add_dam_info Logical; add distance-to-upstream-obstruction fields.
#' @param make_polygons Logical; derive per-reach polygons.
#' @param add_grwl_info Logical; append GRWL summaries to polygons
#'   (ignored if `make_polygons = FALSE`).
#' @param verbose Logical verbosity flag.
#' @return Invisibly returns `NULL`; writes project files to disk.
#' @examples
#' \dontrun{
#' # aoi <- sf::st_sfc(sf::st_point(c(-122.872557, 44.049917)), crs = "EPSG:4326")
#' # make_riversat_project("demo_project", aoi = aoi, base_dir = tempdir())
#' }
#' @export
make_riversat_project <- function(project_name, # A name for the project. Everything will be saved in file.path(project_dir, project_name).
                                  aoi, # point or polygon
                                  base_dir, # Where to start the project
                                  use_huc = TRUE, # use the huc intersecting with the aoi? If false, will use the given aoi for the project boundaries.
                                  huc_level = "huc08", # if use_huc = TRUE, what size of huc to use? Will create AOI including all hucs of this level intersecting the aoi. Format = "huc##"
                                  activate = TRUE, # activate as current riversat project? This will not change the current R project but will update the defaults for some functions.
                                  download = TRUE, # download datasets if not already found in cache directory?
                                  add_dam_info = TRUE, # include distance to upstream dam? Only includes dams on SWORD reaches within the AOI.
                                  make_polygons = TRUE,
                                  add_grwl_info = TRUE, # add info from global river widths from landsat database to polygons? Ignored if make_polygons = FALSE.
                                  verbose = TRUE) {
  if (use_huc) {
    aoi <- nhdplusTools::get_huc(aoi, type = huc_level)
  }
  project_dir <- file.path(base_dir, project_name)
  if (dir.exists(project_dir) &
      length(list.files(project_dir)) > 0) {
    stop(project_dir, " exists and is not empty. Cannot create a new project here.")
  }
  if (!dir.exists(base_dir)) {
    stop("could not find ", base_dir)
  }
  dir.create(project_dir)
  write_sf(aoi, file.path(project_dir, "project_bounds.gpkg"))
  # get river reaches
  sword <- load_sword(download = download)
  sword_reaches <- sword[st_transform(aoi, st_crs(sword)), ]
  if (add_dam_info) {
    sword_reaches <- get_dam_info(sword_reaches, append = TRUE)
  }
  write_sf(sword_reaches, file.path(project_dir, "reaches.gpkg"))
  rm(sword)
  if (make_polygons) {
    polygons <- sword_to_polygons(sword_reaches)
    if (add_grwl_info) {
      polygons <- get_grwl_info(polygons)
    }
    write_sf(polygons, file.path(project_dir, "water_polygons.gpkg"))
  }
  if (activate) {
    loadRiversatProject(project_dir)
  }

}
