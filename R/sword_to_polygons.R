

#' Create non-overlapping reach polygons from SWORD lines
#'
#' Creates a per-reach polygon representation by buffering each reach,
#' discretizing to raster space, assigning pixels to nearest overlapping reach,
#' and converting the selected mask back to polygons.

#'
#' @param sword_reaches `sf` line features for reaches with width metadata.
#' @param crs Coordinate reference system used for polygonization.
#' @param res Raster resolution (meters) used during vector-raster-vector
#'   conversion.
#' @return `sf` polygons with columns `reach_id` and `geometry`.
#' @details
#' Buffer width is constrained by:
#' * reach length,
#' * `width + 2 * sqrt(width_var)`, and
#' * `2 * width`.
#'
#' This follows a nearest-reach partitioning approach to avoid overlap between
#' adjacent reach polygons.
#'
#' @export
sword_to_polygons <- function(sword_reaches,
                              crs = "EPSG:5070",
                              res = 10 # resolution in meters to descretize when going from vector to rast and back again
                              ) {

  sword_reaches <- st_transform(sword_reaches, crs)
  template_rast <- rast(ext(sword_reaches), crs = crs(sword_reaches), resolution = res)
  geoms <- st_geometry(sword_reaches)
  # buffer width is the minimum of:
  # 1. the length of the reach (can't be wider than it is long)
  # 2. double its average width
  # 3. width + 2*sqrt(var(width)) (~95% quartile)
  # Don't want it too big or else we end up with big polygons
  widths <- pmin(as.numeric(st_length(sword_reaches)),
                 sword_reaches$width + 2*sqrt(sword_reaches$width_var),
                 2*sword_reaches$width)
  geoms_buff <- st_buffer(geoms, widths)
  sword_reaches$buff_geom <- geoms_buff

  for (i in 1:nrow(sword_reaches)) {
    reach_id = sword_reaches[i,]$reach_id
    # print(as.character(reach_id))

    # Just get this to use as a template raster
    template_reach <- crop(template_rast, geoms_buff[i])
    buff_r <- rasterize(vect(geoms_buff[i]), template_reach)
    r <- buff_r
    # Make distance rasters for all overlapping polygons
    # shrink buffer a tiny bit when looking for neighbors
    # to avoid bugs caused when a line just touches the buffer.
    neighbors <- sword_reaches[st_buffer(geoms_buff[i], -1), ]
    for (g in st_geometry(neighbors)) {
      d <- rasterize(vect(g), template_reach)
      if (!all(is.na(values(d)))) {
        r <- c(r, distance(d))
      }
    }
    # Make raster with index of closest reach
    r <- r[[-1]]
    # way faster than apply(x, 1, which.min)
    closest_vect <- max.col(-round(as.matrix(r), 2))
    closest_r <- rast(template_reach)
    values(closest_r) <- neighbors$reach_id[closest_vect]
    rm(closest_vect)
    # 256 = No Data
    # 255 = River
    # 180 = Lake/reservoir
    # 126 = Tidal rivers/delta
    # 86  = Canal
    reach_mask <- (closest_r == reach_id) & (buff_r == 1)
    # make polygon
    p <- as.polygons(reach_mask)
    p_ <- st_as_sf(p[p[[1]][[1]] == 1,])$geometry
    if ("sfc_MULTIPOLYGON" %in% class(p_)) {
      p_ <- st_cast(p_, "POLYGON")
      p_ <- p_[which.max(st_area(p_)),]
    }
    p_ <- st_simplify(p_, preserveTopology = FALSE, dTolerance = res)
    sword_reaches$buff_geom[i] <- p_ # Only include "true" polygon (it also makes an inverse of "false" values)
  }
  st_drop_geometry(sword_reaches[, c("reach_id", "buff_geom")]) |>
      setNames( c("reach_id", "geometry")) |>
      st_as_sf()
}

#' Attach GRWL raster summary info to polygons
#'
#' Intersects polygons with GRWL tile footprints, extracts class counts from
#' matching GRWL mask rasters, and computes:
#' * `grwl_water_count`: total non-zero GRWL pixels
#' * `grwl_code`: dominant non-zero GRWL class
#'
#' GRWL classes:
#' * 255 = River
#' * 180 = Lake/reservoir
#' * 126 = Tidal rivers/delta
#' * 86  = Canal
#' * 256 = No Data
#'
#' @param polygons `sf` polygon object.
#' @return Input polygons with appended GRWL summary columns.
#' @export
get_grwl_info <- function(polygons # sf polygons
                          ) {
  grwl_tiles <- st_read(getGrwlTilesFilepath()) |>
    st_transform(st_crs(polygons))
  grwl_tiles <- grwl_tiles[polygons, ]
  rast_fps <- paste0(grwl_tiles$TILE_ID, ".tif")
  all_rast_files <- list.files(getGrwlFilepath())
  if (!all(rast_fps %in% all_rast_files)) {
    stop("grwl raster tiles missing: ",
         paste(rast_fps[!rast_fps %in% all_rast_files]),
         ". Perhaps you need to download them (you can download all grwl tiles with download_grwl()), or update the grwl filepath? getGrwlFilepath()=", getGrwlFilepath())
  }
  values_list <- lapply(rast_fps, \(rast_fp) {
    grwl_mask <- rast(file.path(getGrwlFilepath(), rast_fp))
    p <- st_transform(polygons, crs(grwl_mask))
    values <- extract(grwl_mask, p, fun = table)
    data.table(values) |>
      setNames(c("ID", "grwl_code", "count"))
  })
  values <- rbindlist(values_list)
  values = values[, .(count = sum(count)), .(ID, grwl_code)]
  grwl_info = values[grwl_code != 0, .(grwl_water_count = sum(count),
                           grwl_code = grwl_code[which.max(count)]), .(ID)][
                             order(ID)
                           ]
  cbind(polygons, grwl_info[, .(grwl_water_count, grwl_code)])
}
