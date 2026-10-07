#' Download SWORD data archive
#'
#' Downloads SWORD v17c from Zenodo into the configured cache directory,
#' unzips into `SWORD_v17`, and updates internal SWORD cache location.
#'
#' @param version SWORD version. Currently only `"17c"` is supported.
#' @param timeout_min Download timeout in minutes.
#' @param overwrite Logical; re-download if existing extracted files are found.
#' @param keep_zip Logical; keep downloaded zip file after extraction.
#' @return Invisibly returns path to extracted SWORD GPKG directory.
#' @examples
#' \dontrun{download_sword()}
#' @export
download_sword <- function(version = "17c",
                           timeout_min = 60,
                           overwrite = FALSE,
                           keep_zip = FALSE) {
  if (version != "17c") stop("Only version = '17c' currently supported, sorry.")

  sword_dir <- file.path(getCachePath(), "SWORD_v17")
  out_zip <- file.path(getCachePath(), "SWORD_v17c_gpkg.zip")
  url <- "https://zenodo.org/records/22259077/files/SWORD_v17c_gpkg.zip?download=1"

  if (!dir.exists(getCachePath())) {
    dir.create(getCachePath(), recursive = TRUE)
  }

  if (dir.exists(sword_dir) && !overwrite) {
    message("SWORD appears to already be downloaded at: ", sword_dir,
            "\nUse overwrite = TRUE to re-download.")
    setSwordCacheDir(file.path(sword_dir, "gpkg"))
    return(invisible(file.path(sword_dir, "gpkg")))
  }

  if (dir.exists(sword_dir) && overwrite) {
    unlink(sword_dir, recursive = TRUE, force = TRUE)
  }

  timeout_default <- getOption("timeout")
  on.exit(options(timeout = timeout_default), add = TRUE)
  options(timeout = 60 * timeout_min)

  result <- download.file(
    url = url,
    destfile = out_zip,
    mode = "wb"
  )

  if (!identical(result, 0L)) {
    stop("Failed to download SWORD archive from Zenodo.")
  }

  dir.create(sword_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(out_zip, exdir = sword_dir)

  if (!keep_zip && file.exists(out_zip)) {
    unlink(out_zip)
  }

  setSwordCacheDir(file.path(sword_dir, "gpkg"))
  invisible(file.path(sword_dir, "gpkg"))
}

#' Download GRWL datasets
#'
#' Downloads GRWL mask archive from Zenodo and optionally the GRWL tiles
#' archive from Google Drive. Extracts into cache and updates configured
#' GRWL paths.
#'
#' @param version GRWL version. Currently only `"01.01"` is supported.
#' @param timeout_min Download timeout in minutes.
#' @param overwrite Logical; re-download if existing extracted files are found.
#' @param keep_zip Logical; keep downloaded zip files after extraction.
#' @param download_tiles Logical; also download tiles shapefile archive.
#' @return Invisibly returns list with `mask` and `tiles` paths.
#' @examples
#' \dontrun{download_grwl()}
#' @export
download_grwl <- function(version = "01.01",
                          timeout_min = 60,
                          overwrite = FALSE,
                          keep_zip = FALSE,
                          download_tiles = TRUE) {
  if (version != "01.01") stop("Only version = '01.01' currently supported, sorry.")

  cache_dir <- getCachePath()
  if (is.null(cache_dir) || !nzchar(cache_dir)) {
    stop("Cache path is not set. Call setCachePath() first.")
  }

  # Mask dataset paths/URL
  grwl_dir <- file.path(cache_dir, "GRWL_v01.01")
  out_zip  <- file.path(cache_dir, "GRWL_mask_V01.01.zip")
  mask_url <- "https://zenodo.org/records/1297434/files/GRWL_mask_V01.01.zip?download=1"

  # Tiles dataset paths/URL
  tiles_dir <- file.path(grwl_dir, "GRWL_tiles_v01.01")
  out_tiles_zip <- file.path(cache_dir, "GRWL_tiles_v01.01.zip")
  tiles_shared_url <- "https://drive.google.com/file/d/1K6x1E0mmLc0k7er4NCIeaZsTfHi2wxxI/view"
  tiles_url <- .gdrive_direct_download_url(tiles_shared_url)

  need_mask <- overwrite || !dir.exists(grwl_dir)
  need_tiles <- isTRUE(download_tiles) && (overwrite || !dir.exists(tiles_dir))

  if (!need_mask && !need_tiles) {
    message("GRWL data already present in cache. Use overwrite = TRUE to re-download.")
    setGrwlFilepath(grwl_dir)
    if (isTRUE(download_tiles)) setGrwlTilesFilepath(tiles_dir)
    return(invisible(list(mask = grwl_dir,
                          tiles = if (isTRUE(download_tiles)) tiles_dir else NULL)))
  }

  timeout_default <- getOption("timeout")
  on.exit(options(timeout = timeout_default), add = TRUE)
  options(timeout = 60 * timeout_min)

  # Download/rebuild mask dataset
  if (need_mask) {
    if (dir.exists(grwl_dir) && overwrite) {
      unlink(grwl_dir, recursive = TRUE, force = TRUE)
    }

    result_mask <- download.file(
      url = mask_url,
      destfile = out_zip,
      mode = "wb"
    )

    if (!identical(result_mask, 0L)) {
      stop("Failed to download GRWL mask archive from Zenodo.")
    }

    dir.create(grwl_dir, recursive = TRUE, showWarnings = FALSE)
    unzip(out_zip, exdir = grwl_dir)

    if (!keep_zip && file.exists(out_zip)) {
      unlink(out_zip)
    }
  }

  # Optional download/rebuild tiles shapefile dataset
  if (need_tiles) {
    if (dir.exists(tiles_dir) && overwrite) {
      unlink(tiles_dir, recursive = TRUE, force = TRUE)
    }

    result_tiles <- download.file(
      url = tiles_url,
      destfile = out_tiles_zip,
      mode = "wb"
    )

    if (!identical(result_tiles, 0L)) {
      stop("Failed to download GRWL tiles archive from Google Drive.")
    }

    dir.create(tiles_dir, recursive = TRUE, showWarnings = FALSE)
    unzip(out_tiles_zip, exdir = tiles_dir)

    if (!keep_zip && file.exists(out_tiles_zip)) {
      unlink(out_tiles_zip)
    }
  }

  setGrwlFilepath(file.path(grwl_dir, "GRWL_mask_V01.01"))
  if (isTRUE(download_tiles)) {
    setGrwlTilesFilepath(tiles_dir)
  }

  invisible(list(mask = file.path(grwl_dir, "GRWL_mask_V01.01"),
                 tiles = if (isTRUE(download_tiles)) tiles_dir else NULL))
}


#' Download national watershed boundary dataset
#'
#' Downloads the USGS WBD National GPKG zip archive into cache,
#' extracts to `WBD_National_GPKG`, and updates the active WBD filepath.
#'
#' @param timeout_min Download timeout in minutes.
#' @param overwrite Logical; re-download if existing geopackage is found.
#' @param keep_zip Logical; keep downloaded zip file after extraction.
#' @return Invisibly returns path to `WBD_National_GPKG.gpkg`.
#' @examples
#' \dontrun{download_wbd()}
#' @export
download_wbd <- function(timeout_min = 60,
                         overwrite = FALSE,
                         keep_zip = FALSE) {
  cache_dir <- getCachePath()
  if (is.null(cache_dir) || !nzchar(cache_dir)) {
    stop("Cache path is not set. Call setCachePath() first.")
  }

  wbd_dir <- file.path(cache_dir, "WBD_National_GPKG")
  wbd_gpkg <- file.path(wbd_dir, "WBD_National_GPKG.gpkg")
  out_zip <- file.path(cache_dir, "WBD_National_GPKG.zip")
  wbd_url <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GPKG/WBD_National_GPKG.zip"

  if (file.exists(wbd_gpkg) && !overwrite) {
    message("WBD appears to already be downloaded at: ", wbd_dir,
            "\nUse overwrite = TRUE to re-download.")
    setWbdFilepath(wbd_gpkg)
    return(invisible(wbd_gpkg))
  }

  if (file.exists(wbd_gpkg) && overwrite) {
    unlink(wbd_dir, recursive = TRUE, force = TRUE)
  }

  timeout_default <- getOption("timeout")
  on.exit(options(timeout = timeout_default), add = TRUE)
  options(timeout = 60 * timeout_min)

  result <- download.file(
    url = wbd_url,
    destfile = out_zip,
    mode = "wb"
  )

  if (!identical(result, 0L)) {
    stop("Failed to download WBD archive from USGS TNM S3.")
  }

  dir.create(wbd_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(out_zip, exdir = wbd_dir)

  if (!keep_zip && file.exists(out_zip)) {
    unlink(out_zip)
  }

  setWbdFilepath(wbd_gpkg)

  invisible(wbd_gpkg)
}
