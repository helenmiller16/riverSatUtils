# INTERNALS -----


#' Internal package environment
#'
#' Mutable package-level environment used to store paths and active project state,
#' including cache location and dataset filepaths.
#'
#' @keywords internal
#' @noRd
.riversatr_environment <- new.env(parent=emptyenv())

#' Set SWORD cache directory
#'
#' Stores the directory containing SWORD geopackage files, typically
#' `.../SWORD_v17/gpkg`.
#'
#' @param swordDir Character path to SWORD cache directory.
#' @return Invisibly updates internal package state.
#' @keywords internal
#' @noRd
setSwordCacheDir <- function(swordDir) {
  .riversatr_environment$swordDir <- swordDir
}

#' Get the SWORD cache directory
#'
#' @return Character path to configured SWORD cache directory, or `NULL` if unset.
#' @aliases setSwordCacheDir
#' @keywords internal
getSwordCacheDir <- function(swordDir) {
  .riversatr_environment$swordDir
}

#' Set active SWORD geopackage filepath
#'
#' @param swordFilepath Character path to a SWORD geopackage file.
#' @return Invisibly updates internal package state.
#' @keywords internal
#' @noRd
setSwordFilepath <- function(swordFilepath) {
  .riversatr_environment$swordFilepath <- swordFilepath
}

#' Get active SWORD geopackage filepath
#'
#' @return Character path to configured SWORD geopackage file, or `NULL`
#'   if unset.
#' @keywords internal
#' @noRd
getSwordFilepath <- function() {
  .riversatr_environment$swordFilepath
}


#' Set GRWL mask directory path
#'
#' @param grwlFilepath Character path to GRWL mask rasters directory.
#' @return Invisibly updates internal package state.
#' @keywords internal
#' @noRd
setGrwlFilepath <- function(grwlFilepath) {
  .riversatr_environment$grwlFilepath <- grwlFilepath
}

#' Get GRWL mask directory path
#'
#' Returns the configured GRWL mask directory path.
#'
#' @param grwlFilepath Unused argument retained by current implementation.
#' @return Character path to GRWL mask directory, or `NULL` if unset.
#' @keywords internal
#' @noRd
getGrwlFilepath <- function() {
  .riversatr_environment$grwlFilepath
}

#' Set GRWL tiles directory path
#'
#' @param grwlTilesFilepath Character path to GRWL tiles directory.
#' @return Invisibly updates internal package state.
#' @keywords internal
#' @noRd
setGrwlTilesFilepath <- function(grwlTilesFilepath) {
  .riversatr_environment$grwlTilesFilepath <- grwlTilesFilepath
}


#' Get GRWL tiles directory path
#'
#' @return Character path to GRWL tiles directory, or `NULL` if unset.
#' @keywords internal
#' @noRd
getGrwlTilesFilepath <- function() {
  .riversatr_environment$grwlTilesFilepath
}


#' Check whether GRWL data is available
#'
#' Convenience checker that verifies the configured GRWL mask directory exists.
#'
#' @param version GRWL version string. Currently only `"01.01"` is supported.
#' @return Logical scalar. `TRUE` when configured GRWL directory exists.
#' @examples
#' has_grwl()
has_grwl <- function(version = "01.01") {
  if (version != "01.01") stop("Only version = '01.01' currently supported, sorry.")
  dir.exists(getGrwlFilepath())
}

#' Convert Google Drive share URL to direct download URL
#'
#' Extracts the file ID from a shared Google Drive URL and returns a URL
#' suitable for direct use with [utils::download.file()].
#'
#' @param shared_url Character Google Drive sharing URL.
#' @return Character URL for direct download.
#' @keywords internal
#' @noRd
.gdrive_direct_download_url <- function(shared_url) {
  m <- regexec("/d/([^/]+)", shared_url)
  parts <- regmatches(shared_url, m)
  if (length(parts[[1]]) < 2) {
    stop("Could not parse Google Drive file ID from tiles URL.")
  }
  file_id <- parts[[1]][2]
  paste0("https://drive.google.com/uc?export=download&id=", file_id)
}

#' Set WBD geopackage filepath
#'
#' @param wbdFilepath Character path to `WBD_National_GPKG.gpkg`.
#' @return Invisibly updates internal package state.
#' @keywords internal
#' @noRd
setWbdFilepath <- function(wbdFilepath) {
  .riversatr_environment$wbdFilepath <- wbdFilepath
}

#' Get WBD geopackage filepath
#'
#' @return Character path to configured WBD geopackage file, or `NULL` if unset.
#' @keywords internal
#' @noRd
getWbdFilepath <- function() {
  .riversatr_environment$wbdFilepath
}

#' Check whether WBD data is available
#'
#' @return Logical scalar. `TRUE` when configured WBD file exists.
#' @keywords internal
has_wbd <- function() {
  wbd_fp <- getWbdFilepath()
  file.exists(wbd_fp)
}


#' Load RiverSAT project settings into package state
#'
#' Sets the active project directory and, when present, configures
#' `reaches.gpkg` as the active SWORD filepath.
#'
#' @param project_dir Character path to RiverSAT project directory.
#' @return Invisibly updates internal package state.
#' @export
loadRiversatProject <- function(project_dir) {
  .riversatr_environment$project_dir <- project_dir
  sword_filepath <- file.path(project_dir, "reaches.gpkg")
  if (file.exists(sword_filepath)) {
    setSwordFilepath(sword_filepath)
  }
}


#' Get active RiverSAT project directory
#'
#' @param project_dir Unused argument retained by current implementation.
#' @return Character path to active project directory, or `NULL`.
#' @export
getRiversatProject <- function() {
  .riversatr_environment$project_dir
}

# EXPORTS -----

#' Get package cache path
#'
#' Returns the base cache directory used to store SWORD, GRWL, and WBD
#' datasets.
#'
#' @return Character path used for cache.
#' @examples
#' getCachePath()
#' @export
getCachePath <- function() {
  .riversatr_environment$cachePath
}


#' Set package cache path and derived dataset defaults
#'
#' Updates package cache base path and resets derived default locations for:
#' * SWORD GPKG directory (`SWORD_v17/gpkg`)
#' * GRWL mask directory (`GRWL_v01.01/GRWL_mask_V01.01`)
#' * GRWL tiles directory (`GRWL_v01.01/GRWL_tiles_v01.01`)
#' * WBD directory (`WBD_National_GPKG`)
#'
#' @param cachePath Character base directory for cached datasets.
#' @return Invisibly updates internal package state.
#' @examples
#' setCachePath(tempdir())
#' @export
setCachePath <- function(cachePath) {
  .riversatr_environment$cachePath <- cachePath
  setSwordCacheDir(file.path(cachePath, "SWORD_v17", "gpkg"))
  setGrwlFilepath(file.path(cachePath, "GRWL_v01.01", "GRWL_mask_V01.01"))
  setGrwlTilesFilepath(file.path(cachePath, "GRWL_v01.01", "GRWL_tiles_v01.01"))
  setWbdFilepath(file.path(cachePath, "WBD_National_GPKG"))
}

# STAT UTILS -----

#' Draw multivariate normal samples from precision matrix
#'
#' Samples from a Gaussian with mean `mu` and precision matrix `prec`
#' (equivalently covariance `solve(prec)`) using sparse Cholesky
#' factorization in `Matrix`.
#'
#' @param mu Numeric mean vector.
#' @param prec Precision matrix (typically sparse `Matrix`).
#' @param n.sims Number of simulation draws.
#' @return Numeric matrix with one column per simulation draw.
#' @examples
#' \dontrun{
#' library(Matrix)
#' Q <- Diagonal(3)
#' rmvnorm_prec(mu = c(0, 0, 0), prec = Q, n.sims = 2)
#' }
#' @export
rmvnorm_prec <-
  function( mu, # estimated fixed and random effects
            prec, # estimated joint precision
            n.sims = 1) {

    require(Matrix)
    # Simulate values
    z0 = matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
    # Q = t(P) * L * t(L) * P
    L = Cholesky(prec, super=TRUE)
    # Calcualte t(P) * solve(t(L)) * z0 in two steps
    z = solve(L, z0, system = "Lt") # z = Lt^-1 * z
    z = solve(L, z, system = "Pt") # z = Pt    * z
    return(mu + as.matrix(z))
  }


#' Sample latent process from TMB/RTMB joint posterior approximation
#'
#' Builds a joint mean vector from `sdr$par.fixed` and `sdr$par.random`,
#' samples from the corresponding Gaussian approximation via
#' [rmvnorm_prec()], then reconstructs node-time latent means from
#' components named `mu`, `w_n`, and `z_n_t`.
#'
#' @param sdr `sdreport`-like object containing `par.fixed`, `par.random`,
#'   and `jointPrecision`.
#' @param n.sims Number of posterior draws.
#' @return Numeric matrix of sampled latent means indexed by node-time where
#'   available.
#' @export
sample_post <- function(sdr, n.sims = 1) {
  # TO-DO: add checks or make more flexibility for shape and names of sdr rows
  mu <- c(sdr$par.fixed, sdr$par.random)
  prec <- sdr$jointPrecision
  s <- rmvnorm_prec(mu, prec, n.sims = n.sims)
  w_n <- s[rownames(s) == 'w_n', ]
  z_n_t <- s[rownames(s) == 'z_n_t', ]
  w_n_t <- do.call(rbind, rep(list(w_n), nrow(z_n_t)/nrow(w_n)))
  mu_n_t <- do.call(rbind, rep(list(s['mu', ]), nrow(z_n_t)))
  mu_n_t + w_n_t + z_n_t
}


#' Build RAM path matrix from SEM-style path specification
#'
#' Converts model path syntax (via `sem:::parse.path`) into RAM-form columns:
#' `heads`, `to`, `from`, `parameter`, and `start`.
#'
#' @param model Matrix or data frame with path strings in column 1, parameter
#'   names in column 2, and start values in column 3.
#' @param vars Character vector of variable names used by the model.
#' @return Data frame with RAM columns:
#'   `heads`, `to`, `from`, `parameter`, `start`.
#' @references https://github.com/spacetime-ecologist/spacetime-ecologists-RTMB/blob/781d23ba00cb55321b5b9dd3580e675973952c83/chp4/build_ram.R
#' @export
build_ram <-
  function(model,
           vars) {
    # Extract info
    vars <- sapply(vars, FUN = function(char) {
      gsub("-", "", gsub(" ", "", char))
    })
    n.paths <- nrow(model)
    par.names <- model[, 2]
    startvalues <- model[, 3]

    # EXCERPT FROM `getAnywhere("sem.semmod")`
    heads <- from <- to <- rep(0, n.paths)
    for (p in 1:n.paths) {
      path <- sem:::parse.path(model[p, 1])
      heads[p] <- abs(path$direction)
      to[p] <- path$second
      from[p] <- path$first
      if (path$direction == -1) {
        to[p] <- path$first
        from[p] <- path$second
      }
    }

    # Check for errors
    missing_vars <- setdiff(c(from, to), vars)
    if (length(missing_vars) > 0) stop("Check `build_ram`:", paste0(missing_vars, sep = ", "))

    # Constrct RAM
    RAM <- data.frame(matrix(0, nrow = n.paths, ncol = 5))
    pars <- na.omit(unique(par.names))
    RAM[, 1] <- heads
    RAM[, 2] <- apply(outer(vars, to, "=="), 2, which)
    RAM[, 3] <- apply(outer(vars, from, "=="), 2, which)
    par.nos <- apply(outer(pars, par.names, "=="), 2, which)
    if (length(par.nos) > 0) {
      RAM[, 4] <- unlist(lapply(par.nos, function(x) {
        if (length(x) == 0) {
          0
        } else {
          x
        }
      }))
    }
    RAM[, 5] <- startvalues
    colnames(RAM) <- c("heads", "to", "from", "parameter", "start")
    return(RAM)
  }

