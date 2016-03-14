#' Title
#'
#' @param in_raster
#' @param thresholds
#' @param ... any other parameter passable to "calibrate"
#'
#' @return raster object obtained applying a fuzzy "calibration" on the input according to specified thresholds.
#' See help for "calibrate" function in QCA library for details
#' @export
#'
#' @import QCA
#'
#' @examples

lb_fuzzyconversion = function (in_raster = in_raster, thresholds = thresholds, ...) {
  require('QCA')
  out_raster = stack(in_raster)
  values(out_raster) = calibrate(values(out_raster), thresholds = thresholds, type = "fuzzy")
  out_raster
}

