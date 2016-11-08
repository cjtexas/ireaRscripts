#' extent_reproj
#' @description function to convert the extent of a spatial object to a different projection
#'
#' @param ext_in   "Extent" input extent
#' @param in_proj  "proj4string" projection of the input extent
#' @param out_proj "proj4string" desired projection for the output extent
#'
#' @return reprojected extent (in out_proj" projection)
#' @export
#'
#' @importFrom raster extent
#' @importFrom sp CRS spTransform SpatialPoints
#'
#' @examples
#'

extent_reproj <- function(ext_in, in_proj, out_proj){

  if (in_proj != out_proj) {   # If rast and shape proj differ, reproject the shape extent

    out_ext_rep <- extent(spTransform(SpatialPoints(
      data.frame(x = c(ext_in@xmin,ext_in@xmax), y = c(ext_in@ymax, ext_in@ymin)),
      proj4string = CRS(in_proj)),out_proj))
  } else {
    out_ext_rep <- ext_in
  }
  out_ext_rep
}

