#' raster_reclassifier
#'
#' @param in_rast
#' @param in_hash
#'
#' @return
#' @export
#'
#' @importFrom raster reclassify
#'
#' @examples
raster_reclassifier <- function(in_rast, in_hash, out_rast){

  # reformat the hash table to the format wanted by "reclassify"
  rclmat <- in_hash
  datatype = pippo
  raster::reclassify(in_rast, rclmat, filename = out_rast,
                     include.lowest = FALSE, overwrite = TRUE,
                     datatype = datatype)



}
