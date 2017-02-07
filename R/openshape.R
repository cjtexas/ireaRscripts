#' openshape
#' @description function for easily opening a ESRI shapefile by simply specifying its filename
#'
#' @param shp_file filename of ESRI shapefile
#'
#' @return Spatial* object (SpatialPolygons, SpatialPoints, eccetera)
#' @export
#' @importFrom rgdal readOGR
#' @importFrom tools file_path_sans_ext
#' @examples
#' \dontrun{
#'  shp_file = 'D:/temp/polygons.shp'
#'  shp = lb_openshape(shp_file)
#'}

openshape = function(shp_file){

  basename = basename(file_path_sans_ext(shp_file))
  dirname = dirname(shp_file)
  shp = readOGR(dirname, basename, verbose = FALSE)
  return(shp)

}
