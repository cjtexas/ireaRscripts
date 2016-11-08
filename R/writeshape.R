#' writeshape
#' @description function for easily opening a ESRI shapefile by simply specifying its filename
#'
#' @param out_obj  spatial object to save as ESRI shapefile
#' @param out_file filename of output ESRI shapefile
#' @param overw    logical. if TRUE, existing files are overwritten (defaults to FALSE)
#'
#' @return NULL - out_obj is ssaved in shp_file
#' @export
#' @importFrom rgdal readOGR
#' @importFrom tools file_path_sans_ext
#' @examples
#' # build a spatialpoints data frame
#' pts = cbind(1:5, 1:5)
#' dimnames(pts)[[1]] = letters[1:5]
#' df = data.frame(a = 1:5)
#' row.names(df) = letters[5:1]
#' points <- SpatialPointsDataFrame(pts, df, match.ID = TRUE) # don't warn
#'
#' Save it to a shape file
#' out_file  = tempfile(pattern = "test", tmpdir = tempdir(), fileext = ".shp")
#' mysp_object = points
#' writeshape(mysp_object, out_file, overw = TRUE)


writeshape = function(out_obj, out_file, overw = FALSE){

  dsn = dirname(out_file)
  layer = basename(file_path_sans_ext(out_file))
  writeOGR(obj = out_obj, dsn = dsn,
           layer = layer,
           driver = "ESRI Shapefile", overwrite_layer = overw)

}

