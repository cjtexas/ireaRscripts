#' Title
#'
#' @param out_obj
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
lb_writeshape = function(out_obj, filename){

  dsn = dirname(filename)
  layer = basename(file_path_sans_ext(filename))
  writeOGR(obj = out_obj, dsn = dsn,
           layer = layer,
           driver = "ESRI Shapefile", overwrite_layer = T)

}
