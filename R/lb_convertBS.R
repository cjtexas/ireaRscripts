#' Title
#'
#' @param in_files
#' @param in_dates
#' @param out_rast
#' @param shp_in
#'
#' @return
#' @export
#'

lb_convertBS = function(in_files,in_dates, out_rast = out_rast, shp_in){
  out_rast_temp = paste0(out_rast,'.vrt')
  gdalbuildvrt(in_files, out_rast_temp, separate = T, overwrite = T, allow_projection_difference = T)
  in_rts = rts(brick(out_rast_temp), time = in_dates)     # create rasterstack timeseries object
  values(in_rts@raster) = as.integer(100*log10( values(in_rts@raster)))
  lb_writeenvits(in_raster = in_rts@raster, in_dates = in_dates, out_file = out_rast, dtype = 'INT2S')
  gc()
}
