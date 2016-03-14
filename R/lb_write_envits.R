
#' Title
#'
#' @param in_raster a
#' @param in_dates b
#' @param out_file b
#' @param date_type n
#' @param multiband n
#'
#' @return
#' @export
#'
#' @import raster
#'
#' @examples
lb_write_envits = function(in_raster = in_raster, in_dates = in_dates, out_file = out_file,date_type = 'date',  multiband = F) {

  writeRaster(in_raster, filename = out_file,overwrite = T, format = 'ENVI')
  bandnames = paste(basename(file_path_sans_ext(out_file)),in_dates, sep = '_')
  if(date_type == "date") {wl = as.numeric(strftime(in_dates, format = '%j'))}
  hdrfile = paste0(file_path_sans_ext(out_file),'.hdr')
  write(paste("Band Names = {", paste (bandnames, collapse = ', '), "}", sep=""),file=hdrfile,append=TRUE)
  write(paste("wavelength = {", paste (wl, collapse = ', '), "}", sep=""),file=hdrfile,append=TRUE)

}
