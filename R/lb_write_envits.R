
#' lb_write_envits
#' @details Function to save an "R" multitemporal raster object to an ENVI multiband file (BSQ)
#' Bandnames and wavelengths of the output ENVI file are set on the basis of the acquisition dates .
#' In paricular, wl takes the value of the acquisition doy, computed starting from 1st of January of the
#' minimum year in the time serties
#'
#' @param in_raster Input "R" raster object
#' @param in_dates Dates corresponding to the different dates of acquisition (as "Dates" array or numeric array of doys)
#' @param out_file Output file name
#' @param multiband logical. if T
#'
#' @return NULL
#' @export
#'
#' @import raster
#'
#' @examples
lb_write_envits = function(in_raster, in_dates, out_file) {

  writeRaster(in_raster, filename = out_file,overwrite = T, format = 'ENVI')
  bandnames = paste(basename(file_path_sans_ext(out_file)),in_dates, sep = '_')
  if(date_type == "date") {
    wl = as.numeric(lb_datetodoy(in_dates)+365*(year(in_dates)-min(year(in_dates))))
    }
  hdrfile = paste0(file_path_sans_ext(out_file),'.hdr')
  write(paste("Band Names = {", paste (bandnames, collapse = ', '), "}", sep=""),file=hdrfile,append=TRUE)
  write(paste("wavelength = {", paste (wl, collapse = ', '), "}", sep=""),file=hdrfile,append=TRUE)

}
