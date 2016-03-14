#' Title
#'
#' @param in_files
#' @param in_dates
#' @param shp
#' @param id_field
#' @param BS
#' @param format
#' @param avg
#' @param sd
#' @param minmax
#' @param cvar
#' @param out_rast
#' @param start_date
#' @param end_date
#' @param verbose
#' @param na.rm
#'
#' @return
#' @export
#'
#' @import data.table raster rgdal rgeos
#'
#' @examples

lb_gettsdata = function(in_files, in_dates, shp,id_field, buffer = NULL, BS = F, format = 'ENVI', avg = T, sd = F, minmax = F, cvar = F ,out_rast = NULL, start_date = NULL, end_date = NULL,verbose = T, na.rm = T){
  # beginCluster()

  shp_ok = spTransform(shp, proj4string(raster(in_files[1])))  # Transform shapefile in proj of raster

  if (length(buffer == 1)) { shp_ok = gBuffer(shp_ok, byid=T, id=shp_ok@data[,eval(id_field)],width=buffer)}
  order = order(in_dates)     # sort dates and files according to acquisition date
  in_files = in_files[order]
  in_dates = in_dates[order]

  if (length(out_rast) == 0) {out_rast = paste0(dirname(in_files[1]), 'temp_vrt.vrt')}   # If no raster ts output, create a temporary vrt filename to be then removed

  gdalbuildvrt(in_files, out_rast, separate = T,       # create temporary cropped ts file (vrt)
               te = c(extent(shp_ok)@xmin,extent(shp_ok)@ymin,extent(shp_ok)@xmax,extent(shp_ok)@ymax), overwrite = T, allow_projection_difference = T)
  in_rts = rts(brick(out_rast), time = in_dates)     # create rasterstack timeseries object

  if (BS == T) {        # If backscatter series, compute logarithm of values
    values(in_rts@raster) = 10*log10( values(in_rts@raster))
    if (length(out_rast) ==1) {
      if (format == 'ENVI') {    # if output format envi, update the header with band names and wavelengths (doys)
        lb_write_envits(in_raster = in_rts@raster, in_dates = in_dates, out_file = out_rast)
      } else {writeRaster(in_rts@raster, filename = out_rast,overwrite = T, format = format)}
    }
  } else {  # If not backscatter series, keep the original values of the time series

    if (length(out_rast) ==1) {
      if (format == 'ENVI') {    # if output format envi, update the header with band names and wavelengths (doys)
        lb_write_envits(in_raster = in_rts@raster, in_dates = in_dates, out_file = out_rast)
      } else {writeRaster(in_rts@raster, filename = out_rast,overwrite = T, format = format)}
    }
  }

  # From here onwards, take the raster time series and compute the zonal statistics

  zone_raster = rasterize(shp_ok, in_rts@raster[[1]], id_field)    # rasterize the shapefile
  zones = getValues(zone_raster)   # get zones values
  ok_zones = which(is.finite(zones))  # find good zones
  zones = zones [ok_zones]

  if (length(start_date == 1) & length(end_date) ==1) {   # if dates selected, find the bands within the selected range
    sel_indexes = which(index(in_rts) >= start_date & index(in_rts) <= end_date)
  } else {
    sel_indexes = seq(1:length(index(in_rts)))
  }
  dim_out =  matrix(nrow = length(sel_indexes), ncol = length(unique(zones)))
  # prepare matrixes to contain the results of avg and stdev
  if (avg) {ts_avg = dim_out}
  if (sd) {ts_sd = dim_out}
  if (minmax) {ts_min = dim_out ; ts_min = dim_out}
  if (cvar) {ts_cvar = dim_out}
  # browser()
  for (f in 1:length(sel_indexes)) {    # extract values for the different zones and dates
    if (verbose == T) {print(paste0('Extracting data from date: ', index(in_rts)[sel_indexes[f]]))}
    value = getValues(in_rts@raster[[sel_indexes[f]]]) [ok_zones]
    rDT <- data.table(value, zones)
    setkey(rDT, zones)
    if (avg == TRUE) {
      ts_avg[f,] = rDT[, lapply(.SD,mean, na.rm = na.rm), by=zones]$value
    }
    if (sd) {ts_sd[f,] = rDT[, lapply(.SD, sd, na.rm = na.rm), by=zones]$value}
    if (minmax) {
      ts_min[f,] = rDT[, lapply(.SD, min, na.rm = na.rm), by=zones]$value
      ts_max[f,] = rDT[, lapply(.SD, max, na.rm = na.rm), by=zones]$value
    }
    if (cvar) {ts_cvar[f,] = ts_sd[f,]/ts_avg[f,]}
  }

  ts_sd = as.data.frame(ts_sd)
  if (avg) { ts_avg = as.data.frame(ts_avg)    # convert to data frame
  names(ts_avg) = unique(rDT$zones) # put the "id_field" values as names for the different columns
  }
  if (sd) { ts_sd= as.data.frame(ts_sd)    # convert to data frame
  names(ts_sd) = unique(rDT$zones) # put the "id_field" values as names for the different columns
  }

  if (minmax) { ts_min = as.data.frame(ts_min) ; ts_max = as.data.frame(ts_max)  ; names(ts_min) = names(ts_max) = unique(rDT$zones) }
  if (cvar) {ts_cvar = as.data.frame(ts_cvar) ; names(ts_cvar) =  unique(rDT$zones) }
  out_list = list()

  if(avg) {out_list[['avg']] = ts_avg}
  if(sd) {out_list[['sd']] = ts_sd}
  if(minmax) {out_list[['min']] = ts_min  ;  out_list[['max']] = ts_max}
  if(cvar) {out_list[['cvar']] = ts_cvar}

  if (length(out_rast) == 0) {file.remove(paste0(file.path(in_files[1], 'temp_vrt.vrt')))} else {file.remove(out_rast)}   # delete temporary vrt file
  return(out_list)  # return results
}
