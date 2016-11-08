#' Title
#'
#' @param in_rts
#' @param sp_object
#' @param start_date
#' @param end_date
#' @param id_field
#' @param FUN
#' @param out_format
#' @param small
#' @param small_method
#' @param na.rm
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#'
lb_fastzonal = function (in_rts, sp_object, start_date = NULL, end_date = NULL,
                         id_field = NULL, FUN = "mean", out_format = "xts", small = TRUE,
                         small_method = "centroids", na.rm = TRUE, verbose = FALSE)
{
  if (!class(in_rts) %in% c("RasterStack", "RasterBrick")) {
    stop("Input is not a RasterStack or RasterBrick object")
  }
  if (!class(getZ(in_rts)) == "Date") {
    stop("Input doesn't contain valid dates in its 'Z' attribute !")
  }
  if (length(start_date) == 0) {
    start_date <- min(getZ(in_rts))
    if (verbose)
      message("Starting date not provided - Using the first date in the stack")
  }
  if (length(end_date) == 0) {
    end_date <- max(getZ(in_rts))
    if (verbose)
      message("Ending date not provided - Using the last date in the stack")
  }
  if (!class(start_date) %in% c("Date", "POSIXct", "POSIXlt")) {
    start_date = try(as.Date(start_date), silent = TRUE)
    if (class(start_date) == "try-error") {
      stop("start_date is not a Date object or string cohercible to date")
    }
  }
  if (!class(end_date) %in% c("Date", "POSIXct", "POSIXlt")) {
    end_date = try(as.Date(end_date), silent = TRUE)
    if (class(end_date) == "try-error") {
      stop("end_date is not a Date object or string cohercible to date")
    }
  }
  if (start_date > end_date) {
    stop("start_date larger than end_date")
  }
  if (!small_method %in% c("centroids", "full")) {
    warning("Unknown 'small_method' value - resetting to 'centroids'")
  }
  if (!out_format %in% c("xts", "dframe")) {
    if (verbose)
      message("Unknown 'out_format' value - resetting to 'xts'")
    out_format = "xts"
  }
  if (!class(sp_object) %in% c("SpatialPolygonsDataFrame",
                               "SpatialPolygons", "SpatialPointsDataFrame", "SpatialPoints",
                               "SpatialLines", "SpatialLinesDataFrame")) {
    if (class(sp_object) == "character") {
      sp_object <- try(readOGR(dirname(sp_object), basename(file_path_sans_ext(sp_object))))
      if (class(sp_object) == "try-error") {
        stop("sp_object is not a valid Spatial object or Shapefile")
      }
    }
  }
  if (length(id_field) != 0) {
    if (!id_field %in% names(sp_object)) {
      warning("Invalid 'id_field' value - names of output columns will be the record number of the shapefile feature")
      id_field <- NULL
    }
  }
  dates <- getZ(in_rts)
  sel_dates <- which(dates >= start_date & dates <= end_date)
  # browser()
  if (length(sel_dates) > 0) {
    if (proj4string(sp_object) != proj4string(in_rts)) {
      sp_object <- spTransform(sp_object, CRS(proj4string(in_rts[[1]])))
    }
    sp_object@data$mdxtnq = seq(1:length(sp_object@data[,1]))
    shape = crop(sp_object, extent(in_rts[[1]]))
    if (!isTRUE(all.equal(extent(shape),(extent(sp_object)), scale = 100))) {
      warning("Some features of the spatial object are outside or partially outside\n the extent of the input RasterStack ! Output for features outside rasterstack extent\n            will be set to NODATA. Outputs for features only partially inside will be retrieved\n            using only the available pixels !")
      if (!setequal(sp_object$mdxtnq, shape$mdxtnq)){

        outside_feat = setdiff(sp_object$mdxtnq, shape$mdxtnq)
      }
    }
    if (class(shape) %in% c("SpatialPointsDataFrame", "SpatialPoints",
                            "SpatialLines", "SpatialLinesDataFrame")) {

      ts <- matrix(nrow = length(sel_dates), ncol = length(shape[,1]))
      for (f in 1:length(sel_dates)) {
        if (verbose == TRUE) {
          print(paste0("Extracting data from date: ",
                       dates[sel_dates[f]]))
        }
        ts[f, ] <- extract(in_rts[[sel_dates[f]]], shape,
                           fun = FUN)
      }
      ts <- as.data.frame(ts)
      if (length(id_field) == 1) {
        feat_names <- as.character(shape@data[, eval(id_field)])
        names(ts) <- c(feat_names)
      }
      else {
        names(ts) <- 1:length(shape[, 1])
      }
      if (out_format == "dframe") {
        ts <- cbind(date = dates[sel_dates], ts)
      }
    }
    else {
      if (verbose)
        (message("Rasterizing shape"))
      if (verbose) {
        message("Writing temporary shapefile")
      }
      tempshape = tempfile(tmpdir = tempdir(), fileext = ".shp")
      writeOGR(shape, dsn = dirname(tempshape), layer = basename(file_path_sans_ext(tempshape)),
               driver = "ESRI Shapefile", overwrite_layer = TRUE,
               verbose = FALSE)
      if (verbose) {
        message("Writing temporary rasterized shapefile")
      }
      tempraster = tempfile(tmpdir = tempdir(), fileext = ".tiff")
      ext_conv = function(x) {
        ext = extent(x)
        c(ext[1], ext[3], ext[2], ext[4])
      }
      if (max(shape@data$mdxtnq) <= 255) {
        ot = "Byte"
      }
      else {
        if (max(shape@data$mdxtnq) <= 65536) {
          ot = "Int16"
        }
        else {
          ot = "Int32"
        }
      }
      gdal_rasterize(tempshape, tempraster, tr = raster::res(in_rts),
                     te = ext_conv(in_rts[[1]]), a = "mdxtnq", ot = ot)
      zone_raster <- raster(tempraster)
      zones <- getValues(zone_raster)
      ok_zones <- which(is.finite(zones) & zones != 0)
      zones <- zones[ok_zones]
      ncols <- length(unique(zones))
      ts <- matrix(nrow = length(sel_dates), ncol = ncols)

      for (f in 1:length(sel_dates)) {
        if (verbose == TRUE) {
          message(paste0("Extracting data from date: ",
                         dates[sel_dates[f]]))
        }
        # browser()
        value <- getValues(in_rts[[sel_dates[f]]])[ok_zones]
        rDT <- data.table(value, zones)
        setkey(rDT, zones)
        # browser()
        ts[f, 1:ncols] <- rDT[, lapply(.SD, match.fun(FUN),
                                       na.rm = na.rm), by = zones]$value
      }
      ts <- as.data.frame(ts)
      if (length(id_field) == 1) {
        feat_names <- as.character(sp_object@data[,
                                                  eval(id_field)])[sort(unique(zones))]
        names(ts) <- feat_names
      }
      else {
        feat_names <- as.character(shape@data[, "mdxtnq"])[sort(unique(zones))]
        names(ts) <- feat_names
      }
      if (out_format == "dframe") {
        ts <- cbind(date = dates[sel_dates], ts)
      }

      if (small & ncols != length(shape@data[, 1])) {
        if (length(id_field) == 1) {
          miss_feat <- setdiff(as.character(shape@data[,"mdxtnq"]),names(ts))
          pos_missing <- which(as.character(shape@data[,"mdxtnq"]) %in% miss_feat)
        }
        else {
          pos_missing <- miss_feat <- which(as.character(shape@data[,"mdxtnq"]) %in% miss_feat)
        }
        shpsub <- shape[pos_missing, ]
        ts_mis <- matrix(nrow = length(sel_dates), ncol = length(pos_missing))
        for (f in 1:length(sel_dates)) {
          if (verbose == TRUE) {
            print(paste0("Extracting data from date: ",
                         dates[sel_dates[f]]))
          }
          if (small_method == "centroids") {
            ts_mis[f, ] <- extract(in_rts[[sel_dates[f]]],
                                   coordinates(shpsub), fun = mean)
          }
          else {
            ts_mis[f, ] <- extract(in_rts[[sel_dates[f]]],
                                   shpsub, fun = mean)
          }
        }
        colnames(ts_mis) <- miss_feat
        ts <- cbind(ts, ts_mis)
      }
      file.remove(tempraster)
      file.remove(tempshape)
    }
    if (exists("outside_feat")) {
      if (length(id_field) == 1) {
        feat_names_outside = as.character(sp_object@data[,
                                                         eval(id_field)])[outside_feat]
      }
      else {
        feat_names_outside = as.character(sp_object@data[,
                                                         "mdxtnq"])[outside_feat]
      }

      ts_outside = matrix(nrow = length(sel_dates), ncol = length(feat_names_outside))
      ts_outside = data.frame(ts_outside)
      names(ts_outside) = feat_names_outside
      ts = cbind(ts, ts_outside)
      if (length(id_field) == 1) {
        sortindex = match(sp_object@data[,eval(id_field)], names(ts))
      } else {
        sortindex = match(sp_object@data[,"mdxtnq"], names(ts))
      }
      ts = ts[, c(1,sortindex)]
    }
    if (out_format == "xts") {
      ts <- as.xts(ts, order.by = dates[sel_dates])
    }
    return(ts)
  }
  else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
}
