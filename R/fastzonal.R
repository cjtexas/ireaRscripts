#' fastzonal
#'
#' @param in_rts
#' @param zone_object
#' @param start_date
#' @param end_date
#' @param id_field
#' @param FUN
#' @param out_format
#' @param small
#' @param small_method
#' @param na.rm
#' @param verbose
#' @param start_band
#' @param end_band
#' @param maxchunk 
#'
#' @importFrom xts as.xts
#' @importFrom rgdal writeOGR readOGR
#' @importFrom sp proj4string spTransform CRS
#' @importFrom tools file_path_sans_ext
#' @importFrom raster getValues crop extent getZ extract rasterize res
#' @importFrom tools file_path_sans_ext
#' @importFrom gdalUtils gdal_rasterize
#' @return
#' @export
#'
#' @examples
#'
fastzonal = function(in_rts,
                     zone_object,
                     start_date = NULL,
                     end_date   = NULL,
                     start_band = NULL,
                     end_band   = NULL,
                     id_field   = NULL,
                     FUN        = "mean",
                     out_format = "xts",
                     small      = TRUE,
                     small_method = "centroids",
                     na.rm      = TRUE,
                     verbose    = FALSE,
                     maxchunk   = 30E6)
{
  
  ras_type   <-  check_spatype(in_rts)
  zone_type <-  check_spatype(zone_object)
  if (ras_type %in% "rastobject") {
    stop("Input in_rts is not a RasterStack or RasterBrick object")
  }
  if (zone_type == "none") {
    stop("Input zone_object is not a valid spatial file or object !")
  }
  
  if (!class(getZ(in_rts)) == "Date") {
    message("Input doesn't contain valid dates in its 'Z' attribute\nBand numbers will be used instead on the outputs")
    ts_check = FALSE
  } else {
    dates <- getZ(in_rts)
    ts_check = TRUE
  }
  
  if (is.null(start_date) & is.null(start_band)) {
    start_band <- 1
    if (verbose) {message("Starting date/Starting band not provided - Using the first layer in the stack")}
  }
  
  if (is.null(end_date) & is.null(end_band)) {
    end_band <- nlayers(in_rts)
    if (verbose) {message("Starting date/Starting band not provided - Using the last layer in the stack")}
  }
  if (!class(start_date) %in% c("Date", "POSIXct", "POSIXlt")) {
    start_date = try(as.Date(start_date), silent = TRUE)
    if (class(start_date) == "try-error") {
      warning("start_date is not a Date object or string cohercible to date - it will be ignored")
      start_date <- 1
    }
  }
  if (!class(end_date) %in% c("Date", "POSIXct", "POSIXlt")) {
    end_date = try(as.Date(end_date), silent = TRUE)
    if (class(end_date) == "try-error") {
      warning("end_date is not a Date object or string cohercible to date - it will be ignored")
      end_date <- nlayers(in_rts)
    }
  }
  
  if (!class(start_band) == "numeric") { stop("start_band is not numeric") }
  
  if (!class(end_band) == "numeric") { stop("end_band is not numeric")}
  
  
  # if (start_date > end_date) {
  #   stop("start_date larger than end_date")
  # }
  if (!small_method %in% c("centroids", "full")) {
    warning("Unknown 'small_method' value - resetting to 'centroids'")
  }
  if (!out_format %in% c("xts", "dframe")) {
    if (verbose)
      message("Unknown 'out_format' value - resetting to 'xts'")
    out_format = "xts"
  }
  
  # check which kind of object was passed in input: Spatial object, raster file , shape name or raster name
  
  if (length(id_field) != 0) {
    if (!id_field %in% names(zone_object)) {
      warning(
        "Invalid 'id_field' value - names of output columns will be the record number of the shapefile feature"
      )
      id_field <- NULL
    }
  }
  
  if (!ts_check) {
    dates <- seq(1, nlayers(in_rts), 1)
  }
  
  sel_dates <- which(dates >= start_date & dates <= end_date)
  
  if (length(sel_dates) > 0) {
    
    if (check_spatype(zone_object) %in% c("spfile", "spobject")) {
      
      if (check_spatype(zone_object) == "spfile") {zone_object <- openshape(zone_object)}
      
      if (proj4string(zone_object) != proj4string(in_rts)) {
        zone_object <- spTransform(zone_object, CRS(proj4string(in_rts[[1]])))
      }
      
      zone_object@data$mdxtnq = seq(1:length(zone_object@data[, 1]))
      zone_cropped = crop(zone_object, extent(in_rts[[1]]))
      
      if (!isTRUE(all.equal(extent(zone_cropped), (extent(zone_object)), scale = 100))) {
        warning(
          "Some features of the spatial object are outside or partially outside\n the extent of the input RasterStack !
          Output for features outside rasterstack extent\n will be set to NODATA. Outputs for features only partially inside\n
          will be retrieved\n using only the available pixels !"
        )
        if (!setequal(zone_object$mdxtnq, zone_cropped$mdxtnq)) {
          outside_feat = setdiff(zone_object$mdxtnq, zone_cropped$mdxtnq)
        }
      }
      
      if (class(zone_cropped) %in% c("SpatialPointsDataFrame","SpatialPoints","SpatialLines", 
                                     "SpatialLinesDataFrame")) {
        if (verbose) { message("On point and lines shapefiles, the standard `extract` function is used. 
              This could be slow !")}
        
        ts <- matrix(nrow = length(sel_dates), ncol = length(zone_cropped[, 1]))
        
        for (f in 1:length(sel_dates)) {
          if (verbose == TRUE) {
            message(paste0("Extracting data from ", ifelse(ts_check, "date: ", "band: "),
                           dates[sel_dates[f]]))
          }
          ts[f,] <- extract(in_rts[[sel_dates[f]]], zone_cropped, fun = FUN)
        }
        ts <- as.data.frame(ts)
        if (length(id_field) == 1) {
          all_feats <- as.character(zone_cropped@data[, eval(id_field)])
          names(ts) <- c(all_feats)
        }
        else {
          names(ts) <- 1:length(zone_cropped[, 1])
          all_feats <- as.character(names(ts))
        }
        if (out_format == "dframe") {
          ts <- cbind(date = dates[sel_dates], ts)
        }
      } else {   # On polygon shapes, rasterize the polygons
        
        if (verbose) { message("Rasterizing shape")}
        if (verbose) { message("Writing temporary shapefile")}
        
        tempshape = tempfile(tmpdir = tempdir(), fileext = ".shp")
        writeOGR(zone_cropped, dsn = dirname(tempshape),layer = basename(file_path_sans_ext(tempshape)),
                 driver = "ESRI Shapefile", overwrite_layer = TRUE,verbose = FALSE)
        
        if (verbose) {(message("Writing temporary rasterized shapefile"))}
        tempraster = tempfile(tmpdir = tempdir(), fileext = ".tiff")
        
        if (max(zone_cropped@data$mdxtnq) <= 255) {
          ot = "Byte"
        } else {
          if (max(zone_cropped@data$mdxtnq) <= 65536) {
            ot = "Int16"
          } else {
            ot = "Int32"
          }
        }
        gdal_rasterize(tempshape, tempraster, tr = raster::res(in_rts), te = extent(in_rts)[c(1, 3, 2, 4)], a = "mdxtnq", ot = ot)
        zone_object <- raster(tempraster)
      }
    } else {
      
      if (check_spatype(zone_object) == "rastfile") { zone_object = raster(zone_object)}
      zone_object = crop(zone_object, extent(in_rts[[1]]))
      
    }
    browser()
    n_cells   <- nrow(zone_object) * ncol(zone_object)
    ncols <- ncol(zone_object)
    n_chunks  <- floor(n_cells / maxchunk)
    full_data <-  list()
    
    for (f in 1:length(sel_dates)) {
      if (verbose == TRUE) {
        message(paste0("Extracting data from date: ",
                       dates[sel_dates[f]]))
      }
      
      if (n_chunks > 1) {
      for (chunk in 1:n_chunks) {
        startrow <- (chunk - 1) *  ceiling(nrow(zone_object) / n_chunks) + 1
        nrows <- ifelse(chunk != n_chunks, floor(nrow(zone_object) / n_chunks),
                                           nrow(zone_object) - startrow)
        message(chunk, " ", startrow, " ",  startrow + nrows)
        full_data[[chunk]] <-
          data.table(
            value = getValues(in_rts[[sel_dates[f]]], startrow, nrows),
            zones = getValues(zone_object, startrow, nrows)
          )
        #gc()
      }
      full_data <- rbindlist(full_data)
      
      } else {
        full_data <- data.table(
          value = getValues(in_rts[[sel_dates[f]]]),
          zones = getValues(zone_object)
        )
      }
      #gc()
      setkey(full_data, "zones")
      #gc()
      
      if (f == 1) {
        zones <- unique(full_data)$zones
        
        ts <-
          matrix(nrow = length(sel_dates), ncol = length(zones))
      }
      # browser()
      ts[f,] <- full_data[, lapply(.SD, match.fun(FUN),
                                   na.rm = na.rm), by = zones]$value
    }
    # browser()
    ts <- as.data.frame(ts)
    
    if (zone_type == "spobject") {
      if (length(id_field) == 1) {
        feat_names <- as.character(zone_object@data[,
                                                    eval(id_field)])[sort(unique(zones))]
        names(ts) <- feat_names
      } else {
        feat_names <-
          as.character(zone_cropped@data[, "mdxtnq"])[sort(unique(zones))]
        names(ts) <- feat_names
      }
    } else {
      feat_names <- as.character(sort(unique(zones)))
      names(ts) <- feat_names
    }
    if (out_format == "dframe") {
      ts <- cbind(date = dates[sel_dates], ts)
    }
    
    if (zone_type %in% c("spobject", "spfile")) {
      if (small & ncols != length(all_feats)) {
        if (length(id_field) == 1) {
          miss_feat   <-
            setdiff(as.character(zone_cropped@data[, "mdxtnq"]), names(ts))
          pos_missing <-
            which(as.character(zone_cropped@data[, "mdxtnq"]) %in% miss_feat)
        } else {
          pos_missing <-
            miss_feat <-
            which(as.character(zone_cropped@data[, "mdxtnq"]) %in% miss_feat)
        }
        shpsub <- zone_cropped[pos_missing,]
        ts_mis <-
          matrix(nrow = length(sel_dates),
                 ncol = length(pos_missing))
        for (f in 1:length(sel_dates)) {
          if (verbose == TRUE) {
            print(paste0("Extracting data from date: ",
                         dates[sel_dates[f]]))
          }
          if (small_method == "centroids") {
            ts_mis[f,] <- extract(in_rts[[sel_dates[f]]],
                                  raster::coordinates(shpsub), fun = mean)
          } else {
            ts_mis[f,] <- extract(in_rts[[sel_dates[f]]],
                                  shpsub, fun = mean)
          }
        }
        colnames(ts_mis) <- miss_feat
        ts <- cbind(ts, ts_mis)
      }
    }
    if (zone_type == "spobject") {
      file.remove(tempraster)
      file.remove(tempshape)
      
      if (exists("outside_feat")) {
        if (length(id_field) == 1) {
          feat_names_outside = as.character(zone_object@data[,
                                                             eval(id_field)])[outside_feat]
        }
        else {
          feat_names_outside = as.character(zone_object@data[,
                                                             "mdxtnq"])[outside_feat]
        }
        
        ts_outside = matrix(nrow = length(sel_dates),
                            ncol = length(feat_names_outside))
        ts_outside = data.frame(ts_outside)
        names(ts_outside) = feat_names_outside
        ts = cbind(ts, ts_outside)
        if (length(id_field) == 1) {
          sortindex = match(zone_object@data[, eval(id_field)], names(ts))
        } else {
          sortindex = match(zone_object@data[, "mdxtnq"], names(ts))
        }
        ts = ts[, c(1, sortindex)]
      }
    }
    if (out_format == "xts") {
      ts <- as.xts(ts, order.by = dates[sel_dates])
    }
    #gc()
    ts
  } else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
  #gc()
}
