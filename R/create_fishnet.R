#' create_fishnet
#' @description funtion to create a polygon fishnet of specified resoultion over a spatial object
#'  Object can be a raster file, an extent, or a generic *SP object. If a raster is selected, the
#'  grid is made so that upper left corner coincides with upper left corner of the raster.
#' If both in_ext and in_rast are declared, the fishnet is cropped on raster extent.
#' @param in_ext
#' @param cellsize
#' @param out_name_shp string(optional)
#' @param expand
#' @param in_rast
#'
#' @import rgdal
#' @import raster
#' @return
#' @export
#'
#' @examples

create_fishnet <- function(in_ext = NULL, crop_ext = NULL, out_name_shp = NULL, cellsize = NULL, in_proj,
                           out_format = 'raster', out_rastfile = NULL, out_shapefile = NULL, crop_onmask = FALSE) {

  if (class(in_ext) != "Extent" | class(crop_ext) != c("Extent")) {
    stop('error')
  }

  if (is.null(in_ext) ) {
    stop('error')
  }

  if (!is.null(in_ext)) {
    cc <- c(in_ext@xmin,in_ext@ymin)
  }

  # if (is.null(crop_ext)) {
  #   in_ext   <-
  #   cellsize <- res(in_rast)
  #   cc       <- c(in_ext@xmin,in_ext@ymin) + (cs/2)  # cell offset   # move 1/2 pixel if necessary
  # }

  if (is.null(cellsize)){
      stop('error')
    }


  cs <- c(cellsize,cellsize)  # cell size.

  # compute number of cells per direction
  cd <- ceiling(c(((in_ext@xmax - in_ext@xmin)/cs[1]),((in_ext@ymax - in_ext@ymin)/cs[2]))) - 1

  # construct grid topology
  grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)   # Define grd characteristics

  #transform to spatial grid
  sp_grd <- SpatialGridDataFrame(grd,
                                 data = data.frame(id = seq(1,(prod(cd)),1)),  # ids are numbers between 1 and ns*nl
                                 proj4string = CRS(in_proj))


  if (out_format == "raster") {
    out_raster <- raster(sp_grd)
    setValues(out_raster, sp_grd@data$id)
    if (crop_onmask == TRUE) {
      out_raster <- crop(out_raster, crop_ext)
    }
    if (!is.null(out_rastfile)) {
      writeRaster(out_raster, out_rastfile, overwrite = TRUE)
      return()
    } else{
      return(out_raster)
    }
  }

  if (out_format == "shape") {
    print('Creating Polygon Grid - Please Wait !')
    if (crop_onmask == TRUE) {
      sp_grd <- crop(sp_grd, crop_ext)
    }
    sp_polygons = as(sp_grd, "SpatialPolygonsDataFrame")
    if (!is.null(out_shapefile)) {
      writeshape(sp_polygons, out_name_shp, overw = TRUE) #save the shapefile
    } else {
      return(sp_polygons)
    }
  }
}
