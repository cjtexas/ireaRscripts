#' MODISmasker
#'
#'
#' @importFrom raster raster extent intersect
#' @importFrom sp proj4string
#' @return
#' @export
#'
#' @examples
#'

MODISmasker <- function(){

  in_maskfile <- "/home/lb/projects/ermes/datasets/rs_products/RICE_map/Gambia/Delivery/Classification2016_Geographic"

  mod_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

  # Open the raster mask - if necessary do something to convert it to 0-1
  if(class(in_maskfile) == "character") {
    in_mask <- raster(in_maskfile)
  }

  m <- c(0, 0, NA,
         0, 4, 0,
         4, 5, 1,
         5, 8, 0,
         8, 10,1,
         10,11,0)
  rclmat <- matrix(m, ncol = 3, byrow = TRUE)

  mask_reclass_file = "/home/lb/Temp/buttami/Classification2016_Geographic_reclass.tif"
  raster::reclassify(in_mask, rclmat, filename = mask_reclass_file,
                     include.lowest = FALSE, overwrite = TRUE)
  in_mask     <- raster(mask_reclass_file)
  in_maskfile <- mask_reclass_file


  # Get mask projection information and extent
  mask_proj <- proj4string(in_mask)
  ext_mask  <- extent(in_mask)

  # if necessary, reproject the input mask
  mask_reprojfile <- tempfile(tmpdir= tempdir(),fileext = ".tif")
  mask_reprojfile <- "/home/lb/Temp/buttami/repr_mask.tif"

  gdalwarp(in_maskfile, mask_reprojfile, s_srs = mask_proj, t_srs = mod_proj, overwrite = TRUE)

  # Convert mask extent to MODIS projection
  mask_repr     <- raster(mask_reprojfile)
  ext_mask_sinu <- extent(mask_repr)

  # find the MODIs tiles which intersect the mask
  int_tiles <- intersect(modis_grid, ext_mask_sinu)
  minh <- min(int_tiles$H)
  maxh <- max(int_tiles$H)
  minv <- min(int_tiles$V)
  maxv <- max(int_tiles$V)

  submod_grid <- subset(modis_grid, (H >= minh & H <= maxh & V >= minv & V <= maxv))

  # Create a MODIS polygon grid spanning the identified tiles and crop it on
  # extent of the mask

  # Create a polygon grid encompassing the extent of the tiles
  in_ext  <- extent(submod_grid)
  zones_raster <- "/home/lb/Temp/buttami/fishrastw.tif"

  create_fishnet(in_ext, crop_ext =  ext_mask_sinu,  out_shape = FALSE, cellsize = 231.656358,
                            in_proj = mod_proj, out_rastfile = zones_raster, overw = TRUE
                            )
  # make so that the mask and the zones tiff have the same extent and resolution

  zones_raster_hr <- tempfile(tmpdir = tempdir(), fileext = '.tif')
  gdal_translate(zones_raster, zones_raster_hr, tr = raster::res(mask_repr),
           te = extent(mask_repr)[c(1,3,2,4)], overwrite = T, tap = T)
  mask_alignedfile <- tempfile(tmpdir = tempdir(), fileext = '.tif')
  align_rasters(mask_reprojfile, zones_raster_hr, mask_alignedfile)

  # Compute zonal statistics on MODIS pixels using the mask as input and a user function to extract
  # values from the mask

  zonestats <- fastzonal(mask_repr, sp_object = zones_raster_hr)



  # Save the results as a "MODIS mask", that is, a mask that covers the extent of the input mask (or of
  # input MODIS file), contataining values derived from aggregation of the higher resolution
  # input file. The output can be a 0-1 file, but also a continuous value file !!!!!


  # End processing










}
