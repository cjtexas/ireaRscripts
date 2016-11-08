#' MODISmasker
#'
#'
#' @importFrom raster raster
#' @return
#' @export
#'
#' @examples
#'

MODISmasker <- function(){

  in_maskfile <- "/home/lb/projects/ermes/datasets/rs_products/RICE_map/Gambia/Delivery/Classification2016_Geographic"

  mod_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  # Open the raster
  if(class(in_maskfile) == "character") {
    in_mask <- raster(in_maskfile)
  }

  # Get mask projection information and extent
  mask_proj <- proj4string(in_mask)
  ext_mask  <- extent(in_mask)

  # Convert mask extent to MODIS projection
  ext_mask_sinu <- extent_reproj(ext_mask,mask_proj,mod_proj)

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
  sp_grid <- create_fishnet(in_ext, crop_ext = ext_mask_sinu, cellsize = 231.656358,
                            in_proj = mod_proj, out_shapefile = "/home/lb/Temp/buttami/aaa2.shp", out_format = "shape",
                            crop_onmask = FALSE)




  # create a virtual raster with MODIS resolution that covers the mask



  # Compute zonal statistics on MODIS pixels using the mask as input and a user function to extract
  # values from the mask



  # Save the results as a "MODIS mask", that is, a mask that covers the extent of the input mask (or of
  # input MODIS file), contataining values derived from aggregation of the higher resolution
  # input file. The output can be a 0-1 file, but also a continuous value file !!!!!


  # End processing










}
