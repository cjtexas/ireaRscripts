library(raster)
library(rgdal)
library(data.table)
library(gdalUtils)
library(ireaRscripts)

in_mod       <- "/home/lb/Temp/quality_phenorice/PHL/QUALITY_ts_input_2012_177_2014_065.dat"
inmodrast    <- raster(in_mod)
mod_proj     <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
in_ext       <- extent(inmodrast)
zones_raster <- "/home/lb/Temp/quality_phenorice/PHL/mask_fishnet_WS.tif"
in_maskfile  <- "/home/lb/Temp/quality_phenorice/PHL/Rice_WS_Sinusoidal.tif"
outmaskfile  <- "/home/lb/Temp/quality_phenorice/PHL/mask_quality_WS.tif"


inmask = raster(in_maskfile)
mask_proj <- proj4string(inmask)
# ext_mask  <- extent(outmask)
mask_reprojfile <- tempfile(tmpdir = tempdir(),fileext = ".tif")



gdalwarp(in_maskfile, mask_reprojfile, s_srs = mask_proj,
         t_srs = mod_proj, overwrite = TRUE, ot = 'Byte', of = "ENVI", srcnodata = 255, dstnodata = 255)

mask_repr     <- raster(mask_reprojfile)
ext_mask_sinu <- extent(mask_repr)
create_fishnet(in_ext, out_shape = FALSE, cellsize = 231.656358,
               in_proj = mod_proj, out_rastfile = zones_raster, overw = TRUE, crop_ext = ext_mask_sinu)

# zones_raster <- crop(zones_raster, extent(mask_repr))
zones_raster_hr <- tempfile(tmpdir = tempdir(), fileext = '.tif')
gdal_translate(zones_raster, zones_raster_hr, tr = raster::res(mask_repr),
               te = extent(mask_repr)[c(1,3,2,4)], overwrite = T, tap = T, ot = "uint32")

mask_alignedfile <- tempfile(tmpdir = tempdir(), fileext = '.tif')
align_rasters(mask_reprojfile, zones_raster_hr, mask_alignedfile, overwrite = TRUE)

# Compute zonal statistics on MODIS pixels using the mask as input and a user function to extract
# values from the mask

zonestats <- fastzonal(in_rts = raster(mask_alignedfile), zone_object = zones_raster_hr, out_format = 'dframe')

outrast   <- raster(zones_raster)
outrast[] <- as.numeric(zonestats[2:length(names(zonestats))])

rcl_mat <- list(
  list(start = 0, end  = 0.75, new = NA),
  list(start = 0.75, end  = 4, new = 1)
)

outmask2 = rast_reclass(r, rcl_mat, out_rast = outmaskfile, r_out = TRUE, ovr = TRUE)


 mask_DS = raster("/home/lb/Temp/quality_phenorice/PHL/mask_quality_DS.tif")
 mask_WS = raster("/home/lb/Temp/quality_phenorice/PHL/mask_quality_WS.tif")
 
 mask_ws_ds = align_rasters("/home/lb/Temp/quality_phenorice/PHL/mask_quality_DS.tif", "/home/lb/Temp/quality_phenorice/PHL/mask_quality_WS.tif", "/home/lb/Temp/quality_phenorice/PHL/mask_quality_bis.tif", overwrite = TRUE)
 
 mask_ws_ds = raster("/home/lb/Temp/quality_phenorice/PHL/mask_quality_bis.tif")
