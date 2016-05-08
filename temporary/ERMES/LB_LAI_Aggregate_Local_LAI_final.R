args <- commandArgs(TRUE)
t1 = Sys.time()
args
# load required libraries (install if missing)
pkg_list = c('tools','raster','sp', 'gdalUtils','rgdal','data.table','plyr', 'utils', 'xts')
pkg_test <- function(x) {
  if (!require(x,character.only = TRUE)) {install.packages(x,dep=TRUE)
    require(x,character.only=TRUE)}
}
for (pkg in pkg_list) {pkg_test(pkg)}

library(ireaRscripts)

memory.limit(8000)
rasterOptions (setfileext = F)

#Retrieve parameters from the caller
# infile = args[1]
# in_nodata = args[2]
# in_raster_proj = args[3]
# in_ermes_grid_laea  = args[4]
# in_lc_file  = args[5]
# out_folder = args[6]
# out_filename = args[7]
# out_format = args[8]
#
# fc_threshold = as.numeric(args[9])
#

# Load scripts

script_folder = "D:/Documents/ERMES/Documents/Data_Processing/aggregate_for_local/x_Manu/source_code/"
source(file.path(script_folder,"lb_fastzonal.R" ))
source(file.path(script_folder,"lb_openshape.R" ))
source(file.path(script_folder,"lb_writeshape.R" ))

# Setting inputs and outputs --------

type = "MOD"

if (type == "MOD") {
  coarse_file = "D:/Documents/ERMES/Documents/Data_Processing/aggregate_for_local/Spain/template_rasters/MOD15A2_LAI_A2013225_euro_ISIN"
  templ_2k_file = "D:/Documents/ERMES/Documents/Data_Processing/aggregate_for_local/Spain/template_rasters/ES_LAI_MOD_2015_265.tif"
}

in_folder = 'D:/Documents/ERMES/Documents/Data_Processing/aggregate_for_local/Spain/'
in_lc_file = 'D:/Documents/ERMES/Documents/Data_Processing/aggregate_for_local/LC_maps/SIOSE_MOSAIC_mask_ARABLE.tif'
in_nodata = -1   # set to nodata value of input LAI images

# set to the location of the ERMES grid shapoefile
in_ermes_grid_laea  = 'D:/Documents/ERMES/Documents/Data_Processing/EO_Data_Aggregation/shape_polygons/ERMES_Grid_Spain_shp_LAEA.shp'

fc_threshold = 0.75  # set the arable land fc threshold
out_folder = 'D:/Documents/ERMES/Documents/Data_Processing/aggregate_for_local/Test/Spain'  # set to output folder

# start -----------
# Set-up input/output folders and filenames
temp_folder = 'D:/Documents/ERMES/Documents/Data_Processing/aggregate_for_local/Test/Spain/temp'
dir.create(temp_folder)

# get list of tiff filenames to be processed

in_list = list.files(in_folder, pattern = '.HDR$', full.names = TRUE)
# Start cycling on input files

for (file in in_list) {

  # set output filename and format
  infile = file_path_sans_ext(file)
  out_filename_avg = file.path(out_folder,paste0(basename(infile),'avg_2km.tif'))
  out_filename_sd = file.path(out_folder,paste0(basename(infile),'sd_2km.tif'))

  # get projections of in and lc file
  infile_proj4 = proj4string(raster(infile))
  in_lc_raster_proj = proj4string(raster(in_lc_file))
  laea_crs = CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  out_folder_raster = file.path(out_folder,'Reprojected_Native_Res')
  out_file_raster = file.path(out_folder_raster, paste(basename(infile),'_LAEA', sep = ''))
  out_ermes_grid_reproj = tempfile(fileext = '.shp')

  # Reproject grid to input file projection
  ERMES_cells_poly = lb_openshape(in_ermes_grid_laea)
  ERMES_cells_poly_reproj = spTransform(ERMES_cells_poly, infile_proj4)
  lb_writeshape(ERMES_cells_poly_reproj,out_ermes_grid_reproj)
  extent_poly_reproj = extent(ERMES_cells_poly_reproj)  # Retrieve extnt of ERMES grid in the projection of input raster
  # extent_poly_laea = extent(ERMES_cells_poly)           # Retrieve extnt of ERMES grid in original LAEA proj

  # crop ermes grid to input file extent

  fullgrid = lb_openshape(out_ermes_grid_reproj)
  extout = extent(raster(infile))
  cropped_grid = crop(fullgrid,extout)
  ext_cropped = extent (cropped_grid)
  ext_cropped_laea = extent(spTransform(cropped_grid, laea_crs))
  cropped_grid_LAEA = spTransform(cropped_grid, laea_crs)

  temprast_LAEA = tempfile(fileext = '.tif')
  # Reproject the original raster to LAEA projection , clip it on ERMES cropped Grid extent
  gdalwarp(infile,temprast_LAEA, s_srs = infile_proj4, t_srs = laea_crs,
           r = "near", te = c(ext_cropped_laea@xmin, ext_cropped_laea@ymin,ext_cropped_laea@xmax, ext_cropped_laea@ymax),
           srcnodata = in_nodata, dstnodata =  in_nodata, of = 'GTiff', overwrite = T)

  # Compute fc arable on pixels of original coarse resoultion file (MOD or VGT)
  # then create a mask to be applied on the input HR lai file
  in_lc_file_temp = tempfile(fileext = '.tif')
  lc_temp = gdalwarp(in_lc_file, in_lc_file_temp, s_srs = laea_crs, t_srs = laea_crs, output_Raster = T,
                     te = c(ext_cropped_laea@xmin, ext_cropped_laea@ymin,ext_cropped_laea@xmax, ext_cropped_laea@ymax),
                     r = "near", srcnodata = 255, dstnodata =  255, of = "GTiff", overwrite = T, tr = res(raster(temprast_LAEA)) )

  # to Find out shich pixels of the original LAI file were NOT used in the aggregation of the coarase resolution file to 2km
  # open an example coarse res file to build a grid on to which computing the fcs
  rtemp = 		# get the original LAI file
    tempfile_coarse_repr = tempfile(fileext = '.tif')
  coarse_repr = gdalwarp(coarse_file,tempfile_coarse_repr, s_srs = proj4string(raster(coarse_file)), t_srs = laea_crs, output_Raster = TRUE, overwrite = T)
  rtemp_repr = crop(raster(tempfile_coarse_repr),ext_cropped_laea)
  grid = getGridTopology(as(rtemp_repr, "SpatialGrid"))		# create a grid from the cropped coarse res file
  pix_grid = SpatialGrid(grid,laea_crs)
  pix_grid_cell = SpatialGridDataFrame(pix_grid, data = data.frame(id = seq(1:(dim(rtemp_repr)[1]*dim(rtemp_repr)[2]))),laea_crs)
  sp_polygons_fc = as(pix_grid_cell, "SpatialPolygonsDataFrame")	#create the shape - convert grid to polygons
  extent_poly_fc = extent(sp_polygons_fc)
  #   # write to shapefile
  temp_grid_coarse = tempfile(fileext = '.shp')
  lb_writeshape(sp_polygons_fc,temp_grid_coarse)   # write temporarty grid

  # Compute the fcs
  indata = stack(raster(in_lc_file))
  indata = setZ(indata, as.Date('2015-01-01'), name = "time")
  arable_fc  = lb_fastzonal(indata, sp_polygons_fc,id_field = "id", verbose = T, out_format = 'xts', small = T)
  arable_fc_df = data.frame(id = as.numeric(names(arable_fc)), arab_fc = as.numeric(arable_fc))
  sp_polygons_fc@data = join(sp_polygons_fc@data, arable_fc_df, type = 'left')
  lb_writeshape(sp_polygons_fc, temp_grid_coarse)
  temprast_fc = tempfile(fileext = '.tif')
  file.copy(temprast_LAEA,temprast_fc)
  gdal_rasterize(temp_grid_coarse, temprast_fc, a = "arab_fc")
  temprast_fc_repro = tempfile(fileext = '.tif')
  extout_laea = extent(raster(temprast_LAEA))
  gdalwarp(temprast_fc,temprast_fc_repro, s_srs = laea_crs, t_srs = laea_crs,
           te = c(ext_cropped_laea@xmin, ext_cropped_laea@ymin,ext_cropped_laea@xmax, ext_cropped_laea@ymax),
           r = "near", srcnodata = 255, dstnodata =  255, of = "GTiff", overwrite = T, tr = res(raster(temprast_LAEA)))
  mask_fc = raster(temprast_fc_repro)
  mask_fc[mask_fc >= fc_threshold] = 1
  mask_fc[mask_fc < fc_threshold] = 0

  masked_LAI_map = mask(raster(temprast_LAEA), mask_fc,maskvalue = 0)
  # tempraster_masked = tempfile(fileext = '.tif')
  # out_temp_raster_masked = tempfile(fileext = '.tif')
  # writeRaster(masked_LAI_map ,filename = tempraster_masked, overwrite = T)
  #
  # Extract avg and sd LAI values from the masked image ----
  indata = stack(masked_LAI_map)
  indata = setZ(indata, as.Date('2015-01-01'), name = "time")
  avg_LAI  = lb_fastzonal(indata, cropped_grid_LAEA,id_field = "int_id", verbose = T, out_format = 'xts')
  avg_LAI_df = data.frame(int_id = as.numeric(names(avg_LAI)), LAIavg = as.numeric(avg_LAI))

  sd_LAI  = lb_fastzonal(indata, cropped_grid_LAEA,id_field = "int_id", verbose = T, out_format = 'xts', FUN = sd, na.rm = T)
  sd_LAI_df = data.frame(int_id = as.numeric(names(sd_LAI)), LAIsd = as.numeric(sd_LAI))

  coarselai_shape = cropped_grid_LAEA
  newdata = join(coarselai_shape@data, avg_LAI_df, type = 'left')
  newdata = join(newdata, sd_LAI_df, type = 'left')
  coarselai_shape@data = newdata
  tempshpfile = tempfile(fileext = '.shp')
  lb_writeshape(coarselai_shape, tempshpfile)

  in_2k = crop(raster(templ_2k_file), extent(cropped_grid_LAEA))
  temp2k_file = tempfile(fileext = '.tif')
  writeRaster(in_2k, temp2k_file)
  temprast = tempfile(fileext = '.tif')
  temprastsd = tempfile(fileext = '.tif')
  file.copy(temp2k_file,temprast)
  file.copy(temp2k_file,temprastsd)
  out_rast = gdal_rasterize(tempshpfile, temprast, a ="LAIavg", output_Raster = TRUE)
  out_rast_sd = gdal_rasterize(tempshpfile, temprastsd, a ="LAIsd", output_Raster = TRUE)
  # raster(temprast)
  writeRaster(out_rast, out_filename_avg, NAflag = -1, overwrite = T)
  writeRaster(out_rast_sd, out_filename_sd, NAflag = -1, overwrite = T)
}

