lb_class_accuracy_points

library (hash)
library(raster)
library(ireaRscripts)

in_classrast = "/home/lb/projects/ermes/datasets/rs_products/RICE_map/2016/ITA/early_maps/EarlyMaps/asc+desc_v2_MRA"     # input classified raster
in_shape = "/home/lb/Temp/campagna_2016_ALL_points.shp"        # input shapefile of ground truths

hash_rast = hash( "Rice" = 1,     # Hash table of raster: value to class
                  "Rice" = 2,
                  "Rice" = 4,
                  "NoRice" = 2,
                  "Unclassified" = 0)

hash_shape = hash( "Rice" = "Rice",     # Hash table of shape: value to class
                  "NoRice" = "NoRIce",
                  "Unknown" = "Unknown",
                  "Unclassified" = "Unclassified")

# Open raster and shape ----
inrast = raster(in_classrast)
inshape = lb_openshape(in_shape)

# Extract raster value for each point of the shape
rast_values = lb_convertBS()




# Reassign classes on the basis of hash tables to have homogeneity ----




# Compute accuracy matrix ----




# output the results ----




