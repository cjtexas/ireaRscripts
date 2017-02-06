library(raster)
in_maskfile = "d:/Documents/GDrive/ERMES/WP_9/Task9.3_outside_Europe/Senegal_SoS_2016.tif" 
# Open  the masking file
 in_mask <- raster(in_maskfile)
 # setup the reclassification matrix
 rcl_mat <- list(
    list(start = 0, end  = 2, new = NA),
    list(start = 2, end  = 3, new = datetodoy(as.Date("2016-01-20"))),
    list(start = 3, end  = 4, new = datetodoy(as.Date("2016-02-01"))),
    list(start = 4, end  = 5, new = datetodoy(as.Date("2016-02-13"))),
    list(start = 5, end  = 6, new = datetodoy(as.Date("2016-02-25"))),
    list(start = 6, end  = 7, new = datetodoy(as.Date("2016-03-08"))),
    list(start = 7, end  = 8, new = datetodoy(as.Date("2016-03-20"))),
    list(start = 8, end  = 9, new = datetodoy(as.Date("2016-04-01"))),
    list(start = 9, end  = 10, new = datetodoy(as.Date("2016-04-13"))),
    list(start = 10, end  = 11, new = datetodoy(as.Date("2016-04-25"))),
    list(start = 11, end  = 12, new = datetodoy(as.Date("2016-05-07"))),
    list(start = 12, end  = 13, new = datetodoy(as.Date("2016-05-19"))),
    list(start = 13, end  = 14, new = datetodoy(as.Date("2016-05-31"))),
    list(start = 14, end  = 15, new = datetodoy(as.Date("2016-06-12"))),
    list(start = 15, end  = 16, new = datetodoy(as.Date("2016-07-06"))),
    list(start = 16, end  = 17, new = datetodoy(as.Date("2016-07-18"))),
    list(start = 17, end  = 17, new = datetodoy(as.Date("2016-08-11"))),
    list(start = 18, end  = 18, new = datetodoy(as.Date("2016-08-23"))),
    list(start = 19, end  = 20, new = datetodoy(as.Date("2016-09-04"))),
    list(start = 20, end  = 21, new = datetodoy(as.Date("2016-09-16"))),
    list(start = 21, end  = 22, new = datetodoy(as.Date("2016-09-28"))),
    list(start = 22, end  = 23, new = datetodoy(as.Date("2016-10-10"))),
    list(start = 23, end = 100, new = NA)
   )


 reclass_file = "d:/Documents/GDrive/ERMES/WP_9/Task9.3_outside_Europe/Senegal_SoS_2016_doys_NEW.tif"
 outmask = rast_reclass(in_mask, rcl_mat, out_rast = reclass_file,
                                 r_out = TRUE, ovr = TRUE)
 summary(outmask)
 
 file_in = raster("d:/Documents/GDrive/ERMES/WP_9/Task9.3_outside_Europe/SN_Phenology_MinDoys_2016_Dry.tif")

 reclass = raster(reclass_file)
 file_in_wgs = projectRaster(file_in, crs = CRS(proj4string(reclass)))
 inrast  <- crop(file_in_wgs, extent(raster(reclass)))
 writeRaster(inrast, filename = "d:/Documents/Temp/fishnet.tif", overwrite = T)
 inrastok = raster(inrast)
   ext_rast <- extent(inrast)
   csize    <- res(inrast)[1]

   out_file <- "d:/Documents/GDrive/ERMES/WP_9/Task9.3_outside_Europe/MODGRID3.shp"
   create_fishnet(ext_rast, cellsize = csize, out_shape = TRUE, out_shapefile = out_file,
          overw = TRUE, out_raster = FALSE, in_proj = proj4string(inrast), pypath = "C:\\OSGeo4W64\\bin\\gdal_polygonize.py")
 
   in_rts = stack(inrast)
   in_rts = setZ(in_rts,as.Date("2000-01-01","time"))
   out = fastzonal(in_rts = in_rts, id_field = 'id', out_format = "dframe",
                   na.rm = T,verbose = T,zone_object = "D:/Documents/temp/grid_2k.shp")
   
   a = fastzonal()
   
   val = getValues(reclass)
   reclass[reclass <32] = NA 
   reclass[reclass >100] = NA
   setValues(reclass, val)
   zones_modis = raster::extract(inrast, openshape("D:/Documents/temp/grid_2k.shp"))
   zones_sar = raster::extract(reclass, openshape("D:/Documents/temp/grid_2k.shp"))
   
   
   pro = as_tibble(openshape("D:/Documents/temp/grid_2k.shp"))
   
   names(pro)
   
   p1 = ggplot(pro, aes(x = round(sarmean), y = round(moddoys_ok))) +
     geom_count() + 
     theme_bw() + xlim(40,100)+ ylim(40,100)+
     coord_cartesian(xlim = c(30,100)) + 
     geom_smooth(method = 'lm', se = F)
   
   p1
   
   pro2 = pro %>% 
     select(id, sarmean, modmean) %>% 
     filter(!is.na(modmean)) %>% 
     filter(modmean > 40 & modmean < 90 & sarmean >40) %>% 
     gather(key,value, -id)
   
   p1 = ggplot(pro2, aes(x = round(sarmean), y = round(modmean))) +
     geom_count() + 
     theme_bw() + xlim(30,100)+ ylim(30,100)+
     coord_cartesian(xlim = c(30,100)) + 
     geom_smooth(method = 'lm', se = F)+
     ggtitle("MODIS vs SAR sowing dates")+
     xlab("SAR") + ylab("MODIS")
   p1
   
   p1 = ggplot(pro2, aes(x = doytodate(pro2$sarmean,2016), y =doytodate(pro2$modmean,2016))) +
     geom_hex(binwidth = 5) + 
     theme_bw() + xlim(doytodate(30,2016),doytodate(100,2016))+ ylim(doytodate(30,2016),doytodate(100,2016))+
     # coord_cartesian(xlim = xlim(doytodate(30,2016),doytodate(100,2016))) + 
     scale_fill_gradient(high = "black", low = 'grey95')+
     xlab("SAR") + ylab("MODIS")+
     ggtitle("MODIS vs SAR sowing dates")
   
   p1
   
   p2 = ggplot(pro2) +
     geom_violin(aes(x = key, y = value))+
     theme_bw()
   
   pirateplot(formula =value ~key  ,data = pro2, ylab = "Sowing DOY",
           main = "Sowing dates distribution",
           theme =1,
           pal = "southpark",
           ylim = c(30 ,100),
           title = "Sowing dates distribution")
   
   p2
   
   
   
   