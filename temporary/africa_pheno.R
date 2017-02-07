library(raster)
library(ireaRscripts)
library(data.table)
library(tidyverse)
library(gdalUtils)
library(mapview)

# Reclassify the input SAR SoS maps ----

in_sar_dry    <- "/home/lb/Google_Drive/My_Files/srv_phenology/Senegal_2016_Dry_SoS.tif" 
in_sar_wet    <- "/home/lb/Google_Drive/My_Files/srv_phenology/Senegal_2016_Wet_SoS.tif" 
in_file_price <- "/home/lb/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/2016/Phenorice_out_2015_209_2017_097.dat"

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
    list(start = 17, end  = 18, new = datetodoy(as.Date("2016-08-11"))),
    list(start = 18, end  = 19, new = datetodoy(as.Date("2016-08-23"))),
    list(start = 19, end  = 20, new = datetodoy(as.Date("2016-09-04"))),
    list(start = 20, end  = 21, new = datetodoy(as.Date("2016-09-16"))),
    list(start = 21, end  = 22, new = datetodoy(as.Date("2016-09-28"))),
    list(start = 22, end  = 23, new = datetodoy(as.Date("2016-10-10"))),
    list(start = 23, end  = 24, new = datetodoy(as.Date("2016-10-22"))),
    list(start = 24, end  = 25, new = datetodoy(as.Date("2016-11-03"))),
    list(start = 25, end  = 26, new = datetodoy(as.Date("2016-11-15"))),
    list(start = 26, end  = 27, new = datetodoy(as.Date("2016-11-27"))),
    list(start = 27, end  = 28, new = datetodoy(as.Date("2016-12-09"))),
    list(start = 28, end  = 29, new = datetodoy(as.Date("2016-12-21"))),
    list(start = 29, end = 100, new = NA)
   )

 doysar_dry   <-  rast_reclass(raster(in_sar_dry), rcl_mat, out_rast = tempfile(),r_out = TRUE, ovr = TRUE)
 doysar_wet   <-  rast_reclass(raster(in_sar_wet), rcl_mat, out_rast = tempfile(),r_out = TRUE, ovr = TRUE)
 
 doysar_dry[doysar_dry == 0] = NA
 doysar_wet[doysar_wet == 0] = NA
# Get data from phenorice outputs, reproject and crop on SAR extent ----
 
 doymod_dry    <- sum(raster(in_file_price, band = 2),raster(in_file_price, band = 3), na.rm = TRUE) %>% 
                  projectRaster(crs = CRS(proj4string(doysar_dry)), method = "ngb") %>% 
                  crop(extent(doysar_dry))
 
 doymod_wet    <- sum(raster(in_file_price, band = 4),raster(in_file_price, band = 3), na.rm = TRUE) %>%
                  projectRaster(crs = CRS(proj4string(doysar_dry)), method = "ngb") %>% 
                  crop(extent(doysar_dry))
 
 doymod_dry[doymod_dry == 0]   <- NA
 doymod_dry[doymod_dry <= 0]   <- NA
 doymod_dry[doymod_dry >= 120] <- NA
 doymod_wet[doymod_wet == 0]   <- NA
 doymod_wet[doymod_wet < 120]  <- NA
 doymod_wet[doymod_wet > 270]  <- NA
 
 doysar_dry[doysar_dry >= 120] <- NA
 doysar_dry[doysar_dry <= 0]   <- NA
 
 doysar_wet[doysar_wet < 120]  <- NA
 doysar_wet[doysar_wet > 270]  <- NA
 
 NAvalue(doymod_dry) <- 0
 NAvalue(doymod_wet) <- 0
 
 
 
# Create fishnet for zonal statistics computation
 
 grd_file <- "/home/lb/Google_Drive/My_Files/srv_phenology/MODGRIDdry.shp"
 ext_rast <- extent(doymod_dry)
 csize    <- res(doymod_dry)
 create_fishnet(ext_rast, cellsize = csize, out_shape = TRUE, out_shapefile = grd_file,
                overw = TRUE, out_raster = FALSE, in_proj = proj4string(doymod_dry), 
                res_factor = 10)
 
 
 avg_moddry    = fastzonal(in_rts = doymod_dry, id_field = 'id', out_format = "dframe",
                     na.rm = T,verbose = T,zone_object = grd_file) %>% 
                 gather(cell, value, -band)                        %>%
                 select(-band)                                     %>% 
                 mutate(type = "MOD", seas = "dry")%>%
                 as_tibble()
 
 avg_modwet    = fastzonal(in_rts = doymod_wet, id_field = 'id', out_format = "dframe",
                           na.rm = T,verbose = T,zone_object = grd_file) %>% 
                 gather(cell, value, -band)                        %>% 
                 select(-band)                                     %>% 
                 mutate(type = "MOD", seas = "wet") %>%
                 as_tibble()
 
 avg_sardry    = fastzonal(in_rts = doysar_dry, id_field = 'id', out_format = "dframe",
                           na.rm = T,verbose = T,zone_object = grd_file) %>% 
                 gather(cell, value, -band)                        %>%
                 select(-band)                                     %>% 
                 mutate(type = "SAR", seas = "dry") %>%
                 as_tibble()
 
 avg_sarwet    = fastzonal(in_rts = doysar_wet, id_field = 'id', out_format = "dframe",
                           na.rm = T,verbose = T,zone_object = grd_file) %>% 
                 gather(cell, value, -band)                        %>% 
                 select(-band)                                     %>% 
                 mutate(type = "SAR", seas = "wet") %>%
                 as_tibble()
 
 all_data_mod <- rbind(avg_moddry, avg_modwet)
 all_data_sar <- rbind(avg_sardry, avg_sarwet)
 
 all_data <- select(all_data_mod, cell, seas, value) %>% 
   cbind(all_data_sar$value)  %>% 
   as_tibble() %>% 
   rename("value_sar" = `all_data_sar$value`)
 
 p1 <- ggplot(subset(all_data, seas == "dry"), aes(x = doytodate(floor(value), 2016), y = doytodate(floor(value_sar-12),2016))) + 
   geom_point(alpha = 0.6) + theme_bw() + facet_wrap(~seas, scales = "free") + geom_abline(slope = 1, intercept = 0, color = 'red', lty = 2) +
   coord_cartesian(xlim = doytodate(c(0,130),2016), ylim = doytodate(c(0,130),2016)) + 
   xlab("Sowing Date - PhenoRice") + ylab("Sowing Date - Sentinel-1")
 p1
 
 p2 <- ggplot(subset(all_data, seas == "wet"), aes(x = doytodate(floor(value), 2016), y = doytodate(floor(value_sar-12),2016))) +
   geom_point(alpha = 0.6) + theme_bw() + facet_wrap(~seas) +
   coord_cartesian(xlim = doytodate(c(100,315),2016), ylim = doytodate(c(100,315),2016)) +
   geom_abline(slope = 1, intercept = 0, color = 'red', lty = 2) + 
   xlab("Sowing Date - PhenoRice") + ylab("Sowing Date - Sentinel-1")
   
 p2
 
 p3 <- ggplot(subset(all_data), aes(x = doytodate(floor(value), 2016), y = doytodate(floor(value_sar-12),2016))) +
   geom_point(aes(color = seas), alpha = 0.6) + theme_bw() +
   coord_cartesian(xlim = doytodate(c(0,315),2016), ylim = doytodate(c(0,315),2016)) +
   geom_abline(slope = 1, intercept = 0, color = 'red', lty = 2) + 
   xlab("Sowing Date - PhenoRice") + ylab("Sowing Date - Sentinel-1") + 
   scale_color_manual("Rice Season", values = c("darkred", "darkblue"))
 
 p3 + theme(legend.background = element_rect(colour = "azure2", 
    size = 0.3, linetype = "solid"), legend.position = c(0.93, 
    0.13))
 
 all_data2 <- all_data %>% 
   mutate(value_sar = value_sar - 6)
 all_data_melt <- all_data2 %>% 
   gather(key, value, -cell, -seas)
 
 p4 <- ggplot(all_data_melt) + 
   geom_boxplot(aes(x = key, y = doytodate(value ,2016))) + 
   facet_wrap(~seas)
 p4
 
 p4 <- ggplot(all_data_melt) + theme_bw() +
   # geom_histogram(binwidth = 12, aes(x = doytodate(value,2016), fill = key, y = 100*(..count..)/sum(..count..)), alpha = 0.4, color = "black", position = "identity") + 
   geom_bar(aes(x = value, y = (..count..)/sum(..count..),group = key,fill = key))+
   facet_wrap(~key) + 
   scale_fill_manual("Rice Season", values = c("darkred", "darkblue"))
 p4
 
 
   pirateplot(formula =value ~key  ,data = pro2, ylab = "Sowing DOY",
           main = "Sowing dates distribution",
           theme =1,
           pal = "southpark",
           ylim = c(30 ,100),
           title = "Sowing dates distribution")
   
   p2
   
   
   
   
