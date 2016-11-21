
library(maptools)
library(mapproj)
library(dplyr)
library(rgdal)
library(raster)

# Maptools dataset
data(wrld_simpl)
world <- fortify(wrld_simpl)

worldmerc <-  SpatialPointsDataFrame(coords = data_frame(x = world$long, y = world$lat),
                                data = world, proj4string = CRS("+init=epsg:4326")) %>%
         subset((lat < 85 & lat > -85)) %>%   # needed because transform not defined at the poles !!!!
         spTransform(CRS("+init=epsg:3035"))
worldmerc  <-  mutate(worldmerc@data, longmerc = coordinates(worldmerc)[,1], latmerc = coordinates(worldmerc)[,2])

#for africa
xlim = c(-10, 34.5)
ylim = c(34.5, 71)
-10.6700, 34.5000, 31.5500, 71.0500
# Get the coordinates of the limits in mercator projection
lims = SpatialPoints(coords = data_frame(x = xlim, y = ylim),
                     proj4string = CRS("+init=epsg:4326"))%>%
       spTransform(CRS("+init=epsg:3035"))

# Create regular "grids" of latlon coordinates and find points
# within xlim/ylim - will be our labels

majgrid_wid_lat = 20
majgrid_wid_lon = 30

majbreaks_lon = data_frame(x=seq(-180,  180, majgrid_wid_lon)) %>%
                filter(x >= xlim[1] & x <= xlim[2]) %>%
                as.data.frame()
majbreaks_lat = data_frame(x=seq(-90,   90, majgrid_wid_lat)) %>%
                filter(x >= ylim[1] & x <= ylim[2]) %>%
                as.data.frame()

#Find corresponding mercator coordinates

mercbreaks_lat = SpatialPoints(coords = expand.grid(x = majbreaks_lon$x, y = majbreaks_lat$x), proj4string = CRS("+init=epsg:4326"))%>%
  spTransform(CRS("+init=epsg:3035")) %>% coordinates() %>% extract(,2) %>% unique()
mercbreaks_lon = SpatialPoints(coords = expand.grid(x = majbreaks_lon$x, y = majbreaks_lat$x), proj4string = CRS("+init=epsg:4326"))%>%
  spTransform(CRS("+init=epsg:3035")) %>% coordinates()  %>% extract(,1) %>% unique()

# Plot using mercator coordinates, but latlon labels

ggplot(worldmerc, mapping = aes(x = longmerc, y = latmerc, group = group)) +
  geom_polygon(fill = "black", colour = "black") +
  coord_fixed(xlim = coordinates(lims)[,1], ylim = coordinates(lims)[,2])+
  scale_x_continuous("lon", breaks = mercbreaks_lon, labels = signif(majbreaks_lon$x, 2)) +
  scale_y_continuous("lat", breaks = mercbreaks_lat, labels = signif(majbreaks_lat$x,2))+theme_bw()



minbreaks_lon = data_frame(x=seq(-180,  180, mingrid_wid_lon)) %>%
                filter(x >= xlim[1] & x <= xlim[2]) %>%
                as.data.frame()
minbreaks_lat = data_frame(x=seq(-90,   90, mingrid_wid_lat)) %>%
                filter(x >= xlim[1] & x <= xlim[2]) %>%
                as.data.frame()

ggplot(worldmerc, mapping = aes(x = longmerc, y = latmerc, group = group)) +
  geom_polygon(fill = "black", colour = "black") +
  coord_fixed(xlim = coordinates(lims)[,1], ylim = coordinates(lims)[,2])+
  scale_x_continuous("lon", breaks = mercbreaks_lon, labels = signif(majbreaks_lon$x, 2), minor_breaks=mercminbreaks_lon ) +
  scale_y_continuous("lat", breaks = mercbreaks_lat, labels = signif(majbreaks_lat$x,2), minor_breaks=mercminbreaks_lat )+theme_bw()



mercminbreaks_lat = SpatialPoints(coords = expand.grid(x = minbreaks_lon$x, y = minbreaks_lat$x), proj4string = CRS("+init=epsg:4326"))%>%
  spTransform(CRS("+init=epsg:3035")) %>% coordinates() %>% extract(,2) %>% unique()
mercminbreaks_lon = SpatialPoints(coords = expand.grid(x = minbreaks_lon$x, y = minbreaks_lat$x), proj4string = CRS("+init=epsg:4326"))%>%
  spTransform(CRS("+init=epsg:3035")) %>% coordinates()  %>% extract(,1) %>% unique()




mingrid_lat = seq(-90,  90, mingrid_wid_lon)
majgrid_lat = seq(-90, 90, majgrid_wid_lat)

majbreaks_lon = majgrid_lon[which(majgrid_lon >= xlim[1] & majgrid_lon <= xlim[2])]
majbreaks_lat = majgrid_lat[which(majgrid_lat >= ylim[1] & majgrid_lat <= ylim[2])]

minbreaks_lon = mingrid_lon[which(mingrid_lon >= xlim[1] & mingrid_lon <= xlim[2])]
minbreaks_lat = mingrid_lat[which(mingrid_lat >= ylim[1] & mingrid_lat <= ylim[2])]

breaks_lon = unique(coordinates(majgrid_merc)[,1])
minbreaks_lat = unique(coordinates(mingrid_merc)[,2])
minbreaks_lon = unique(coordinates(mingrid_merc)[,1])



minbreaks_lat = which(mingrid_lat >= ylim[1] & mingrid_lat <= ylim[2])
minbreaks_lon = which(mingrid_lon >= xlim[1] & mingrid_lon <= xlim[2])

majgrid_merc = SpatialPoints(coords = expand.grid(x = majbreaks_lon, y = majbreaks_lat), proj4string = CRS("+init=epsg:4326"))%>%
  spTransform(CRS("+init=epsg:3035"))
mingrid_merc = SpatialPoints(coords = expand.grid(x = minbreaks_lon, y = minbreaks_lat), proj4string = CRS("+init=epsg:4326"))%>%
  spTransform(CRS("+init=epsg:3035"))





break_wid_x = (extent(lims)@xmax- extent(lims)@xmin)/50  # Define some regular breaks - you may want to chage "4" to something else
break_wid_y = (extent(lims)@ymax- extent(lims)@ymin)/50
breaks_x = c(extent(lims)@xmin, extent(lims)@xmin + seq(1:49)*break_wid_x)
breaks_y = c(extent(lims)@ymin ,extent(lims)@ymin + seq(1:49)*break_wid_y)

# Find the lat/lon coordinates corresponding to those breaks
breaks_latlon = SpatialPoints(coords = data_frame(x = breaks_x, y = breaks_y), proj4string = CRS("+init=epsg:3035"))%>%
  spTransform(CRS("+init=epsg:4326"))
latlonlines = data_frame(x = breaks_x, y = breaks_y)
# plot using mercator coordinates breaks, but lat/lon labels
latlongrid = expand.grid(latlonlines)
ggplot(worldmerc, mapping = aes(x = longmerc, y = latmerc, group = group)) +
  geom_polygon(fill = "black", colour = "black") +
  coord_fixed(xlim = coordinates(lims)[,1], ylim = coordinates(lims)[,2])+
  scale_x_continuous("lon", breaks = breaks_x, labels = signif(coordinates(breaks_latlon)[,1], 2)) +
  scale_y_continuous("lat", breaks = breaks_y, labels = signif(coordinates(breaks_latlon)[,2],2))

