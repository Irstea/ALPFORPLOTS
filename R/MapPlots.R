## Map plot with inset this only work with the private GIS data available


## This function is based on http://r-nold.blogspot.fr/2014/06/creating-inset-map-with-ggplot2.html
mapplots <- function(df){

require(grid)
require(gridExtra)
require(dplyr)
 require(raster)
 require(rgdal)
  require("maptools")
  require("ggplot2")
 path_dem <- "/run/user/1001/gvfs/smb-share:server=syno,share=ign/BD_ALTI/500m/mnt_500m_rgf"
 path_limits <- "/run/user/1001/gvfs/smb-share:server=syno,share=ign/BD_GEOFLA/BD_GEOFLA07/SHP"

 france <- readOGR(path_limits,
                   "FRANCE_2006_GEOFLA",
                   verbose = FALSE)
 france@data$id = rownames(france@data)
 france.points = fortify(france, region="id",
                         verbose = FALSE)
 france.df = left_join(france.points, france@data, by="id")

 depart <- readOGR(path_limits,
                   "DEPART_2006_FR_C_GEOFLA",
                   verbose = FALSE)
 depart@data$id = rownames(depart@data)
 depart.points = fortify(depart, region="id", verbose = FALSE)
 depart.df = left_join(depart.points, depart@data, by="id")


 dem <- raster(file.path(path_dem, "w001001.adf"))
 dem.c <- mask(dem, france)
 dem_p <- rasterToPoints(dem.c)
 dem_df <- as.data.frame(dem_p)
 colnames(dem_df) <- c("Longitude", "Latitude", "Elevation")

p1 <- ggplot() +
  geom_raster(data = dem_df, aes(y = Latitude, x = Longitude, fill = Elevation)) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_path(data= depart.df, aes(long, lat, group = group), colour = "black") +
  geom_path(data = depart.df[depart.df$CODE_DEP %in% c("01", "38", "73", "74", "04", "05", "06", "26"), ],
            aes(long, lat, group = group), colour = "blue")+
  geom_point(data=df, aes(x=x_lamb93, y=y_lamb93), color="red", size=3) +
  xlim(740000, 1040000) +
  ylim(6350000, 6600000) +
  coord_equal() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank())   +
  scale_bar(lon = 741500, lat = 6361500,
            distance_lon = 20000, distance_lat = 5000,
            dist_unit = "m", distance_legend = 10000, arrow_length = 20000,
            arrow_distance = 12000)

p2 <- ggplot() +
  geom_path(data= depart.df, aes(long, lat, group = group), colour = "black") +
  geom_path(data = depart.df[depart.df$CODE_DEP %in% c("01", "38", "73", "74", "04", "05", "06", "26"), ],
            aes(long, lat, group = group), colour = "blue")+
  coord_equal() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))+
  theme_bw() +
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(),
        axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.ticks.margin = unit(0, "lines"),
        axis.title.y= element_blank(),
        plot.margin =  unit(rep(0, 4), "lines"))

# Extent rectangle for inset map
pol<-data.frame(xmin=750000,xmax= 8700000 ,ymin=6450000 ,ymax=6600000)



## png(file=file.path('figures', 'map_plots.png'),w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.3, x = 0.20, y = 0.69) #plot area for the inset map
print(p1,vp=v1)
print(p2,vp=v2)
## dev.off()

}



## Taken from https://github.com/3wen/legendMap/blob/master/R/legende_cartes.R

#
# Result #
#--------#
# Return a list whose elements are :
# 	- rectangle : a data.frame containing the coordinates to draw the first rectangle ;
# 	- rectangle2 : a data.frame containing the coordinates to draw the second rectangle ;
# 	- legend : a data.frame containing the coordinates of the legend texts, and the texts as well.
#
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distance_lon : length of each rectangle ;
# distance_lat : width of each rectangle ;
# distance_legend : distance between rectangles and legend texts ;
# dist_units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
	rectangle <- cbind(lon = c(lon, lon, lon + distance_lon, lon + distance_lon, lon),
                           lat = c(lat, lat + distance_lat , lat + distance_lat ,lat, lat))
	rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)

	# Second rectangle t right of the first rectangle
	rectangle2 <- cbind(lon = c(lon + distance_lon, lon + distance_lon, lon + 2*distance_lon,
                                   lon + 2*distance_lon, lon+ distance_lon),
                           lat = c(lat, lat + distance_lat , lat + distance_lat ,lat, lat))
	rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)

	# Now let's deal with the text
	legend <- cbind(lon = c(lon , lon + distance_lon,  lon + 2*distance_lon),
                        lat = c(lat + distance_legend, lat + distance_legend, lat + distance_legend))

	legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)),
                             stringsAsFactors = FALSE, row.names = NULL)
	return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

#
# Result #
#--------#
# Returns a list containing :
#	- res : coordinates to draw an arrow ;
#	- coordinates of the middle of the arrow (where the "N" will be plotted).
#
                                        # Arguments : #
#-------------#
# scale_bar : result of create_scale_bar() ;
# length : desired length of the arrow ;
# distance : distance between legend rectangles and the bottom of the arrow ;
# dist_units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
create_orientation_arrow <- function(scale_bar, length, distance , dist_units = "km"){
	lon <- scale_bar$rectangle2[1,1]
	lat <- scale_bar$rectangle2[1,2]

	res <- rbind(
			cbind(x = lon, y = lat+distance, xend = lon, yend = lat + length + distance),
			cbind(x = lon + sin(pi/4) * length/5, y = lat+length+distance - cos(pi/4) * length/5,
                              xend = lon, yend = lat + length + distance),
			cbind(x = lon - sin(pi/4) * length/5, y = lat+length+distance - cos(pi/4) * length/5,
                              xend = lon, yend = lat + length + distance)
                    )

	res <- as.data.frame(res, stringsAsFactors = FALSE)

	# Coordinates from which "N" will be plotted
	coords_n <- cbind(x = lon, y = lat + distance +length/2)
	return(list(res = res, coords_n = coords_n))
}


#
# Result #
#--------#
# This function enables to draw a scale bar on a ggplot object, and optionally an orientation arrow #
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distance_lon : length of each rectangle ;
# distance_lat : width of each rectangle ;
# distance_legend : distance between rectangles and legend texts ;
# dist_units : units of distance "km" (kilometers) (by default), "nm" (nautical miles), "mi" (statute miles) ;
# rec_fill, rec2_fill : filling colour of the rectangles (default to white, and black, resp.);
# rec_colour, rec2_colour : colour of the rectangles (default to black for both);
# legend_colour : legend colour (default to black);
# legend_size : legend size (default to 3);
# orientation : (boolean) if TRUE (default), adds an orientation arrow to the plot ;
# arrow_length : length of the arrow (default to 500 km) ;
# arrow_distance : distance between the scale bar and the bottom of the arrow (default to 300 km) ;
# arrow_north_size : size of the "N" letter (default to 6).
scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend,
                      arrow_length, arrow_distance ,
                      dist_unit = "km", rec_fill = "white", rec_colour = "black",
                      rec2_fill = "black", rec2_colour = "black", legend_colour = "black",
                      legend_size = 2, orientation = TRUE,arrow_north_size = 5){
	the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon,
                                          distance_lat = distance_lat, distance_legend = distance_legend,
                                          dist_unit = dist_unit)
	# First rectangle
	rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat),
                                   fill = rec_fill, colour = rec_colour)

	# Second rectangle
	rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat),
                                   fill = rec2_fill, colour = rec2_colour)

	# Legend
	scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=" "),
                                     x = the_scale_bar$legend[,"lon"], y = the_scale_bar$legend[,"lat"],
                                     size = legend_size, colour = legend_colour)

	res <- list(rectangle1, rectangle2, scale_bar_legend)

	if(orientation){# Add an arrow pointing North
		coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length,
                                                         distance = arrow_distance, dist_unit = dist_unit)
		arrow <- list(geom_segment(data = coords_arrow$res,
                                           aes(x = x, y = y, xend = xend, yend = yend)),
                              annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"],
                                       y = coords_arrow$coords_n[1,"y"], size = arrow_north_size,
                                       colour = "black"))
		res <- c(res, arrow)
	}
	return(res)
}
