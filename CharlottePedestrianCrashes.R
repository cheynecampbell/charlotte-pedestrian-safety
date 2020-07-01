# Cheyne Campbell 
# Charlotte, NC Pedestrian Safety
# UCL Dissertation 2020

#---------------------------------------------------------------------------------------------------------------

# GET LIBRARIES

library(sf)
library(sp)
library(rgdal)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(leaflet)
library(tmap)
library(spatstat)
library(maptools)
library(lubridate)
library(GEOmap)
library(scales)
library(purrr)
library(reshape2)
library(plyr)
library(wesanderson)

#---------------------------------------------------------------------------------------------------------------

# LOAD DATA

# get Charlotte, NC boundary
# SOURCE: http://maps.co.mecklenburg.nc.us/openmapping/data.html (Mecklenburg County Jurisdictions)
Mecklenburg <- st_read("./Data/Mecklenburg_Jurisdictions/Jurisdictions.shp")
Charlotte <- Mecklenburg[Mecklenburg$name == "Charlotte",]

# get wards in Uptown Charlotte
# SOURCE: https://data.charlottenc.gov/datasets/64dde23672004971a1b2dbf3a12f733e_13
Uptown <- st_read("./Data/Downtown_Wards/Downtown_Wards.shp")

# get pedestrian crashes in NC 2007 - 2018
# SOURCE: https://www.arcgis.com/home/item.html?id=801a34b6d3e64786a131d1fa2eca793d
crashData <- "./Data/NCDOT_BikePed_Crashes_2007_2018.gdb"
pedCrashDataSF <- sf::st_read(dsn = crashData, layer = "NCPed0718")

# extract Mecklenburg, Charlotte, and Uptown crash data
pedCrashChar   <- pedCrashDataSF[Charlotte,]
pedCrashUptown <- pedCrashDataSF[Uptown,]

#---------------------------------------------------------------------------------------------------------------

# GENERATE CITY OF CHARLOTTE, NC HEATMAP

# get basemap for the City of Charlotte
charExtent <- Charlotte %>% st_transform(4326) %>% as_Spatial() %>% bbox()
baseMapChar <- get_stamenmap(bbox = c(right = charExtent[1,1], left = charExtent[1,2], bottom = charExtent[2,1], top = charExtent[2,2]),
                               maptype = "terrain",
                               color = "bw",
                               crop = FALSE,
                               zoom = 13)

# save pedestrian crash heatmap for Uptown Charlotte
png(file = "./Figures/heatCharlotte.png", height = 6, width = 8, units = "in", res = 300)

heatChar <- ggmap(ggmap = baseMapChar)  
heatChar <- heatChar + geom_density2d(data = pedCrashChar, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), size = .25, bins = 20)
heatChar <- heatChar + stat_density2d(data = pedCrashChar, aes(x = as.numeric(Longitude), y = as.numeric(Latitude), fill = ..level.., alpha = ..level..), geom = "polygon", bins = 40)
heatChar <- heatChar + scale_alpha(range = c(0.10, 0.12), guide = FALSE)
heatChar <- heatChar + scale_fill_distiller(palette = 'YlOrRd', trans = "reverse")
heatChar <- heatChar + labs(x = "",
                                y = "",
                                title = "Crashes Involving Pedestrians",
                                subtitle = "City of Charlotte 2007-2018")
heatChar <- heatChar + scale_y_continuous(labels = function(x) paste0(x,"°N"), breaks = seq(35.00,	35.50, 0.10))
heatChar <- heatChar + scale_x_continuous(labels = function(x) paste0(x,"°W"), breaks = seq(-81.00, -80.70, 0.10))
heatChar <- heatChar + theme_minimal() + theme(legend.position = "none") 

plot(heatChar)

dev.off()

#---------------------------------------------------------------------------------------------------------------

# GENERATE UPTOWN CHARLOTTE, NC HEATMAP

# get basemap for Uptown Charlotte
uptownExtent <- Uptown %>% st_transform(4326) %>% as_Spatial() %>% bbox()
baseMapUptown <- get_stamenmap(bbox = c(right = uptownExtent[1,1], left = uptownExtent[1,2], bottom = uptownExtent[2,1], top = uptownExtent[2,2]),
                               maptype = "terrain",
                               color = "bw",
                               crop = FALSE,
                               zoom = 15)

# save pedestrian crash heatmap for Uptown Charlotte
png(file = "./Figures/heatUptown.png", height = 6, width = 8, units = "in", res = 300)

heatUptown <- ggmap(ggmap = baseMapUptown)  
heatUptown <- heatUptown + geom_density2d(data = pedCrashUptown, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), size = .25, bins = 20)
heatUptown <- heatUptown + stat_density2d(data = pedCrashUptown, aes(x = as.numeric(Longitude), y = as.numeric(Latitude), fill = ..level.., alpha = ..level..), geom = "polygon", bins = 40)
heatUptown <- heatUptown + scale_alpha(range = c(0.10, 0.12), guide = FALSE)
heatUptown <- heatUptown + scale_fill_distiller(palette = 'YlOrRd', trans = "reverse")
#heatUptown <- heatUptown + scale_fill_gradient2(low = "mediumseagreen", midpoint = 7500, mid = "gold", high = "orangered")
heatUptown <- heatUptown + labs(x = "",
                            y = "",
                            # fill = "Number of Crashes",
                            title = "Crashes Involving Pedestrians",
                            subtitle = "Uptown Charlotte 2007-2018")
heatUptown <- heatUptown + scale_y_continuous(labels = function(x) paste0(x,"°N"), breaks = seq(35.210, 35.245, 0.01))
heatUptown <- heatUptown + scale_x_continuous(labels = function(x) paste0(x,"°W"), breaks = seq(-80.870, -80.830, 0.01))
heatUptown <- heatUptown + theme_minimal() + theme(legend.position = "none") 

plot(heatUptown)

dev.off()

#---------------------------------------------------------------------------------------------------------------
























































































png(file = "/Users/cheynecampbell/Desktop/dissertation_code/CharlottePedestrianCrashes/crashMeckMap.png", height = 6, width = 8, units = "in", res = 300)

crashMeckMap <- ggplot()
crashMeckMap <- crashMeckMap + geom_sf(data = Uptown) 
crashMeckMap <- crashMeckMap + geom_sf(data = pedCrashUptown, size = 0.2, aes(color = PedInjury))
crashMeckMap <- crashMeckMap + labs(x = "",
                                    y = "",
                                    fill = "Number of Crashes",
                                    title = "Crashes Involving Pedestrians",
                                    subtitle = "Mecklenburg County 2007-2018")
crashMeckMap <- crashMeckMap + theme_bw()

crashMeckMap

dev.off()



#---------------------------------------------------------------------------------------------------------------

# make pie charts for each of the following variables:
variableList <- c("PedRace", "PedInjury", "LightCond", "CrashType", "RdClass", "PedAgeGrp", "CrashYear")
variableNames <- c("Pedestrian Race", "Pedestrian Injury", "Lighting Condition", "Crash Type", "Road Class", "Pedestrian Age Group", "Crash Year")

# function to make pie charts
makePieChart <- function(variable, varName) {
  
  # save pie chart as PNG
  png(file = paste("/Users/cheynecampbell/Desktop/dissertation_code/CharlottePedestrianCrashes/", variable, ".png", sep = ""), height = 6, width = 8, units = "in", res = 300)
  
  # get counts for each value of variable
  variableValueCounts <- count(pedCrashMeck, variable)
  names(variableValueCounts)[1] <- "value"
  
  # make pie chart 
  pieChart <- ggplot(variableValueCounts, aes(x = "", y = freq, fill = value)) + 
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start=0) +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = paste("Crashes Involving Pedestrians: ", varName, sep = ""),
         subtitle = "Mecklenburg County 2007-2018",
         x = "",
         y = "",
         fill = "")
  
  # plot pie chart
  plot(pieChart)
  
  # save pie chart
  dev.off()
}

# make a pie chart for each variable
for (i in 1:length(variableList)) {
  print(i)
  makePieChart(variableList[i], variableNames[i])
}

#---------------------------------------------------------------------------------------------------------------

# pedestrian exposure modeling 
# predestrian count data from Eco Counters 
# SOURCE: https://itre.ncsu.edu/focus/bike-ped/nc-nmvdp/

#---------------------------------------------------------------------------------------------------------------

# get heat map using kernel density estimation - spatstat package

# MeckSP <- as_Spatial(Mecklenburg)
# windowMeck <- as.owin(MeckSP)
# plot(windowMeck)

# pedCrashMeckSP <- as_Spatial(pedCrashMeck)
# pedCrashMeckSP <- remove.duplicates(pedCrashMeckSP)
# pedCrashMeck.ppp <- ppp(x = pedCrashMeckSP@coords[,1], y = pedCrashMeckSP@coords[,2], window = windowMeck)

# plot(pedCrashMeck.ppp, pch = 16, cex = 0.5, main = "Pedestrian Crashes in Mecklenburg")
# plot(density(pedCrashMeck.ppp, sigma = 5000))



# map crash data 
#theme_set(theme_bw())

#tmap_mode("plot")

#tm_basemap(leaflet::providers$Stamen.TonerHybrid) +
#  tm_shape(Mecklenburg) +
#  tm_polygons(col = NA, alpha = 0.5) +
#  tm_shape(pedCrashMeck) +
#  tm_dots(col = "blue")






