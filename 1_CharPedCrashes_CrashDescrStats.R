# Cheyne Campbell 
# Charlotte, NC Pedestrian Safety
# UCL Dissertation 2020

# PEDESTRIAN CRASH VISUALIZATIONS AND DESCRIPTIVE STATS

#---------------------------------------------------------------------------------------------------------------

# GET LIBRARIES

library(sf)
library(sp)
library(rgeos)
library(ggmap)
library(ggplot2)
library(ggcorrplot)
library(cowplot)
library(ggpubr)
library(plyr)
library(maptools)
library(mapview)
library(dplyr)
library(GISTools)
library(spatialEco)
library(ggsci)
library(gdata)
library(party)
library(partykit)
library(tidygraph)
library(MASS)
library(stats)
library(rlist)
library(randomForest)

#---------------------------------------------------------------------------------------------------------------

# LOAD SOME DATA

# get Charlotte, NC boundary
# SOURCE: http://maps.co.mecklenburg.nc.us/openmapping/data.html (Mecklenburg County Jurisdictions)
Mecklenburg <- st_read("./Data/Mecklenburg_Jurisdictions/Jurisdictions.shp")
Charlotte <- Mecklenburg[Mecklenburg$name == "Charlotte",]

# get wards in Uptown Charlotte
# SOURCE: https://data.charlottenc.gov/datasets/64dde23672004971a1b2dbf3a12f733e_13
Uptown <- st_read("./Data/Downtown_Wards/Downtown_Wards.shp")

# get pedestrian crashes in NC 2007 - 2018
# SOURCE: https://www.arcgis.com/home/item.html?id=801a34b6d3e64786a131d1fa2eca793d
pedCrashData <- "./Data/NCDOT_BikePed_Crashes_2007_2018.gdb"
pedCrashDataSF <- sf::st_read(dsn = pedCrashData, layer = "NCPed0718")

# extract Charlotte and Uptown crash data
pedCrashChar <- pedCrashDataSF[Charlotte,]
pedCrashUptown <- pedCrashDataSF[Uptown,]

# get Uptown road segments 
# SOURCE: https://www.nconemap.gov/datasets/NCDOT::ncdot-road-characteristics/data?geometry=-80.856%2C35.222%2C-80.825%2C35.228
# ATTRIBUTES: https://xfer.services.ncdot.gov/gisdot/DistDOTData/NCDOTRouteCharacteristicsFieldDescriptions.pdf
roadsUptown <- st_read("./Data/Uptown_Road_Characteristics/Uptown_RoadCharacteristics.shp")

# convert street names to upper case
roadsUptown$StreetName <- toupper(roadsUptown$StreetName)

# filter out interstates and ramps (and straggling segments)
roadsUptownFilt <- roadsUptown[roadsUptown$RouteClass != 1 & roadsUptown$RouteClass != 80 &
                                 roadsUptown$OBJECTID != 1048242 & roadsUptown$OBJECTID != 1020257, ]

# figure out how many roads have duplicates - segments are not necessarily stored at the block level; 
# occassionally attribute values change mid block 
roadsUnique <- roadsUptownFilt %>% as.data.frame() %>% 
  dplyr::select(MedianType, RtShldrWid, RtShldrTyp, LftShldrWi, LftShldrTy, TravelDire, StreetName) %>%
  unique()

# calculate lengths of segments in feet
roadsUptownFilt$variable_cont_length_ft <- as.numeric(st_length(roadsUptownFilt))

#---------------------------------------------------------------------------------------------------------------

# INSET MAP LOCATING THE STUDY AREA WITHIN THE UNITED STATES

# get coordinates for centroid of Charlotte
charCenter <- Charlotte %>% st_transform(4326) %>% as_Spatial() %>% gCentroid() %>% as.data.frame()

# get USA basemap tiles
baseMapUSA <- get_stamenmap(bbox = c(right = -125.784, left = -65.897, bottom = 24.942, top = 49.867),
                             maptype = "terrain",
                             color = "bw",
                             crop = FALSE,
                             zoom = 6)

# generate USA study area inset map
studyAreaUSA <- ggmap(ggmap = baseMapUSA) + 
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey40", size = 0.25) + 
  geom_point(data = charCenter, aes(x = x, y = y), alpha = 1, size = 2, color = "red") + 
  labs(x = NULL, y = NULL) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  theme_void() + theme(panel.border = element_rect(colour = "grey40", fill = NA, size = 0.75))

# check output
studyAreaUSA
dev.off()

#---------------------------------------------------------------------------------------------------------------

# GENERATE CITY OF CHARLOTTE, NC HEATMAP WITH INSET MAP INCLUDED

# get basemap tiles for the City of Charlotte
charExtent <- Charlotte %>% st_transform(4326) %>% as_Spatial() %>% bbox()
baseMapChar <- get_stamenmap(bbox = c(right = charExtent[1,1], left = charExtent[1,2], bottom = charExtent[2,1], top = charExtent[2,2]),
                               maptype = "terrain",
                               color = "bw",
                               crop = FALSE,
                               zoom = 13)

# generate City of Charlotte heatmap
heatChar <- ggmap(ggmap = baseMapChar) + 
  geom_density2d(data = pedCrashChar, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), size = .25) + 
  stat_density2d(data = pedCrashChar, aes(x = as.numeric(Longitude), y = as.numeric(Latitude), fill = ..level.., alpha = ..level..), geom = "polygon") +
  # uncomment to display crashes as points on map
  # geom_point(data = pedCrashChar, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), alpha = 0.25, size = 0.25, shape = 16, color = "magenta4") +
  scale_alpha(range = c(0.20, 0.40), guide = FALSE) +
  scale_fill_distiller(palette = 'YlOrRd', trans = "reverse") +
  labs(x = "", y = "", fill = "Relative\nDensity") +
  scale_y_continuous(labels = function(x) paste0(x,"°N"), breaks = seq(35.00, 35.50, 0.10)) +
  scale_x_continuous(labels = function(x) paste0(x,"°W"), breaks = seq(-81.00, -80.70, 0.10)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# check output
heatChar
dev.off()

# save heatmap for City of Charlotte with USA inset 
png(file = "./Figures/heatCharlotte.png", height = 6, width = 8, units = "in", res = 300)

# generate City of Charlotte heatmap with inset
charInsetMap <-
  ggdraw() +
  draw_plot(heatChar) +
  draw_plot(studyAreaUSA, x = 0.65, y = 0.14, width = .25, height = .25)

# check output
plot(charInsetMap)
dev.off()

#---------------------------------------------------------------------------------------------------------------

# GENERATE UPTOWN CHARLOTTE, NC HEATMAP

# get basemap tiles for Uptown Charlotte
uptownExtent <- Uptown %>% st_transform(4326) %>% as_Spatial() %>% bbox()
baseMapUptown <- get_stamenmap(bbox = c(right = uptownExtent[1,1], left = uptownExtent[1,2], bottom = uptownExtent[2,1], top = uptownExtent[2,2]),
                               maptype = "terrain",
                               color = "bw",
                               crop = FALSE,
                               zoom = 15)

# save pedestrian crash heatmap for Uptown Charlotte
png(file = "./Figures/heatUptown.png", height = 6, width = 8, units = "in", res = 300)

# generate Uptown heatmap
heatUptown <- ggmap(ggmap = baseMapUptown) + 
  geom_density2d(data = pedCrashUptown, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), size = .25) + 
  stat_density2d(data = pedCrashUptown, aes(x = as.numeric(Longitude), y = as.numeric(Latitude), fill = ..level.., alpha = ..level..), geom = "polygon") +
  # uncomment to display crashes as points on map
  # heatUptown <- heatUptown + geom_point(data = pedCrashUptown, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), alpha = 0.25, size = 0.25, shape = 16, color = "magenta4") +
  scale_alpha(range = c(0.20, 0.40), guide = FALSE) + 
  scale_fill_distiller(palette = 'YlOrRd', trans = "reverse") + 
  labs(x = "", y = "", fill = "Relative\nDensity") + 
  scale_y_continuous(labels = function(x) paste0(x,"°N"), breaks = seq(35.210, 35.245, 0.01)) + 
  scale_x_continuous(labels = function(x) paste0(x,"°W"), breaks = seq(-80.870, -80.830, 0.01)) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# check output
heatUptown
dev.off()

#---------------------------------------------------------------------------------------------------------------

# HEATMAP OF DAY AND TIME OF PEDESTRIAN CRASHES 

# save day and time heatmap
png(file = "./Figures/crashDayTime.png", height = 6, width = 12, units = "in", res = 300)

# set up frequency dataframe for day and time heatmap
crashDayTime <- pedCrashUptown[c("CrashDay", "CrashHour")] %>% as.data.frame()
crashDayTime <- plyr::ddply(crashDayTime, .variables = c('CrashHour','CrashDay'), nrow)

# generate day and time heatmap
heatDayTime <- ggplot(crashDayTime, aes(CrashHour, CrashDay)) + 
  geom_tile(aes(fill = V1), color = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "lightgoldenrod1", high = "red3") +  
  guides(fill = guide_legend(title = "Total\nPedestrian\nCrashes")) +
  theme_bw() + theme_minimal() + 
  labs(x = "", y = "") +
  scale_y_discrete(limits = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")) +
  scale_x_discrete(limits = 0:23, labels = c("12AM\n-1AM", "1AM\n-2AM","2AM\n-3AM", "3AM\n-4AM", "4AM\n-5AM", "5AM\n-6AM", 
                                             "6AM\n-7AM", "7AM\n-8AM", "8AM\n-9AM", "9AM\n-10AM","10AM\n-11AM", "11AM\n-12PM", 
                                             "12PM\n-1PM", "1PM\n-2PM", "2PM\n-3PM", "3PM\n-4PM", "4PM\n-5PM", "5PM\n-6PM", 
                                             "6PM\n-7PM", "7PM\n-8PM", "8PM\n-9PM", "9PM\n-10PM", "10PM\n-11PM", "11PM\n-12AM")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# check output
heatDayTime
dev.off()

#---------------------------------------------------------------------------------------------------------------

# BAR CHART OF TYPES OF PEDESTRIAN CRASHES

# save bar chart of types
png(file = "./Figures/crashTypesHist.png", height = 6, width = 8, units = "in", res = 300)

# set up crash type dataframe with percentages
crashTypeHist <- pedCrashUptown["CrashType"] %>% as.data.frame()
crashTypeHist <- plyr::ddply(crashTypeHist, .variables = "CrashType", nrow)
crashTypeHist$perc <- (crashTypeHist$V1 / sum(crashTypeHist$V1)) * 100
crashTypeHist <- crashTypeHist[order(crashTypeHist$V1),]
crashTypeHist <- crashTypeHist[(crashTypeHist$V1 >= 15),]

# adjust crash type labels
crashTypeHist$CrashType <- gsub(" - ", " ", crashTypeHist$CrashType)
crashTypeHist$CrashType <- gsub(" ", "\n", crashTypeHist$CrashType)

# generate bar plot
ggplot(data = crashTypeHist, aes(x = reorder(CrashType, V1), y = V1, fill = CrashType)) +
  scale_fill_jco() + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Number of Crashes") +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste(round(perc), "%", sep = "")), vjust = 1.6, color = "white", size = 3.5) +
  theme_linedraw() +
  theme(panel.grid.major.x = element_blank())

dev.off()

#---------------------------------------------------------------------------------------------------------------

# EXPLORATORY BAR CHARTS - VARIABLES FROM PEDESTRIAN CRASH DATA

# function to make pie charts
makeBarChart <- function(variable, varName) {
  
  # get counts for each value of variable
  variableValueCounts <- plyr::count(pedCrashUptown, variable)
  variableValueCounts$perc <- (variableValueCounts$freq / sum(variableValueCounts$freq)) * 100
  names(variableValueCounts)[1] <- "value"
  
  # make bar chart 
  barChart <- ggplot(variableValueCounts, aes(x = value, y = perc, fill = value)) + 
    geom_bar(stat = "identity", show.legend = TRUE) +
    scale_fill_jco(labels = paste(variableValueCounts$value, " - ", round(variableValueCounts$perc), "%", sep = "")) +
    #scale_fill_brewer(palette = "Dark2", labels = paste(variableValueCounts$value, " - ", round(variableValueCounts$perc), "%", sep = "")) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = NULL, x = NULL, y = NULL, fill = varName) +
    theme_linedraw() + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.key.size = unit(0.10, "in"), legend.text = element_text(size = 6),
          legend.position = "bottom", legend.direction = "vertical")
  
  # return pie chart
  print(barChart)
}

# edit pedestrian injury labels
pedCrashUptown$PedInjury <- gsub(".*: ", "", pedCrashUptown$PedInjury)

# make pie charts for each of the following variables:
variableList <- c("PedInjury", "PedPos", "CrashLoc", "RdClass", "RdConfig", "LightCond")
variableNames <- c("Pedestrian Injury", "Pedestrian Position", "Crash Location", "Road Class", "Road Configuration", "Lighting Condition")

# save pie charts in list 
barCharts <- list()

# make a pie chart for each variable
for (i in 1:length(variableList)) {
  print(i)
  barCharts[[i]] <- makeBarChart(variableList[i], variableNames[i])
}

# save pie charts as one image
png(file = "./Figures/exploratoryBarCharts.png", height = 9, width = 6, units = "in", res = 300)
ggarrange(plotlist = barCharts, ncol = 2, nrow = 3)
dev.off()

#---------------------------------------------------------------------------------------------------------------

# REMOVE UNEEDED VARIABLES FROM ENVIRONMENT

rm(charCenter, charExtent, charInsetMap, Charlotte, crashDayTime, crashTypeHist, heatChar, 
   heatDayTime, heatUptown, Mecklenburg, pedCrashChar, pedCrashDataSF, barCharts, roadsUnique, 
   roadsUptown, studyAreaUSA, uptownExtent, baseMapUSA, baseMapChar, baseMapUptown, i, pedCrashData, 
   variableList, variableNames, makeBarChart)

dev.off()

