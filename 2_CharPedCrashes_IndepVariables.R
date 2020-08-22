# Cheyne Campbell 
# Charlotte, NC Pedestrian Safety
# UCL Dissertation 2020

# INDEPENDENT VARIABLE PREPARATION AND VISUALIZATION

#---------------------------------------------------------------------------------------------------------------

# PREPARE ROAD SEGMENTS

# edit proj4string of road segments to match other data (and save original)
roadsUptownFiltSF <- roadsUptownFilt
roadsUptownFilt <- spTransform(as_Spatial(roadsUptownFilt), CRS("+proj=lcc +lat_1=36.16666666666666 +lat_2=34.33333333333334 +lat_0=33.75 +lon_0=-79 +x_0=609601.2192024384 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")) 

# create buffer polygons around road segments to count points
roadsUptownBuffer <- gBuffer(roadsUptownFilt, byid = TRUE, width = 5, capStyle = "ROUND", joinStyle = "ROUND")

#---------------------------------------------------------------------------------------------------------------

# FUNCTION - snapPointsToLines1 (retains attributes of snapped points)
# SOURCE: http://geotux.tuxfamily.org/index.php/en/geo-blogs/item/296-snapping-points-to-lines-in-r

snapPointsToLines1 <- function(points, lines, maxDist=NA, withAttrs=TRUE) {
  
  require("rgeos")
  
  if (!is.na(maxDist)) {
    w = gWithinDistance(points, lines, dist=maxDist, byid=TRUE)
    validPoints = apply(w,2,any)
    validLines = apply(w,1,any)
    points = points[validPoints,]
    lines =  lines[validLines,]
  }
  
  d = gDistance(points, lines, byid=TRUE) 
  nearest_line_index = apply(d, 2, which.min) # Position of each nearest line in lines object 
  
  coordsLines = coordinates(lines)  
  coordsPoints = coordinates(points)  
  
  # Get coordinates of nearest points lying on nearest lines
  mNewCoords = vapply(1:length(points), 
                      function(x) 
                        nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                           coordsPoints[x,]), FUN.VALUE=c(0,0))
  
  # Recover lines' Ids (Ids and index differ if maxDist is given)
  if (!is.na(maxDist)) nearest_line_id = as.numeric(rownames(d)[nearest_line_index])+1 
  else nearest_line_id = nearest_line_index 
  
  # Create data frame and sp points
  if (withAttrs) df = cbind(points@data, nearest_line_id) 
  else df = data.frame(nearest_line_id, row.names=names(nearest_line_index))
  
  SpatialPointsDataFrame(coords=t(mNewCoords), data=df, 
                         proj4string=CRS(proj4string(points)))
}

#---------------------------------------------------------------------------------------------------------------

# FUNCTIONS TO PLOT AND MAP DATA

# make point plot of original data 
pointMap <- function(pointData, pointDescr){
  varPointMap <- ggplot() +
    geom_sf(data = st_as_sf(roadsUptownFilt), color = "darkgrey", size = 0.25) +
    geom_sf(data = st_as_sf(pointData), aes(color = "a", alpha = 0.5), size = 1.0, shape = 16, key_glyph = draw_key_point) +
    scale_color_manual(values = c(a = "#D53E4F"), labels = c(pointDescr)) +
    scale_alpha(guide = "none") +
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 4),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.justification = c(0, 0), legend.position = c(0, 0),
          legend.box.margin = ggplot2::margin(c(10,10,10,10)))
  
  # check output
  print(varPointMap)
}

# road plot of continuous segment variables 
networkMapCont <- function(variable, variableDescr) {
  variableMap <- ggplot() +
    geom_sf(data = st_as_sf(roadsUptownFilt), aes(color = variable, size = variable)) + 
    scale_size(range = c(0.5, 1.0), guide = "none") + 
    scale_fill_distiller(palette = "Spectral", aesthetics = c("colour", "fill"), 
                         guide = guide_colorbar(barheight = 2.5, ticks = FALSE), na.value = "darkgrey") +
    labs(colour = variableDescr) + 
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank(), 
          axis.text = element_text(size = 4),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 7),
          legend.justification = c(0, 0), legend.position = c(0, 0),
          legend.box.margin = ggplot2::margin(c(10,10,10,10)))
  
  # check output
  print(variableMap)
}

# road plot of discrete segment variables
networkMapDiscr <- function(variable, variableDescr) {
  variableMap <- ggplot() +
    geom_sf(data = st_as_sf(roadsUptownFilt), color = "darkgrey", size = 0.25, show.legend = "line") +
    geom_sf(data = st_as_sf(roadsUptownFilt), aes(color = variable), size = 0.75, show.legend = "line") + 
    scale_color_npg() +
    labs(colour = variableDescr) + 
    theme_classic() + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.line = element_blank(),
          axis.ticks = element_blank(), 
          axis.text = element_text(size = 4),
          legend.background = element_rect(fill = "transparent"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.75, "lines"),
          legend.justification = c(0, 0), legend.position = c(0, 0),
          legend.box.margin = ggplot2::margin(c(10,10,10,10)))
  
  # check output
  print(variableMap)
}

# distribution plot of data 
distPlot <- function(variable, variableDescr, binw){
  distPlot <- ggplot(st_as_sf(roadsUptownFilt), aes(x = variable)) + 
    geom_histogram(aes(y=..count..), binwidth = binw, fill = "#D53E4F") +
    labs(title = NULL, x = variableDescr, y = "Count") +
    theme_classic() + 
    theme(aspect.ratio = 1,
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 6))
  
  # check output 
  print(distPlot)
}

#---------------------------------------------------------------------------------------------------------------

# DEPENDENT VARIABLE - PEDESTRIAN CRASHES

# filter out weekend, non roadway, and non local street crashes
pedCrashUptownFilt <- pedCrashUptown[((pedCrashUptown$CrashLoc != "Non-Roadway") & 
                                        (pedCrashUptown$RdClass == "Local Street") &
                                        (pedCrashUptown$CrashDay != "Saturday") &
                                        (pedCrashUptown$CrashDay != "Sunday")), ]

# snap crashes to road network and count number of crashes along each segment
pedCrashUptownFilt <- snapPointsToLines1(as_Spatial(pedCrashUptownFilt), roadsUptownFilt)
roadsUptownFilt$variable_cont_crashes <- poly.counts(pedCrashUptownFilt, roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (1) - NUMBER OF HOUSING UNITS (proxy for pedestrian volumes)
# SOURCE: http://maps.co.mecklenburg.nc.us/openmapping/data.html?search=par (Tax Parcel Ownership and Building Data)

taxParcels <- st_read("./Data/Parcel_TaxData_Uptown/Parcel_TaxData_Uptown.shp")

# edit tax parcels street name column
taxParcels$StreetName <- paste(taxParcels$stdir, taxParcels$stname, taxParcels$sttype, taxParcels$stsuffix) 
taxParcels$StreetName <- gsub(" NA", "", taxParcels$StreetName)
taxParcels$StreetName <- gsub("NA ", "", taxParcels$StreetName)

# find tax parcel centroids 
taxParcelsSP <- as(taxParcels, "Spatial")
taxParcelCentroids <- gCentroid(taxParcelsSP, byid = TRUE) %>% st_as_sf()
taxParcelCentroids <- cbind(taxParcelCentroids, taxParcels) %>% dplyr::select(-geometry.1)

# find and remove centroids that do not have matching name in street network
noMatch <- setdiff(taxParcelCentroids$StreetName, roadsUptownFilt$StreetName)
taxParcelCentroids <- taxParcelCentroids[!taxParcelCentroids$StreetName %in% noMatch, ]

# find unique street names in tax parcel data 
tp_streetNames <- taxParcelCentroids$StreetName %>% unique()

# create empty SF to save snapped points
taxParcelCentroids_Snapped <- st_as_sf(st_sfc()) %>% 
  st_set_crs(CRS("+proj=lcc +lat_1=36.16666666666666 +lat_2=34.33333333333334 +lat_0=33.75 +lon_0=-79 +x_0=609601.2192024384 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"))

# iterate through unique street names and snap tax parcel points with matching addresses
for (i in tp_streetNames) {
  print(i)
  
  getPoints <- taxParcelCentroids[taxParcelCentroids$StreetName == i, ] %>% as_Spatial()
  getLines <- roadsUptownFilt[roadsUptownFilt$StreetName == i, ] %>% st_as_sf()
  getLines <- getLines[!st_is_empty(getLines), , drop = FALSE] %>% as_Spatial()
  getLines <- spTransform(getLines, CRS("+proj=lcc +lat_1=36.16666666666666 +lat_2=34.33333333333334 +lat_0=33.75 +lon_0=-79 +x_0=609601.2192024384 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"))
  
  snappedPoints <- snapPointsToLines1(getPoints, getLines) %>% st_as_sf()
  taxParcelCentroids_Snapped <- rbind(taxParcelCentroids_Snapped, snappedPoints)
}

# sum number of housing units along each road and add variable to segment data 
getHousingUnits <- point.in.poly(taxParcelCentroids_Snapped, roadsUptownBuffer, poly.id = OBJECTID) %>% as.data.frame()
getHousingUnits <- getHousingUnits %>% group_by(OBJECTID) %>% summarize(variable_cont_housingunits = sum(units)) %>% 
  dplyr::select(OBJECTID, variable_cont_housingunits)
roadsUptownFilt <- merge(roadsUptownFilt, getHousingUnits[, c("OBJECTID", "variable_cont_housingunits")], by = "OBJECTID")
roadsUptownFilt$variable_cont_housingunits[is.na(roadsUptownFilt$variable_cont_housingunits)] <- 0

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (2) - NUMBER OF BUS STOPS
# SOURCE: https://data.charlottenc.gov/datasets/e0643b893ea24703830c3e41e231db69_0

busStops <- st_read("./Data/Bus_Stops/Bus_Stops.shp")
busStops <- busStops[Uptown,]

# snap bus stops to road network and count number of bus stops along each segment
busStops <- snapPointsToLines1(as_Spatial(busStops), roadsUptownFilt)
roadsUptownFilt$variable_cont_busstops <- poly.counts(busStops, roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (3) - NUMBER OF LIGHT RAIL STATIONS
# SOURCE 1: https://data.charlottenc.gov/datasets/lynx-blue-line-stations
# SOURCE 2: https://data.charlottenc.gov/datasets/lynx-red-line-stations (no operational stations)
# SOURCE 3: https://data.charlottenc.gov/datasets/lynx-gold-line-stops

blueLR <- st_read("./Data/LYNX_Blue_Line_Stations/LYNX_Blue_Line_Stations.shp") %>% 
  dplyr::select(OBJECTID_1, NAME) %>% rename(OBJECTID = OBJECTID_1)

goldLR <- st_read("./Data/LYNX_Gold_Line_Stops/LYNX_Gold_Line_Stops.shp") %>% filter(Status == "Operating") %>%
  dplyr::select(OBJECTID, Stop_Name) %>% rename(NAME = Stop_Name)

# combine light rail data 
lightRailStops <- rbind(blueLR, goldLR)
lightRailStops <- lightRailStops[Uptown,]

# snap light rail stations to road network and count number of light rail stations along each road
lightRailStops <- snapPointsToLines1(as_Spatial(lightRailStops), roadsUptownFilt)
roadsUptownFilt$variable_cont_lightrail <- poly.counts(lightRailStops, roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (4) - SIGNALS AT INTERSECTIONS
# SOURCE: https://data.charlottenc.gov/datasets/traffic-signals-1

trafficSignals <- st_read("./Data/Traffic_Signals/Traffic_Signals.shp")
trafficSignals <- trafficSignals[Uptown,]

# snap traffic signals to road network and count number of traffic signals along each road 
trafficSignals <- snapPointsToLines1(as_Spatial(trafficSignals), roadsUptownFilt)
roadsUptownFilt$variable_cont_trafficsignals <- poly.counts(trafficSignals, roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (5) - STREET TREES
# SOURCE: https://koordinates.com/layer/96938-charlotte-nc-greenways-trees/

trees <- st_read("./Data/Greenways_Trees/charlotte-nc-greenways-trees.shp")
trees <- trees[Uptown,]

# snap trees to road network and count number of trees along each road
trees <- snapPointsToLines1(as_Spatial(trees), roadsUptownFilt, maxDist = 50)
roadsUptownFilt$variable_cont_trees <- poly.counts(trees, roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (6) - PRESENCE OF BIKE LANES
# SOURCE: https://data.charlottenc.gov/datasets/bike-lanes?geometry=-82.798%2C34.842%2C-78.843%2C35.627

bikeLanes <- st_read("./Data/Bike_Lanes/Bike_Lanes.shp")
bikeLanes <- bikeLanes[Uptown,]

# find which road segements have a bike lane 
bikeLanesBuffer <- gBuffer(as_Spatial(bikeLanes), width = 5, capStyle = "ROUND", joinStyle = "ROUND")
roadsUptownFilt$variable_cat_bikelanes <- st_within(st_as_sf(roadsUptownFilt), st_as_sf(bikeLanesBuffer), sparse = FALSE)

# edit values from True / False to Yes / No
roadsUptownFilt$variable_cat_bikelanes[roadsUptownFilt$variable_cat_bikelanes == TRUE] <- "Yes"
roadsUptownFilt$variable_cat_bikelanes[roadsUptownFilt$variable_cat_bikelanes == FALSE] <- "No"

# convert to factor
roadsUptownFilt$variable_cat_bikelanes <- as.factor(roadsUptownFilt$variable_cat_bikelanes)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (7) - PARKING METERS
# https://data.charlottenc.gov/datasets/parking-meters?geometry=-80.849%2C35.223%2C-80.834%2C35.226

parkingMeters <- st_read("./Data/Parking_Meters/Parking_Meters.shp")
parkingMeters <- parkingMeters[Uptown,]

# snap parking meters to road network and count number of parking meters along each road
parkingMeters <- snapPointsToLines1(as_Spatial(parkingMeters), roadsUptownFilt)
roadsUptownFilt$variable_cont_parkingmeters <- poly.counts(parkingMeters, roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (8-12) - LAND USE MIX 
# SOURCE: http://maps.co.mecklenburg.nc.us/openmapping/data.html?search=par (Tax Parcel Ownership and Building Data)
# LAND USE CODES: https://www.mecknc.gov/AssessorsOffice/MeckReval/Documents/2019-Schedule-of-Values.pdf

# replace land use codes with general categories
taxParcelCentroids_Snapped$landusecod <- gsub("^R\\d\\d\\d", "Residential", taxParcelCentroids_Snapped$landusecod)
taxParcelCentroids_Snapped$landusecod <- gsub("^A\\d\\d\\d", "Multi-family", taxParcelCentroids_Snapped$landusecod)
taxParcelCentroids_Snapped$landusecod <- gsub("^O\\d\\d\\d", "Office", taxParcelCentroids_Snapped$landusecod)
taxParcelCentroids_Snapped$landusecod <- gsub("^I\\d\\d\\d", "Industrial", taxParcelCentroids_Snapped$landusecod)
taxParcelCentroids_Snapped$landusecod <- gsub("^C\\d\\d\\d", "Commercial", taxParcelCentroids_Snapped$landusecod)
taxParcelCentroids_Snapped$landusecod <- gsub("\\d\\d\\d\\d", "Other", taxParcelCentroids_Snapped$landusecod)
taxParcelCentroids_Snapped$landusecod[is.na(taxParcelCentroids_Snapped$landusecod)] <- "Other"

# extract parcel centroids from each category
taxParcelResidential <- taxParcelCentroids_Snapped[taxParcelCentroids_Snapped$landusecod == "Residential",] %>% dplyr::filter(!duplicated(pid))
taxParcelMultifamily <- taxParcelCentroids_Snapped[taxParcelCentroids_Snapped$landusecod == "Multi-family",] %>% dplyr::filter(!duplicated(pid))
taxParcelOffice      <- taxParcelCentroids_Snapped[taxParcelCentroids_Snapped$landusecod == "Office",] %>% dplyr::filter(!duplicated(pid))
taxParcelCommercial  <- taxParcelCentroids_Snapped[taxParcelCentroids_Snapped$landusecod == "Commercial",] %>% dplyr::filter(!duplicated(pid))

# count number of each land use type along road segments
roadsUptownFilt$variable_cont_residential <- poly.counts(as_Spatial(taxParcelResidential), roadsUptownBuffer)
roadsUptownFilt$variable_cont_multifamily <- poly.counts(as_Spatial(taxParcelMultifamily), roadsUptownBuffer)
roadsUptownFilt$variable_cont_office      <- poly.counts(as_Spatial(taxParcelOffice), roadsUptownBuffer)
roadsUptownFilt$variable_cont_commercial  <- poly.counts(as_Spatial(taxParcelCommercial), roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (13) - CROSSWALKS
# SOURCE: OpenStreetMap

crosswalks <- st_transform(st_read("./Data/OSM_Crossings/OSM_Crossings.shp"), 2264)
crosswalks <- crosswalks[Uptown,] 
crosswalks <- crosswalks[(crosswalks$crossing == "marked"), ]
crosswalks <- crosswalks[st_is_empty(crosswalks) == FALSE, ]

# snap crosswalks to road network and count number of crosswalks along each road
crosswalks <- snapPointsToLines1(as_Spatial(crosswalks), roadsUptownFilt, maxDist = 50)
roadsUptownFilt$variable_cont_crosswalks <- poly.counts(crosswalks, roadsUptownBuffer)

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (14) - TRAVEL DIRECTION
# TravelDire
# VALUES: both, one-way, NA

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (15-16) - SHOULDER SURFACE TYPE
# RtShldrTyp, LftShldrTy
# VALUES: curb-concrete, curb-bituminous, NA

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (17) - PRESENCE AND TYPE OF MEDIAN
# MedianType
# VALUES: curb, grass, paved mountable, none

# missing data indicates no median
levels(roadsUptownFilt$MedianType) <- c(levels(roadsUptownFilt$MedianType), "None") 
roadsUptownFilt$MedianType[is.na(roadsUptownFilt$MedianType)]  <- "None" 

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (17-20) - NUMBER OF LANES
# ThruLaneCo, LftTrnLnTy, RtTrnLnTy
# VALUES: continuous, multiple, single, none

# missing data indicates no designated left turn lane
levels(roadsUptownFilt$LftTrnLnTy) <- c(levels(roadsUptownFilt$LftTrnLnTy), "None") 
roadsUptownFilt$LftTrnLnTy[is.na(roadsUptownFilt$LftTrnLnTy)]  <- "None" 

# missing data indicates no designated right turn lane
levels(roadsUptownFilt$RtTrnLnTyp) <- c(levels(roadsUptownFilt$RtTrnLnTyp), "None") 
roadsUptownFilt$RtTrnLnTyp[is.na(roadsUptownFilt$RtTrnLnTyp)]  <- "None" 

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (21) - SURFACE WIDTH IN FEET
# SrfcWidth

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (22) - AADT TRAFFIC VOLUMES 
# SOURCE: https://connect.ncdot.gov/resources/State-Mapping/Pages/Traffic-Survey-GIS-Data.aspx

trafficVolumes <- st_read("./Data/NCDOT_2018_TrafficSegments/NCDOT_AADT_Traffic_Segments.shp") %>% st_zm() 
trafficVolumesUptown <- trafficVolumes[Uptown,] 
trafficVolumesUptown <- spTransform(as_Spatial(trafficVolumesUptown), CRS("+proj=lcc +lat_1=36.16666666666666 +lat_2=34.33333333333334 +lat_0=33.75 +lon_0=-79 +x_0=609601.2192024384 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")) 

# buffer traffic volume line segments and check for overlap
trafficVolumesBuffer <- gBuffer(trafficVolumesUptown, byid = TRUE, width = 5, capStyle = "ROUND", joinStyle = "ROUND")
tvcheck <- sp::over(roadsUptownFilt, trafficVolumesBuffer, returnList = FALSE)

# average AADT values over common route IDs 
volumes <- tvcheck %>% group_by(RouteID) %>% summarize(variable_cont_traffvols = mean(AADT_2018)) %>% 
  dplyr::select(RouteID, variable_cont_traffvols)

# merge to road segements
roadsUptownFilt <- merge(roadsUptownFilt, volumes[, c("RouteID", "variable_cont_traffvols")], by = c("RouteID"))

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (23) - FUNCTIONAL CLASSIFICATION
# FuncClass
# VALUES: 2 (PA-FrwyExp), 3 (PA-Other), 4 (Minor Arterial), 5 (Major Collector), 7 (Local)

# convert to factor and change to descriptions
roadsUptownFilt$FuncClass <- as.factor(roadsUptownFilt$FuncClass)
roadsUptownFilt$FuncClass <- mapvalues(roadsUptownFilt$FuncClass, from = c("2", "3", "4", "5", "7"), 
                                       to = c("PA-FrwyExp", "PA-Other", "Minor Arterial", "Major Collector", "Local"))

#---------------------------------------------------------------------------------------------------------------

# INDEPENDENT VARIABLE (24) - EDGE BETWEENESS CENTRALITY

# FUNCTION SOURCE: https://www.r-spatial.org/r/2019/09/26/spatial-networks.html
sf_to_tidygraph = function(x, directed = TRUE) {
  
  edges <- x %>%
    mutate(edgeID = c(1:n()))
  
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    select(-xy)
  
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
}

# get road network as graph object
graph <- sf_to_tidygraph(st_as_sf(roadsUptownFilt), directed = FALSE)

# get edge betweeness 
graph <- graph %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = variable_cont_length_ft))

# add edge betweeness as road network variable
graphDF <- graph %>% as.data.frame()
roadsUptownFilt$variable_cont_edgebetw <- graphDF$betweenness

#---------------------------------------------------------------------------------------------------------------

# MAP CONTINUOUS VARIABLES 

crashNet   <- networkMapCont(roadsUptownFilt$variable_cont_crashes, "Pedestrian\nCrashes")
lengthNet   <- networkMapCont(roadsUptownFilt$variable_cont_length_ft, "Segment\nLength (ft)")
busstopsNet   <- networkMapCont(roadsUptownFilt$variable_cont_busstops, "Bus Stops")
lightrailNet   <- networkMapCont(roadsUptownFilt$variable_cont_lightrail, "Light Rail\nStations")
edgebetwNet <- networkMapCont(roadsUptownFilt$variable_cont_edgebetw, "Edge\nBetweenness")

# extract non missing traffic volumes
noMissingTraffVols <- roadsUptownFilt[!is.na(roadsUptownFilt$variable_cont_traffvols),] %>% st_as_sf()

# map traffic volumes
traffvolsNet <- ggplot() +
  geom_sf(data = st_as_sf(roadsUptownFilt), color = "darkgrey", size = 0.25) +
  geom_sf(data = noMissingTraffVols, aes(color = variable_cont_traffvols, size = variable_cont_traffvols)) + 
  scale_size(range = c(0.5, 1.0), guide = "none") + 
  scale_fill_distiller(palette = "Spectral", aesthetics = c("colour", "fill"), guide = guide_colorbar(barheight = 2.5, ticks = FALSE)) +
  labs(colour = "Average Annual\nDaily Traffic") + 
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_text(size = 4),
        legend.background = element_rect(fill = "transparent"), legend.title = element_text(size = 10),
        legend.text = element_text(size = 7), legend.justification = c(0, 0), legend.position = c(0, 0),
        legend.box.margin = ggplot2::margin(c(10,10,10,10)))

# extract non missing thru lane count values for mapping
noMissingThruLanes <- roadsUptownFilt[!is.na(roadsUptownFilt$ThruLaneCo),] %>% st_as_sf()

# map thru lanes
thrulanesNet <- ggplot() +
  geom_sf(data = st_as_sf(roadsUptownFilt), color = "darkgrey", size = 0.25) +
  geom_sf(data = noMissingThruLanes, aes(color = ThruLaneCo, size = ThruLaneCo)) + 
  scale_size(range = c(0.5, 1.0), guide = "none") + 
  scale_fill_distiller(palette = "Spectral", aesthetics = c("colour", "fill"), guide = guide_colorbar(barheight = 2.5, ticks = FALSE)) +
  labs(colour = "Thru Lane\nCount") + 
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_text(size = 4),
        legend.background = element_rect(fill = "transparent"), legend.title = element_text(size = 10),
        legend.text = element_text(size = 7), legend.justification = c(0, 0), legend.position = c(0, 0),
        legend.box.margin = ggplot2::margin(c(10,10,10,10)))

# extract non missing surface width values for mapping
noMissingSrfcWidth <- roadsUptownFilt[!is.na(roadsUptownFilt$SrfcWidth),] %>% st_as_sf()

# map surface widths
srfcwidthNet <- ggplot() +
  geom_sf(data = st_as_sf(roadsUptownFilt), color = "darkgrey", size = 0.25) +
  geom_sf(data = noMissingSrfcWidth, aes(color = SrfcWidth, size = SrfcWidth)) + 
  scale_size(range = c(0.5, 1.0), guide = "none") + 
  scale_fill_distiller(palette = "Spectral", aesthetics = c("colour", "fill"), guide = guide_colorbar(barheight = 2.5, ticks = FALSE)) +
  labs(colour = "Surface\nWidth") + 
  theme_classic() + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_text(size = 4),
        legend.background = element_rect(fill = "transparent"), legend.title = element_text(size = 10),
        legend.text = element_text(size = 7), legend.justification = c(0, 0), legend.position = c(0, 0),
        legend.box.margin = ggplot2::margin(c(10,10,10,10)))

parkingmetersNet <- networkMapCont(roadsUptownFilt$variable_cont_parkingmeters, "Parking\nMeters")
trafficsignalsNet <- networkMapCont(roadsUptownFilt$variable_cont_trafficsignals, "Traffic\nSignals")
crosswalksNet <- networkMapCont(roadsUptownFilt$variable_cont_crosswalks, "Crosswalks")
treesNet <- networkMapCont(roadsUptownFilt$variable_cont_trees, "Street\nTrees")
housingunitsNet   <- networkMapCont(roadsUptownFilt$variable_cont_housingunits, "Housing\nUnits")
residentialNet <- networkMapCont(roadsUptownFilt$variable_cont_residential, "Residential\nParcels")
multifamilyNet <- networkMapCont(roadsUptownFilt$variable_cont_multifamily, "Multifamily\nParcels")
officeNet <- networkMapCont(roadsUptownFilt$variable_cont_office, "Office\nParcels")
commercialNet <- networkMapCont(roadsUptownFilt$variable_cont_commercial, "Commercial\nParcels")

png(file = "./Figures/continuousVariableMaps.png", height = 20, width = 12, units = "in", res = 300)
ggarrange(crashNet, lengthNet, busstopsNet, lightrailNet, thrulanesNet, srfcwidthNet, parkingmetersNet, trafficsignalsNet, 
          crosswalksNet, treesNet, housingunitsNet, residentialNet, multifamilyNet, 
          officeNet, commercialNet,
          ncol = 3, nrow = 5)
dev.off()

#---------------------------------------------------------------------------------------------------------------

# MAP CATEGORICAL VARIABLES

funcclassNet <- networkMapDiscr(roadsUptownFilt$FuncClass, "Functional\nClassification")
traveldireNet <- networkMapDiscr(roadsUptownFilt$TravelDire, "Travel\nDirection")
lfttrnlaneNet <- networkMapDiscr(roadsUptownFilt$LftTrnLnTy, "Left Turn\nLane Type")
rttrnlaneNet <- networkMapDiscr(roadsUptownFilt$RtTrnLnTyp, "Right Turn\nLane Type")
medianNet <- networkMapDiscr(roadsUptownFilt$MedianType, "Median\nType")
lftshldrNet <- networkMapDiscr(roadsUptownFilt$LftShldrTy, "Left Shoulder\nType")
rtshouldrNet <- networkMapDiscr(roadsUptownFilt$RtShldrTyp, "Right Shoulder\nType")
bikelanesNet <- networkMapDiscr(roadsUptownFilt$variable_cat_bikelanes, "Bike Lanes")

png(file = "./Figures/categoricalVariableMaps.png", height = 20, width = 12, units = "in", res = 300)
ggarrange(edgebetwNet, traffvolsNet, funcclassNet, traveldireNet, lfttrnlaneNet, rttrnlaneNet , medianNet, lftshldrNet, rtshouldrNet, bikelanesNet,
          ncol = 3, nrow = 5)
dev.off()

#---------------------------------------------------------------------------------------------------------------

# rename some variables in roads network data
roadsUptownFiltDF <- roadsUptownFilt %>% as.data.frame() %>% 
  rename(variable_cat_traveldirection = TravelDire,
         variable_cat_rightshoulder = RtShldrTyp,
         variable_cat_leftshoulder = LftShldrTy,
         variable_cat_median = MedianType,
         variable_cont_thrulanes = ThruLaneCo,
         variable_cat_lfttrnlntyp = LftTrnLnTy,
         variable_cat_rttrnlntyp = RtTrnLnTyp,
         variable_cont_surfacewidth = SrfcWidth,
         variable_cat_funcclass = FuncClass)

#---------------------------------------------------------------------------------------------------------------

# SUMMARY STATISTICS 

# seperate continuous and categorical variables
variablesCont <- roadsUptownFiltDF[ , grepl("variable_cont", names(roadsUptownFiltDF))] %>% as.data.frame()
variablesCat  <- roadsUptownFiltDF[ , grepl("variable_cat" , names(roadsUptownFiltDF))] %>% as.data.frame()

# get max, min, mean and standard deviation for continuous variables
variableObs <- sapply(variablesCont, function(x) sum(!is.na(x)))
variableMax <- sapply(variablesCont, max, na.rm = TRUE)
variableMin <- sapply(variablesCont, min, na.rm = TRUE)
variableMean <- sapply(variablesCont, mean, na.rm = TRUE)
variableSTD <- sapply(variablesCont, sd, na.rm = TRUE)

# make table and write to CSV for overleaf
descrStats <- t(rbind(variableObs, variableMax, variableMin, variableMean, variableSTD))
descrStats <- signif(descrStats, 3)
write.table(descrStats, "./Data/contDescrStats.csv", row.names = TRUE, sep = "&", eol = "\\\n", append = FALSE)

# get summary stats for categorical variables
for (i in names(variablesCat)){
  print(i)
  table <- table(variablesCat[i]) %>% as.data.frame()
  table$perc <- signif(((table$Freq / sum(table$Freq)) * 100), 3)
  print(table)
  print(paste("Observations: ", sum(!is.na(variablesCat[i]))), sep = "")
  print(paste("Missing values: ", signif((sum(is.na(variablesCat[i])) / nrow(variablesCat)) * 100, 3), "%", sep = ""))
}

#---------------------------------------------------------------------------------------------------------------

# REMOVE UNEEDED VARIABLES FROM ENVIRONMENT

keep(roadsUptownFilt, roadsUptownFiltDF, Uptown, sure = TRUE)

dev.off()

