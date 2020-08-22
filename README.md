# Charlotte, North Carolina Pedestrian Safety Analysis

Can features of the built environment be used to predict pedestrian crashes at the street level in Charlotte, North Carolina?

Run files in the following order WITHOUT clearing the environment:
1. 1_CharPedCrashes_CrashDescrStats.R
2. 2_CharPedCrashes_IndepVariables.R
3. 3_CharPedCrashes_RandomForest_NBR.R

Sources for data clearly linked within R files. 

|File Name                           | Description              | Source                          |
|:----------------------------------:|:------------------------:|:-------------------------------:|
Bike_Lanes                          | Bike Lanes               | Charlotte DOT (2019a)
Bus_Stops                           | Bus Stops                | Charlotte Area Transit (2019)
Downtown_Wards                      | Uptown Boundary          | Char-Meck Planning Dept. (2019)
Greenways_Trees *                   | Trees                    | Porter (2018)
High_Injury_Network                 | High Injury Network      | CharlotteNC (2020)
LYNX_Blue_Line_Stations             | Light Rail Stations      | Jenkins (2019a)
LYNX_Gold_Line_Stops                | Light Rail Stations      | Jenkins (2019b)
Mecklenburg_Jurisdictions           | Charlotte Boundary       | Mecklenburg County (2020b)
NCDOT_2018_TrafficSegments *        | AADT Volumes             | Traffic Survey Group (2018)
NCDOT_BikePed_Crashes_2007_2018.gdb | Pedestrian Crashes       | NCDOT DBPT (2020)
OSM_Crossings                       | Crosswalks               | OpenStreetMap Contributors (2020)
Parcel_TaxData_Uptown               | Tax Parcel Building Data | Mecklenburg County (2020c)
Parking_Meters                      | Parking Meters           | Charlotte DOT (2019b)
Traffic_Signals                     | Traffic Signals          | Charlotte DOT (2019c)
Uptown_Road_Characteristics         | Road Segments            | NCDOT (2020)

* = files too large to be uploaded to GitHub; can be downloaded from link 

Data Sources

Charlotte DOT (2019a), ‘Bike lanes’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/bike-lanes?geometry=-82.798%2C34.842%2C-78.843%2C35.627 Accessed 21 Jul. 2020.

Charlotte Area Transit (2019), ‘Bus stops’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/e0643b893ea24703830c3e41e231db69_0 Accessed 21 Jul. 2020.

Char-Meck Planning Dept. (2019), ‘Downtown wards’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/64dde23672004971a1b2dbf3a12f733e_13 Accessed 21 Jul. 2020.

Porter, T. (2018), ‘Charlotte, NC greenways trees’. Electronic dataset, Koordinates, available at: https://koordinates.com/layer/96938-charlotte-nc-greenways-trees/ Accessed 21 Jul. 2020.

CharlotteNC (2020), ‘High Injury Network’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/4a27df737ba24be7acf1314f38925a4f_87 Accessed 31 Jul. 2020.

Jenkins, W. (2019a), ‘Lynx blue line stations’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/lynx-blue-line-stations Accessed 16 Jul. 2020.

Jenkins, W. (2019b), ‘Lynx gold line stops’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/lynx-gold-line-stops Accessed 14 Jul. 2020.

Mecklenburg County (2020b), ‘Mecklenburg County jurisdictions’. Electronic dataset, Open Mapping, available at: http://maps.co.mecklenburg.nc.us/openmapping/data.html?search=Mecklenburg%20County%20Jurisdictions Accessed 21 Jul. 2020.

Traffic Survey Group (2018), ‘2018 traffic segments shapefile’. Electronic dataset, Connect NCDOT, available at: https://connect.ncdot.gov/resources/State-Mapping/Pages/Traffic-Survey-GIS-Data.aspx Accessed 21 Jul. 2020.

NCDOT DBPT (2020), ‘Bicyclist and pedestrian crashes that occurred in North Carolina between years 2007 through 2018’. Electronic dataset, ArcGIS, available at: https://www.arcgis.com/home/item.html?id=801a34b6d3e64786a131d1fa2eca793d Accessed 21 Jul. 2020.

OpenStreetMap Contributors (2020), ‘Planet dump retrieved from https://planet.osm.org’.

Mecklenburg County (2020c), ‘Tax parcel ownership and building data’. Electronic dataset, Open Mapping, available at: http://maps.co.mecklenburg.nc.us/openmapping/data.html?search=Tax%20Parcel%20Ownership%20and%20Building%20Data Accessed 21 Jul. 2020.

Charlotte DOT (2019b), ‘Parking meters’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/parking-meters?geometry=-80.849%2C35.223%2C-80.834%2C35.226 Accessed 21 Jul. 2020.

Charlotte DOT (2019c), ‘Traffic signals’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/traffic-signals-1 Accessed 21 Jul. 2020.


NCDOT (2020), ‘NCDOT road characteristics’. Electronic dataset, NC OneMap, available at: https://www.nconemap.gov/datasets/NCDOT::ncdot-road-characteristics?geometry=-80.856%2C35.222%2C-80.825%2C35.228 Accessed 21 Jul. 2020.

