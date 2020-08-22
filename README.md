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
Greenways_Trees                     | Trees                    | Porter (2018)
High_Injury_Network                 | High Injury Network      | CharlotteNC (2020)
LYNX_Blue_Line_Stations             | Light Rail Stations      | Jenkins (2019a)
LYNX_Gold_Line_Stops                | Light Rail Stations      | Jenkins (2019b)
Mecklenburg_Jurisdictions           | Charlotte Boundary       | Mecklenburg County (2020b)
NCDOT_2018_TrafficSegments          | AADT Volumes             | Traffic Survey Group (2018)
NCDOT_BikePed_Crashes_2007_2018.gdb | Pedestrian Crashes       | NCDOT DBPT (2020)
OSM_Crossings                       | Crosswalks               | OpenStreetMap Contributors (2020)
Parcel_TaxData_Uptown               | Tax Parcel Building Data | Mecklenburg County (2020c)
Parking_Meters                      | Parking Meters           | Charlotte DOT (2019b)
Traffic_Signals                     | Traffic Signals          | Charlotte DOT (2019c)
Uptown_Road_Characteristics         | Road Segments            | NCDOT (2020)


Data Sources

Charlotte DOT (2019a), ‘Bike lanes’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/bike-lanes?geometry=-82.798%2C34.842%2C-78.843%2C35.627 Accessed 21 Jul. 2020.

Charlotte Area Transit (2019), ‘Bus stops’. Electronic dataset, Charlotte Open Data Portal, available at: https://data.charlottenc.gov/datasets/e0643b893ea24703830c3e41e231db69_0 Accessed 21 Jul. 2020.
