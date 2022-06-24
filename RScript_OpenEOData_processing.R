#Load libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)
#Load the data downloaded from the OpenEO platform and pre-processed previously
load("C:/Rprojects/RStudio_EOdata_GitHub/Open_EO_Platform_Editor/OpenEO_platform_indices.RData")
load("C:/Rprojects/RStudio_EOdata_GitHub/Open_EO_Platform_Editor/OpenEO_platform_meteo_data.RData")
load("C:/Rprojects/RStudio_EOdata_GitHub/Open_EO_Platform_Editor/OpenEO_platform_landcover_dem.RData")
#--------------------------------------------------------------------------------------------------------------------
#CGLS_FAPAR_V2_GLOBAL 
#Copernicus Global Land FAPAR product V2, 1km resolution, 10-daily
#Description.........................................................................................................
#Global FAPAR at 1km resolution, 10-daily. 
#The FAPAR quantifies the fraction of the solar radiation absorbed by live leaves for the photosynthesis activity. 
#Then, it refers only to the green and alive elements of the canopy. 
#The FAPAR depends on the canopy structure, vegetation element optical properties, atmospheric conditions, and angular configuration.
#To overcome this latter dependency, a daily integrated FAPAR value is assessed.
#--------------------------------------------------------------------------------------------------------------------
fapar <- result_fapar
fapar$tile <- as.factor(fapar$tile) #Convert the "tile" field to factor
fapar$date <-  as.Date(fapar$date,format="%Y-%m-%d")#Basic date format
fapar$undate <-  unclass(fapar$date) #displays a single value for each day of the time series
fapar.id <- unite(fapar, variables,c(2,4),  sep = "_", remove = TRUE)
fapar$id <- fapar.id$variables #transfer the tile-undate field to the dataframe

#POSIXlt which stores a list of day, month, year, hour, minute, second
fapar$dateXlt <-  as.POSIXlt(fapar$date,origin = "1998-10-10", format="%Y-%m-%d")
fapar$month <- as.factor(month(fapar$dateXlt))
fapar$year <- as.factor(year(fapar$dateXlt))

save(fapar, file="faparEO.RData") #save backup 
#--------------------------------------------------------------------------------------------------------------------
#ECMWF AGERA5 meteo dataset - experimental
#AGERA5
#Description.........................................................................................................
#This dataset provides daily surface meteorological data at 0.1°x0.1° spatial resolution for the period from 2015 to present as input for agro-ecological studies. 
#This dataset is based on the hourly ECMWF ERA5 data at surface level and is referred to as AgERA5.  
#The data was produced on behalf of the Copernicus Climate Change Service.
#This dataset is marked as experimental because the integration in this backend has not yet been fully validated. Use with caution.
#--------------------------------------------------------------------------------------------------------------------
##Bands
#dewpoint-temperature	
#Mean dewpoint temperature at a height of 2 metres above the surface over the period 00h-24h local time. The dew point is the temperature to which air must be cooled to become saturated with water vapor. In combination with the air temperature it is used to assess relative humidity.
#Offset	Scale	Unit
#0	    0,01	K
#precipitation-flux	
#Total volume of liquid water (mm3) precipitated over the period 00h-24h local time per unit of area (mm2), per day.
#Offset	Scale	Unit
#0	    0,01	mm/day
#solar-radiation-flux	
#Total amount of energy provided by solar radiation at the surface over the period 00-24h local time per unit area and time.
#Offset	Scale	Unit
#0	    1	    j/m²day
#temperature-max	
#Maximum air temperature at 2m height (daily maximum)
#Offset	Scale	Unit
#0	    0,01	K
#temperature-mean	
#Mean air temperature at 2m height (daily mean)
#Offset	Scale	Unit
#0	    0,01	K
#temperature-min	
#Minimum air temperature at 2m height (daily minimum)
#Offset	Scale	Unit
#0	    0,01	K
#vapour-pressure	
#Contribution to the total atmospheric pressure provided by the water vapour over the period 00-24h local time per unit of time.
#Offset	Scale	Unit
#0	    0,001	hPa
#wind-speed	
#Mean wind speed at a height of 10 metres above the surface over the period 00h-24h local time.
#Offset	Scale	Unit
#0	    0,01	m/s
#--------------------------------------------------------------------------------------------------------------------
prec <- result_prec
prec.scale <- 0.01
prec.unit <- "mm/day"

prec$tile <- as.factor(prec$tile) 
#Rescaling
prec$value <- prec$value*prec.scale 
prec$date <-  as.Date(prec$date,format="%Y-%m-%d")
prec$undate <-  unclass(prec$date)
prec.id <- unite(prec, variables,c(2,4),  sep = "_", remove = TRUE)
prec$id <- prec.id$variables 

#POSIXlt which stores a list of day, month, year, hour, minute, second
prec$dateXlt <-  as.POSIXlt(prec$date,origin = "1998-10-10", format="%Y-%m-%d")
prec$month <- as.factor(month(prec$dateXlt))
prec$year <- as.factor(year(prec$dateXlt))
#--------------------------------------------------------------------------------------------------------------------
tdew <- result_tdew
tmax <- result_tmax
tmed <- result_tmed
tmin <- result_tmin

t.scale <- 0.01
t.unit <- "K"

#Rescaling and change from ºkelvin to ºC
tdew$value <- ((tdew$value*t.scale)-273.15) 
tmax$value <- ((tmax$value*t.scale)-273.15) 
tmed$value <- ((tmed$value*t.scale)-273.15) 
tmin$value <- ((tmin$value*t.scale)-273.15) 

tdew$tile <- as.factor(tdew$tile) 
tdew$date <-  as.Date(tdew$date,format="%Y-%m-%d")
tdew$undate <-  unclass(tdew$date) 
tdew.id <- unite(tdew, variables,c(2,4),  sep = "_", remove = TRUE)
tdew$id <- tdew.id$variables 

tmax$tile <- as.factor(tmax$tile) 
tmax$date <-  as.Date(tmax$date,format="%Y-%m-%d")
tmax$undate <-  unclass(tmax$date) 
tmax.id <- unite(tmax, variables,c(2,4),  sep = "_", remove = TRUE)
tmax$id <- tmax.id$variables 

tmed$tile <- as.factor(tmed$tile) 
tmed$date <-  as.Date(tmed$date,format="%Y-%m-%d")
tmed$undate <-  unclass(tmed$date) 
tmed.id <- unite(tmed, variables,c(2,4),  sep = "_", remove = TRUE)
tmed$id <- tmed.id$variables 

tmin$tile <- as.factor(tmin$tile) 
tmin$date <-  as.Date(tmin$date,format="%Y-%m-%d")
tmin$undate <-  unclass(tmin$date) 
tmin.id <- unite(tmin, variables,c(2,4),  sep = "_", remove = TRUE)
tmin$id <- tmin.id$variables 

#--------------------------------------------------------------------------------------------------------------------
radi <- result_radi
pres <- result_pres
wind <- result_wind

radi.scale <- 1
radi.unit <- "j/m2day"
pres.scale <- 0.001
pres.unit <- "hPa"
wind.scale <- 0.01
wind.unit <- "m/s"

#Rescaling 
pres$value <- (pres$value*pres.scale) 
wind$value <- (wind$value*wind.scale)

radi$tile <- as.factor(radi$tile) 
radi$date <-  as.Date(radi$date,format="%Y-%m-%d")
radi$undate <-  unclass(radi$date) 
radi.id <- unite(radi, variables,c(2,4),  sep = "_", remove = TRUE)
radi$id <- radi.id$variables 

pres$tile <- as.factor(pres$tile) 
pres$date <-  as.Date(pres$date,format="%Y-%m-%d")
pres$undate <-  unclass(pres$date) 
pres.id <- unite(pres, variables,c(2,4),  sep = "_", remove = TRUE)
pres$id <- pres.id$variables 

wind$tile <- as.factor(wind$tile) 
wind$date <-  as.Date(wind$date,format="%Y-%m-%d")
wind$undate <-  unclass(wind$date) 
wind.id <- unite(wind, variables,c(2,4),  sep = "_", remove = TRUE)
wind$id <- wind.id$variables 

#--------------------------------------------------------------------------------------------------------------------
#Next step, merge the atmospheric variables in each tile.
#1) Sort tables by id field in alphabetical order (ascending by default)
prec_a <- arrange(prec,id)
tdew_a <- arrange(tdew,id)
tmax_a <- arrange(tmax,id)
tmed_a <- arrange(tmed,id)
tmin_a <- arrange(tmin,id)
radi_a <- arrange(radi,id)
pres_a <- arrange(pres,id)
wind_a <- arrange(wind,id)
#2)Join all data 
df.atm <-cbind(prec_a,
               tdew_a[,c(3,5)],
               tmax_a[,c(3,5)],
               tmed_a[,c(3,5)],
               tmin_a[,c(3,5)],
               radi_a[,c(3,5)],
               pres_a[,c(3,5)],
               wind_a[,c(3,5)])
head(df.atm)
colnames(df.atm) <- c("date", "tile", "prec_value", "undate","id","dateXlt", "month", "year", "tdew_value", "id", "tmax_value", "id", "tmed_value", "id", "tmin_value","id",  "radi_value",  "id", "pres_value", "id", "wind_value", "id")
#We check that all the id fields match for all the variables

#3)Transform the daily data series to the 10-day average values
df.atm$day <- day(df.atm$dateXlt)
df.atm$month <- as.factor(month(df.atm$dateXlt))
df.atm$year <- as.factor(year(df.atm$dateXlt))

#We adapt the temporal resolution of the meteorological data to the fapar data,
#which provides data every 10 days (specifically on the 10th, 20th and 31st of each month).
#So, the defining break points are:
break_values <- c(0,11,21,31) 
break_labels <- c("d10","d20","d30")

#defining the new categories according to the break points 
df.atm$aggregate <- cut(df.atm$day, breaks=break_values, labels=break_labels) #with labels
df.atm$aggregaten <- cut(df.atm$day, breaks=break_values) #with numerical ranges of the breaks

#Select the columns with which you want to calculate
str(df.atm)
df.atm.stats <- df.atm[,c(2,3,9,11,13,15,17,19,21,24,7,8)]
df.atm.10 <- df.atm.stats%>% 
  group_by(tile,year,month,aggregate ) %>% 
  summarise_all(mean)

#The accumulated precipitation in those 10 days will be Pmean x 10
df.atm.10$Pacc_value <- 10*df.atm.10$prec_value

#Save
save(df.atm.10, file="df_atm._10days.RData")

#--------------------------------------------------------------------------------------------------------------------
#GLOBAL_LAND_COVER
#Global Land Cover products at 100 m resolution are delivered annually by the global component of the Copernicus Land Service. 
#These Land Cover products provide a main discrete land cover classification map according to UN-FAO Land Cover Classification System LCCS.
class_map <- result_map
head(class_map)
colnames(class_map) <- c("date", "tile", "Code")

#First of all, we have to look at the available coverage data, and we find that there are data for four different years: 2015, 2016, 2017, 2018 and 2019.
class_map$date <- as.factor(class_map$date)
levels(class_map$date)

#Load the land use description processed from the Open EO platform product information
load("C:/Rprojects/RStudio_EOdata_GitHub/Open_EO_Platform_Editor/OpenEO_platform_data&code/Landcover_Codes.RData")
head(Landcover_Codes)
LandcoverR <- Landcover_Codes[,c(1,3)]
head(LandcoverR)
#We separate the data for each year
class_map2015 <- class_map[c(class_map$date=="2015-01-01"),]
class_map2016 <- class_map[c(class_map$date=="2016-01-01"),]
class_map2017 <- class_map[c(class_map$date=="2017-01-01"),]
class_map2018 <- class_map[c(class_map$date=="2018-01-01"),]
class_map2019 <- class_map[c(class_map$date=="2019-01-01"),]

#We check if there are changes of use in each tile for the different years, and we see that there is no difference, with the exception of 10 tiles out of 1737, so we take 2015 as a reference year.
class_mapEO <- class_map2015
head(class_mapEO)
#Analyse the landuse types in the study area:
class_mapEO <- left_join(class_mapEO, LandcoverR, by="Code")
head(class_mapEO)
class_mapEO$RCober <- as.factor(class_mapEO$RCober)
#save
save(class_mapEO, file="class_mapEO.RData")

#--------------------------------------------------------------------------------------------------------------------
#DIGITAL ELEVATION MODEL
#Copernicus Global 30 meter Digital Elevation Model dataset in COG format.
dem_30 <- result_dem
#First, we analyse the available data. 
dem_30$date <- as.factor(dem_30$date)
levels(dem_30$date)
#Splitting the data into the 11 existing date ranges
dem_2013.1 <- dem_30[c(dem_30$date=="2013-01-01"),]
dem_2013.2 <- dem_30[c(dem_30$date=="2013-04-27"),]
dem_2013.3 <- dem_30[c(dem_30$date=="2013-05-08"),]
dem_2013.4 <- dem_30[c(dem_30$date=="2013-09-10"),]
dem_2013.5 <- dem_30[c(dem_30$date=="2013-12-18"),]
dem_2014.1 <- dem_30[c(dem_30$date=="2014-02-23"),]
dem_2014.2 <- dem_30[c(dem_30$date=="2014-03-01"),]
dem_2014.3 <- dem_30[c(dem_30$date=="2014-03-06"),]
dem_2014.4 <- dem_30[c(dem_30$date=="2014-03-28"),]
dem_2014.5 <- dem_30[c(dem_30$date=="2014-08-13"),]
dem_2014.6 <- dem_30[c(dem_30$date=="2014-08-18"),]

#Load tile index
load("C:/Rprojects/RStudio_EOdata_GitHub/Open_EOData_processing/tile_lon_lat.RData") 
#we join data iteratively: with full_join to keep all tiles even if it has no data
df.tile.dem <- tile_lon_lat
df.tile.dem <- full_join(df.tile.dem, dem_2013.1, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2013.2, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2013.3, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2013.4, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2013.5, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2014.1, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2014.2, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2014.3, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2014.4, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2014.5, by="tile")
df.tile.dem <- full_join(df.tile.dem, dem_2014.6, by="tile")
head(df.tile.dem)
df.tile.dem <- df.tile.dem[,c(1:3,5,7,9,11,13,15,17,19,21,23,25)]
colnames(df.tile.dem) <- c("tile", "lon", "lat", "dem2013-01-01", "dem2013-04-27", "dem2013-05-08", "dem2013-09-10", "dem2013-12-18", "dem2014-02-23", "dem2014-03-01", "dem2014-03-06", "dem2014-03-28", "dem2014-08-13", "dem2014-08-18")
head(df.tile.dem)
#we note that each tile has a dem data for a different year but they are not repeated. 
#there are some cells without data, we replace the NA data by zero

df.tile.dem[is.na(df.tile.dem)] <- 0

#Create a new field with the elevation data

df.tile.dem$dem <- (df.tile.dem$`dem2013-01-01`+
                     df.tile.dem$`dem2013-04-27`+
                     df.tile.dem$`dem2013-05-08`+                   
                     df.tile.dem$`dem2013-09-10`+
                     df.tile.dem$`dem2013-12-18`+
                     df.tile.dem$`dem2014-02-23`+
                     df.tile.dem$`dem2014-03-01`+
                     df.tile.dem$`dem2014-03-06`+
                     df.tile.dem$`dem2014-03-28`+   
                     df.tile.dem$`dem2014-08-13`+
                     df.tile.dem$`dem2014-08-18`)
head(df.tile.dem)
#We return the data 0 to NA
df.tile.dem[c(df.tile.dem$dem == 0),c(15)] <- NA

dem_EOData <- df.tile.dem[,c(1:3,15)]
summary(dem_EOData)

#Create a map to visualize the dem.

library(ggplot2)

ggplot(dem_EOData) +
 aes(x = lon, y = lat, fill = dem) +
 geom_tile(size = 1.5) +
 scale_fill_viridis_c(option = "inferno", 
 direction = 1) +
 theme_minimal()

#save
save(dem_EOData, file="dem_EOData.RData")
