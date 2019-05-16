######################################################################################################################################
##
## This scripts complete pre-processing of 4-day mean LAI raster datasets obtained from Google Earth Engine.
##
######################################################################################################################################

library(raster)
library(sf)
library(plyr)


bc.albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

LAI <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/GEE-LAI/reproject_1.tif", crs = bc.albers)

landcover <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/EOSD_alb_Snap.tif", crs = bc.albers)

vegetation.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/vegetation_codes.csv")

model.watersheds <- st_read("/var/obwb-hydro-modelling/input-data/raw/spatial/EFN_WS.shp", crs = bc.albers)

## New version of raster package does not seem to support "sf" objects for cropping/masking. So shapefile must be converted to spatialpolygon
model.watersheds.shape <- as(model.watersheds, "Spatial")

## crop lai and landcover datasets to model watersheds so coordinates are consistent in both dataframes
LAI.ok <- mask(crop(LAI, model.watersheds.shape), model.watersheds.shape)

landcover.ok <- mask(crop(landcover, model.watersheds.shape), model.watersheds.shape)


## extract coordinates, lai, and landcover values
coords <- coordinates(landcover.ok)

lai <- values(LAI.ok)

landcover <- values(landcover.ok)

## attach lai and landcover values to coordinates so all data in one dataframe
data <- data.frame(coords, lai, landcover)

## Calculate mean lai for each landcover value - na.rm must be used as there are some cells with no lai data available, but landcover data present (i.e., around lake edges etc.). LAI is calculated using all available cells for each landcover
x <- ddply(data, .(landcover), summarize, mean = mean(lai, na.rm = T))

## link vegetation bin to landcover values in resultant dataframe
for(i in 1:nrow(x)){
  x[i, "bin"] <- ifelse(is.na(x[i, "landcover"]), "[NONE]", as.character(vegetation.codes$Bin_type)[x[i, "landcover"] == vegetation.codes$Value])
}


plot(LAI.ok)
plot(model.watersheds.shape, add = T)
plot(landcover.ok)

# for(i in 1:nrow(landcover.df)){
# landcover.df[i, "bin"] <- ifelse(is.na(landcover.df[i, "landcover"]), "[NONE]", as.character(vegetation.codes$Bin_type)[landcover.df[i, "bin"] == vegetation.codes$Value])
# print(i)
# }
# 
# 
# HRU.output[i, "VEG_CLASS"] <- ifelse(is.na(HRU.output[i, "VEG_CLASS"]), "[NONE]", as.character(vegetation.codes$Bin_type)[HRU.output[i, "VEG_CLASS"] == vegetation.codes$Value])



## Read in GIS data except for the LAI *.tif files
bc.albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

landcover <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/EOSD_alb_Snap.tif", crs = bc.albers)

vegetation.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/vegetation_codes.csv")

model.watersheds <- st_read("/var/obwb-hydro-modelling/input-data/raw/spatial/EFN_WS.shp", crs = bc.albers)

## New version of raster package does not seem to support "sf" objects for cropping/masking. So shapefile must be converted to spatialpolygon
model.watersheds.shape <- as(model.watersheds, "Spatial")

## Crop landcover raster to model watershed
landcover.ok <- mask(crop(landcover, model.watersheds.shape), model.watersheds.shape)

coords <- coordinates(landcover.ok)

landcover.values <- values(landcover.ok)

## Read in all LAI *.tif files
months <- 1:3

lai.var.names <- paste("lai_", months, sep = "")

data <- data.frame(coords)

for(i in months){
  
  lai <- raster(paste("/var/obwb-hydro-modelling/input-data/raw/spatial/GEE-LAI/reproject_", months[i], ".tif", sep = ""), crs = bc.albers)
  
  lai <- mask(crop(lai, model.watersheds.shape), model.watersheds.shape)
  
  assign(lai.var.names[i], lai)
 
  data <- cbind(data, values(lai))
  
  print(i)
   
}

colnames(data) <- c("x", "y", lai.var.names)

data$landcover <- landcover.values

library(dplyr)

data %>% group_by(landcover) %>% summarise_all(.funs = c(mean="mean", na.rm = T))


results <- ddply(data, .(landcover), colwise(mean, na.rm = T))
