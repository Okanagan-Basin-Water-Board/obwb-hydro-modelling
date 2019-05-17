######################################################################################################################################
##
## This scripts complete pre-processing of 4-day mean LAI raster datasets obtained from Google Earth Engine.
##
######################################################################################################################################

library(raster)
library(sf)
library(plyr)
library(dplyr)

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
months <- 1:12

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

## Join vegetation bins (i.e., "Value" from vegetation.codes) to the data object - this allows vegetation bins, rather than raw values to be used for averaging
data.all <- inner_join(data, vegetation.codes, by = c("landcover" = "Value"))

## Calculate monthly average for all 
results.mean <- data.all %>% group_by(Bin_type) %>% summarise_at(.vars = c(lai.var.names),
                                                            .funs = c(mean="mean"), na.rm = T)

results.max <- data.all %>% group_by(Bin_type) %>% summarise_at(.vars = c(lai.var.names),
                                                                  .funs = c(max="max"), na.rm = T)

max <- data.frame("Bin_type" = results.max$Bin_type, "MAX_LAI" = apply(results.max[,grepl("lai", names(results.max))], 1, max))

exclusions <- c("NO_DATA", "NON_VEGETATED", "SHADOW", "SNOW_ICE", "WATER")

max[max$Bin_type %in% exclusions, "MAX_LAI"] <- 0



LAI <- results.mean[,grepl("lai", names(results.mean))] / max$MAX_LAI

is.na(LAI)<-sapply(LAI, is.infinite)

LAI[is.na(LAI)]<-0

LAI$Bin_type <- results.mean$Bin_type

## Plot monthly LAI across different bin types
require(reshape)
require(ggplot2)


results.melted <- melt(as.data.frame(results.mean, id.vars = "Bin_type"))
ggplot(results.melted, aes(x = variable, y = value)) + geom_line(aes(color = Bin_type, group = Bin_type))
