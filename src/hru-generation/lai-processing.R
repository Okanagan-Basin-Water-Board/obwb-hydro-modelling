######################################################################################################################################
##
## This scripts complete pre-processing of 4-day mean LAI raster datasets obtained from Google Earth Engine.
##
######################################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

library(raster)
library(sf)
library(plyr)
library(dplyr)

# Load base packages for error-free execution using Rscript from the command line
# require(stats)
# require(graphics)
# require(grDevices)
# require(utils)
# require(datasets)
library(methods)
# require(base)
# require(tfruns)

## Read in GIS data except for the LAI *.tif files
bc.albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

landcover <- raster(file.path(global.input.dir, raw.spatial.in.dir, landcover.in.file), crs = bc.albers)
  
vegetation.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, Veg.in.file))
  
model.watersheds <- st_read(file.path(global.input.dir, raw.spatial.in.dir, WS.shape.in.file), crs = bc.albers)
  
## New version of raster package does not seem to support "sf" objects for cropping/masking. So shapefile must be converted to spatialpolygon
model.watersheds.shape <- as(model.watersheds, "Spatial")

## Crop landcover raster to model watershed
landcover.ok <- mask(crop(landcover, model.watersheds.shape), model.watersheds.shape)

coords <- coordinates(landcover.ok)

landcover.values <- values(landcover.ok)

## Read in all LAI *.tif files
months <- 1:12

scaling.factor <- 0.1

lai.var.names <- paste("lai_", months, sep = "")

data <- data.frame(coords)

for(i in months){
  
  lai <- raster(paste(file.path(global.input.dir, raw.spatial.in.dir, LAI.in.dir, "reproject_"), months[i], ".tif", sep = ""), crs = bc.albers)
    
  lai <- mask(crop(lai, model.watersheds.shape), model.watersheds.shape)
  
  assign(lai.var.names[i], lai)
 
  data <- cbind(data, (values(lai) * scaling.factor))
  
  print(i)
   
}

colnames(data) <- c("x", "y", lai.var.names)

data$landcover <- landcover.values

## Join vegetation bins (i.e., "Value" from vegetation.codes) to the data object - this allows vegetation bins, rather than raw values to be used for averaging
data.all <- inner_join(data, vegetation.codes, by = c("landcover" = "Value"))

## Calculate monthly average for all 
results.mean <- data.all %>% group_by(Bin_type) %>% summarise_at(.vars = c(lai.var.names),
                                                            .funs = c(mean="mean"), na.rm = T)

# ## Calculate maximum LAI for all
# results.max <- data.all %>% group_by(Bin_type) %>% summarise_at(.vars = c(lai.var.names),
#                                                                   .funs = c(max="max"), na.rm = T)

## Identify the maximum annual value for each vegetation type
# max <- data.frame("Bin_type" = results.max$Bin_type, "MAX_LAI" = apply(results.max[,grepl("lai", names(results.max))], 1, max))

## Identify the maximum monthly mean LAI value - use this as the "MAX_LAI value within raven (rather than the maximum value recorded for each month. I don't believe this actually affects the results, only the seasonal scaling factor to adjust max_lai to seaonal lai values. Max_lai does not appear to be used anywhere else in the model.
max <- data.frame("Bin_type" = results.mean$Bin_type, "MAX_LAI" = apply(results.mean[,grepl("lai", names(results.mean))], 1, max))


## Identify vegetation bins which should not have variable LAI
exclusions <- c("NO_DATA", "NON_VEGETATED", "SHADOW", "SNOW_ICE", "WATER", "URBAN")

## Set maximum to zero for appropriate excluded vegetation bins - these vegetation classes do not have LAI
max[max$Bin_type %in% exclusions, "MAX_LAI"] <- 0



## Calculate fractional reduction for monthly LAI from maximum for each vegetation type
LAI <- results.mean[,grepl("lai", names(results.mean))] / max$MAX_LAI

## Add Bin_Type column to front of LAI dataframe
LAI <- data.frame(Bin_Type = results.mean$Bin_type, LAI)

## Set all exclusions to 0
# LAI[LAI$Bin_Type %in% exclusions, grepl("lai", names(LAI))] <- 0

## Delete all vegetation type and exclusions that should not vary monthly
LAI <- LAI[!LAI$Bin_Type %in% c(exclusions, "CONIFEROUS", "CONIFEROUS_OPEN", "CONIFEROUS_DENSE", "GRASS", "WETLAND"), ]

## Make Broadleaf the same for all broadleaf vegetation types. Density will be captured by interception factor
LAI[LAI$Bin_Type %in% c("BROADLEAF_DENSE", "BROADLEAF_OPEN"), 2:13] <- LAI[LAI$Bin_Type == "BROADLEAF", 2:13]


###################################################################
##
## MANUAL ADJUSTMENTS

## Set no change to Coniferous, Coniferous_Open, and Coniferous_Dense
# LAI[LAI$Bin_Type %in% c("CONIFEROUS", "CONIFEROUS_OPEN", "CONIFEROUS_DENSE"), grepl("lai", names(LAI))] <- 1


write.csv(max, file.path(global.input.dir, processed.spatial.dir, "lai", paste("max-lai.", Sys.Date(), ".csv", sep = "")), row.names = FALSE)
          
write.csv(LAI, file.path(global.input.dir, processed.spatial.dir, "lai", paste("seasonal-lai.", Sys.Date(), ".csv", sep = "")), row.names = FALSE)
          

# ## Plot monthly LAI across different bin types
# require(reshape)
# require(ggplot2)
# 
# 
# results.melted <- melt(as.data.frame(results.mean, id.vars = "Bin_type"))
# ggplot(results.melted, aes(x = variable, y = value)) + geom_line(aes(color = Bin_type, group = Bin_type))
