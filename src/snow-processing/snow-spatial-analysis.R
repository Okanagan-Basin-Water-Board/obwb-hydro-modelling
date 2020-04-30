#####################################################################################################################
##
## This code generates summary of tables of all snow pillow and snow course locations to be included in the Raven
## model domain.
##
## Jun-26-2019 LAB
##
#####################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

require(raster)


## Read in require Tidy HRU raster, subbasin raster, and subbasin codes (accompanying attribute table)
Tidy.hru <- raster(file.path(global.input.dir, processed.spatial.dir, tidy.hru.processed.file))
  
subbasin.raster <- raster(file.path(global.input.dir, processed.spatial.dir, subbasin.processed.file))
  
subbasin.codes <- read.csv(file.path(global.input.dir, processed.spatial.dir, SB.in.file))
  
## Read in snow course and snow pillow location csv files directly downloaded from dataBC
snow.course.locations <- read.csv(file.path(global.input.dir, raw.snow.in.dir, snow.course.locations.in.file))
  
snow.pillow.locations <- read.csv(file.path(global.input.dir, raw.snow.in.dir, snow.pillow.locations.in.file))


## Extract the corresponding SUBBASIN ID for all snow course locations
snow.course.locations$Subbasin_ID <- extract(subbasin.raster, snow.course.locations[,c("X", "Y")])

## Extract the corresponding HRU ID for all snow course locations
snow.course.locations$HRU <- extract(Tidy.hru, snow.course.locations[,c("X", "Y")])

## Remove values which fall outside the model domain spatial extent
snow.course.locations <- snow.course.locations[!is.na(snow.course.locations$HRU), ]

## Merge the subbasin attribute table and snow course locations so GNIS_NAME is included
snow.course.locations <- merge(subbasin.codes, snow.course.locations, by = "Subbasin_ID")
  


## Repeat all above steps for snow pillow locations
snow.pillow.locations$Subbasin_ID <- extract(subbasin.raster, snow.pillow.locations[,c("X", "Y")])

snow.pillow.locations$HRU <- extract(Tidy.hru, snow.pillow.locations[,c("X", "Y")])

snow.pillow.locations <- snow.pillow.locations[!is.na(snow.pillow.locations$HRU), ]

snow.pillow.locations <- merge(subbasin.codes, snow.pillow.locations, by = "Subbasin_ID")


## Write out condensed csv file which include the HRU and Subbasin_ID for relevant snow locations
write.csv(snow.course.locations, file.path(global.input.dir, processed.spatial.dir, "snow", paste("snow-course-locations-model-domain.", Sys.Date(), ".csv", sep = "")))
          
write.csv(snow.pillow.locations, file.path(global.input.dir, processed.spatial.dir, "snow", paste("snow-pillow-locations-model-domain.", Sys.Date(), ".csv", sep = "")))
