#####################################################################################################################
##
## This code generates summary of tables of all snow pillow and snow course locations to be included in the Raven
## model domain.
##
## Jun-26-2019 LAB
##
#####################################################################################################################

require(raster)


## Read in require Tidy HRU raster, subbasin raster, and subbasin codes (accompanying attribute table)
Tidy.hru <- raster("/var/obwb-hydro-modelling/input-data/processed/spatial/tidy-HRU-id.tif")

subbasin.raster <- raster("/var/obwb-hydro-modelling/input-data/processed/spatial/subbasin.tif")

subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

## Read in snow course and snow pillow location csv files directly downloaded from dataBC
snow.course.locations <- read.csv("/var/obwb-hydro-modelling/input-data/raw/snow-data/snow-course-locations.csv")

snow.pillow.locations <- read.csv("/var/obwb-hydro-modelling/input-data/raw/snow-data/snow-pillow-locations.csv")


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
write.csv(snow.course.locations, "/var/obwb-hydro-modelling/input-data/processed/spatial/snow/snow-course-locations-model-domain.csv")

write.csv(snow.pillow.locations, "/var/obwb-hydro-modelling/input-data/processed/spatial/snow/snow-pillow-locations-model-domain.csv")
