###########################################################################################
##
## This script generates initial Hydrologic Response Units (HRUs) for the Okanagan Basin
## 
## Feb-15-2019 LAB
###########################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

## Source function
source(file.path(global.src.dir, "functions.R"))


## Create temporary directory to store intermediate output
tmp.loc <- file.path(global.input.dir, processed.spatial.dir, "temp")

ifelse(dir.exists(tmp.loc), "Temporary location exists", dir.create(tmp.loc))
  
##########################
## LOAD REQUIRED PACKAGES
##########################

library(sf)
library(raster)
library(data.table)
library(rgdal)
library(proj4)
library(cloudml)
library(plyr)

# Load base packages for error-free execution using Rscript from the command line
# require(stats)
# require(graphics)
# require(grDevices)
# require(utils)
# require(datasets)
library(methods)
# require(base)
# require(tfruns)

###########################################################################################
##
##  Specify bc.albers coordinate reference system to ensure everything is consistent
##
## Read-in required sptial datasets:
##  - Okanagan-overlapping DEM (20 m Resolution): DEM_alb.tif (Overlapping Okanagan required to allow correct calculation of slope/aspect before clipping)
##  - Okanagan Landcover Raster (30 m Resolution - Resampled to 20 m Resolution to match DEM): eosd_urban41.tif
##  - Okanagan Parent Soils Raster (resampled to 20m resolution): Soils_PM1.tif
##  - Okanagan Aquifers Shapefile (OBWB Aquifers): OBWB_Aquifer.tif
##  - Model domain subbasins (as delineated by Associated): WS_Raster_Final.tif
##
##  - Okanagan Basin Shapefile: FW_Atlas_OK_Basin.shp
##  - Named/mapped watersheds included in model domain: EFN_WS.shp
###########################################################################################
## QAQC: Could list input files in one text file, rather than hardcoding.

print("reading in spatial data...")

bc.albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

dem <- raster(file.path(global.input.dir, raw.spatial.in.dir, dem.in.file), crs = bc.albers)

landcover <- raster(file.path(global.input.dir, raw.spatial.in.dir, landcover.in.file), crs = bc.albers)

soils <- raster(file.path(global.input.dir, processed.spatial.dir, soils.processed.file), crs = bc.albers)
  
aquifers <- raster(file.path(global.input.dir, raw.spatial.in.dir, aquifer.in.file), crs = bc.albers)

subbasin <- raster(file.path(global.input.dir, raw.spatial.in.dir, WS.raster.in.file), crs = bc.albers)
  
subbasin.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  

model.watersheds <- st_read(file.path(global.input.dir, raw.spatial.in.dir, WS.shape.in.file), crs = bc.albers)
  
## New version of raster package does not seem to support "sf" objects for cropping/masking. So shapefile must be converted to spatialpolygon
model.watersheds.shape <- as(model.watersheds, "Spatial")

print("done reading in all data...")

###########################################################################################
##
## Crop dem, landcover, and subbsain to Okanagan extent
##
###########################################################################################
print("calculating slope and aspect...")

## Calculate slope and aspect for all of BC, and seperate to individual objects
## Default method (neighbors = 8) used here as recommended for rough (vs. smooth) terrain. Methodology follows Horn (1981)..
## The terrain indices are according to Wilson et al. (2007), as in gdaldem (https://gdal.org/programs/gdaldem.html) - i.e., azimuth 0 = north, east = 90, south = 180, west = 270.

slope.aspect <- terrain(dem, opt = c('slope','aspect'), unit = 'degrees', neighbors = 8)

slope <- subset(slope.aspect, subset = 'slope')

aspect <- subset(slope.aspect, subset = 'aspect')


print("cropping extents to the Okanagan...")

## Crop all components to match model watersheds
slope.ok <- mask(crop(slope, model.watersheds.shape), model.watersheds.shape)

aspect.ok <- mask(crop(aspect, model.watersheds.shape), model.watersheds.shape)

dem.ok <- mask(crop(dem, model.watersheds.shape), model.watersheds.shape)

landcover.ok <- mask(crop(landcover, model.watersheds.shape), model.watersheds.shape)

soils.ok <- mask(crop(soils, model.watersheds.shape), model.watersheds.shape)

aquifers.ok <- mask(crop(aquifers, model.watersheds.shape), model.watersheds.shape)

subbasin.ok <- mask(crop(subbasin, model.watersheds.shape), model.watersheds.shape)

###########################################################################################
##
## Compile data.table of coordinates and paramater values
##
###########################################################################################

## Coordinates are extracted row-wise from the top-left to the bottom-right. This is the same extraction order as values() and getValues().
coords <- coordinates(dem.ok)

## ----------------------------------------
##
## Check to ensure all coordinates match (i.e., reprojection and snapping is correct)
##
## ----------------------------------------

coords.slope <- coordinates(slope.ok)

coords.aspect <- coordinates(aspect.ok)

coords.landcover <- coordinates(landcover.ok)

coords.soils <- coordinates(soils.ok)

coords.aquifers <- coordinates(aquifers.ok)

coords.subbasin <- coordinates(subbasin.ok)

if(base::all.equal(coords, coords.slope) != TRUE |
   base::all.equal(coords, coords.aspect) != TRUE |
   base::all.equal(coords, coords.landcover) != TRUE |
   base::all.equal(coords, coords.soils) != TRUE |
   base::all.equal(coords, coords.aquifers) != TRUE |
   base::all.equal(coords, coords.subbasin) != TRUE){
  stop(paste("Coordinates do not match. Ensure all input datasets are reprojected and snapped to the same grid. Use",  dem.in.file, "as the base grid."))
} else{
  print("Coordinates match...continue mapping HRUs")
  rm(coords.slope, coords.aspect, coords.landcover, coords.soils, coords.aquifers, coords.subbasin)
}



## ----------------------------------------
##
## Extract values for all required components
##
## ----------------------------------------
## #TD6: 08052020 - process changed from values() to getValues() to allow values to be pulled from file if/when rasters become larger.
## getValues() and coordinates() both pull values row-wise, from the top-left, to the bottom-right corner of a matrix.

slope.values <- getValues(slope.ok)

aspect.values <- getValues(aspect.ok)

elevation.values <- getValues(dem.ok)

landcover.values <- getValues(landcover.ok)

subbasin.values <- getValues(subbasin.ok)

soils.values <- getValues(soils.ok)

aquifer.values <- getValues(aquifers.ok)


## #B3: 08052020 - Raven expects aspect values calculated as positive COUNTERCLOCKWISE from north.
## Standard convention computes aspect as positive CLOCKWISE from north. Therefore, an adjustment is needed.
## To convert CLOCKWISE to COUNTERCLOCKWISE, subtract from 360.

aspect.values.adjusted <- 360 - aspect.values

## NOTE: By default,R assigns an aspect of 90-degrees to areas with a slope of 0. Flat areas cannont be assigned a value other than 0-360 (Raven requires 0-360); therefore, the default
## value is left. Therefore, followint this correction, all flat areas are assigned a value of 270-degrees (i.e., east facing).


## TEMP SAVE #1 - allows intermediate state to be grabbed if necessary
save.image(file = file.path(tmp.loc, paste("temp1.", Sys.Date(), ".RData", sep = "")))

## Remove unneeded items from workspace
rm(aquifers, aquifers.ok, dem, dem.ok, landcover, landcover.ok, slope.aspect, slope, aspect, slope.ok, aspect.ok, soils, soils.ok, subbasin.ok, model.watersheds, model.watersheds.shape)

gc()

## Put together data table
DT <- data.table(
  coords = coords,
  # coords.subbasin = coords.subbasin, # This isn't needed anymore as coordinate matches are confirmed above.
  slope = slope.values,
  aspect = aspect.values.adjusted, # #B3: 08052020 - use adjusted aspect values, rather than raw values.
  elevation = elevation.values,
  landcover = landcover.values,
  soils = soils.values,
  aquifer = aquifer.values,
  subbasin = subbasin.values
)

###########################################################################################
##
## Generate bins to be used to determine HRUS
##
###########################################################################################

print("Generating binned values for elevation, landcover, and aspect...")

## Determine elevation bands
############################

DT$elevation.bin <- ifelse(elevation.values <= 100, 200,
                    ifelse(elevation.values > 200 & elevation.values <= 300, 201,
                    ifelse(elevation.values > 300 & elevation.values <= 400, 202,
                    ifelse(elevation.values > 400 & elevation.values <= 500, 203,
                    ifelse(elevation.values > 500 & elevation.values <= 600, 204, 
                    ifelse(elevation.values > 600 & elevation.values <= 700, 205,
                    ifelse(elevation.values > 700 & elevation.values <= 800, 206,
                    ifelse(elevation.values > 800 & elevation.values <= 900, 207,
                    ifelse(elevation.values > 900 & elevation.values <= 1000, 208,
                    ifelse(elevation.values > 1000 & elevation.values <= 1100, 209,
                    ifelse(elevation.values > 1100 & elevation.values <= 1200, 210,
                    ifelse(elevation.values > 1200 & elevation.values <= 1300, 211,
                    ifelse(elevation.values > 1300 & elevation.values <= 1400, 212,
                    ifelse(elevation.values > 1400 & elevation.values <= 1500, 213,
                    ifelse(elevation.values > 1500 & elevation.values <= 1600, 214,
                    ifelse(elevation.values > 1600 & elevation.values <= 1700, 215,
                    ifelse(elevation.values > 1700 & elevation.values <= 1800, 216,
                    ifelse(elevation.values > 1800 & elevation.values <= 1900, 217,
                    ifelse(elevation.values > 1900 & elevation.values <= 2000, 218,
                    ifelse(elevation.values > 2000, 219, 999))))))))))))))))))))




## Determine landcover bins
############################
## #TD13 - 22052020 - Update to landcover binning process to reduce error-prone nature. Landcover bins are now read/assigned from LC.in.file, rather than hardcoded.

# Read in Landcover codes/bin info ; convert landcover.values to df to facilitate joining
landcover.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, LC.in.file))

landcover.values.df <- as.data.frame(landcover.values)

colnames(landcover.values.df) <- "Value"

# Join bins to landcover values
tmp <- plyr::join(landcover.values.df, landcover.codes, by = "Value")

# Integrate landcover bins to DT
DT$landcover.bin <- tmp$Bin_Value


## Determine aspect bins
############################
DT$aspect.bin <- ifelse(aspect.values.adjusted >= 315, 400, # North
                 ifelse(aspect.values.adjusted >= 0 & aspect.values.adjusted < 45, 400, # North
                 ifelse(aspect.values.adjusted >= 45 & aspect.values.adjusted < 135, 401, # West #B3: 08052020 - EAST is now WEST as aspect direction changed
                 ifelse(aspect.values.adjusted >= 135 & aspect.values.adjusted < 225, 402, # South
                 ifelse(aspect.values.adjusted >= 225 & aspect.values.adjusted < 315, 403, 999))))) # East #B3: 08052020 - WEST is now EAST as aspect direction changed (999 = something went wrong)

############################
## Check to make sure elevation/aspect binning is completed correctly..
## #TD13 - 22052020 - landcover.bin check removed here as no longer relevant.
if(nrow(DT[DT$elevation.bin == 999, ]) > 0 | nrow(DT[DT$aspect.bin == 999, ]) >0 ){
  stop("One or more of the binning processes did not complete correctly. Review hardcoded elevation & aspect binning processes in hru-generator.R.")
}

## TEMP SAVE #2 - allows intermediate state to be grabbed if necessary
save.image(file = file.path(tmp.loc, paste("temp2.", Sys.Date(), ".RData", sep = "")))

########################################
##
## Remove NA values to reduce size of DT (maintain "DT.revert" just in case...)
## NOTE: Cannot use complete.cases as aquifers as empty under lakes, so it removed HRUs from Kal & Wood lakes
########################################

## Create a backup
DT.revert <- DT

warning("Ensure that the string hardcoded in hru-generator.R for missing Soil Profiles and Aquifer types is correct.")

## Read in soil.codes and aquifer codes to assign values to pixels with blank/missing information
soil.codes <- read.csv(file.path(global.input.dir, processed.spatial.dir, soil.attribute.in.file), 
                       col.names = c("OID", "Value", "Count", "soil_type"))

aquifer.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, AQ.in.file))

## Fill missing aquifer and soil pixels with a value other than NA.
## Assign cells with no aquifer information (i.e., under lakes, or a few random pixels missing aquifer information) with values relating to "NONE" aquifer type so that they're not removed.
DT[is.na(DT$aquifer), "aquifer"] <- aquifer.codes[aquifer.codes$Aquifer_ty == "[NONE]", "Value"]

## For pixels with missing soil information, assign them the "blank" soil profile value - this will be replaced with the most common soil type for each subbasin in the rvh file
## This requires the correct string for "missing" soils to be entered here, matching that included in the soil.attribute.in.file - see check below
DT[is.na(DT$soils), "soils"] <- soil.codes[soil.codes$soil_type == " ", "Value"]

## Remove areas outside the model watershed (i.e., subbasin = NA)
DT <- DT[!is.na(DT$subbasin), ]

############################
## Check that Landcover binning completed correctly. This has to be completed here since NA's are retained throughthe joining processes above.
## #TD13 - 22052020 - Ensure that the landcover bin process completed correctly
if(anyNA(unique(DT$landcover.bin))){
  stop(paste("One or more landcover values / bins are missing from the", LC.in.file, "file. Review and re-generate all HRU bins."))
}

########################################
##
## Replace binned values for all locations which are lakes / reservoirs to ensure that only one HRU will be defined for these.
## NOTE: This may be an unnecessary step, since all reservoirs should be one sub-basin anyway (and therefore generate a unique ID). But it helps to isolate from all components of HRU definitions.
##
########################################

## Identify which subbasins are reservoirs
reservoirs <- subbasin.codes[subbasin.codes$Reservoir_name != "<Null>", "Subbasin_ID"]

## Assign ID of 999 to all rows which are within the reservoir / lake subbasins
DT[DT$subbasin %in% reservoirs, ]$elevation.bin <- 999

DT[DT$subbasin %in% reservoirs, ]$landcover.bin <- 999

DT[DT$subbasin %in% reservoirs, ]$aspect.bin <- 999


########################################
##
## Generate unique IDs for all grid cells to be used in HRU development
##
########################################
print("generating unique ID values for HRUs")

DT$ID <- paste(DT$landcover.bin,
               DT$elevation.bin,
               DT$aspect.bin,
               DT$subbasin,
               sep = '')


print(paste("There are", length(unique(DT$ID)), "unique HRUs within the model domain"))
########################################
##
## Generate Rasters to plot
##
########################################

x <- DT$coords.x

y <- DT$coords.y

landcover.bin <- DT$landcover.bin

subbasin <- DT$subbasin

elevation.bin <- DT$elevation.bin

aspect.bin <- DT$aspect.bin

raw.ID <- as.numeric(DT$ID)

soils.type <- DT$soils


print("Writing spatial layers to file...")

## write landcover bin to raster
m.landcover.bin <- do.call(cbind, list(x, y, landcover.bin))

r.landcover.bin <- rasterFromXYZ(m.landcover.bin, crs = bc.albers)

writeRaster(r.landcover.bin, file.path(global.input.dir, processed.spatial.dir, paste("landcover.bin.", Sys.Date(), ".tif", sep = "")), overwrite = T)


## write subbasin to raster
m.subbasin <- do.call(cbind, list(x, y, subbasin))

r.subbasin <- rasterFromXYZ(m.subbasin, crs = bc.albers)

writeRaster(r.subbasin, file.path(global.input.dir, processed.spatial.dir, paste("subbasin.", Sys.Date(), ".tif", sep = "")), overwrite = T)


## write elevation bin to raster
m.elevation.bin <- do.call(cbind, list(x, y, elevation.bin))

r.elevation.bin <- rasterFromXYZ(m.elevation.bin, crs = bc.albers)

writeRaster(r.elevation.bin, file.path(global.input.dir, processed.spatial.dir, paste("elevation-bin.", Sys.Date(), ".tif", sep = "")), overwrite = T)


## write aspect bin to raster
m.aspect.bin <- do.call(cbind, list(x, y, aspect.bin))

r.aspect.bin <- rasterFromXYZ(m.aspect.bin, crs = bc.albers)

writeRaster(r.aspect.bin, file.path(global.input.dir, processed.spatial.dir, paste("aspect-bin.", Sys.Date(), ".tif", sep = "")), overwrite = T)


## write raw ID to raster
raw.m.ID <- do.call(cbind, list(x, y, raw.ID))

raw.r.ID <- rasterFromXYZ(raw.m.ID, crs = bc.albers)

writeRaster(raw.r.ID, file.path(global.input.dir, processed.spatial.dir, paste("raw-HRU-id.", Sys.Date(), ".tif", sep = "")), overwrite = T)


## write soils to raster
m.soils.type <- do.call(cbind, list(x, y, soils.type))

r.soils.type <- rasterFromXYZ(m.soils.type, crs = bc.albers)

writeRaster(r.soils.type, file.path(global.input.dir, processed.spatial.dir, paste("soils.", Sys.Date(), ".tif", sep = "")), overwrite = T)

print("Done writing spatial layers...")

########################################
##
## Reassign ID's so that they start from 1, rather than stupid large numbers
##
########################################
print("Tidying HRU IDs...")

## Udentify all of the unique IDs/HRUs
unique.ID <- unique(raw.ID)

## Generate a sequence from 1 to the number of HRUs
replace.ID <- 1:length(unique(raw.ID))

## Loop over each unique HRU, find all of it's occurrences, and replace all of those values with the "tidy" value.
for(i in 1:length(unique.ID)){
  place <- which(raw.ID == unique.ID[i])  
  raw.ID[place] <- replace.ID[i]
  print(i)
}

## Add tidy IDs to DT
DT$Tidy.ID <- raw.ID

print("Writing Tidy HRU layer to file...")

# ## write tidy ID to raster
tidy.m.ID <- do.call(cbind, list(x, y, DT$Tidy.ID))

tidy.r.ID <- rasterFromXYZ(tidy.m.ID, crs = bc.albers)

writeRaster(tidy.r.ID, file.path(global.input.dir, processed.spatial.dir, paste("tidy-HRU-id.", Sys.Date(), ".tif", sep = "")), overwrite = T)
            
print("Done writing TIdy HRU layer to file...")

########################################
##
## Plot all spatial layers used in HRU generation
##
########################################

print("Saving all spatial plots to file...")

# ## Plot tidy ID
pdf(file.path(global.input.dir, processed.spatial.dir, paste("HRU-output.", Sys.Date(), ".pdf", sep = "")), height = 17, width = 11)

ncolors <- length(unique.ID)
colpalette <- rgb(runif(ncolors), runif(ncolors), runif(ncolors))

plot(raw.r.ID, col = colpalette, main = "Raw HRUs")

plot(tidy.r.ID, col = colpalette, main = "Tidy HRUs")

plot(r.subbasin, col = colpalette, main = "Subbasins")

plot(r.elevation.bin, col = colpalette, main = "Elevation bins")

plot(r.landcover.bin, col = colpalette, main = "Landcover bins")

plot(r.aspect.bin, col = colpalette, main = "Aspect bins")

plot(r.soils.type, col = colpalette, main = "Soils")

dev.off()

########################################
##
## Reproject HRU coordinates from BC Albers to Lat Lon
## Write csv of final HRUs
##
########################################
print("reprojecting coordinates to lat/lon...")

## create new coords object from the reduced DT (i.e., excluding NAs)
## Use "x" and "y" objects since they were extracted from reduced DT above
coords.reduced <- cbind(x, y)

HRUlatlon <- project(coords.reduced, bc.albers, inverse = T, degrees = T)

DT$X <- HRUlatlon[,1]

DT$Y <- HRUlatlon[,2]

########################################
##
## Remove unneeded items from workspace and save results to RData object
##
########################################
print("Saving output to file...")


rm(list = ls()[! ls() %in% c("DT", "DT.revert", "tmp.loc")])

## Source file configuration again for output paths
source("/var/obwb-hydro-modelling/file-config.R")

save.image(file = file.path(global.input.dir, processed.spatial.dir, paste("okanagan_hru.", Sys.Date(), ".RData", sep = "")))

## Delete Temporary Files
file.remove(file.path(tmp.loc, paste("temp1.", Sys.Date(), ".RData", sep = "")))
file.remove(file.path(tmp.loc, paste("temp2.", Sys.Date(), ".RData", sep = "")))

 
print("Done!")


