###########################################################################################
##
## This script generates initial Hydrologic Response Units (HRUs) for the Okanagan Basin
## 
## Feb-15-2019 LAB
###########################################################################################

tmp.loc <- "/var/obwb-hydro-modelling/input-data/processed/spatial/temp"

##########################
## LOAD REQUIRED PACKAGES
##########################

library(sf)
library(raster)
library(data.table)
library(rgdal)
library(proj4)
library(cloudml)

# Load base packages for error-free execution using Rscript from the command line
# require(stats)
# require(graphics)
# require(grDevices)
# require(utils)
# require(datasets)
library(methods)
# require(base)
# require(tfruns)

source("/var/obwb-hydro-modelling/src/functions.R")

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

dem <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/DEM_Fix2.tif", crs = bc.albers)

# dem <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/dem_alb_fill.tif", crs = bc.albers)

# dem <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/archive/DEM_alb.tif", crs = bc.albers)

landcover <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/eosd_urban41.tif", crs = bc.albers)

# soils <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/Soils_PM1.tif", crs = bc.albers)

soils <- raster("/var/obwb-hydro-modelling/input-data/processed/spatial/soils/Soils_final.tif", crs = bc.albers)

aquifers <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/OBWB_Aquifer.tif", crs = bc.albers)

subbasin <- raster("/var/obwb-hydro-modelling/input-data/raw/spatial/WS_Raster_Final_ID.tif", crs = bc.albers)

subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")



#okanagan.basin <- st_read("/var/obwb-hydro-modelling/input-data/raw/spatial/FW_Atlas_OK_Basin.shp", crs = bc.albers)

model.watersheds <- st_read("/var/obwb-hydro-modelling/input-data/raw/spatial/WS_Boundaries_Final.shp", crs = bc.albers)

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
slope.aspect <- terrain(dem, opt = c('slope','aspect'), unit = 'degrees')

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
  stop("Coordinates do not match. Ensure all input datasets are reprojected and snapped to the same grid. Use DEM_fix2.tif as the base grid.")
} else{
  print("Coordinates match...continue mapping HRUs")
  rm(coords.slope, coords.aspect, coords.landcover, coords.soils, coords.aquifers, coords.subbasin)
}



## ----------------------------------------
##
## Extract values for all required components
##
## ----------------------------------------

slope.values <- values(slope.ok)

aspect.values <- values(aspect.ok)

elevation.values <- values(dem.ok)

landcover.values <- values(landcover.ok)

subbasin.values <- values(subbasin.ok)

soils.values <- values(soils.ok)

aquifer.values <- values(aquifers.ok)


## TEMP SAVE #1
# save.image(file = file.path(tmp.loc, "temp1.RData"))

## Remove unneeded items from workspace
rm(aquifers, aquifers.ok, dem, dem.ok, landcover, landcover.ok, slope.aspect, slope, aspect, slope.ok, aspect.ok, soils, soils.ok, subbasin.ok, model.watersheds, model.watersheds.shape)

gc()

## Put together data table
DT <- data.table(
  coords = coords,
  # coords.subbasin = coords.subbasin,
  slope = slope.values,
  aspect = aspect.values,
  elevation = elevation.values,
  landcover = landcover.values,
  soils = soils.values,
  aquifer = aquifer.values,
  subbasin = subbasin.values
)

## By default, R assigns an aspect of 90-deg to areas with a slope of 0. Herein, we reassign a value of -999 to areas with 0 slope.
## NOTE: RAVEN ONLY ACCEPTS ASPECT BETWEEN 0-360. LEAVE ASPECT AS DEFAULT BEHAVIOUR (I.E., FLAT = 90-DEGREES)
# DT[DT$slope == 0]$aspect <- -999

## Reassign aspect.values based on this new subset
# aspect.values <- DT$aspect

###########################################################################################
##
## Generate bins to be used to determine HRUS
##
###########################################################################################

print("Generating binned values for elevation, landcover, and aspect...")

## Determine elevation bands

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

DT$landcover.bin <- ifelse(landcover.values <= 11, 300, # No data / Unclassified / Cloud 
                    ifelse(landcover.values == 12, 301, # Shadow
                    ifelse(landcover.values == 20, 302, # Water
                    ifelse(landcover.values == 31 , 303, # Snow / Ice
                    ifelse(landcover.values == 30 | landcover.values >= 32 & landcover.values <= 34, 304, # Non-veg Land / Rock/Rubble / Exposed Barren Land / Developed
                    ifelse(landcover.values >= 80 & landcover.values <= 83, 305, # Wetland / Wetland Treed / Wetland Shrub / Wetland Herb
                    ifelse(landcover.values == 40 | landcover.values == 100| landcover.values == 110, 306, # Bryoids / Herbs / Grassland
                    ifelse(landcover.values >= 120 & landcover.values <= 122, 307, # Agriculture / Agriculture Cropland / Agriculture Pasture Forage
                    ifelse(landcover.values >=50 & landcover.values <= 52, 308, # Shrubland / Shrub Tall / Shrub Low
                    ifelse(landcover.values >200 & landcover.values < 1033, 309, # Forest-Trees / Coniferous / Coniferous Dense / Coniferous Open / Coniferous Sparse / Broadleaf / Broadleaf / Broadleaf Dense / Broadleaf Open / Broadleaf Sparse / Mixedwood / Mixedwood Dense / Mixedwood Open / Mixedwood Sparse
                    ifelse(landcover.values == 1033, 310, 300)))))))))))  # URBAN
## Determine aspect bins

DT$aspect.bin <- ifelse(aspect.values >= 315, 400, # North
                       ifelse(aspect.values >= 0 & aspect.values < 45, 400, # North
                       ifelse(aspect.values >= 45 & aspect.values < 135, 401, # East
                       ifelse(aspect.values >= 135 & aspect.values < 225, 402, # South
                       ifelse(aspect.values >= 225 & aspect.values < 315, 403, 999))))) #West (999 = something went wrong)

# DT$aspect.bin <- ifelse(aspect.values >= 270, 400, # north
#                  ifelse(aspect.values >= 0 & aspect.values < 90, 400, # north
#                  ifelse(aspect.values >= 90 & aspect.values < 270, 401, 999))) # south (999 = something went wrong)

## Determine slope bins

# DT$slope.bin <- ifelse(slope.values <= 5, 1,
#                       ifelse(slope.values > 5 & slope.values <= 20, 2,
#                       ifelse(slope.values > 20 & slope.values <= 40, 3,
#                       ifelse(slope.values > 40 & slope.values <= 60, 4,
#                       ifelse(slope.values > 60, 5, 0)))))


########################################
##
## Remove NA values to reduce size of DT (maintain "DT.revert" just in case...)
## NOTE: Cannot use complete.cases as aquifers as empty under lakes, so it removed HRUs from Kal & Wood lakes
########################################

## TEMP SAVE #2
# save.image(file = file.path(tmp.loc, "temp2.RData"))

soil.codes <- read.csv("/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil_attributes.csv",
                       col.names = c("OID", "Value", "Count", "soil_type"))


DT.revert <- DT

## Assign cells with no aquifer information (i.e., under lakes) with 0 so that they're not removed
DT[is.na(DT$aquifer), "aquifer"] <- 0

# ## Assign the most common soil type to all cells with missing soils information - this fills a couple of areas that had no soil polygon available with the most common for the Okanagan
# DT[is.na(DT$soils), "soils"] <- getmode(DT[!is.na(DT$soils), "soils"])

## For pixels with missing soil information, assign them the "blank" soil profile value - this will be replaced with the most common soil type for each subbasin in the rvh file
DT[is.na(DT$soils), "soils"] <- soil.codes[soil.codes$soil_type == " ", "Value"]

## Remove areas outside the model watershed (i.e., subbasin = NA)
DT <- DT[!is.na(DT$subbasin), ]

## Remove incomplete cases (i.e., dead area outside of the model watersheds)
# DT <- DT[complete.cases(DT),]

# NOTE: Remove all cells with empty elevation data
# DT <- DT[!is.na(DT$slope)]

########################################
##
## Replace binned values for all locations which are lakes / reservoirs
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

writeRaster(r.landcover.bin, "/var/obwb-hydro-modelling/input-data/processed/spatial/landcover-bin.tif", overwrite = T)


## write subbasin to raster
m.subbasin <- do.call(cbind, list(x, y, subbasin))

r.subbasin <- rasterFromXYZ(m.subbasin, crs = bc.albers)

writeRaster(r.subbasin, "/var/obwb-hydro-modelling/input-data/processed/spatial/subbasin.tif", overwrite = T)


## write elevation bin to raster
m.elevation.bin <- do.call(cbind, list(x, y, elevation.bin))

r.elevation.bin <- rasterFromXYZ(m.elevation.bin, crs = bc.albers)

writeRaster(r.elevation.bin, "/var/obwb-hydro-modelling/input-data/processed/spatial/elevation-bin.tif", overwrite = T)


## write aspect bin to raster
m.aspect.bin <- do.call(cbind, list(x, y, aspect.bin))

r.aspect.bin <- rasterFromXYZ(m.aspect.bin, crs = bc.albers)

writeRaster(r.aspect.bin, "/var/obwb-hydro-modelling/input-data/processed/spatial/aspect-bin.tif", overwrite = T)


## write raw ID to raster
raw.m.ID <- do.call(cbind, list(x, y, raw.ID))

raw.r.ID <- rasterFromXYZ(raw.m.ID, crs = bc.albers)

writeRaster(raw.r.ID, "/var/obwb-hydro-modelling/input-data/processed/spatial/raw-HRU-id.tif", overwrite = T)


## write soils to raster
m.soils.type <- do.call(cbind, list(x, y, soils.type))

r.soils.type <- rasterFromXYZ(m.soils.type, crs = bc.albers)

writeRaster(r.soils.type, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils.tif", overwrite = T)

print("Done writing spatial layers...")

########################################
##
## Reassign ID's so that they start from 1, rather than stupid large numbers
##
########################################
print("Tidying HRU IDs...")

unique.ID <- unique(raw.ID)
replace.ID <- 1:length(unique(raw.ID))



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

writeRaster(tidy.r.ID, "/var/obwb-hydro-modelling/input-data/processed/spatial/tidy-HRU-id.tif", overwrite = T)

print("Done writing TIdy HRU layer to file...")

########################################
##
## Plot all spatial layers used in HRU generation
##
########################################

print("Saving all spatial plots to file...")

# ## Plot tidy ID
pdf("/var/obwb-hydro-modelling/input-data/processed/spatial/HRU-output.pdf", height = 17, width = 11)

ncolors=length(unique.ID)
colpalette<-rgb(runif(ncolors),runif(ncolors ),runif(ncolors ))

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

## create new coords object drom the reduced DT (i.e., excluding NAs)
## QAQC: Replace this with cbind(x, y) since they were extracted from DT above to create the series of raster
coords.reduced <- matrix(nrow = nrow(DT), ncol = 2, data = c(x = DT$coords.x,
                                                     y = DT$coords.y))

HRUlatlon <- project(coords.reduced, bc.albers, inverse = T, degrees = T)

DT$X <- HRUlatlon[,1]

DT$Y <- HRUlatlon[,2]

########################################
##
## Remove unneeded items from workspace and save results to RData object and csv file
##
########################################
print("Saving output to file...")

# rm(list = ls()[! ls() %in% c("DT", "DT.revert")])
rm(list = ls()[! ls() %in% c("DT", "DT.revert")])

save.image(file = "/var/obwb-hydro-modelling/input-data/processed/spatial/okanagan_hru.RData")

# write.csv(DT, "/var/obwb-hydro-modelling/input-data/processed/spatial/okanagan_hru.csv")

print("Done!")
