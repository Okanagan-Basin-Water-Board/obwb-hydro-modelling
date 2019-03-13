###########################################################################################
##
## This script generates initial Hydrologic Response Units (HRUs) for the Whiteman Creek
## watershed. The following GIS layers were provided by Dan Austin on Dec 19, 2018:
## - Provincially mapped watershed and sub-basin boundaries (i.e., XXX)
## - Provincially mapped landcover (i.e., XXXX)
## 
## Feb-15-2019 LAB
###########################################################################################

##########################
## LOAD REQUIRED PACKAGES
##########################

library(sf)
library(raster)
library(data.table)
library(rgdal)
library(proj4)

library(cloudml)

# ## Load in updated raw data from bucket
# gs_rsync("gs://associated-environmental/hru-generation/raw/", "/home/lawrence/var/Data/Raw/")

###########################################################################################
##
## Read-in required sptial datasets:
##  - Okanagan basin DEM (20 m Resolution): DEM_alb.tif
##  - Okanagan Landcover Raster (30 m Resolution - Resampled to 20 m Resolution to match DEM): EOSD_alb_Snap.tif
##  - Okanagan Basin Shapefile: Okanagan_River.shp
##  - Okanagan named watersheds: Okanagan_Watersheds.shp
##  - Okanagan subbasins: Okanagan_subbasins.shp
###########################################################################################

bc.albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"



dem <- raster("/home/lawrence/var/Data/Raw/DEM_alb.tif", crs = bc.albers)

landcover <- raster("/home/lawrence/var/Data/Raw/EOSD_alb_Snap.tif", crs = bc.albers)

soils <- raster("/home/lawrence/var/Data/Raw/Soils_PM1.tif", crs = bc.albers)

aquifers <- raster("/home/lawrence/var/Data/Raw/OBWB_Aquifer.tif", crs = bc.albers)

subbasin <- raster("/home/lawrence/var/Data/Raw/WS_Raster3.tif", crs = bc.albers)



okanagan.basin <- st_read("/home/lawrence/var/Data/Raw/FW_Atlas_OK_Basin.shp", crs = bc.albers)

model.watersheds <- st_read("/home/lawrence/var/Data/Raw/EFN_WS.shp", crs = bc.albers)

## New version of raster package does not seem to support "sf" objects for cropping/masking. So shapefile must be converted to spatialpolygon
model.watersheds.shape <- as(model.watersheds, "Spatial")

# whiteman <- model.watersheds[model.watersheds$GNIS_NAME == "Whiteman Creek",]
# ###########################################################################################
# ##
# ## Read-in subbasins for Whiteman Creek
# ##
# ###########################################################################################
# 
# ## filepath to geodatabase with all subbasins
# geodatabase <- "//VER-AUSTIN/mapping/_projects/2019/Raven/Watersheds_subbasins.gdb"
# 
# ## list all available layers in geodatabase
# layer.list <- ogrListLayers(geodatabase)
# 
# ## print the layer list
# print(layer.list)
# 
# ## read in layer list
# subbasin <- readOGR(dsn = geodatabase, layer = layer.list[1])
# 
# ## convert spatial polygons dataframe to sf object
# subbasin <- st_as_sf(subbasin, crs = bc.albers)
# 
# whiteman.shape <- subbasin[subbasin$NAMED_WATERSHED_ID == 6346,]
# # 

###########################################################################################
##
## Crop dem, landcover, and subbsain to Okanagan extent
##
###########################################################################################

slope.aspect <- terrain(dem, opt = c('slope','aspect'), unit = 'degrees')

slope <- subset(slope.aspect, subset = 'slope')

aspect <- subset(slope.aspect, subset = 'aspect')


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

# coords.subbasin <- coordinates(subbasin.ok)

# coords.landcover <- coordinates(landcover.ok)

slope.values <- values(slope.ok)

aspect.values <- values(aspect.ok)

elevation.values <- values(dem.ok)

landcover.values <- values(landcover.ok)

subbasin.values <- values(subbasin.ok)

soils.values <- values(soils.ok)

aquifer.values <- values(aquifers.ok)


## Remove unneeded items from workspace
rm(aquifers, aquifers.ok, dem, dem.ok, landcover, landcover.ok, slope.aspect, slope, aspect, slope.ok, aspect.ok,
   soils, soils.ok, subbasin.ok, model.watersheds, model.watersheds.shape)

gc()

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

###########################################################################################
##
## Generate bins to be used to determine HRUS
##
###########################################################################################

## Determine elevation bands

DT$elevation.bin <- ifelse(elevation.values <= 200, 200,
                    ifelse(elevation.values > 200 & elevation.values <= 400, 202,
                    ifelse(elevation.values > 400 & elevation.values <= 600, 203,
                    ifelse(elevation.values > 600 & elevation.values <= 800, 204,
                    ifelse(elevation.values > 800 & elevation.values <= 1000, 205, 
                    ifelse(elevation.values > 1000 & elevation.values <= 1200, 206,
                    ifelse(elevation.values > 1200 & elevation.values <= 1400, 207,
                    ifelse(elevation.values > 1400 & elevation.values <= 1600, 208,
                    ifelse(elevation.values > 1600 & elevation.values <= 1800, 209,
                    ifelse(elevation.values > 1800 & elevation.values <= 2000, 210,
                    ifelse(elevation.values > 2000, 211, 212)))))))))))

## Determine landcover classes

DT$landcover.bin <- ifelse(landcover.values <= 11, 300, # No data / Unclassified / Cloud 
                    ifelse(landcover.values == 12, 301, # Shadow
                    ifelse(landcover.values == 20, 302, # Water
                    ifelse(landcover.values == 31 , 303, # Snow / Ice
                    ifelse(landcover.values == 30 | landcover.values >= 32 & landcover.values <= 34, 304, # Non-veg Land / Rock/Rubble / Exposed Barren Land / Developed
                    ifelse(landcover.values >= 80 & landcover.values <= 83, 305, # Wetland / Wetland Treed / Wetland Shrub / Wetland Herb
                    ifelse(landcover.values == 40 | landcover.values == 100| landcover.values == 110, 306, # Bryoids / Herbs / Grassland
                    ifelse(landcover.values >= 120 & landcover.values <= 122, 307, # Agriculture / Agriculture Cropland / Agriculture Pasture Forage
                    ifelse(landcover.values >=50 & landcover.values <= 52, 308, # Shrubland / Shrub Tall / Shrub Low
                    ifelse(landcover.values >200, 309, 300)))))))))) # Forest-Trees / Coniferous / Coniferous Dense / Coniferous Open / Coniferous Sparse
                                                                  # Broadleaf / Broadleaf / Broadleaf Dense / Broadleaf Open / Broadleaf Sparse
                                                                  # Mixedwood / Mixedwood Dense / Mixedwood Open / Mixedwood Sparse


# DT$aspect.bin <- ifelse(aspect.values >= 315, 400, # North
#                        ifelse(aspect.values >= 0 & aspect.values < 135, 401, # North
#                        ifelse(aspect.values >= 45 & aspect.values < 135, 402, # East
#                        ifelse(aspect.values >= 135 & aspect.values < 225, 403, # Weast
#                        ifelse(aspect.values >= 225 & aspect.values < 315, 404, 0))))) #South (0 = -999)

DT$aspect.bin <- ifelse(aspect.values >= 270, 400, # north
                 ifelse(aspect.values >= 0 & aspect.values < 90, 400, # north
                 ifelse(aspect.values >= 90 & aspect.values < 270, 401, 0))) # south (0 = -999)

########################################
##
## Remove NA values to reduce size of DT (maintain "DT.revert" just in case...)
##
########################################

DT.revert <- DT

DT <- DT[complete.cases(DT), ]


########################################
##
## Generate unique IDs for all grid cells to be used in HRU development
##
########################################

DT$ID <- paste(DT$landcover.bin,
               DT$elevation.bin,
               DT$aspect.bin,
               DT$subbasin,
               sep = '')

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

ID <- as.numeric(DT$ID)



# ## write landcover bin to raster
# m.landcover.bin <- matrix(nrow = length(landcover.bin), ncol = 3, data = c(x = x,
#                                                       y = y,
#                                                       z = landcover.bin))
# 
# r.landcover.bin <- rasterFromXYZ(m.landcover.bin, crs = bc.albers)
# 
# 
# writeRaster(r.landcover.bin, "/home/lawrence/var/Data/Processed/landcover-bin.tif", overwrite = T)
# 
# 
# 
# ## write subbasin to raster
# m.subbasin <- matrix(nrow = length(subbasin), ncol = 3, data = c(x = x,
#                                                           y = y,
#                                                           z = subbasin))
# 
# r.subbasin <- rasterFromXYZ(m.subbasin, crs = bc.albers)
# 
# 
# writeRaster(r.subbasin, "/home/lawrence/var/Data/Processed/subbasin.tif", overwrite = T)
# 
# 
# 
# ## write elevation bin to raster
# m.elevation.bin <- matrix(nrow = length(elevation.bin), ncol = 3, data = c(x = x,
#                                                                y = y,
#                                                                z = elevation.bin))
# 
# r.elevation.bin <- rasterFromXYZ(m.elevation.bin, crs = bc.albers)
# 
# writeRaster(r.elevation.bin, "/home/lawrence/var/Data/Processed/elevation-bin.tif", overwrite = T)
# 
# 
# ## write elevation bin to raster
# m.aspect.bin <- matrix(nrow = length(aspect.bin), ncol = 3, data = c(x = x,
#                                                                            y = y,
#                                                                            z = aspect.bin))
# 
# r.aspect.bin <- rasterFromXYZ(m.aspect.bin, crs = bc.albers)
# 
# writeRaster(r.aspect.bin, "/home/lawrence/var/Data/Processed/aspect-bin-2bins.tif", overwrite = T)
# 
# 
# ## write raw ID to raster
# m.ID <- matrix(nrow = length(ID), ncol = 3, data = c(x = x,
#                                                     y = y,
#                                                     z = ID))
# 
# 
# r.ID <- rasterFromXYZ(m.ID, crs = bc.albers)
# 
# writeRaster(r.ID, "/home/lawrence/var/Data/Processed/ID-raw.tif", overwrite = T)
# 

########################################
##
## Reassign ID's so that they start from 1, rather than 3 million
##
########################################

unique.ID <- unique(ID)
replace.ID <- 1:length(unique(ID))


for(i in 1:length(unique.ID)){
  place <- which(ID == unique.ID[i])  
  ID[place] <- replace.ID[i]
  print(i)
}


## Reassign tidy IDs to DT
DT$ID <- ID

# ## write tidy ID to raster
# m.ID <- matrix(nrow = length(DT$ID), ncol = 3, data = c(x = x,
#                                                      y = y,
#                                                      z = ID))
# 
# 
# r.ID <- rasterFromXYZ(m.ID, crs = bc.albers)
# 
# writeRaster(r.ID, "/home/lawrence/var/Data/Processed/ID-tidy.tif", overwrite = T)
# 
# 
# ## Plot tidy ID
# ncolors=length(unique.ID)
# colpalette<-rgb(runif(ncolors),runif(ncolors ),runif(ncolors ))
# 
# plot(r.ID, col = colpalette)


########################################
##
## Reproject HRU coordinates from BC Albers to Lat Lon
## Write csv of final HRUs
##
########################################

## create new coords object drom the reduced DT (i.e., excluding NAs)
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


rm(list = ls()[! ls() %in% c("DT", "DT.revert")])

save.image(file = "/home/lawrence/var/Data/Processed/okanagan_hru.RData")

write.csv(DT, "/home/lawrence/var/Data/Processed/okanagan_HRU.csv")
