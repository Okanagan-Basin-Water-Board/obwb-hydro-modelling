#######################################################################################################################################
##
## This script pre-processes Soils data for inclusion into the Raven Hydrological Modelling Framework. Raw Soils mapping information
## is used from BC Environment http://www.env.gov.bc.ca/esd/distdata/ecosystems/Soil_Data/Soil_Data_Pkgs/
##
#######################################################################################################################################

source("/var/obwb-hydro-modelling/src/functions.R")

library(sf)
library(plyr)
library(stringr)
library(raster)

## Read in GIS information
# soils.layers <- st_read(dsn = "/var/obwb-hydro-modelling/input-data/raw/spatial/soils/BC_Soil_Map.gdb", layer = "BCSLF_Soil_Layer_File")

# soils.poly <- st_read(dsn = "/var/obwb-hydro-modelling/input-data/raw/spatial/soils/BC_Soil_Map.gdb", layer = "BC_Soil_Surveys")

## Read in GIS information, clipped to the Okanagan. Reading in the whole BC datasets (above) returned an error when trying to clip to the Okanagan
bc.albers <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

soils.layers <- read.csv("/var/obwb-hydro-modelling/input-data/raw/spatial/soils/BCSLF_Soil_Layer_File.csv")

soils.poly.base <- st_read("/var/obwb-hydro-modelling/input-data/raw/spatial/soils/Soil_Clip_final.shp")

model.watersheds <- st_read("/var/obwb-hydro-modelling/input-data/raw/spatial/WS_Boundaries_Final.shp")

#############################################################################################
##
## Convert soils polygons and model watersheds to spatial objects
## Define the crs for each
## Isolate all watershed names
## Intersect the two so that only soil polygons within the model watersheds are retained
## Append a unique tag to the soils polygons, depending on their watershed

model.watersheds <- as(model.watersheds, "Spatial")

soils.poly.base <- as(soils.poly.base, "Spatial")

crs(model.watersheds) <- bc.albers

crs(soils.poly.base) <- bc.albers

watersheds <- model.watersheds$GNIS_NAME

## Intersect soils polygons with watersheds
soils.poly.base <- intersect(soils.poly.base, model.watersheds)

## Create unique tag
soils.poly.base$tag <- paste(soils.poly.base$SOILSYM_1, gsub(" ", "-", soils.poly.base$GNIS_NAME), sep = "-")


#############################################################################
##
## Setup the output to house results, and specify the horizons to be included in the thickness calculation
## NOTE: C horizon (Parent material) is not included in the thickness calculation as it not an "active" soil layer.
##    The C horizon is included in the final profiles to act as the GW store.
##

## Specify which horizons should be included in the thickness calculations
horizons <- c("B", "BC", "AB", "BA", "A", "AC")

## Create empty dataframe to bind results to
output <- data.frame(matrix(NA, ncol = ncol(soils.layers)+8, nrow = 0))

#############################################################################
##
## Loop over each watershed and identify watershed-specific soil information
##

for(j in 1:length(watersheds)){
  
  soils.poly <- soils.poly.base[soils.poly.base$GNIS_NAME == watersheds[j], ]

  ## Isolate unique soil symbols within BC
  bc.soils <- as.character(unique(soils.poly$SOILSYM_1))
  
  ## Isolate only records with soil symbol mapped/attached
  bc.soils.layers <- soils.layers[soils.layers$Soil_Symbol %in% bc.soils,]
  
  ## Redefine which soil symbols should be queried
  bc.soils <- as.character(unique(bc.soils.layers$Soil_Symbol))
  
  ## Replace -9 (i.e., NA) with 0 values so they're not included in totals
  bc.soils.layers$TSAND[bc.soils.layers$TSAND == -9] <- 0
  bc.soils.layers$TSILT[bc.soils.layers$TSILT == -9] <- 0
  bc.soils.layers$TCLAY[bc.soils.layers$TCLAY == -9] <- 0
  bc.soils.layers$KSAT[bc.soils.layers$KSAT == -9] <- 0
  
  
  
  
  for(i in 1:length(bc.soils)){
    
    tmp <- bc.soils.layers[bc.soils.layers$Soil_Symbol == bc.soils[i],]
    
    tsand <- sum(tmp$TSAND)
    
    tsilt <- sum(tmp$TSILT)
    
    tclay <- sum(tmp$TCLAY)
    
    total <- sum(tsand, tsilt, tclay, na.rm = T)
    
    
    textures <- c(tsand, tsilt, tclay)
    
    labels <- c("SAN", "SIL", "CLA")
    
    ## Determine the rank of the texture classes
    order(textures, decreasing = T)
    
    if(total == 0){dominant <- tmp$HZN_MAS
    
    } else {
      
      ## order the labels to match the rank of the texture classes
      dominant <- paste(labels[order(textures, decreasing = T)], collapse = "-")
    }
    
    tmp$dominant <- dominant
    
    #########################################################################
    ## Calculate percentage makeup
    
    if(total == 0){frac <- NA
    
    } else {
      
      sand.frac <- round((tsand / total) * 100, -1)
      
      silt.frac <- round((tsilt / total) * 100, -1)
      
      clay.frac <- round((tclay / total) * 100, -1)
      
      
      fractions <- c(sand.frac, silt.frac, clay.frac)
      
      order(fractions, decreasing = T)
      
      frac <- paste(fractions[order(fractions, decreasing = T)], collapse = "-")
      
    }
    
    tmp$frac <- frac
    
    #########################################################################
    ## Determine low/med/high dominant proportion
    
    if(total == 0){flag <- NA
    
    } else {
      
      dom.frac <- fractions[order(fractions, decreasing = T)][1]
      
      # if(!is.na(dom.frac)){
      if(dom.frac >0 & dom.frac < 50){flag <- "low"}
      if(dom.frac >= 50 & dom.frac < 75){flag <- "mod"}
      if(dom.frac >= 75 & dom.frac <= 100){flag <- "high"}
      # } else {flag <- "oops"}
    }
    
    tmp$flag <- flag
    
    
    
    ##########################################################################
    ## Calculate soil profile thickness
    
    if(total == 0){depth <- sum(tmp$HZN_THICK)
    } else {
      
      ## sum the thickness of specified horizons. Note that str_trim is needed to trim false white space (i.e., random spaces) from the tmp object
      thickness <- sum(tmp$HZN_THICK[which(str_trim(as.character(tmp$HZN_MAS), side = "both") %in% horizons)])
      
      if(thickness > 0 & thickness <= 75){depth <- "shallow"}
      if(thickness > 75 & thickness <= 150){depth <- "medium"}
      if(thickness > 150){depth <- "deep"}
    }
    
    tmp$depth <- depth
    
    ##########################################################################
    ## Add soil type tag
    
    if(total == 0){soil_type <- NA
    } else {
      
      soil_type <- paste(labels[order(textures, decreasing = T)][1], flag, depth, sep = "-")
      
    }
    
    tmp$soil_type <- soil_type
    
    
    ##########################################################################
    ## Address custom soil types
    
    tmp[tmp$Soil_Symbol == "BC$GP~~~~~N", "soil_type"] <- "GRAVEL_PIT" ## Gravel Pit
    
    tmp[tmp$Soil_Symbol == "BC$DK~~~~~N", "soil_type"] <- "DIKE" ## Dike
    
    tmp[tmp$Soil_Symbol == "BC$CF~~~~~N", "soil_type"] <- "CUT_FILL" ## Cut and Fill
    
    tmp[tmp$Soil_Symbol == "BC$MA~~~~~N", "soil_type"] <- "MARL" ## MARL
    
    
    ##########################################################################
    ## append watershed to soil type
    tmp$watershed <- watersheds[j]
    
    tmp$base_soil_type <- tmp$soil_type
    
    tmp$soil_type <- paste(tmp$soil_type, gsub(" ", "-", watersheds[j]), sep = "-")
    
    ##########################################################################
    ## Address RAVEN custom soil types
    
    tmp[tmp$Soil_Symbol == "BCZZZ~~~~~N", "soil_type"] <- "LAKE" # OPEN WATER
    
    tmp[tmp$Soil_Symbol == "BCUUU~~~~~N", "soil_type"] <- "URBAN" ## Unclassified Urban
    
    tmp[tmp$Soil_Symbol == "BC$BR~~~~~N" | tmp$Soil_Symbol == "BC$UR~~~~~N" | tmp$Soil_Symbol == "BC$AR~~~~~N", "soil_type"] <- "ROCK" ## Bedrock Basic; Undifferentiated Bedrock; Bedrock Acidic
    
    tmp[tmp$Soil_Symbol == "BCWGL~~~~~N" | tmp$Soil_Symbol == "BCDKLca~~~N" | tmp$Soil_Symbol == "BCDKLca~~~A", "soil_type"] <- "LAKE" ## Willgress Lake; Darke Lake
    
    
    ##########################################################################
    ## Create Tag
    
    tmp$tag <- paste(tmp$SOIL_ID, gsub(" ", "-", watersheds[j]), sep = "-")
    

    ##########################################################################
    ## Assign the same names to output to allow successful rbind
    
    names(output) <- names(tmp)
    
    output <- rbind(output, tmp)
    
  } # End bs.soils loop
  
  print(paste(watersheds[j], "Watershed complete"))

} # End watersheds loop

print(paste("There are", length(unique(output$soil_type)), "unique soil types identified"))


## Write out master soils CSV for remapping soils tif. This must be imported to ArcGIS, and a raster generated using the new soil_type parameters
write.csv(output, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soils-output.csv")

##########################################################################
## Merge table and polygons & Write Shapefile

require(rgdal)

Final.Soils <- merge(soils.poly.base, output, by = 'tag', duplicateGeoms = TRUE)

## Export to shape file
writeOGR(obj=Final.Soils, dsn="/var/obwb-hydro-modelling/input-data/processed/spatial/soils", layer="Soil_type", driver="ESRI Shapefile") # this is in geographical projection

####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################


soil_type <- unique(output$soil_type)

soil_type <- soil_type[!is.na(soil_type)]

results <- data.frame(matrix(NA, ncol = 11, nrow = 0))

results.range <- results

for(i in 1:length(soil_type)){
  
  tmp <- output[output$soil_type == soil_type[i],]
  
  tmp$model.horizon <- NA
  
  ## Group mapped soil layers to A,B,C model horizons
  tmp$model.horizon[tmp$HZN_MAS == "A" | tmp$HZN_MAS == "AB" | tmp$HZN_MAS == "AC"] <- "A"
  tmp$model.horizon[tmp$HZN_MAS == "B" | tmp$HZN_MAS == "BA" | tmp$HZN_MAS == "BC"] <- "B"
  tmp$model.horizon[tmp$HZN_MAS == "C" | tmp$HZN_MAS == "CA" | tmp$HZN_MAS == "CB"] <- "C"
  
  
  tmp2 <- tmp[tmp$model.horizon %in% c("A", "B", "C"),]
  
  ## If one or multiple horizons are missing, add an empty row for that horizon
  if(length(tmp2$model.horizon[tmp2$model.horizon == "A"]) == 0){tmp2[nrow(tmp2)+1, "model.horizon"] <- "A"}
  if(length(tmp2$model.horizon[tmp2$model.horizon == "B"]) == 0){tmp2[nrow(tmp2)+1, "model.horizon"] <- "B"}
  if(length(tmp2$model.horizon[tmp2$model.horizon == "C"]) == 0){tmp2[nrow(tmp2)+1, "model.horizon"] <- "C"}
  
  
  
  ## Average out thickness of each model horizon, and sand, silt, clay percentages.
  ## Convert thickness from cm to m
  thickness.mean <- ddply(tmp2, .(model.horizon), summarize, mean_thickness = mean(HZN_THICK) / 100)
  
  sand <- ddply(tmp2, .(model.horizon), summarize, mean_sand = mean(TSAND))
  
  silt <- ddply(tmp2, .(model.horizon), summarize, mean_silt = mean(TSILT))
  
  clay <- ddply(tmp2, .(model.horizon), summarize, mean_clay = mean(TCLAY))
  
  ksat <- ddply(tmp2, .(model.horizon), summarize, mean_ksat = mean(KSAT))
  
  
  group <- do.call(cbind, list(thickness.mean, sand, silt, clay, ksat))
  
  group$soil_type <- soil_type[i]
  
  
  names(results) <- names(group)
  
  results <- rbind(results, group)
  
  
  
  ##############################################################################
  ##
  ## Determine minimum and maximum soil thicknesses and export to results.range
  
  thickness.min <- ddply(tmp2, .(model.horizon), summarize, min_thickness = min(HZN_THICK) / 100)
  
  thickness.max <- ddply(tmp2, .(model.horizon), summarize, max_thickness = max(HZN_THICK) / 100)
  
  group <- do.call(cbind, list(thickness.min, thickness.max, thickness.mean))  

  group$soil_type <- soil_type[i]
  
  names(results.range) <- names(group)
  
  results.range <- rbind(results.range, group)
  
}


results <- results[!is.na(results$soil_type),]

results[is.na(results)] <- 0

write.csv(results, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soils-parameters.csv")

#########################################################################################################
##
## Overwrite custom soil types
##
#########################################################################################################

## Remove the rows for "special cases" (i.e., Lake, Rock, Wetland, Glacier)
special.cases <- c("LAKE", "ROCK")

results <- results[!grepl(paste(special.cases, collapse = "|"), results$soil_type),]

## Create individual vectors for special cases, as needed by RAVEN (i.e., lake = 0 horizons; rock = zero horizons)
lake <- as.vector(c("LAKE", 0))

rock <- as.vector(c("ROCK", 0))


#########################################################################################################
##
## Generate the required data structure for Raven soil parameters tables
##
#########################################################################################################
require(tidyr)

## Create unique soil class names for each soil layer
results$soil_class_name <- paste(results$soil_type, results$model.horizon, sep = "_")

###################################
##
## Generate the SoilProfiles Table
##
###################################

## Isolate the required columns
profiles <- results[,c("soil_type", "soil_class_name", "mean_thickness")]

## Indentify all unique soil types
soil_type <- unique(profiles$soil_type)

## Create empty recipient for soil profiles information
soil_profiles <- matrix(NA, nrow = 0, ncol = 8)

## Loop through each soil type and restructure data to required raven format. rbind it bottom of soil_profiles
for(i in 1:length(soil_type)){
  
  tmp <- profiles[profiles$soil_type == soil_type[i],]
  
  tmp2 <- as.vector(t(tmp[2:3]))
  
  tmp3 <- c(soil_type[i], 3, tmp2)
  
  soil_profiles <- rbind(soil_profiles, tmp3)
  
}

## Add special cases to the bottom of the table - NA's are places as filler where required.
lake <- c(lake, rep(NA, ncol(soil_profiles) - length(lake)))

rock <- c(rock, rep(NA, ncol(soil_profiles) - length(rock)))

soil_profiles <- do.call("rbind", list(soil_profiles, lake, rock))


## Write soil_profiles to csv to be read-in to rvp-filegenertor.R. The format is as required by Raven
write.csv(soil_profiles, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil-profile-table.csv",
          na = "", row.names = FALSE)


###################################
##
## Generate the SoilClasses Table
##
###################################

## Isolate the required columns
classes <- results[,c("soil_class_name", "mean_sand", "mean_clay", "mean_silt")]

## Remove the rows that are all zeros - no information is available for these
# classes <- classes[!rowSums(classes[,c("mean_sand", "mean_clay", "mean_silt")]) == 0,]

## Redistribute values which do not sum exactly to 100%
for(i in 1:nrow(classes)){
  
  # tryCatch(round_percent(classes[i,c("mean_sand", "mean_clay", "mean_silt")]))
  # classes[i,c("mean_sand", "mean_clay", "mean_silt")] <- 
  classes[i,c("mean_sand", "mean_clay", "mean_silt")] <- round_percent(classes[i,c("mean_sand", "mean_clay", "mean_silt")]) / 100
  
}


## Write classes to csv to be read-in to rvp-filegenerator.R. The format is as required by Raven
write.csv(classes, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil-class-table.csv",
          na = "", row.names = FALSE)



save.image("/var/obwb-hydro-modelling/input-data/processed/spatial/soils/post-process-image.RData")
############################################################################################################################################################
##
## Tidy and write out soil thickness ranges for thickness calibrations
##
############################################################################################################################################################

load("/var/obwb-hydro-modelling/input-data/processed/spatial/soils/post-process-image.RData")

results.range <- results.range[!is.na(results.range$soil_type),]

results.range[is.na(results.range)] <- 0


######################################################
##
## Overwrite custom soil types
##


## Remove the rows for "special cases" (i.e., Lake, Rock, Wetland, Glacier)
special.cases <- c("LAKE", "ROCK")

results.range <- results.range[!grepl(paste(special.cases, collapse = "|"), results.range$soil_type),]

## Create individual vectors for special cases, as needed by RAVEN (i.e., lake = 0 horizons; rock = zero horizons)
lake <- as.vector(c("LAKE", 0))

rock <- as.vector(c("ROCK", 0))

## If desired to calibrate all soil thicknesses, add _THICK to all soil type names
results.range$Parameter_Name <- paste(results.range$soil_type, "_", results.range$model.horizon, "_THICK", sep = "")

results.range$soil_class_name <- paste(results.range$soil_type, "_", results.range$model.horizon, sep = "")


###################################
##
## Generate the SoilProfiles Table
##
###################################

## Isolate the required columns
profiles.range <- results.range[,c("soil_type", "soil_class_name", "Parameter_Name")]

## Indentify all unique soil types
soil_type <- unique(profiles.range$soil_type)

## Create empty recipient for soil profiles information
soil_profiles_range <- matrix(NA, nrow = 0, ncol = 8)

## Loop through each soil type and restructure data to required raven format. rbind it bottom of soil_profiles
for(i in 1:length(soil_type)){
  
  tmp <- profiles.range[profiles.range$soil_type == soil_type[i],]
  
  tmp2 <- as.vector(t(tmp[2:3]))
  
  tmp3 <- c(soil_type[i], 3, tmp2)
  
  soil_profiles_range <- rbind(soil_profiles_range, tmp3)
  
}

## Add special cases to the bottom of the table - NA's are places as filler where required.
lake <- c(lake, rep(NA, ncol(soil_profiles_range) - length(lake)))

rock <- c(rock, rep(NA, ncol(soil_profiles_range) - length(rock)))

soil_profiles_ranges <- do.call("rbind", list(soil_profiles_range, lake, rock))


########################
##
## If desired only to calibrate the thickness of the top layer, and fix B and C layers at 10m, run the following 6 lines of code. If not, write csv's now

soil_profiles_ranges[,6] <- 10

soil_profiles_ranges[,8] <- 10


soil.thickness.range <- results.range[,c("Parameter_Name", "min_thickness", "max_thickness", "mean_thickness")]

# soil.thickness.range <- soil.thickness.range

soil.thickness.range <- soil.thickness.range[which(endsWith(soil.thickness.range$Parameter_Name, "A_THICK")), ]

########################
##
## Write csvs

## Write soil_profiles to csv to be read-in to rvp-filegenertor.R. The format is as required by Raven
write.csv(soil_profiles_ranges, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil-profile-table-calibration.csv",
          na = "", row.names = FALSE)

write.csv(soil.thickness.range, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil-thickness-ranges-calibration.csv",
          na = "", row.names = FALSE)

#####################################
##
## Overwrite min and max soil range thicknesses for calibration purposes.

soil.thickness.range <- read.csv("/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil-thickness-ranges-calibration.csv")

soil.thickness.range$min_thickness <- 0

soil.thickness.range$max_thickness <- 1

write.csv(soil.thickness.range, "/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil-thickness-ranges-calibration.csv",
          na = "", row.names = FALSE)
