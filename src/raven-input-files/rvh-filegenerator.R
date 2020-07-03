############################################################################################################################
##
## This script takes output from "hru-generator.R" and generates the required *.rvh file for input into Raven.
##
## Feb-28-2019
##
############################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

warning(paste("Ensure that ", okanagan.hru.table.file, " is the latest processed HRU table", sep = ""))

Sys.sleep(10)

## Generate required functions
source("/var/obwb-hydro-modelling/src/functions.R")


## Load Required Packages
require(raster)
require(plyr)

## Specify the subbasin ID for Brenda Mines - this is used to replace landuse/vegetation cover under natural conditions
bm.subbasin.id <- 2709

## Print warning for users to confirm that the BM subbasin is correct.
warning(paste("Confirm that Brenda Mines Subbasin ID is still Subbasin", bm.subbasin.id))


#################################################3
##
## Read-in required datasets and variables
##
#################################################3
print(paste("loading", okanagan.hru.table.file))

load(file.path(global.input.dir, processed.spatial.dir, okanagan.hru.table.file))
  
print("all loaded")

HRU.table <- as.data.frame(DT)

rm(DT, DT.revert)

#################################################3
##
## Add a "vegetation" column to allow breakdown of vegetation types. This will be determined from "value" of landcover dataset
##
#################################################3

# currently just duplicating the "landcover" column. This is an unnecessary step, but is nice for cosistency/visual interpretation
# The vegetation type could simply be determined from the landcover column instead.
HRU.table$vegetation <- HRU.table$landcover


##################################################################################################
##
## Generate required HRU output table (in format required by RAVEN)
##
##################################################################################################

HRU.output <- matrix(nrow = length(unique(HRU.table$ID)), ncol = 13, NA)

colnames(HRU.output) <- c("ID",
                          "AREA",
                          "ELEVATION",
                          "LATITUDE",
                          "LONGITUDE",
                          "BASIN_ID",
                          "LAND_USE_CLASS",
                          "VEG_CLASS",
                          "SOIL_PROFILE",
                          "AQUIFER_PROFILE",
                          "TERRAIN_CLASS",
                          "SLOPE",
                          "ASPECT")

unique.HRU <- unique(HRU.table$Tidy.ID)

## Populate table with summary of all HRUs
print("generating HRU-table...")

ptm <- proc.time()

## NOTE: Should the rounding within this table be removed to correct the "total modelled area"?
for(i in 1:length(unique.HRU)){
  
  index <- which(HRU.table$Tidy.ID == unique.HRU[i])
  
  HRU.output[i, "ID"] <- unique.HRU[i]
  
  HRU.output[i,"AREA"] <- round((length(index) * 19.80255838 * 19.80255838) / (1000 * 1000), 4)
  
  ## #TD14: 17062020 - Raven expects HRU elevations to be the MEAN elevation. Confirmed with James Craig on June 17, 2020.
  ## Median was originally used as it is more commonly used as an elevation of interest in hydrology assessment. However, now updated to mean reflect Raven requirements.
  HRU.output[i, "ELEVATION"] <- round(mean(HRU.table[index,"elevation"]), 2)
  
  ## #TD7: 15052020 - James Craig confirmed that mean is appropriate here as it determines the geometric centre of the hru.
  HRU.output[i, "LATITUDE"] <- round(mean(HRU.table[index, "Y"]), 4)
  
  HRU.output[i, "LONGITUDE"] <- round(mean(HRU.table[index, "X"]), 4)
  
  HRU.output[i, "BASIN_ID"] <- unique(HRU.table[index, "subbasin"])
  
  HRU.output[i, "LAND_USE_CLASS"] <- unique(HRU.table[index, "landcover.bin"])
  
  HRU.output[i, "VEG_CLASS"] <- getmode(HRU.table[index, "vegetation"])
  
  HRU.output[i, "SOIL_PROFILE"] <- getmode(HRU.table[index, "soils"])
  
  HRU.output[i, "AQUIFER_PROFILE"] <- getmode(HRU.table[index, "aquifer"])
  
  HRU.output[i, "TERRAIN_CLASS"] <- "[NONE]"

  HRU.output[i, "SLOPE"] <- round(mean(HRU.table[index, "slope"]), 2)
  
  HRU.output[i, "ASPECT"] <- round(mean(HRU.table[index, "aspect"]), 2)

  print(i)
}

proc.time() - ptm

print("HRU table built")

save.image(file = file.path(global.input.dir, processed.spatial.dir, paste("Raw-HRU-Output.", Sys.Date(), ".RData", sep = "")))
  
load(file = file.path(global.input.dir, processed.spatial.dir, paste("Raw-HRU-Output.", Sys.Date(), ".RData", sep = "")))
###########################################################################
##
## Assign names to soil profiles, aquifer profiles, and land use classes
##
## Note: This requires attribute tables for soil, aquifer, and lancover raster to be ingested as csv files
##
###########################################################################

soil.codes <- read.csv(file.path(global.input.dir, processed.spatial.dir, soil.attribute.in.file), col.names = c("OID", "Value", "Count", "soil_type"))

aquifer.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, AQ.in.file))
  
landcover.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, LC.in.file))
  
vegetation.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, Veg.in.file))
  
subbasin.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  
## Populate all soil, aquifer, land use and velegation class values with appropriate types, based on lookup with codes csv's.
for(i in 1:nrow(HRU.output)){

  HRU.output[i, "SOIL_PROFILE"] <- ifelse(is.na(HRU.output[i, "SOIL_PROFILE"]), "[NONE]", as.character(soil.codes$soil_type)[HRU.output[i,"SOIL_PROFILE"] == soil.codes$Value])
  
  HRU.output[i, "AQUIFER_PROFILE"] <- ifelse(is.na(HRU.output[i, "AQUIFER_PROFILE"]), "[NONE]", as.character(aquifer.codes$Aquifer_ty)[HRU.output[i,"AQUIFER_PROFILE"] == aquifer.codes$Value])
  
  HRU.output[i, "LAND_USE_CLASS"] <- ifelse(is.na(HRU.output[i, "LAND_USE_CLASS"]), "[NONE]", as.character(landcover.codes$Bin_type)[HRU.output[i, "LAND_USE_CLASS"] == landcover.codes$Bin_Value])
  
  ## note that vegetation type is determined based on the "Value", rather than the "Bin_Value". Bin value refers to the landcover bin, rather than the specific vegetation bin.
  HRU.output[i, "VEG_CLASS"] <- ifelse(is.na(HRU.output[i, "VEG_CLASS"]), "[NONE]", as.character(vegetation.codes$Bin_type)[HRU.output[i, "VEG_CLASS"] == vegetation.codes$Value])
  
  print(i)

}

## Remove HRUs with zero area.
HRU.output.clean <- HRU.output[!as.numeric(HRU.output[,"AREA"]) <= 0, ]

## Check to see if any HRUs were removed (i.e., have zero area). If so, throw an error.
if(nrow(HRU.output) != nrow(HRU.output.clean)){
  stop(print("Some HRUs were removed due to zero area. Review HRU generation."))
}


##################################################################################################
##
## Replace missing SOIL_PROFILE with the most common SOIL_PROFILE in each given subbasin
##
##################################################################################################

warning("Ensure that the approach used to fill missing SOIL_PROFILE information is reasonable given the input datasets.")

## Identify the subbasins that have missing soil profile information
missing.soil.profiles.basin <- HRU.output.clean[which(HRU.output.clean[, "SOIL_PROFILE"] == " "), "BASIN_ID"]

unique.missing.soil.profiles.basin <- unique(missing.soil.profiles.basin)

## For each subbasin with missing soil profile information...
for(i in 1:length(unique.missing.soil.profiles.basin)){
  
  ## create subset of all HRUs in given subbasin, listing only their area and soil profile information
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.missing.soil.profiles.basin[i], c("AREA", "SOIL_PROFILE")]
  
  ## Convert to a dataframe
  sub.df <- as.data.frame(sub)
  
  ## Calculate the total area covered by each soil profile within the subbasin
  SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the soil profile that covers the largest area within the subbasin that is NOT blank
  common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != " ", "total"])), "SOIL_PROFILE"]
  
  ## Replace the missing soil information in the HRU.output.clean table for all HRUs with missing soil profiles for the given subbasin
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.missing.soil.profiles.basin[i] & HRU.output.clean[, "SOIL_PROFILE"] == " "), "SOIL_PROFILE"] <- as.character(common.SP)

}

## Replace soil profiles underneath "WATER" Landuse and "WATER" Vegetation HRUs with LAKE profiles - this turns off all soil processed under open water HRUs.
HRU.output.clean[which(HRU.output.clean[,"LAND_USE_CLASS"] == "WATER" & HRU.output.clean[,"VEG_CLASS"] == "WATER"), "SOIL_PROFILE"] <- "LAKE"

##################################################################################################
##
## Replace SOIL_PROFILE and VEG_CLASS with LAKE for HRUs which are Lakes / Reservoirs
##
##################################################################################################
reservoirs <- as.character(subbasin.codes[subbasin.codes$Reservoir_name != "<Null>", "Subbasin_ID"])

HRU.output.clean[HRU.output.clean[, "BASIN_ID"] %in% reservoirs, "VEG_CLASS"] <- "LAKE"

HRU.output.clean[HRU.output.clean[, "BASIN_ID"] %in% reservoirs, "SOIL_PROFILE"] <- "LAKE"


##################################################################################################
##
## For HRUs with ROCK SOIL_PROFILE, replace LAND_USE_CLASS and VEG_CLASS with NON_VEGETATED (simply to represent no canopy)
##
##################################################################################################
## #TD8: 11052020 - To ensure landcover information is given preference over soil information throughout, ROCK soils that are NOT URBAN or NON_VEGETATED LUC are replaced.

## Identify the subbasins that have missing soil profile information
rock.soil.profiles.basins <- HRU.output.clean[which(HRU.output.clean[, "SOIL_PROFILE"] == "ROCK"), "BASIN_ID"]

unique.rock.soil.profiles.basins <- unique(rock.soil.profiles.basins)

## For each subbasin with rock soil profiles...
for(i in 1:length(unique.missing.soil.profiles.basin)){
  
  ## create subset of all HRUs in given subbasin, listing only their area and soil profile information
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == rock.soil.profiles.basins[i], c("AREA", "SOIL_PROFILE")]
  
  ## Convert to a dataframe
  sub.df <- as.data.frame(sub)
  
  ## Calculate the total area covered by each soil profile  within the subbasin
  SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the soil profile that covers the largest area within the subbasin that is NOT ROCK
  common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != "ROCK", "total"])), "SOIL_PROFILE"]
  
  ## Replace the missing soil information in the HRU.output.clean table for all HRUs with missing soil profiles for the given subbasin
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == rock.soil.profiles.basins[i] & HRU.output.clean[, "SOIL_PROFILE"] == "ROCK" & HRU.output.clean[, "LAND_USE_CLASS"] != "URBAN" & HRU.output.clean[, "LAND_USE_CLASS"] != "NON_VEGETATED"), "SOIL_PROFILE"] <- as.character(common.SP)
  
}

##################################################################################################
##
## Replace SHADOW Land use and vegetation classes with most common land use and vegetaion class (by area) by subbasin
##
##################################################################################################

## Identify the subbasins that have "SHADOW" land use types
shadow.subbasin <- HRU.output.clean[which(HRU.output.clean[, "LAND_USE_CLASS"] == "SHADOW"), "BASIN_ID"]

## Identify all unique subbasins with "SHADOW" land use types
unique.shadow.subbasin <- unique(shadow.subbasin)

## For each subbasin with "SHADOW" land use type, replace land use type, and vegetation type, with the most common (by area) for the given subbasin
for(i in 1:length(unique.shadow.subbasin)){
  
  ##-----------------------------------------
  ##
  ## Replace Land Use Class
  ##
  ##-----------------------------------------
  
  ## Identify all HRUs that are within the gievn subbasin
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.shadow.subbasin[i], c("AREA", "LAND_USE_CLASS", "VEG_CLASS", "SOIL_PROFILE")]
  
  ## Convert to a datafram
  sub.df <- as.data.frame(sub)
  
  ## Calculate the total area covered by each land use class
  LUC.proportion <- ddply(sub.df, .(LAND_USE_CLASS), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the most common land use class that is NOT "SHADOW"
  common.LUC <- LUC.proportion[which(LUC.proportion$total == max(LUC.proportion[LUC.proportion$LAND_USE_CLASS != "SHADOW", "total"])), "LAND_USE_CLASS"]
  
  # original <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.non.veg.subbasin[i], ]
  
  ## Replace all "SHADOW" Land use types with the most common other land use class
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.shadow.subbasin[i] & HRU.output.clean[, "LAND_USE_CLASS"] == "SHADOW"), "LAND_USE_CLASS"] <- as.character(common.LUC)
  
  
  ##-----------------------------------------
  ##
  ## Replace Vegetation Class
  ##
  ##-----------------------------------------
  
  ## #B1: 11052020 - Update to prevent non-logical land use/vegetation combinations.
  ## determine most common vegatation class WITHIN the most common Land Use Class. This prevents issues with FORESTED/GRASS definitions (when forested is main LUC, but because of many different forest vegetation types, grass is most common vegetation type).
  sub.df.lc <- sub.df[sub.df$LAND_USE_CLASS == common.LUC, ]
  
  VC.proportion <- ddply(sub.df.lc, .(VEG_CLASS), summarize, total = sum(as.numeric(as.character(AREA))))
  
  common.VC <- VC.proportion[which(VC.proportion$total == max(VC.proportion[VC.proportion$VEG_CLASS != "SHADOW", "total"])), "VEG_CLASS"]
  
  ## Replace all "SHADOW" vegetation class with the most common (by area) other vegetation class within the most common land use class.
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.shadow.subbasin[i] & HRU.output.clean[, "VEG_CLASS"] == "SHADOW"), "VEG_CLASS"] <- as.character(common.VC)
  
}

##################################################################################################
##
## For those HRUs that have LAKE Soil profile, but not WATER or LAKE Land use, replace the soil profiles with the most common (by area) within a given subbasin
## - This process preferences the landuse dataset over the soil dataset.
##
##################################################################################################

## Identify those that are LAKE soils, but not WATER landuse
lake.soil.subbasins <- HRU.output.clean[which(HRU.output.clean[,"SOIL_PROFILE"] == "LAKE" & HRU.output.clean[,"LAND_USE_CLASS"] != "WATER" & HRU.output.clean[,"VEG_CLASS"] != "WATER"), ]

## Remove those that are LAKE soils and LAKE land use - these are correct.
lake.soil.subbasins <- lake.soil.subbasins[which(lake.soil.subbasins[,"LAND_USE_CLASS"] != "LAKE"), "BASIN_ID"]

## Identify all unique subbasins where this occurs.
unique.lake.soil.subbasins <- unique(lake.soil.subbasins)

for(i in 1:length(unique.lake.soil.subbasins)){
  
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.lake.soil.subbasins[i], ]
  
  ## Convert to a datafram
  sub.df <- as.data.frame(sub)
  
  ## Calculate the total area covered by each soil profile
  SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the most common soil profile that is NOT blank
  common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != "LAKE", "total"])), "SOIL_PROFILE"]
  
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.lake.soil.subbasins[i] & HRU.output.clean[, "SOIL_PROFILE"] == "LAKE" & !HRU.output.clean[, "LAND_USE_CLASS"] %in% c("LAKE", "WATER")), "SOIL_PROFILE"] <- as.character(common.SP)

}

##################################################################################################
##
## #B4: 11052020 - Overwrite the slope for Lakes/reservoirs/open water to be ZERO
## - This prevents lakes/reservoirs inadvertently being placed on steep hillslopes.
##
##################################################################################################

HRU.output.clean[HRU.output.clean[, "LAND_USE_CLASS"] == "LAKE" | HRU.output.clean[, "LAND_USE_CLASS"] == "WATER", "SLOPE"] <- 0

## Write HRU Table to file incase adjustments are needed
save.image(file = file.path(global.input.dir, processed.spatial.dir, paste("Raven-HRU-Table.", Sys.Date(), ".RData", sep = "")))
             
load(file.path(global.input.dir, processed.spatial.dir, paste("Raven-HRU-Table.", Sys.Date(), ".RData", sep = "")))

##################################################################################################
##
## Generate required Subbasin output table (in format required by RAVEN)
##
##################################################################################################

warning(paste("Ensure the SB.in.file (", SB.in.file, ") contains attribute information that matches the WS.raster.in.file (", WS.raster.in.file, ").", sep = ""))

## Read in subbasin attribute table from Dan - **ensure the attribute table matches the version of the subbasin raster *.tif being used**
subbasin.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))

## replce space in watershed name with "_"
subbasin.codes$GNIS_NAME <- gsub("\\s", "_", subbasin.codes$GNIS_NAME)

subbasin.codes$SubBasin_name <- gsub("\\s", "_", subbasin.codes$SubBasin_name)

Subbasin.output <- matrix(nrow = nrow(subbasin.codes), ncol = 6, NA)

colnames(Subbasin.output) <- c("ID",
                               "NAME",
                               "DOWNSTREAM_ID",
                               "PROFILE",
                               "REACH_LENGTH",
                               "GAUGED")

Subbasin.output[, 1] <- subbasin.codes$Subbasin_ID

Subbasin.output[, 2] <- as.character(subbasin.codes$SubBasin_name)

Subbasin.output[, 3] <- as.character(subbasin.codes$Downstream_ID)

Subbasin.output[, 4] <- "DEFAULT_TRAP"

Subbasin.output[, 5] <- "_AUTO"

Subbasin.output[, 6] <- as.character(subbasin.codes$Gauged) ## Pull the gauged fla (i.e., 1/0) from the subbasin_codes.csv rather than using the lines below to determine which subbasins are gauged.

## Assign zero reach length to those subbasins which are reservoirs
Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Reservoir_name != "<Null>"], 5] <- 0

# ## Assign all subbasins a 0 so that they are ungauged. Following lines overwrite this behaviour for subbasins that should be gauged
# Subbasin.output[, 6] <- 0
# 
# ## Flag all subbasins which have a WSC station associated with them to be gauged (1). All other subbasins are ungauged (0)
# Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Hydrometric_stn != "<Null>"], 6] <- 1
# 
# ## Flag all subbasins which are a reservoir. All other subbasins are ungauged (0)
# Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Reservoir_name != "<Null>"], 6] <- 1
# 
# 
# ## Flag all subbasin are either: The Apex of the Fan (Reports_to_Fan == "A") OR are the mouth of the creek (Downstream_ID == -1)
# Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Reports_to_Fan == "A"], 6] <- 1
# 
# Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Downstream_ID == -1], 6] <- 1


###########################################################################
##
## Generate "Residual" *.rvh file
##
###########################################################################

# RVHoutFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = ""))
RVHoutFile.Residual <- file.path(global.simulation.dir, paste("Master_residual.", Sys.Date(), ".rvh", sep = ""))
  
cat(file = RVHoutFile.Residual, append = F, sep = "",
    
    "#########################################################################","\n",
    "# RESIDUAL land base for the Okanagan", "\n",
    ":FileType rvh ASCII Raven 2.8","\n",
    "# DataType         Raven HRU file","\n",
    ":Application       R","\n",
    ":WrittenBy         Lawrence Bird","\n",
    ":CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    
    ":SubBasins","\n",
    "     :Attributes, ID,   NAME, DOWNSTREAM_ID, PROFILE, REACH_LENGTH, GAUGED","\n",
    "     :Units,      none, none, none,          none,    km,           none","\n"
)

write.table(Subbasin.output, RVHoutFile.Residual, append = T, col.names = F, row.names = F, sep = ",", quote = F) 

cat(file = RVHoutFile.Residual, append = T, sep = "",
    ":EndSubBasins","\n",
    "#---------------------------------------------------------","\n",    
    "# Define all HRUs","\n",
    ":HRUs","\n",
    "     :Attributes, ID, AREA, ELEVATION, LATITUDE, LONGITUDE, BASIN_ID, LAND_USE_CLASS, VEG_CLASS, SOIL_PROFILE, AQUIFER_PROFILE, TERRAIN_CLASS, SLOPE, ASPECT","\n",
    "     :Units, none, km2,  m,  deg, deg, none, none, none, none, none, none, deg, deg","\n"
)

write.table(HRU.output.clean, RVHoutFile.Residual, append = T, col.names = F, row.names = F, sep = ",", quote = F)

cat(file = RVHoutFile.Residual, append = T, sep = "",
    ":EndHRUs", "\n",
    "#---------------------------------------------------------","\n",
    "# Define HRU Groups (by watershed)", "\n"
)    

######################################################################
##
## Generate required HRU Groups (by watershed) and append to rvh file
##

watersheds <- unique(as.character(subbasin.codes$GNIS_NAME))

for(i in 1:length(watersheds)){
  place <- which(subbasin.codes$GNIS_NAME == watersheds[i])
  
  corresponding.ID <- subbasin.codes$Subbasin_ID[place]
  
  corresponding.HRU.location <- which(HRU.output.clean[, "BASIN_ID"] %in% corresponding.ID)
  
  corresponding.HRUs.ID <- HRU.output.clean[corresponding.HRU.location, "ID"]
  
  ## Generate a sequence for splitting HRU group entries over multiple lines - 20 HRUs per line
  split <- seq(20, length(corresponding.HRUs.ID), by= 20)
  
  ## Add "\n" to each 20th HRU - this forces a line break
  corresponding.HRUs.ID[split] <- paste(corresponding.HRUs.ID[split], "\n", sep = "")
  
  cat(file = RVHoutFile.Residual, append = T, sep = "",
      paste(":HRUGroup", watersheds[i], sep = ' '), "\n",
      paste(corresponding.HRUs.ID, collapse = ","), "\n",
      ":EndHRUGroup", "\n"
  )
}

######################################################################
##
## Generate HRU Group for zero depth soils (i.e., Gravel Pit, Cut fill, Dike, Urban)
##

tomatch <- c("GRAVEL_PIT", "CUT_FILL", "DIKE", "URBAN")

matches <- unique(grep(paste(tomatch,collapse="|"), 
                        HRU.output.clean[,"SOIL_PROFILE"], value=TRUE))

zero.soils <- HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] %in% matches, "ID"]

## Generate a sequence for splitting HRU group entries over multiple lines - 20 HRUs per line
split <- seq(20, length(zero.soils), by= 20)

zero.soils[split] <- paste(zero.soils[split], "\n", sep = "")


cat(file = RVHoutFile.Residual, append = T, sep = "",
    paste(":HRUGroup", "Zero_Soils", sep = ' '), "\n",
    paste(zero.soils, collapse = ","), "\n",
    ":EndHRUGroup", "\n"
)


##################################################################################################
##################################################################################################
##
## Create a "Natural" *.rvh file
##
##################################################################################################
##################################################################################################

## Identify the subbasins that have "URBAN" land use types
urban.subbasin <- HRU.output.clean[which(HRU.output.clean[, "LAND_USE_CLASS"] == "URBAN"), "BASIN_ID"]

## Identify all unique subbasins with "URBAN" land use types
unique.urban.subbasin <- unique(urban.subbasin)

## For each subbasin with "URBAN" land use type, replace land use type, vegetation type, and soil profiles with the most common for the given subbasin
for(i in 1:length(unique.urban.subbasin)){
  
  ##-----------------------------------------
  ##
  ## Replace Land Use Class
  ##
  ##-----------------------------------------
  
  ## Identify all HRUs that are within the given subbasin
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.urban.subbasin[i], c("AREA", "LAND_USE_CLASS", "VEG_CLASS", "SOIL_PROFILE")]
  
  ## Convert to a dataframe
  sub.df <- as.data.frame(sub)

  ## Calculate the total area covered by each land use class
  LUC.proportion <- ddply(sub.df, .(LAND_USE_CLASS), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the most common land use class (by area) that is NOT "URBAN"
  common.LUC <- LUC.proportion[which(LUC.proportion$total == max(LUC.proportion[LUC.proportion$LAND_USE_CLASS != "URBAN", "total"])), "LAND_USE_CLASS"]
  
    ## Replace all "URBAN" Land use types with the most common other land use class
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.urban.subbasin[i] & HRU.output.clean[, "LAND_USE_CLASS"] == "URBAN"), "LAND_USE_CLASS"] <- as.character(common.LUC)
  
  
  ##-----------------------------------------
  ##
  ## Replace Vegetation Class
  ##
  ##-----------------------------------------
  
  ## #B1: 11052020 - Update to prevent non-logical land use/vegetation combinations.
  ## determine most common vegatation class WITHIN the most common Land Use Class. This prevents issues with FORESTED/GRASS definitions (when forested is main LUC, but because of many different forest vegetation types, grass is most common vegetation type).
  sub.df.lc <- sub.df[sub.df$LAND_USE_CLASS == common.LUC, ]
  
  VC.proportion <- ddply(sub.df.lc, .(VEG_CLASS), summarize, total = sum(as.numeric(as.character(AREA))))
  
  common.VC <- VC.proportion[which(VC.proportion$total == max(VC.proportion[VC.proportion$VEG_CLASS != "URBAN", "total"])), "VEG_CLASS"]
  
  ## Replace all "URBAN" vegetation class with the most common other vegetation class
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.urban.subbasin[i] & HRU.output.clean[, "VEG_CLASS"] == "URBAN"), "VEG_CLASS"] <- as.character(common.VC)
  
  
  ##-----------------------------------------
  ##
  ## Replace Soil Profile
  ##
  ##-----------------------------------------
  
  SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))
  
  common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != "URBAN", "total"])), "SOIL_PROFILE"]
  
  ## Replace all "URBAN" soil profiles with the most common other soil profile
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.urban.subbasin[i] & HRU.output.clean[, "SOIL_PROFILE"] == "URBAN"), "SOIL_PROFILE"] <- as.character(common.SP)
  
  
}



##################################################################################################
##
## For those HRUs that have LAKE Soil profile, introduced, but but not WATER or LAKE Land use, replace the soil profiles
##
##################################################################################################

## Identify those that are LAKE soils, but not WATER landuse
lake.soil.subbasins <- HRU.output.clean[which(HRU.output.clean[,"SOIL_PROFILE"] == "LAKE" & HRU.output.clean[,"LAND_USE_CLASS"] != "WATER" & HRU.output.clean[,"VEG_CLASS"] != "WATER"), ]

## Remove those that are LAKE soils and LAKE land use - these are correct.
lake.soil.subbasins <- lake.soil.subbasins[which(lake.soil.subbasins[,"LAND_USE_CLASS"] != "LAKE"), "BASIN_ID"]

## Identify all unique subbasins where this occurs.
unique.lake.soil.subbasins <- unique(lake.soil.subbasins)

for(i in 1:length(unique.lake.soil.subbasins)){
  
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.lake.soil.subbasins[i], ]
  
  ## Convert to a datafram
  sub.df <- as.data.frame(sub)
  
  ## Calculate the total area covered by each land use class
  SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the most common soil profile that is NOT LAKE
  common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != "LAKE", "total"])), "SOIL_PROFILE"]
  
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.lake.soil.subbasins[i] & HRU.output.clean[, "SOIL_PROFILE"] == "LAKE" & !HRU.output.clean[, "LAND_USE_CLASS"] %in% c("LAKE", "WATER")), "SOIL_PROFILE"] <- as.character(common.SP)
  
  # subbasin.common.soil.profile <- getmode(HRU.output.clean[HRU.output.clean[, "BASIN_ID"] ==  missing.soil.profiles.basin[i], "SOIL_PROFILE"])
  
  # HRU.output.clean[HRU.output.clean[, "ID"] == missing.soil.profiles.id[i], "SOIL_PROFILE"] <- subbasin.common.soil.profile
  
}


# HRU.output.clean[HRU.output.clean[,"LAND_USE_CLASS"] == "URBAN",]

# HRU.output.clean[HRU.output.clean[,"SOIL_PROFILE"] == "LAKE" & !HRU.output.clean[,"VEG_CLASS"] %in% c("LAKE", "WATER"), ]

##-----------------------------------------
##
## Replace all Land use / vegetation within Brenda Mines subbasin with FORESTED / CONIFEROUS and most common soil profile
##
##-----------------------------------------

## Identify all HRUs that are within the gievn subbasin
BM.sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == bm.subbasin.id, c("AREA", "LAND_USE_CLASS", "VEG_CLASS", "SOIL_PROFILE")]

## Convert to a datafram
sub.df <- as.data.frame(BM.sub)

#-------------------
## Replace and vegetation
#-------------------
HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == bm.subbasin.id), "LAND_USE_CLASS"] <- "FORESTED"

HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == bm.subbasin.id), "VEG_CLASS"] <- "CONIFEROUS"

#-----------------
## Replace soil profile
#-------------------

SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))

common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != "URBAN", "total"])), "SOIL_PROFILE"]

HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == bm.subbasin.id), "SOIL_PROFILE"] <- as.character(common.SP)


##################################################################################################
##
## For all ROCK HRUs that have had a canopy introduced, revert them to NON-VEGETATED (simply to represent no canopy)
##
## For all Open Water HRUs, ensure LAKE profile is below them - there are some that get converted to Water
##
##################################################################################################

HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] == "ROCK" & HRU.output.clean[, "LAND_USE_CLASS"] != "URBAN", "LAND_USE_CLASS"] <- "NON_VEGETATED"

HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] == "ROCK" & HRU.output.clean[, "LAND_USE_CLASS"] != "URBAN", "VEG_CLASS"] <- "NON_VEGETATED"

HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] != "LAKE" & HRU.output.clean[, "LAND_USE_CLASS"] == "WATER", "SOIL_PROFILE"] <- "LAKE"


##################################################################################################
##
## #B4: 03072020 - Overwrite the slope for Lakes/reservoirs/open water to be ZERO
## - There are a few HRUs that are converted to WATER during naturalization (in Vernon Creek watershed) - convert these slopes to 0
##
##################################################################################################

HRU.output.clean[HRU.output.clean[, "LAND_USE_CLASS"] == "LAKE" | HRU.output.clean[, "LAND_USE_CLASS"] == "WATER", "SLOPE"] <- 0

##############################################################################################
##
## Write Natural RVH file
##


RVHoutFile.Natural <- file.path(global.simulation.dir, paste("Master_natural.", Sys.Date(), ".rvh", sep = ""))

cat(file=RVHoutFile.Natural, append=F, sep="",
    
    "#########################################################################","\n",
    "# NATURAL land base for the Okanagan", "\n",
    ":FileType rvh ASCII Raven 2.8","\n",
    "# DataType         Raven HRU file","\n",
    ":Application       R","\n",
    ":WrittenBy         Lawrence Bird","\n",
    ":CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    
    ":SubBasins","\n",
    "     :Attributes, ID,   NAME, DOWNSTREAM_ID, PROFILE, REACH_LENGTH, GAUGED","\n",
    "     :Units,      none, none, none,          none,    km,           none","\n"
)

write.table(Subbasin.output, RVHoutFile.Natural, append = T, col.names = F, row.names = F, sep = ",", quote = F) 

cat(file=RVHoutFile.Natural, append=T, sep="",
    ":EndSubBasins","\n",
    "#---------------------------------------------------------","\n",    
    "# Define all HRUs","\n",
    ":HRUs","\n",
    "     :Attributes, ID, AREA, ELEVATION, LATITUDE, LONGITUDE, BASIN_ID, LAND_USE_CLASS, VEG_CLASS, SOIL_PROFILE, AQUIFER_PROFILE, TERRAIN_CLASS, SLOPE, ASPECT","\n",
    "     :Units, none, km2,  m,  deg, deg, none, none, none, none, none, none, deg, deg","\n"
)

write.table(HRU.output.clean, RVHoutFile.Natural, append = T, col.names = F, row.names = F, sep = ",", quote = F)

cat(file=RVHoutFile.Natural, append=T, sep="",
    ":EndHRUs", "\n",
    "#---------------------------------------------------------","\n",
    "# Define HRU Groups (by watershed)", "\n"
)   


######################################################################
##
## Generate required HRU Groups (by watershed) and append to rvh file
##

watersheds <- unique(as.character(subbasin.codes$GNIS_NAME))

for(i in 1:length(watersheds)){
  place <- which(subbasin.codes$GNIS_NAME == watersheds[i])
  
  corresponding.ID <- subbasin.codes$Subbasin_ID[place]
  
  corresponding.HRU.location <- which(HRU.output.clean[, "BASIN_ID"] %in% corresponding.ID)
  
  corresponding.HRUs.ID <- HRU.output.clean[corresponding.HRU.location, "ID"]
  
  ## Generate a sequence for splitting HRU group entries over multiple lines - 20 HRUs per line
  split <- seq(20, length(corresponding.HRUs.ID), by= 20)
  
  ## Add "\n" to each 20th HRU - this forces a line break
  corresponding.HRUs.ID[split] <- paste(corresponding.HRUs.ID[split], "\n", sep = "")
  
  cat(file = RVHoutFile.Natural, append = T, sep = "",
      paste(":HRUGroup", watersheds[i], sep = ' '), "\n",
      paste(corresponding.HRUs.ID, collapse = ","), "\n",
      ":EndHRUGroup", "\n"
  )
}


######################################################################
##
## Generate HRU Group for zero depth soils (i.e., Gravel Pit, Cut fill, Dike, Urban)
##

tomatch <- c("GRAVEL_PIT", "CUT_FILL", "DIKE", "URBAN")

matches <- unique (grep(paste(tomatch,collapse="|"), 
                        HRU.output.clean[,"SOIL_PROFILE"], value=TRUE))

zero.soils <- HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] %in% matches, "ID"]

## Generate a sequence for splitting HRU group entries over multiple lines - 20 HRUs per line
split <- seq(20, length(zero.soils), by= 20)

zero.soils[split] <- paste(zero.soils[split], "\n", sep = "")


cat(file = RVHoutFile.Natural, append = T, sep = "",
    paste(":HRUGroup", "Zero_Soils", sep = ' '), "\n",
    paste(zero.soils, collapse = ","), "\n",
    ":EndHRUGroup", "\n"
)




print("ALL DONE!")
