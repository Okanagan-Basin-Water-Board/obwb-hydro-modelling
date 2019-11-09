############################################################################################################################
##
## This script takes output from "hru-generator.R" and generates the required *.rvh file for input into Raven.
##
## Feb-28-2019
##
############################################################################################################################

## Load Required Packages
require(raster)

## Generate required functions
source("/var/obwb-hydro-modelling/src/functions.R")

#################################################3
##
## Read-in required datasets and variables
##
#################################################3
print("loading okanagan_hru.RData")

load("/var/obwb-hydro-modelling/input-data/processed/spatial/okanagan_hru.RData")

print("all loaded")

HRU.table <- as.data.frame(DT)

rm(DT, DT.revert)

#################################################3
##
## Add a "vegetation" column to allow breakdown of vegetation types. This will be determined from "value" of landcover dataset
##
#################################################3

# currently just duplicating the "landcover" column. This is an unnecessary step, but is nice for cosistency/visual interpretation
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
  
  HRU.output[i,"AREA"] <- round((length(index) * 19.80255838 * 19.80255838) / (1000*1000), 4)
  
  HRU.output[i, "ELEVATION"] <- round(median(HRU.table[index,"elevation"]), 2)
  
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
###########################################################################
##
## Assign names to soil profiles, aquifer profiles, and land use classes
##
## Note: This requires attribute tables for soil, aquifer, and lancover raster to be ingested as csv files
##
###########################################################################

# soil.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/soil_profile_codes.csv")
soil.codes <- read.csv("/var/obwb-hydro-modelling/input-data/processed/spatial/soils/soil_attributes.csv",
                       col.names = c("OID", "Value", "Count", "soil_type"))

aquifer.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/aquifer_codes.csv")

landcover.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/landcover_codes.csv")

vegetation.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/vegetation_codes.csv")

subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

for(i in 1:nrow(HRU.output)){

  # HRU.output[i, "SOIL_PROFILE"] <- ifelse(is.na(HRU.output[i, "SOIL_PROFILE"]), "[NONE]", as.character(soil.codes$PM1_1)[HRU.output[i,"SOIL_PROFILE"] == soil.codes$Value])
  HRU.output[i, "SOIL_PROFILE"] <- ifelse(is.na(HRU.output[i, "SOIL_PROFILE"]), "[NONE]", as.character(soil.codes$soil_type)[HRU.output[i,"SOIL_PROFILE"] == soil.codes$Value])
  
  HRU.output[i, "AQUIFER_PROFILE"] <- ifelse(is.na(HRU.output[i, "AQUIFER_PROFILE"]), "[NONE]", as.character(aquifer.codes$Aquifer_ty)[HRU.output[i,"AQUIFER_PROFILE"] == aquifer.codes$Value])
  
  HRU.output[i, "LAND_USE_CLASS"] <- ifelse(is.na(HRU.output[i, "LAND_USE_CLASS"]), "[NONE]", as.character(landcover.codes$Bin_type)[HRU.output[i, "LAND_USE_CLASS"] == landcover.codes$Bin_Value])
  
  ## note that vegetation type is determined based on the "Value", rather than the "Bin_Value". Bin value refers to the landcover bin, rather than the specific vegetation bin.
  HRU.output[i, "VEG_CLASS"] <- ifelse(is.na(HRU.output[i, "VEG_CLASS"]), "[NONE]", as.character(vegetation.codes$Bin_type)[HRU.output[i, "VEG_CLASS"] == vegetation.codes$Value])
  
  print(i)

}

# ## Remove HRUs with zero area.
HRU.output.clean <- HRU.output[!as.numeric(HRU.output[,"AREA"]) <= 0,]


if(nrow(HRU.output) != nrow(HRU.output.clean)){
  stop(print("Some HRUs were removed due to zero area. Review HRU generation."))
}



## Replace "NA" soil profiles with most common soil profile
HRU.output.clean[which(is.na(HRU.output.clean[,"SOIL_PROFILE"])), "SOIL_PROFILE"] <- as.character(soil.codes$soil_type[soil.codes$Value == getmode(HRU.table$soils)])

## Replace soil profiles underneath "WATER" Landuse and "WATER" Vegetation HRUs with LAKE profiles - this turns off all soil processed under open water HRUs.
HRU.output.clean[which(HRU.output.clean[,"LAND_USE_CLASS"] == "WATER" & HRU.output.clean[,"VEG_CLASS"] == "WATER"), "SOIL_PROFILE"] <- "LAKE"


# ## re-order the table so that all HRUs for each subbasin are next to each other
# HRU.output <- HRU.output[order(HRU.output[,"BASIN_ID"]),]
# 
# ## re-assign IDs to HRUs to that all sub-basin HRUs are contiguous
# HRU.output[,"ID"] <- 1:nrow(HRU.output)

##################################################################################################
##
## Replace SOIL_PROFILE and VEG_CLASS with LAKE for HRUs which are Lakes / Reservoirs
##
##################################################################################################
reservoirs <- as.character(subbasin.codes[subbasin.codes$Reservoir_name != "<Null>", "Subbasin_ID"])

## Assign ID of 999 to all rows which are within the reservoir / lake subbasins
HRU.output.clean[HRU.output.clean[, "BASIN_ID"] %in% reservoirs, "VEG_CLASS"] <- "LAKE"

HRU.output.clean[HRU.output.clean[, "BASIN_ID"] %in% reservoirs, "SOIL_PROFILE"] <- "LAKE"


## Write HRU Table to csv incase adjustments are needed
save.image(file = "/var/obwb-hydro-modelling/input-data/processed/spatial/Raven-HRU-table.RData")

# write.csv(HRU.output.clean, "/var/obwb-hydro-modelling/input-data/processed/spatial/HRU-table.csv")
##################################################################################################
##
## Generate required Subbasin output table (in format required by RAVEN)
##
##################################################################################################

## Read in subbasin attribute table from Dan - **ensure the attribute table matches the version of WS_RasterX.tif raster being used**


subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

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

## Assign zero reach length to those subbasins which are reservoirs
Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Reservoir_name != "<Null>"], 5] <- 0

## Assign all subbasins a 0 so that they are ungauged. Following lines overwrite this behaviour for subbasins that should be gauged
Subbasin.output[, 6] <- 0

## Flag all subbasins which have a WSC station associated with them to be gauged (1). All other subbasins are ungauged (0)
Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Hydrometric_stn != "<Null>"], 6] <- 1

## Flag all subbasins which are a reservoir. All other subbasins are ungauged (0)
Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Reservoir_name != "<Null>"], 6] <- 1


## Flag all subbasin are either: The Apex of the Fan (Reports_to_Fan == "A") OR are the mouth of the creek (Downstream_ID == -1)
Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Reports_to_Fan == "A"], 6] <- 1

Subbasin.output[Subbasin.output[,1] %in% subbasin.codes$Subbasin_ID[subbasin.codes$Downstream_ID == -1], 6] <- 1


###########################################################################
##
## Generate *.rvh file
##
###########################################################################

# RVHoutFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = ""))
RVHoutFile <- file.path("/var/obwb-hydro-modelling/simulations/Master.rvh")

cat(file=RVHoutFile, append=F, sep="",
    
    "#########################################################################","\n",
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

write.table(Subbasin.output, RVHoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F) 

cat(file=RVHoutFile, append=T, sep="",
    ":EndSubBasins","\n",
    "#---------------------------------------------------------","\n",    
    "# Define all HRUs","\n",
    ":HRUs","\n",
    "     :Attributes, ID, AREA, ELEVATION, LATITUDE, LONGITUDE, BASIN_ID, LAND_USE_CLASS, VEG_CLASS, SOIL_PROFILE, AQUIFER_PROFILE, TERRAIN_CLASS, SLOPE, ASPECT","\n",
    "     :Units, none, km2,  m,  deg, deg, none, none, none, none, none, none, deg, deg","\n"
)

write.table(HRU.output.clean, RVHoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)

cat(file=RVHoutFile, append=T, sep="",
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
  
  cat(file = RVHoutFile, append = T, sep = "",
      paste(":HRUGroup", watersheds[i], sep = ' '), "\n",
      paste(corresponding.HRUs.ID, collapse = ","), "\n",
      ":EndHRUGroup", "\n"
  )
}

##################################################################################################
##
## Copy rvh file to GCP bucket
##
##################################################################################################

print("ALL DONE!")

# require(cloudml)
# gs_copy("/home/lawrence/var/Data/Processed/test.rvh", "gs://associated-environmental/hru-generation/processed")
