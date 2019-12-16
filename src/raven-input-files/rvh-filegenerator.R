############################################################################################################################
##
## This script takes output from "hru-generator.R" and generates the required *.rvh file for input into Raven.
##
## Feb-28-2019
##
############################################################################################################################

## Load Required Packages
require(raster)
require(plyr)

## Generate required functions
source("/var/obwb-hydro-modelling/src/functions.R")

## Specify the subbasin ID for Brenda Mines - this is used to replace landuse/vegetation cover under natural conditions
bm.subbasin.id <- 2709


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
# HRU.output.clean[which(is.na(HRU.output.clean[,"SOIL_PROFILE"])), "SOIL_PROFILE"] <- as.character(soil.codes$soil_type[soil.codes$Value == getmode(HRU.table$soils)])

## Replace missing soil profiles (i.e., soil profile = " ") with the most common within the corresponding subbasin
# missing.soil.profiles.id <- HRU.output.clean[which(HRU.output.clean[, "SOIL_PROFILE"] == " "), "ID"]

missing.soil.profiles.basin <- HRU.output.clean[which(HRU.output.clean[, "SOIL_PROFILE"] == " "), "BASIN_ID"]

unique.missing.soil.profiles.basin <- unique(missing.soil.profiles.basin)

for(i in 1:length(unique.missing.soil.profiles.basin)){
  
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.missing.soil.profiles.basin[i], c("AREA", "SOIL_PROFILE")]
  
  ## Convert to a datafram
  sub.df <- as.data.frame(sub)
  
  ## Calculate the total area covered by each land use class
  SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the most common land use calss that is NOT "NON-VEGETATED"
  common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != " ", "total"])), "SOIL_PROFILE"]
  
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.missing.soil.profiles.basin[i] & HRU.output.clean[, "SOIL_PROFILE"] == " "), "SOIL_PROFILE"] <- as.character(common.SP)
  
  # subbasin.common.soil.profile <- getmode(HRU.output.clean[HRU.output.clean[, "BASIN_ID"] ==  missing.soil.profiles.basin[i], "SOIL_PROFILE"])
  
  # HRU.output.clean[HRU.output.clean[, "ID"] == missing.soil.profiles.id[i], "SOIL_PROFILE"] <- subbasin.common.soil.profile
  
}


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


##################################################################################################
##
## For HRUs with ROCK SOIL_PROFILE, replace LAND_USE_CLASS and VEG_CLASS with NON_VEGETATED (simply to represent no canopy)
##
##################################################################################################

HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] == "ROCK" & HRU.output.clean[, "LAND_USE_CLASS"] != "URBAN", "LAND_USE_CLASS"] <- "NON_VEGETATED"

HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] == "ROCK" & HRU.output.clean[, "LAND_USE_CLASS"] != "URBAN", "VEG_CLASS"] <- "NON_VEGETATED"


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
RVHoutFile.Residual <- file.path("/var/obwb-hydro-modelling/simulations/Master_residual.rvh")

cat(file=RVHoutFile.Residual, append=F, sep="",
    
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

cat(file=RVHoutFile.Residual, append=T, sep="",
    ":EndSubBasins","\n",
    "#---------------------------------------------------------","\n",    
    "# Define all HRUs","\n",
    ":HRUs","\n",
    "     :Attributes, ID, AREA, ELEVATION, LATITUDE, LONGITUDE, BASIN_ID, LAND_USE_CLASS, VEG_CLASS, SOIL_PROFILE, AQUIFER_PROFILE, TERRAIN_CLASS, SLOPE, ASPECT","\n",
    "     :Units, none, km2,  m,  deg, deg, none, none, none, none, none, none, deg, deg","\n"
)

write.table(HRU.output.clean, RVHoutFile.Residual, append = T, col.names = F, row.names = F, sep = ",", quote = F)

cat(file=RVHoutFile.Residual, append=T, sep="",
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

matches <- unique (grep(paste(tomatch,collapse="|"), 
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
non.veg.subbasin <- HRU.output.clean[which(HRU.output.clean[, "LAND_USE_CLASS"] == "URBAN"), "BASIN_ID"]

## Identify all unique subbasins with "NON-VEGETATED" land use types
unique.non.veg.subbasin <- unique(non.veg.subbasin)

## For each subbasin with "NON-VEGETATED" land use type, replace land use type, vegetation type, and soil profiles with the most common for the given subbasin
for(i in 1:length(unique.non.veg.subbasin)){
  
  ##-----------------------------------------
  ##
  ## Replace Land Use Class
  ##
  ##-----------------------------------------
  
  ## Identify all HRUs that are within the gievn subbasin
  sub <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.non.veg.subbasin[i], c("AREA", "LAND_USE_CLASS", "VEG_CLASS", "SOIL_PROFILE")]
  
  ## Convert to a datafram
  sub.df <- as.data.frame(sub)

  ## Calculate the total area covered by each land use class
  LUC.proportion <- ddply(sub.df, .(LAND_USE_CLASS), summarize, total = sum(as.numeric(as.character(AREA))))
  
  ## Identify the most common land use calss that is NOT "URBAN"
  common.LUC <- LUC.proportion[which(LUC.proportion$total == max(LUC.proportion[LUC.proportion$LAND_USE_CLASS != "URBAN", "total"])), "LAND_USE_CLASS"]
  
  # original <- HRU.output.clean[HRU.output.clean[, "BASIN_ID"] == unique.non.veg.subbasin[i], ]
  
  ## Replace all "URBAN" Land use types with the most common other land use class
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.non.veg.subbasin[i] & HRU.output.clean[, "LAND_USE_CLASS"] == "URBAN"), "LAND_USE_CLASS"] <- as.character(common.LUC)
  
  
  ##-----------------------------------------
  ##
  ## Replace Vegetation Class
  ##
  ##-----------------------------------------
  
  VC.proportion <- ddply(sub.df, .(VEG_CLASS), summarize, total = sum(as.numeric(as.character(AREA))))
  
  common.VC <- VC.proportion[which(VC.proportion$total == max(VC.proportion[VC.proportion$VEG_CLASS != "URBAN", "total"])), "VEG_CLASS"]
  
  ## Replace all "URBAN" vegetation class with the most common other vegetation class
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.non.veg.subbasin[i] & HRU.output.clean[, "VEG_CLASS"] == "URBAN"), "VEG_CLASS"] <- as.character(common.VC)
  
  
  ##-----------------------------------------
  ##
  ## Replace Soil Profile
  ##
  ##-----------------------------------------
  
  SP.proportion <- ddply(sub.df, .(SOIL_PROFILE), summarize, total = sum(as.numeric(as.character(AREA))))
  
  common.SP <- SP.proportion[which(SP.proportion$total == max(SP.proportion[SP.proportion$SOIL_PROFILE != "URBAN", "total"])), "SOIL_PROFILE"]
  
  ## Replace all "URBAN" soil profiles with the most common other soil profile
  HRU.output.clean[which(HRU.output.clean[, "BASIN_ID"] == unique.non.veg.subbasin[i] & HRU.output.clean[, "SOIL_PROFILE"] == "URBAN"), "SOIL_PROFILE"] <- as.character(common.SP)
  
  
}


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
##################################################################################################

HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] == "ROCK" & HRU.output.clean[, "LAND_USE_CLASS"] != "URBAN", "LAND_USE_CLASS"] <- "NON_VEGETATED"

HRU.output.clean[HRU.output.clean[, "SOIL_PROFILE"] == "ROCK" & HRU.output.clean[, "LAND_USE_CLASS"] != "URBAN", "VEG_CLASS"] <- "NON_VEGETATED"


##############################################################################################
##
## Write Natural RVH file
##


RVHoutFile.Natural <- file.path("/var/obwb-hydro-modelling/simulations/Master_natural.rvh")

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
