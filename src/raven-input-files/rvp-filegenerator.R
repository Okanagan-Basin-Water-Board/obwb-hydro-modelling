############################################################################################################################
##
## This script generates the required *.rvp file for input into Raven.
##
## Feb-28-2019
##
############################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

## Read in the same XXX_codes.csv files used to develop the RVH file. **at the moment, vegetation codes is not used in the 
## RVH file generation as LAND_USE_CLASS AND VEG_CLASS are the same for now. However, it's built in here to allow these
## to be differentiated in future iterations of this code.

RVP.template.base <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, RVP.template.in.file), na.strings = c(""))
  
# soil.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/soil_profile_codes.csv")

soil.profiles <- read.csv(file.path(global.input.dir, processed.spatial.dir, soil.profile.table.in.file))
  
soil.classes <- read.csv(file.path(global.input.dir, processed.spatial.dir, soil.class.table.in.file))
  
landcover.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, LC.in.file))
  
vegetation.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, Veg.in.file))
  
annual.runoff <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, AR.in.file))
  
seasonal.LAI <- read.csv(file.path(global.input.dir, processed.spatial.dir, seasonal.lai.processed.file))
  
max.LAI <- read.csv(file.path(global.input.dir, processed.spatial.dir, max.lai.processed.file))
  

############################################################################################################################
##
## Generate required tables (in format required by RAVEN), specifically:
## - Soil Profiles Table
## - Vegetation Classes Table
## - Land Use Classes Table
##
############################################################################################################################

##-------------------------------------------------------
##
## Vegetation Classes Table
##
##-------------------------------------------------------

# - identify unique vegetation types (i.e., unique rows) and exclude columns 1, 2, and 4 (i.e., value, Cover_type, and Bin_Value)
vegetation.classes <- unique(vegetation.codes[,-which(names(vegetation.codes) %in% c("Value","Cover_type", "Bin_Value"))])

## Replace MAX_LAI with values from max.LAI dataframe
vegetation.classes$MAX_LAI <- max.LAI[match(vegetation.classes$Bin_type, max.LAI$Bin_type), "MAX_LAI"]

## Replace missing MAX_LAI values with 0
vegetation.classes[is.na(vegetation.classes$MAX_LAI), "MAX_LAI"] <- 0

# seasonal.LAI <- vegetation.classes[,which(names(vegetation.classes) %in% c("Bin_type", paste("LAI", month.abb, sep = "_")))]

seasonal.HT <- vegetation.classes[,which(names(vegetation.classes) %in% c("Bin_type", paste("HT", month.abb, sep = "_")))]

if(ncol(as.data.frame(seasonal.HT)) > 1){
  vegetation.classes <- vegetation.classes[,-(which(names(vegetation.classes) %in% c(paste("LAI", month.abb, sep = "_"), paste("HT", month.abb, sep = "_"))))]
}

##-------------------------------------------------------
##
## Grouped Calibration Parameters
##
## - Replace "GROUP_X" name with the value specified for the group for each given parameter / group combination
##
##-------------------------------------------------------

## Make a duplicate of the base RVP template to allow it to be recalled when making Ostrich template file
RVP.template <- RVP.template.base

## Make all columns chracter vectors
RVP.template[,] <- lapply(RVP.template.base[, ], as.character)

## Subset all grouped calibration parameters
calibration.specials <- RVP.template[RVP.template$GROUP == "CalibrationGroups", ]


## If grouped calibration parameters are presernt, replace the value for each individual parameter with the values specified for the group.
if(nrow(calibration.specials) > 0){
  
  for(i in 1:nrow(calibration.specials)){
    
    special_parameter <- calibration.specials[i,]
    
    RVP.template[which(RVP.template$PARAMETER == special_parameter$PARAMETER & RVP.template$VALUE == special_parameter$DEFINITION), "VALUE"] <- special_parameter$VALUE
    
  }
  
  ## Delete the rows that house the CalibrationGroup definitions
  RVP.template <- RVP.template[!RVP.template$GROUP == "CalibrationGroups",]
  
}

##-------------------------------------------------------
##
## Vegetation Parameters
##
##-------------------------------------------------------

# Isolate all vegetation parameters
vegetation.parameters <- RVP.template[RVP.template$GROUP == "VegetationParameter", ]

## identifty vegetation parameter names - ordered by the most common (this negates the need for all paramaters to be defined for all vegetaion types)
vegetation.parameter.names <- names(sort(summary(as.factor(vegetation.parameters$PARAMETER)), decreasing = T))

## identify all vegetation types
layers <- unique(vegetation.parameters$DEFINITION)

## create an empty vector to house units for all vegetation parameters
vegetation.units <- rep(NA, length(vegetation.parameter.names))

## create empty matrix to house vegetation parameters in the format requierd by Raven
vegetation.parameter.table <- matrix(nrow = length(layers), ncol = length(vegetation.parameter.names) + 1, NA)


## loop over all vegetation parameters to populate the vegetation.parameter.table
for(i in 1:length(vegetation.parameter.names)){
  
  para <- vegetation.parameters[vegetation.parameters$PARAMETER == vegetation.parameter.names[i],]
  
  extract <- para[, c("DEFINITION", "VALUE")]
  
  if(i == 1){
    ## if i == 1, include the "DEFINITION" column as vegetation types. if i !=1, only include the "VALUE" column
    vegetation.parameter.table[,1:2] <- as.matrix(extract)
    
  } else {
    ## Concatenate the extracted value(s) and "_DEFAULT" to fill any missing specified parameter values. This allows not all paramaters to have to be specified for all vegetation classes.
    vegetation.parameter.table[,i+1] <- c(as.matrix(extract[,"VALUE"]), rep("_DEFAULT", length(layers) - nrow(extract)))
  }
  
  vegetation.units[i] <- unique(para[,"UNITS"])
  
}

##-------------------------------------------------------
##
## Landuse Parameters
##
##-------------------------------------------------------

## Land Use Classes Table:
# - identify unique landuse types (i.e., unique rows) and exclude columns 1, 2, and 4 (i.e., value, Cover_type, and Bin_Value)
landuse.classes <- unique(landcover.codes[,c(3, 5:ncol(landcover.codes))])

## isolate all landuse parameters 
landuse.parameters <- RVP.template[RVP.template$GROUP == "LandUseParameter", ]

## identifty landuse parameter names - ordered by the most common (this negates the need for all paramaters to be defined for all landuse types)
landuse.parameter.names <- names(sort(summary(as.factor(landuse.parameters$PARAMETER)), decreasing = T))

## identify all landuse types
layers <- unique(landuse.parameters$DEFINITION)

## create an empty vector to house units for all landuse parameters
landuse.units <- rep(NA, length(landuse.parameter.names))

## create empty matrix to house landuse parameters in the format requierd by Raven
landuse.parameter.table <- matrix(nrow = length(layers), ncol = length(landuse.parameter.names) + 1, NA)


## loop over all landuse parameters to populate the landuse.parameter.table
for(i in 1:length(landuse.parameter.names)){
  
  para <- landuse.parameters[landuse.parameters$PARAMETER == landuse.parameter.names[i],]
  
  extract <- para[, c("DEFINITION", "VALUE")]
  
  if(i == 1){
    ## if i == 1, include the "DEFINITION" column as landuse types. if i !=1, only include the "VALUE" column
    landuse.parameter.table[,1:2] <- as.matrix(extract)
    
  } else { 
    ## Concatenate the extracted value(s) and "_DEFAULT" to fill any missing specified parameter values. This allows not all paramaters to have to be specified for all landuse classes.
    landuse.parameter.table[,i+1] <- c(as.matrix(extract[,"VALUE"]), rep("_DEFAULT", length(layers) - nrow(extract)))
  }
  
  landuse.units[i] <- unique(para[,"UNITS"])
  
}

##-------------------------------------------------------
##
## Soil Parameters
##
##-------------------------------------------------------

## isolate all soil parameters
soil.parameters <- RVP.template[RVP.template$GROUP == "SoilParameter", ]

## identify unique soil parameter names, and order them based on their occurence. This means that if one soil parameter is specified for all soil classes, this will be written first and therefore populate all rows correctly.
soil.parameter.names <- names(sort(summary(as.factor(soil.parameters$PARAMETER)), decreasing = T))

## identify all soil classes
layers <- unique(soil.parameters$DEFINITION)

## create an empty vector to house units for all soil parameters
soil.units <- rep(NA, length(soil.parameter.names))

## create empty matrix to house soil parameters in the format requierd by Raven
soil.parameter.table <- matrix(nrow = length(layers), ncol = length(soil.parameter.names) + 1, NA)

## loop over all soil parameters to populate the soil.parameter.table
for(i in 1:length(soil.parameter.names)){
  
  para <- soil.parameters[soil.parameters$PARAMETER == soil.parameter.names[i],]
  
  extract <- para[, c("DEFINITION", "VALUE")]
  
  if(i == 1){
    ## if i == 1, include the "DEFINITION" column as soil types. if i !=1, only include the "VALUE" column
    soil.parameter.table[,1:2] <- as.matrix(extract)
    
  } else { 
    # Assign value for next soil parameter. If not all soil classes are specified, _DEFAULT is tagged on to remaining classes.
    soil.parameter.table[,i+1] <- c(as.matrix(extract[,"VALUE"]), rep("_DEFAULT", length(layers) - nrow(extract)))
  }
  
  soil.units[i] <- unique(para[,"UNITS"])
  
}

##-------------------------------------------------------
##
## Global Parameters
##
##-------------------------------------------------------

## isolate all global parameters
global <- RVP.template[RVP.template$GROUP == "GlobalParameter", c("PARAMETER", "DEFINITION", "VALUE")]

## paste a ":" to the front of the parameters to ensure that Raven recognizes them as commands
global$PARAMETER <- paste(":", global$PARAMETER, sep = '')


## add annual runoff value to global table from annual.runoff master list
## If multiple watersheds included, the mean annual runoff from all is included
avg.annual.runoff <- annual.runoff[annual.runoff$WATERSHED %in% include.watersheds, "AVG_ANNUAL_RUNOFF_APEX_FAN"]

## take the mean of the avg_annual_runoff for all watersheds included in the current run.
global[global$DEFINITION == "AVG_ANNUAL_RUNOFF", "VALUE"] <- as.character(mean(avg.annual.runoff))

##-------------------------------------------------------
##
## Channel Parameters
##
##-------------------------------------------------------

## isolate all channel parameters
channel <- RVP.template[RVP.template$GROUP == "ChannelParameter", ]

## isolate survey points for channel profile
surveypoints <- channel[channel$PARAMETER == "SurveyPoints", c("DEFINITION", "VALUE")]

############################################################################################################################
##
## Generate Raven *.rvp file
##
############################################################################################################################

RVPoutFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvp", sep = ""))


cat(file = RVPoutFile, append = F, sep = "",
    
    "#########################################################################","\n",
    ":FileType rvp Raven 2.8","\n",
    "# DataType         Raven RVP file","\n",
    ":Application       R","\n",
    ":WrittenBy         Lawrence Bird","\n",
    ":CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Soil Classes --------------------------------------", "\n",
    
    ":SoilClasses","\n",
    "     :Attributes,", "%SAND, %CLAY, %SILT, %ORGANIC", "\n",
    "     :Units,", "none, none, none, none", "\n"
)

write.table(soil.classes, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)

cat(file = RVPoutFile, append = T, sep = "",
    ":EndSoilClasses", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Land Use Classes ----------------------------------", "\n",
    ":LandUseClasses", "\n",
    "     :Attributes,  IMPERMEABLE_FRAC, FOREST_COVERAGE", "\n",
    "     :Units, fract,  fract", "\n"
)

write.table(landuse.classes, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)

cat(file = RVPoutFile, append = T, sep = "",
    ":EndLandUseClasses", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Vegetation Classes ----------------------------------", "\n",
    ":VegetationClasses", "\n",
    "     :Attributes,  MAX_HT, MAX_LAI,  MAX_LEAF_COND", "\n",
    "     :Units, m,  none, mm_per_s", "\n"
)

write.table(vegetation.classes, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)

cat(file = RVPoutFile, append = T, sep = "",
    ":EndVegetationClasses", "\n")

if(ncol(as.data.frame(seasonal.LAI)) > 1){
  cat(file = RVPoutFile, append = T, sep = "",
      "#------Seaonal Canopy LAI Changes-------------------", "\n",
      ":SeasonalCanopyLAI","\n"
  )
  
  write.table(seasonal.LAI, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
  
  cat(file = RVPoutFile, append = T, sep = "",
      ":EndSeasonalCanopyLAI", "\n",
      "\n")
}

if(ncol(as.data.frame(seasonal.HT)) > 1){
  cat(file = RVPoutFile, append = T, sep = "",
      "#------Seaonal Canopy Height Changes-------------------", "\n",
      ":SeasonalCanopyHeight","\n"
  )
  
  write.table(seasonal.HT, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
  
  cat(file = RVPoutFile, append = T, sep = "",
      ":EndSeasonalCanopyHeight", "\n",
      "\n")
}


cat(file = RVPoutFile, append = T, sep = "",
    "#---------------------------------------------------------", "\n",
    "# ---- Soil Profiles -------------------------------------", "\n",
    ":SoilProfiles", "\n",
    "# Name, # Horizons, Horizon 1, Thickness_1, Horizon 2, Thickness_2, ..., Horizon X, Thickness_X", "\n"
)

write.table(soil.profiles, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")

cat(file = RVPoutFile, append = T, sep = "",
    ":EndSoilProfiles", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Global Parameters ---------------------------------", "\n"
)

write.table(global, RVPoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)


cat(file = RVPoutFile, append = T, sep = "",
    "#---------------------------------------------------------", "\n",
    "# ---- Land Use Parameters -------------------------------", "\n",
    ":LandUseParameterList", "\n",
    ":Parameters, ", paste(landuse.parameter.names, collapse = ", "), "\n",
    ":Units, ", paste(landuse.units, collapse = ","), "\n"
)

write.table(landuse.parameter.table, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")

cat(file = RVPoutFile, append = T, sep = "",
    ":EndLandUseParameterList", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Vegetation Parameters -----------------------------------", "\n",
    ":VegetationParameterList", "\n",
    ":Parameters, ", paste(vegetation.parameter.names, collapse = ", "), "\n",
    ":Units, ", paste(vegetation.units, collapse = ", "), "\n"
)

write.table(vegetation.parameter.table, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")

cat(file = RVPoutFile, append = T, sep = "",
    ":EndVegetationParameterList", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Soil Parameters -----------------------------------", "\n",
    ":SoilParameterList", "\n",
    ":Parameters, ", paste(soil.parameter.names, collapse = ", "), "\n",
    ":Units, ", paste(soil.units, collapse = ", "), "\n"
)

write.table(soil.parameter.table, RVPoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")


cat(file = RVPoutFile, append = T, sep = "",    
    ":EndSoilParameterList", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Channel Profiles ----------------------------------", "\n",
    ":ChannelProfile  ", paste(channel[channel$PARAMETER == "ChannelProfile", "VALUE"]), "\n",
    ":Bedslope  ", paste(channel[channel$PARAMETER == "Bedslope", "VALUE"]), "\n",
    ":SurveyPoints", "\n"
)

write.table(surveypoints, RVPoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)


cat(file = RVPoutFile, append = T, sep = "",
    ":EndSurveyPoints", "\n",
    "\n",
    ":RoughnessZones", "\n",
    paste(channel[channel$PARAMETER == "RoughnessZones", "DEFINITION"]), "\t",
    paste(channel[channel$PARAMETER == "RoughnessZones", "VALUE"]), "\n",
    ":EndRoughnessZones", "\n",
    ":EndChannelProfile", "\n"
)

############################################################################################################################
##
## Generate Ostrich Template file for same conditions
##
############################################################################################################################

if(run.ostrich == TRUE){
  
  ## Reassign the base RVP.template to ensure that all GROUP labels are captured.
  RVP.template <- RVP.template.base
  
  ## Make all columns characters
  RVP.template[,] <- lapply(RVP.template.base[, ], as.character)
  
  ## Subset all grouped calibration parameters
  calibration.specials <- RVP.template[RVP.template$GROUP == "CalibrationGroups", ]
  
  
  ## If the special parameter (i.e., calibration group) does not have CAL_MIN / CAL_MAX specified, replace those values across the board to eliminate all from calibration
  if(nrow(calibration.specials) > 0){
    
    for(i in 1:nrow(calibration.specials)){
      
      special_parameter <- calibration.specials[i,]
      
      if(is.na(special_parameter$CAL_MIN)){
      
        RVP.template[which(RVP.template$PARAMETER == special_parameter$PARAMETER & RVP.template$VALUE == special_parameter$DEFINITION), "CAL_MIN"] <- special_parameter$CAL_MIN
        
        RVP.template[which(RVP.template$PARAMETER == special_parameter$PARAMETER & RVP.template$VALUE == special_parameter$DEFINITION), "CAL_MAX"] <- special_parameter$CAL_MAX
        
        RVP.template[which(RVP.template$PARAMETER == special_parameter$PARAMETER & RVP.template$VALUE == special_parameter$DEFINITION), "VALUE"] <- special_parameter$VALUE
      
      }
      
    }
    
    ## Delete the rows that house the CalibrationGroup definitions
    # RVP.template <- RVP.template[!RVP.template$GROUP == "CalibrationGroups",]
    
  }
  
  ## Add empty column to house the calibration variable
  RVP.template$CAL_VAR <- NA
  
  ## Those paramaters which are not included in calibration should have their value assigned to the CAL_VAR column
  RVP.template[is.na(RVP.template$CAL_MIN), "CAL_VAR"] <- RVP.template$VALUE[is.na(RVP.template$CAL_MIN)]
  
  ## Delete rows that are Calibrationgroups AND CAL_MIN is na
  RVP.template <- RVP.template[!(RVP.template$GROUP == "CalibrationGroups" & is.na(RVP.template$CAL_MIN)),]
  
  ##-------------------------------------------------------
  ##
  ## Soil Parameters
  ##
  ##-------------------------------------------------------
  
  ## Test if soil.parameters has any values in CAL_MAX then it should be included for calibration.
  ## If NOT all values are NA, generate soils.parameters.calibrate table and populate.
  ## If all values ARE NA, 
  if(!all(is.na(soil.parameters$CAL_MAX))){
    
    soil.calibrate <- TRUE
    
    
    soil.parameters.calibrate <- RVP.template[RVP.template$GROUP == "SoilParameter" | RVP.template$GROUP == "CalibrationGroups", ]
    
    ## convert all columns to character
    # soil.parameters.calibrate[,] <- lapply(soil.parameters.calibrate[, ], as.character)
    
    ## Create an empty column called CAL_VAR
    # soil.parameters.calibrate$CAL_VAR <- NA
    
    # soil.parameters.calibrate$CAL_VAR[which(!is.na(soil.parameters.calibrate$CAL_MAX))] <- paste("par", soil.parameters.calibrate$PARAMETER[which(!is.na(soil.parameters.calibrate$CAL_MAX))], soil.parameters.calibrate$DEFINITION[which(!is.na(soil.parameters.calibrate$CAL_MAX))], sep = "_")
    soil.parameters.calibrate$CAL_VAR[which(is.na(soil.parameters.calibrate$CAL_VAR))] <- paste(soil.parameters.calibrate$DEFINITION[which(is.na(soil.parameters.calibrate$CAL_VAR))], soil.parameters.calibrate$PARAMETER[which(is.na(soil.parameters.calibrate$CAL_VAR))], sep = "_")
    
    # soil.parameters.calibrate$CAL_VAR[which(is.na(soil.parameters.calibrate$CAL_MAX))] <- as.character(soil.parameters.calibrate$VALUE[which(is.na(soil.parameters.calibrate$CAL_MAX))])
    
    ##-------------------------------------------------------
    ## Grouped Soil Parameters
    ##-------------------------------------------------------
    
    soil.calibration.specials <- soil.parameters.calibrate[soil.parameters.calibrate$GROUP == "CalibrationGroups", ]
    
    if(nrow(soil.calibration.specials) > 0){
      
      for(i in 1:nrow(soil.calibration.specials)){
        
        soil.special_parameter <- soil.calibration.specials[i,]
        
        soil.parameters.calibrate[soil.parameters.calibrate$PARAMETER == soil.special_parameter$PARAMETER & soil.parameters.calibrate$VALUE == soil.special_parameter$DEFINITION, "CAL_VAR"] <- paste(soil.special_parameter$DEFINITION, soil.special_parameter$PARAMETER, sep = "_")
        
      }
      
      soil.parameters.calibrate <- soil.parameters.calibrate[!soil.parameters.calibrate$GROUP == "CalibrationGroups", ]
      
    }
    
    ##-------------------------------------------------------
    ## Generate soil.parameter.table.calibrate
    ##-------------------------------------------------------
    
    layers <- unique(soil.parameters$DEFINITION)
    
    soil.parameter.table.calibrate <- matrix(nrow = length(layers), ncol = length(soil.parameter.names) + 1, NA)
    
    
    for(i in 1:length(soil.parameter.names)){
      
      para <- soil.parameters.calibrate[soil.parameters.calibrate$PARAMETER == soil.parameter.names[i],]
      
      extract <- para[, c("DEFINITION", "CAL_VAR")]
      
      if(i == 1){
        
        soil.parameter.table.calibrate[,1:2] <- as.matrix(extract)
        
      } else { 
        
        soil.parameter.table.calibrate[,i+1] <- c(as.matrix(extract[,"CAL_VAR"]), rep("_DEFAULT", length(layers) - nrow(extract)))
      }
      
      soil.units[i] <- unique(para[,"UNITS"])
      
    }
    
    print("One or more soil parameters will be included in the calibration...")
    
  } else {
    
    soil.calibrate <- FALSE
    
    print("No soil parameters will be included in the calibration...")
    
  }
  
  
  ##-------------------------------------------------------
  ##
  ## Landuse Parameters
  ##
  ##-------------------------------------------------------
  
  if(!all(is.na(landuse.parameters$CAL_MAX))){
    
    landuse.calibrate <- TRUE
    
    
    landuse.parameters.calibrate <- RVP.template[RVP.template$GROUP == "LandUseParameter" | RVP.template$GROUP == "CalibrationGroups",  ]
    
    ## convert all columns to character
    # landuse.parameters.calibrate[,] <- lapply(landuse.parameters.calibrate[, ], as.character)
    
    # landuse.parameters.calibrate$CAL_VAR[which(!is.na(landuse.parameters.calibrate$CAL_MAX))] <- paste("par", landuse.parameters.calibrate$PARAMETER[which(!is.na(landuse.parameters.calibrate$CAL_MAX))], landuse.parameters.calibrate$DEFINITION[which(!is.na(landuse.parameters.calibrate$CAL_MAX))], sep = "_")
    # landuse.parameters.calibrate$CAL_VAR[which(!is.na(landuse.parameters.calibrate$CAL_MAX))] <- paste(landuse.parameters.calibrate$DEFINITION[which(!is.na(landuse.parameters.calibrate$CAL_MAX))], landuse.parameters.calibrate$PARAMETER[which(!is.na(landuse.parameters.calibrate$CAL_MAX))], sep = "_")
    
    landuse.parameters.calibrate$CAL_VAR[which(is.na(landuse.parameters.calibrate$CAL_VAR))] <- paste(landuse.parameters.calibrate$DEFINITION[which(is.na(landuse.parameters.calibrate$CAL_VAR))], landuse.parameters.calibrate$PARAMETER[which(is.na(landuse.parameters.calibrate$CAL_VAR))], sep = "_")
    
    # landuse.parameters.calibrate$CAL_VAR[which(is.na(landuse.parameters.calibrate$CAL_MAX))] <- as.character(landuse.parameters.calibrate$VALUE[which(is.na(landuse.parameters.calibrate$CAL_MAX))])
    
    
    ##-------------------------------------------------------
    ## Grouped Landuse Parameters
    ##-------------------------------------------------------
    
    landuse.calibration.specials <- landuse.parameters.calibrate[landuse.parameters.calibrate$GROUP == "CalibrationGroups", ]
    
    if(nrow(landuse.calibration.specials) > 0){
      
      for(i in 1:nrow(landuse.calibration.specials)){
        
        landuse.special_parameter <- landuse.calibration.specials[i,]
        
        landuse.parameters.calibrate[landuse.parameters.calibrate$PARAMETER == landuse.special_parameter$PARAMETER & landuse.parameters.calibrate$VALUE == landuse.special_parameter$DEFINITION, "CAL_VAR"] <- paste(landuse.special_parameter$DEFINITION, landuse.special_parameter$PARAMETER, sep = "_")
        
      }
      
      landuse.parameters.calibrate <- landuse.parameters.calibrate[!landuse.parameters.calibrate$GROUP == "CalibrationGroups", ]
      
    }
    
    ##-------------------------------------------------------
    ## Generate landuse.parameter.table.calibrate
    ##-------------------------------------------------------
    
    layers <- unique(landuse.parameters.calibrate$DEFINITION)
    
    landuse.parameter.table.calibrate <- matrix(nrow = length(layers), ncol = length(landuse.parameter.names) + 1, NA)
    
    for(i in 1:length(landuse.parameter.names)){
      
      para <- landuse.parameters.calibrate[landuse.parameters.calibrate$PARAMETER == landuse.parameter.names[i],]
      
      extract <- para[, c("DEFINITION", "CAL_VAR")]
      
      if(i == 1){
        
        landuse.parameter.table.calibrate[,1:2] <- as.matrix(extract)
        
      } else { 
        landuse.parameter.table.calibrate[,i+1] <- c(as.matrix(extract[,"CAL_VAR"]), rep("_DEFAULT", length(layers) - nrow(extract)))
      }
      
      landuse.units[i] <- unique(para[,"UNITS"])
      
    }
    
    print("One or more landuse parameters will be included in the calibration...")
    
  } else {
    
    landuse.calibrate <- FALSE
    
    print("No landuse parameters will be included in the calibration...")
    
  }
  
  
  ##-------------------------------------------------------
  ##
  ## Vegetation Parameters
  ##
  ##-------------------------------------------------------
  
  if(!all(is.na(vegetation.parameters$CAL_MAX))){
    
    vegetation.calibrate <- TRUE
    
    
    vegetation.parameters.calibrate <- RVP.template[RVP.template$GROUP == "VegetationParameter" | RVP.template$GROUP == "CalibrationGroups", ]
    
    ## convert all columns to character
    # vegetation.parameters.calibrate[,] <- lapply(vegetation.parameters.calibrate[, ], as.character)
    
    # vegetation.parameters.calibrate$CAL_VAR[which(!is.na(vegetation.parameters.calibrate$CAL_MAX))] <- paste("par", vegetation.parameters.calibrate$PARAMETER[which(!is.na(vegetation.parameters.calibrate$CAL_MAX))], vegetation.parameters.calibrate$DEFINITION[which(!is.na(vegetation.parameters.calibrate$CAL_MAX))], sep = "_")
    # vegetation.parameters.calibrate$CAL_VAR[which(!is.na(vegetation.parameters.calibrate$CAL_MAX))] <- paste(vegetation.parameters.calibrate$DEFINITION[which(!is.na(vegetation.parameters.calibrate$CAL_MAX))], vegetation.parameters.calibrate$PARAMETER[which(!is.na(vegetation.parameters.calibrate$CAL_MAX))], sep = "_")
    vegetation.parameters.calibrate$CAL_VAR[which(is.na(vegetation.parameters.calibrate$CAL_VAR))] <- paste(vegetation.parameters.calibrate$DEFINITION[which(is.na(vegetation.parameters.calibrate$CAL_VAR))], vegetation.parameters.calibrate$PARAMETER[which(is.na(vegetation.parameters.calibrate$CAL_VAR))], sep = "_")
    # vegetation.parameters.calibrate$CAL_VAR[which(is.na(vegetation.parameters.calibrate$CAL_MAX))] <- as.character(vegetation.parameters.calibrate$VALUE[which(is.na(vegetation.parameters.calibrate$CAL_MAX))])
    
    
    ##-------------------------------------------------------
    ## Grouped Vegetation Parameters
    ##-------------------------------------------------------
    
    vegetation.calibration.specials <- vegetation.parameters.calibrate[vegetation.parameters.calibrate$GROUP == "CalibrationGroups", ]
    
    if(nrow(vegetation.calibration.specials) > 0){
      
      for(i in 1:nrow(vegetation.calibration.specials)){
        
        vegetation.special_parameter <- vegetation.calibration.specials[i,]
        
        vegetation.parameters.calibrate[vegetation.parameters.calibrate$PARAMETER == vegetation.special_parameter$PARAMETER & vegetation.parameters.calibrate$VALUE == vegetation.special_parameter$DEFINITION, "CAL_VAR"] <- paste(vegetation.special_parameter$DEFINITION, vegetation.special_parameter$PARAMETER, sep = "_")
        
      }
      
      vegetation.parameters.calibrate <- vegetation.parameters.calibrate[!vegetation.parameters.calibrate$GROUP == "CalibrationGroups", ]
      
    }
    
    ##-------------------------------------------------------
    ## Generate vegetation.parameter.table.calibrate
    ##-------------------------------------------------------
    
    layers <- unique(vegetation.parameters.calibrate$DEFINITION)
    
    vegetation.parameter.table.calibrate <- matrix(nrow = length(layers), ncol = length(vegetation.parameter.names) + 1, NA)
    
    
    for(i in 1:length(vegetation.parameter.names)){
      
      para <- vegetation.parameters.calibrate[vegetation.parameters.calibrate$PARAMETER == vegetation.parameter.names[i],]
      
      extract <- para[, c("DEFINITION", "CAL_VAR")]
      
      if(i == 1){
        
        vegetation.parameter.table.calibrate[,1:2] <- as.matrix(extract)
        
      } else { 
        vegetation.parameter.table.calibrate[,i+1] <- c(as.matrix(extract[,"CAL_VAR"]), rep("_DEFAULT", length(layers) - nrow(extract)))
      }
      
      vegetation.units[i] <- unique(para[,"UNITS"])
      
    }
    
    print("One or more vegetation parameters will be included in the calibration...")
    
  } else {
    
    vegetation.calibrate <- FALSE
    
    print("No vegetation parameters will be included in the calibration...")
    
  }
  
  ##-------------------------------------------------------
  ##
  ## Global Parameters
  ##
  ##-------------------------------------------------------
  
  global.parameters <- RVP.template[RVP.template$GROUP == "GlobalParameter",]
  
  if(!all(is.na(global.parameters$CAL_MAX))){
    
    global.calibrate <- TRUE
    
    global.parameters.calibrate <- global.parameters
    
    # global.parameters.calibrate$CAL_VAR[which(!is.na(global.parameters.calibrate$CAL_MAX))] <- paste("par", global.parameters.calibrate$PARAMETER[which(!is.na(global.parameters.calibrate$CAL_MAX))], global.parameters.calibrate$DEFINITION[which(!is.na(global.parameters.calibrate$CAL_MAX))], sep = "_")
    global.parameters.calibrate$CAL_VAR[which(!is.na(global.parameters.calibrate$CAL_MAX))] <- paste(global.parameters.calibrate$DEFINITION[which(!is.na(global.parameters.calibrate$CAL_MAX))], global.parameters.calibrate$PARAMETER[which(!is.na(global.parameters.calibrate$CAL_MAX))], sep = "_")
    
    global.parameters.calibrate$CAL_VAR[which(is.na(global.parameters.calibrate$CAL_MAX))] <- as.character(global.parameters.calibrate$VALUE[which(is.na(global.parameters.calibrate$CAL_MAX))])
    
    global.parameters.calibrate <- global.parameters.calibrate[, c("PARAMETER", "DEFINITION", "CAL_VAR")]  
    
    ## add annual runoff value to global table from annual.runoff master list
    avg.annual.runoff <- annual.runoff[annual.runoff$WATERSHED %in% include.watersheds, "AVG_ANNUAL_RUNOFF_APEX_FAN"]
    
    global.parameters.calibrate[global.parameters.calibrate$DEFINITION == "AVG_ANNUAL_RUNOFF", "CAL_VAR"] <- as.character(mean(avg.annual.runoff))
    
    global.parameters.calibrate$PARAMETER <- paste(":", global.parameters.calibrate$PARAMETER, sep = "")
    
    print("One or more global parameters will be included in the calibration...")
    
  } else {
    
    global.calibrate <- FALSE
    
    print("No global parameters will be included in the calibration...")
    
  }
  
  
  ##-------------------------------------------------------
  ##
  ## Soil Profiles - if calibrate.soil.thickness =- TRUE, soil thicknesses are included.
  ##
  ##-------------------------------------------------------
  
  if(calibrate.soil.thicknesses == TRUE & run.ostrich == TRUE){
    
    soils.to.calibrate <- c()
    
    for(i in 1:length(include.watersheds)){
    
    soil.profiles.calibrate <- read.csv(file.path(global.input.dir, processed.spatial.dir, soil.profile.table.calibration.file))
      
    soil.classes.calibrate <- read.csv(file.path(global.input.dir, processed.spatial.dir, soil.thickness.range.calibration.file))
    
    included.soils <- soil.profiles.calibrate[which(soil.profiles.calibrate[,1] %like% include.watersheds[i]), ]
    
    included.soils <- lapply(included.soils[, ], as.character)
    
    soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), ] <- included.soils
    
    soil.profiles[which(soil.profiles[,1] %like% "GRAVEL_PIT"), c(4,6,8)] <- 0
    
    soil.profiles[which(soil.profiles[,1] %like% "CUT_FILL"), c(4,6,8)] <- 0
    
    soil.profiles[which(soil.profiles[,1] %like% "DIKE"), c(4,6,8)] <- 0
    
    soils.to.calibrate <- c(soils.to.calibrate, as.character(soil.classes.calibrate[which(soil.classes.calibrate$Parameter_Name %like% include.watersheds[i]), "Parameter_Name"]))
    }
  
    ## IF all soil thicknesses are being calibrated, use the below code.
    
    # soils.to.calibrate <- c()
    # 
    # for(i in 1:length(include.watersheds)){
    #   
    #   soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 4] <- paste(soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 3], "_THICK", sep = "")
    #   
    #   soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 6] <- paste(soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 5], "_THICK", sep = "")
    #   
    #   soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 8] <- paste(soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 7], "_THICK", sep = "")
    # 
    #   soil.profiles[which(soil.profiles[,1] %like% "GRAVEL_PIT"), c(4,6,8)] <- 0
    #   
    #   soil.profiles[which(soil.profiles[,1] %like% "CUT_FILL"), c(4,6,8)] <- 0
    #   
    #   soil.profiles[which(soil.profiles[,1] %like% "DIKE"), c(4,6,8)] <- 0
    #   
    #   soils.to.calibrate <- c(soils.to.calibrate, c(paste(soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 3], "_THICK", sep = ""),
    #                           paste(soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 5], "_THICK", sep = ""),
    #                           paste(soil.profiles[which(soil.profiles[,1] %like% include.watersheds[i]), 7], "_THICK", sep = "")))
    #     
    # }
    
    print("Soil Thicknesses are included in Calibration...")
    
  }
  
  
  
  #############################################################################################
  ## 
  ##  Generate *.rvp.tpl file
  ##
  #############################################################################################
  
  OstrichRVPTemplateFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvp.tpl", sep = ""))
  
  
  cat(file = OstrichRVPTemplateFile, append = F, sep = "",
      
      "#########################################################################","\n",
      ":FileType rvp Raven 2.8","\n",
      "# DataType         Raven RVP file","\n",
      ":Application       R","\n",
      ":WrittenBy         Lawrence Bird","\n",
      ":CreationDate  ",    paste(Sys.time()),"\n",
      "#---------------------------------------------------------", "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Soil Classes --------------------------------------", "\n",
      
      ":SoilClasses","\n",
      "     :Attributes,", "%SAND, %CLAY, %SILT, %ORGANIC", "\n",
      "     :Units,", "none, none, none, none", "\n"
  )
  
  write.table(soil.classes, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      ":EndSoilClasses", "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Land Use Classes ----------------------------------", "\n",
      ":LandUseClasses", "\n",
      "     :Attributes,  IMPERMEABLE_FRAC, FOREST_COVERAGE", "\n",
      "     :Units, fract,  fract", "\n"
  )
  
  write.table(landuse.classes, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      ":EndLandUseClasses", "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Vegetation Classes ----------------------------------", "\n",
      ":VegetationClasses", "\n",
      "     :Attributes,  MAX_HT, MAX_LAI,  MAX_LEAF_COND", "\n",
      "     :Units, m,  none, mm_per_s", "\n"
  )
  
  write.table(vegetation.classes, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      ":EndVegetationClasses", "\n")
  
  if(ncol(as.data.frame(seasonal.LAI)) > 1){
    cat(file = OstrichRVPTemplateFile, append = T, sep = "",
        "#------Seaonal Canopy LAI Changes-------------------", "\n",
        ":SeasonalCanopyLAI","\n"
    )
    
    write.table(seasonal.LAI, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
    
    cat(file = OstrichRVPTemplateFile, append = T, sep = "",
        ":EndSeasonalCanopyLAI", "\n",
        "\n")
  }
  
  if(ncol(as.data.frame(seasonal.HT)) > 1){
    cat(file = OstrichRVPTemplateFile, append = T, sep = "",
        "#------Seaonal Canopy Height Changes-------------------", "\n",
        ":SeasonalCanopyHeight","\n"
    )
    
    write.table(seasonal.HT, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
    
    cat(file = OstrichRVPTemplateFile, append = T, sep = "",
        ":EndSeasonalCanopyHeight", "\n",
        "\n")
  }
  
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      "#---------------------------------------------------------", "\n",
      "# ---- Soil Profiles -------------------------------------", "\n",
      ":SoilProfiles", "\n",
      "# Name, # Horizons, Horizon 1, Thickness_1, Horizon 2, Thickness_2, ..., Horizon X, Thickness_X", "\n"
  )
  
  write.table(soil.profiles, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      ":EndSoilProfiles", "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Global Parameters ---------------------------------", "\n"
  )
  
  if(global.calibrate == TRUE){
    
    write.table(global.parameters.calibrate, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
    
  } else {
    
    write.table(global, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  }
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      "#---------------------------------------------------------", "\n",
      "# ---- Land Use Parameters -------------------------------", "\n",
      ":LandUseParameterList", "\n",
      ":Parameters, ", paste(landuse.parameter.names, collapse = ", "), "\n",
      ":Units, ", paste(landuse.units, collapse = ","), "\n"
  )
  
  if(landuse.calibrate == TRUE){
    
    write.table(landuse.parameter.table.calibrate, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
    
  } else {
    
    write.table(landuse.parameter.table, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
  }
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      ":EndLandUseParameterList", "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Vegetation Parameters -----------------------------------", "\n",
      ":VegetationParameterList", "\n",
      ":Parameters, ", paste(vegetation.parameter.names, collapse = ", "), "\n",
      ":Units, ", paste(vegetation.units, collapse = ", "), "\n"
  )
  
  if(vegetation.calibrate == TRUE){
    
    write.table(vegetation.parameter.table.calibrate, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
    
  } else {
    
    write.table(vegetation.parameter.table, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
  }
  
  
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      ":EndVegetationParameterList", "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Soil Parameters -----------------------------------", "\n",
      ":SoilParameterList", "\n",
      ":Parameters, ", paste(soil.parameter.names, collapse = ", "), "\n",
      ":Units, ", paste(soil.units, collapse = ", "), "\n"
  )
  
  if(soil.calibrate == TRUE){
    
    write.table(soil.parameter.table.calibrate, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
    
  } else {
    
    write.table(soil.parameter.table, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
    
  }
  
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",    
      ":EndSoilParameterList", "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Channel Profiles ----------------------------------", "\n",
      ":ChannelProfile  ", paste(channel[channel$PARAMETER == "ChannelProfile", "VALUE"]), "\n",
      ":Bedslope  ", paste(channel[channel$PARAMETER == "Bedslope", "VALUE"]), "\n",
      ":SurveyPoints", "\n"
  )
  
  write.table(surveypoints, OstrichRVPTemplateFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
  
  cat(file = OstrichRVPTemplateFile, append = T, sep = "",
      ":EndSurveyPoints", "\n",
      "\n",
      ":RoughnessZones", "\n",
      paste(channel[channel$PARAMETER == "RoughnessZones", "DEFINITION"]), "\t",
      paste(channel[channel$PARAMETER == "RoughnessZones", "VALUE"]), "\n",
      ":EndRoughnessZones", "\n",
      ":EndChannelProfile", "\n"
  )
  
  
} else {print("No Ostrich files are being generated for this model run...")}

