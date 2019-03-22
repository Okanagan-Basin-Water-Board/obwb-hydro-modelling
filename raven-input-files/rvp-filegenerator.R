############################################################################################################################
##
## This script generates the required *.rvp file for input into Raven.
##
## Feb-28-2019
##
############################################################################################################################

## Read in the same XXX_codes.csv files used to develop the RVH file. **at the moment, vegetation codes is not used in the 
## RVH file generation as LAND_USE_CLASS AND VEG_CLASS are the same for now. However, it's built in here to allow these
## to be differentiated in future iterations of this code.

RVP.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv")

soil.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/soil_profile_codes.csv")

aquifer.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/aquifer_codes.csv")

landcover.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/landcover_codes.csv")

vegetation.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/vegetation_codes.csv")

############################################################################################################################
##
## Generate required tables (in format required by RAVEN), specifically:
## - Soil Classes Table
## - Soil Profiles Table
## - Vegetation Classes Table
## - Land Use Classes Table
##
############################################################################################################################

##########################################################
## Soil Classes Table:
# - at the moment, only the soil horizon name is included - this can be extended to include %SAND, #CLAY, %SILT, %ORGANIC if available

## remove the "ALIAS_NAME" columns as this information is not needed in the *.rvp file
soil.codes <- soil.codes[ , !grepl("ALIAS", names(soil.codes))]

## remove the "LAKE" as 'NA' is not needed in soil horizons
soil.horizons <- soil.codes[complete.cases(soil.codes),]

## Exttract the names of soil layers
soil.horizons <- soil.horizons[ , grepl("NAME", names(soil.horizons) ) ]

## unlist to make a character vector
soil.horizons <- unique(unlist(soil.horizons))

## make a matrix of soil names
soil.classes <- matrix(nrow = length(soil.horizons), ncol = 1, data = soil.horizons)

##########################################################
## Soil Profiles Table:
# Remove columns 1, 2, and 3 (i.e., OID, Value, Count)

soil.profiles <- soil.codes[,-c(1,2,3)]

##########################################################
## Vegetation Classes Table:
# - identify unique vegetation types (i.e., unique rows) and exclude columns 1, 2, and 4 (i.e., value, Cover_type, and Bin_Value)
vegetation.classes <- unique(vegetation.codes[,c(3, 5:ncol(vegetation.codes))])

##########################################################
## Land Use Classes Table:
# - identify unique landuse types (i.e., unique rows) and exclude columns 1, 2, and 4 (i.e., value, Cover_type, and Bin_Value)

landuse.classes <- unique(landcover.codes[,c(3, 5:ncol(landcover.codes))])

##########################################################
## Land Use Parameters List

landuse.parameters <- RVP.template[RVP.template$GROUP == "LandUseParameter", ]

## convert all columns to character
landuse.parameters[,] <- lapply(landuse.parameters[, ], as.character)

landuse.parameter.names <- unique(landuse.parameters$PARAMETER)

layers <- unique(landuse.parameters$DEFINITION)

landuse.units <- rep(NA, length(landuse.parameter.names))

landuse.parameter.table <- matrix(nrow = length(layers), ncol = length(landuse.parameter.names) + 1, NA)



  for(i in 1:length(landuse.parameter.names)){
  
  para <- landuse.parameters[landuse.parameters$PARAMETER == landuse.parameter.names[i],]
  
  extract <- para[, c("DEFINITION", "VALUE")]
  
    if(i == 1){
    
      landuse.parameter.table[,1:2] <- as.matrix(extract)
      
      } else { 
        landuse.parameter.table[,i+1] <- as.matrix(extract[,"VALUE"])
      }
  
  landuse.units[i] <- unique(para[,"UNITS"])
    
  }


 

##########################################################
## Soil Parameters List

soil.parameters <- RVP.template[RVP.template$GROUP == "SoilParameter", ]

## convert all columns to character
soil.parameters[,] <- lapply(soil.parameters[, ], as.character)

soil.parameter.names <- unique(soil.parameters$PARAMETER)

layers <- unique(soil.parameters$DEFINITION)

soil.units <- rep(NA, length(soil.parameter.names))

soil.parameter.table <- matrix(nrow = length(layers), ncol = length(soil.parameter.names) + 1, NA)


for(i in 1:length(soil.parameter.names)){
  
  para <- soil.parameters[soil.parameters$PARAMETER == soil.parameter.names[i],]
  
  extract <- para[, c("DEFINITION", "VALUE")]
  
  if(i == 1){
    
    soil.parameter.table[,1:2] <- as.matrix(extract)
    
  } else { 
    soil.parameter.table[,i+1] <- as.matrix(extract[,"VALUE"])
  }
  
  soil.units[i] <- unique(para[,"UNITS"])
  
}


##########################################################
## Global Parameters Table:
##

global <- RVP.template[RVP.template$GROUP == "GlobalParameter", c("PARAMETER", "DEFINITION", "VALUE")]

global$PARAMETER <- paste(":", global$PARAMETER, sep = '')

##########################################################
## Channel Parameters Table:
##

channel <- RVP.template[RVP.template$GROUP == "ChannelParameter", ]

surveypoints <- channel[channel$PARAMETER == "SurveyPoints", c("DEFINITION", "VALUE")]

#############################################################################################
## Generate *.rvp file
##
#############################################################################################

RVPoutFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvp", sep = ""))


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
    "     :Attributes,", "\n",
    "     :Units,", "\n"
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
    ":EndVegetationClasses", "\n",
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

##########################################################
## Soil Parameters List

## Test if soil.parameters has any values in CAL_MAX then it should be included for calibration.
## If NOT all values are NA, generate soils.parameters.calibrate table and populate.
## If all values ARE NA, 
if(!all(is.na(soil.parameters$CAL_MAX))){
  
    soil.calibrate <- TRUE
    

    soil.parameters.calibrate <- RVP.template[RVP.template$GROUP == "SoilParameter", ]
    
    ## convert all columns to character
    soil.parameters.calibrate[,] <- lapply(soil.parameters.calibrate[, ], as.character)
    
    soil.parameters.calibrate$CAL_VAR[which(!is.na(soil.parameters.calibrate$CAL_MAX))] <- paste("par", soil.parameters.calibrate$PARAMETER[which(!is.na(soil.parameters.calibrate$CAL_MAX))], soil.parameters.calibrate$DEFINITION[which(!is.na(soil.parameters.calibrate$CAL_MAX))], sep = "_")
    
    soil.parameters.calibrate$CAL_VAR[which(is.na(soil.parameters.calibrate$CAL_MAX))] <- as.character(soil.parameters.calibrate$VALUE[which(is.na(soil.parameters.calibrate$CAL_MAX))])
    
    layers <- unique(soil.parameters$DEFINITION)
    
    soil.parameter.table.calibrate <- matrix(nrow = length(layers), ncol = length(soil.parameter.names) + 1, NA)
    
    
    for(i in 1:length(soil.parameter.names)){
      
      para <- soil.parameters.calibrate[soil.parameters.calibrate$PARAMETER == soil.parameter.names[i],]
      
      extract <- para[, c("DEFINITION", "CAL_VAR")]
      
      if(i == 1){
        
        soil.parameter.table.calibrate[1:nrow(para),1:2] <- as.matrix(extract)
        
      } else { 
        soil.parameter.table.calibrate[,i+1] <- as.matrix(extract[,"CAL_VAR"])
      }
      
      soil.units[i] <- unique(para[,"UNITS"])
      
    }
    
    print("One or more soil parameters will be included in the calibration...")

} else {
  
  soil.calibrate <- FALSE
  
  print("No soil parameters will be included in the calibration...")
  
  }


##########################################################
## Land Use Parameters List

if(!all(is.na(landuse.parameters$CAL_MAX))){
  
    landuse.calibrate <- TRUE


    landuse.parameters.calibrate <- RVP.template[RVP.template$GROUP == "LandUseParameter", ]
    
    ## convert all columns to character
    landuse.parameters.calibrate[,] <- lapply(landuse.parameters.calibrate[, ], as.character)
    
    landuse.parameters.calibrate$CAL_VAR[which(!is.na(landuse.parameters.calibrate$CAL_MAX))] <- paste("par", landuse.parameters.calibrate$PARAMETER[which(!is.na(landuse.parameters.calibrate$CAL_MAX))], landuse.parameters.calibrate$DEFINITION[which(!is.na(landuse.parameters.calibrate$CAL_MAX))], sep = "_")
    
    landuse.parameters.calibrate$CAL_VAR[which(is.na(landuse.parameters.calibrate$CAL_MAX))] <- as.character(landuse.parameters.calibrate$VALUE[which(is.na(landuse.parameters.calibrate$CAL_MAX))])
    
    layers <- unique(landuse.parameters.calibrate$DEFINITION)
    
    landuse.parameter.table.calibrate <- matrix(nrow = length(layers), ncol = length(landuse.parameter.names) + 1, NA)
    
    
    
    for(i in 1:length(landuse.parameter.names)){
      
      para <- landuse.parameters.calibrate[landuse.parameters.calibrate$PARAMETER == landuse.parameter.names[i],]
      
      extract <- para[, c("DEFINITION", "CAL_VAR")]
      
      if(i == 1){
        
        landuse.parameter.table.calibrate[,1:2] <- as.matrix(extract)
        
      } else { 
        landuse.parameter.table.calibrate[,i+1] <- as.matrix(extract[,"CAL_VAR"])
      }
      
      landuse.units[i] <- unique(para[,"UNITS"])
      
    }

    print("One or more landuse parameters will be included in the calibration...")

} else {
  
  landuse.calibrate <- FALSE
  
  print("No landuse parameters will be included in the calibration...")
  
  }


##########################################################
## Global Parameters

global.parameters <- RVP.template[RVP.template$GROUP == "GlobalParameter",]

if(!all(is.na(global.parameters$CAL_MAX))){
  
  global.calibrate <- TRUE

  global.parameters.calibrate <- global.parameters
  
  global.parameters.calibrate$CAL_VAR[which(!is.na(global.parameters.calibrate$CAL_MAX))] <- paste("par", global.parameters.calibrate$PARAMETER[which(!is.na(global.parameters.calibrate$CAL_MAX))], global.parameters.calibrate$DEFINITION[which(!is.na(global.parameters.calibrate$CAL_MAX))], sep = "_")
  
  global.parameters.calibrate$CAL_VAR[which(is.na(global.parameters.calibrate$CAL_MAX))] <- as.character(global.parameters.calibrate$VALUE[which(is.na(global.parameters.calibrate$CAL_MAX))])

  global.parameters.calibrate <- global.parameters.calibrate[, c("PARAMETER", "DEFINITION", "CAL_VAR")]  

  global.parameters.calibrate$PARAMETER <- paste(":", global.parameters.calibrate$PARAMETER, sep = "")
    
  print("One or more global parameters will be included in the calibration...")
  
} else {
  
  global.calibrate <- FALSE
  
  print("No global parameters will be included in the calibration...")
  
}

#############################################################################################
## Generate *.rvp.tpl file
##
#############################################################################################

OstrichRVPTemplateFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvp.tpl", sep = ""))


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
    "     :Attributes,", "\n",
    "     :Units,", "\n"
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
    ":EndVegetationClasses", "\n",
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
