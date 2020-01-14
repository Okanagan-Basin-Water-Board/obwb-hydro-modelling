#############################################################################################################################
##
## This script reads in "RVI-Template.csv" and generates required *.rvi file for ingestion to Raven.
##
#############################################################################################################################

## Load required packages
# library(cloudml)

####################################################################
## Read in required files
##
RVI.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVI-Template.csv")

# soil.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/soil_profile_codes.csv")

subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")


## Add colon to all parameter calls - this is required by Raven
RVI.template$PARAMETER <- paste(":", RVI.template$PARAMETER, sep = '')

## replce space in watershed name with "_"
subbasin.codes$GNIS_NAME <- gsub("\\s", "_", subbasin.codes$GNIS_NAME)


watersheds <- unique(subbasin.codes$GNIS_NAME)

model.watersheds <- paste(include.watersheds, "_Creek", sep = "")

disabled.watersheds <- watersheds[!watersheds %in% model.watersheds]

disabled.watersheds <- matrix(nrow = length(disabled.watersheds), ncol = 2, data = c(rep(":DisableHRUGroup", length(disabled.watersheds)),
                                                                   disabled.watersheds))

####################################################################
##
## Separate out group of parameters for writing to file

time <- RVI.template[RVI.template$GROUP == "Time", c("PARAMETER", "DEFINITION")]

timestep <- time$DEFINITION[time$PARAMETER == ":TimeStep"]

time$DEFINITION <- paste(as.Date(time$DEFINITION, format = "%m/%d/%Y"), "00:00:00", sep = ' ')

time$DEFINITION[time$PARAMETER == ":TimeStep"] <- as.character(timestep)


# filecontrol <- RVI.template[RVI.template$GROUP == "FileControl", c("PARAMETER", "DEFINITION")]

methods <- RVI.template[RVI.template$GROUP == "Methods", c("PARAMETER", "DEFINITION")]

routing <- RVI.template[RVI.template$GROUP == "Routing", c("PARAMETER", "DEFINITION")]

options <- RVI.template[RVI.template$GROUP == "Options", c("PARAMETER", "DEFINITION")]

processes <- RVI.template[RVI.template$GROUP == "HydrologicalProcesses", c("PARAMETER", "DEFINITION", "FROM", "TO")]

output <- RVI.template[RVI.template$GROUP == "OutputOptions", c("PARAMETER", "DEFINITION")]

customOptions <- RVI.template[RVI.template$GROUP == "CustomOptions", c("PARAMETER", "DEFINITION", "FROM", "TO")]

if(run.ostrich == TRUE){
  
  calibration.options <- RVI.template[RVI.template$GROUP == "CalibrationOptions", c("PARAMETER", "DEFINITION")]
  
}


####################################################################
##
## Generate Soil Layer Alias' Table


# soil.alias.names <- soil.codes[complete.cases(soil.codes),]
# 
# soil.alias <- soil.alias.names[ , grepl("ALIAS", names(soil.alias.names) ) ]
# 
# soil.alias <- unique(unlist(soil.alias))
# 
# soil.layers <- soil.alias.names[ , grepl("LAYER", names(soil.alias.names) ) ]
# 
# soil.layers <- unique(unlist(soil.layers))
# 
# # soil.classes <- matrix(nrow = length(soil.horizons), ncol = 1, data = soil.horizons)
# 
# soil.alias.table <- matrix(nrow = length(soil.alias), ncol = 3, data = c(alias = rep(":Alias", length(soil.alias)),
#                                                                         alias.name = paste(soil.alias),
#                                                                         soil.layer = paste(soil.layers)))

############################################################################################################################
##
## Generate *.rvi file
##
############################################################################################################################

RVIoutFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvi", sep = ""))

cat(file = RVIoutFile, append = F, sep = "",
    
    "#########################################################################","\n",
    ":FileType rvi Raven 2.8","\n",
    "# DataType         Raven Input file","\n",
    ":Application       R","\n",
    ":WrittenBy         Lawrence Bird","\n",
    ":CreationDate  ",    paste(Sys.time()),"\n",
    # ":NoisyMode", "\n",
    "#---------------------------------------------------------", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Temporal Specifications ---------------------------", "\n",
    "\n"
)

  write.table(time, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- File Control --------------------------------------", "\n",
      "\n",
      ":RunName ", paste(ws.interest, run.number, sep = "-"), "\n"
)  

  # write.table(filecontrol, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
  
cat(file = RVIoutFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Method Specifications -----------------------------", "\n",
    "\n"
)

  write.table(methods, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Routing Specifications ----------------------------", "\n",
      "\n"
)

  write.table(routing, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Options Specifications ----------------------------", "\n",
      "\n"
)

  write.table(options, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Soil Layer Alias Definitions ----------------------", "\n",
      "\n",
    ":Alias TOP_SOIL SOIL[0]", "\n",
    ":Alias INT_SOIL SOIL[1]", "\n",
    ":Alias DEEP_SOIL SOIL[2]", "\n"
)
  
  # write.table(soil.alias.table, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)


cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Process Specifications ----------------------------", "\n",
      ":HydrologicProcesses", "\n",
      "\n"
)
  
  write.table(processes, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)


## Specify HRU Groups to be disabled.
cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      ":EndHydrologicProcesses", "\n",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Define Watershed HRU Groups -----------------------", "\n",
      ":DefineHRUGroups ", paste(watersheds, collapse = ","), "\n",
      "\n",
      "#---------------------------------------------------------", "\n",
      "#----- Define HRU Group for all HRUs with Zero Soil Depth-", "\n",
      ":DefineHRUGroups Zero_Soils", "\n"
    
)

cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Specify HRU Groups to Disable  --------------------", "\n"
)

write.table(disabled.watersheds, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

if(nrow(customOptions) > 0){
  
  cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Custom Options ------------------------------------", "\n",
      "\n"
      )
  write.table(customOptions, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
  
}

cat(file = RVIoutFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Output Options ------------------------------------", "\n",
    "\n"
)  

write.table(output, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)


if(run.ostrich == TRUE){
  
  cat(file = RVIoutFile, append = T, sep = "",
      "\n",
      "#---------------------------------------------------------", "\n",
      "# ---- Calibration Specific Commands ---------------------", "\n",
      "\n"
  )
  
write.table(calibration.options, RVIoutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)  
  
}
  