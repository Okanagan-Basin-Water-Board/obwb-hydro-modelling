############################################################################################################################
##
## This script generates the required ostIn.txt file for input into Ostrich
##
## Mar-21-2019
##
############################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

require(dplyr)

OST.template <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, OST.template.in.file))
  
essential.var <- OST.template[OST.template$VARIABLE == "Essential", c("TYPE", "DEFINITION")]

useful.var <- OST.template[OST.template$VARIABLE == "Useful", c("TYPE", "DEFINITION")]

# response.var <- OST.template[OST.template$VARIABLE == "Response", c("TYPE", "DEFINITION")]

constraint.var <- OST.template[OST.template$VARIABLE == "Constraint", c("TYPE", "DEFINITION")]

algorithm.defs <- OST.template[OST.template$VARIABLE == "Algorithm", c("TYPE", "DEFINITION")]

seed.var <- OST.template[OST.template$VARIABLE == "Seed", c("TYPE", "DEFINITION")]


##########################################################
##
## Determine File Pairs
##
##########################################################


### Identify all *.tpl files
OST.template.files <- list.files(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")), pattern = ".tpl", recursive = TRUE)

## Create an empty matrix to house all file pairs
file.pairs <- matrix(NA, nrow = length(OST.template.files), ncol = 2)

## Create an empty vector to house file names which should not be moved to the model subdirectory
do.not.move <- c()

## Loop over *.tpl files and find the location of the partner
for(i in 1:length(OST.template.files)){
  
  Raven.template.files <- list.files(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")), pattern = gsub('.{4}$', '', strsplit(OST.template.files[i], "/")[[1]][2]), recursive = TRUE)
  
  raven.files <- dplyr::setdiff(Raven.template.files, OST.template.files)
  
  file.pairs[i,1] <- OST.template.files[i]
  
  ## Because reservoirs subfolder is moved into model folder in parallel processing, add model/ path to file pairs, where appropriate
  file.pairs[i,2] <- ifelse(sub("\\/.*", "", raven.files) == "reservoirs", paste("model/", raven.files, sep = ""), raven.files)
  
  ## Identify files that should not be moved to model subdirectory
  if(sub("\\/.*", "", raven.files) != "reservoirs"){
    
    do.not.move <- c(do.not.move, raven.files)
    
  }
  
}

# if(length(OST.template.files >= 1)){
# 
# raven.files <- gsub('.{4}$', '', OST.template.files)
# 
# print(paste(length(OST.template.files), "Ostrich template file(s) found in specified directory..."))
# 
#   } else {
#   
#     print("No Ostrich template files found in specified directory ...")
#   
#   }


# file.pairs <- matrix(NA, nrow = length(raven.files), ncol = 2)
# 
# file.pairs[,1] <- OST.template.files
# 
# file.pairs[,2] <- raven.files

###############################################################
##
## Generate Parameter Table
##
###############################################################

##------------------------------------------------------------
##
## Generate parameters from RVP file
##
##------------------------------------------------------------

RVP.template <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, RVP.template.in.file), na.strings = c(""))



## Make all columns characters
RVP.template[,] <- lapply(RVP.template[, ], as.character)

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

## Delete rows that are Calibrationgroups AND CAL_MIN is na
RVP.template <- RVP.template[!(RVP.template$GROUP == "CalibrationGroups" & is.na(RVP.template$CAL_MIN)),]



## Isolate only parameters that will be included in calibration (i.e., remove parameters that don't have a calibration range specified)
parameters <- RVP.template[which(!is.na(RVP.template$CAL_MIN)), c("GROUP", "PARAMETER", "DEFINITION", "VALUE", "CAL_MIN", "CAL_MAX")]

## Convert all values to characters
parameters[,] <- lapply(parameters[, ], as.character)

calibration.specials <- parameters[parameters$GROUP == "CalibrationGroups", ]

# calibration.special.names <- paste(calibration.specials$DEFINITION, calibration.specials$PARAMETER, sep = "_")

if(nrow(calibration.specials) > 0){
  
  for(i in 1:nrow(calibration.specials)){
    
    special_parameter <- calibration.specials[i,]
    
    parameters[parameters$PARAMETER == special_parameter$PARAMETER & parameters$VALUE == special_parameter$DEFINITION, "DEFINITION"] <- special_parameter$DEFINITION
    
    parameters[parameters$PARAMETER == special_parameter$PARAMETER & parameters$VALUE == special_parameter$DEFINITION, "CAL_MIN"] <- special_parameter$CAL_MIN
    
    parameters[parameters$PARAMETER == special_parameter$PARAMETER & parameters$VALUE == special_parameter$DEFINITION, "CAL_MAX"] <- special_parameter$CAL_MAX
    
    parameters[parameters$PARAMETER == special_parameter$PARAMETER & parameters$VALUE == special_parameter$DEFINITION, "VALUE"] <- special_parameter$VALUE
    
    ## Remove duplicated rows from paramaters table
    parameters <- parameters[!duplicated(parameters), ]
    
  }
  
  ## Delete the rows that house the CalibrationGroup definitions
  parameters <- parameters[!parameters$GROUP == "CalibrationGroups",]
  
}

##------------------------------------------------------------
##
## Identify which SubbasinProperties should be excluded. This REQUIRES that the watershed name is used for the group name by default.
##
##------------------------------------------------------------

remove.subbasin.properties <- gsub('(.*)_\\w+', '\\1', watersheds)[!gsub('(.*)_\\w+', '\\1', watersheds) %in% include.watersheds]

parameters <- parameters[!c(parameters$GROUP == "SubbasinProperties" & parameters$DEFINITION %in% remove.subbasin.properties), ]


## Extract "Value" as the initial starting value for Ostrich. This matches the initial values in the *.rvp file and prevents having to use the "extract" function in Ostrich
initial <- as.numeric(as.character(parameters$VALUE))

tx.in <- "none"

tx.ost <- "none"

tx.out <- "none"

parameter.table <- matrix(NA, ncol = 7, nrow = length(parameters$DEFINITION))

# parameter.table[,1] <- paste("par", parameters$PARAMETER, parameters$DEFINITION, sep = "_")
parameter.table[,1] <- paste(parameters$DEFINITION, parameters$PARAMETER, sep = "_")

parameter.table[,2] <- initial

parameter.table[,3] <- parameters$CAL_MIN

parameter.table[,4] <- parameters$CAL_MAX

parameter.table[,5] <- tx.in

parameter.table[,6] <- tx.ost

parameter.table[,7] <- tx.out

initial.all <- initial

##------------------------------------------------------------
##
## Generate parameters from reservoir files (if applicable)
##
##------------------------------------------------------------


if(calibrate.reservoir.parameters == TRUE){
  
  all.reservoirs <- unique(subbasins.present$Reservoir_name)
  
  all.reservoirs <- as.character(all.reservoirs[!all.reservoirs %in% "<Null>"])
  
  if(length(all.reservoirs) >0){
    
    for(i in 1:length(all.reservoirs)){
      
      tmp <- read_xlsx(file.path(global.input.dir, raw.reservoir.in.dir, reservoir.in.file), sheet = all.reservoirs[i])
      
      calibration.parameter.table <- na.omit(tmp[!is.na(tmp$CAL_MIN) ,c("PARAMETER", "VALUE", 'CAL_MIN', "CAL_MAX")])
      
      reservoir.parameter.table <- matrix(NA, ncol = 7, nrow = length(calibration.parameter.table$PARAMETER))
      
      initial.res <- as.numeric(as.character(calibration.parameter.table$VALUE))
      
      reservoir.parameter.table[,1] <- paste(gsub('([[:punct:]])|\\s+','_',all.reservoirs[i]), calibration.parameter.table$PARAMETER, sep = "_")
      
      reservoir.parameter.table[,2] <- initial.res
      
      reservoir.parameter.table[,3] <- calibration.parameter.table$CAL_MIN
      
      reservoir.parameter.table[,4] <- calibration.parameter.table$CAL_MAX
      
      reservoir.parameter.table[,5] <- tx.in
      
      reservoir.parameter.table[,6] <- tx.ost
      
      reservoir.parameter.table[,7] <- tx.out
      
      
      ## Append reservoir parameters to main parameter table
      parameter.table <- rbind(parameter.table, reservoir.parameter.table)
      
      initial.all <- c(initial.all, initial.res)
      
    }
    
  }
  
}


##------------------------------------------------------------
##
## Generate parameters for soil thicknesses (if applicable)
##
##------------------------------------------------------------

if(calibrate.soil.thicknesses == TRUE){

  soil.thickness.ranges <- read.csv(file.path(global.input.dir, processed.spatial.dir, soil.thickness.range.calibration.file))
    
  ## Remove gravel pit, cut fill, and dike soils from calibrate soils
  
  soils.to.calibrate <- soils.to.calibrate[startsWith(soils.to.calibrate, "CUT_FILL") == FALSE]
  
  soils.to.calibrate <- soils.to.calibrate[startsWith(soils.to.calibrate, "GRAVEL_PIT") == FALSE]
  
  soils.to.calibrate <- soils.to.calibrate[startsWith(soils.to.calibrate, "DIKE") == FALSE]
  
  
  soil.thickness.calibrate <- soil.thickness.ranges[soil.thickness.ranges$Parameter_Name %in% soils.to.calibrate, ]

  soil.thickness.calibrate.table <- matrix(NA, ncol = 7, nrow = length(soil.thickness.calibrate$Parameter_Name))  

  initial.soil <- as.numeric(as.character(soil.thickness.calibrate$mean_thickness))
  
  soil.thickness.calibrate.table[,1] <- as.character(soil.thickness.calibrate$Parameter_Name)
  
  soil.thickness.calibrate.table[,2] <- initial.soil
  
  soil.thickness.calibrate.table[,3] <- soil.thickness.calibrate$min_thickness
  
  soil.thickness.calibrate.table[,4] <- soil.thickness.calibrate$max_thickness
  
  soil.thickness.calibrate.table[,5] <- tx.in
  
  soil.thickness.calibrate.table[,6] <- tx.ost
  
  soil.thickness.calibrate.table[,7] <- tx.out
  
  parameter.table <- rbind(parameter.table, soil.thickness.calibrate.table)
  
  initial.all <- c(initial.all, initial.soil)
  
}




#####################################
## Create response variables table
####################################

## Read-in the diagnostic file from the current model run
diag <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = "")))


## Determine the file path for the response variables
response.var.file <- file.path("./model", paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""))

## Generate requires response variable names
response.var.names <- paste(rep(available.response.vars, each = length(calibration.stations)), calibration.stations, sep = "_")

## Generate a string of response variable flags based on the names
response.var.string <- sapply(strsplit(response.var.names, split='_', fixed=TRUE), function(x) (x[1]))

## Generate appropriately weights response variables weights
response.var.weights <- rep(as.numeric(response.variable.weights), each = length(calibration.stations)) * as.numeric(calibration.station.weights)

## Identify the location of those variables which are weighted 0 (for removal)
zero.weights <- which(response.var.weights == 0)

## If response variables have 0 weights, remove them.
if(length(zero.weights) > 0){
  response.var.names <- response.var.names[-c(zero.weights)]
  
  response.var.weights <- response.var.weights[-c(zero.weights)]
  
  response.var.string <- response.var.string[-c(zero.weights)]
}


## Identify the row for each calibration station
row <- c()

for(i in 1:length(calibration.stations)){
  
  current.row <- which(grepl(paste(calibration.stations[i], collapse = "|"), diag$filename))
  
  row <- c(row, current.row)
  
}


## Identify the column of each response variable
response.var.column.names <- c()

response.var.column.numbers <- c()

for(i in 1:length(response.var.names)){
  
  current.response.var <- ifelse(response.var.string[i] == "NS", "DIAG_NASH_SUTCLIFFE", ifelse(response.var.string[i] == "LNS", "DIAG_LOG_NASH", stop("Current response variable cannot be used. Please include NS or LNS only")))
  
  response.var.column.names <- c(response.var.column.names, current.response.var)
  
  current.response.var.column.number <- which(colnames(diag) == current.response.var)
  
  response.var.column.numbers <- c(response.var.column.numbers, current.response.var.column.number)
  
}



# row <- which(grepl(paste(calibration.stations, collapse = "|"), diag$filename))
# col.response_1 <- which(colnames(diag) == "DIAG_NASH_SUTCLIFFE")
# 
# col.response

key <- "OST_NULL"

token <- "','"


#####################################
## Create constraints table
####################################

constraint.var.names <- paste(constraint.var$DEFINITION, calibration.stations, sep = "_")

col.constraint <- which(colnames(diag) == "DIAG_PCT_BIAS")





# #####################################
# ## Begin Tied Parameters (i.e., Field Capacity / Sat Wilt)
# ####################################
# 
# 
# tied.parameter.loc <- which(parameter.table[,1] %like% "FIELD_CAPACITY")
# 
# tied.parameters <- parameter.table[tied.parameter.loc, 1]
# 
# 
# parameter.table[tied.parameter.loc, 1] <- "FIELD_CAPACITY_X_PARAM"
# 
# 
# tied.parameter.table <- matrix(nrow = 1, ncol = 9)
# 
# 
# tied.parameter.table[1,1] <- tied.parameters[1]
# 
# tied.parameter.table[1,2] <- 2
# 
# tied.parameter.table[1,3] <- "[DEFAULT]_SAT_WILT"
# 
# tied.parameter.table[1,4] <- "FIELD_CAPACITY_X_PARAM"
# 
# tied.parameter.table[1,5] <- "linear"
# 
# tied.parameter.table[1,6] <- 0.0
# 
# tied.parameter.table[1,7] <- 1.0
# 
# tied.parameter.table[1,8] <- 1.0
# 
# tied.parameter.table[1,9] <- 0.0


###################################################################
##
## Write OstIn.txt File
##
###################################################################

OSTInFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "ostIn.txt")

cat(file = OSTInFile, append = F, sep = "",
    
    "#########################################################################","\n",
    "# Ostrich Input File for ", paste(ws.interest, run.number, sep = "-"), "\n",
    "#Application       R","\n",
    "#WrittenBy         Lawrence Bird","\n",
    "#CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Essential Variables -------------------------------", "\n",
    "\n"
)

write.table(essential.var, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Useful Variables ----------------------------------", "\n",
    "\n"
)

write.table(useful.var, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define File Pairs ---------------------------------", "\n",
    "\n",
    "BeginFilePairs", "\n"
)

write.table(file.pairs, OSTInFile, append = T, col.names = F, row.names = F, sep = "; ", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "EndFilePairs", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Extra Directories ---------------------------------", "\n",
    "\n",
    "BeginExtraDirs", "\n",
    "model", "\n",
    "EndExtraDirs", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Parameters ---------------------------------", "\n",
    "\n",
    "BeginParams", "\n",
    "# Parameter, Init, low, high, tx_in, tx_ost, tx_out", "\n"
)

write.table(parameter.table, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "EndParams", "\n",
    "\n"
    # "#---------------------------------------------------------", "\n",
    # "# ---- Define Tied Parameters -------------------------", "\n",
    # "\n",
    # "BeginTiedParams",
    # "\n"
)

# write.table(tied.parameter.table, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    # "\n",
    # "EndTiedParams",
    # "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Response Variables -------------------------", "\n",
    "\n",
    "BeginResponseVars", "\n",
    "# Name, Filename, Keyword, Line, Col, Token", "\n",
    
    paste(paste(response.var.names, response.var.file, ";", key, row, response.var.column.numbers, token, sep = " "), "\n"), "\n",
    paste(paste(constraint.var.names, response.var.file, ";", key, row, col.constraint, token, sep = " "), "\n"), "\n"
)

cat(file = OSTInFile, append = T, sep = "",
    "EndResponseVars", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Tied Response Variables --------------------", "\n",
    "\n",
    "BeginTiedRespVars", "\n",
    "# Name, Number of parameters, Parameter Names, Type, Type_Data", "\n",
    
    paste("NegNS", length(response.var.names), paste(response.var.names, collapse = " "), "wsum", paste(as.numeric(response.var.weights) * -1, collapse = " ")), "\n",

    paste("PBias",  length(constraint.var.names), paste(constraint.var.names, collapse = " "), "wsum", paste(as.numeric(calibration.station.weights), collapse = " ")), "\n"

)

cat(file = OSTInFile, append = T, sep = "",
    "EndTiedRespVars", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Objective Function -------------------------", "\n",
    "\n",
    "BeginGCOP", "\n",
    paste("CostFunction", "NegNS"), "\n",
    paste("PenaltyFunction", "APM"), "\n"
)

cat(file = OSTInFile, append = T, sep = "",
    "EndGCOP", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Set Constraints -----------------------------------", "\n",
    "\n",
    "BeginConstraints", "\n",
    "# name type  penalty lwr upr resp.var", "\n",
    "PbiasConst general 0.01 -50.0  50.0  PBias", "\n"
)

cat(file = OSTInFile, append = T, sep = "",
    "EndConstraints", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Set Seed -----------------------------------", "\n",
    "\n"
)

write.table(seed.var, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F )

cat(file = OSTInFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Begin InitParams ----------------------------------", "\n",
    "\n",
    "BeginInitParams","\n",
    paste(initial.all, collapse=" "),"\n",
    "EndInitParams"
)

cat(file = OSTInFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Begin Algorithm ----------------------------------", "\n",
    "\n",
    paste("Begin", essential.var[essential.var$TYPE == "ProgramType","DEFINITION"], "Alg", sep=""), "\n"
)

write.table(algorithm.defs, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F )

cat(file = OSTInFile, append = T, sep = "",
    paste("End", essential.var[essential.var$TYPE == "ProgramType", "DEFINITION"], "Alg", sep=""), "\n"
)


###################################################################
##
## Create required Ost-RAVEN.sh file for given run conditions
##
###################################################################

OSTRAVENFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "Ost-RAVEN.sh")

cat(file = OSTRAVENFile, append = F, sep = "",
    "set -e", "\n",
    "\n")
# paste("cp ", paste(ws.interest, "-",  run.number, ".rvp   ", sep = ""), paste("model/", paste(ws.interest, "-",  run.number, ".rvp", sep = ""), sep = ""), sep = ""), "\n",
# paste("cp ", paste(ws.interest, "-",  run.number, ".rvt   ", sep = ""), paste("model/", paste(ws.interest, "-",  run.number, ".rvt", sep = ""), sep = ""), sep = ""), "\n")
# Copy all relevant files into model subdirectory to initiate Ostrich run.
for(i in 1:length(do.not.move)){
  
  cat(file = OSTRAVENFile, append = T, sep = "",
      paste("cp ", do.not.move[i], "   model/", do.not.move[i], sep = ""),
      "\n"
  )
}

cat(file = OSTRAVENFile, append = T, sep = "",
    paste("cd model"), "\n",
    # paste("./raven_rev.exe", paste(ws.interest, run.number, sep = "-"), sep = " "), "\n",
    paste("./Raven.exe", paste(ws.interest, run.number, sep = "-"), sep = " "), "\n",
    "\n",
    "exit 0"
)


## Specify file permission for Ost-RAVEN.sh to allow it to be executable
Sys.chmod(path = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "Ost-RAVEN.sh"),
          mode = "777")

###################################################################
##
## Create required save_best.sh file for given run conditions
##
###################################################################


SaveBestFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "save_best.sh")

cat(file = SaveBestFile, append = F, sep = "",
    "set -e", "\n",
    "echo", " 'saving input files for the best solution found...'", "\n",
    "if [ ! -e best ] ; then", "\n",
    "mkdir best", "\n",
    "fi", "\n",
    "\n"
)


if(calibrate.reservoir.parameters == TRUE){
  
  cat(file = SaveBestFile, append = T, sep = "",
      "if [ ! -e best/reservoirs ] ; then", "\n",
      "mkdir best/reservoirs", "\n",
      "fi", "\n",
      "\n",
      "cp model/reservoirs/*.rvh best/reservoirs/.", "\n",
      "\n"
      #"cp ", paste(ws.interest, "-", run.number, ".rvp  ", sep = ""), paste("best/", ws.interest, "-", run.number, ".rvp", sep = ""), "\n",
      #"cp ", paste(ws.interest, "-", run.number, ".rvt  ", sep = ""), paste("best/", ws.interest, "-", run.number, ".rvt", sep = ""), "\n"
  )
  
}

for(i in 1:length(do.not.move)){
  
  cat(file = SaveBestFile, append = T, sep = "",
      paste("cp ", do.not.move[i], "   best/", do.not.move[i], sep = ""),
      "\n"
  )
}

cat(file = SaveBestFile, append = T, sep = "",
    # "cp ", paste(ws.interest, "-", run.number, ".rvp  ", sep = ""), paste("best/", ws.interest, "-", run.number, ".rvp", sep = ""), "\n",
    # "cp ", paste(ws.interest, "-", run.number, "_Diagnostics.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""), "\n",
    # "cp ", paste(ws.interest, "-", run.number, "_Hydrographs.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Hydrographs.csv", sep = ""), "\n",
    "cp ", paste("./model/", ws.interest, "-", run.number, "_Diagnostics.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""), "\n",
    # "cp ", paste("./model/", ws.interest, "-", run.number, "_Hydrographs.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Hydrographs.csv", sep = ""), "\n",
    
    "exit 0", "\n"
)

Sys.chmod(path = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "save_best.sh"),
          mode = "777")
