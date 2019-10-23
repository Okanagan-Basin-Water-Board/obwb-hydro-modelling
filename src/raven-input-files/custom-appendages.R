#############################################################################################################################################################################
##
## This script handles a series of custom appendages to required Raven input files. The appendages included herein are not *required* (i.e., Raven will execute successfully
## withuot them); however, they are included to solicit improved model results, and/or support CustomOutput and plotting.
##
## Individual Code blocks are separated by #****************************************#
##
## Oct-21-2019 LAB
##
#############################################################################################################################################################################


## **************************************************************************************************************************************************************************
##
## This code block adds SubbasinProperties to the *.rvh file for a particular model run. This process is separated from the rvh-filegenerator.R due to computational efficieny.
## The processes within rvh-filegenerator.R take a long time to complete  whereas this process is fairly minor and quick. In addition, this process is affected by calibration 
## procedures


#########################################################
##
## Generate the required SubbasinProperties table and append it to the main *.rvh file
##
#########################################################

RVP.template.base <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv")

## Isolate only CalibrationGroups and SubbasinProperties
RVP.template.base <- RVP.template.base[RVP.template.base$GROUP == "CalibrationGroups" | RVP.template.base$GROUP == "SubbasinProperties", ]


##-------------------------------------------------------
##
## Grouped Calibration Parameters
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
    
    RVP.template[RVP.template$PARAMETER == special_parameter$PARAMETER & RVP.template$VALUE == special_parameter$DEFINITION, "VALUE"] <- special_parameter$VALUE
    
  }
  
  ## Delete the rows that house the CalibrationGroup definitions
  RVP.template <- RVP.template[!RVP.template$GROUP == "CalibrationGroups",]
  
}

##-------------------------------------------------------
##
## Generate the reauired SubbasinProperties Table
##
##-------------------------------------------------------

## Isolate only subbasins present within the current model run
subbasin.properties <- RVP.template[RVP.template$DEFINITION %in% subbasins.present$Subbasin_ID, ]

subbasin.property.names <- names(sort(summary(as.factor(subbasin.properties$PARAMETER)), decreasing = T))

layers <- unique(subbasin.properties$DEFINITION)

subbasin.units <- rep(NA, length(subbasin.property.names))

subbasin.properties.table <- matrix(nrow = length(layers), ncol = length(subbasin.property.names) + 1, NA)


## loop over all landuse parameters to populate the landuse.parameter.table
for(i in 1:length(subbasin.property.names)){
  
  para <- subbasin.properties[subbasin.properties$PARAMETER == subbasin.property.names[i],]
  
  extract <- para[, c("DEFINITION", "VALUE")]
  
  if(i == 1){
    ## if i == 1, include the "DEFINITION" column as landuse types. if i !=1, only include the "VALUE" column
    subbasin.properties.table[,1:2] <- as.matrix(extract)
    
  } else { 
    ## Concatenate the extracted value(s) and "_DEFAULT" to fill any missing specified parameter values. This allows not all paramaters to have to be specified for all landuse classes.
    subbasin.properties.table[,i+1] <- c(as.matrix(extract[,"VALUE"]), rep("_DEFAULT", length(layers) - nrow(extract)))
  }
  
  subbasin.units[i] <- unique(para[,"UNITS"])
  
}


##-------------------------------------------------------
##
## Read in the main *.rvh file to append the subbasin.properties.table to
##
##-------------------------------------------------------

## Identify the rvh file to append SubbasinProperties to
main.RVH.file <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = ""))  


cat(file = main.HRU.file, append = T, sep = "",
    "\n",
    "#-------------------------------------------------------", "\n",
    "#----- Specify Subbasin Properties", "\n",
    ":SubBasinProperties", "\n",
    ":Parameters, ", paste(subbasin.property.names, collapse = ", "), "\n",
    ":Units, ", paste(subbasin.units, collapse = ", "), "\n"
)

write.table(subbasin.properties.table, main.HRU.file, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")


cat(file = main.HRU.file, append = T, sep = "",
    ":EndSubBasinProperties", "\n"
)

#########################################################
##
## If run.ostrich == TRUE, generate an *.rvh.tpl file so subbasin properties are included in calibration
##
#########################################################

if(run.ostrich == TRUE){
  
  ## Reassign the base RVP.template to ensure that all GROUP labels are captured.
  RVP.template <- RVP.template.base
  
  ## Make all columns characters
  RVP.template[,] <- lapply(RVP.template[, ], as.character)
  
  ## Add empty column to house the calibration variable
  RVP.template$CAL_VAR <- NA
  
  ## Those paramaters which are not included in calibration should have their value assigned to the CAL_VAR column
  RVP.template[is.na(RVP.template$CAL_MIN), "CAL_VAR"] <- RVP.template$VALUE[is.na(RVP.template$CAL_MIN)]
  
  ##-------------------------------------------------------
  ##
  ## Subbasin Properties
  ##
  ##-------------------------------------------------------
  
  ## Test if subbasin.properties has any values in CAL_MAX then it should be included for calibration.
  ## If NOT all values are NA, generate subbasin.properties.calibrate table and populate.
  ## If all values ARE NA, 
  if(!all(is.na(subbasin.properties$CAL_MAX))){
    
    subbasins.calibrate <- TRUE
    
    ## Isolate only the subasins that are included in the current model run, as well as all CalibrationGroups
    subbasin.properties.calibrate <- RVP.template[RVP.template$DEFINITION %in% subbasins.present$Subbasin_ID | RVP.template$GROUP == "CalibrationGroups", ]
    
    ## concatenate all definitions and parameters to make unique calibration names for each entry
    subbasin.properties.calibrate$CAL_VAR[which(is.na(subbasin.properties.calibrate$CAL_VAR))] <- paste(subbasin.properties.calibrate$DEFINITION[which(is.na(subbasin.properties.calibrate$CAL_VAR))], subbasin.properties.calibrate$PARAMETER[which(is.na(subbasin.properties.calibrate$CAL_VAR))], sep = "_")
    
    ##-------------------------------------------------------
    ## Grouped Subbasin Properties
    ##-------------------------------------------------------
    
    subbasin.calibration.specials <- subbasin.properties.calibrate[subbasin.properties.calibrate$GROUP == "CalibrationGroups", ]
    
    if(nrow(subbasin.calibration.specials) > 0){
      
      for(i in 1:nrow(subbasin.calibration.specials)){
        
        subbasin.special_parameter <- subbasin.calibration.specials[i,]
        
        subbasin.properties.calibrate[subbasin.properties.calibrate$PARAMETER == subbasin.special_parameter$PARAMETER & subbasin.properties.calibrate$VALUE == subbasin.special_parameter$DEFINITION, "CAL_VAR"] <- paste(subbasin.special_parameter$DEFINITION, subbasin.special_parameter$PARAMETER, sep = "_")
        
      }
      
      subbasin.properties.calibrate <- subbasin.properties.calibrate[!subbasin.properties.calibrate$GROUP == "CalibrationGroups", ]
      
    }
    
    ##-------------------------------------------------------
    ## Generate subbasin.properties.table.calibrate
    ##-------------------------------------------------------
    
    subbasin.properties.table.calibrate <- matrix(nrow = length(layers), ncol = length(subbasin.property.names) + 1, NA)
    
    
    for(i in 1:length(subbasin.property.names)){
      
      para <- subbasin.properties.calibrate[subbasin.properties.calibrate$PARAMETER == subbasin.property.names[i],]
      
      extract <- para[, c("DEFINITION", "CAL_VAR")]
      
      if(i == 1){
        
        subbasin.properties.table.calibrate[,1:2] <- as.matrix(extract)
        
      } else { 
        
        subbasin.properties.table.calibrate[,i+1] <- c(as.matrix(extract[,"CAL_VAR"]), rep("_DEFAULT", length(layers) - nrow(extract)))
      }
      
      subbasin.units[i] <- unique(para[,"UNITS"])
      
    }
    
    print("One or more subbasin property will be included in the calibration...")
    
  } else {
    
    subbasins.calibrate <- FALSE
    
    print("No subbasin properties will be included in the calibration...")
    
  }
  


  #############################################################################################
  ## 
  ##  Generate *.rvh.tpl file
  ##
  #############################################################################################
  
  
  if(subbasins.calibrate == TRUE){
  
    ## Re-copy the master *.rvh files to the templates folder. This will be adjusted to operate as an OSTRICH template.
    file.copy(from = file.path("/var/obwb-hydro-modelling/simulations/Master.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "templates"))
    
    ## Renames the above file to be *.rvh.tpl
    file.rename(from = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", "Master.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvh.tpl", sep = "")))
  
    
  
    OstrichRVHTemplateFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvh.tpl", sep = ""))
    
    
    cat(file = OstrichRVHTemplateFile, append = T, sep = "",
        "\n",
        "#-------------------------------------------------------", "\n",
        "#----- Specify Subbasin Properties", "\n",
        ":SubBasinProperties", "\n",
        ":Parameters, ", paste(subbasin.property.names, collapse = ", "), "\n",
        ":Units, ", paste(subbasin.units, collapse = ", "), "\n"
    )
    
    write.table(subbasin.properties.table.calibrate, OstrichRVHTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F, na = "")
    
    
    cat(file = OstrichRVHTemplateFile, append = T, sep = "",
        ":EndSubBasinProperties", "\n"
    )
  
  }

}


## **************************************************************************************************************************************************************************
##
## This code block handles the definition, and inclusion of custom HRU groups for all Snow Courses and/or Snow Pillows within a given model run. This process is separated from
## the *.rvi/*.rvh file generators since it is not *required*. This process allows CustomOutput to provide summary of SNOW by HRU Group (i.e., the HRU that corresponds o each snow
## course or snow pillow)

##-------------------------------------------------------
##
## Define HRU groups for all snow courses/snow pillows within the modelled watershed(s) within the *.rvi file
##
##-------------------------------------------------------


## Identify the main RVI file to define the snow groups
main.RVI.file <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvi", sep = ""))

## Identify the main RVH file to create the snow groups
main.RVH.file <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = ""))


## ---------------------------------
## If snow courses are included in the modelled watershed(s), define and create groups in rvi and rvh files
## ---------------------------------
if(length(all.snow.courses.included) > 0){
  
  ## Define snow course groups in the *.rvi file
  cat(file = main.RVI.file, append = T, sep = "",
      "\n",
      "#-------------------------------------------------------", "\n",
      "#-------- Define Snow Course Groups --------------------", "\n",
      "\n",
      ":DefineHRUGroups ", paste(paste("SC", all.snow.courses.included, sep = "_"), collapse = ","), "\n"
  )
  
  
  ## Create snow course groups in the *.rvh file
  for(i in 1:length(all.snow.courses.included)){
  
    HRU_ID <- snow.course.locations$HRU[snow.course.locations$LCTN_ID %in% all.snow.courses.included[i]]
  
  if(i == 1){
    cat(file = main.RVH.file, append = T, sep = "",
        "\n",
        "\n",
        "#-------------------------------------------------------", "\n",
        "#-------- Create Snow Course Groups --------------------", "\n",
        "\n",
        ":HRUGroup ", paste(paste("SC", all.snow.courses.included[i], sep = "_"), collapse = ","), "\n",
        HRU_ID, "\n",
        ":EndHRUGroup", "\n"
    )} else {
      cat(file = main.RVH.file, append = T, sep = "",
          ":HRUGroup ", paste(paste("SC", all.snow.courses.included[i], sep = "_"), collapse = ","), "\n",
          HRU_ID, "\n",
          ":EndHRUGroup", "\n"
      ) 
    }
  
  
  } # End for loop of snow.courses.included
} # End if exists("snow.courses.included)



## ---------------------------------
## If snow pillows are included in the modelled watershed(s), define and create groups in rvi and rvh files
## ---------------------------------
if(length(all.snow.pillows.included) > 0){
  
  all.snow.pillows.included <- sub('.', '', all.snow.pillows.included)
  
  ## Define snow pillow groups in the *.rvi file
  cat(file = main.RVI.file, append = T, sep = "",
      "\n",
      "#-------------------------------------------------------", "\n",
      "#-------- Define Snow Pillow Groups --------------------", "\n",
      "\n",
      ":DefineHRUGroups ", paste(paste("SP", all.snow.pillows.included,sep = "_"), collapse = ","), "\n"
  )
  
  
  ## Create snow pillow groups in the *.rvh file
  for(i in 1:length(all.snow.pillows.included)){
    
    HRU_ID <- snow.pillow.locations$HRU[snow.pillow.locations$LCTN_ID %in% all.snow.pillows.included[i]]
    
    if(i == 1){
      cat(file = main.RVH.file, append = T, sep = "",
          "\n",
          "\n",
          "#-------------------------------------------------------", "\n",
          "#-------- Create Snow Pillow Groups --------------------", "\n",
          "\n",
          ":HRUGroup ", paste(paste("SP", all.snow.pillows.included[i], sep = "_"), collapse = ","), "\n",
          HRU_ID, "\n",
          ":EndHRUGroup", "\n"
      )} else {
        cat(file = main.RVH.file, append = T, sep = "",
            ":HRUGroup ", paste(paste("SP", all.snow.pillows.included[i], sep = "_"), collapse = ","), "\n",
            HRU_ID, "\n",
            ":EndHRUGroup", "\n"
        ) 
      }
    
    
  } # End for loop of snow.pillows.included
} # End if exists("snow.pillows.included)

