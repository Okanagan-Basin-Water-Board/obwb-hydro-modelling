############################################################################################################################
##
## This script generates individual *.rvh files for reservoirs included in a given model run
##
## Jun-19-2019
##
############################################################################################################################

require(readxl)
require(RavenR)

#################################################
##
## Read-in required datasets and variables
##
#################################################

## Read in the subbasin attribute table
subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

## Subset to isolate only the rows relevant to the watershed(s) of interest. gsub command removes the " Creek" from GNIS_NAME
subbasin.subset <- subbasin.codes[gsub( " .*$", "", subbasin.codes$GNIS_NAME) %in% include.watersheds,]

## Identify all reservoirs/lakes within the watershed(s) of interest
reservoirs <- unique(subbasin.subset$Reservoir_name)

## Remove "<Null>"
reservoirs <- as.character(reservoirs[!reservoirs %in% "<Null>"])

if(length(reservoirs) <1){print(paste("No reservoirs included within the", include.watersheds, "watershed(s)"))
  } else {

  ## Read in the rvh file to identify HRU number associated with each reservoir
  main.HRU.file <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = ""))  
  
  HRUs <- rvh.read(main.HRU.file)
  
  HRUs <- HRUs$HRUtable
  
    ## Create a "reservoirs" folder to house individual *.rvh files for all reservoirs
  dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs"))
  
  #################################################
  ##
  ## Loop over all reservoirs and generate individual *.rvh files for each reservoir
  ##
  #################################################
  
  ## For all unique reservoirs, read in the stage-storage and stage-are information
  for(i in 1:length(reservoirs)){
    
    tmp <- read_xlsx("/var/obwb-hydro-modelling/input-data/raw/reservoirs/raven-reservoirs.xlsx", sheet = reservoirs[i])
    
    ##-----------------------------------------------------------------------------
    ##
    ## Extract required weir information and lake depth from tmp
    ##
    ##-----------------------------------------------------------------------------
    
    parameters <- na.omit(tmp[ ,c("PARAMETER", "VALUE")])
    
    
    ##-----------------------------------------------------------------------------
    ##
    ## Check if there is a stage-storage curve for the specified reservoir. If so, incude it, if not, add simple lake-like reservoir.
    ##
    ##-----------------------------------------------------------------------------
    
    if("Current_Storage_dam3" %in% names(tmp)){
    
      ##-----------------------------------------------------------------------------
      ##
      ## Isolate the stage-storage curve for the given reservoir and identify the number of points
      ##
      ##-----------------------------------------------------------------------------
      
      ## Remove "Dead" Storage from the curve, but include zero storage
      tmp2 <- tmp[tmp$Storage_Type == "Zero" | tmp$Storage_Type == "Live" | tmp$Storage_Type == "Potential", ]
      
      ## Convert volume to m3 as required by Raven
      tmp2$Current_Storage_m3 <- tmp2$Current_Storage_dam3 * 1000
      
      StageStorage <- tmp2[ ,c("Stage_m_GSC", "Current_Storage_m3")]
      
      npoints <- nrow(StageStorage)
      
      ##-----------------------------------------------------------------------------
      ##
      ## Extract the correspinding subbasin ID and HRU ID for the given reservoirs, as well as the lake area
      ##
      ##-----------------------------------------------------------------------------
      
      SubBasinID <- subbasin.subset[subbasin.subset$Reservoir_name == reservoirs[i], "Subbasin_ID"]
      
      HRUID <- HRUs[HRUs$SBID == SubBasinID, "ID"]
      
      LakeArea <- HRUs[HRUs$SBID == SubBasinID, "Area"] * (1000*1000)
      
      ##-----------------------------------------------------------------------------
      ##
      ## Generate individual *.rvh files for all reservoirs within the model watershed
      ##
      ##-----------------------------------------------------------------------------
      
      ReservoirRVHoutFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs", paste(reservoirs[i], ".rvh", sep = ""))
      
      cat(file=ReservoirRVHoutFile, append=F, sep="",
          
          "#########################################################################","\n",
          "##", "\n",
          "## Individual Reservoir RVH file. Redirected from Master.rvh file","\n",
          ":Application       R","\n",
          ":WrittenBy         Lawrence Bird","\n",
          ":CreationDate  ",    paste(Sys.time()),"\n",
          "#---------------------------------------------------------", "\n",
          "\n",
          ":Reservoir ", as.character(reservoirs[i]), "\n",
          ":SubBasinID ", SubBasinID, "\n",
          ":HRUID ", HRUID, "\n",
          ":WeirCoefficient ", parameters$VALUE[parameters$PARAMETER == "WeirCoefficient"], "\n",
          ":CrestWidth ", parameters$VALUE[parameters$PARAMETER == "CrestWidth"], "\n",
          ":MaxDepth ", parameters$VALUE[parameters$PARAMETER == "MaxDepth"], "\n",
          ":LakeArea ", LakeArea, "\n",
          ":AbsoluteCrestHeight ", parameters$VALUE[parameters$PARAMETER == "AbsoluteCrestHeight"], "\n",
          "\n",
          ":VolumeStageRelation LOOKUP_TABLE", "\n",
          npoints, " # number of points in curve", "\n"
      )
      
      write.table(StageStorage, ReservoirRVHoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F) 
      
      cat(file=ReservoirRVHoutFile, append=T, sep="",
          ":EndVolumeStageRelation", "\n",
          ":EndReservoir", "\n"
      )
    
    } else {
      
      ##-----------------------------------------------------------------------------
      ##
      ## Extract the correspinding subbasin ID and HRU ID for the given reservoirs, as well as the lake area
      ##
      ##-----------------------------------------------------------------------------
      
      SubBasinID <- subbasin.subset[subbasin.subset$Reservoir_name == reservoirs[i], "Subbasin_ID"]
      
      HRUID <- HRUs[HRUs$SBID == SubBasinID, "ID"]
      
      LakeArea <- HRUs[HRUs$SBID == SubBasinID, "Area"] * (1000*1000)

      ##-----------------------------------------------------------------------------
      ##
      ## Generate individual *.rvh files for all reservoirs within the model watershed
      ##
      ##-----------------------------------------------------------------------------
      
      ReservoirRVHoutFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs", paste(reservoirs[i], ".rvh", sep = ""))
      
      cat(file=ReservoirRVHoutFile, append=F, sep="",
          
          "#########################################################################","\n",
          "##", "\n",
          "## Individual Reservoir RVH file. Redirected from Master.rvh file","\n",
          ":Application       R","\n",
          ":WrittenBy         Lawrence Bird","\n",
          ":CreationDate  ",    paste(Sys.time()),"\n",
          "#---------------------------------------------------------", "\n",
          "\n",
          ":Reservoir ", as.character(reservoirs[i]), "\n",
          ":SubBasinID ", SubBasinID, "\n",
          ":HRUID ", HRUID, "\n",
          ":WeirCoefficient ", parameters$VALUE[parameters$PARAMETER == "WeirCoefficient"], "\n",
          ":CrestWidth ", parameters$VALUE[parameters$PARAMETER == "CrestWidth"], "\n",
          ":MaxDepth ", parameters$VALUE[parameters$PARAMETER == "MaxDepth"], "\n",
          ":LakeArea ", LakeArea, "\n",
          ":EndReservoir", "\n"
          )
      
    }
    
    ##-----------------------------------------------------------------------------
    ##
    ## Append :RedirectToFile command(s) to the main *.rvh file to point to individual reservoir files
    ##
    ##-----------------------------------------------------------------------------
    
    if(i == 1){
      cat(file = main.HRU.file, append = T, sep = "",
          "\n",
          "#-------------------------------------------------------", "\n",
          "# Redirect to Reservoir Files", "\n",
          "\n",
          ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvh", sep = ""), "\n"
      )} else {
        cat(file = main.HRU.file, append = T, sep = "",
            ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvh", sep = ""), "\n"
        ) 
      }
    
    
    ############################################################################################################################
    ##
    ## Generate Ostrich Template file for same conditions
    ##
    ############################################################################################################################
    
    
    if(run.ostrich == TRUE){
      
      calibration.parameter.table <- na.omit(tmp[ ,c("PARAMETER", "VALUE", 'CAL_MIN', "CAL_MAX")])
      
      ## convert all columns to character
      calibration.parameter.table[,] <- lapply(calibration.parameter.table[, ], as.character)
      
      ## Create an empty column called CAL_VAR
      calibration.parameter.table$CAL_VAR <- NA
      
      calibration.parameter.table$CAL_VAR[which(!is.na(calibration.parameter.table$CAL_MAX))] <- paste(reservoirs[i], calibration.parameter.table$PARAMETER[which(!is.na(calibration.parameter.table$CAL_MAX))],sep = "_")
      
      ## Replace any spaces with underscores - this is needed for lakes with names longer than one word
      calibration.parameter.table$CAL_VAR[which(!is.na(calibration.parameter.table$CAL_MAX))] <- gsub('([[:punct:]])|\\s+','_',calibration.parameter.table$CAL_VAR[which(!is.na(calibration.parameter.table$CAL_MAX))])
      
      calibration.parameter.table$CAL_VAR[which(is.na(calibration.parameter.table$CAL_MAX))] <- as.character(calibration.parameter.table$VALUE[which(is.na(calibration.parameter.table$CAL_MAX))])  
      
      
      
      ##-----------------------------------------------------------------------------
      ##
      ## Generate *.rvh.tpl file
      ##
      ##-----------------------------------------------------------------------------
      
      OstrichReservoirRVHTemplateFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(reservoirs[i], ".rvh.tpl", sep = ""))
      
        ##-----------------------------------------------------------------------------
        ##
        ## Check if there is a stage-storage curve for the specified reservoir. If so, incude it, if not, add simple lake-like reservoir.
        ##
        ##-----------------------------------------------------------------------------
      
        if("Current_Storage_dam3" %in% names(tmp)){
        
          cat(file=OstrichReservoirRVHTemplateFile, append=F, sep="",
              
              "#########################################################################","\n",
              "##", "\n",
              "## Individual Ostrich Template Reservoir RVH file.","\n",
              ":Application       R","\n",
              ":WrittenBy         Lawrence Bird","\n",
              ":CreationDate  ",    paste(Sys.time()),"\n",
              "#---------------------------------------------------------", "\n",
              "\n",
              ":Reservoir ", as.character(reservoirs[i]), "\n",
              ":SubBasinID ", SubBasinID, "\n",
              ":HRUID ", HRUID, "\n",
              ":WeirCoefficient ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "WeirCoefficient"], "\n",
              ":CrestWidth ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "CrestWidth"], "\n",
              ":MaxDepth ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "MaxDepth"], "\n",
              ":LakeArea ", LakeArea, "\n",
              ":AbsoluteCrestHeight ", parameters$VALUE[parameters$PARAMETER == "AbsoluteCrestHeight"], "\n",
              "\n",
              ":VolumeStageRelation LOOKUP_TABLE", "\n",
              npoints, " # number of points in curve", "\n"
          )
          
          write.table(StageStorage, OstrichReservoirRVHTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F) 
          
          cat(file=OstrichReservoirRVHTemplateFile, append=T, sep="",
              ":EndVolumeStageRelation", "\n",
              ":EndReservoir", "\n"
          )
        
        } else {
          
          cat(file=OstrichReservoirRVHTemplateFile, append=F, sep="",
              
              "#########################################################################","\n",
              "##", "\n",
              "## Individual Ostrich Template Reservoir RVH file.","\n",
              ":Application       R","\n",
              ":WrittenBy         Lawrence Bird","\n",
              ":CreationDate  ",    paste(Sys.time()),"\n",
              "#---------------------------------------------------------", "\n",
              "\n",
              ":Reservoir ", as.character(reservoirs[i]), "\n",
              ":SubBasinID ", SubBasinID, "\n",
              ":HRUID ", HRUID, "\n",
              ":WeirCoefficient ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "WeirCoefficient"], "\n",
              ":CrestWidth ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "CrestWidth"], "\n",
              ":MaxDepth ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "MaxDepth"], "\n",
              ":LakeArea ", LakeArea, "\n",
              ":EndReservoir", "\n"
          )
          
        }
      
      if(i == length(reservoirs)){print(paste(length(reservoirs), "required Ostrich template(s) generated..."))}
    } # End if statement for running Ostrich
    
    
  } # end for loop
  
  print(paste(length(reservoirs), "reservoir(s) included within the", include.watersheds, "watershed(s)"))
  
} # end if / else


