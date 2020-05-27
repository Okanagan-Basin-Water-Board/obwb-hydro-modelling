############################################################################################################################
##
## This script generates individual *.rvh files for reservoirs included in a given model run
##
## Jun-19-2019
##
############################################################################################################################
## TODO: Update :MinStageConstraintDominant commands to only be written when manage.reservoirs == TRUE.
## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

require(readxl)
require(RavenR)

#################################################
##
## Read-in required datasets and variables
##
#################################################

## Read in the subbasin attribute table
subbasin.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  
## Check if there are reservoirs present in any of the include.watersheds. If so, create a "reservoirs" directory.
subbasins.present <- subbasin.codes[gsub( " .*$", "", subbasin.codes$GNIS_NAME) %in% include.watersheds,]

## Remove any subbasins that are included in the disable.subbasins string
subbasins.present <- subbasins.present[!subbasins.present$Subbasin_ID %in% disable.subbasins, ]

if(length(unique(subbasins.present$Reservoir_name)) > 1){dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs"))}


for(j in 1:length(include.watersheds)){
  
  ## Subset to isolate only the rows relevant to the watershed(s) of interest. gsub command removes the " Creek" from GNIS_NAME
  subbasin.subset <- subbasin.codes[gsub( " .*$", "", subbasin.codes$GNIS_NAME) %in% include.watersheds[j],]

  ## Identify all reservoirs/lakes within the watershed(s) of interest
  reservoirs <- unique(subbasin.subset$Reservoir_name)
  
  ## Remove "<Null>"
  reservoirs <- as.character(reservoirs[!reservoirs %in% "<Null>"])
  
  if(length(reservoirs) <1){print(paste("No reservoirs included within the", include.watersheds[j], "watershed"))
    } else {
  
    ## Read in the rvh file to identify HRU number associated with each reservoir
    main.HRU.file <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = ""))  
    
    HRUs <- rvh.read(main.HRU.file)
    
    HRUs <- HRUs$HRUtable
    
      ## Create a "reservoirs" folder to house individual *.rvh files for all reservoirs
    # dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs"))
    
    #################################################
    ##
    ## Loop over all reservoirs and generate individual *.rvh files for each reservoir
    ##
    #################################################
    
    ## For all unique reservoirs, read in the stage-storage and stage-are information
    for(i in 1:length(reservoirs)){
      
      tmp <- read_xlsx(file.path(global.input.dir, raw.reservoir.in.dir, reservoir.in.file), sheet = reservoirs[i])
      
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
      
      if("Future_Storage_dam3" %in% names(tmp)){
        
        ##-----------------------------------------------------------------------------
        ##
        ## Isolate the stage-storage curve for the given reservoir and convert volume from dam3 to m3
        ##
        ##-----------------------------------------------------------------------------
        
        ## Specify which storage types should be included
        tmp2 <- tmp[tmp$Storage_Type == "Zero" | tmp$Storage_Type == "Live" | tmp$Storage_Type == "Potential" | tmp$Storage_Type == "Dead", ]
        
        ## Convert volume to m3 as required by Raven
        tmp2$Future_Storage_m3 <- tmp2$Future_Storage_dam3 * 1000
        
        ##-----------------------------------------------------------------------------
        ##
        ## Check to see if Variable Stage relations are present for the given reservoir. This is determined by the presence of "Q1_start_julian" in the PARAMETER column.
        ## NOTE: Currently, only 2 different curves can be included.
        ## NOTE: This is only applicable if include.water.demand == TRUE (i.e., residual streamflows are being modelled); otherwise, the gate operations are not included and the reservoir simply fills and spills
        ##-----------------------------------------------------------------------------

        if("Q1_start_julian" %in% parameters$PARAMETER & include.water.demand == TRUE){
          
          ##TODO: Update this so that this reservoir is NOT available to support downstream demand - this should be captured by the variable resealse rules which represent "real" release rules
          
          variable.stage <- TRUE
          
          tmp2$Area_m2 <- HRUs[HRUs$SBID == SubBasinID, "Area"] * (1000*1000) ## Assume that the area is constant under all stage(s).
          
          StageStorage <- tmp2[, c("Stage_m_GSC", "Future_Storage_m3", "Area_m2", "Q1_m3s", "Q2_m3s")]
          
          # StageStorage[is.na(StageStorage)] <- -1.2345 # Cannot have blank values, so remove this command
          
          npoints <- nrow(StageStorage)
          
          julian.starts <- unlist(c(parameters[parameters$PARAMETER == "Q1_start_julian", "VALUE"], parameters[parameters$PARAMETER == "Q2_start_julian", "VALUE"]))

        } else {
          
          variable.stage <- FALSE
          
          ##-----------------------------------------------------------------------------
          ##
          ##  Extract the required columns for stage storage and determine the number of points (if VaryingStageRelations not present)
          ##
          ##-----------------------------------------------------------------------------
          
          StageStorage <- tmp2[ ,c("Stage_m_GSC", "Future_Storage_m3")]
          
          npoints <- nrow(StageStorage)
          
          ##-----------------------------------------------------------------------------
          ##
          ##  Extract the required columns for stage area - if successful, npoints will be the same as above, by definition
          ##
          ##-----------------------------------------------------------------------------
          
          ## Check if "Area_acres" exists, and it's the same length as stage (i.e., its a complete curve)
          if("Area_acres" %in% names(tmp2) & length(tmp2$Area_acres) == length(tmp2$Future_Storage_m3) & !anyNA(tmp2$Area_acres)){
            
            include.stage.area <- TRUE
            
            ## Convert Acres to m2, as required by Raven
            tmp2$Area_m2 <- tmp2$Area_acres * 4046.86
            
            StageArea <- tmp2[, c("Stage_m_GSC", "Area_m2")]
          
          } else {
            
            include.stage.area <- FALSE
            
          }
          
        }
      
        ##-----------------------------------------------------------------------------
        ##
        ## Determine information required to write reservoir *.rvt file to specify minimum stage
        ##
        ##-----------------------------------------------------------------------------

        invert.elevation <- tmp2[tmp2$Storage_Type == "Zero", "Stage_m_GSC"]
        
        start.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "StartDate", "DEFINITION"], format = "%m/%d/%Y")
        
        end.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "EndDate", "DEFINITION"], format = "%m/%d/%Y")
        
        model.period <- seq(start.date, end.date, by = "days")
        
        ##-----------------------------------------------------------------------------
        ##
        ## Extract the correspinding subbasin ID and HRU ID for the given reservoirs, as well as the lake area and Max Capacity (as maximum storage capacity from stage-storage curve)
        ##
        ##-----------------------------------------------------------------------------
        
        SubBasinID <- subbasin.subset[subbasin.subset$Reservoir_name == reservoirs[i], "Subbasin_ID"]
        
        HRUID <- HRUs[HRUs$SBID == SubBasinID, "ID"]
        
        LakeArea <- HRUs[HRUs$SBID == SubBasinID, "Area"] * (1000*1000)
        
        # Define the maximum capacity as the top of the live storage - the potential storage is not actually available at the moment.
        MaxCapacity <- max(tmp2[tmp$Storage_Type == "Live", "Future_Storage_m3"])
        
        ##-----------------------------------------------------------------------------
        ##
        ## Generate individual *.rvh files for all reservoirs within the model watershed
        ##
        ##-----------------------------------------------------------------------------
        
        ReservoirRVHoutFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs", paste(reservoirs[i], ".rvh", sep = ""))
        
        
        if(variable.stage == TRUE){
          
          if(i == 1){print("Minimum Stage Constraint has been made dominant to prevent reservoirs to be drawn down below the invert under residual streamflows.")}
          
          cat(file = ReservoirRVHoutFile, append = F, sep = "",
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
              ":MaxCapacity ", MaxCapacity, "\n",
              ":MinStageConstraintDominant", "\n", ## #NF3 - 22052020 - This command forces reservoirs to respect the minimum stage constraint. Variable stage is ONLY included when include.water.demand == TRUE, so no additional IF is needed here.
              ":Type RESROUTE_STANDARD", "\n",
              ":VaryingStageRelations", "\n",
              npoints, "\n",
              paste(julian.starts, collapse = " "), "\n"
              )
          
          write.table(StageStorage, ReservoirRVHoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F) 
          
          
          cat(file=ReservoirRVHoutFile, append=T, sep="",
              ":EndVaryingStageRelations", "\n",
              ":EndReservoir", "\n"
          )
          
          
        } else {
          
          if(include.water.demand == TRUE & i == 1){print("Minimum Stage Constraint has been made dominant to prevent reservoirs to be drawn down below the invert under residual streamflows.")}
          
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
              ":MaxCapacity ", MaxCapacity, "\n",
              if(include.water.demand == TRUE){paste(":MinStageConstraintDominant", "\n")}, ## #NF3 - 22052020 - This command forces reservoirs to respect the minimum stage constraint. It is ONLY included under residual conditions
              "\n",
              ":VolumeStageRelation LOOKUP_TABLE", "\n",
              npoints, " # number of points in curve", "\n"
          )
          
          write.table(StageStorage, ReservoirRVHoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F) 
          
          cat(file=ReservoirRVHoutFile, append=T, sep="",
              ":EndVolumeStageRelation", "\n")
          
          ## If StageArea was defined above (and therefore exists), write it to the reservoir block.
          if(include.stage.area == TRUE){
            cat(file = ReservoirRVHoutFile, append = T, sep = "",
            "\n",
            ":AreaStageRelation LOOKUP_TABLE", "\n",
            npoints, " # number of points in curve", "\n"
            )
            
            
            write.table(StageArea, ReservoirRVHoutFile, append = T, col.names = F, row.names = F, sep = ",", quote = F)
            
            cat(file = ReservoirRVHoutFile, append = T, sep = "",
                ":EndAreaStageRelation", "\n")
            
          }
          
          ## End the :Reservoir Block
          cat(file = ReservoirRVHoutFile, append = T, sep = "",
              ":EndReservoir", "\n"
          )
          
        }
        
        ##-----------------------------------------------------------------------------
        ##
        ## Generate individual *.rvt files for all reservoirs within the model watershed - this cotains the Minimum Stage timeseries
        ##
        ##-----------------------------------------------------------------------------
        ## UPDATE: Reservoir Minimum Stage Timeseries is now only written under residual conditions. Reservours "could" dry out under natural conditions.
        
        if(include.water.demand == TRUE){
          
          print(paste("Minimum Stage Contraint timeseries has been written for", reservoirs[i], "Reservoir at an elevation of:", invert.elevation))
        
          ReservoirRVToutFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs", paste(reservoirs[i], ".rvt", sep = ""))
          
          cat(file=ReservoirRVToutFile, append=F, sep="",
              
              "#########################################################################","\n",
              "##", "\n",
              "## Individual Reservoir RVT File. Redirected from main *.rvt file.", "\n",
              "#---------------------------------------------------------", "\n",
              "\n",
              ":ReservoirMinStage ", SubBasinID, "\n",
              sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(start.date)),length(model.period)), "\n"
              )
              
              write.table(paste(rep(invert.elevation, length(model.period)), collapse = "\n"), ReservoirRVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
              
              cat(file = ReservoirRVToutFile, append = T, sep = "",
                  ":EndReservoirMinStage", "\n"
                  )
        }
        
      } else { # End if "Future_Storage_dam3" is in the dataframe (i.e., whether a stage-storage curve is included.)
        
        ##-----------------------------------------------------------------------------
        ##
        ## Extract the correspinding subbasin ID and HRU ID for the given reservoirs, as well as the lake area and Max Capacity:
        ## - Max Capacity calculated as product of lake area * Max Depth
        ##
        ##-----------------------------------------------------------------------------
        
        SubBasinID <- subbasin.subset[subbasin.subset$Reservoir_name == reservoirs[i], "Subbasin_ID"]
        
        HRUID <- HRUs[HRUs$SBID == SubBasinID, "ID"]
        
        LakeArea <- HRUs[HRUs$SBID == SubBasinID, "Area"] * (1000*1000)
        
        ## Estimate MaxCapacity from LakeArea and MaxDepth
        MaxCapacity <- LakeArea * as.numeric(parameters$VALUE[parameters$PARAMETER == "MaxDepth"])
  
        ##-----------------------------------------------------------------------------
        ##
        ## Generate individual *.rvh files for all reservoirs within the model watershed
        ##
        ##-----------------------------------------------------------------------------
        
        ReservoirRVHoutFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs", paste(reservoirs[i], ".rvh", sep = ""))
        
        print(paste("No Minimum Stage Constraint can be written for", reservoir[i], "since no stage-storage information is available."))
        
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
            ":MaxCapacity ", MaxCapacity, "\n",
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
            paste("# Redirect to", include.watersheds[j], "Creek Reservoir Files"), "\n",
            "\n",
            ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvh", sep = ""), "\n"
        )} else {
          cat(file = main.HRU.file, append = T, sep = "",
              ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvh", sep = ""), "\n"
          ) 
        }
      
      
      ##-----------------------------------------------------------------------------
      ##
      ## Append :RedirectToFile command(s) to the main *.rvt file to point to individual reservoir files
      ##
      ## NOTE: This is only completed if a reservoir-specific *.rvt file was generated (use file.exists to check this)
      ##
      ##----------------------------------------------------------------------------- 
      if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs", paste(reservoirs[i], ".rvt", sep = "")))){
        if(i == 1){
          cat(file = main.RVT.file, append = T, sep = "",
               "\n",
               "#-------------------------------------------------------", "\n",
               paste("# Redirect to", include.watersheds[j], "Creek Reservoir Files"), "\n",
               "\n",
               ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvt", sep = ""), "\n"
          )} else {
            cat(file = main.RVT.file, append = T, sep = "",
                ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvt", sep = ""), "\n"
            )
          } # End else
      } # End If file exists
      
      ############################################################################################################################
      ##
      ## If *.rvh.tpl exists, write the redircet commands to that file too.
      ##
      ############################################################################################################################
      
      if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvh.tpl", sep = "")))){
        
        OstrichRVHTemplateFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvh.tpl", sep = ""))
        
        if(i == 1){
          cat(file = OstrichRVHTemplateFile, append = T, sep = "",
              "\n",
              "#-------------------------------------------------------", "\n",
              paste("# Redirect to", include.watersheds[j], "Creek Reservoir Files"), "\n",
              "\n",
              ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvh", sep = ""), "\n"
          )} else {
            cat(file = OstrichRVHTemplateFile, append = T, sep = "",
                ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvh", sep = ""), "\n"
            ) 
          }
      }
      
      ############################################################################################################################
      ##
      ## If *.rvt.tpl exists, write the redircet commands to that file too.
      ##
      ############################################################################################################################
      
      
      if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
      
        OstrichRVTTemplateFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
        
        if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs", paste(reservoirs[i], ".rvt", sep = "")))){
          if(i == 1){
            cat(file = OstrichRVTTemplateFile, append = T, sep = "",
                "\n",
                "#-------------------------------------------------------", "\n",
                paste("# Redirect to", include.watersheds[j], "Creek Reservoir Files"), "\n",
                "\n",
                ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvt", sep = ""), "\n"
            )} else {
              cat(file = OstrichRVTTemplateFile, append = T, sep = "",
                  ":RedirectToFile ", paste("reservoirs/", reservoirs[i], ".rvt", sep = ""), "\n"
              )
            } # End else
        } # End If Ostrich template exists
       } # End If reservoir rvt file exists
        
        
      ############################################################################################################################
      ##
      ## Generate Ostrich Template file for same conditions
      ##
      ############################################################################################################################
      
      
      if(run.ostrich == TRUE & calibrate.reservoirs == TRUE){
        
        # calibration.parameter.table <- na.omit(tmp[ ,c("PARAMETER", "VALUE", 'CAL_MIN', "CAL_MAX")])
        
        calibration.parameter.table <- tmp[!is.na(tmp$PARAMETER) ,c("PARAMETER", "VALUE", "CAL_MIN", "CAL_MAX")]
        
    
        
        # convert all columns to character
        calibration.parameter.table[,] <- lapply(calibration.parameter.table[, ], as.character)
        
        ## Create an empty column called CAL_VAR
        calibration.parameter.table$CAL_VAR <- NA
        
        calibration.parameter.table[is.na(calibration.parameter.table$CAL_MIN), "CAL_VAR"] <- calibration.parameter.table$VALUE[is.na(calibration.parameter.table$CAL_MIN)]
        
        calibration.parameter.table$CAL_VAR[which(!is.na(calibration.parameter.table$CAL_MAX))] <- paste(reservoirs[i], calibration.parameter.table$PARAMETER[which(!is.na(calibration.parameter.table$CAL_MAX))],sep = "_")
        
        ## Replace any spaces with underscores - this is needed for lakes with names longer than one word
        calibration.parameter.table$CAL_VAR[which(!is.na(calibration.parameter.table$CAL_MAX))] <- gsub('([[:punct:]])|\\s+','_',calibration.parameter.table$CAL_VAR[which(!is.na(calibration.parameter.table$CAL_MAX))])
        
        calibration.parameter.table$CAL_VAR[which(is.na(calibration.parameter.table$CAL_MAX))] <- as.character(calibration.parameter.table$VALUE[which(is.na(calibration.parameter.table$CAL_MAX))])  
        
        
        
        ##-----------------------------------------------------------------------------
        ##
        ## Generate *.rvh.tpl file
        ##
        ##-----------------------------------------------------------------------------
        
        OstrichReservoirRVHTemplateFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(reservoirs[i], ".rvh.tpl", sep = ""))
        
          ##-----------------------------------------------------------------------------
          ##
          ## Check if there is a stage-storage curve for the specified reservoir. If so, incude it, if not, add simple lake-like reservoir.
          ##
          ##-----------------------------------------------------------------------------
        
          if("Future_Storage_dam3" %in% names(tmp)){
            
            
            if(variable.stage == TRUE){
              
              cat(file = OstrichReservoirRVHTemplateFile, append = F, sep = "",
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
                  ":WeirCoefficient ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "WeirCoefficient"], "\n",
                  ":CrestWidth ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "CrestWidth"], "\n",
                  ":MaxDepth ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "MaxDepth"], "\n",
                  ":LakeArea ", LakeArea, "\n",
                  ":AbsoluteCrestHeight ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "AbsoluteCrestHeight"], "\n",
                  ":MaxCapacity ", MaxCapacity, "\n",
                  ":MinStageConstraintDominant", "\n", ## #NF3 - 22052020 - This command forces reservoirs to respect the minimum stage constraint. Variable stage is ONLY included when include.water.demand == TRUE, so no additional IF is needed here.
                  ":Type RESROUTE_STANDARD", "\n",
                  ":VaryingStageRelations", "\n",
                  npoints, "\n",
                  paste(julian.starts, collapse = " "), "\n"
              )
              
              write.table(StageStorage, OstrichReservoirRVHTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F) 
              
              
              cat(file=OstrichReservoirRVHTemplateFile, append=T, sep="",
                  ":EndVaryingStageRelations", "\n",
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
                ":AbsoluteCrestHeight ", calibration.parameter.table$CAL_VAR[calibration.parameter.table$PARAMETER == "AbsoluteCrestHeight"], "\n",
                ":MaxCapacity ", MaxCapacity, "\n",
                if(include.water.demand == TRUE){paste(":MinStageConstraintDominant", "\n")}, ## #NF3 - 22052020 - This command forces reservoirs to respect the minimum stage constraint. It is ONLY included under residual conditions
                "\n",
                ":VolumeStageRelation LOOKUP_TABLE", "\n",
                npoints, " # number of points in curve", "\n"
            )
            
            write.table(StageStorage, OstrichReservoirRVHTemplateFile, append = T, col.names = F, row.names = F, sep = ",", quote = F) 
            
            cat(file=OstrichReservoirRVHTemplateFile, append=T, sep="",
                ":EndVolumeStageRelation", "\n",
                ":EndReservoir", "\n"
            )
            }
          
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
                ":MaxCapacity ", MaxCapacity, "\n",
                ":EndReservoir", "\n"
            )
            
          }
        
        if(i == length(reservoirs)){print(paste(length(reservoirs), "required Ostrich template(s) generated for the", include.watersheds[j], " Creek watershed..."))}
      } # End if statement for running Ostrich
      
      
    } # end for loop
    
    print(paste(length(reservoirs), "reservoir(s) included within the", include.watersheds[j], "Creek watershed..."))
    
  } # end if / else
  
}# end include.watersheds for loop
