############################################################################################################################
##
## This script generates individual *.rvt files for snow pillows and snow courses within the model domain
##
## Jun-19-2019
##
############################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

## Read in the RVI template and identify the start and end dates. Generate a sequence of dates which will be used to extract relevant data only
RVI.template <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, RVI.template.in.file))

start.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "StartDate", "DEFINITION"], format = "%m/%d/%Y")

end.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "EndDate", "DEFINITION"], format = "%m/%d/%Y")

model.period <- seq(start.date, end.date, by = "days")


#######################################################
##
## Generate required *.rvt files for snow course sites
##
#######################################################

## Read in all snow course and snow pillow locations within the model domain
snow.course.locations <- read.csv(file.path(global.input.dir, processed.spatial.dir, snow.course.locations.processed.file))
  
## Add container for all snow courses included in all model watersheds. This is used in custom.appendages
all.snow.courses.included <- c()

for(j in 1:length(include.watersheds)){

  ## Identify which snow courses and snow pillows are located within the current watershed(s)
  watershed.snow.courses <- snow.course.locations$LCTN_ID[gsub( " .*$", "", snow.course.locations$GNIS_NAME) %in% include.watersheds[j]]
  
  ## Read in snow course and snow pillow data.
  snow.course.data <- read.csv(file.path(global.input.dir, raw.snow.in.dir, manual.snow.data.in.file))
    
  ## Extract SWE data for the stations within the watershed(s) of interest
  snow.course.SWE <- snow.course.data[snow.course.data$Number %in% watershed.snow.courses, c("Number", "Date.of.Survey", "Water.Equiv..mm")]
  
  ## Convert date to consistent tiso format
  snow.course.SWE$tiso <- as.POSIXct(snow.course.SWE$Date.of.Survey, format = "%Y/%m/%d")
  
  ## Isolate only the period for which model is being run
  snow.course.SWE.model <- snow.course.SWE[snow.course.SWE$tiso %in% model.period, ]
  
  ## Return which snow courses have data available for the period of interest. This may be different to 'watershed.snow.courses' if data falls outside the window of modelling
  snow.courses.included <- unique(snow.course.SWE.model$Number)
  
  ## Create a vector with all snow courses in the modelled watershed(s) - this is needed when there is more than one modelled watershed
  all.snow.courses.included <- c(all.snow.courses.included, as.character(snow.courses.included))
  
  if(length(snow.courses.included) > 0){
  
    ## Write snow course rvt files to file
    for(i in 1:length(snow.courses.included)){
      
      SnowRVToutFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste("SC_", snow.courses.included[i], ".rvt", sep = ""))
      
      HRU_ID <- snow.course.locations$HRU[snow.course.locations$LCTN_ID %in% snow.courses.included[i]]
      
      snow.course <- snow.course.SWE.model[snow.course.SWE.model$Number %in% snow.courses.included[i],]
      
      snow.course$tiso <- paste(snow.course$tiso, "00:00:00")
      
      cat(file = SnowRVToutFile, append = F, sep = "",
          ":IrregularObservations SNOW ", HRU_ID, " ", nrow(snow.course), " mm", " # Station ", as.character(snow.courses.included[i]),
          "\n"
      )
      
      write.table(snow.course[,c("tiso", "Water.Equiv..mm")], SnowRVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
      
      
      cat(file = SnowRVToutFile, append = T, sep = "",
          ":EndIrregularObservations",
          "\n"
      )
      
      
      ##----------------------------------------------
      ##
      ## Write Observation Weights for the calibration/validation period
      ##
      ##----------------------------------------------
      
      if(validate.model == FALSE){
        
        snow.course$weights <- ifelse(snow.course$tiso < base::as.Date(calibration.start) | snow.course$tiso > base::as.Date(calibration.end), 0, 1)
        
      } else {
        
        snow.course$weights <- ifelse(snow.course$tiso < base::as.Date(validation.start) | snow.course$tiso > base::as.Date(validation.end), 0, 1)
        
      }
      
      cat(file = SnowRVToutFile, sep = "", append = T,
          "\n",
          "# Write ObservationWeights", "\n",
          ":IrregularWeights SNOW ", HRU_ID, " ", nrow(snow.course), "\n"
      )
      
      write.table(snow.course[,c("tiso", "weights")], SnowRVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
      
      cat(file = SnowRVToutFile, sep = "", append = T,
          ":EndIrregularWeights", "\n"
      )
      
      ## Append :RedirectToFile command to end of main *.rvt file
      main.RVT.file <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvt", sep = ""))  
      
      if(i == 1){
        cat(file = main.RVT.file, append = T, sep = "",
            "\n",
            "#-------------------------------------------------------", "\n",
            paste("# Redirect to", include.watersheds[j], "Creek Snow Course Files"), "\n",
            "\n",
            ":RedirectToFile ", paste("SC_", snow.courses.included[i], ".rvt", sep = ""), "\n"
        )} else {
          cat(file = main.RVT.file, append = T, sep = "",
              ":RedirectToFile ", paste("SC_", snow.courses.included[i], ".rvt", sep = ""), "\n"
          ) 
        }
      
      
      ##----------------------------------------------------------------
      ## If run.ostrich == TRUE, add redircts to the template file too
      ##----------------------------------------------------------------
      
      if(run.ostrich == TRUE & file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
        
      ## Add :RedirectToFile commands to the end of the rvt.tpl file to match the structure of the master *.rvt file.
      OstrichRVTTemplateFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
      
      if(i == 1){
        cat(file = OstrichRVTTemplateFile, append = T, sep = "",
            "\n",
            "#-------------------------------------------------------", "\n",
            paste("# Redirect to", include.watersheds[j], "Creek Snow Course Files"), "\n",
            "\n",
            ":RedirectToFile ", paste("SC_", snow.courses.included[i], ".rvt", sep = ""), "\n"
        )} else {
          cat(file = OstrichRVTTemplateFile, append = T, sep = "",
              ":RedirectToFile ", paste("SC_", snow.courses.included[i], ".rvt", sep = ""), "\n"
          ) 
        }
      }
    }
    
    print(paste(length(snow.courses.included), "snow course(s) included in the", include.watersheds[j], "Creek watershed..."))
  
  } else {
    
    print(paste("No snow courses with overlapping data records are located within the", include.watersheds[j], "Creek watershed..."))
    
  }
  
}
#######################################################
##
## Generate required *.rvt files for snow pillow stations
##
#######################################################

snow.pillow.locations <- read.csv(file.path(global.input.dir, processed.spatial.dir, snow.pillow.locations.processed.file))
  
## Add container for all snow pillows included in all model watersheds. This is used in custom.appendages
all.snow.pillows.included <- c()

for(j in 1:length(include.watersheds)){

  watershed.snow.pillows <- snow.pillow.locations$LCTN_ID[gsub( " .*$", "", snow.pillow.locations$GNIS_NAME) %in% include.watersheds[j]]
  
  if(length(watershed.snow.pillows) > 0){
  
    snow.pillow.data <- read.csv(file.path(global.input.dir, raw.snow.in.dir, automated.snow.data.in.file))
      
    ## Reasign column names in snow pillow data to be only the station ID
    snow.pillow.data.columns <- strsplit(colnames(snow.pillow.data), ".", fixed = T)
    
    colnames(snow.pillow.data) <- lapply(snow.pillow.data.columns, `[[`, 1)
    
    ## Extract SWE data for the stations within the watershed(s) of interest
    snow.pillow.SWE <- snow.pillow.data[,c("DATE", paste("X", as.character(watershed.snow.pillows), sep = ""))]
    
    ## Convert date to consistent tiso format
    snow.pillow.SWE$tiso <- as.POSIXct(format(as.POSIXct(snow.pillow.SWE$DATE, format = "%Y-%m-%d %H:%M"), format = "%Y-%m-%d"), format = "%Y-%m-%d")
    
    ## Select only data that falls within the model period
    snow.pillow.SWE.model <- snow.pillow.SWE[snow.pillow.SWE$tiso %in% model.period, ]
    
    ## remove any columns which are all NA (i.e., no data is available for the model period)
    snow.pillow.SWE.model <- snow.pillow.SWE.model[,colSums(is.na(snow.pillow.SWE.model))<nrow(snow.pillow.SWE.model)]
    
    ## Return a listing of all stations which exist (i.e., had data available for the model period)
    snow.pillows.included <- colnames(snow.pillow.SWE.model)[colnames(snow.pillow.SWE.model)!= "DATE" & colnames(snow.pillow.SWE.model)!= "tiso"]
    
    ## Create a vector with all snow pillows in the modelled watershed(s) - this is needed when there is more than one modelled watershed
    all.snow.pillows.included <- c(all.snow.pillows.included, as.character(snow.pillows.included))
    
    if(length(snow.pillows.included) > 0){
      
      ## Write snow course rvt files to file
      for(i in 1:length(snow.pillows.included)){
        
        station.name <- sub('.', '', snow.pillows.included[i])
        
        snow.pillow <- snow.pillow.SWE.model[, c("DATE", snow.pillows.included[i])]
        
        ## Replace NA with Raven missing value code
        snow.pillow[is.na(snow.pillow[, snow.pillows.included[i]]), snow.pillows.included[i]] <- -1.2345
        
        snow.pillow$DATE <- as.POSIXct(snow.pillow$DATE, format = "%Y-%m-%d %H:%M")
        
        SnowRVToutFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste("SP_", station.name, ".rvt", sep = ""))
        
        HRU_ID <- snow.pillow.locations$HRU[snow.pillow.locations$LCTN_ID %in% station.name]
        
        cat(file = SnowRVToutFile, append = F, sep = "",
            ":IrregularObservations SNOW ", HRU_ID, " ", nrow(snow.pillow), " mm", " # Station ", station.name,
            "\n"
        )
        
        write.table(snow.pillow, SnowRVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
        
        
        cat(file = SnowRVToutFile, append = T, sep = "",
            ":EndIrregularObservations",
            "\n"
        )
        
        ##----------------------------------------------
        ##
        ## Write Observation Weights for the calibration/validation period
        ##
        ##----------------------------------------------
        
        if(validate.model == FALSE){
          
          snow.pillow$weights <- ifelse(base::as.Date(snow.pillow$DATE) < base::as.Date(calibration.start) | base::as.Date(snow.pillow$DATE) > base::as.Date(calibration.end), 0, 1)
          
        } else {
          
          snow.pillow$weights <- ifelse(base::as.Date(snow.pillow$DATE) < base::as.Date(validation.start) | base::as.Date(snow.pillow$DATE) > base::as.Date(validation.end), 0, 1)
          
        }
        
        cat(file = SnowRVToutFile, sep = "", append = T,
            "\n",
            "# Write ObservationWeights", "\n",
            ":IrregularWeights SNOW ", HRU_ID, " ", nrow(snow.pillow), "\n"
        )
        
        write.table(snow.pillow[,c("DATE", "weights")], SnowRVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
        
        cat(file = SnowRVToutFile, sep = "", append = T,
            ":EndIrregularWeights", "\n"
        )
        
        
        ## Append :RedirectToFile command to end of main *.rvt file
        main.RVT.file <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvt", sep = ""))  
        
        if(i == 1){
          cat(file = main.RVT.file, append = T, sep = "",
              "\n",
              "#-------------------------------------------------------", "\n",
              paste("# Redirect to", include.watersheds[j], "Creek Snow Pillow Files"), "\n",
              "\n",
              ":RedirectToFile ", paste("SP_", station.name, ".rvt", sep = ""), "\n"
          )} else {
            cat(file = main.RVT.file, append = T, sep = "",
                ":RedirectToFile ", paste("SP_", station.name, ".rvt", sep = ""), "\n"
            ) 
          }
        
        ##----------------------------------------------------------------
        ## If run.ostrich == TRUE, add redircts to the template file too
        ##----------------------------------------------------------------
        
        if(run.ostrich == TRUE & file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
          
          
        OstrichRVTTemplateFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
          
        ## Add :RedirectToFile commands to the end of the rvt.tpl file to match the structure of the master *.rvt file.
        if(i == 1){
          cat(file = OstrichRVTTemplateFile, append = T, sep = "",
              "\n",
              "#-------------------------------------------------------", "\n",
              paste("# Redirect to", include.watersheds[j], "Creek Snow Pillow Files"), "\n",
              "\n",
              ":RedirectToFile ", paste("SP_", station.name, ".rvt", sep = ""), "\n"
          )} else {
            cat(file = OstrichRVTTemplateFile, append = T, sep = "",
                ":RedirectToFile ", paste("SP_", station.name, ".rvt", sep = ""), "\n"
            ) 
          }
        }
        
      }
      
      print(paste(length(snow.pillows.included), "snow pillows included in the", include.watersheds[j], "Creek watershed..."))
      
    } else {
      
      print(paste("No snow pillows with overlapping data records are located within the", include.watersheds[j], "Creek watershed..."))
      
    }
    
    
  } else {
      
      print(paste("No snow pillows with overlapping data records are located within the", include.watersheds[j], "Creek watershed..."))
    
  }

}