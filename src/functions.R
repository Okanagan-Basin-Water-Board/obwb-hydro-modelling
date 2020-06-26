## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

# function to find mode.
# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# function to distribute percentage values that don't quite sum to 100%
round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(is.na(rsum)) {res <- 0
  } else {
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
  }
  }

# function to obtain WSC data from HYDAT and generate required rvt file(s).
ECflow.rvt.tidy.single.obs <- function(ff,master,dir,include.watersheds,run.number,calibration.start,calibration.end,prd=NULL,stnNames=NULL,write.redirect=F,flip.number=F) {
  
  # data checks
  if (!(is.null(stnNames)) & (length(unique(master$Subbasin_ID)) != length(stnNames))) {
    stop("Length of subIDs must be the same as stnNames.")
  }
  
  # PARAMETERS
  # param (flow) == 1
  # param (level) == 2
  
  # SYMBOLS - not currently reported
  #
  
  # determine period ----
  # determine the period to use
  if (!(is.null(prd))) {
    
    # period is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd,"/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
        || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
  }
  
  # # check the stations in the supplied file
  # dd <- as.data.frame(ff)
  # 
  # # fix to handle multi-byte marker/byte order marker,
  # #   appears if there is more than one station per file
  # stns <- as.character(unique(iconv(dd$STATION_NUMBER,to="ASCII")))
  # stns <- stns[!(is.na(stns))]
  # if (length(stns) != length(subIDs)) {
  #   stop(sprintf("Number of stations found in file not equal to the length of subIDs or stnNames, please check the
  #                supplied file and function inputs. Found %i stations, %s",length(stns),toString(paste(stns))))
  # }
  
  # check the stations in the supplied file
  dd <- as.data.frame(ff)
  
  # iterate the watersheds in the master table and create a redirect file for each watershed
  
  fc.redirect <- file(file.path(dir, paste(ws.interest, "-", run.number, ".rvt", sep = '')), open = "a+")
  
  stations <- unique(dd$STATION_NUMBER)
  
  ## create a timeseries of correct dates - this will be used to merge dates/data from HYDAT to.
  dates <- data.frame(Date = seq(base::as.Date(start.date), base::as.Date(end.date), by = "day"))

  for(j in 1:length(stations)){
    
    dd.sub <- dd[which(dd$STATION_NUMBER %in% stations[j]),]
    
    dd.sub <- merge(dates, dd.sub, by.all = "Date", all.x = T)
    
    subID <- master$Subbasin_ID[which(master$Hydrometric_stn == stations[j])]
    
    # fix to handle multi-byte marker/byte order marker,
    #   appears if there is more than one station per file
    # stns <- as.character(unique(iconv(dd.sub$STATION_NUMBER,to="ASCII")))
    stations[j] <- stations[j][!(is.na(stations[j]))]
    
    if (length(stations[j]) != length(subID)) {
      stop(sprintf("Number of stations found in file not equal to the length of subIDs or stnNames, please check the
                   supplied file and function inputs. Found %i stations, %s",length(stns),toString(paste(stns))))
    }
    
    
    # dd.temp <- dd.sub[(dd.sub$STATION_NUMBER == stations[j] & dd.sub$Parameter == "Flow"),]
    # date.temp <- as.Date(dd.temp$Date,format="%Y/%m/%d")
    ts.temp <- xts(order.by=base::as.Date(dd.sub$Date,format="%Y-%m-%d"),x=dd.sub$Value)
    if (!(is.null(prd))) {
      ts.temp <- ts.temp[prd]
    }
    # change all NA values to Raven NA (-1.2345)
    ts.temp[is.na(ts.temp)] = -1.2345
    # check for empty time series
    if (nrow(ts.temp)==0) {
      close(fc.redirect)
      stop(sprintf("Empty time series for station %s, check the supplied period and/or the availability of flow data in the supplied file.",stns[i]))
    }
    
    
    # write .rvt file
    if (flip.number) {
      if (!(is.null(stnNames))) {
        rvt.name <- sprintf('%i_%s.rvt',subID,stations[j])
      } else {
        rvt.name <- sprintf('%i_%s.rvt',subID,stations[j])
      }
    } else {
      if (!(is.null(stnNames))) {
        rvt.name <- sprintf('%s_%i.rvt',stations[j],subID)
      } else {
        rvt.name <- sprintf('%s_%i.rvt',stations[j],subID)
      }
    }
    
    fc <- file(file.path(dir, rvt.name),open='w+')
    writeLines(sprintf(':ObservationData HYDROGRAPH %i m3/s # %s',subID,rvt.name),fc)
    writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(ts.temp[1])),nrow(ts.temp)),fc)
    
    for (k in 1:nrow(ts.temp)) {
      writeLines(sprintf('%g',ts.temp[k]),fc)
    }
    
    writeLines(':EndObservationData',fc)
    
    
    
    ###################################################################################################
    ##
    ## Calculate Observation Weights
    ###################################################################################################
    
    ## ---------------------------------------------------------
    ##
    ## If model validation is not being completed, generate observation weights based on calibration dates, and pad remaining days with 0 (i.e., simply observations)
    ##
    ## ---------------------------------------------------------
    
    if(validate.model == FALSE){
    
      spinup.start <- as.character(lubridate::date(ts.temp[1]))
      
      spinup.end <- as.character(lubridate::date(calibration.start)-1)
      
      spinup.weights <- ts.temp[paste(spinup.start, spinup.end, sep = "/")]
      
      coredata(spinup.weights) <- rep(0, length(spinup.weights))
      
      calibration.weights <- ts.temp[paste(calibration.start, calibration.end, sep = "/")]
      
      coredata(calibration.weights) <- rep(1, length(calibration.weights))
      
      obs.start <- as.character(lubridate::date(calibration.end)+1)
      
      obs.end <- as.character(lubridate::date(ts.temp[length(ts.temp)]))
      
      obs.weights <- ts.temp[paste(obs.start, obs.end, sep = "/")]
      
      coredata(obs.weights) <- rep(0, length(obs.weights))
      
      
      observation.weights.table <- rbind(spinup.weights, calibration.weights, obs.weights)
      
      ## Write Observation Weights to File
      writeLines(sprintf(':ObservationWeights HYDROGRAPH %i m3/s # %s',subID,rvt.name),fc)
      writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(observation.weights.table[1])),nrow(observation.weights.table)),fc)
      
      for (k in 1:nrow(observation.weights.table)) {
        writeLines(sprintf('%g',observation.weights.table[k]),fc)
      }
      
      writeLines(':EndObservationWeights',fc)
      
  
      ## Close file connection    
      close(fc)
    
    }
    
    ## ---------------------------------------------------------
    ##
    ## If model validation is being completed, generate observation weights based on validation dates, and pad remaining days with 0 (i.e., simply observations)
    ##
    ## ---------------------------------------------------------
    
    if(validate.model == TRUE){
      
      spinup.start <- as.character(lubridate::date(ts.temp[1]))
      
      spinup.end <- as.character(lubridate::date(calibration.start)-1)
      
      spinup.weights <- ts.temp[paste(spinup.start, spinup.end, sep = "/")]
      
      coredata(spinup.weights) <- rep(0, length(spinup.weights))
      
      calibration.weights <- ts.temp[paste(calibration.start, calibration.end, sep = "/")]
      
      coredata(calibration.weights) <- rep(0, length(calibration.weights))
      
      validation.weights <- ts.temp[paste(validation.start, validation.end, sep = "/")]
      
      coredata(validation.weights) <- rep(1, length(validation.weights))
      
      
      obs.start <- as.character(lubridate::date(validation.end)+1)
      
      obs.end <- as.character(lubridate::date(ts.temp[length(ts.temp)]))
      
      obs.weights <- ts.temp[paste(obs.start, obs.end, sep = "/")]
      
      coredata(obs.weights) <- rep(0, length(obs.weights))
      
      
      observation.weights.table <- rbind(spinup.weights, calibration.weights, validation.weights, obs.weights)
      
      ## Write Observation Weights to File
      writeLines(sprintf(':ObservationWeights HYDROGRAPH %i m3/s # %s',subID,rvt.name),fc)
      writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(observation.weights.table[1])),nrow(observation.weights.table)),fc)
      
      for (k in 1:nrow(observation.weights.table)) {
        writeLines(sprintf('%g',observation.weights.table[k]),fc)
      }
      
      writeLines(':EndObservationWeights',fc)
      
      
      ## Close file connection    
      close(fc)
      
    }
    
    
    # write to support file
    if (write.redirect) {
      writeLines(sprintf(':RedirectToFile %s',rvt.name),fc.redirect)
    }
    
    
    
    print(j)
    
    }
  
  if (write.redirect) {
    close(fc.redirect)
  }
  
  return(TRUE)
}

# Generate Year/Week timeseries which is consistent with the OWDM approach 
# i.e., 8 day last week of year, and 8 day Feb 29. week in leap years). 
make.AWDM.weeks <- function(start.date = "1996-01-01", end.date = "2010-12-31", weeks.wanted = c(1:52)){

  ## Generate a sequence of years which matches the naturalized streamflow datasets (i.e, 1996-2010)
  Years <- seq(as.numeric(substr(start.date, 1, 4)), as.numeric(substr(end.date, 1, 4)), 1)
  
  ## Generate Months sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
  Months <- seq(base::as.Date(start.date), base::as.Date(end.date), by = "month")
  
  ## Generate a sequence of weeks which matches the naturalized streamflow datasets (i.e., 1-52)
  Weeks <- paste("Week", seq(1, 52, 1))
  
  ## Generate Days sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
  Days <- seq(base::as.Date(start.date), base::as.Date(end.date), by = "day")
  
  ## Develop a vector of 365 days to represent a "Regular Year", broken into weeks which match the OWDM model setup (i.e., 8-day last week)
  RegularYear <- c(rep(1:51, each = 7), rep(52, each = 8))
  
  ## Develop a vector of 366 days to represent a "Leap Year", broken into weeks which match the OWDM model setup (i.e., 8-day Feb 29. and last week)
  LeapYear <- c(rep(1:8, each = 7), rep(9, each = 8), rep(10:51, each = 7), rep(52, each = 8))
  
  ## Generate a timeseries of years between start and end dates
  Year.timeseries <- data.frame()
  
  for(i in 1:length(Years)){
    ifelse(lubridate::leap_year(Years[i]) == T,
           Y <- rep(Years[i], each = 366),
           Y <- rep(Years[i], each = 365))
    Y <- data.frame(Y)
    Year.timeseries <- rbind(Year.timeseries, Y)
  }

  # Attach a timeseries of weeks to the Year.timeseries and save the combined as "Output"
  Times <- data.frame()
  
  for(i in 1:length(Years)){
    Z <- as.data.frame(Year.timeseries[Year.timeseries$Y == Years[i],])
    ifelse(lubridate::leap_year(Years[i]) == T, Z$Week <- LeapYear, Z$Week <- RegularYear)
    
    Times <- rbind(Times,Z)
  }
  
  colnames(Times) <- c("Year", "Week")
  
  # Add date column - this is used to merge dates with Raven Output
  Times$date <- Days

  # Filter to keep only the weeks wanted
  Times <- Times[which(Times$Week %in% weeks.wanted), ]
   
  return(Times)
}

# function to plot model run results
plot.results <- function(ws.interest, run.number, subbasins.present){
  
  ###############################
  ##
  ## Plot Modelled Streamflows
  ##
  ###############################
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))){
  
    ## Read-in modelled hydrographs
    hydrographs <- hyd.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
    
    # adjust the date index of the hydrograph xts object so that the observed streamflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    index(hydrographs$hyd) <- index(hydrographs$hyd) - 86400
    
    ## Identify which columns have obsrved data available
    subs.obs <- colnames(hydrographs$hyd[,hydrographs$obs.flag == TRUE])
    
    if(length(subs.obs) >0){
      
      ## Remove NA
      subs.obs <- subs.obs[!is.na(subs.obs)]
      
      ## remove the "_obs" characters to allow successful extraction
      my.subs <- gsub("_obs", "", subs.obs)
      
      par(mfrow = c(1,1))
      
      ## Generate a plot for all subbasins which have observed data available
      for(i in 1:length(my.subs)){
        x <- hyd.extract(subs = my.subs[i], hydrographs)
        hyd.plot(x$sim, x$obs, precip = hydrographs$hyd$precip)
        title(my.subs[i])
      }
      
      ## identofy the subbasin that drains to the mouth of the creek (i.e., downstream id = -1)
      mouth.ID <- subbasins.present$Subbasin_ID[subbasins.present$Downstream_ID == "-1"] 
      
      for(i in 1:length(mouth.ID)){
        
        mouth.sub <- names(hydrographs$hyd)[which(grepl(mouth.ID[i], names(hydrographs$hyd)))]
        
        x <- hyd.extract(subs = mouth.sub, hydrographs)
        
        hyd.plot(x$sim, precip = hydrographs$hyd$precip)
        title(paste("Mouth of Creek:", mouth.sub))
      }
      
    } else {
      
      ## identofy the subbasin that drains to the mouth of the creek (i.e., downstream id = -1)
      mouth.ID <- subbasins.present$Subbasin_ID[subbasins.present$Downstream_ID == "-1"] 
      
      for(i in 1:length(mouth.ID)){
      
        mouth.sub <- names(hydrographs$hyd)[which(grepl(mouth.ID[i], names(hydrographs$hyd)))]
        
        x <- hyd.extract(subs = mouth.sub, hydrographs)
        
        hyd.plot(x$sim, precip = hydrographs$hyd$precip)
        title(paste("Mouth of Creek:", mouth.sub))
      }
      
    }
  
  }
  ###############################
  ##
  ## Plot watershed storage components
  ##
  ###############################
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))){
  
    ws.storage <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))
    
    par(mfrow = c(4, 1), mar= c(2,4,2,2))
    
    for(i in 6:ncol(ws.storage)){
      
      plot(ws.storage[,i], type = 'l', main = colnames(ws.storage[i]))
      
    }
  
  }
  ###############################
  ##
  ## Plot Model Forcings
  ##
  ###############################
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ForcingFunctions.csv", sep = "")))){
  
    forcing <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ForcingFunctions.csv", sep = "")))
    
    forcing$tiso <- as.POSIXct(forcing$date, format = "%Y-%m-%d")
    
    par(mfrow = c(2, 1))
    
    par(mar = c(0,5,2.5,1))
    plot(forcing$tiso, forcing$temp_daily_min..C., type = "l",
         col = 'blue',
         xlab = "",
         xaxt = "n",
         ylab = expression(paste("Daily Air Temperature (", ~degree~C, ")")),
         ylim = c(-30, 50),
         panel.first = abline(h = 0, lty = 3, col = 'grey'))
    
    lines(forcing$tiso, forcing$temp_daily_max..C., type = 'l', col = 'red')
    
    legend("topleft", legend = c("Minimum Daily Air Temperatue", "Maximum Daily Air Temperature"), lty = 1, col = c("blue", "red"), bty = 'n')
    
    
    par(mar = c(2,5,0.5,1))
    plot(forcing$tiso, forcing$rain..mm.d., type = 'h',
         xlab = '',
         ylab = "Daily Total Precipitation (mm)",
         panel.first = abline(h = 0, lty = 3, col = 'grey'),
         ylim = c(0, 65)
    )
    lines(forcing$tiso, forcing$snow..mm.d., col = 'lightblue', type = 'h')
    
    legend("topright", legend = c("Daily Total Rain", "Daily Total Snow"), col = c("black", "lightblue"), bty = 'n', lty = 1)
  
  }
  ###############################
  ##
  ## Plot Reservoir Stage (if reservoirs present)
  ##
  ###############################
  
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2))
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))){
    
    reservoir.subbasins <- subbasins.present[subbasins.present$Reservoir_name != "<Null>", "SubBasin_name"]
    
    reservoir.stage <- res.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))
    
    # adjust the date index of the reservoir xts object so that the observed outflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    index(reservoir.stage$res) <- index(reservoir.stage$res) - 86400
    
    for(i in 1:length(reservoir.subbasins)){
      
      col.name <- gsub(' ','_',reservoir.subbasins[i])
      
      res.plot(reservoir.stage$res[,col.name], zero.axis = F)
      
      title(col.name)
    }  
  }
  
  
  ###############################
  ##
  ## Plot Modelled Snow and Observed Snow
  ##
  ###############################
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_ByHRUGroup.csv", sep = "")))){
    
    ## Read-in modelled snow
    snow <- custom.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_ByHRUGroup.csv", sep = "")))
    
    # TODO: fix custom output time index to be period starting rather than period ending
    #       this is only relevant when the custom output is CONINTUOUS not DAILY
    # adjust the date index of the reservoir xts object so that the observed outflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    # index(snow) <- index(snow) - 86400
    
    dates <- index(snow)
    
    if(length(all.snow.courses.included) > 0){
      
      for(i in 1:length(all.snow.courses.included)){
        
        SC.station <- paste("SC", all.snow.courses.included[i], sep= "_")
        
        SC.station.watershed <- unique(snow.course.locations[snow.course.locations$LCTN_ID == all.snow.courses.included[i], "GNIS_NAME"])
        
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SC.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
        
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SC.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
        
        mod.snow <- as.numeric(snow[,SC.station])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste(SC.station.watershed, "- Snow Course", all.snow.courses.included[i]))
        points(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, pch = 19, col = 'red')
        
        legend("topright", legend = c(paste("Modelled SWE at Snow Course", all.snow.courses.included[i]), paste("Observed SWE at Snow Course", all.snow.courses.included[i])),
               pch = c(NA, 19),
               col = c("black", "red"),
               lty = c(1, NA),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.courses.included)
    
    
    
    if(length(all.snow.pillows.included) > 0){
      
      par(mfrow = c(length(all.snow.pillows.included), 1))
      
      for(i in 1:length(all.snow.pillows.included)){
        
        SP.station <- paste("SP", all.snow.pillows.included[i], sep= "_")
        
        SP.station.watershed <- unique(snow.pillow.locations[snow.pillow.locations$LCTN_ID == all.snow.pillows.included[i], "GNIS_NAME"])
        
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SP.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
        
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SP.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
        
        mod.snow <- as.numeric(snow[,SP.station])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste(SP.station.watershed, " - Snow Pillow", all.snow.pillows.included[i]))
        lines(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, col = 'red')
        
        legend("topright", legend = c(paste("Modelled SWE at Snow Pillow", all.snow.pillows.included[i]), paste("Observed SWE at Snow Pillow", all.snow.pillows.included[i])),
               col = c("black", "red"),
               lty = c(1, 1),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.courses.included)
  } # End if file exists
  
  
  ##------------------------------------------
  ##
  ## Plot Modelled Snow (BASIN AVERAGE) against Observed Snow
  ##
  ##------------------------------------------
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_BySubbasin.csv", sep = "")))){
    
    ## Read-in modelled snow
    # CALIBRATION FILE LOC
    # snow <- custom.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_BySubbasin.csv", sep = "")))
    snow <- custom.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_BySubbasin.csv", sep = "")))
    
    # TODO: fix custom output time index to be period starting rather than period ending
    #       this is only relevant when the custom output is CONINTUOUS not DAILY
    # adjust the date index of the reservoir xts object so that the observed outflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    # index(snow) <- index(snow) - 86400
    
    dates <- index(snow)
    
    
    if(length(all.snow.courses.included) > 0){
      
      for(i in 1:length(all.snow.courses.included)){
        
        SC.station <- paste("SC", all.snow.courses.included[i], sep= "_")
        
        SC.station.subbasin <- unique(snow.course.locations[snow.course.locations$LCTN_ID == all.snow.courses.included[i], "Subbasin_ID"])
        
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SC.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
        
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SC.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
        
        mod.snow <- as.numeric(snow[,as.character(SC.station.subbasin)])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste("Subbasin", SC.station.subbasin, "- Snow Course", all.snow.courses.included[i]))
        points(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, pch = 19, col = 'red')
        
        legend("topright", legend = c(paste("Daily Average Modelled SWE in Subbasin", SC.station.subbasin), paste("Observed SWE at Snow Course", all.snow.courses.included[i])),
               pch = c(NA, 19),
               col = c("black", "red"),
               lty = c(1, NA),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.courses.included)
    
    
    if(length(all.snow.pillows.included) > 0){
      
      par(mfrow = c(length(all.snow.pillows.included), 1))
      
      for(i in 1:length(all.snow.pillows.included)){
        
        SP.station <- paste("SP", all.snow.pillows.included[i], sep= "_")
        
        SP.station.subbasin <- unique(snow.pillow.locations[snow.pillow.locations$LCTN_ID == all.snow.pillows.included[i], "Subbasin_ID"])
        
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SP.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
        
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(SP.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
        
        mod.snow <- as.numeric(snow[,as.character(SP.station.subbasin)])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste("Subbasin", SP.station.subbasin, " - Snow Pillow", all.snow.pillows.included[i]))
        lines(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, col = 'red')
        
        legend("topright", legend = c(paste("Daily Average Modelled SWE in Subbasin", SP.station.subbasin), paste("Observed SWE at Snow Pillow", all.snow.pillows.included[i])),
               col = c("black", "red"),
               lty = c(1, 1),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.pillows.included)
  } # End if file exists
  
  
} # End Function


plot.calibration.results <- function(ws.interest, run.number, subbasin.subset) {

  ###############################
  ##
  ## Plot Modelled Streamflows
  ##
  ###############################

  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))){
  
    hydrographs <- hyd.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
    
    # adjust the date index of the hydrograph xts object so that the observed streamflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    index(hydrographs$hyd) <- index(hydrographs$hyd) - 86400
    
    ## Identify which columns have obsrved data available
    subs.obs <- colnames(hydrographs$hyd[,hydrographs$obs.flag == TRUE])
    
    if(length(subs.obs) >0){
      
      ## Remove NA
      subs.obs <- subs.obs[!is.na(subs.obs)]
      
      ## remove the "_obs" characters to allow successful extraction
      my.subs <- gsub("_obs", "", subs.obs)
      
      par(mfrow = c(1,1))
      
      ## Generate a plot for all subbasins which have observed data available
      for(i in 1:length(my.subs)){
        x <- hyd.extract(subs = my.subs[i], hydrographs)
        hyd.plot(x$sim, x$obs, precip = hydrographs$hyd$precip)
        title(my.subs[i])
      }
      
      ## identofy the subbasin that drains to the mouth of the creek (i.e., downstream id = -1)
      mouth.ID <- subbasins.present$Subbasin_ID[subbasins.present$Downstream_ID == "-1"] 
      
      for(i in 1:length(mouth.ID)){
        
        mouth.sub <- names(hydrographs$hyd)[which(grepl(mouth.ID[i], names(hydrographs$hyd)))]
        
        x <- hyd.extract(subs = mouth.sub, hydrographs)
        
        hyd.plot(x$sim, precip = hydrographs$hyd$precip)
        title(paste("Mouth of Creek:", mouth.sub))
      }
      
    } else {
      
      ## identofy the subbasin that drains to the mouth of the creek (i.e., downstream id = -1)
      mouth.ID <- subbasins.present$Subbasin_ID[subbasins.present$Downstream_ID == "-1"] 
      
      for(i in 1:length(mouth.ID)){
        
        mouth.sub <- names(hydrographs$hyd)[which(grepl(mouth.ID[i], names(hydrographs$hyd)))]
        
        x <- hyd.extract(subs = mouth.sub, hydrographs)
        
        hyd.plot(x$sim, precip = hydrographs$hyd$precip)
        title(paste("Mouth of Creek:", mouth.sub))
      }
      
      
    }
  
  }
  
  ###############################
  ##
  ## Plot watershed storage components
  ##
  ###############################
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))){
  
    ws.storage <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))
    
    par(mfrow = c(4, 1), mar= c(2,4,2,2))
    
    for(i in 6:ncol(ws.storage)){
      
      plot(ws.storage[,i], type = 'l', main = colnames(ws.storage[i]))
      
    }
  
  }
  ###############################
  ##
  ## Plot Model Forcings
  ##
  ###############################
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_ForcingFunctions.csv", sep = "")))){
  
    forcing <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_ForcingFunctions.csv", sep = "")))
    
    forcing$tiso <- as.POSIXct(forcing$date, format = "%Y-%m-%d")
    
    par(mfrow = c(2, 1))
    
    par(mar = c(0,5,2.5,1))
    plot(forcing$tiso, forcing$temp_daily_min..C., type = "l",
         col = 'blue',
         xlab = "",
         xaxt = "n",
         ylab = expression(paste("Daily Air Temperature (", ~degree~C, ")")),
         ylim = c(-30, 50),
         panel.first = abline(h = 0, lty = 3, col = 'grey'))
    
    lines(forcing$tiso, forcing$temp_daily_max..C., type = 'l', col = 'red')
    
    legend("topleft", legend = c("Minimum Daily Air Temperatue", "Maximum Daily Air Temperature"), lty = 1, col = c("blue", "red"), bty = 'n')
    
    
    par(mar = c(2,5,0.5,1))
    plot(forcing$tiso, forcing$rain..mm.d., type = 'h',
         xlab = '',
         ylab = "Daily Total Precipitation (mm)",
         panel.first = abline(h = 0, lty = 3, col = 'grey'),
         ylim = c(0, 65)
    )
    lines(forcing$tiso, forcing$snow..mm.d., col = 'lightblue', type = 'h')
    
    legend("topright", legend = c("Daily Total Rain", "Daily Total Snow"), col = c("black", "lightblue"), bty = 'n', lty = 1)

  }
  
  ###############################
  ##
  ## Plot Reservoir Stage (if reservoirs present)
  ##
  ###############################
  
  par(mfrow = c(1,1), mar = c(5, 4, 4, 2))
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))){
    
    reservoir.subbasins <- subbasins.present[subbasins.present$Reservoir_name != "<Null>", "SubBasin_name"]
    
    reservoir.stage <- res.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))
    
    # adjust the date index of the reservoir xts object so that the observed outflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    index(reservoir.stage$res) <- index(reservoir.stage$res) - 86400
    
    for(i in 1:length(reservoir.subbasins)){
      
      col.name <- gsub(' ','_',reservoir.subbasins[i])
      
      res.plot(reservoir.stage$res[,col.name], zero.axis = F)
      
      title(col.name)
    }  
    
  }

  ###############################
  ##
  ## Plot Modelled Snow and Observed Snow
  ##
  ###############################
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_ByHRUGroup.csv", sep = "")))){
     
    ## Read-in modelled snow
    snow <- custom.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_ByHRUGroup.csv", sep = "")))
    
    # TODO: fix custom output time index to be period starting rather than period ending
    #       this is only relevant when the custom output is CONINTUOUS not DAILY
    # adjust the date index of the reservoir xts object so that the observed outflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    # index(snow) <- index(snow) - 86400
    
    dates <- index(snow)
    
    if(length(all.snow.courses.included) > 0){
      
      for(i in 1:length(all.snow.courses.included)){
        
        SC.station <- paste("SC", all.snow.courses.included[i], sep= "_")
        
        SC.station.watershed <- unique(snow.course.locations[snow.course.locations$LCTN_ID == all.snow.courses.included[i], "GNIS_NAME"])
        
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SC.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
        
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SC.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
        
        mod.snow <- as.numeric(snow[,SC.station])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste(SC.station.watershed, "- Snow Course", all.snow.courses.included[i]))
        points(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, pch = 19, col = 'red')
        
        legend("topright", legend = c(paste("Modelled SWE at Snow Course", all.snow.courses.included[i]), paste("Observed SWE at Snow Course", all.snow.courses.included[i])),
               pch = c(NA, 19),
               col = c("black", "red"),
               lty = c(1, NA),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.courses.included)
    
    
    
    if(length(all.snow.pillows.included) > 0){
      
      par(mfrow = c(length(all.snow.pillows.included), 1))
      
      for(i in 1:length(all.snow.pillows.included)){
        
        SP.station <- paste("SP", all.snow.pillows.included[i], sep= "_")
        
        SP.station.watershed <- unique(snow.pillow.locations[snow.pillow.locations$LCTN_ID == all.snow.pillows.included[i], "GNIS_NAME"])
        
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SP.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
        
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SP.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
        
        mod.snow <- as.numeric(snow[,SP.station])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste(SP.station.watershed, " - Snow Pillow", all.snow.pillows.included[i]))
        lines(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, col = 'red')
        
        legend("topright", legend = c(paste("Modelled SWE at Snow Pillow", all.snow.pillows.included[i]), paste("Observed SWE at Snow Pillow", all.snow.pillows.included[i])),
               col = c("black", "red"),
               lty = c(1, 1),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.courses.included)
  } # End if file exists
  
  
  ##------------------------------------------
  ##
  ## Plot Modelled Snow (BASIN AVERAGE) against Observed Snow
  ##
  ##------------------------------------------
  
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_BySubbasin.csv", sep = "")))){
    
    ## Read-in modelled snow
    snow <- custom.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_SNOW_Daily_Average_BySubbasin.csv", sep = "")))
    
    # TODO: fix custom output time index to be period starting rather than period ending
    #       this is only relevant when the custom output is CONINTUOUS not DAILY
    # adjust the date index of the reservoir xts object so that the observed outflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    # index(snow) <- index(snow) - 86400
    
    dates <- index(snow)
    
    
    if(length(all.snow.courses.included) > 0){
      
      for(i in 1:length(all.snow.courses.included)){
        
        SC.station <- paste("SC", all.snow.courses.included[i], sep= "_")
        
        SC.station.subbasin <- unique(snow.course.locations[snow.course.locations$LCTN_ID == all.snow.courses.included[i], "Subbasin_ID"])
        
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SC.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
        
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SC.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
      
        mod.snow <- as.numeric(snow[,as.character(SC.station.subbasin)])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste("Subbasin", SC.station.subbasin, "- Snow Course", all.snow.courses.included[i]))
        points(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, pch = 19, col = 'red')
        
        legend("topright", legend = c(paste("Daily Averaged Modelled SWE in Subbasin", SC.station.subbasin), paste("Observed SWE at Snow Course", all.snow.courses.included[i])),
               pch = c(NA, 19),
               col = c("black", "red"),
               lty = c(1, NA),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.courses.included)
    
    
    if(length(all.snow.pillows.included) > 0){
      
      par(mfrow = c(length(all.snow.pillows.included), 1))
      
      for(i in 1:length(all.snow.pillows.included)){
        
        SP.station <- paste("SP", all.snow.pillows.included[i], sep= "_")
        
        SP.station.subbasin <- unique(snow.pillow.locations[snow.pillow.locations$LCTN_ID == all.snow.pillows.included[i], "Subbasin_ID"])
       
        n.records <- strsplit(readLines(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SP.station, ".rvt", sep = "")), n = 1), " ")[[1]][4]
   
        obs.snow <- read.table(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(SP.station, ".rvt", sep = "")), skip = 1, nrows = as.numeric(n.records), na.strings = "-1.2345")
   
        mod.snow <- as.numeric(snow[,as.character(SP.station.subbasin)])
        
        plot(dates, mod.snow, type = 'l',
             ylim = c(0, max(mod.snow, obs.snow$V3, na.rm = T)),
             xlab = "Date",
             ylab = "Snow Water Equivalent (SWE) (mm)",
             main = paste("Subbasin", SP.station.subbasin, " - Snow Pillow", all.snow.pillows.included[i]))
        lines(as.POSIXct(obs.snow$V1, format = "%Y-%m-%d"), obs.snow$V3, col = 'red')
        
        legend("topright", legend = c(paste("Daily Averaged Modelled SWE in Subbasin", SP.station.subbasin), paste("Observed SWE at Snow Pillow", all.snow.pillows.included[i])),
               col = c("black", "red"),
               lty = c(1, 1),
               bty = "n")
        
      } # End for loop
    } # End if statement (length of all.snow.pillows.included)
  } # End if file exists
  
} # End function

# aggregate output
# specify the time bins to use for aggregating the Raven output. Set value to NULL if not wanted.
aggregate.output <- function(ws.interest, run.number, subbasin.subset,  
                             AWDM.weeks = c(1:52), ISO.weeks = c(1:52), months = c(1:12), years = c(1996:2010)){
 
  # create output directory and base directory object for easy output filename generation
  if(dir.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))){
    dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste0("aggregated_output")))
    base.dir <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste0("aggregated_output"))
  }
  
  # create AWDM weeks time series. Assume full period of record, 1996 - 2010
  awdm.w <- make.AWDM.weeks(weeks.wanted = AWDM.weeks)
  
  # aggregate hydrographs
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))){
    
    ## Read-in modelled hydrographs
    hydrographs <- hyd.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
    
    # adjust the date index of the hydrograph xts object so that the observed streamflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    index(hydrographs$hyd) <- index(hydrographs$hyd) - 86400
    
    hyd <- as.data.frame(hydrographs$hyd[-1, ])
    
    cnames <- colnames(hyd)
    
    hyd <- hyd %>%
      dplyr::mutate(date = lubridate::ymd(rownames(hyd))) %>%
      left_join(awdm.w, by = "date") %>%
      dplyr::mutate(ISO.Week = lubridate::week(date),
                    Month = lubridate::month(date)) %>%
      dplyr::select(date, Year, Month, Week, ISO.Week, cnames)
    
    # aggregate hydrographs. Retain only the time grouping variable, summarize all 
    # subbasins as the mean of the time grouping variable (i.e., by year)
    if(exists('years') & !is.null(years)){
      hyd.year <- hyd %>%
        dplyr::select(-c(date, Month, Week, ISO.Week)) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "Hydrographs", "Annual.csv", sep = "-"))
      data.table::fwrite(hyd.year, out.fn)
    }
    if(exists('months') & !is.null(months)){
      hyd.months <- hyd %>%
        dplyr::select(-c(date, Week, ISO.Week)) %>%
        dplyr::group_by(Year, Month) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "Hydrographs", "Monthly.csv", sep = "-"))
      data.table::fwrite(hyd.months, out.fn)
    }
    if(exists('AWDM.weeks') & !is.null(AWDM.weeks)){
      hyd.AWDM <- hyd %>%
        dplyr::select(-c(date, Month, ISO.Week)) %>%
        dplyr::group_by(Year, Week) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "Hydrographs", "AWDM-Weeks.csv", sep = "-"))
      data.table::fwrite(hyd.AWDM, out.fn)
    }
    if(exists('ISO.weeks') & !is.null(ISO.weeks)){
      hyd.ISO <- hyd %>%
        dplyr::select(-c(date, Month, Week)) %>%
        dplyr::group_by(Year, ISO.Week) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "Hydrographs", "ISO-Weeks.csv", sep = "-"))
      data.table::fwrite(hyd.ISO, out.fn)
    }
  } # end hydrograph aggregation
  
  # aggregate watershed storage
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))){
    
    # read in watershed storage output
    ws.storage <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")),
                           check.names = FALSE, stringsAsFactors = FALSE)
    
    cnames <- colnames(ws.storage)
    
    ws.storage <- ws.storage %>%
      dplyr::mutate(date = lubridate::ymd(date)) %>%
      left_join(awdm.w, by = "date") %>%
      dplyr::mutate(ISO.Week = lubridate::week(date),
                    Month = lubridate::month(date)) %>%
      dplyr::select(date, Year, Month, Week, ISO.Week, cnames)
    
    # aggregate hydrographs. Retain only the time grouping variable, summarize all 
    # subbasins as the mean of the time grouping variable (i.e., by year)
    if(exists('years') & !is.null(years)){
      ws.storage.year <- res %>%
        dplyr::select(-c(date, Month, Week, ISO.Week)) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "WatershedStorage", "Annual.csv", sep = "-"))
      data.table::fwrite(ws.storage.year, out.fn)
    }
    if(exists('months') & !is.null(months)){
      ws.storage.months <- res %>%
        dplyr::select(-c(date, Week, ISO.Week)) %>%
        dplyr::group_by(Year, Month) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "WatershedStorage", "Monthly.csv", sep = "-"))
      data.table::fwrite(ws.storage.months, out.fn)
    }
    if(exists('AWDM.weeks') & !is.null(AWDM.weeks)){
      ws.storage.AWDM <- res %>%
        dplyr::select(-c(date, Month, ISO.Week)) %>%
        dplyr::group_by(Year, Week) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "WatershedStorage", "AWDM-Weeks.csv", sep = "-"))
      data.table::fwrite(ws.storage.AWDM, out.fn)
    }
    if(exists('ISO.weeks') & !is.null(ISO.weeks)){
      ws.storage.ISO <- res %>%
        dplyr::select(-c(date, Month, Week)) %>%
        dplyr::group_by(Year, ISO.Week) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "WatershedStorage", "ISO-Weeks.csv", sep = "-"))
      data.table::fwrite(ws.storage.ISO, out.fn)
    }
  } # end watershed storage aggregation 
  
  # aggregate reservoir stages
  if(file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))){
    
    ## Read-in modelled reservoir stages
    reservoir.subbasins <- subbasins.present[subbasins.present$Reservoir_name != "<Null>", "SubBasin_name"]
    reservoir.stage <- res.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))
    
    # adjust the date index of the reservoir xts object so that the observed outflow
    # is aligned with its day of observation (Period beginning vs. period ending Raven output)
    index(reservoir.stage$res) <- index(reservoir.stage$res) - 86400
    
    # drop first row as it is 1996-05-31, due to the fix of the time index from period ending
    # to period starting
    res <- as.data.frame(reservoir.stage$res[-1, ])
    
    cnames <- colnames(res)
    
    res <- res %>%
      dplyr::mutate(date = lubridate::ymd(rownames(res))) %>%
      left_join(awdm.w, by = "date") %>%
      dplyr::mutate(ISO.Week = lubridate::week(date),
                    Month = lubridate::month(date)) %>%
      dplyr::select(date, Year, Month, Week, ISO.Week, cnames)
    
    # aggregate hydrographs. Retain only the time grouping variable, summarize all 
    # subbasins as the mean of the time grouping variable (i.e., by year)
    if(exists('years') & !is.null(years)){
      res.year <- res %>%
        dplyr::select(-c(date, Month, Week, ISO.Week)) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "ReservoirStage", "Annual.csv", sep = "-"))
      data.table::fwrite(res.year, out.fn)
    }
    if(exists('months') & !is.null(months)){
      res.months <- res %>%
        dplyr::select(-c(date, Week, ISO.Week)) %>%
        dplyr::group_by(Year, Month) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "ReservoirStage", "Monthly.csv", sep = "-"))
      data.table::fwrite(res.months, out.fn)
    }
    if(exists('AWDM.weeks') & !is.null(AWDM.weeks)){
      res.AWDM <- res %>%
        dplyr::select(-c(date, Month, ISO.Week)) %>%
        dplyr::group_by(Year, Week) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "ReservoirStage", "AWDM-Weeks.csv", sep = "-"))
      data.table::fwrite(res.AWDM, out.fn)
    }
    if(exists('ISO.weeks') & !is.null(ISO.weeks)){
      res.ISO <- res %>%
        dplyr::select(-c(date, Month, Week)) %>%
        dplyr::group_by(Year, ISO.Week) %>%
        dplyr::summarize_all(mean, na.rm = TRUE)
      out.fn <- file.path(base.dir, paste(ws.interest, run.number, "ReservoirStage", "ISO-Weeks.csv", sep = "-"))
      data.table::fwrite(res.ISO, out.fn)
    }
  } # end reservoir aggregation
}

## Function to remove the :SuppressOutput command from the rvi file to facilitate plotting of calibration results
rewrite.output <- function(ws.interest, run.number){
  
  main.RVI.file <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, ".rvi", sep = ""))
  
  new.RVI.file <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "-replace.rvi", sep = ""))
  
  con <- file(main.RVI.file, open = 'r')
  while(TRUE) {
    line <- readLines(con, n = 1)
    if(length(line) == 0) break
    else if(!startsWith(line, ":SuppressOutput")){
      write(line, file = new.RVI.file, append = TRUE)
    } 
  }
  
  RVI.template <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, RVI.template.in.file))
    
  ## Add colon to all parameter calls - this is required by Raven
  RVI.template$PARAMETER <- paste(":", RVI.template$PARAMETER, sep = '')
  
  custom.output <- RVI.template[RVI.template$PARAMETER == ":CustomOutput", c("PARAMETER", "DEFINITION", "FROM", "TO")]
  
  
  write.table(custom.output, file = new.RVI.file, sep = "\t", quote = F, row.names = F, col.names = F, append = T)

  close(con)
  
  file.remove(main.RVI.file)
  
  file.rename(new.RVI.file, main.RVI.file)
  
}




ephemeral.calibration <- function(ws.interest, run.number){
  
  ## Create Ephemeral VM
  system2("gcloud", args = paste("compute instances create ", run.number, " --zone=northamerica-northeast1-a --boot-disk-size=100GB --boot-disk-type=pd-ssd --boot-disk-device-name=", run.number, " --custom-extensions --custom-cpu=8 --custom-memory=10GB --image=raven-ephemeral-vm", sep = ""))
  
  ## Write a bash script to control what happens on the ephemeral VM:
  # - change directory into /var/raven/run-name
  # - execute ostrich
  # - execute an R script thqt performs plotting of calibration results
  # - find and delete all NetCDF files to reduce file size for transfer back to Master VM
  # - create "complete.txt"
  ephemeral.vm.controls <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "ephemeral.vm.controls.sh")
  
  cat(file = ephemeral.vm.controls, append = F, sep = "",
      
      ## Change directory to the run directory
      "cd /var/raven/", paste(ws.interest, run.number, sep = "-"), "\n",
      
      ## Execute Ostrich
      "/usr/bin/mpirun -n 7 ", file.path("/var/raven", paste(ws.interest, run.number, sep = "-"), "OstrichMPI"), "\n",
      
      ## Execute an Rscript to plot the results
      "Rscript ", paste(ws.interest, "-", run.number, "-plot-results.R", sep = ""), "\n",
      
      ## Find NetCDF files and delete them
      "find . -name '*.nc' -type f -delete", "\n",
      
      ## Create "complete.txt" to signal that Master VM that it's all done
      "touch ", file.path("/var/raven", paste(ws.interest, run.number, sep = "-"), "complete.txt"), "\n"
      
      )
  
  
  ## Write an Rscript that will handle all plotting functions once calibration is complete
  # - Define all required variables
  # - Define rewrite output function
  # - Define plot.calibration.results function
  # - Execute rewrite output function
  # - Execute plot.calibration.results function
  
  plotting.script <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, "-plot-results.R", sep = ""))
  
  
  ## Copy initial Raven run/results and Ostrich input files and executables to the ephemeral VM
  
  ## Execute Ostrich on the ephemeral VM
  
  ## Plot the results of the calibration
  
  ## Create some sign of success
  
  ## copy back all results
  
  ## Turn off & destroy the ephemeral VM
  
  
  
  
  
  ## Create ephemeral.vm.vcontrols.sh
  ephemeral.vm.controls <- file.path("/var/obwb-hydro-modelling", "ephemeral.vm.controls.sh")
  
  cat(file = ephemeral.vm.controls, append = F, sep = "",
      
      ## Command to copy all Raven files for given run
      "sudo gcloud ", paste("compute scp --compress --zone=northamerica-northeast1-a --recurse ", file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), " ", Sys.getenv("LOGNAME"), "@", run.number, ":/var/raven", sep = ""), "\n",
      
      ## Change directory to the run directory
      "cd /var/raven/", paste(ws.interest, run.number, sep = "-"), "\n",
      
      ## Execute Ostrich
      "/usr/bin/mpirun -n 7 ", file.path("/var/raven", paste(ws.interest, run.number, sep = "-"), "OstrichMPI"), "\n",
      
      "Rscript plot-results.R", "\n",
      
      "touch success.txt"
      
      # ## Remove all netCDF files to reduce file transfer coming back
      # "sudo rm -r *.nc", "\n",
      # 
      # ## Copy calibration results back to master vm
      # "sudo gcloud ", paste("compute scp --compress --zone=northamerica-northeast1-a --recurse ", file.path("/var/raven", paste(ws.interest, run.number, sep = "-")), " ", file.path("obwb-hydro-modelling:/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "calibration-results")), "\n",
      # 
      # ## shut down the master vm
      # "sudo shutdown -h now"
      
      )
  
  ## copy ephemeral.vm.controls to the vm
  system2("gcloud", args = paste("compute scp --compress --zone=northamerica-northeast1-a --recurse ", ephemeral.vm.controls, " ", Sys.getenv("LOGNAME"), "@", run.number, ":/var/raven", sep = "")) 
  
  ## Execute ephemeral.vm.controls on the vm
  system2("gcloud", args = paste("compute ssh ", Sys.getenv("LOGNAME"), "@", run.number, " --zone=northamerica-northeast1-a --command='cd /var/raven/ ; bash ephemeral.vm.controls.sh'", sep = ""), wait = F, stdout = NULL)
  
}