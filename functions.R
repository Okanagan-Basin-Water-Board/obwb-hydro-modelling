# function to find mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# functio to distribute percentage values that don't quite sum to 100%
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
  dates <- data.frame(Date = seq(as.Date(start.date), as.Date(end.date), by = "day"))

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
    ts.temp <- xts(order.by=as.Date(dd.sub$Date,format="%Y-%m-%d"),x=dd.sub$Value)
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

# function to plot model run results
plot.results <- function(ws.interest, run.number, subbasins.present) {
  
  ###############################
  ##
  ## Plot Modelled Streamflows
  ##
  ###############################
  
  ## Read-in modelled hydrographs
  hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
  
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
  
  ###############################
  ##
  ## Plot watershed storage components
  ##
  ###############################
  
  ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))
  
  par(mfrow = c(4, 1), mar= c(2,4,2,2))
  
  for(i in 6:ncol(ws.storage)){
    
    plot(ws.storage[,i], type = 'l', main = colnames(ws.storage[i]))
    
  }
  
  ###############################
  ##
  ## Plot Model Forcings
  ##
  ###############################
  
  forcing <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ForcingFunctions.csv", sep = "")))
  
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
  
  ###############################
  ##
  ## Plot Subbasin Network
  ##
  ###############################
  
  # HRU.file <- rvh.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
  # 
  # subbasinNetwork.plot(HRU.file$SBtable[sub("\\_.*", "", HRU.file$SBtable$Name) == include.watersheds, ], labeled = T)
  
  
}


plot.calibration.results <- function(ws.interest, run.number, subbasin.sibset) {

  ###############################
  ##
  ## Plot Modelled Streamflows
  ##
  ###############################

  hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
  
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
  
  ###############################
  ##
  ## Plot watershed storage components
  ##
  ###############################
  
  ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))
  
  par(mfrow = c(4, 1), mar= c(2,4,2,2))
  
  for(i in 6:ncol(ws.storage)){
    
    plot(ws.storage[,i], type = 'l', main = colnames(ws.storage[i]))
    
  }
  
  ###############################
  ##
  ## Plot Model Forcings
  ##
  ###############################
  
  forcing <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_ForcingFunctions.csv", sep = "")))
  
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

  ##############################
  ##
  ## Plot Subbasin Network
  ##
  ###############################
  
  # HRU.file <- rvh.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, ".rvh", sep = "")))
  # 
  # subbasinNetwork.plot(HRU.file$SBtable[sub("\\_.*", "", HRU.file$SBtable$Name) == include.watersheds, ], labeled = T)
  
  
}