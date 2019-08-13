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
