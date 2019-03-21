# function to obtain WSC flow data and convert to Raven required rvt files.
ECflow.rvt.tidy.master <- function(ff,master,dir,prd=NULL,stnNames=NULL,write.redirect=F,flip.number=F) {
  
  
  watersheds <- as.character(unique(master$Watershed))
  
  
  # data checks
  if (!(is.null(stnNames)) & (length(unique(master$SubID)) != length(stnNames))) {
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
  for(i in 1:length(watersheds)){
    
    fc.redirect <- file(file.path(dir, paste("flow_stn_redirect_", watersheds[i], ".rvt", sep = '')), open = "a+")
    
    stations <- master$Station_No[which(master$Watershed == watersheds[i])]
    
    for(j in 1:length(stations)){
      
      dd.sub <- dd[which(dd$STATION_NUMBER %in% stations[j]),]
      
      subID <- master$SubID[which(master$Station_No == stations[j])]
      
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
    
    print(i)
    
  }
  
  return(TRUE)
  
}

# function to find mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# function to obtain WSC flow data and convert to Raven required rvt files.
ECflow.rvt.tidy.single <- function(ff,master,dir,include.watersheds,run.number,prd=NULL,stnNames=NULL,write.redirect=F,flip.number=F) {
  
  # data checks
  if (!(is.null(stnNames)) & (length(unique(master$SubID)) != length(stnNames))) {
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
    
    for(j in 1:length(stations)){
      
      dd.sub <- dd[which(dd$STATION_NUMBER %in% stations[j]),]
      
      subID <- master$SubID[which(master$Station_No == stations[j])]
      
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

