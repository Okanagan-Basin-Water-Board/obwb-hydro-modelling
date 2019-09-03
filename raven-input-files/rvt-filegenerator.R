############################################################################################################################
##
## This script generates the requird *.rvt file for input to Raven with select WSC hydrometric stations
##
## Mar-01-2019 LAB
##
############################################################################################################################

source("/var/obwb-hydro-modelling/src/functions.R")

# Download the lastest HYDAT database from WSC and save in specified location
# download_hydat(dl_hydat_here = "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/")

## Load required packages
library(tidyhydat)
library(RavenR)
library(ncdf4)

ws.interest <- ws.interest

include.watersheds <- include.watersheds

run.number <- run.number

## Specify the location where the HYDAT database is saved
hydat_here <- "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3"

## Read in the list of WSC stations required to be downloaded. This table must also include associated subbasins and watersheds
# download.list <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/WSC_download_list.csv")

## Read in the RVI template and identify the start and end dates. WSC data is then only imported for this period. Start.date and end.date
## are formatted as required by hy_daily_flows.
RVI.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVI-Template.csv")

start.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "StartDate", "DEFINITION"], format = "%m/%d/%Y")

end.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "EndDate", "DEFINITION"], format = "%m/%d/%Y")

## Specify the location RAVEN *.rvt files should be saved
output.location <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"))


## Extract corresponding WSC gauges from subbasin_codes.csv
subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

## Extract all WSC station numbers for use in hy_daily_flows to retrieve datasets for all
station.no <- subbasin.codes[subbasin.codes$GNIS_NAME %in% paste(include.watersheds, " Creek", sep = ''), "Hydrometric_stn"]

## Remove "<NULL">
station.no <- as.character(station.no[!station.no %in% "<Null>"])


## If there are no WSC stations within the ws.interest (i.e., station.no is zero), no redirects are written and the model will not be calibrated to WSC data
if(length(station.no) > 0) {

## Retrieve all available data for all required stations and save in one large tibble.
  # tmp <- hy_daily_flows(station_number = station.no, hydat_path = hydat_here, start_date = start.date, end_date = end.date)

  
  ## Add error handling for those stations which are not contained in the HYDAT database
  tryCatch(
    
    ## Retrieve all available data for all required stations and save in one large tibble
    {tmp <- hy_daily_flows(station_number = station.no, hydat_path = hydat_here, start_date = start.date, end_date = end.date)
    
    
    # Identify the stations that are actually included - these allow calibration station to be specified.
    stations.included <- unique(tmp$STATION_NUMBER)
    
    ## Execute my custom function which does the follows:
    # - A bunch of QA/QC tests written by Rob Chlumsky
    # - Isolates all stations for each Watershed
    # - Creates ONE "WatershednName-RunNumber.rvt" file for the select watershed. This file contains the file names for individual 
    #   *.rvt files for each WSC gauge within the select watershed
    # - Creates INDIVIDUAL "subid_wscname.rvt" file for each WSC station and the associated subbasin.
    ECflow.rvt.tidy.single.obs(ff = tmp,
                               master = subbasin.codes,
                               dir = output.location,
                               include.watersheds = include.watersheds,
                               run.number = run.number,
                               calibration.start = calibration.start,
                               calibration.end = calibration.end,
                               write.redirect = T,
                               flip.number = T)
    },
    
    ## If this returns an error (most likely because the station(s) don't exist in the HYDAT database), return an error, but keep executing
    error = function(e) {print(paste("WSC station:", station.no, "does not exist in the HYDAT database. No observed flows will be included in the model run."))
    ## Create empty file so remaining commands can be appended.
    RVToutFile <- file(file.path(output.location, paste(ws.interest, "-", run.number, ".rvt", sep = '')), open = "a+")
    close(RVToutFile)}
  
    ) ## End of error handling

} else {print(paste("No WSC stations exist within the", include.watersheds, "Creek watershed(s)..."))
  ## Create empty file so remaining commands can be appended.
  RVToutFile <- file(file.path(output.location, paste(ws.interest, "-", run.number, ".rvt", sep = '')), open = "a+")
  close(RVToutFile)
  }



################################################################################################
##
## Add climate weighting commands to end of master rvt file
##
################################################################################################

## read in RVH file to compute the number of HRUs
HRUs <- rvh.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))

precip.forcing.filename <- "pr.HRU.timeseries.DRAFT.nc"

tasmax.forcing.filename <- "tasmax.HRU.timeseries.DRAFT.nc"

tasmin.forcing.filename <- "tasmin.HRU.timeseries.DRAFT.nc"


## Get dimensions of netcdf file - only uses tasmin since all are the same dimensions
tasmin.nc.file <- nc_open(file.path("/var/obwb-hydro-modelling/input-data/processed/climate", tasmin.forcing.filename))

# tasmin <- ncvar_get(tasmin.nc.file, "tasmin")

# nGridCell <- dim(tasmin)[1]

nGridCell <- tasmin.nc.file$dim$HRU$len

# ntime <- dim(tasmin)[2]

ntime <- tasmin.nc.file$dim$time$len

nHRU <- nrow(HRUs$HRUtable)


weights <- matrix(nrow = nHRU, ncol = 3, data = c(HRU = HRUs$HRUtable$ID,
                                                  Station = seq(0, max(as.numeric(HRUs$HRUtable$ID) -1), 1),
                                                  weight = rep(1, nHRU)), byrow = F)

RVToutFile <- file.path(output.location, paste(ws.interest, "-", run.number, ".rvt", sep = ""))

cat(file = RVToutFile, append = T, sep = "",
    "\n",
    ":StationForcing", "\n",
    ":ForcingType PRECIP", "\n",
    ":FileNameNC  ", precip.forcing.filename, "\n",
    ":VarNameNC   pr", "\n",
    ":DimNamesNC  HRU time", "\n",
    ":GridWeights", "\n",
    ":NumberHRUs  ", nHRU, "\n",
      ":NumberGridCells ", nGridCell, "\n"
)

write.table(weights, RVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = RVToutFile, append = T, sep = "",
    ":EndGridWeights", "\n",
    ":EndStationForcing","\n"
)


cat(file = RVToutFile, append = T, sep = "",
    "\n",
    ":StationForcing", "\n",
    ":ForcingType TEMP_MIN", "\n",
    ":FileNameNC  ", tasmin.forcing.filename, "\n",
    ":VarNameNC   tasmin", "\n",
    ":DimNamesNC  HRU time", "\n",
    ":GridWeights", "\n",
    ":NumberHRUs  ", nHRU, "\n",
    ":NumberGridCells ", nGridCell, "\n"
)

write.table(weights, RVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = RVToutFile, append = T, sep = "",
    ":EndGridWeights", "\n",
    ":EndStationForcing","\n"
)

cat(file = RVToutFile, append = T, sep = "",
    "\n",
    ":StationForcing", "\n",
    ":ForcingType TEMP_MAX", "\n",
    ":FileNameNC  ", tasmax.forcing.filename, "\n",
    ":VarNameNC   tasmax", "\n",
    ":DimNamesNC  HRU time", "\n",
    ":GridWeights", "\n",
    ":NumberHRUs  ", nHRU, "\n",
    ":NumberGridCells ", nGridCell, "\n"
)

write.table(weights, RVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = RVToutFile, append = T, sep = "",
    ":EndGridWeights", "\n",
    ":EndStationForcing","\n",
    "\n"
)

############################################################################################################################
##
## Grab, process, and generate Raven files for OWDM files for Whiteman Creek
##
############################################################################################################################

if(include.water.demand == TRUE){
  
  RVI.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVI-Template.csv")
  
  RVI.template$PARAMETER <- paste(":", RVI.template$PARAMETER, sep = '')
  
  time <- RVI.template[RVI.template$GROUP == "Time", c("PARAMETER", "DEFINITION")]
  
  time$DEFINITION <- paste(as.Date(time$DEFINITION, format = "%m/%d/%Y"), "00:00:00", sep = ' ')
  
  ## Read in OWDM data (could be one master file for all watersheds/subbasins)
  owdm <- read.csv("/var/obwb-hydro-modelling/input-data/raw/owdm/Whiteman.csv")
  
  owdm$watershed <- gsub( " .*$", "", owdm$subbasin)
  
  ## Isolate specified watershed
  owdm.sub <- owdm[owdm$watershed == include.watersheds,]
  
  owdm.sub$extraction.total <- rowSums(owdm.sub[,c("indoor", "outdoor_domestic", "outdoor_animal", "outdoor_other_irrigation")])
  
  owdm.sub$date <- paste(owdm.sub$year, owdm.sub$day, sep = "-")
  
  owdm.sub$tiso <- as.Date(owdm.sub$date, format = "%Y-%j")
  
  model.period.start <- as.Date(time$DEFINITION[time$PARAMETER == ":StartDate"])
  
  model.period.end <- as.Date(time$DEFINITION[time$PARAMETER == ":EndDate"])
  
  if(nrow(owdm.sub) > 0){
    
    subs <- unique(owdm.sub$subbasin_id)
    
    for(i in 1:length(subs)){
      
      ## isolate extractionf or one subbasin
      tmp <- owdm.sub[owdm.sub$subbasin_id == subs[i],]
      
      warmup.demand.period <- tmp$tiso[1] - model.period.start
      
      # If the model is to be run prior to water demand being included, create "empty"/Zero demand for the warmup period
      if(warmup.demand.period > 0){
        date.fills <- seq(model.period.start, length.out = warmup.demand.period, by = 1)
      
        warmup.demand <- data.frame(matrix(NA, ncol = ncol(tmp), nrow = length(date.fills)))
        
        colnames(warmup.demand) <- colnames(tmp)
        
        warmup.demand$tiso <- date.fills
        
        warmup.demand$extraction.total <- 0
      
        tmp <- rbind(warmup.demand, tmp)
        
      }
      
      ## make extraction total negative to represent "extraction" rather than addition to flow
      tmp$extraction.total <- tmp$extraction.total * -1
      
      ## convert extraction total to m3/s from m3/day
      tmp$extraction.total <- tmp$extraction.total / (60*60*24)
  
      tmp$extraction.total <- ifelse(tmp$extraction.total == -0, 0, tmp$extraction.total)
      
      fc <- file(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(subs[i], "owdm.rvt", sep = "_")), open = "w+")
      
      writeLines(sprintf(':BasinInflowHydrograph2 %i # %s',subs[i], paste(subs[i], "owdm.rvt", sep = "_")), fc)
      writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(tmp$tiso[1]),nrow(tmp)), fc)
      
      for (k in 1:nrow(tmp)) {
        writeLines(sprintf('%g',tmp[k,"extraction.total"]), fc)
      }
      
      writeLines(':EndBasinInflowHydrograph2',fc)
      close(fc)
      
      cat(file = RVToutFile, append = T, sep = "",
          ":RedirectToFile ", paste(subs[i], "owdm.rvt", sep = "_"), "\n"
      )
      
    }
    
  } else {print("No OWDM data exists for currently included watershed(s)...")}

} else {print("Water demand was not included in this model run...")}

