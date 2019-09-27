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

## -----------------------------------------------------
##
## Read-in RVP template and extract climate correction factors
##
## -----------------------------------------------------

## Read in RVP.template to identify required correction factor for Precipitation
RVP.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv", na.strings = c(""))

## Make all columns characters
RVP.template[,] <- lapply(RVP.template.base[, ], as.character)

## Extract the correction factor for Precipitation
pr.correction <- RVP.template[RVP.template$GROUP == "ClimateParameter" & RVP.template$PARAMETER == "LinearTransform" & RVP.template$DEFINITION == "pr", "VALUE"]

## -----------------------------------------------------
##
## Specify multiple commands required to generate WSC rvt files.
##
## -----------------------------------------------------


## Specify the location where the HYDAT database is saved
hydat_here <- "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3"

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

## If running an OSTRICH calibration, include the :LinearTransform function for precipitation. If not, exclude this command (Expected to slow down model initialization process)
if(run.ostrich == TRUE & length(pr.correction) == 1){
  cat(file = RVToutFile, append = T, sep = "",
      "\n",
      ":StationForcing", "\n",
      ":ForcingType PRECIP", "\n",
      ":FileNameNC  ", precip.forcing.filename, "\n",
      ":VarNameNC   pr", "\n",
      ":LinearTransform ", pr.correction, " 0.0", "\n",
      ":DimNamesNC  HRU time", "\n",
      ":GridWeights", "\n",
      ":NumberHRUs  ", nHRU, "\n",
        ":NumberGridCells ", nGridCell, "\n"
  )
  
} else {
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
}

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
## Generate *.rvt.tpl file to allow LinearTransform of climate variable(s)
##
############################################################################################################################

if(run.ostrich == TRUE){

  
  climate.parameters <- RVP.template[RVP.template$GROUP == "ClimateParameter", ]
  
  if(!all(is.na(climate.parameters$CAL_MAX))){

    climate.parameters.calibrate <- climate.parameters
  
    climate.parameters.calibrate$CAL_VAR <- NA
  
    climate.parameters.calibrate$CAL_VAR[which(is.na(climate.parameters.calibrate$CAL_VAR))] <- paste(climate.parameters.calibrate$DEFINITION[which(is.na(climate.parameters.calibrate$CAL_VAR))], climate.parameters.calibrate$PARAMETER[which(is.na(climate.parameters.calibrate$CAL_VAR))], sep = "_")

    pr.correction.calibrate <- climate.parameters.calibrate[climate.parameters.calibrate$PARAMETER == "LinearTransform" & climate.parameters.calibrate$DEFINITION == "pr", "CAL_VAR"]
    
    #############################################################################################
    ## 
    ##  Write *.rvt.tpl file
    ##
    #############################################################################################
    
    OstrichRVTTemplateFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
    
    
    cat(file = OstrichRVTTemplateFile, append = T, sep = "",
        "\n",
        ":StationForcing", "\n",
        ":ForcingType PRECIP", "\n",
        ":FileNameNC  ", precip.forcing.filename, "\n",
        ":VarNameNC   pr", "\n",
        ":LinearTransform ", pr.correction.calibrate, " 0.0", "\n",
        ":DimNamesNC  HRU time", "\n",
        ":GridWeights", "\n",
        ":NumberHRUs  ", nHRU, "\n",
        ":NumberGridCells ", nGridCell, "\n"
    )
    
    write.table(weights, OstrichRVTTemplateFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
    
    cat(file = OstrichRVTTemplateFile, append = T, sep = "",
        ":EndGridWeights", "\n",
        ":EndStationForcing","\n"
    )
    
    
    cat(file = OstrichRVTTemplateFile, append = T, sep = "",
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
    
    write.table(weights, OstrichRVTTemplateFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
    
    cat(file = OstrichRVTTemplateFile, append = T, sep = "",
        ":EndGridWeights", "\n",
        ":EndStationForcing","\n"
    )
    
    cat(file = OstrichRVTTemplateFile, append = T, sep = "",
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
    
    write.table(weights, OstrichRVTTemplateFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
    
    cat(file = OstrichRVTTemplateFile, append = T, sep = "",
        ":EndGridWeights", "\n",
        ":EndStationForcing","\n",
        "\n"
    )
    
    
    print("One or more climate parameters will be included in the calibration...")
  
  } else {
    
    print("No climate parameters will be included in the calibration...")
    
  }
}

