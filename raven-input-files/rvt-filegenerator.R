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

ws.interest <- ws.interest

include.watersheds <- include.watersheds

run.number <- run.number

## Specify the location where the HYDAT database is saved
hydat_here <- "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3"

## Read in the list of WSC stations required to be downloaded. This table must also include associated subbasins and watersheds
download.list <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/WSC_download_list.csv")

## Specify the location RAVEN *.rvt files should be saved
output.location <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"))

## Extract all WSC station numbers for use in hy_daily_flows to retrieve datasets for all
station.no <- download.list[download.list$Watershed %in% paste(include.watersheds, "_Creek", sep = ''), "Station_No"]
  
## Retrieve all available data for all required stations and save in one large tibble.
tmp <- hy_daily_flows(station_number = station.no, hydat_path = hydat_here)

## Execute my custom function which does the follows:
# - A bunch of QA/QC tests written by Rob Chlumsky
# - Isolates all stations for each Watershed
# - Creates ONE "WatershednName-RunNumber.rvt" file for the select watershed. This file contains the file names for individual 
#   *.rvt files for each WSC gauge within the select watershed
# - Creates INDIVIDUAL "subid_wscname.rvt" file for each WSC station and the associated subbasin.

ECflow.rvt.tidy.single.obs(ff = tmp,
                       master = download.list,
                       dir = output.location,
                       include.watersheds = include.watersheds,
                       run.number = run.number,
                       calibration.start = calibration.start,
                       calibration.end = calibration.end,
                       write.redirect = T,
                       flip.number = T)



################################################################################################
##
## Add climate weighting commands to end of master rvt file
##
################################################################################################

## read in RVH file to compute the number of HRUs
HRUs <- rvh.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))

nHRU <- nrow(HRUs$HRUtable)

nGridCell <- 2601

ntime <- 24837

precip.forcing.filename <- "pr.HRU.timeseries.DRAFT.nc"

tasmax.forcing.filename <- "tasmax.HRU.timeseries.DRAFT.nc"

tasmin.forcing.filename <- "tasmin.HRU.timeseries.DRAFT.nc"

weights <- matrix(nrow = nHRU, ncol = 3, data = c(HRU = HRUs$HRUtable$ID,
                                                  Station = HRUs$HRUtable$ID,
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
