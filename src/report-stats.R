#########################################################################################################################################
##
## This script is a scratch pad for generating required numbers for the model report
##
## 11-Dec-2019 LAB
##
#########################################################################################################################################

## ----------------------------------------------------------------------------------------
##
## Determine number of HRUs/Subbasins/Reservoirs in each watershed
##
## ----------------------------------------------------------------------------------------

require(RavenR)

subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

HRUs <- rvh.read("/var/obwb-hydro-modelling/simulations/Master_residual.rvh")

HRUs <- HRUs$HRUtable

watersheds <- unique(subbasin.codes$GNIS_NAME)

results <- data.frame(watershed = watersheds,
                      subbasins = NA,
                      reservoirs = NA,
                      HRUs = NA)

for(i in 1:length(watersheds)){
  
  subs <- subbasin.codes[subbasin.codes$GNIS_NAME == watersheds[i], "Subbasin_ID"]
  
  reservoirs <- subbasin.codes[subbasin.codes$GNIS_NAME == watersheds[i], "Reservoir_name"]
  
  watershed.HRUs <- HRUs[HRUs$SBID %in% subs, ]  
  
  results[i,2] <- length(subs)
  
  results[i,3] <- length(reservoirs[reservoirs != "<Null>"])
  
  results[i,4] <- nrow(watershed.HRUs)
  
  
}



## ----------------------------------------------------------------------------------------
##
## Retrieve all information for all WSC stations included
##
## ----------------------------------------------------------------------------------------

require(tidyhydat)

hydat_here <- "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3"

subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

stations <- subbasin.codes[subbasin.codes$Hydrometric_stn != "<Null>" , "Hydrometric_stn"]

wsc_station_summaries <- hy_stations(stations, hydat_path = hydat_here)

wsc_station_data <- hy_stn_data_range(stations, hydat_path = hydat_here)

wsc_station_regulation <- hy_stn_regulation(stations, hydat_path = hydat_here)
