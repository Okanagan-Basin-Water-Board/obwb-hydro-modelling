#########################################################################################################################################
##
## This script is a scratch pad for generating required numbers for the model report
##
## 11-Dec-2019 LAB
##
#########################################################################################################################################

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

