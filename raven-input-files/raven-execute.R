################################################################################################################
##
## RUN RAVEN
##
## This script generates all required input data files, and executes Raven for given watershed(s).
##
## Mar-18-2019 LAB
################################################################################################################

## Specify the name to be associated with output files - note that this could be "Multi" if multiple watersheds to be modelled
ws.interest <- "Whiteman"

## Specify the watersheds to be modelled. IF multiple, generate a string using c("WS1", "WS2"...WSn")
include.watersheds <- ws.interest

## Specify a run number to associated with outputs
run.number <- 2

## Create a directory within "Simulations" for the model input/output files to be stored
dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), recursive = T)

#####################################################################
##
## Check that OSTRICH softlink exists in the watershed parent directory
##
#####################################################################

if("Ostrich" %in% list.files(file.path("/var/obwb-hydro-modelling/simulations", ws.interest))){print("Ostrich already exists in this directory...")
  } else { 
    file.symlink(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/Linux/openmpi/2.0.2/Ostrich"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest))
    print("Ostrich softlink created to executable at /var/obwb-hydro-modelling/src/ostrich_src/Linux/openmpi/2.0.2...")
    }

#####################################################################
##
## Create required soft links for:
##  - Precipitation netCDF file
##  - Max. Temperature netCDF file
##  - Min. Temperature netCDF file
##
##  - Raven Executable
#####################################################################

file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/pr.HRU.timeseries.DRAFT.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/tasmax.HRU.timeseries.DRAFT.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/tasmin.HRU.timeseries.DRAFT.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))

file.symlink(from = file.path("/var/obwb-hydro-modelling/src/raven_src/src/raven_rev.exe"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))

#####################################################################
##
## Generate required input files
##
#####################################################################

source("/var/obwb-hydro-modelling/src/raven-input-files-2/rvc-filegenerator.R")

source("/var/obwb-hydro-modelling/src/raven-input-files-2/rvh-filegenerator.R")

source("/var/obwb-hydro-modelling/src/raven-input-files-2/rvi-filegenerator.R")

source("/var/obwb-hydro-modelling/src/raven-input-files-2/rvp-filegenerator.R")

source("/var/obwb-hydro-modelling/src/raven-input-files-2/rvt-filegenerator.R")


#####################################################################
##
## Run Raven executable
##
#####################################################################

setwd(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))

system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "raven_rev.exe"), args = paste(ws.interest, run.number, sep = '-'))
       