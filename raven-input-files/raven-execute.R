################################################################################################################
##
## RUN RAVEN
##
## This script generates all required input data files, and executes Raven for given watershed(s).
##
## Mar-18-2019 LAB
#################################################################################################################

require(doParallel)
require(tools)
require(filesstrings)

cores <- detectCores() - 1

## Start timer
ptm <- proc.time()

## Specify the name to be associated with output files - note that this could be "Multi" if multiple watersheds to be modelled
ws.interest <- "Whiteman"

## Specify the watersheds to be modelled. If multiple, generate a string using c("WS1", "WS2"...WSn")
include.watersheds <- ws.interest

## Specify a run number to associated with outputs
run.number <- "June-18-Calibration"

## Specify whether Ostrich templates and input files should be written for this run
run.ostrich <- TRUE

## Should the global rvh file be regenerated?
recreate.rvh <- FALSE

## Should water demand information be included in the model run?
include.water.demand <- FALSE

## Define the period of calibration
calibration.start <- "2005-01-01"

calibration.end <- "2010-12-31"

## Create a directory within "Simulations" for the model input/output files to be stored
dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), recursive = T)

## Create a README file with a summary of the run
file.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"))

cat(file = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"), append = FALSE, sep = "",
    
      paste("- Run completed on ", Sys.time()), "\n",
      paste("- Run completed by ", Sys.getenv("LOGNAME")), "\n",
      if(recreate.rvh == FALSE){paste("- *.rvh file generated on ", file.info("/var/obwb-hydro-modelling/simulations/Master.rvh")$mtime, " was used for this model run")}
    else {"- New *.rvh file generated"}, "\n",
      if(run.ostrich == FALSE){"- Ostrich was not used for model calibration"}
    else {"- Ostrich was used for model calibration"}, "\n",
      if(include.water.demand == FALSE){"- Water demand data were not included in this model run"}
    else {"- Water demand data were included in this model run"}, "\n",
      
      "- Run completed using climate data last modified as follows:", "\n",
      paste("   - Precipitation: ", file.info("/var/obwb-hydro-modelling/input-data/processed/climate/tasmin.HRU.timeseries.DRAFT.nc")$mtime), "\n",
      paste("   - Maximum Daily Temperature: ", file.info("/var/obwb-hydro-modelling/input-data/processed/climate/tasmax.HRU.timeseries.DRAFT.nc")$mtime), "\n",
      paste("   - Minimum Daily Temperature: ", file.info("/var/obwb-hydro-modelling/input-data/processed/climate/tasmin.HRU.timeseries.DRAFT.nc")$mtime), "\n",
      
      paste("- Calibration was completed for the period", calibration.start, "to", calibration.end), "\n"
    )

#####################################################################
##
## Create required soft links for:
##  - Precipitation netCDF file
##  - Max. Temperature netCDF file
##  - Min. Temperature netCDF file
##
##  - Raven Executable
##
##  - Ostrich executable and save_best.sh script
#####################################################################

file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/pr.HRU.timeseries.DRAFT.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/tasmax.HRU.timeseries.DRAFT.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/tasmin.HRU.timeseries.DRAFT.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))

# file.symlink(from = file.path("/var/obwb-hydro-modelling/src/raven_src.175/src/raven_rev.exe"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path("/var/obwb-hydro-modelling/src/raven_src/src/Raven.exe"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))

if("Ostrich" %in% list.files(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))){print("Ostrich files already exist in this directory...")
} else { 
  file.symlink(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/Linux/openmpi/2.0.2/OstrichMPI"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
  # file.symlink(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/Linux/openmpi/2.0.2/Ostrich"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
  # file.copy(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/save_best.sh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
  print(paste("Ostrich required softlinks created in ", ws.interest, "-", run.number, " directory...", sep = ''))
}



#####################################################################
##
## Generate required input files
##
#####################################################################

source("/var/obwb-hydro-modelling/src/raven-input-files/rvc-filegenerator.R")

## Only recreate the rvh file if necessary. Otherwise "Master.rvh" is copied from parent /simulations directory
if(recreate.rvh == TRUE){
  print("Regenerating master *.rvh file...")
  
  source("/var/obwb-hydro-modelling/src/raven-input-files/rvh-filegenerator.R")
  
  file.copy(from = file.path("/var/obwb-hydro-modelling/simulations/Master.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
  file.rename(from = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Master.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
  
} else {print("Existing rvh file is being used for this model run...")
        file.copy(from = file.path("/var/obwb-hydro-modelling/simulations/Master.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
        file.rename(from = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Master.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
        }

source("/var/obwb-hydro-modelling/src/raven-input-files/rvi-filegenerator.R")

source("/var/obwb-hydro-modelling/src/raven-input-files/rvp-filegenerator.R")

source("/var/obwb-hydro-modelling/src/raven-input-files/rvt-filegenerator.R")

if(run.ostrich == TRUE){
  source("/var/obwb-hydro-modelling/src/ostrich-file-generator.R")
}

#####################################################################
##
## Run Ostrich and/or Raven
##
#####################################################################
if(run.ostrich == TRUE){
  
  ## set working directory to current model run directory
  setwd(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
  
  ## execute RAVEN
  # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "raven_rev.exe"), args = paste(ws.interest, run.number, sep = '-'))
  system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Raven.exe"), args = paste(ws.interest, run.number, sep = '-'))
  
  print("Moving model files to model sub-directory to begin Ostrich calibration...")
  
  ## Generate a list of all files in the current model directory
  files <- list.files(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), full.names = TRUE)
  
  ## Remove rvp and tpl files from the list
  move.files <- files[file_ext(files) != "rvp" & file_ext(files) != "tpl" & file_ext(files) != "sh" & file_ext(files) != "txt" & file_ext(files) != ""] 
  # &
                        # files != file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""))]
  
  ## create a "model" sub-directory
  dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"))
  
  ## move all files except rvp and tpl files to "model" sub-directory
  file.move(move.files, file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"))
  
  print("Beginning Ostrich Calibration...")
  
  system2("/usr/bin/mpirun",args = paste("-n", cores, "OstrichMPI"))
  # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Ostrich"))
  
  
  
  
  #####################################################################
  ##
  ## Plot simulated vs. Observed flows where observed flows exist
  ##
  #####################################################################
  require(RavenR)
  
  hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model/", paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
  
  ## Identify which columns have obsrved data available
  subs.obs <- colnames(hydrographs$hyd[,hydrographs$obs.flag == TRUE])
  
  ## Remove NA
  subs.obs <- subs.obs[!is.na(subs.obs)]
  
  ## remove the "_obs" characters to allow successful extraction
  my.subs <- gsub("_obs", "", subs.obs)
  
  
  pdf(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model/", paste(ws.interest, "-", run.number, "-Output.pdf", sep = "")), width = 8.5, height = 11)
  
  par(mfrow = c(1,1))
  
  ## Generate a plot for all subbasins which have observed data available
  for(i in 1:length(my.subs)){
    x <- hyd.extract(subs = my.subs[i], hydrographs)
    hyd.plot(x$sim, x$obs, precip = hydrographs$hyd$precip)
    title(my.subs[i])
  }
  
  
  
  ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model/", paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))
  
  par(mfrow = c(4, 1), mar= c(2,4,2,2))
  
  for(i in 4:ncol(ws.storage)){
    
    plot(ws.storage[,i], type = 'l', main = colnames(ws.storage[i]))
    
  }
  
  dev.off()
  
#####################################################################
##
## Run Raven executable with improved parameter values, or with base value is run.ostrich == FALSE
##
#####################################################################

} else {
  
  setwd(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
  
  # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "raven_rev.exe"), args = paste(ws.interest, run.number, sep = '-'))
  system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Raven.exe"), args = paste(ws.interest, run.number, sep = '-'))
  
  print("Ostrich was not used for model calibration during this run...")
  
  #####################################################################
  ##
  ## Plot simulated vs. Observed flows where observed flows exist
  ##
  #####################################################################
  require(RavenR)
  
  hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
  
  ## Identify which columns have obsrved data available
  subs.obs <- colnames(hydrographs$hyd[,hydrographs$obs.flag == TRUE])
  
  ## Remove NA
  subs.obs <- subs.obs[!is.na(subs.obs)]
  
  ## remove the "_obs" characters to allow successful extraction
  my.subs <- gsub("_obs", "", subs.obs)
  
  
  pdf(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "-Output.pdf", sep = "")), width = 8.5, height = 11)
  
  par(mfrow = c(1,1))
  
  ## Generate a plot for all subbasins which have observed data available
  for(i in 1:length(my.subs)){
    x <- hyd.extract(subs = my.subs[i], hydrographs)
    hyd.plot(x$sim, x$obs, precip = hydrographs$hyd$precip)
    title(my.subs[i])
  }
  
  
  
  ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))
  
  par(mfrow = c(4, 1), mar= c(2,4,2,2))
  
  for(i in 4:ncol(ws.storage)){
    
    plot(ws.storage[,i], type = 'l', main = colnames(ws.storage[i]))
    
  }
  
  dev.off()
  
  }

## end timer
proc.time() - ptm


# #####################################################################
# ##
# ## Plot simulated vs. Observed flows where observed flows exist
# ##
# #####################################################################
# require(RavenR)
# 
# hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
# 
# ## Identify which columns have obsrved data available
# subs.obs <- colnames(hydrographs$hyd[,hydrographs$obs.flag == TRUE])
# 
# ## Remove NA
# subs.obs <- subs.obs[!is.na(subs.obs)]
# 
# ## remove the "_obs" characters to allow successful extraction
# my.subs <- gsub("_obs", "", subs.obs)
# 
# 
# pdf(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "-Output.pdf", sep = "")), width = 8.5, height = 11)
# 
# par(mfrow = c(1,1))
# 
# ## Generate a plot for all subbasins which have observed data available
# for(i in 1:length(my.subs)){
#   x <- hyd.extract(subs = my.subs[i], hydrographs)
#   hyd.plot(x$sim, x$obs, precip = hydrographs$hyd$precip)
#   title(my.subs[i])
# }
# 
# 
# 
# ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_WatershedStorage.csv", sep = "")))
# 
# par(mfrow = c(4, 1), mar= c(2,4,2,2))
# 
# for(i in 4:ncol(ws.storage)){
#   
#    plot(ws.storage[,i], type = 'l', main = colnames(ws.storage[i]))
#   
# }
# 
# dev.off()

###########################################
##
## 

# hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste("processor_2/model/Whiteman-Apr-24-19_Hydrographs.csv", sep = "")))

# ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste("processor_2/model/Whiteman-Apr-24-19_WatershedStorage.csv", sep = "")))
