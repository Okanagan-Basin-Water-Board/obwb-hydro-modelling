################################################################################################################
##
## RUN RAVEN
##
## This script generates all required input data files, and executes Raven for given watershed(s).
##
## Mar-18-2019 LAB
#################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

require(conflicted)
require(doParallel)
require(tools)
require(filesstrings)
require(mailR)
require(data.table)

cores <- detectCores() - 1

## Start timer
ptm <- proc.time()

## Specify the name to be associated with output files - note that this could be "Multi" if multiple watersheds to be modelled. Spaces must be omitted.
ws.interest <- "Andras-functions"

## Specify the watersheds to be modelled. If multiple, generate a string using c("WS1", "WS2"...WSn")
# include.watersheds <- c("Coldstream", "Equesis", "Inkaneep", "McDougall", "McLean", "Mill", "Mission", "Naramata", "Naswhito", "Penticton", "Powers", "Shingle", "Shorts", "Shuttleworth", "Trepanier", "Trout", "Vaseux", "Vernon", "Whiteman")
# include.watersheds <- c("Whiteman", "Trout", "Coldstream", "Vaseux")
include.watersheds <- "Mill"

## Specify a run number to associated with outputs
run.number <- "Mill-03"

## Add comments to README file.
run.comments <- "Working on writing of ReservoirDemandMultiplier under difference calibration scenarios"

# Specify individual subbasins that should be disabled (e.g., Lambly Lake & Contributing area under natural conditions, and all diversions)
disable.subbasins <- c(2407, 2408, 2423, 2422, 2421, 1421, 256)
# disable.subbasins <- c(2423, 2422, 2421, 1421, 256) # For Residuals, only disable the diversions.

## Specify whether Ostrich templates and input files should be written for this run
run.ostrich <- FALSE

## Specify whether the model is being run for validation purposes
validate.model <- FALSE

## Should the global rvh files be regenerated?
recreate.rvh <- FALSE

## Should water demand information be included in the model run?
include.water.demand <- FALSE

## Define the date that water demand should begin.
demand.start.date <- "1996-01-01"

# Should reservoir parameters be included in the calibration?
calibrate.reservoir.parameters <- FALSE

# Should individual reservoir demand multipliers be calibrated?
calibrate.reservoir.supply <- FALSE

## Should reservoirs be managed to satisfy downstream demand?
manage.reservoirs <- FALSE

## Should soil thicknesses be calibrated?
calibrate.soil.thicknesses <- FALSE

## Should diversion volumes for the given watershed(s) be calculated?
calculate.diversions <- FALSE

## Define the period of calibration / diagnostics
calibration.start <- "1996-01-01"

calibration.end <- "2010-12-31"

## Define the period of validation / diagnostics
validation.start <- "2011-01-01"

validation.end <- "2017-12-31"

## Specify the version of climate data to use - ensure that this matches the current Master_XXX.rvh file
# Climate.Version.Tag <- "V1.0.1"


if(manage.reservoirs == TRUE & include.water.demand == FALSE){stop("In order to manage reservoirs to satisfy downstream demand, water demand must be included in the model run. Set include.water.demand == TRUE")}

if(manage.reservoirs != TRUE & calibrate.reservoir.supply == TRUE){stop("In order to calibrate reservoir supply, manage.reservoirs must be TRUE.")}

if(length(disable.subbasins) > 0){warning(paste("The following subbasins are disabled: ", paste(disable.subbasins, collapse = ", "), ". Ensure that this is correct.", sep = ""))}

if(include.water.demand == TRUE){print(paste("Water demand is included from", demand.start.date, "onwards."))}

#####################################################################
##
## Set-up directory (and sub-directories [if needed]) to store model input/output files
##
#####################################################################

## Create a directory within "Simulations" for the model input/output files to be stored
dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")), recursive = T)

## If run.ostrich == TRUE, create required sub-directories to store templates and model files.
if(run.ostrich == TRUE){

  ## create a "model" sub-directory
  dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "model"))

  dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates"))

}

#####################################################################
##
## Create a README file with a summary of the run
##
#####################################################################

file.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"))

cat(file = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"), append = F, sep = "",

      paste("- Run completed on ", Sys.time()), "\n",
      paste("- Run completed by ", Sys.getenv("LOGNAME")), "\n",
      if(recreate.rvh == FALSE){paste("- *.rvh file generated on ", file.info(file.path(global.simulation.dir,  master.natural.rvh.file))$mtime, " was used for this model run")} # Note: Master_natural.rvh and Master_residual.rvh are generated at the same time, so the file.info is the same, regardless of which one is used for the current model run.
    else {"- New *.rvh file generated"}, "\n",
      if(run.ostrich == FALSE){"- Ostrich was not used for model calibration"}
    else {"- Ostrich was used for model calibration"}, "\n",
      if(include.water.demand == FALSE){"- Water demand data were not included in this model run"}
    else {"- Water demand data were included in this model run"}, "\n",

      "- Run completed using climate data last modified as follows:", "\n",
      paste("   - Precipitation: ", file.info(file.path(global.input.dir, processed.climate.dir, precip.processed.file))$mtime), "\n",
      paste("   - Maximum Daily Temperature: ", file.info(file.path(global.input.dir, processed.climate.dir, tasmax.processed.file))$mtime), "\n",
      paste("   - Minimum Daily Temperature: ", file.info(file.path(global.input.dir, processed.climate.dir, precip.processed.file))$mtime), "\n",

      paste("- Model Diagnostics were calculated for the period", calibration.start, "to", calibration.end), "\n",
    "\n", 
      run.comments, "\n",
    "\n",
    "-------------------- R OUTPUT --------------------",
    "\n"
    )


## Dump all R output to the same Read me file
sink(file = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"), append = T,
     type = c("output", "message"),
     split = T)

#####################################################################
##
## Create required soft links for:
##  - Precipitation netCDF file
##  - Max. Temperature netCDF file
##  - Min. Temperature netCDF file
##
##  - Raven Executable
##
##  - Ostrich executable (if required)
#####################################################################

file.symlink(from = file.path(global.input.dir, processed.climate.dir, precip.processed.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path(global.input.dir, processed.climate.dir, tasmax.processed.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path(global.input.dir, processed.climate.dir, tasmin.processed.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))

# file.symlink(from = file.path("/var/obwb-hydro-modelling/src/raven_src.175/src/raven_rev.exe"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
file.symlink(from = file.path(global.src.dir, raven.executable.directory, raven.executable.name), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))

## If run.ostrich == TRUE, create Ostrich softlink in the model directory
if(run.ostrich == TRUE){
  file.symlink(from = file.path(global.src.dir, ostrich.executable.directory, ostrich.executable.name), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
  file.symlink(from = file.path(global.src.dir, ostrich.executable.directory, ostrich.parallel.executable.name), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
  # file.copy(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/save_best.sh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
}

#####################################################################
##
## Copy input template fies into the run directory to track changes to RVP-template.csv and RVI-template.csv
##
#####################################################################

dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"), recursive = T)

file.copy(from = file.path(global.input.dir, raw.parameter.codes.in.dir, RVP.template.in.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"))
file.copy(from = file.path(global.input.dir, raw.parameter.codes.in.dir, RVI.template.in.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"))

if(run.ostrich == T){
  
  file.copy(from = file.path(global.input.dir, raw.parameter.codes.in.dir, OST.template.in.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"))
  
}


#####################################################################
##
## Generate required input files
##
#####################################################################

# #RVP ADJUSTMENT SCRIPT HERE
# if(run.ostrich == TRUE & include.water.demand == TRUE & manage.reservoirs == TRUE & calibrate.reservoir.parameters == FALSE & calibrate.soil.thicknesses == FALSE){
#   
#   source("/var/obwb-hydro-modelling/src/rvp-template-residual-adjustor.R")
#   
# }



source(file.path(global.src.dir, "raven-input-files/rvc-filegenerator.R"))
  
## Only recreate the rvh file if necessary. Otherwise "Master.rvh" is copied from parent /simulations directory
if(recreate.rvh == TRUE){
  print("Regenerating master *.rvh file...")

  source(file.path(global.src.dir, "raven-input-files/rvh-filegenerator.R"))
  
  ## With update to remotely defined file paths, if recreate.rvh == TRUE, users must redefine the rvh filenames for use in the file-config file.
  stop("RVH files have been updated. Update the relevant *.RVH file names in the file-config.R file.")

  # if(include.water.demand == TRUE){
  #   
  #   print("Existing RESIDUAL *.rvh file being used for this model run...")
  #   file.copy(from = file.path(global.simulation.dir, master.residual.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
  #   file.rename(from = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), master.residual.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
  #   
  # } else {
  #   
  #   print("Existing NATURAL *.rvh file being used for this model run...")
  #   file.copy(from = file.path(global.simulation.dir, master.natural.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
  #   file.rename(from = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), master.natural.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
  #   
  # }
  
} else {if(include.water.demand == TRUE){
  
  print("Existing RESIDUAL *.rvh file being used for this model run...")
  file.copy(from = file.path(global.simulation.dir, master.residual.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
  file.rename(from = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), master.residual.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))

} else {

  print("Existing NATURAL *.rvh file being used for this model run...")
  file.copy(from = file.path(global.simulation.dir, master.natural.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
  file.rename(from = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), master.natural.rvh.file), to = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))

  }
}


source(file.path(global.src.dir, "raven-input-files/rvi-filegenerator.R"))

source(file.path(global.src.dir, "raven-input-files/rvp-filegenerator.R"))

source(file.path(global.src.dir, "raven-input-files/rvt-filegenerator.R"))

source(file.path(global.src.dir, "raven-input-files/snow-rvt-filegenerator.R"))

source(file.path(global.src.dir, "naturalized-flows/nat-flow-disaggregation.R"))

source(file.path(global.src.dir, "raven-input-files/reservoir-rvh-rvt-filegenerator.R"))

source(file.path(global.src.dir, "raven-input-files/custom-appendages.R"))

if(include.water.demand == TRUE){
  
  source(file.path(global.src.dir, "raven-input-files/owdm-rvt-filegenerator.R"))
  
  source(file.path(global.src.dir, "raven-input-files/custom-rvt-filegenerator.R"))
  
}


#####################################################################
##
## Run Ostrich and/or Raven
##
#####################################################################
if(run.ostrich == TRUE & exists("stations.included") == TRUE){
  
  ## Request user input on which WSC station the model should be calibrated to.
  source(file.path(global.src.dir, "calibration-select.R"))
  # 
  # calibration.stations <-c("HYDROGRAPH_Powers_EPP_Irr_08NM570")
  # # 
  # calibration.station.weights <- c(1)
  # 
  ## set working directory to current model run directory
  setwd(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))

  ## execute RAVEN
  # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "raven_rev.exe"), args = paste(ws.interest, run.number, sep = '-'))
  system2(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), raven.executable.name), args = paste(ws.interest, run.number, sep = '-'))

  ## Generate the Ostrich Input file
  source(file.path(global.src.dir, "ostrich-file-generator.R"))

  print("Moving model files to model sub-directory to begin Ostrich calibration...")

  ## Generate a list of all files in the current model directory
  files <- list.files(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")), full.names = TRUE)

  ## Remove bash fies, text files, and folders from the list
  move.files <- files[file_ext(files) != "sh" & file_ext(files) != "txt" & file_ext(files) != ""]
  
  ## Remove "master" templates (i.e., any of the 5 required input files) from the list so they're not moved. These files are included in "do.not.move" which is defined in ostrich-file-generator.R
  move.files <- move.files[!move.files %in% file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), do.not.move)]
  
  ## move all files except rvp and tpl files to "model" sub-directory
  file.move(move.files, file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "model"))

  ## If reservoirs are included in the model, move the reservoirs folder into the model folder
  if(dir.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs"))){
    system2("mv", paste(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs"), file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
  }
  
  ## If daily naturalized flows folder exists, move it to the model folder
  if(dir.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "daily_naturalized_flows"))){
    system2("mv", paste(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "daily_naturalized_flows"), file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
  }
  
  ## If the owdm folder exists, move it to the model folder
  if(dir.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm"))){
    system2("mv", paste(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm"), file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
  }
  
  ## If the custom_timeseries folder exists, move it to the model folder
  if(dir.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "custom_timeseries"))){
    system2("mv", paste(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "custom_timeseries"), file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
  }
  

  print("Beginning Ostrich Calibration...")
  
  # save.image("/var/obwb-hydro-modelling/simulations/Manual-Calibrations/Manual-Calibrations-MAX_LAI/pre-calibration-image.RData")

  ## TODO: Confirm that this directory location is correct and needed - can OstrichMPI be run from the simulation directory?
  system2(mpirun.dir ,args = paste("-n", cores, ostrich.parallel.executable.name))
  # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Ostrich"))


  #####################################################################
  ##
  ## Plot a series of model results
  ##
  #####################################################################
  
  ## Remove the :SuppressOutput command from the RvI file
  rewrite.output(ws.interest, run.number)
  
  ## Change the directoyr into the final calibration directory
  setwd(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model"))
  
  ## Complete one more model run to write all model output
  system2(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", "Raven.exe"), args = paste(ws.interest, run.number, sep = '-'))
  
  
  
  require(RavenR)

  ## Generate a pdf of results
  pdf(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "-Output.pdf", sep = "")), width = 8.5, height = 11)

  plot.calibration.results(ws.interest, run.number, subbasins.present)

  source(file.path(global.src.dir, "naturalized-flows/naturalized-flow-processing.R"))
    
  dev.off()

  # aggregate the model output by Year, Year-month, Year-AWDM-week, and Year-ISO-Week.
  # By default, it will aggregate for the full model period, for all years, months, and weeks.
  # Set time groups that aren't wanted = NULL, e.g., years = NULL
  aggregate.output(ws.interest, run.number, subbasins.present,
                   AWDM.weeks = c(1:52), ISO.weeks = c(1:52), months = c(1:12), years = c(1996:2010))
 
  ## If calculate.diversions = TRUE, calculate the diversion timeseries for the given watershed(s)
  if(calculate.diversions == TRUE){
    
    source(file.path(global.src.dir, "diverted-flows-calculation.R"))
      
  }
  
  
  send.mail(from = "birdl@ae.ca",
            to =  "birdl@ae.ca",
            subject = "Calibration Complete",
            body = paste("Model run", run.number, "has completed. The VM has been turned off."),
            authenticate = TRUE,
            smtp = list(host.name = "smtp.office365.com",
                        port = 587,
                        user.name = "birdl@ae.ca",
                        passwd = "Winter202",
                        tls = TRUE),
            attach.files = file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = "")))
  
  
  ## Shutdown the VM.
  # system2("sudo", args = "shutdown -h now")
  
#####################################################################
##
## Run Raven executable with improved parameter values, or with base value is run.ostrich == FALSE
##
#####################################################################

} else {
  
  if(run.ostrich == TRUE){
    
    print(paste("There are no WSC stations within the ", include.watersheds, " Creek watershed(s). No calibration is possible. One execution of Raven will be completed.", sep = ""))
    
  }
  
  setwd(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-")))
  
  # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "raven_rev.exe"), args = paste(ws.interest, run.number, sep = '-'))
  system2(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), raven.executable.name), args = paste(ws.interest, run.number, sep = '-'), wait = F)
  
  
  print("Ostrich was not used for model calibration during this run...")
  
  #####################################################################
  ##
  ## Plot a series of model results
  ##
  #####################################################################
  require(RavenR)
  
  ## Generate a pdf of results
  pdf(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "-Output.pdf", sep = "")), width = 8.5, height = 11)

  plot.results(ws.interest, run.number, subbasins.present)
  
  # aggregate the model output by Year, Year-month, Year-AWDM-week, and Year-ISO-Week.
  # By default, it will aggregate for the full model period, for all years, months, and weeks.
  # Set time groups that aren't wanted = NULL, e.g., years = NULL
  aggregate.output(ws.interest, run.number, subbasins.present,
                   AWDM.weeks = c(1:52), ISO.weeks = c(1:52), months = c(1:12), years = c(1996:2010))
  
  
  source(file.path(global.src.dir, "naturalized-flows/naturalized-flow-processing.R"))
    
  dev.off()
  
  ## If calculate.diversions = TRUE, calculate the diversion timeseries for the given watershed(s)
  if(calculate.diversions == TRUE){
    
    source(file.path(global.src.dir, "diverted-flows-calculation.R"))
      
  }
  
  }

  
  ## end timer
proc.time() - ptm

## Close sink() connection so no more output is written to file.
sink(NULL)

# hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste("processor_2/model/Whiteman-Apr-24-19_Hydrographs.csv", sep = "")))

# ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste("processor_2/model/Whiteman-Apr-24-19_WatershedStorage.csv", sep = "")))


# reservoirs <- res.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))
# res.plot(reservoirs$res$Mission_Creek219, zero.axis = F)
