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
require(mailR)
require(data.table)

cores <- detectCores() - 1
# 
# ## Start timer
# ptm <- proc.time()
# 
# ## Specify the name to be associated with output files - note that this could be "Multi" if multiple watersheds to be modelled
# ws.interest <- "Global-Snow-Calibration"
# 
# ## Specify the watersheds to be modelled. If multiple, generate a string using c("WS1", "WS2"...WSn")
# include.watersheds <- c("Coldstream", "Equesis", "Inkaneep", "McDougall", "McLean", "Mill", "Mission", "Naramata", "Naswhito", "Penticton", "Powers", "Shingle", "Shorts", "Shuttleworth", "Trepanier", "Trout", "Vaseux", "Vernon", "Whiteman")
# # include.watersheds <- c("Whiteman", "Trout", "Coldstream", "Vaseux")
# # include.watersheds <- "Vaseux"
# 
# ## Specify a run number to associated with outputs
# run.number <- "Global-Snow-Calibration-All-Data"
# 
# ## Add comments to README file.
# run.comments <- "Global Snow Calibration using all snow course and snow pillow sites. Initial values set from output of previous global snow calibration. No soils or reservoirs included in calibration."
# 
# ## Specify individual subbasins that should be disabled (e.g., Lambly Lake & Contributing area under natural conditions, and all diversions)
# snow.courses <- read.csv("/var/obwb-hydro-modelling/input-data/processed/spatial/snow/snow-course-locations-model-domain.csv")
# snow.pillows <- read.csv("/var/obwb-hydro-modelling/input-data/processed/spatial/snow/snow-pillow-locations-model-domain.csv")
# subbasins <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")
# include.subbasins <- c(snow.courses$Subbasin_ID, snow.pillows$Subbasin_ID)
# remove.subbasins <- subbasins[which(!as.character(subbasins$Subbasin_ID) %in% as.character(include.subbasins)), "Subbasin_ID"]
# 
# disable.subbasins <- remove.subbasins
# 
# ## Specify whether Ostrich templates and input files should be written for this run
# run.ostrich <- TRUE
# 
# ## Specify whether the model is being run for validation purposes
# validate.model <- FALSE
# 
# ## Should the global rvh files be regenerated?
# recreate.rvh <- FALSE
# 
# ## Should water demand information be included in the model run?
# include.water.demand <- FALSE
# 
# # Should reservoir parameters be included in the calibration?
# calibrate.reservoirs <- FALSE
# 
# ## Should reservoirs be managed to satisfy downstream demand?
# manage.reservoirs <- FALSE
# 
# ## Should soil thicknesses be calibrated?
# calibrate.soil.thicknesses <- FALSE
# 
# ## Define the period of calibration / diagnostics
# calibration.start <- "1996-01-01"
# 
# calibration.end <- "2010-12-31"
# 
# ## Define the period of validation / diagnostics
# validation.start <- "2011-01-01"
# 
# validation.end <- "2017-12-31"
# 
# 
# if(manage.reservoirs == TRUE & include.water.demand == FALSE){stop("In order to manage reservoirs to satisfy downstream demand, water demand must be included in the model run. Set include.water.demand == TRUE")}
# 
# if(length(disable.subbasins) > 0){warning(paste("The following subbasins are disabled: ", paste(disable.subbasins, collapse = ", "), ". Ensure that this is correct.", sep = ""))}
# 
# #####################################################################
# ##
# ## Set-up directory (and sub-directories [if needed]) to store model input/output files
# ##
# #####################################################################
# 
# ## Create a directory within "Simulations" for the model input/output files to be stored
# dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), recursive = T)
# 
# ## If run.ostrich == TRUE, create required sub-directories to store templates and model files.
# if(run.ostrich == TRUE){
# 
#   ## create a "model" sub-directory
#   dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"))
# 
#   dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "templates"))
# 
# }
# 
# #####################################################################
# ##
# ## Create a README file with a summary of the run
# ##
# #####################################################################
# 
# file.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"))
# 
# cat(file = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"), append = F, sep = "",
# 
#       paste("- Run completed on ", Sys.time()), "\n",
#       paste("- Run completed by ", Sys.getenv("LOGNAME")), "\n",
#       if(recreate.rvh == FALSE){paste("- *.rvh file generated on ", file.info("/var/obwb-hydro-modelling/simulations/Master_natural.rvh")$mtime, " was used for this model run")} # Note: Master_natural.rvh and Master_residual.rvh are generated at the same time, so the file.info is the same, regardless of which one is used for the current model run.
#     else {"- New *.rvh file generated"}, "\n",
#       if(run.ostrich == FALSE){"- Ostrich was not used for model calibration"}
#     else {"- Ostrich was used for model calibration"}, "\n",
#       if(include.water.demand == FALSE){"- Water demand data were not included in this model run"}
#     else {"- Water demand data were included in this model run"}, "\n",
# 
#       "- Run completed using climate data last modified as follows:", "\n",
#       paste("   - Precipitation: ", file.info("/var/obwb-hydro-modelling/input-data/processed/climate/pr.HRU.timeseries.V1.nc")$mtime), "\n",
#       paste("   - Maximum Daily Temperature: ", file.info("/var/obwb-hydro-modelling/input-data/processed/climate/tasmax.HRU.timeseries.V1.nc")$mtime), "\n",
#       paste("   - Minimum Daily Temperature: ", file.info("/var/obwb-hydro-modelling/input-data/processed/climate/tasmin.HRU.timeseries.V1.nc")$mtime), "\n",
# 
#       paste("- Model Diagnostics were calculated for the period", calibration.start, "to", calibration.end), "\n",
#     "\n", 
#       run.comments, "\n",
#     "\n",
#     "-------------------- R OUTPUT --------------------",
#     "\n"
#     )
# 
# 
# ## Dump all R output to the same Read me file
# sink(file = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "README.txt"), append = T,
#      type = c("output", "message"),
#      split = T)
# 
# #####################################################################
# ##
# ## Create required soft links for:
# ##  - Precipitation netCDF file
# ##  - Max. Temperature netCDF file
# ##  - Min. Temperature netCDF file
# ##
# ##  - Raven Executable
# ##
# ##  - Ostrich executable (if required)
# #####################################################################
# 
# file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/pr.HRU.timeseries.V1.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
# file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/tasmax.HRU.timeseries.V1.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
# file.symlink(from = file.path("/var/obwb-hydro-modelling/input-data/processed/climate/tasmin.HRU.timeseries.V1.nc"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
# 
# # file.symlink(from = file.path("/var/obwb-hydro-modelling/src/raven_src.175/src/raven_rev.exe"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
# file.symlink(from = file.path("/var/obwb-hydro-modelling/src/raven_src/src/Raven.exe"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
# 
# ## If run.ostrich == TRUE, create Ostrich softlink in the model directory
# if(run.ostrich == TRUE){
#   file.symlink(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/Linux/openmpi/2.0.2/OstrichMPI"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
#   file.symlink(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/Linux/openmpi/2.0.2/Ostrich"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
#   # file.copy(from = file.path("/var/obwb-hydro-modelling/src/ostrich_src/save_best.sh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
# }
# 
# #####################################################################
# ##
# ## Copy input template fies into the run directory to track changes to RVP-template.csv and RVI-template.csv
# ##
# #####################################################################
# 
# dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"), recursive = T)
# 
# file.copy(from = file.path("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"))
# file.copy(from = file.path("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVI-Template.csv"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"))
# 
# if(run.ostrich == T){
#   
#   file.copy(from = file.path("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/OST-Template.csv"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "input-templates"))
#   
# }
# 
# 
# #####################################################################
# ##
# ## Generate required input files
# ##
# #####################################################################
# 
# source("/var/obwb-hydro-modelling/src/raven-input-files/rvc-filegenerator.R")
# 
# ## Only recreate the rvh file if necessary. Otherwise "Master.rvh" is copied from parent /simulations directory
# if(recreate.rvh == TRUE){
#   print("Regenerating master *.rvh file...")
# 
#   source("/var/obwb-hydro-modelling/src/raven-input-files/rvh-filegenerator.R")
# 
#   if(include.water.demand == TRUE){
#     
#     print("Existing RESIDUAL *.rvh file being used for this model run...")
#     file.copy(from = file.path("/var/obwb-hydro-modelling/simulations/Master_residual.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
#     file.rename(from = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Master_residual.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
#     
#   } else {
#     
#     print("Existing NATURAL *.rvh file being used for this model run...")
#     file.copy(from = file.path("/var/obwb-hydro-modelling/simulations/Master_natural.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
#     file.rename(from = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Master_natural.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
#     
#   }
#   
# } else {if(include.water.demand == TRUE){
#   
#   print("Existing RESIDUAL *.rvh file being used for this model run...")
#   file.copy(from = file.path("/var/obwb-hydro-modelling/simulations/Master_residual.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
#   file.rename(from = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Master_residual.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
# 
# } else {
# 
#   print("Existing NATURAL *.rvh file being used for this model run...")
#   file.copy(from = file.path("/var/obwb-hydro-modelling/simulations/Master_natural.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
#   file.rename(from = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Master_natural.rvh"), to = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvh", sep = "")))
# 
#   }
# }
# 
# 
# source("/var/obwb-hydro-modelling/src/raven-input-files/rvi-filegenerator.R")
# 
# source("/var/obwb-hydro-modelling/src/raven-input-files/rvp-filegenerator.R")
# 
# source("/var/obwb-hydro-modelling/src/raven-input-files/rvt-filegenerator.R")
# 
# source("/var/obwb-hydro-modelling/src/raven-input-files/snow-rvt-filegenerator.R")
# 
# source("/var/obwb-hydro-modelling/src/naturalized-flows/nat-flow-disaggregation.R")
# 
# source("/var/obwb-hydro-modelling/src/raven-input-files/reservoir-rvh-rvt-filegenerator.R")
# 
# source("/var/obwb-hydro-modelling/src/raven-input-files/custom-appendages.R")
# 
# if(include.water.demand == TRUE){
#   
#   source("/var/obwb-hydro-modelling/src/raven-input-files/owdm-rvt-filegenerator.R")
#   
#   source("/var/obwb-hydro-modelling/src/raven-input-files/custom-rvt-filegenerator.R")
#   
# }
# 
# 
# #####################################################################
# ##
# ## Run Ostrich and/or Raven
# ##
# #####################################################################
# if(run.ostrich == TRUE & exists("stations.included") == TRUE){
#   
#   ## Request user input on which WSC station the model should be calibrated to.
#   source("/var/obwb-hydro-modelling/src/calibration-select.R")
# 
#   ## set working directory to current model run directory
#   setwd(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
# 
#   ## execute RAVEN
#   # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "raven_rev.exe"), args = paste(ws.interest, run.number, sep = '-'))
#   system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Raven.exe"), args = paste(ws.interest, run.number, sep = '-'))
# 
# 
#   ## Generate the Ostrich Input file
#   source("/var/obwb-hydro-modelling/src/ostrich-file-generator.R")
# 
#   print("Moving model files to model sub-directory to begin Ostrich calibration...")
# 
#   ## Generate a list of all files in the current model directory
#   files <- list.files(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), full.names = TRUE)
# 
#   ## Remove bash fies, text files, and folders from the list
#   move.files <- files[file_ext(files) != "sh" & file_ext(files) != "txt" & file_ext(files) != ""]
#   
#   ## Remove "master" templates (i.e., any of the 5 required input files) from the list so they're not moved. These files are included in "do.not.move" which is defined in ostrich-file-generator.R
#   move.files <- move.files[!move.files %in% file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), do.not.move)]
#   
#   ## move all files except rvp and tpl files to "model" sub-directory
#   file.move(move.files, file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"))
# 
#   ## If reservoirs are included in the model, move the reservoirs folder into the model folder
#   if(dir.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs"))){
#     system2("mv", paste(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "reservoirs"), file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
#   }
#   
#   ## If daily naturalized flows folder exists, move it to the model folder
#   if(dir.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "daily_naturalized_flows"))){
#     system2("mv", paste(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "daily_naturalized_flows"), file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
#   }
#   
#   ## If the owdm folder exists, move it to the model folder
#   if(dir.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm"))){
#     system2("mv", paste(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm"), file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
#   }
#   
#   ## If the custom_timeseries folder exists, move it to the model folder
#   if(dir.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "custom_timeseries"))){
#     system2("mv", paste(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "custom_timeseries"), file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "model"), sep =" "))
#   }

  # set working directory to current model run directory
  setwd("/var/obwb-hydro-modelling/simulations/Global-Snow-Calibration/Global-Snow-Calibration-Global-Snow-Calibration-All-Data")

  print("Beginning Ostrich Calibration...")
  
  load("/var/obwb-hydro-modelling/simulations/Global-Snow-Calibration/Global-Snow-Calibration-Global-Snow-Calibration-All-Data/pre-calibration-image.RData")

  system2("/usr/bin/mpirun",args = paste("-n", cores, "OstrichMPI"))
  # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Ostrich"))


  #####################################################################
  ##
  ## Plot a series of model results
  ##
  #####################################################################
  
  ## Remove the :SuppressOutput command from the RvI file
  rewrite.output(ws.interest, run.number)
  
  ## Change the directoyr into the final calibration directory
  setwd(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model"))
  
  ## Complete one more model run to write all model output
  system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", "Raven.exe"), args = paste(ws.interest, run.number, sep = '-'))
  
  

  
  
  require(RavenR)

  ## Generate a pdf of results
  pdf(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "-Output.pdf", sep = "")), width = 8.5, height = 11)

  plot.calibration.results(ws.interest, run.number, subbasins.present)

  source("/var/obwb-hydro-modelling/src/naturalized-flows/naturalized-flow-processing.R")

  dev.off()

  ## Send email to notify of completion

  # send.mail(from = "birdl@ae.ca",
  #           to =  "birdl@ae.ca",
  #           subject = "Calibration Complete",
  #           body = paste("Please find attached the latest calibration for the", include.watersheds, "Creek watershed(s)."),
  #           authenticate = TRUE,
  #           smtp = list(host.name = "smtp.office365.com",
  #                       port = 587,
  #                       user.name = "birdl@ae.ca",
  #                       passwd = "Summer2019",
  #                       tls = TRUE),
  #           attach.files = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = "")))

  
  send.mail(from = "birdl@ae.ca",
            to =  "birdl@ae.ca",
            subject = "Calibration Complete",
            body = paste("Model run", run.number, "has completed. The VM has been turned off."),
            authenticate = TRUE,
            smtp = list(host.name = "smtp.office365.com",
                        port = 587,
                        user.name = "birdl@ae.ca",
                        passwd = "Fall2019",
                        tls = TRUE),
            attach.files = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = "")))
  
  
  
  
  ## Shutdown the VM.
  # system2("sudo", args = "shutdown -h now")
  
#####################################################################
##
## Run Raven executable with improved parameter values, or with base value is run.ostrich == FALSE
##
#####################################################################
# 
# } else {
#   
#   if(run.ostrich == TRUE){
#     
#     print(paste("There are no WSC stations within the ", include.watersheds, " Creek watershed(s). No calibration is possible. One execution of Raven will be completed.", sep = ""))
#     
#   }
#   
#   setwd(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")))
#   
#   # system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "raven_rev.exe"), args = paste(ws.interest, run.number, sep = '-'))
#   system2(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Raven.exe"), args = paste(ws.interest, run.number, sep = '-'), wait = F)
#   
#   
#   print("Ostrich was not used for model calibration during this run...")
#   
#   #####################################################################
#   ##
#   ## Plot a series of model results
#   ##
#   #####################################################################
#   require(RavenR)
#   
#   ## Generate a pdf of results
#   pdf(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "-Output.pdf", sep = "")), width = 8.5, height = 11)
# 
#   plot.results(ws.interest, run.number, subbasins.present)
#   
#   source("/var/obwb-hydro-modelling/src/naturalized-flows/naturalized-flow-processing.R")
#   
#   dev.off()
#   
#   }
# 
#   
#   ## end timer
# proc.time() - ptm

## Close sink() connection so no more output is written to file.
sink(NULL)

# hydrographs <- hyd.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste("processor_2/model/Whiteman-Apr-24-19_Hydrographs.csv", sep = "")))

# ws.storage <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste("processor_2/model/Whiteman-Apr-24-19_WatershedStorage.csv", sep = "")))


# reservoirs <- res.read(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))
# res.plot(reservoirs$res$Mission_Creek219, zero.axis = F)
