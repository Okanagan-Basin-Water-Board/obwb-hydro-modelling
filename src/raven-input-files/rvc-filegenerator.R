############################################################################################################################
##
## This Script generates the initial conditions*.rvc file for a given model run. If reservoirs are present,
## initial reservoir stages are entered as the Absolute Crest Height. If no reservoirs are present, no initial
## conditions are specified.
##
## Jul-04-2019
##
############################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

require(readxl)


RVCoutFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvc", sep = ""))

cat(file = RVCoutFile, append = F, sep = "",
    
    "#########################################################################","\n",
    ":FileType rvc Raven 2.8","\n",
    "# DataType         Raven Initial Conditions file","\n",
    "# Watershed Name   ", paste(ws.interest),"\n",
    "# Run Number       ", paste(run.number), "\n",
    ":Application       R","\n",
    ":WrittenBy         rvc-filegenerator.R","\n",
    ":CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    "#---------------------------------------------------------", "\n",
    "\n"
)

#################################################
##
## Establish Initial Conditions for reservoir stage (if reservoirs are present within the watershed)
##
#################################################

## Read in the subbasin attribute table
subbasin.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  
## Subset to isolate only the rows relevant to the watershed(s) of interest. gsub command removes the " Creek" from GNIS_NAME
subbasins.present <- subbasin.codes[gsub( " .*$", "", subbasin.codes$GNIS_NAME) %in% include.watersheds,]

## Identify all reservoirs/lakes within the watershed(s) of interest
reservoirs <- unique(subbasins.present$Reservoir_name)

## Remove "<Null>"
reservoirs <- as.character(reservoirs[!reservoirs %in% "<Null>"])

if(length(reservoirs) <1){print("No initial conditions specified...")
} else {
  
  ## For all unique reservoirs, read in the stage-storage and stage-are information
  for(i in 1:length(reservoirs)){
    
    tmp <- read_xlsx(file.path(global.input.dir, raw.reservoir.in.dir, reservoir.in.file), sheet = reservoirs[i])
    
    ## Check if there is a stage-storage curve
    if("Future_Storage_dam3" %in% names(tmp)){
      
       ##-----------------------------------------------------------------------------
      ##
      ## Extract required weir information and lake depth from tmp
      ##
      ##-----------------------------------------------------------------------------
      
      parameters <- na.omit(tmp[ ,c("PARAMETER", "VALUE")])
      
      AbsoluteCrestHeight <- as.numeric(parameters[parameters$PARAMETER == "AbsoluteCrestHeight", "VALUE"])
    
      SubBasinID <- subbasins.present[subbasins.present$Reservoir_name == reservoirs[i], "Subbasin_ID"]
      
     if(i == 1){
      cat(file = RVCoutFile, append = T, sep = "",
          "\n",
          "# ----- Specify Initial Conditions for Reservoir Stage ----", "\n",
          "\n",
          ":InitialReservoirStage ", SubBasinID, " ", AbsoluteCrestHeight, "\n"
          )
      } else {
        cat(file = RVCoutFile, append = T, sep = "",
        ":InitialReservoirStage ", SubBasinID, " ", AbsoluteCrestHeight, "\n"
        )
      }
    }# Enf if (check if there is a stage-storage curve)
  } # End For Loop
}# End Else
