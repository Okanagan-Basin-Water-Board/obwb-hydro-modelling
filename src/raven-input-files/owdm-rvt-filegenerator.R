############################################################################################################################
##
## This script generates the requird *.rvt file for input to Raven with OWDM Demand Data
##
## Sep-20-2019 LAB
##
############################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

#####################################################################
##
## Read in and format required information to support OWDM *.rvt file generation
##
#####################################################################

##-------------------------------------------------------------------
##
## Read in RVI, subbasin, and owdm data
##
##-------------------------------------------------------------------

RVI.template <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, RVI.template.in.file))
  
subbasins <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  
owdm <- read.csv(file.path(global.input.dir, raw.owdm.in.dir, owdm.water.demand.in.file))
  
colnames(owdm) <- c("subbasin", "day", "year", "extraction.total")

##-------------------------------------------------------------------
##
## Format RVI information to meet Raven Requirements (i.e., add 00:00:00)
##
##-------------------------------------------------------------------

RVI.template$PARAMETER <- paste(":", RVI.template$PARAMETER, sep = '')

time <- RVI.template[RVI.template$GROUP == "Time", c("PARAMETER", "DEFINITION")]

time$DEFINITION <- paste(base::as.Date(time$DEFINITION, format = "%m/%d/%Y"), "00:00:00", sep = ' ')

##-------------------------------------------------------------------
##
## Format OWDM data to prepare it for writing to OWDM rvt files
##
##-------------------------------------------------------------------

## add watershed column
owdm$watershed <- gsub( " .*$", "", owdm$subbasin)

## Subset OWDM data to isolate only the watersheds included in the current model run
owdm.sub <- owdm[owdm$watershed %in% include.watersheds,]

## merege subbasin table to ensure that subbasins are correct
owdm.sub <- merge(owdm.sub, subbasins[,c("SubBasin_name", "Subbasin_ID")], by.x = "subbasin", by.y = "SubBasin_name", all.x = T, all.y = F)

## Calculate the total irrigation as the sum of indoor and all outdoor irrigation needs
# owdm.sub$extraction.total <- rowSums(owdm.sub[,c("indoor", "outdoor_domestic", "outdoor_animal", "outdoor_other_irrigation")])

## Paste year and day together
owdm.sub$date <- paste(owdm.sub$year, owdm.sub$day, sep = "-")

## format year-day string to a tiso date
owdm.sub$tiso <- base::as.Date(owdm.sub$date, format = "%Y-%j")

##-------------------------------------------------------------------
##
## Determine the start and end dates of the model. This allows missing data (i.e., prior to beginning of available owdm data to be filled in)
##
##-------------------------------------------------------------------

model.period.start <- base::as.Date(time$DEFINITION[time$PARAMETER == ":StartDate"])

model.period.end <- base::as.Date(time$DEFINITION[time$PARAMETER == ":EndDate"])

# model.period <- data.frame(Date = seq(as.Date(model.period.start), as.Date(model.period.end), by = "day"))

#####################################################################
##
## If there is some water demand within the selected watersheds (i.e., owdm.sub is not empty), loop through all subbasins and create the owdm.rvt file(s).
##
#####################################################################

if(nrow(owdm.sub) > 0){
  
  print(paste(length(unique(owdm.sub$subbasin)), "subbasins have water demand data available. Writing required *.rvt files..."))
  
  subs <- unique(owdm.sub$Subbasin_ID)
  
  ## Creat a sub-directory to house all owdm timeseries
  dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm"))
  
  for(i in 1:length(subs)){
    
    ## isolate extractionf or one subbasin
    tmp <- owdm.sub[owdm.sub$Subbasin_ID == subs[i],]

    ##-------------------------------------------------------------------
    ##
    ## Split out day 0 values across all days within week 36-39. This approach is consistent with that used for the naturalized streamflow dataset development.
    ## - This results in day 0 values being divided between 28 days, between September 3 - September 30, inclusive.
    ##
    ##-------------------------------------------------------------------
    
    year.0 <- tmp[tmp$day == 0, "year"]
    
    if(length(year.0) > 0){
      
      for(j in 1:length(year.0)){
        
        ## Identidy the day 0 extraction total
        day.0.demand <- tmp[tmp$year == year.0[j] & tmp$day == 0, "extraction.total"]
        
        ## Distribute the day 0 extraction total across 28 days between September 3 and September 30
        tmp[which(tmp$tiso >= base::as.Date(paste(year.0[j], "09-03", sep = "-")) & tmp$tiso <= base::as.Date(paste(year.0[j], "09-30", sep = "-"))), "extraction.total"] <- tmp[which(tmp$tiso >= base::as.Date(paste(year.0[j], "09-03", sep = "-")) & tmp$tiso <= base::as.Date(paste(year.0[j], "09-30", sep = "-"))), "extraction.total"] + (day.0.demand / 28)
      }
    }
    
    ##-------------------------------------------------------------------
    ##
    ## Prepare tmp for writing to owdm.rvt file. 
    ##
    ##-------------------------------------------------------------------

    ## Delete all day 0 rows from tmp
    tmp <- tmp[!tmp$day == 0, ]
        
    ## Reorder tmp based on the dates - this ensures that 1996-01-01 is the first record.
    tmp <- tmp[order(tmp$tiso),]
    
    ## convert extraction total to m3/s from m3/day
    tmp$extraction.total <- tmp$extraction.total / (60*60*24)
    
    ## Because it is a diversion, it should not be included in the model warm-up period. Include 0 in the period during model warm-up
    tmp[tmp$tiso < demand.start.date, "extraction.total"] <- 0
    
    ##-------------------------------------------------------------------
    ##
    ## If the model run begins prior to water demand data being available, insert filler NA (-1.2345) data points
    ##
    ##-------------------------------------------------------------------

    ## Determine how long the modl runs for before owdm data begins
    warmup.demand.period <- tmp$tiso[1] - model.period.start

    # If the model is to be run prior to water demand being included, create "empty"/Zero demand for the warmup period. If the demand data is available before the model run starts, no filling is needed.
    if(!is.na(warmup.demand.period) & warmup.demand.period >1){
      
      ## Create daily date sequence that spans the model start date to the date before owdm data is available
      date.fills <- seq(model.period.start, length.out = warmup.demand.period, by = 1)

      ## Create a dataframe of length warmup period, and same column dimensions to match tmp (to allow rbinding)
      warmup.demand <- data.frame(matrix(NA, ncol = ncol(tmp), nrow = length(date.fills)))

      ## Match column names to those of tmp
      colnames(warmup.demand) <- colnames(tmp)

      ## Add in dates to tiso column
      warmup.demand$tiso <- date.fills

      ## Add Raven NA (-1.2345) to entire warm-up period
      ## NOTE: 0 is used here as a place holder - Raven currently doesn't respect the -1.2345 blank flag - James will update in due course.
      warmup.demand$extraction.total <- 0

      ## Bind warmup period and tmp together to provide complete timesereis
      tmp <- rbind(warmup.demand, tmp)

    }
    
    ##-------------------------------------------------------------------
    ##
    ## Check to see if there are -0 values and make then 0.
    ##
    ##-------------------------------------------------------------------

    tmp$extraction.total <- ifelse(tmp$extraction.total == -0, 0, tmp$extraction.total)
    
    #####################################################################
    ##
    ## Write the required XXXX_owdm.rvt file
    ##
    #####################################################################
    
    ##-------------------------------------------------------------------
    ##
    ## Check to see if the demand is directly from a reservoir
    ##
    ##-------------------------------------------------------------------
    
    ## If reservoir_name is NULL, then it is NOT a reservoir - add :IrrigationDemand tags
    if(subbasins[subbasins$Subbasin_ID == subs[i], "Reservoir_name"] == "<Null>"){
    
      fc <- file(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm", paste(subs[i], "owdm.rvt", sep = "_")), open = "w+")
      
      writeLines(sprintf(':IrrigationDemand %i # %s',subs[i], paste(subs[i], "owdm.rvt", sep = "_")), fc)
      writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(tmp$tiso[1]),nrow(tmp)), fc)
      
      for (k in 1:nrow(tmp)) {
        writeLines(sprintf('%g',tmp[k,"extraction.total"]), fc)
      }
      
      writeLines(':EndIrrigationDemand',fc)

      ##-------------------------------------------------------------------
      ##
      ## Add :ReservoirDownstreamDemand command to specify how water demand is supplied to the given subbasin
      ##
      ##-------------------------------------------------------------------
      
      if(manage.reservoirs == TRUE){
        
        ## Only write the reservoir demand tag if the flag is not <Null>
        if(subbasins[subbasins$Subbasin_ID == subs[i], "Upstream_Reservoir"] != "<Null>" & subbasins[subbasins$Subbasin_ID == subs[i], "Pct_Demand_Met"] != "<Null>"){
          
          ## #NF2 - 26052020: Code block updated to allow A) multiple reservoirs to be specified to satisfy demand at a given subbasin; b) Temporal constraints to be specified for a given demand.
          ## Unlist Upstream Reservoirs, Associated Percentage Demands, and Start/End Dates
          ## Create a dataframe for the current subbasin and make all characters
          sub_dm <- lapply(subbasins[subbasins$Subbasin_ID == subs[i], c("Upstream_Reservoir", "Pct_Demand_Met", "Demand_julian_start", "Demand_julian_end")], as.character)

          ## Split elements on commas to allow indexing relevant to individual reservoirs
          res_dm <- sapply(sub_dm, strsplit, ",")
          
          ## check that all elements are the same size (i.e., nothing is missing)
          if(!all(lengths(res_dm)[1] == lengths(res_dm))){
            stop(paste("Ensure that the same number of elements are included for all Upstream_Reservoirs, Pct_Demand_Met, Demand_julian_start, and Demand_julian_end for Subbasin", subs[i]))
          } 
          
          ## Loop over all reservoirs (and/or _AUTO flag) related to the given subbasin
          for(res in 1:length(res_dm[[1]])){
            
            ## Append a header to the relevant *.rvt file
            cat(file = fc, append = T, sep = "",
                if(res == 1){"\n"}, "\n",
                if(res == 1){paste("#---------------------------------------------", "\n")},
                if(res == 1){paste("# Specify water demand management for subbasin", subs[i], "\n")}
                )
                
                ## check if start/end dates for demand satisfaction are specified. If so, append the :ReservoirDownstreamDemand command, including reservoir, percentage, and julian day start/end
                if(res_dm$Demand_julian_start[res] != "<Null>" & res_dm$Demand_julian_end[res] != "<Null>"){
                  
                  cat(file = fc, append = T, sep = "",
                  paste(":ReservoirDownstreamDemand ", subs[i], res_dm$Upstream_Reservoir[res], res_dm$Pct_Demand_Met[res], res_dm$Demand_julian_start[res], res_dm$Demand_julian_end[res], sep = " ")
                  )
                  
                  ## Print a statement specifying how the demand has been managed
                  print(paste("Reservoir releases from Reservoir", res_dm$Upstream_Reservoir[res], "to satisfy water demand in Subbasin", subs[i], "are constrained between Julian Day",res_dm$Demand_julian_start[res], "and Julain Day", res_dm$Demand_julian_end[res]))
                  
                  ## check if BOTH start/end dates are Null (i.e., not constrained temporally). If so, append the :ReservoirDownstreamDemand command, including reservoir and percentage, but no julian day constraints.
                } else if(res_dm$Demand_julian_start[res] == "<Null>" & res_dm$Demand_julian_end[res] == "<Null>"){
                  
                  cat(file = fc, append = T, sep = "",
                  paste(":ReservoirDownstreamDemand ", subs[i], res_dm$Upstream_Reservoir[res], res_dm$Pct_Demand_Met[res], sep = " ")
                  )
                  
                  print(paste("Reservoir releases from Reservoir", res_dm$Upstream_Reservoir[res], "to satisfy water demand in Subbasin", subs[i], "are not constrained temporally."))
                  
                } else{
                  
                  ## If one start/end dates are not both defined, or excluded, throw an error.
                  stop(paste("Please specify BOTH Demand_julian_start AND Demand_julian_end dates for water demand at Subbsasin", subs[i]))
                  
                } # End else

          } # End for loop
          
        } else {  ## If no upland reservoir support is included, add a statement to this effect.
          
          cat(file = fc, append = T, sep = "",
              "\n",
              "#---------------------------------------------", "\n",
              paste("# Subbasin", subs[i], "is NOT supported by upland storage."), "\n"
          )
        
        } # End if subbasin not supported by upland reservoir storage.
      
      } # End if manage.reservoir
      
      ## IF subs[i] IS a reservoir, then use :ReservoirExtraction command instead of :IrrigationDemand
    } else {
      
      fc <- file(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm", paste(subs[i], "owdm.rvt", sep = "_")), open = "w+")
      
      writeLines(sprintf(':ReservoirExtraction %i # %s',subs[i], paste(subs[i], "owdm.rvt", sep = "_")), fc)
      writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(tmp$tiso[1]),nrow(tmp)), fc)
      
      for (k in 1:nrow(tmp)) {
        writeLines(sprintf('%g',tmp[k,"extraction.total"]), fc)
      }
      
      writeLines(':EndReservoirExtraction',fc)
      
    } # End Else
    
    close(fc)
    
    ##-------------------------------------------------------------------
    ##
    ## Add RedirctToFile command to the end of the master *.vt file
    ##
    ##-------------------------------------------------------------------
    
    if(i == 1){
        cat(file = RVToutFile, append = T, sep = "",
        "\n",
        "#-------------------------------------------------------", "\n",
         "# Redirect to Water Demand Data", "\n",
        "\n",
            ":RedirectToFile ", "owdm/", paste(subs[i], "owdm.rvt", sep = "_"), "\n"
        )
    } else {
      cat(file = RVToutFile, append = T, sep = "",
          ":RedirectToFile ", "owdm/", paste(subs[i], "owdm.rvt", sep = "_"), "\n"
      )
    }
    
    #####################################################################
    ##
    ## If run.ostrich == TRUE, add redircts to the template file too
    ##
    #####################################################################
    
    if(run.ostrich == TRUE & file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
      
      
      OstrichRVTTemplateFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
      
      ## Add :RedirectToFile commands to the end of the rvt.tpl file to match the structure of the master *.rvt file.
      if(i == 1){
        cat(file = OstrichRVTTemplateFile, append = T, sep = "",
            "\n",
            "#-------------------------------------------------------", "\n",
            "# Redirect to Water Demand Data", "\n",
            "\n",
            ":RedirectToFile ", "owdm/", paste(subs[i], "owdm.rvt", sep = "_"), "\n"
        )
      } else {
        cat(file = OstrichRVTTemplateFile, append = T, sep = "",
            ":RedirectToFile ", "owdm/", paste(subs[i], "owdm.rvt", sep = "_"), "\n"
        )
      }
      
    }
    
  }
  
  if(manage.reservoirs != TRUE){
    
    print("Reservoirs are NOT managed to satisfy downstream demand.")
    
  }
  
} else {print("No OWDM data exists for currently included watershed(s)...")}
