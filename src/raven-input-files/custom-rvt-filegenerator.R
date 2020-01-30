############################################################################################################################
##
## This script generates required custom *.rvt files for custom timeseries provided by the user.
##
## The required DATA_TYPES are supported:
## - HYDROGRAPH = Observed streamflow for a given subbasin (Continuous [:ObservationData] or Irreglar [:IrregularObservations] supported)
## - OVERRIDE_STREAMFLOW = Streamflow record to override modelled values for a given subbasin (Continuous timeseries required and overrides the model flows for a specified subbasin)
## - DIVERSION_IN = Water added into a given subbasin (Continuous timeseries required - water is added to reservoirs [:ReservoirExtraction] or subbasin [:BasinInflowHydrograph2], respectively)
## - DIVERSION_OUT = Water removed from a given subbasin(Continuous timeseries required - water is removed from reservoir [:ReservoirExtraction] or subbasin [:BasinInflowHydrograph2], respectievly)
## - RESERVOIR_STAGE = Reservoir Stage(Continuous [:ObservationData] or Irreglar [:IrregularObservations] supported)
## - RESERVOIR_OUT = Reservior Outflows (Continuous [:ObservationData] or Irreglar [:IrregularObservations] supported)
## - DIVERSION_CURVE = Rating curve to define diversions from one subbasin to another based on a hydraulic equation.
## - OVERRIDE_RESERVOIR = Reservoir outflows to override modelled values for a given reservoir (Continuous timeseries required and overrides the model outflows for a specified reservoir)
## - DIVERSION_PCT = Diverts a constant percentage of flow from one subbasin to another between (optional) julian days. This percentage is applied to flows in addition to the minimum flow.
##
## Oct-29-2019 LAB
##
############################################################################################################################

require(readxl)

## --------------------------------------------------
##
## Determine which custom data types needs to be included in the current model run.
##
## --------------------------------------------------

## Read in subbasin.codes, for completeness
subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

## Read in the Summary sheet from custom_timeseries.xlsx
custom.timeseries <- read_xlsx("/var/obwb-hydro-modelling/input-data/raw/custom-timeseries/custom_timeseries.xlsx", sheet = "Summary")

## Isolate only the custom timeseries for the watersheds included
custom.timeseries <- custom.timeseries[custom.timeseries$Watershed %in% include.watersheds, ]

## Determine all the custom data types that need to be included
custom.data.types <- unique(custom.timeseries$Data_Type)

## Determine if the custom data relates to reservoirs or not
custom.timeseries$IS_RES <- ifelse(custom.timeseries$Subbasin %in% subbasin.codes[subbasin.codes$Reservoir_name != "<Null>", "Subbasin_ID"], "Y", "N")


## Create empty vector to store timeseries that cannot be included in calibration
not.available.for.calibration <- c()


## If there is at least one custom timeseries to be included, Read it in and generate a custom rvt.
if(nrow(custom.timeseries) > 0){
  
  ## Create a subdirectory to house all custom timeseries
  dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "custom_timeseries"))

  ## Loop over all custom data types
  for(i in 1:length(custom.data.types)){
  
    tmp <- custom.timeseries[custom.timeseries$Data_Type == custom.data.types[i], ]
    
    ## Loop over each row (i.e., different custom timeseries)
    for(j in 1:nrow(tmp)){
    
      custom.data <- read_xlsx("/var/obwb-hydro-modelling/input-data/raw/custom-timeseries/custom_timeseries.xlsx", sheet = as.character(tmp[j,"Sheet_Name"]))
      
      main.RVT.file <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), paste(ws.interest, "-", run.number, ".rvt", sep = ""))
      
      customRVTfile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "custom_timeseries", paste(tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""))
      
      ## ------------------------------------------------
      ##
      ## Check to see if the data is a DIVERSION_PCT
      ##
      ## ------------------------------------------------
      
      if(custom.data.types[i] == "DIVERSION_PCT"){
        
        from.subbasin <- as.character(custom.data[1, "From_Subbasin"])
        
        to.subbasin <- as.character(custom.data[1, "To_Subbasin"])
        
        diversion.pct <- as.character(custom.data[1, "Diversion_Pct"])
        
        Qmin <- as.character(custom.data[1, "Qmin"])
        
        julian.start <- as.character(custom.data[1, "Julian_Start"])
        
        julian.end <- as.character(custom.data[1, "Julian_End"])
        
        
        ## Write custom RVT file to house the diversion command
        cat(file = customRVTfile, sep = "", append = T,
            "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
            paste(":FlowDiversion ", from.subbasin, to.subbasin, diversion.pct, Qmin, julian.start, julian.end, sep = " "),  "\n"
        )
        
        ## Write redirect the above file
        cat(file = main.RVT.file, append = T, sep = "",
            "\n",
            "# -- Redirect to Diversion Percentage(s)----", "\n",
            ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
        )
        
          if(run.ostrich == TRUE & file.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
            
            OstrichRVTFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
            
            cat(file = OstrichRVTFile, append = T, sep = "",
                "\n",
                "# -- Redirect to Diversion Percentage(s)----", "\n",
                ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
            )
          
        }
      
      ## ------------------------------------------------
      ##
      ## Check to see if the data is a DIVERSION_CURVE (i.e., Mill - Mission Diversion)
      ##
      ## ------------------------------------------------
      
    } else if(custom.data.types[i] == "DIVERSION_CURVE"){
        
        npoints <- nrow(custom.data)
        
        from.subbasin <- as.character(custom.data[1, "From_Subbasin"])
        
        to.subbasin <- as.character(custom.data[1, "To_Subbasin"])
        
        
        cat(file = customRVTfile, sep = "", append = T,
            "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
            ":FlowDiversionLookupTable ", from.subbasin, " ", to.subbasin, "\n",
            as.character(npoints), "\n"
        )
        
        write.table(custom.data[, c("Inflow_m3s", "Diversion_m3s")], customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
        
        cat(file = customRVTfile, sep = "", append = T,
            ":EndFlowDiversionLookupTable", "\n"
        )
        
        
        cat(file = main.RVT.file, append = T, sep = "",
            "\n",
            "# -- Redirect to Diversion Curve(s)----", "\n",
            ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
        )
        
        
        if(run.ostrich == TRUE & file.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
          
          OstrichRVTFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
        
          cat(file = OstrichRVTFile, append = T, sep = "",
              "\n",
              "# -- Rdirect to Diversion Curve(s)----", "\n",
              ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
          )
          
        }
        
        ## ------------------------------------------------
        ##
        ## Check to see if data exist within the model period of interest. If not, no custom *.rvt file will be writtem
        ##
        ## ------------------------------------------------
        
      } else if((base::as.Date(custom.data$Date[1]) <= start.date & base::as.Date(custom.data$Date[1]) < end.date) | (base::as.Date(custom.data$Date[1]) > start.date & base::as.Date(custom.data$Date[1]) < end.date)){
        
        
        ## ------------------------------------------------
        ##
        ## Check to see if the data are continuous or irregular records
        ##
        ## ------------------------------------------------
        
        if(tmp[j, "Observation_Type"] == "Continuous"){
        
          ## ------------------------------------------------
          ## Check for missing records and generate continuous timeseries for the model period
          ##------------------------------------------------
          
          custom.data$Date <- base::as.Date(custom.data$Date)
  
          dates <- data.frame(Date = seq(base::as.Date(start.date), base::as.Date(end.date), by = "day"))
          
          custom.data <- merge(dates, custom.data, by.all = "Date", all.x = T)
          
        }
        
        if(tmp[j, "Observation_Type"] == "Irregular"){
          
          ## Format dates correctly for read-in to Raven
          custom.data$tiso <- as.POSIXct(custom.data$Date_Time, format = "%m/%d/%Y %H:%M:%S")
          custom.data$tiso <- strftime(custom.data$tiso, format = "%Y-%m-%d %H:%M:%S")
          
        }
        
  
        ## ------------------------------------------------
        ## If the custom data is a diversion, figure out how it should be handled
        ##------------------------------------------------
        
        if(custom.data.types[i] == "DIVERSION_IN" | custom.data.types[i] == "DIVERSION_OUT"){
        
          ## Check to see if Observation_Type is Continuous
          if(tmp[j,"Observation_Type"] != "Continuous"){stop(print(paste(custom.data.types[i], "data require continuous data records. Irregular data series cannot be read in.")))}
            
          ## Make na values = 0
          custom.data[is.na(custom.data$Mean_Daily_Diversion_m3s), "Mean_Daily_Diversion_m3s"] <- 0
         
                  ## If the Diversion is into a reservoir, use :ReservoirExtraction and make diversion values negative (to denote an input)
          if(custom.data.types[i] == "DIVERSION_IN" & tmp[j,"IS_RES"] == "Y"){
            
            cat(file = customRVTfile, sep = "", append = T,
              "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
              ":ReservoirExtraction ", as.character(tmp[j, "Subbasin"]), "\n",
              sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$Mean_Daily_Diversion_m3s * -1, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
              
            cat(file = customRVTfile, sep = "", append = T,
              ":EndReservoirExtraction", "\n"
            )
            
          } # End IF for Diversions into Reservoirs
          
          ## If the Diversion is into a Subbasin, use :BasinInflowHydrograph2
          if(custom.data.types[i] == "DIVERSION_IN" & tmp[j, "IS_RES"] == "N"){
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":BasinInflowHydrograph2 ", as.character(tmp[j, "Subbasin"]), "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$Mean_Daily_Diversion_m3s, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndBasinInflowHydrograph", "\n"
            )
            
            
          } # End if for Diversions INTO SUBBASINS
          
          ## If the diversion is out of a reservoir, use :ReservoirExtraction
          if(custom.data.types[i] == "DIVERSION_OUT" & tmp[j,"IS_RES"] == "Y"){
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":ReservoirExtraction ", as.character(tmp[j, "Subbasin"]), "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$Mean_Daily_Diversion_m3s, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndReservoirExtraction", "\n"
            )
            
          } # End if for Diversions OUT OF reservoirs
          
          ## If the Diversion is out of a Subbasin, use :BasinInflowHydrograph2 and make diversion values negative (to denote an output)
          if(custom.data.types[i] == "DIVERSION_OUT" & tmp[j,"IS_RES"] == "N"){
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":BasinInflowHydrograph2 ", as.character(tmp[j, "Subbasin"]), "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$Mean_Daily_Diversion_m3s * -1, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndBasinInflowHydrograph", "\n"
            )
            
          } # End if for Diversions OUT OF Subbasins
          
        } # End if for Diversions
        
        ## ------------------------------------------------
        ## If the custom data is an OVERRIDE_STREAMFLOW, generate custom rvt, and add :OverrideStreamflow tags to rvt file (and ostrich template is necessary)
        ##------------------------------------------------
        
        if(custom.data.types[i] == "OVERRIDE_STREAMFLOW"){
          
          ## Check to see if Observation_Type is Continuous
          if(tmp[j,"Observation_Type"] != "Continuous"){stop(print(paste(custom.data.types[i], "data require continuous data records. Irregular data series cannot be read in.")))}
          
          ## Make na values = 0 - the timeseries MUST be complete to successfully be used to override a subbasin - no missing values can exist.
          custom.data[is.na(custom.data$Mean_Daily_Discharge_m3s), "Mean_Daily_Discharge_m3s"] <- 0
          
          ## TEMPORARY - temporary bug fix to account for timestep bug in Raven. Once this is fixed, this extra day section can be removed
          extra.day <- custom.data[1,]
          
          extra.day$Date <- lubridate::date(extra.day$Date)-1
          
          custom.data <- rbind(extra.day, custom.data)
          
          
          cat(file = customRVTfile, sep = "", append = T,
              "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
              ":ObservationData HYDROGRAPH " ,as.character(tmp[j, "Subbasin"]), " m3/s", "\n",
              sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
              )
          
          write.table(custom.data$Mean_Daily_Discharge_m3s, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
          
          cat(file = customRVTfile, sep = "", append = T,
              ":EndObservationData", "\n"
          )
          
          ##----------------------------------------------
          ##
          ## Write Observation Weights for the calibration/validation period
          ##
          ##----------------------------------------------
          
          if(validate.model == FALSE){
            
            custom.data$weights <- ifelse(custom.data$Date < base::as.Date(calibration.start) | custom.data$Date > base::as.Date(calibration.end), 0, 1)
            
          } else {
            
            custom.data$weights <- ifelse(custom.data$Date < base::as.Date(validation.start) | custom.data$Date > base::as.Date(validation.end), 0, 1)
            
          }
          
          cat(file = customRVTfile, sep = "", append = T,
              "\n",
              "# Write ObservationWeights", "\n",
              ":ObservationWeights HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), "\n",
              sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
              )
          
          write.table(custom.data$weights, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
          
          cat(file = customRVTfile, sep = "", append = T,
              ":EndObservationWeights", "\n"
          )
          
          
          ## RAVEN Requires that the :OverrideStreamflow command occurs AFTER the :ObservationData is read in. Therfore, the :RedirectToFile command must be written here for OVERRIDE_STREAMFLOW ONLY
          cat(file = main.RVT.file, append = T, sep = "",
              "\n",
              "\n",
              "#-------------------------------------------------------", "\n",
              "#-------- Redirect to Custom Timeseries ----------------", "\n",
              "\n",
              ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
          )
          
          
          # Add :OverrideStreamflow command to main rvt file
          cat(file = main.RVT.file, append = T, sep = "",
              "\n",
              "\n",
              "#----------------------------------------------------------", "\n",
              "# Override Streamflows in the following Subbasins", "\n",
              "#", "\n",
              ":OverrideStreamflow  ", as.character(tmp[j, "Subbasin"]), "\n"
              )
          
          if(run.ostrich == TRUE & file.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
            
              OstrichRVTFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
            
              ## RAVEN Requires that the :OverrideStreamflow command occurs AFTER the :ObservationData is read in. Therfore, the :RedirectToFile command must be written here for OVERRIDE_STREAMFLOW ONLY
              cat(file = OstrichRVTFile, append = T, sep = "",
                  "\n",
                  "\n",
                  "#-------------------------------------------------------", "\n",
                  "#-------- Redirect to Custom Timeseries ----------------", "\n",
                  "\n",
                  ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
              )
            
            
             cat(file = OstrichRVTFile, append = T, sep = "",
                "#----------------------------------------------------------", "\n",
                "# Override Streamflows in the following Subbasins", "\n",
                "#", "\n",
                ":OverrideStreamflow  ", as.character(tmp[j, "Subbasin"]), "\n"
            )
          } # End if Ostrich is true.
          
        } # End if for Override Streamflow
        
        
        ## ------------------------------------------------
        ## If the custom data is an OVERRIDE_RESERVOIR, generate custom rvt, and add :OverrideReservoirFlow tags to rvt file (and ostrich template is necessary)
        ##------------------------------------------------
        
        if(custom.data.types[i] == "OVERRIDE_RESERVOIR"){
          
          ## Check to see if Observation_Type is Continuous
          if(tmp[j,"Observation_Type"] != "Continuous"){stop(print(paste(custom.data.types[i], "data require continuous data records. Irregular data series cannot be read in.")))}
          
          ## Make na values = 0 - the timeseries MUST be complete to successfully be used to override a subbasin - no missing values can exist.
          custom.data[is.na(custom.data$Mean_Daily_Discharge_m3s), "Mean_Daily_Discharge_m3s"] <- 0
          
          cat(file = customRVTfile, sep = "", append = T,
              "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
              ":OverrideReservoirFlow " ,as.character(tmp[j, "Subbasin"]), "\n",
              sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
          )
          
          write.table(custom.data$Mean_Daily_Discharge_m3s, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
          
          cat(file = customRVTfile, sep = "", append = T,
              ":EndOverrideReservoirFlow", "\n"
          )
          
        } # End if for Override Reservoirs
        
        ## ------------------------------------------------
        ## If the custom data is a HYDROGRAPH, add ObservationData
        ##------------------------------------------------
        if(custom.data.types[i] == "HYDROGRAPH"){
          
          ## Write Continuous Records
          if(tmp[j,"Observation_Type"] == "Continuous"){
          
            ## Make na values = 0
            custom.data[is.na(custom.data$Mean_Daily_Discharge_m3s), "Mean_Daily_Discharge_m3s"] <- -1.2345
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":ObservationData HYDROGRAPH " ,as.character(tmp[j, "Subbasin"]), " m3/s", "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$Mean_Daily_Discharge_m3s, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndObservationData", "\n"
            )
            
          } # End Continuous Hydrograph
          
          
          ##----------------------------------------------
          ##
          ## Write Observation Weights for the calibration/validation period
          ##
          ##----------------------------------------------
          
          if(validate.model == FALSE){
            
            custom.data$weights <- ifelse(custom.data$Date < base::as.Date(calibration.start) | custom.data$Date > base::as.Date(calibration.end), 0, 1)
            
          } else {
            
            custom.data$weights <- ifelse(custom.data$Date < base::as.Date(validation.start) | custom.data$Date > base::as.Date(validation.end), 0, 1)
            
          }
          
          cat(file = customRVTfile, sep = "", append = T,
              "\n",
              "# Write ObservationWeights", "\n",
              ":ObservationWeights HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), "\n",
              sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
          )
          
          write.table(custom.data$weights, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
          
          cat(file = customRVTfile, sep = "", append = T,
              ":EndObservationWeights", "\n"
          )
          
          
          if(tmp[j, "Observation_Type"] == "Irregular"){
            
            ## Make na values = 0
            custom.data[is.na(custom.data$Mean_Daily_Discharge_m3s), "Mean_Daily_Discharge_m3s"] <- -1.2345
            
            cat(file - customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":IrregularObservations HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), " ", nrow(custom.data), " m3/s", "\n"
              )
            
            write.table(custom.data[,c("Date_Time", "Mean_Daily_Discharge_m3")], customRVToutFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVToutFile, append = T, sep = "",
                ":EndIrregularObservations",
                "\n"
            )
            
            ##----------------------------------------------
            ##
            ## Write Observation Weights for the calibration/validation period
            ##
            ##----------------------------------------------
            
            
            if(validate.model == FALSE){
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(calibration.start) | custom.data$Date > base::as.Date(calibration.end), 0, 1)
              
            } else {
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(validation.start) | custom.data$Date > base::as.Date(validation.end), 0, 1)
              
            }
            
            cat(file = customRVTfile, sep = "", append = T,
                "\n",
                "# Write ObservationWeights", "\n",
                ":IrregularWeights HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), " ", nrow(custom.data), "\n"
            )
            
            write.table(custom.data[,c("Date_Time", "weights")], customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndIrregularWeights", "\n"
            )
  
          } # End Irregular Hydrograph
          
          
          if(tmp[j, "Observation_Type"] != "Continuous" & tmp[j, "Observation_Type"] != "Irregular"){stop(print(paste(custom.data.types[i], "data require 'Continuous' or 'Irregular' Observation_Type.")))}
          
        } # End if for Hydrographs
          
        ## ------------------------------------------------
        ## If the custom data is a HYDROGRAPH, add ObservationData
        ##------------------------------------------------
        if(custom.data.types[i] == "RESERVOIR_STAGE"){
          
          if(tmp[j, "Observation_Type"] == "Continuous"){
            ## Make na values = 0
            custom.data[is.na(custom.data$Res_Stage_m), "Res_Stage_m"] <- -1.2345
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ':ObservationData RESERVOIR_STAGE ',as.character(tmp[j, "Subbasin"], " m"), "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$Res_Stage_m, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndObservationData", "\n"
            )
            
            ##----------------------------------------------
            ##
            ## Write Observation Weights for the calibration/validation period
            ##
            ##----------------------------------------------
            
            if(validate.model == FALSE){
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(calibration.start) | custom.data$Date > base::as.Date(calibration.end), 0, 1)
              
            } else {
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(validation.start) | custom.data$Date > base::as.Date(validation.end), 0, 1)
              
            }
            
            cat(file = customRVTfile, sep = "", append = T,
                "\n",
                "# Write ObservationWeights", "\n",
                ":ObservationWeights RESERVOIR_STAGE ", as.character(tmp[j, "Subbasin"]), "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$weights, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndObservationWeights", "\n"
            )
            
            
          } # End Contiuous Reservoir Stage
          
          if(tmp[j, "Observation_Type"] == "Irregular"){
  
            ## Make na values = 0
            custom.data[is.na(custom.data$Res_Stage_m), "Res_Stage_m"] <- -1.2345
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":IrregularObservations RESERVOIR_STAGE ", as.character(tmp[j, "Subbasin"]), " ", nrow(custom.data), " m", "\n"
            )
            
            write.table(custom.data[,c("tiso", "Res_Stage_m")], customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, append = T, sep = "",
                ":EndIrregularObservations",
                "\n"
            )
            
            ##----------------------------------------------
            ##
            ## Write Observation Weights for the calibration/validation period
            ##
            ##----------------------------------------------
            
            
            if(validate.model == FALSE){
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(calibration.start) | custom.data$Date > base::as.Date(calibration.end), 0, 1)
              
            } else {
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(validation.start) | custom.data$Date > base::as.Date(validation.end), 0, 1)
              
            }
            
            cat(file = customRVTfile, sep = "", append = T,
                "\n",
                "# Write ObservationWeights", "\n",
                ":IrregularWeights RESERVOIR_STAGE ", as.character(tmp[j, "Subbasin"]), " ",nrow(custom.data), "\n"
            )
            
            write.table(custom.data[,c("Date_Time", "weights")], customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndIrregularWeights", "\n"
            )
            
          } # End Irregular Reservoir Stage
          
        } # End if for Reservoir Stage
        
        ## ------------------------------------------------
        ## If the custom data is a HYDROGRAPH, add ObservationData
        ##------------------------------------------------
        if(custom.data.types[i] == "RESERVOIR_OUT"){
          
          if(tmp[j, "Observation_Type"] == "Continuous"){
            ## Make na values = 0
            custom.data[is.na(custom.data$Res_Out_m3s), "Res_Out_m3s"] <- -1.2345
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":ObservationData HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), " m3/s", "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$Res_Out_m3s, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndObservationData", "\n"
            )
            
            ##----------------------------------------------
            ##
            ## Write Observation Weights for the calibration/validation period
            ##
            ##----------------------------------------------
            
            if(validate.model == FALSE){
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(calibration.start) | custom.data$Date > base::as.Date(calibration.end), 0, 1)
              
            } else {
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(validation.start) | custom.data$Date > base::as.Date(validation.end), 0, 1)
              
            }
            
            cat(file = customRVTfile, sep = "", append = T,
                "\n",
                "# Write ObservationWeights", "\n",
                ":ObservationWeights HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), "\n",
                sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(custom.data$Date[1])),nrow(custom.data)), "\n"
            )
            
            write.table(custom.data$weights, customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndObservationWeights", "\n"
            )
            
          } # End Continupus Reservoir Outflow
          
          if(tmp[j, "Observation_Type"] == "Irregular"){
            
            ## Make na values = 0
            custom.data[is.na(custom.data$Res_Out_m3s), "Res_Out_m3s"] <- -1.2345
            
            cat(file = customRVTfile, sep = "", append = T,
                "# Custom rvt file for ", as.character(tmp[j, "Sheet_Name"]), "\n",
                ":IrregularObservations HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), " ", nrow(custom.data), " m3/s", "\n"
            )
            
            write.table(custom.data[,c("tiso", "Res_Out_m3s")], customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, append = T, sep = "",
                ":EndIrregularObservations",
                "\n"
            )
            
            ##----------------------------------------------
            ##
            ## Write Observation Weights for the calibration/validation period
            ##
            ##----------------------------------------------
            
            
            if(validate.model == FALSE){
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(calibration.start) | custom.data$Date > base::as.Date(calibration.end), 0, 1)
              
            } else {
              
              custom.data$weights <- ifelse(custom.data$Date < base::as.Date(validation.start) | custom.data$Date > base::as.Date(validation.end), 0, 1)
              
            }
            
            cat(file = customRVTfile, sep = "", append = T,
                "\n",
                "# Write ObservationWeights", "\n",
                ":IrregularWeights HYDROGRAPH ", as.character(tmp[j, "Subbasin"]), " ", nrow(custom.data), "\n"
            )
            
            write.table(custom.data[,c("Date_Time", "weights")], customRVTfile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
            
            cat(file = customRVTfile, sep = "", append = T,
                ":EndIrregularWeights", "\n"
            )
            
          } # End Irregular Reservoir Outflow
          
        } # End if for reservoir outflows
          
        ## ----------------------------------------------------------------------
        ##
        ## Write Redirect command to the end of the main RVT file.
        ##
        ## NOTE: Redirect commands should only be written for all custom types that are NOT OVERRIDE_STREAMFLOW. Redirects for OVERRIDE_STREMFLOW are handles earlier on. 
        ##
        ## ----------------------------------------------------------------------
        
        if(i == 1 & j == 1 & custom.data.types[i] != "OVERRIDE_STREAMFLOW"){
          
          cat(file = main.RVT.file, append = T, sep = "",
              "\n",
              "\n",
              "#-------------------------------------------------------", "\n",
              "#-------- Redirect to Custom Timeseries ----------------", "\n",
              "\n",
              ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
              )
          
        } else if(custom.data.types[i] != "OVERRIDE_STREAMFLOW"){
          
          cat(file = main.RVT.file, append = T, sep = "",
              ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
              )
          
        } # End else
            
        ## ----------------------------------------------------------------------
        ##
        ## If Ostrich RVT.TPL exists, write the Redirect to that too
        ##
        ## ----------------------------------------------------------------------
        
        if(run.ostrich == TRUE & file.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
          
          OstrichRVTFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
          
          if(i == 1 & j == 1 & custom.data.types[i] != "OVERRIDE_STREAMFLOW"){
            
            cat(file = OstrichRVTFile, append = T, sep = "",
                "\n",
                "\n",
                "#-------------------------------------------------------", "\n",
                "#-------- Redirect to Custom Timeseries ----------------", "\n",
                "\n",
                ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
            )
            
          } else if(custom.data.types[i] != "OVERRIDE_STREAMFLOW"){
            
            cat(file = OstrichRVTFile, append = T, sep = "",
                ":RedirectToFile  ", paste("custom_timeseries/", tmp[j, "Data_Type"], "_", tmp[j, "Sheet_Name"], ".rvt", sep = ""), "\n"
            )
            
          } # End else
    
        } # End if Ostrich is TRUE and RVT Template exists
     
      } else {print(paste("Custom Timeseries Dataset:", as.character(tmp[j,"Sheet_Name"]), "is not available within the current model period.", sep = " "))
        
        not.available.for.calibration <- c(not.available.for.calibration, as.character(tmp[j,"Sheet_Name"]))
        
        } # End for loop for rows in tmp
      
    } # End for loops for all custom data types
    
  } # End if data exist within the current model time period of interest

} # End if statement for if custom timeseries exist in current model watersheds

if(nrow(custom.timeseries) <1){
  
  print("No custom timeseries data are included in the current model run.")

}


## Identify those custom timeseries that are available for calibration. At this time, Rservoir stage, Reservoir outflow, and Hydrographs can be used as calibration targets.
## Those datasets that are not within the current model period are excluded.
available.for.calibration <- custom.timeseries[custom.timeseries$Data_Type == "RESERVOIR_STAGE" |
                                               custom.timeseries$Data_Type == "RESERVOIR_OUT" | 
                                               custom.timeseries$Data_Type == "HYDROGRAPH", ]

available.for.calibration <- available.for.calibration[!c(available.for.calibration$Sheet_Name %in% not.available.for.calibration), ]

## If there are custom datasets available for calibration, form the custom.calibration.targets object
if(nrow(available.for.calibration) > 0){

  custom.calibration.targets <- c()
  
  for(i in 1:nrow(available.for.calibration)){
    
    custom.calibration.targets <- c(custom.calibration.targets, paste(tmp[i, "Data_Type"], "_", tmp[i, "Sheet_Name"], sep = ""))
    
  }
}
