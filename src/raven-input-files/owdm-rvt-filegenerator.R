############################################################################################################################
##
## This script generates the requird *.rvt file for input to Raven with OWDM Demand Data
##
## Sep-20-2019 LAB
##
############################################################################################################################

RVI.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVI-Template.csv")

subbasins <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")


## Isolate time commands and format to meet Raven requirements (i.e., add 00:00:00)
RVI.template$PARAMETER <- paste(":", RVI.template$PARAMETER, sep = '')

time <- RVI.template[RVI.template$GROUP == "Time", c("PARAMETER", "DEFINITION")]

time$DEFINITION <- paste(as.Date(time$DEFINITION, format = "%m/%d/%Y"), "00:00:00", sep = ' ')


## Read in OWDM data for all watersheds
owdm <- read.csv("/var/obwb-hydro-modelling/input-data/raw/owdm/okanagan_2019.09.19.csv")

owdm$watershed <- gsub( " .*$", "", owdm$subbasin)

## Isolate specified watershed
owdm.sub <- owdm[owdm$watershed %in% include.watersheds,]

## merege subbasin table to ensure that subbasins are correct
owdm.sub <- merge(owdm.sub, subbasins[,c("SubBasin_name", "Subbasin_ID")], by.x = "subbasin", by.y = "SubBasin_name", all.x = T, all.y = F)


owdm.sub$extraction.total <- rowSums(owdm.sub[,c("indoor", "outdoor_domestic", "outdoor_animal", "outdoor_other_irrigation")])

owdm.sub$date <- paste(owdm.sub$year, owdm.sub$day, sep = "-")

owdm.sub$tiso <- as.Date(owdm.sub$date, format = "%Y-%j")


## split day 0 values across weeks 36-39 and delete day 0 rows.
# day.0 <- which(owdm.sub$day == 0)
# 
# day.0.subbasins <- owdm.sub[day.0, "Subbasin_ID"]


## for each subbasin, loop through all years with day 0 and distribute to days which fall within week 36-39 (need to confirm these dates.)



model.period.start <- as.Date(time$DEFINITION[time$PARAMETER == ":StartDate"])

model.period.end <- as.Date(time$DEFINITION[time$PARAMETER == ":EndDate"])

if(nrow(owdm.sub) > 0){
  
  subs <- unique(owdm.sub$Subbasin_ID)
  
  ## Creat a sub-directory to house all owdm timeseries
  dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm"))
  
  for(i in 1:length(subs)){
    
    ## isolate extractionf or one subbasin
    tmp <- owdm.sub[owdm.sub$Subbasin_ID == subs[i],]
    
    ## Reorder tmp based on the dates - this ensures that 1996-01-01 is the first record.
    tmp <- tmp[order(tmp$tiso),]
    
    # warmup.demand.period <- tmp$tiso[1] - model.period.start
    # 
    # # If the model is to be run prior to water demand being included, create "empty"/Zero demand for the warmup period
    # if(warmup.demand.period > 0){
    #   date.fills <- seq(model.period.start, length.out = warmup.demand.period, by = 1)
    #   
    #   warmup.demand <- data.frame(matrix(NA, ncol = ncol(tmp), nrow = length(date.fills)))
    #   
    #   colnames(warmup.demand) <- colnames(tmp)
    #   
    #   warmup.demand$tiso <- date.fills
    #   
    #   warmup.demand$extraction.total <- 0
    #   
    #   tmp <- rbind(warmup.demand, tmp)
    #   
    # }
    
    ## make extraction total negative to represent "extraction" rather than addition to flow
    tmp$extraction.total <- tmp$extraction.total * -1
    
    ## convert extraction total to m3/s from m3/day
    tmp$extraction.total <- tmp$extraction.total / (60*60*24)
    
    tmp$extraction.total <- ifelse(tmp$extraction.total == -0, 0, tmp$extraction.total)
    
    fc <- file(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "owdm", paste(subs[i], "owdm.rvt", sep = "_")), open = "w+")
    
    writeLines(sprintf(':BasinInflowHydrograph2 %i # %s',subs[i], paste(subs[i], "owdm.rvt", sep = "_")), fc)
    writeLines(sprintf('%s 00:00:00 1.0 %i',as.character(tmp$tiso[1]),nrow(tmp)), fc)
    
    for (k in 1:nrow(tmp)) {
      writeLines(sprintf('%g',tmp[k,"extraction.total"]), fc)
    }
    
    writeLines(':EndBasinInflowHydrograph2',fc)
    close(fc)
    
    if(i == 1){
        cat(file = RVToutFile, append = T, sep = "",
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
    
  }
  
} else {print("No OWDM data exists for currently included watershed(s)...")}
