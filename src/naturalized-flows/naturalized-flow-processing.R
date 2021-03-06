###################################################################################################################################################
##
## This script ingests and summarizes naturalized streamflow datasets prepared by Associated for key Okanagan tributaries
##
## Apr-18-2019 LAB
##
###################################################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

# require(xlsx)
require(openxlsx)
require(lubridate)
require(tidyr)
require(plyr)
require(hydroGOF)
require(RavenR)
require(data.table)

###################################################################################################################################################
##
## - Identify the correct naturalized streamflow dataset file to read in
##
###################################################################################################################################################

## Read in master Naturalized flows map file
nat.flows.summary <- read.csv(file.path(global.input.dir, raw.nat.flows.in.dir, nat.flow.summary.in.file))
  
## List all files (all Associated Naturalized Streamflow files)
filenames <- c(coldstream.nat.flow.in.file, equesis.nat.flow.in.file, inkaneep.nat.flow.in.file, mcdougall.nat.flow.in.file, mclean.nat.flow.in.file,
               mill.nat.flow.in.file, mission.nat.flow.in.file, naramata.nat.flow.in.file, naswhito.nat.flow.in.file, penticton.nat.flow.in.file,
               powers.nat.flow.in.file, shingle.nat.flow.in.file, shorts.nat.flow.in.file, shuttleworth.nat.flow.in.file, trepanier.nat.flow.in.file,
               trout.nat.flow.in.file, vaseux.nat.flow.in.file, vernon.nat.flow.in.file, whiteman.nat.flow.in.file)

## Identify which file is required to be read in based on the "include.watersheds" variable
required.files <- filenames[gsub( " .*$", "", filenames) %in% include.watersheds]

###################################################################################################################################################
##
## Generate Year/Week timeseries which is consistent with the OWDM approach (i.e., 8 day last week of year, and 8 day Feb 29. week in leap years)
##
###################################################################################################################################################

## Generate a sequence of years which matches the naturalized streamflow datasets (i.e, 1996-2010)
Years <- seq(1996, 2010, 1)

## Generate Months sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
Months <- seq(base::as.Date("1996-01-01"), base::as.Date("2010-12-31"), by = "month")

## Generate a sequence of weeks which matches the naturalized streamflow datasets (i.e., 1-52)
Weeks <- paste("Week", seq(1, 52, 1))

## Generate Days sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
Days <- seq(base::as.Date("1996-01-01"), base::as.Date("2010-12-31"), by = "day")

## Develop a vector of 365 days to represent a "Regular Year", broken into weeks which match the OWDM model setup (i.e., 8-day last week)
RegularYear <- c(rep(1:51, each = 7), rep(52, each = 8))

## Develop a vector of 366 days to represent a "Leap Year", broken into weeks which match the OWDM model setup (i.e., 8-day Feb 29. and last week)
LeapYear <- c(rep(1:8, each = 7), rep(9, each = 8), rep(10:51, each = 7), rep(52, each = 8))


##---------------------------------------------------------------------------------------------------------------
##
## Generate a timeseries of years between 1996 - 2010
##
##---------------------------------------------------------------------------------------------------------------

Year.timeseries <- data.frame()

for(i in 1:length(Years)){
  ifelse(leap_year(Years[i]) == T, Y <- rep(Years[i], each = 366), Y <- rep(Years[i], each = 365))
  Y <- data.frame(Y)
  Year.timeseries <- rbind(Year.timeseries, Y)
}

##---------------------------------------------------------------------------------------------------------------
##
## Attach a timeseries of weeks to the Year.timeseries and save the combined as "Output"
##
##---------------------------------------------------------------------------------------------------------------

Times <- data.frame()

for(i in 1:length(Years)){
  Z <- as.data.frame(Year.timeseries[Year.timeseries$Y == Years[i],])
  ifelse(leap_year(Years[i]) == T, Z$Week <- LeapYear, Z$Week <- RegularYear)
  
  Times <- rbind(Times,Z)
}

colnames(Times) <- c("Year", "Week")

## Add date column - this is used to merge dates with Raven Output
Times$date <- Days


###################################################################################################################################################
###################################################################################################################################################
##
## LOOP over all required files to plot naturalized streamflows and calculae residual at the Streamflow POI (if available)
##
###################################################################################################################################################
###################################################################################################################################################

## Read-in the subbasin table to allow subbasins at apex of fan of creek to be identified
subbasins <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  
for(i in 1:length(required.files)){
  
  current.file <- required.files[i]

  current.watershed <- gsub( " .*$", "", current.file)
  
  
  ##################################################################################################################
  ##
  ## Read in and process Raven output for the current model run
  ##
  ##################################################################################################################
  
  if(run.ostrich == TRUE){
    
    ## Read-in the model results (hydrographs)
    raven.output <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
                             
    } else {
  
    ## Read-in the model results (hydrographs)
    raven.output <- read.csv(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
  
  }
  
  ## Convert the dates to Date characters
  raven.output$date <- base::as.Date(raven.output$date)
  
  ## Remove any dates which fall outside of the 1996-2010 window for which naturalized streamflows are available
  raven.output <- raven.output[raven.output$date >= base::as.Date("1996-01-01") & raven.output$date <= base::as.Date("2010-12-31"),]
  
  
  ## Isolate the subbasin ID which corresponds with the Apex of the Alluvial Fan
  stream.poi.subbasin.ID <- subbasins[subbasins$GNIS_NAME %in% paste(current.watershed, "Creek") & subbasins$Reports_to_Fan == "A", "Subbasin_ID"]
  
  
  ## If there are more than one subbasins which report to the Apex of the fan (i.e., on Trepanier Creek), add and "or" (i.e., |) which resuts in multiple column names being identified
  if(length(stream.poi.subbasin.ID) > 1){
    
    stream.poi.subbasin.ID <- paste(stream.poi.subbasin.ID, collapse = "|")

  }
  
  stream.poi.subbasin.ID.col.name <- colnames(raven.output)[grepl(stream.poi.subbasin.ID, colnames(raven.output)) & !grepl("observed", colnames(raven.output))]

  # stream.poi.subbasin.ID.col <- which(colnames(raven.output) %like% stream.poi.subbasin.ID)
  
  ##---------------------------------------------------------------------------------------------------------------
  ##
  ## Check to see if the current watershed is Coldstream Creek. If so, only generate comparison for the apex of the fan
  ##
  ##---------------------------------------------------------------------------------------------------------------
  
  # if(current.watershed != "Coldstream"){
  # 
  # ## Isolate the subbasin ID which corresponds with the mouth of the creek
  # # mouth.subbasin.ID <- subbasins[subbasins$GNIS_NAME %in% paste(current.watershed, "Creek") & subbasins$Downstream_ID == "-1", "Subbasin_ID"]
  # 
  # # mouth.subbasin.ID.col.name <- colnames(raven.output)[grepl(mouth.subbasin.ID, colnames(raven.output)) & !grepl("observed", colnames(raven.output))]
  # 
  # } else {
  #   
  # print(paste("No streamflows were returned for the mouth of", current.watershed, "Creek since it contributes directly to Vernon Creek"))
  #   
  # }
  # mouth.subbasin.ID.col <- which(colnames(raven.output) %like% mouth.subbasin.ID)
  
  ##---------------------------------------------------------------------------------------------------------------
  ##
  ## Check dates from Raven output and subset naturalized streamflow if Raven wasn't run for the entire 1996-2010 period
  ##
  ##---------------------------------------------------------------------------------------------------------------
  
  
  raven.output <- merge(Times, raven.output, by.all = "date", all.x = T)
  
  
  ##---------------------------------------------------------------------------------------------------------------
  ##
  ## Amalgamate "Times" and raven output for Apex of fan and mouth of creek
  ##
  ##---------------------------------------------------------------------------------------------------------------
  
  ## If multiple columns were identified (i.e, more than one subbasin reports to the apex), calculate row sums (i.e., cumulative sum of all subbasins which report to the apex.)
  if(length(stream.poi.subbasin.ID.col.name) > 1){
    
    ## Use rowSums so that wherever more than one subbasin reports to the apex of the fan (i.e., Trepanier Creek), the sum of the relevant subbasins is returned.
    Times$Raven.apex <- rowSums(raven.output[,stream.poi.subbasin.ID.col.name])
  } else {
    
    ## If only one column present, rowSums is not needed
    Times$Raven.apex <- raven.output[,stream.poi.subbasin.ID.col.name]
    
  }
  
  # Times$Raven.mouth <- raven.output[,mouth.subbasin.ID.col.name]
  
  ##---------------------------------------------------------------------------------------------------------------
  ##
  ## Calculate weekly mean for all weeks for Apex of fan
  ##
  ##---------------------------------------------------------------------------------------------------------------
  
  Raven.apex.weekly <- ddply(Times, .(Year, Week), plyr::summarize, Weekly.mean = mean(Raven.apex))
  
  # Raven.mouth.weekly <- ddply(Times, .(Year, Week), summarize, Weekly.mean = mean(Raven.mouth))
  
  
  ##################################################################################################################
  ##
  ## Read-in Naturalized Flows for the Streamflow POI (i.e., Apex of the fan)
  ##
  ##################################################################################################################
  
  ## Generate a character string which matches the tab which is needed from the reaquired.file
  required.tab <- paste(current.watershed, "Nat Q_EFN-POI")

  ## Read in the required tab, from the required file.
  nat.stream.flow <- read.xlsx(file.path(global.input.dir, raw.nat.flows.in.dir, current.file),
                               sheet = required.tab)

  ## Identify the column which contains "UNADJUSTED" - this is within the "UNADJUSTED FOR LONG-TERM CONDITIONS" statement. This flag is used to identify the correct dataset to use for comparison to 1996-2010 dataset
  stream.poi.col.start <- which(apply(nat.stream.flow, 2, function(x) any(grepl("UNADJUSTED", x))))

  ## Select only the rows that contain data for Week 1 - Week 52
  nat.stream.flow <- nat.stream.flow[nat.stream.flow[,stream.poi.col.start] %in% Weeks,]

  ## Select the column with the weeks in, plus all of the columns for the 15 years of record
  nat.stream.flow <- nat.stream.flow[,(stream.poi.col.start + 1):(stream.poi.col.start + length(Years))]

  ## Ensure that all values are numeric
  nat.stream.flow[] <- lapply(nat.stream.flow, function(x) as.numeric(as.character(x)))

  ## Convert the matrix of values into a timeseries of values using the gather function. This stacks each column on top of each other
  nat.stream.flow.long <- gather(nat.stream.flow)

  ## Add a sequece of the years
  nat.stream.flow.long$Year <- rep(Years, each = 52)

  ## Add a sequence of the months
  nat.stream.flow.long$Week <- rep(1:52, length(Years))

  # nat.efn.flow.long <- nat.stream.flow.long$value - as.numeric(as.character(nat.flows.summary[nat.flows.summary$WATERSHED == include.watersheds, "GW_FACTOR"]))
  
  # ###########################################################
  # ##
  # ## Read-in Residual Flows for the Streamflow POI (i.e., Apex of the fan) - THESE ARE BASED ON THE LONG-TERM ADJUSTED NATURALIZED VALUES
  # ##
  # ###########################################################
  # 
  # ## Generate a character string which matches the tab which is needed from the reaquired.file
  # required.tab <- paste(include.watersheds, "Residual Q_EFN-POI")
  # 
  # ## Read in the required tab, from the required file.
  # res.stream.flow <- read.xlsx(file.path("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows", required.files),
  #                              sheet = required.tab)
  # 
  # ## Identify the column which contains "Residual - Streamflow POI" - This flag is used to identify the correct dataset to use for comparison to 1996-2010 dataset
  # stream.poi.location <- which(apply(res.stream.flow, 2, function(x) any(grepl("Residual - Streamflow POI", x))))
  # 
  # ## Select only the rows that contain data for Week 1 - Week 52
  # res.stream.flow <- res.stream.flow[res.stream.flow[,stream.poi.location] %in% Weeks,]
  # 
  # ## Select the column with the weeks in, plus all of the columns for the 15 years of record
  # res.stream.flow <- res.stream.flow[,(stream.poi.location + 1):(stream.poi.location + length(Years))]
  # 
  # ## Convert the matrix of values into a timeseries of values using the gather function. This stacks each column on top of each other
  # res.stream.flow.long <- gather(res.stream.flow)
  # 
  # ## Add a sequece of the years
  # res.stream.flow.long$Year <- rep(Years, each = 52)
  # 
  # ## Add a sequence of the months
  # res.stream.flow.long$Week <- rep(1:52, length(Years))

  
  ################################################################################################################
  ##
  ## Calculate Residual Streamflows at Streamflow POI (If available)
  ##
  #################################################################################################################
  
  ## Look to see if residual streamflows are avaivale for the current watershed
  if(nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "STREAM_RES"] == "Y"){
    
    
    ##---------------------------------------------------------------------------------------------------------------
    ##
    ## Read in OWDM data from naturalized streamflow dataset to allow computation of unadjusted residual flows at the Streamflow POI
    ##
    ##---------------------------------------------------------------------------------------------------------------
    
    ## Read in the OWDM Model Summary tab, from the required file.
    owdm.summary <- read.xlsx(file.path(global.input.dir, raw.nat.flows.in.dir, current.file),
                              sheet = "OWDM Model Summary")
    
    ## Identify the column which contains "Abv Fan" - this is within the "Abv Fan - OWDM Water Use" statement. This flag is used to identify the correct dataset to use for comparison to 1996-2010 dataset
    ## tail() is required to select the last returned colum - sometimes "Abv Fan" exists elsewhere, but I only want the last occurence (i.e., the summary table)
    owdm.col.start <- tail(which(apply(owdm.summary, 2, function(x) any(grepl("Abv Fan", x)))),1)
    
    ## Select only the rows that contain data for Week 1 - Week 52
    owdm.summary <- owdm.summary[owdm.summary[,owdm.col.start] %in% Weeks,]
    
    ## Select the column with the weeks in, plus all of the columns for the 15 years of record
    owdm.data <- owdm.summary[, (owdm.col.start + 1):(owdm.col.start + length(Years))]
    
    ## Ensure that all values are numeric
    owdm.data[] <- lapply(owdm.data, function(x) as.numeric(as.character(x)))
    
    ## Convert the matrix of values into a timeseries of values using the gather function. This stacks each column on top of each other
    owdm.data.long <- gather(owdm.data)
    
    ## Add a sequece of the years
    owdm.data.long$Year <- rep(Years, each = 52)
    
    ## Add a sequence of the months
    owdm.data.long$Week <- rep(1:52, length(Years))
    
    # efn.residual <- nat.efn.flow.long - as.numeric(owdm.data.long$value)
    
    ##---------------------------------------------------------------------------------------------------------------
    ##
    ## Calculate residual flows at the Streamflow POI
    ##
    ##---------------------------------------------------------------------------------------------------------------
    
    res.stream.poi <- nat.stream.flow.long$value - owdm.data.long$value
    
  } ## End IF statement for calculation of residual flows

   ##################################################################################################################
  ##
  ## Plot comparisons with Raven model output
  ##
  ##################################################################################################################

  
  ## Generate plotting locations for custom axis
  x <- 1:length(Years)
  tag <- x
  
  for(i in 2:length(tag)){
    ifelse(leap_year(Years[i-1]) == T, x <- 366, x <- 365)
    tag[i] <- tag[i-1] + x
  }
  
  loc <- seq(1, nrow(nat.stream.flow.long), by = 52)
  
  
  
  if(nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "STREAM_RES"] == "Y"){
  
  
    par(mfrow = c(2,1))
    
    ##-------------------------------------------------
    ##
    ## Plot naturalized streamflows vs. Raven output
    ##
    ##-------------------------------------------------
    
    plot(nat.stream.flow.long$value, type = 'l', ylab = "Mean Weekly Discharge (cms)",
         main = paste(current.watershed, "Creek Naturalized Streamflow Comparison"),
         xaxt = "n",
         xlab = "")
    
    lines(Raven.apex.weekly$Weekly.mean, col = 'red')
    # lines(Raven.mouth.weekly$Weekly.mean, col = 'blue', lty = 3)
    
    legend("topright", legend = c("Naturalized Streamflow (Apex of Fan)", "Raven Output (Apex of Fan)"),
           lty = c(1, 1),
           col = c("black", "red"),
           bty = "n")
    
    abline(h = 0, lty = 3, col = "grey")
    
    abline(v = loc, lty = 3, col = "grey")
    
    axis(side = 1, at = loc, labels = Days[tag])
    
  
    ##-------------------------------------------------
    ##
    ## Plot residual streamflows vs. Raven output
    ##
    ##-------------------------------------------------
  
    plot(res.stream.poi, type = 'l', ylab = "Mean Weekly Discharge (cms)",
         main = paste(current.watershed, "Creek Residual Streamflow Comparison"),
         xaxt = "n",
         xlab = "")
    
    lines(Raven.apex.weekly$Weekly.mean, col = 'red')
    # lines(Raven.mouth.weekly$Weekly.mean, col = 'blue', lty = 3)
    
    legend("topright", legend = c("Residual Streamflow (Apex of Fan)", "Raven Output (Apex of Fan)"),
           lty = c(1, 1),
           col = c("black", "red"),
           bty = "n")
    
    abline(h = 0, lty = 3, col = "grey")
    
    abline(v = loc, lty = 3, col = "grey")
    
    axis(side = 1, at = loc, labels = Days[tag])

    
  } else {
    
    ##-------------------------------------------------
    ##
    ## Plot naturalized streamflows vs. Raven output
    ##
    ##-------------------------------------------------
    
    plot(nat.stream.flow.long$value, type = 'l', ylab = "Mean Weekly Discharge (cms)",
         main = paste(current.watershed, "Creek Naturalized Streamflow Comparison"),
         xaxt = "n",
         xlab = "")
    
    lines(Raven.apex.weekly$Weekly.mean, col = 'red')
    # lines(Raven.mouth.weekly$Weekly.mean, col = 'blue', lty = 3)
    
    legend("topright", legend = c("Naturalized Streamflow (Apex of Fan)", "Raven Output (Apex of Fan)"),
           lty = c(1, 1),
           col = c("black", "red"),
           bty = "n")
    
    abline(h = 0, lty = 3, col = "grey")
    
    abline(v = loc, lty = 3, col = "grey")
    
    axis(side = 1, at = loc, labels = Days[tag])

    
    
    
    print(paste("No residual streamflow estimates exist for the", current.watershed, "watershed.")) 
  }  


} #End for loop of required.files


##################################################################################################################
##
## Generate summary statistics between weekly averaged model output and naturalized/residual streamflow datasets
##
##################################################################################################################

require(grid)
require(gridExtra)

## IF naturalized AND residual datasets exist
if(nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "STREAM_RES"] == "Y"){
  
  ## Round the results so that the number of days/weeks account for tiny flows.
  Times$Raven.apex <- round(Times$Raven.apex, 3)
  Raven.apex.weekly$Weekly.mean <- round(Raven.apex.weekly$Weekly.mean, 3)
  
  ## Create Summary of Naturalized Streamflow 
  
  summary.model.natural <- data.frame(Summary = c("Days with zero flow", "Minimum daily flow", "Weekly NSE", "Weeks with zero flow", "Minimum weekly mean flow", "LTmad"),
                                      Value = c(round(nrow(Times[Times$Raven.apex <= 0,]), 1),
                                                round(min(Times$Raven.apex, na.rm = T), 3),
                                                round(NSE(Raven.apex.weekly$Weekly.mean, nat.stream.flow.long$value), 3),
                                                round(nrow(Raven.apex.weekly[Raven.apex.weekly$Weekly.mean <= 0,]), 0),
                                                round(min(Raven.apex.weekly$Weekly.mean, na.rm = T), 3),
                                                round(mean(Times$Raven.apex, na.rm = T), 3))
    
  )
  
  summary.naturalized.flows <- data.frame(Summary = c("Days with zero flow", "Minimum daily flow", "Weekly NSE", "Weeks with zero flow", "Minimum weekly mean flow", "LTmad"),
                                          Value = c(NA,
                                                    NA,
                                                    NA,
                                                    round(nrow(nat.stream.flow.long[nat.stream.flow.long$value <= 0,]), 0),
                                                    round(min(nat.stream.flow.long$value, na.rm = T), 3),
                                                    round(mean(nat.stream.flow.long$value, na.rm = T), 3))
  )
  
  grid::grid.newpage()
  title.natural.model <- textGrob("Naturalized - Model Summary")
  title.naturalized.flows <- textGrob("Naturalized - Streamlow Summary")
  table.natural.model <- tableGrob(summary.model.natural)
  table.naturalized.flows <- tableGrob(summary.naturalized.flows)
  grid.arrange(title.natural.model, table.natural.model, title.naturalized.flows, table.naturalized.flows, nrow = 2, ncol = 2)
  
 
  ## Create Summary of Residual Streamflow 
  
  summary.model.residual <- data.frame(Summary = c("Days with zero flow", "Minimum daily flow", "Weekly NSE", "Weeks with zero flow", "Minimum weekly mean flow", "LTmad"),
                                      Value = c(round(nrow(Times[Times$Raven.apex <= 0,]), 1),
                                                round(min(Times$Raven.apex, na.rm = T), 3),
                                                round(NSE(Raven.apex.weekly$Weekly.mean, res.stream.poi), 3),
                                                round(nrow(Raven.apex.weekly[Raven.apex.weekly$Weekly.mean <= 0,]), 0),
                                                round(min(Raven.apex.weekly$Weekly.mean, na.rm = T), 3),
                                                round(mean(Times$Raven.apex, na.rm = T), 3))
                                      
  )
  
  summary.residual.flows <- data.frame(Summary = c("Days with zero flow", "Minimum daily flow", "Weekly NSE", "Weeks with zero flow", "Minimum weekly mean flow", "LTmad"),
                                          Value = c(NA,
                                                    NA,
                                                    NA,
                                                    round(length(res.stream.poi[res.stream.poi <= 0]), 0),
                                                    round(min(res.stream.poi, na.rm = T), 3),
                                                    round(mean(res.stream.poi, na.rm = T), 3))
  )
  
  

  grid::grid.newpage()
  title.residual.model <- textGrob("Residual - Model Summary")
  title.residual.flows <- textGrob("Residual - Streamlow Summary")
  table.residual.model <- tableGrob(summary.model.residual)
  table.residual.flows <- tableGrob(summary.residual.flows)
  grid.arrange(title.residual.model, table.residual.model, title.residual.flows, table.residual.flows, nrow = 2, ncol = 2)
  
} else {
  

  ## Round the results so that the number of days/weeks account for tiny flows.
  Times$Raven.apex <- round(Times$Raven.apex, 3)
  Raven.apex.weekly$Weekly.mean <- round(Raven.apex.weekly$Weekly.mean, 3)
  
  ## Create Summary of Naturalized Streamflow 
  summary.model.natural <- data.frame(Summary = c("Days with zero flow", "Minimum daily flow", "Weekly NSE", "Weeks with zero flow", "Minimum weekly mean flow", "LTmad"),
                                      Value = c(round(nrow(Times[Times$Raven.apex <= 0,]), 1),
                                                round(min(Times$Raven.apex, na.rm = T), 3),
                                                round(NSE(Raven.apex.weekly$Weekly.mean, nat.stream.flow.long$value), 3),
                                                round(nrow(Raven.apex.weekly[Raven.apex.weekly$Weekly.mean <= 0,]), 0),
                                                round(min(Raven.apex.weekly$Weekly.mean, na.rm = T), 3),
                                                round(mean(Times$Raven.apex, na.rm = T), 3))
                                      
  )
  
  ## No need to round this since it is rounded in Excel.
  summary.naturalized.flows <- data.frame(Summary = c("Days with zero flow", "Minimum daily flow", "Weekly NSE", "Weeks with zero flow", "Minimum weekly mean flow", "LTmad"),
                                          Value = c(NA,
                                                    NA,
                                                    NA,
                                                    round(nrow(nat.stream.flow.long[nat.stream.flow.long$value <= 0,]), 0),
                                                    round(min(nat.stream.flow.long$value, na.rm = T), 3),
                                                    round(mean(nat.stream.flow.long$value, na.rm = T), 3))
  )
  
  grid::grid.newpage()
  title.natural.model <- textGrob("Naturalized - Model Summary")
  title.naturalized.flows <- textGrob("Naturalized - Streamlow Summary")
  table.natural.model <- tableGrob(summary.model.natural)
  table.naturalized.flows <- tableGrob(summary.naturalized.flows)
  grid.arrange(title.natural.model, table.natural.model, title.naturalized.flows, table.naturalized.flows, nrow = 2, ncol = 2)

}


##################################################################################################################
##
## Calculate uncertainty bands on naturalized/residual streamflow datasets
##
##################################################################################################################

if(nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "STREAM_RES"] == "Y"){
  
  
  ## Determine residual and natural uncertainty values
  naturalized.uncertainty <- nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "NAT_UNCERTAINTY"]
  
  residual.uncertainty <- nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "RES_UNCERTAINTY"]
  
  ## Calculate upper and lower uncertainty bands for naturalized streamflows
  upper.naturalized.uncertainty <- nat.stream.flow.long$value + (nat.stream.flow.long$value * naturalized.uncertainty)
  
  lower.naturalized.uncertainty <- nat.stream.flow.long$value - (nat.stream.flow.long$value * naturalized.uncertainty)
  
  ## Calculate upper and lower uncertainty bands for residual streamflows
  upper.residual.uncertainty <- res.stream.poi + (res.stream.poi * residual.uncertainty)
  
  lower.residual.uncertainty <- res.stream.poi - (res.stream.poi * residual.uncertainty)
  
  ## Determine when the uncertainty is met
  natural.uncertainty.met <- Raven.apex.weekly$Weekly.mean >= lower.naturalized.uncertainty & Raven.apex.weekly$Weekly.mean <= upper.naturalized.uncertainty
  
  residual.uncertainty.met <- Raven.apex.weekly$Weekly.mean >= lower.residual.uncertainty & Raven.apex.weekly$Weekly.mean <= upper.residual.uncertainty
  
  ## Calculate the number of weeks when the modelled values fall within the uncertainty bounds
  no.natural.weeks.within.uncertainty <- length(Raven.apex.weekly$Weekly.mean[natural.uncertainty.met == TRUE])
  
  no.residual.weeks.within.uncertainty <- length(Raven.apex.weekly$Weekly.mean[residual.uncertainty.met == TRUE])
  
  
} else {
  
  ## Determine residual and natural uncertainty values
  naturalized.uncertainty <- nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "NAT_UNCERTAINTY"]
  
  ## Calculate upper and lower uncertainty bands for naturalized streamflows
  upper.naturalized.uncertainty <- nat.stream.flow.long$value + (nat.stream.flow.long$value * naturalized.uncertainty)
  
  lower.naturalized.uncertainty <- nat.stream.flow.long$value - (nat.stream.flow.long$value * naturalized.uncertainty)
  
  ## Determine when the uncertainty is met
  natural.uncertainty.met <- Raven.apex.weekly$Weekly.mean >= lower.naturalized.uncertainty & Raven.apex.weekly$Weekly.mean <= upper.naturalized.uncertainty
  
  ## Calculate the number of weeks when the modelled values fall within the uncertainty bounds
  no.natural.weeks.within.uncertainty <- length(Raven.apex.weekly[Raven.apex.weekly$Weekly.mean >= lower.naturalized.uncertainty & Raven.apex.weekly$Weekly.mean <= upper.naturalized.uncertainty, ] == TRUE)
  
}



if(nat.flows.summary[which(nat.flows.summary$WATERSHED == current.watershed), "STREAM_RES"] == "Y"){

  par(mfrow = c(2,1))
  
  ##-------------------------------------------------
  ##
  ## Plot naturalized streamflows vs. Raven output
  ##
  ##-------------------------------------------------
  
  raven.natural.uncertainty.test <- data.frame(natural.uncertainty.met = Raven.apex.weekly$Weekly.mean,
                                       natural.uncertainty.unmet = Raven.apex.weekly$Weekly.mean)
  
  raven.natural.uncertainty.test$natural.uncertainty.met[natural.uncertainty.met == FALSE] <- NA
  raven.natural.uncertainty.test$natural.uncertainty.unmet[natural.uncertainty.met == TRUE] <- NA
  
  plot(nat.stream.flow.long$value, type = 'l', ylab = "Mean Weekly Discharge (cms)",
       main = paste(current.watershed, "Creek Naturalized Streamflow Comparison"),
       xaxt = "n",
       xlab = "")
  
  polygon(c(1:780, rev(1:780)), c(lower.naturalized.uncertainty, rev(upper.naturalized.uncertainty)),
          col = 'grey',
          border = NA)
  lines(nat.stream.flow.long$value, col = 'black')
  
  lines(Raven.apex.weekly$Weekly.mean, col = "red")
  
  lines(raven.natural.uncertainty.test$natural.uncertainty.met, col = 'green')
  
  legend("topright", legend = c("Naturalized Streamflow (Apex of Fan)", "Raven Output (Apex of Fan) - outside accepted uncertainty", "Raven Output (Apex of Fan) - within accepted uncertainty"),
         lty = c(1, 1, 1),
         col = c("black", "red", "green"),
         bty = "n")
  
  abline(h = 0, lty = 3, col = "grey")
  
  abline(v = loc, lty = 3, col = "grey")
  
  axis(side = 1, at = loc, labels = Days[tag])
  
  ##-------------------------------------------------
  ##
  ## Plot residual streamflows vs. Raven output
  ##
  ##-------------------------------------------------
  
  raven.residual.uncertainty.test <- data.frame(residual.uncertainty.met = Raven.apex.weekly$Weekly.mean,
                                               residual.uncertainty.unmet = Raven.apex.weekly$Weekly.mean)
  
  raven.residual.uncertainty.test$residual.uncertainty.met[residual.uncertainty.met == FALSE] <- NA
  raven.residual.uncertainty.test$residual.uncertainty.unmet[residual.uncertainty.met == TRUE] <- NA
  
  plot(nat.stream.flow.long$value, type = 'l', ylab = "Mean Weekly Discharge (cms)",
       main = paste(current.watershed, "Creek Naturalized Streamflow Comparison"),
       xaxt = "n",
       xlab = "")
  
  polygon(c(1:780, rev(1:780)), c(lower.residual.uncertainty, rev(upper.residual.uncertainty)),
          col = 'grey',
          border = NA)
  lines(nat.stream.flow.long$value, col = 'black')
  
  lines(Raven.apex.weekly$Weekly.mean, col = "red")
  
  lines(raven.residual.uncertainty.test$residual.uncertainty.met, col = 'green')
  
  legend("topright", legend = c("Naturalized Streamflow (Apex of Fan)", "Raven Output (Apex of Fan) - outside accepted uncertainty", "Raven Output (Apex of Fan) - within accepted uncertainty"),
         lty = c(1, 1, 1),
         col = c("black", "red", "green"),
         bty = "n")
  
  abline(h = 0, lty = 3, col = "grey")
  
  abline(v = loc, lty = 3, col = "grey")
  
  axis(side = 1, at = loc, labels = Days[tag])
  
} else {
  
  ##-------------------------------------------------
  ##
  ## Plot naturalized streamflows vs. Raven output
  ##
  ##-------------------------------------------------
  
  raven.natural.uncertainty.test <- data.frame(natural.uncertainty.met = Raven.apex.weekly$Weekly.mean,
                                               natural.uncertainty.unmet = Raven.apex.weekly$Weekly.mean)
  
  raven.natural.uncertainty.test$natural.uncertainty.met[natural.uncertainty.met == FALSE] <- NA
  raven.natural.uncertainty.test$naturaluncertainty.unmet[natural.uncertainty.met == TRUE] <- NA
  
  plot(nat.stream.flow.long$value, type = 'l', ylab = "Mean Weekly Discharge (cms)",
       main = paste(current.watershed, "Creek Naturalized Streamflow Comparison"),
       xaxt = "n",
       xlab = "")
  
  polygon(c(1:780, rev(1:780)), c(lower.naturalized.uncertainty, rev(upper.naturalized.uncertainty)),
          col = 'grey',
          border = NA)
  lines(nat.stream.flow.long$value, col = 'black')
  
  lines(Raven.apex.weekly$Weekly.mean, col = "red")
  
  lines(raven.natural.uncertainty.test$natural.uncertainty.met, col = 'green')
  
  legend("topright", legend = c("Naturalized Streamflow (Apex of Fan)", "Raven Output (Apex of Fan) - outside accepted uncertainty", "Raven Output (Apex of Fan) - within accepted uncertainty"),
         lty = c(1, 1, 1),
         col = c("black", "red", "green"),
         bty = "n")
  
  abline(h = 0, lty = 3, col = "grey")
  
  abline(v = loc, lty = 3, col = "grey")
  
  axis(side = 1, at = loc, labels = Days[tag])
  
}


## Generate summary of number of weeks within uncertainty

# natural.uncertainty.summary <- data.frame(Summary = c("Number of weeks where accepted uncertainty is met", "Percentage of time accepted uncertainty is met"),
#                                           Value = c(no.natural.weeks.within.uncertainty,
#                                                     (no.natural.weeks.within.uncertainty / length(Raven.apex.weekly$Weekly.mean)*100)))
# 
# residual.uncertainty.summary <- data.frame(Summary = c("Number of weeks where accepted uncertainty is met", "Percentage of time accepted uncertainty is met"),
#                                           Value = c(no.residual.weeks.within.uncertainty,
#                                                     (no.residual.weeks.within.uncertainty / length(Raven.apex.weekly$Weekly.mean)*100)))
# 
# 
# grid::grid.newpage()
# title.natural.uncertainty <- textGrob("Naturalized - Model Summary")
# title.residual.uncertainty <- textGrob("Residual - Model Summary")
# table.natural.uncertainty <- tableGrob(natural.uncertainty.summary)
# table.residual.uncertainty <- tableGrob(residual.uncertainty.summary)
# grid.arrange(title.natural.uncertainty, table.natural.uncertainty, title.residual.uncertainty, table.residual.uncertainty, nrow = 2, ncol = 2)
