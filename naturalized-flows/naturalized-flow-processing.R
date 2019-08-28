###################################################################################################################################################
##
## This script ingests and summarizes naturalized streamflow datasets prepared by Associated for key Okanagan tributaries
##
## Apr-18-2019 LAB
##
###################################################################################################################################################

require(xlsx)
require(lubridate)
require(tidyr)
require(plyr)
require(hydroGOF)
require(RavenR)
require(data.table)

###########################################################
##
## - Identify the correct naturalized streamflow dataset file to read in
## - Create sequence of years and weeks
##
###########################################################

## Read in master Naturalized flows map file
nat.flows.summary <- read.csv("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/naturalized-flows-summary.csv")

## List all files (all Associated Naturalized Streamflow files)
filenames <- list.files("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/")

## Identify which file is required to be read in based on the "include.watersheds" variable
required.file <- filenames[grep(include.watersheds, filenames)]

## Generate a sequence of years which matches the naturalized streamflow datasets (i.e, 1996-2010)
Years <- seq(1996, 2010, 1)

## Generate a sequence of weeks which matches the naturalized streamflow datasets (i.e., 1-52)
Weeks <- paste("Week", seq(1, 52, 1))



###########################################################
##
## Read-in Naturalized Flows for the Streamflow POI (i.e., Apex of the fan)
##
###########################################################

## Generate a character string which matches the tab which is needed from the reaquired.file
required.tab <- paste(include.watersheds, "Nat Q_EFN-POI")

## Read in the required tab, from the required file.
nat.stream.flow <- read.xlsx(file.path("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows", required.file),
                             sheetName = required.tab)

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
# res.stream.flow <- read.xlsx(file.path("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows", required.file),
#                              sheetName = required.tab)
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
# 
# 
# lines(res.stream.flow.long$value, type = 'l', col = 'red')



##################################################################################################################
##
## Read in OWDM data from naturalized streamflow dataset to allow computation of unadjusted residual flows at the Streamflow POI
##
##################################################################################################################

## Read in the OWDM Model Summary tab, from the required file.
owdm.summary <- read.xlsx(file.path("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows", required.file),
                          sheetName = "OWDM Model Summary")

## Identify the column which contains "Abv Fan" - this is within the "Abv Fan - OWDM Water Use" statement. This flag is used to identify the correct dataset to use for comparison to 1996-2010 dataset
owdm.col.start <- which(apply(owdm.summary, 2, function(x) any(grepl("Abv Fan - OWDM Water Use", x))))

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


##################################################################################################################
##
## Calculate residual flows at the Streamflow POI
##
##################################################################################################################

res.stream.poi <- nat.stream.flow.long$value - owdm.data.long$value

## plot naturalized streamflow and residual streamflow at streamflow POI
plot(nat.stream.flow.long$value, type = 'l')
lines(res.stream.poi, col = 'red')
abline(h = 0, col = 'grey', lty = 3)

legend("topright", legend = c("Naturalized Streamflow (Apex of Fan)", "Residual Streamflow (Apex of Fan)"),
       lty = 1, col = c("black", "red"), bty = 'n')


# 
# output <- data.frame(stream.poi.nat = nat.stream.flow.long$value,
#                      stream.poi.res = res.stream.poi,
#                      Year = rep(Years, each = 52),
#                      Week = rep(1:52, length(Years)))

##################################################################################################################
##
## Generate Year/Week timeseries which is consistent with the OWDM approach (i.e., 8 day last week of year, and 8 day Feb 29. week in leap years)
##
##################################################################################################################

## Generate Months sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
Months <- seq(as.Date("1996-01-01"), as.Date("2010-12-31"), by = "month")

## Generate Days sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
Days <- seq(as.Date("1996-01-01"), as.Date("2010-12-31"), by = "day")

## Develop a vector of 365 days to represent a "Regular Year", broken into weeks which match the OWDM model setup (i.e., 8-day last week)
RegularYear <- c(rep(1:51, each = 7), rep(52, each = 8))

## Develop a vector of 366 days to represent a "Leap Year", broken into weeks which match the OWDM model setup (i.e., 8-day Feb 29. and last week)
LeapYear <- c(rep(1:8, each = 7), rep(9, each = 8), rep(10:51, each = 7), rep(52, each = 8))

## Create an empty vector 
# LY <- NA

####################
##
## Generate a timeseries of years between 1996 - 2010
##
####################

Year.timeseries <- data.frame()

for(i in 1:length(Years)){
  ifelse(leap_year(Years[i]) == T, Y <- rep(Years[i], each = 366), Y <- rep(Years[i], each = 365))
  Y <- data.frame(Y)
  Year.timeseries <- rbind(Year.timeseries, Y)
}

####################
##
## Attach a timeseries of weeks to the Year.timeseries and save the combined as "Output"
##
####################

Times <- data.frame()

for(i in 1:length(Years)){
  Z <- as.data.frame(Year.timeseries[Year.timeseries$Y == Years[i],])
  ifelse(leap_year(Years[i]) == T, Z$Week <- LeapYear, Z$Week <- RegularYear)
  
  Times <- rbind(Times,Z)
}

colnames(Times) <- c("Year", "Week")


##################################################################################################################
##
## Read in and process Raven output for the current model run
##
##################################################################################################################

## Read-in the subbasin table to allow subbasins at apex of fan and mouth of creek to be identified
subbasins <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

## Read-in the model results (hydrographs)
raven.output <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))

## Convert the dates to Date characters
raven.output$date <- as.Date(raven.output$date)

## Remove any dates which fall outside of the 1996-2010 window for which naturalized streamflows are available
raven.output <- raven.output[raven.output$date >= as.Date("1996-01-01") & raven.output$date <= as.Date("2010-12-31"),]


## Isolate the subbasin ID which corresponds with the Apex of the Alluvial Fan
stream.poi.subbasin.ID <- subbasins[subbasins$GNIS_NAME %in% paste(include.watersheds, "Creek") & subbasins$Reports_to_Fan == "A", "Subbasin_ID"]

stream.poi.subbasin.ID.col.name <- colnames(raven.output)[grepl(stream.poi.subbasin.ID, colnames(raven.output)) & !grepl("observed", colnames(raven.output))]

# stream.poi.subbasin.ID.col <- which(colnames(raven.output) %like% stream.poi.subbasin.ID)

## Isolate the subbasin ID which corresponds with the mouth of the creek
mouth.subbasin.ID <- subbasins[subbasins$GNIS_NAME %in% paste(include.watersheds, "Creek") & subbasins$Downstream_ID == "-1", "Subbasin_ID"]

mouth.subbasin.ID.col.name <- colnames(raven.output)[grepl(mouth.subbasin.ID, colnames(raven.output)) & !grepl("observed", colnames(raven.output))]

# mouth.subbasin.ID.col <- which(colnames(raven.output) %like% mouth.subbasin.ID)




####################
##
## Amalgamate "Times" and raven output for Apex of fan and mouth of creek
##
####################

Times$Raven.apex <- raven.output[,stream.poi.subbasin.ID.col.name]

Times$Raven.mouth <- raven.output[,mouth.subbasin.ID.col.name]

####################
##
## Calculate weekly mean for all weeks for each: Apex and Mouth
##
####################

Raven.apex.weekly <- ddply(Times, .(Year, Week), summarize, Weekly.mean = mean(Raven.apex))

Raven.mouth.weekly <- ddply(Times, .(Year, Week), summarize, Weekly.mean = mean(Raven.mouth))



plot(nat.stream.flow.long$value, type = 'l', ylab = "Mean Weekly Discharge (cms)",
     main = paste(include.watersheds, "Creek"))

lines(Raven.apex.weekly$Weekly.mean, col = 'red')
lines(Raven.mouth.weekly$Weekly.mean, col = 'blue', lty = 3)

legend("topright", legend = c("Naturalized Streamflow (Apex of Fan)", "Raven Output (Apex of Fan)", "Raven Output (Mouth)"),
       lty = c(1, 1, 3),
       col = c("black", "red", "blue"),
       bty = "n")

abline(h = 0, lty = 3, col = "grey")



# 
# 
# 
# plot(nat.stream.flow.long$value, raven.weekly$weekly, pch =19, xlab = "Whiteman Creek Naturalized Streamflow", ylab = "Raven Output")
# 
# 

NSE(Raven.apex.weekly$Weekly.mean, nat.stream.flow.long$value)
