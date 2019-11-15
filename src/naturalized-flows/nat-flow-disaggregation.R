###################################################################################################################################################
##
## This script acts as a trial for daily disaggregation of weekly naturalized streamflow datasets
##
## Nov-12-2019 LAB
## 
###################################################################################################################################################

## Modified to accept a vector of watersheds to loop through and process,
## producing disaggregated daily naturalized Q. 
## 14-Nov-2019 AJS 

# required libraries
library(dplyr)
require(openxlsx)
require(tidyr)
require(tidyhydat)
require(lubridate)
library(magrittr)
#require(plyr)

# for AJS, set wd to let RStudio server File pane show files
setwd("/var/obwb-hydro-modelling/input-data")

# WSC gauges for daily:weekly ratio computation
ratio.gauges <- c("Whiteman" = "08NM174", "Camp" = "08NM134", "Vaseux" = "08NM171",
                  "Coldstream" = "08NM142", "Trepanier" = "08NM041")

# vector of watersheds to process
include.watersheds <- "Whiteman"
#include.watersheds <- c("Whiteman", "Shorts")

### PLACEHOLDER -- read in naturalized-streamflow-summary.csv here to
###                get the lookup table for allocating WSC daily:weekly
###                Q ratio to specific watersheds

###################################################################################################################################################
##
## - Identify the correct naturalized streamflow dataset file to read in
##
###################################################################################################################################################

## Read in master Naturalized flows map file
nat.flows.summary <- read.csv("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/naturalized-flows-summary.csv")

## List all files (all Associated Naturalized Streamflow files)
filenames <- list.files("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/")

## Identify which file is required to be read in based on the "include.watersheds" variable
#required.files <- filenames[gsub( " .*$", "", filenames) %in% include.watersheds]
required.files <- filenames[grep(include.watersheds, filenames)]

###################################################################################################################################################
##
## Generate Year/Week timeseries which is consistent with the OWDM approach (i.e., 8 day last week of year, and 8 day Feb 29. week in leap years)
##
###################################################################################################################################################

## Generate a sequence of years which matches the naturalized streamflow datasets (i.e, 1996-2010)
Years <- seq(1996, 2010, 1)

## Generate Months sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
Months <- seq(as.Date("1996-01-01"), as.Date("2010-12-31"), by = "month")

## Generate a sequence of weeks which matches the naturalized streamflow datasets (i.e., 1-52)
Weeks <- paste("Week", seq(1, 52, 1))

## Generate Days sequence between 1996-01-01 - 2010-12-31 (Dates of available naturalized streamflows)
Days <- seq(as.Date("1996-01-01"), as.Date("2010-12-31"), by = "day")

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
  
  Times <- rbind(Times, Z)
}

colnames(Times) <- c("Year", "Week")

## Add date column - this is used to merge dates with Raven Output
Times$date <- Days

##########################################################################
#
# Compute weekly to daily disaggregation ratios
#
##########################################################################

# for WSC gauges used to create naturalized streamflow datasets, aggregate 
# daily Q to weekly Q and compute, for each day, daily Q : weekly averge Q
# ratio. These ratios will be used to disaggregate the weekly naturalized
# streamflows.
# 
# N.B. some gaps exist in WSC daily Q data. This is ok because Raven can 
# calibrate with gaps in data. The number of dail ynaturalized streamflow 
# obs. (computed) will match the number of WSC daily obs.

# ratio.gauges
# getwd()

# output dataframe to hold WSC-derived daily:weekly Q ratios
ratio.df <- data.frame()

for(i in 1:length(ratio.gauges)){
  # pull WSC daily discharge for current gauge from time period of interest
  wsc.daily <- hy_daily_flows(ratio.gauges[i], 
                              hydat_path = "./raw/wsc-hydat/Hydat.sqlite3",
                              start_date = min(Days), end_date = max(Days))
  
  # add time series vector with index of OWDM weeks, group by unique Year-Week 
  # combinations, compute weekly average discharge.
  # full_join returns all unique rows. may not be necessary if gaps are irrelevant?
  # 1st mutate computes mean Q by each Year-Week group
  # ungroup removes grouping from the dataframe, all pipe calculations are
  # once again done row by row
  temp.df <- wsc.daily %>% mutate(Date = ymd(Date)) %>%
    full_join(Times, by = c("Date" = "date")) %>%
    group_by(Year, Week) %>%
    mutate(Weekly_Q = mean(Value)) %>%
    ungroup() %>%
    mutate(ratio = Value / Weekly_Q) %>%
    rename("Q" = Value) 
  
  # checking code for correct computations
  # mean(test$Value[163:169]) == test$Weekly_Q[163:169]
  # {test$Value[5400] / test$Weekly_Q[5400]} == test$ratio[5400]
  
  # another example of piping sing the %<>% operator.
  # these two lines achieve the same result: 
  # ratio.df %<>% rbind(temp.df)
  ratio.df <- rbind(ratio.df, temp.df)
  
} # end daily:Weekly ratio for-loop 


## In naturalized streamflow summary csv, specify which station(s) should be used to determine the daily distribution.
## Retrieve these data from HYDAT for the 1996-2010 period of interest
# whiteman <- hy_daily_flows("08NM174", hydat_path = "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3",
#                            start_date = "1996-01-01", end_date = "2010-12-31")

## Read in the naturalized streamflow dataset

#### start disaggregation for-loop here

### PLACEHOLDER - make 'required.tab' a programmatically set variable
###               based on the current watershed to disaggregate
# always a c() of watershed name (var) and ' Nat Q_EFN-POI'
required.tab <- "Whiteman Nat Q_EFN-POI"

nat.stream.flow <- read.xlsx(file.path("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows", required.files),
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

###################################################################################################################################################
##
## Merge daily and weekly datasets into one dataframe
##
###################################################################################################################################################


# piped version of the originial workflow. Provided as an example.
disagg.df <- ratio.df %>%
  filter(STATION_NUMBER == ratio.gauges[names(ratio.gauges) == "Whiteman"]) %>%
  left_join(nat.stream.flow.long, by = c("Year", "Week")) %>%
  rename("Weekly_Nat_Q" = value) %>%
  mutate(Daily_Nat_Q = Weekly_Nat_Q * ratio)
  
### PLACEHOLDER - write code here to spit out disaggregated data to
###               where ever Lawrence needs it to go for Raven
###               model run


#### end disaggregation for-loop here


###### redundant after AJS modifications to loop through disagg. process 
# ## Merge data from HYDAT to ensure a complete timeseries
# # merged.data <- merge(whiteman, Times, by.x = "Date", by.y = "date", all.y = T)
# 
# ## Add the year, week, day columns to the merged dataframe
# merged.data <- cbind(merged.data, Times)
# 
# ## Join the weekly naturalized streamflows to the merged data, joining by week and year
# joined.data <- plyr::join(merged.data, nat.stream.flow.long,  by = c("Year", "Week"), match = "all")
# 
# ## Update the column names of the joined data frame
# colnames(joined.data) <- c("Date", "Station_No", "Parameter", "Daily.flow", "Symbol",
#                            "Year", "Week", "Year", "Week", "Date", "key", "Weekly.flow")
# 
# ## Calculate the weekly mean of recorded streamflows
# weekly.mean <- plyr::ddply(joined.data, .(Year, Week), summarize, weekly.mean = mean(Daily.flow))
# 
# ## Join the weekly mean value to the rest of the data
# all.data <- join(joined.data, weekly.mean, by = c("Year", "Week"), match = "all")
# 
# ## Calculate the distribution between daily and weekly mean flows
# all.data$distribution <- all.data$Daily.flow / all.data$weekly.mean
# 
# ## adjust the weekly naturalized streamflow value based on the daily distribution
# all.data$daily.nat.flow <- all.data$Weekly.flow * all.data$distribution
# 
# ## plot the results
# plot(all.data$daily.nat.flow, type = 'l')
# lines(all.data$Weekly.flow, col = 'red')
# 
# plot(test$Daily_Nat_Q, type = "l")


####### OLD. graveyard?
# ## Read in master Naturalized flows map file
# nat.flows.summary <- read.csv("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/naturalized-flows-summary.csv")
# 
# ## List all files (all Associated Naturalized Streamflow files)
# filenames <- list.files("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/")
# 
# ## Identify which file is required to be read in based on the "include.watersheds" variable
# required.files <- filenames[gsub( " .*$", "", filenames) %in% include.watersheds]
# 
# 
# 
# 
# daily.q <- read.xlsx(file.path("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows", required.files), sheet = "Daily Q_08NM174")
# 
# daily.q.start.row <- which(apply(daily.q, 1, function(r) any(r %in% c("Jan 1"))))
# 
# daily.q.start.col <- which(apply(daily.q, 2, function(r) any(r %in% c("Jan 1"))))
# 
# x <- daily.q[daily.q.start.row:(daily.q.start.row+365), (daily.q.start.col+1):ncol(daily.q)]
# 
# years <- as.numeric(daily.q[daily.q.start.row-1, (daily.q.start.col+1):ncol(daily.q)])
# 
# tidy <- gather(x)
# 
# dates <- seq(as.Date(paste(years[1], "-01-01", sep= "")), as.Date(paste(years[length(years)], "-12-31", sep = "")), by = "days")
# 
# 



#------------------------------------------------------------------------v
# temporary scratch pad AJS 14 nov 2019
getwd()

nfs.df <- read.csv("./raw/naturalized-flows/naturalized-flows-summary.csv",
                   header = TRUE)


#------------------------------------------------------------------------


