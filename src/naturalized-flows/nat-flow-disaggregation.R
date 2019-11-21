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
library(ggplot2)
#require(plyr)

# for AJS, set wd to let RStudio server File pane show files
# setwd("/var/obwb-hydro-modelling/input-data")

# LB: Can we just pull these from the naturalized-flows-summary csv file for the watersheds that are included? That way we're not extracting all stations from HYDAT if we're only modelling on watershed
# WSC gauges for daily:weekly ratio computation
ratio.gauges <- c("Whiteman" = "08NM174", "Camp" = "08NM134",
                  "Vaseux" = "08NM171", "Coldstream" = "08NM142",
                  "Trepanier" = "08NM041", "Shatford" = "08NM037",
                  "Inkaneep" = "08NM200", "West Kettle River" = "08NN015")

## Create a subdirectory to house all naturalized streamflow timeseries
dir.create(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "daily_naturalized_flows"))

subbasin.codes <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

## Specify the location where the HYDAT database is saved
hydat_here <- "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3"

# vector of watersheds to process
# include.watersheds <- c("Whiteman", "Equesis", "Inkaneep", "McDougall",
#                         "McLean", "Mission", "Naramata", "Naswhito",
#                         "Powers", "Shingle", "Shorts", "Shuttleworth",
#                         "Trepanier", "Trout", "Vaseux")

# read in naturalized-flows-summary to append 'lookup' info. Which WSC 
# gauge(s) to apply to which watershed, and in what proportion 

## Read in master Naturalized flows map file
#nfs <- read.csv("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/naturalized-flows-summary.csv")
nfs <- read.csv("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/naturalized-flows-summary.csv")

# change column classes to avoid error is disaggregation for loop
nfs$WSC_for_EFN <- as.character(nfs$WSC_for_EFN)
nfs$WSC_weeks_years <- as.character(nfs$WSC_weeks_years)


### which WSC gauge(s)'s daily Q to use to disaggregate weekly EFN streamflows
### WSC_ID to make it easier to join with HYDAT timeseries dataframe
# nfs[, c("WSC_for_EFN", "WSC_ID")] <- NA
# nfs[nfs$WATERSHED %in% c("Equesis", "Naswhito", "Shorts", "Whiteman"), c("WSC_for_EFN", "WSC_ID")] <- rep(c("Whiteman", "08NM174"), each = 4)
# nfs[nfs$WATERSHED %in% c("McLean", "Naramata", "Shuttleworth", "Vaseux"), c("WSC_for_EFN", "WSC_ID")] <- rep(c("Vaseux", "08NM171"), each = 4)
# nfs[nfs$WATERSHED %in% c("Trepanier", "Powers"), c("WSC_for_EFN", "WSC_ID")] <- rep(c("Trepanier", "08NM041"), each = 2)
# nfs[nfs$WATERSHED == "Inkaneep", c("WSC_for_EFN", "WSC_ID")] <- c(paste("Vaseux", "Inkaneep", sep = ", "),
#                                                                    paste("08NM171", "08NM200", sep = ", "))
# nfs[nfs$WATERSHED == "McDougall", c("WSC_for_EFN", "WSC_ID")] <- c(paste("Whiteman", "Camp", sep = ", "),
#                                                                     paste("08NM174", "08NM134", sep = ", "))
# nfs[nfs$WATERSHED == "Mission", c("WSC_for_EFN", "WSC_ID")] <- c("West Kettle River", "08NN015")
# nfs[nfs$WATERSHED == "Shingle", c("WSC_for_EFN", "WSC_ID")] <- c("Shatford", "08NM037")
# nfs[nfs$WATERSHED == "Trout", c("WSC_for_EFN", "WSC_ID")] <- c("Camp", "08NM134")
# # Coldstream, Mill, Penticton don't have rules yet. AJS 18-11-2019
# nfs[nfs$WATERSHED == "Coldstream", c("WSC_for_EFN", "WSC_ID")] <- c(NA, NA)
# nfs[nfs$WATERSHED == "Mill", c("WSC_for_EFN", "WSC_ID")] <- c(NA, NA)
# nfs[nfs$WATERSHED == "Penticton", c("WSC_for_EFN", "WSC_ID")] <- c(NA, NA)
# 
# # rules regarding which years to use which WSC data, and in what proportion (e.g., average 2 WSC gauge data)
# nfs[, c("WSC_weeks_years", "WSC_proportion")] <- NA
# nfs[, c("WSC_weeks_years", "WSC_proportion")] <- rep(c("1-1996 to 52-2010", 1), each = nrow(nfs))
# nfs[nfs$WATERSHED == "Inkaneep", c("WSC_weeks_years")] <- paste("1-1996 to 10-2006", "11-2006 to 52-2010", sep = ", ")
# nfs[nfs$WATERSHED == "McDougall", "WSC_proportion"] <- 0.5
# 
# # rules done. Write naturalized-flows-summary.csv to file
# write.csv(nfs, "./raw/naturalized-flows/naturalized-flows-summary_AJS.csv",
#           row.names = FALSE)

###################################################################################################################################################
##
## - Identify the correct naturalized streamflow dataset file to read in
##
###################################################################################################################################################


## List all files (all Associated Naturalized Streamflow files)
filenames <- list.files("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/")

## Identify which file is required to be read in based on the "include.watersheds" variable
#required.files <- filenames[gsub( " .*$", "", filenames) %in% include.watersheds]
required.files <- filenames[sapply(include.watersheds, grep, filenames)]

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

# Add week-year column for disaggregation rules lookup / filtering
Times$week.year <- paste(Times$Week, Times$Year, sep = "-")

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
  # if Whiteman, Trepanier or Coldstream Ck, pull that gauge and Camp Creek 
  # from HYDAT, and use Camp Creek to fill missing data in Whiteman
  #  else
  # pull data from HYDAT and process as normal
  
  if(ratio.gauges[i] %in% c("08NM174", "08NM041", "08NM142")){
  #if(ratio.gauges[i] == "08NM174"){
  # pull WSC daily discharge for current gauge from time period of interest
  wsc.daily <- hy_daily_flows(ratio.gauges[i], 
                              hydat_path = hydat_here,
                              start_date = min(Days), end_date = max(Days))
  
  # Camp Creek data to fill missing Whiteman Creek days
  wsc.daily2 <- hy_daily_flows(ratio.gauges[ratio.gauges == "08NM134"],
                               hydat_path = hydat_here,
                               start_date = min(Days), end_date = max(Days))
  

  # join Times to WSC data to extend gauge timeseries to full length
  # of timeseries. Due to nature of full_join, some rows of STATION_NUMBER
  # will now be NA. Adjust accordingly.
  wsc.daily <- wsc.daily %>% mutate(Date = ymd(Date)) %>%
    full_join(Times, by = c("Date" = "date")) %>%
    mutate(STATION_NUMBER = ratio.gauges[i])
  
  # Dates with missing Whiteman, Trepanier, or Coldstream data
  no.data <- wsc.daily$Date[is.na(wsc.daily$Value)]
  
  # get concurrent streamflow from Camp Creek
  Q_sub <- wsc.daily2$Value[which(wsc.daily2$Date %in% no.data)]
  rm(wsc.daily2)
  
  # add Camp Creek Q to Whiteman dataset
  wsc.daily$Value[wsc.daily$Date %in% no.data] <- Q_sub
   
  # group by unique Year-Week combinations, compute weekly average discharge.
  # full_join returns all unique rows. may not be necessary if gaps are irrelevant?
  # 1st mutate computes mean Q by each Year-Week group
  # ungroup removes grouping from the dataframe, all pipe calculations are
  # once again done row by row
  temp.df <- wsc.daily %>%
    group_by(Year, Week) %>%
    mutate(Weekly_Q = mean(Value)) %>%
    ungroup() %>%
    mutate(ratio = Value / Weekly_Q) %>%
    rename("Q" = Value) 
  
  } else {
    # pull WSC daily discharge for current gauge from time period of interest
    wsc.daily <- hy_daily_flows(ratio.gauges[i], 
                                hydat_path = hydat_here,
                                start_date = min(Days), end_date = max(Days))
    
    # add time series vector with index of OWDM weeks, group by unique Year-Week 
    # combinations, compute weekly average discharge.
    # full_join returns all unique rows. may not be necessary if gaps are irrelevant?
    # 1st mutate computes mean Q by each Year-Week group
    # ungroup removes grouping from the dataframe, all pipe calculations are
    # once again done row by row
    temp.df <- wsc.daily %>% mutate(Date = ymd(Date)) %>%
      full_join(Times, by = c("Date" = "date")) %>%
      mutate(STATION_NUMBER = ratio.gauges[i]) %>%
      group_by(Year, Week) %>%
      mutate(Weekly_Q = mean(Value)) %>%
      ungroup() %>%
      mutate(ratio = Value / Weekly_Q) %>%
      rename("Q" = Value) 
  }
  
  # checking code for correct computations
  # mean(test$Value[163:169]) == test$Weekly_Q[163:169]
  # {test$Value[5400] / test$Weekly_Q[5400]} == test$ratio[5400]
  
  # another example of piping sing the %<>% operator.
  # these two lines achieve the same result: 
  # ratio.df %<>% rbind(temp.df)
  ratio.df <- rbind(ratio.df, temp.df)
  
} # end daily:Weekly ratio for-loop 

# make week-year column for disaggregation look-up 
ratio.df %<>% mutate(week.year = paste(Week, Year, sep = "-"))

# find which stations and days still have NAs
## Inkaneep NAs are pre- week 11 2006, so that's ok as per
## naturalized-flow-summary rules. 
# na.df <- ratio.df %>%
#   filter(is.na(Q))
# 
# unique(na.df$STATION_NUMBER)
# 
# View(na.df %>% filter(STATION_NUMBER == "08NM200"))

## In naturalized streamflow summary csv, specify which station(s) should be used to determine the daily distribution.
## Retrieve these data from HYDAT for the 1996-2010 period of interest
# whiteman <- hy_daily_flows("08NM174", hydat_path = "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3",
#                            start_date = "1996-01-01", end_date = "2010-12-31")

## Read in the naturalized streamflow dataset

#### start disaggregation for-loop here
for (i in 1:length(include.watersheds)){
  
  # set name of .xlsx worksheet to load from file
  required.tab <- paste(include.watersheds[i], "Nat Q_EFN-POI")
  
  # read in worksheet with EFN data for current watershed
  nat.stream.flow <- read.xlsx(file.path("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows",
                                         required.files[grep(include.watersheds[i], required.files)]),
                               sheet = required.tab)
  
  ## Identify the column which contains "UNADJUSTED" - this is within the "UNADJUSTED FOR LONG-TERM CONDITIONS" 
  # statement. This flag is used to identify the correct dataset to use for comparison to 1996-2010 dataset
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
  
  # dataframe to hold disaggregation rules
  rules.df <- data.frame("wsc.gauge" = NA, "wsc.prop" = NA, "week.year" = NA,
                         "start" = NA, "end" = NA)
  
  # look up WSC gauges to use for disaggregation of EFN flows 
  nfs_row <- which(nfs$WATERSHED == include.watersheds[i])
  wsc.gauge <- unlist(strsplit(nfs$WSC_for_EFN[nfs_row], ", "))
  wsc.id <- ratio.gauges[names(ratio.gauges) %in% wsc.gauge]
  
  # look up proportion of WSC flows to apply to disaggregation 
  wsc.prop <- nfs$WSC_proportion[nfs_row]
  
  # week-years for the relevant WSC gauges
  wsc.years <- strsplit(nfs$WSC_weeks_years[nfs_row], ", ")
  wsc.years <- lapply(wsc.years, strsplit, " to ")
  
  # extract 1st and 2nd elements from list to make vectors of beginning
  # and ending week-year combinations
  wsc.start <- unlist(lapply(wsc.years[[1]], "[", 1))
  wsc.end <- unlist(lapply(wsc.years[[1]], "[", 2))
  
  rules.df <- data.frame("wsc.gauge" = wsc.gauge, 
                         "wsc.id" = wsc.id,
                         "wsc.prop" = wsc.prop,
                         "start" = wsc.start,
                         "end" = wsc.end, stringsAsFactors = FALSE)
  
  start <- rules.df %>% left_join(Times, by = c("start" = "week.year")) %>%
    group_by(start) %>%
    filter(date %in% min(date)) 
  end <- rules.df %>% left_join(Times, by = c("end" = "week.year")) %>%
    group_by(start) %>%
    filter(date %in% max(date))
  rules.df$start <- ymd(start$date)
  rules.df$end <- ymd(end$date)
  
  # filter flow ratio dataset to only the WSC gauges to be used for
  # disaggregation
  disagg.df <- ratio.df %>%
    filter(STATION_NUMBER %in% wsc.id) %>%
    left_join(rules.df, by = c("STATION_NUMBER" = "wsc.id")) %>%
    group_by(STATION_NUMBER) %>%
    filter(Date >= start & Date <= end)
  
  # plot Q vs. Date to see if filtering process worked properly. For
  # Inkaneep, Q timeseries should be split between Vaseux and Inkaneep.
  # for McDougall, both Whiteman and Camp should span the entire timespan
  # disagg.df %>%
  #   ggplot(aes(x = Date, y = Q, colour = STATION_NUMBER)) +
  #   geom_line() +
  #   theme_bw()
    
   
  # piped version of the originial workflow. Provided as an example.
  out.df <- disagg.df %>%
    left_join(nat.stream.flow.long, by = c("Year", "Week")) %>%
    rename("Weekly_Nat_Q" = value) %>%
    mutate(Daily_Nat_Q = Weekly_Nat_Q * ratio * wsc.prop) %>%
    group_by(Date) %>%
    summarize(Daily_Nat_Q = sum(Daily_Nat_Q),
              Week = unique(Week),
              Year = unique(Year)) %>%
    ungroup() %>%
    mutate(EFN_WATERSHED = include.watersheds[i])

  # # plot to check disaggregation of Q
  # out.df %>%
  #   ggplot(aes(x = Date, y = Daily_Nat_Q)) +
  #   geom_line() +
  #   theme_bw()
  # 
  # # plot to compare WSC daily Q vs. disaggregated EFN Q
  # out.df %>% left_join(disagg.df, by = "Date") %>%
  #   ggplot(aes(x = Date)) +
  #   geom_line(aes(y = Q, colour = "WSC")) +
  #   geom_line(aes(y = Daily_Nat_Q, colour = "EFN")) +
  #   theme_bw()
  
  ###################################################################################################################################################
  ##
  ## Write daily naturalized streamflows to *.rvt file(s)
  ##
  ###################################################################################################################################################
  
  DailyNatQRVTFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "daily_naturalized_flows", paste(include.watersheds[i], "_Nat_Q", ".rvt", sep = ""))
  
  main.RVT.file <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), paste(ws.interest, "-", run.number, ".rvt", sep = ""))
  
  apex.subbasin <- subbasin.codes[gsub(" Creek", "", subbasin.codes$GNIS_NAME) == include.watersheds[i] & subbasin.codes$Reports_to_Fan == "A", "Subbasin_ID"]
  
  
  ## Make na values = -1.2345
  out.df[is.na(out.df$Daily_Nat_Q), "Daily_Nat_Q"] <- -1.2345
  
  cat(file = DailyNatQRVTFile, sep = "", append = T,
      "# Custom rvt file for daily disaggregated naturalized streamflow datasets for ", as.character(include.watersheds[i]), " Creek watershed", "\n",
      ":ObservationData HYDROGRAPH " , as.character(apex.subbasin), " m3/s", "\n",
      sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(out.df$Date[1])),nrow(out.df)), "\n"
  )
  
  write.table(out.df$Daily_Nat_Q, DailyNatQRVTFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
  cat(file = DailyNatQRVTFile, sep = "", append = T,
      ":EndObservationData", "\n"
  )
  
  #####################################################
  ##
  ## Write Observation Weights
  ##
  #####################################################
  
  if(validate.model == FALSE){
    
    out.df$weights <- ifelse(out.df$Date < as.Date(calibration.start) | out.df$Date > as.Date(calibration.end), 0, 1)
    
  } else {
    
    out.df$weights <- ifelse(out.df$Date < as.Date(validation.start) | out.df$Date > as.Date(validation.end), 0, 1)
    
  }
  
  cat(file = DailyNatQRVTFile, sep = "", append = T,
      "\n",
      "# Write ObservationWeights", "\n",
      ":ObservationWeights HYDROGRAPH ", as.character(apex.subbasin), "\n",
      sprintf('%s 00:00:00 1.0 %i', as.character(lubridate::date(out.df$Date[1])),nrow(out.df)), "\n"
  )
  
  write.table(out.df$weights, DailyNatQRVTFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
  cat(file = DailyNatQRVTFile, sep = "", append = T,
      ":EndObservationWeights", "\n"
  )
  

  ## ----------------------------------------------------------------------
  ##
  ## Write Redirect command to the end of the main RVT file.
  ##
  ## ----------------------------------------------------------------------
  
  if(i == 1){
    
    cat(file = main.RVT.file, append = T, sep = "",
        "\n",
        "\n",
        "#-------------------------------------------------------", "\n",
        "#-------- Redirect to Daily Naturalized Flows ----------", "\n",
        "\n",
        ":RedirectToFile  ", paste("daily_naturalized_flows/", include.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
    )
    
  } else {
    
    cat(file = main.RVT.file, append = T, sep = "",
        ":RedirectToFile  ", paste("daily_naturalized_flows/", include.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
    )
    
  } # End else
  
  
  ## ----------------------------------------------------------------------
  ##
  ## If Ostrich RVT.TPL exists, write the Redirect to that too
  ##
  ## ----------------------------------------------------------------------
  
  if(run.ostrich == TRUE & file.exists(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
    
    OstrichRVTFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
    
    if(i == 1){
      
      cat(file = OstrichRVTFile, append = T, sep = "",
          "\n",
          "\n",
          "#-------------------------------------------------------", "\n",
          "#-------- Redirect to Custom Timeseries ----------------", "\n",
          "\n",
          ":RedirectToFile  ", paste("daily_naturalized_flows/", include.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
      )
      
    } else {
      
      cat(file = OstrichRVTFile, append = T, sep = "",
          ":RedirectToFile  ", paste("daily_naturalized_flows/", include.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
      )
      
    } # End else
    
  } # End if Ostrich is TRUE and RVT Template exists
  
} #### end disaggregation for-loop here



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
# getwd()
# 
# nfs.df <- read.csv("./raw/naturalized-flows/naturalized-flows-summary.csv",
#                    header = TRUE)


#------------------------------------------------------------------------


