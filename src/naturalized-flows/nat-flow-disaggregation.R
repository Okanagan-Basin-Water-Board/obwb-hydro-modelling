###################################################################################################################################################
##
## This script acts as a trial for daily disaggregation of weekly naturalized streamflow datasets
##
## Nov-12-2019 LAB
## 
###################################################################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

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

# script-specific watershed looping variable
#disagg.watersheds <- include.watersheds[include.watersheds != "Vernon"] # no longer necessary as Vernon Ck has been disaggregated
disagg.watersheds <- include.watersheds

# disagg.watersheds <- "Vernon"


# WSC gauges for daily:weekly ratio computation
ratio.gauges <- c("Whiteman" = "08NM174", "Camp" = "08NM134",
                  "Vaseux" = "08NM171", "Coldstream" = "08NM142",
                  "Trepanier" = "08NM041", "Shatford" = "08NM037",
                  "Inkaneep" = "08NM200", "West Kettle River" = "08NN015",
                  "Two Forty" = "08NM240", "Two Forty One" = "08NM241")

## Create a subdirectory to house all naturalized streamflow timeseries
dir.create(file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, sep = ""), "daily_naturalized_flows"))

subbasin.codes <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  
## Specify the location where the HYDAT database is saved
hydat_here <- file.path(global.input.dir, raw.hydat.in.dir, hydat.in.file)
  
# read in naturalized-flows-summary to append 'lookup' info. Which WSC 
# gauge(s) to apply to which watershed, and in what proportion 
## Read in master Naturalized flows map file
nfs <- read.csv(file.path(global.input.dir, raw.nat.flows.in.dir, nat.flow.summary.in.file),
                stringsAsFactors = FALSE)

###################################################################################################################################################
##
## - Identify the correct naturalized streamflow dataset file to read in
##
###################################################################################################################################################

## List all files (all Associated Naturalized Streamflow files)
filenames <- c(coldstream.nat.flow.in.file, equesis.nat.flow.in.file, inkaneep.nat.flow.in.file, mcdougall.nat.flow.in.file, mclean.nat.flow.in.file,
               mill.nat.flow.in.file, mission.nat.flow.in.file, naramata.nat.flow.in.file, naswhito.nat.flow.in.file, penticton.nat.flow.in.file,
               powers.nat.flow.in.file, shingle.nat.flow.in.file, shorts.nat.flow.in.file, shuttleworth.nat.flow.in.file, trepanier.nat.flow.in.file,
               trout.nat.flow.in.file, vaseux.nat.flow.in.file, vernon.nat.flow.in.file, whiteman.nat.flow.in.file)
  
## Identify which file is required to be read in based on the "disagg.watersheds" variable
required.files <- filenames[sapply(disagg.watersheds, grep, filenames)]

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

# output dataframe to hold WSC-derived daily:weekly Q ratios
ratio.df <- data.frame()

# creating vector of the watersheds to be disaggregated for the current
# model run(s). Saves a bit of time by not pulling WSC data from HYDAT for
# gauges not needed for current run. Drop NA induced by Vernon Creek, which
# is not disaggregated using WSC gauge data.
current.ratio.gauges <- nfs$WSC_ID[nfs$WATERSHED %in% disagg.watersheds]
current.ratio.gauges <- as.vector(na.omit(current.ratio.gauges))
current.ratio.gauges <- unique(unlist(strsplit(paste(current.ratio.gauges, collapse = ", "), ", ")))

# if current ratio gauge is empty, add a station to prevent script from failing
ifelse(length(current.ratio.gauges) == 0,
       current.ratio.gauges <- "08NM142",
       current.ratio.gauges)

for(i in 1:length(current.ratio.gauges)){
  # if Whiteman, Trepanier or Coldstream Ck, pull that gauge and Camp Creek 
  # from HYDAT, and use Camp Creek to fill missing data
  #  else
  # pull data from HYDAT and process as normal
  
  if(current.ratio.gauges[i] %in% c("08NM174", "08NM041", "08NM142")){
  # pull WSC daily discharge for current gauge from time period of interest
  wsc.daily <- hy_daily_flows(current.ratio.gauges[i], 
                              hydat_path = hydat_here,
                              start_date = min(Days), end_date = max(Days))
  
  # Camp Creek data to fill missing Whiteman Creek days
  wsc.daily2 <- hy_daily_flows("08NM134",
                               hydat_path = hydat_here,
                               start_date = min(Days), end_date = max(Days))
  
  # join Times to WSC data to extend gauge timeseries to full length
  # of timeseries. Due to nature of full_join, some rows of STATION_NUMBER
  # will now be NA. Adjust accordingly.
  wsc.daily <- wsc.daily %>% dplyr::mutate(Date = ymd(Date)) %>%
    full_join(Times, by = c("Date" = "date")) %>%
    dplyr::mutate(STATION_NUMBER = current.ratio.gauges[i])
  
  # Dates with missing Whiteman, Trepanier, or Coldstream data
  no.data <- wsc.daily$Date[is.na(wsc.daily$Value)]
  
  # get concurrent streamflow from Camp Creek
  Q_sub <- wsc.daily2$Value[which(wsc.daily2$Date %in% no.data)]
  rm(wsc.daily2)
  
  # add Camp Creek Q to Whiteman dataset
  wsc.daily$Value[wsc.daily$Date %in% no.data] <- Q_sub
   
  # group by unique Year-Week combinations, compute weekly average discharge.
  # full_join returns all unique rows. 
  # 1st mutate computes mean Q by each Year-Week group
  # ungroup removes grouping from the dataframe, all pipe calculations are
  # once again done row by row
  temp.df <- wsc.daily %>%
    group_by(Year, Week) %>%
    dplyr::mutate(Weekly_Q = mean(Value)) %>%
    ungroup() %>%
    dplyr::mutate(ratio = Value / Weekly_Q) %>%
    dplyr::rename("Q" = Value) 
  
  } else {
    # pull WSC daily discharge for current gauge from time period of interest
    wsc.daily <- hy_daily_flows(current.ratio.gauges[i], 
                                hydat_path = hydat_here,
                                start_date = min(Days), end_date = max(Days))
    
    # add time series vector with index of OWDM weeks, group by unique Year-Week 
    # combinations, compute weekly average discharge.
    # full_join returns all unique rows. may not be necessary if gaps are irrelevant?
    # 1st mutate computes mean Q by each Year-Week group
    # ungroup removes grouping from the dataframe, all pipe calculations are
    # once again done row by row
    temp.df <- wsc.daily %>% dplyr::mutate(Date = ymd(Date)) %>%
      full_join(Times, by = c("Date" = "date")) %>%
      dplyr::mutate(STATION_NUMBER = current.ratio.gauges[i]) %>%
      group_by(Year, Week) %>%
      dplyr::mutate(Weekly_Q = mean(Value)) %>%
      ungroup() %>%
      dplyr::mutate(ratio = Value / Weekly_Q) %>%
      dplyr::mutate(ratio = case_when(Value == 0 ~ 0,
                               Weekly_Q == 0 ~ 0,
                               Value != 0 & Weekly_Q != 0 ~ Value / Weekly_Q)) %>%
      dplyr::rename("Q" = Value) 
  }
  
  # compiling single dataframe of WSC gauge streamflow and daily:weekly Q ratios
  ratio.df <- rbind(ratio.df, temp.df)
  
} # end daily:Weekly ratio for-loop 

# make week-year column for disaggregation look-up 
ratio.df %<>% dplyr::mutate(week.year = paste(Week, Year, sep = "-"))

#### start disaggregation for-loop here
for (i in 1:length(disagg.watersheds)){
  
  if(disagg.watersheds[i] == "Vernon"){
    # set name of .xlsx worksheet to load from file. 
    # Nat Q_EFN-POI is for Vernon Creek at the Mouth
    # Outlet Nat Q_EFN-POI is for Vernon Creek at the outlet of Kal Lake
    required.tab <- paste(disagg.watersheds[i], "Nat Q_EFN-POI")
    required.tab2 <- paste(disagg.watersheds[i], "Outlet Nat Q_EFN-POI")
  
    # read in worksheet with EFN data for current watershed
    # Vernon has at the Mouth and Kal Lake Outlet flows to handle
    nat.stream.flow.mouth <- read.xlsx(file.path(global.input.dir, raw.nat.flows.in.dir,
                                       required.files[grep(disagg.watersheds[i], required.files)]),
                                       sheet = required.tab,
                                       startRow = 2)  
    nat.stream.flow.outlet <- read.xlsx(file.path(global.input.dir, raw.nat.flows.in.dir,
                                        required.files[grep(disagg.watersheds[i], required.files)]),
                                        sheet = required.tab2,
                                        startRow = 2)  
    
    #----------------------
    # process Vernon Creek mouth
    #----------------------
    
    # change column name to something more user-friendly
    colnames(nat.stream.flow.mouth)[ncol(nat.stream.flow.mouth)] <- "Weekly_discharge"
    
    nat.stream.flow.mouth <- nat.stream.flow.mouth %>%
      dplyr::mutate(week.year = paste0(as.numeric(Week.ID),
                               "-",
                               Year)) %>%
      select(Year, week.year, Weekly_discharge)
    
    disagg.df <- nat.stream.flow.mouth %>% left_join(Times, by = c("Year", "week.year"))
    
    # find the rows that correspond to the start of a week
    sow <- match(unique((disagg.df$week.year)), disagg.df$week.year)
    
    # create vector of weekly flows for interpolation. only start of week has
    # a value, rest of days in week are NA, to make linear interpolation
    # easier
    weekly_flow <- disagg.df$Weekly_discharge
    gap <- (1:length(weekly_flow))[-sow]
    weekly_flow[gap] <- NA
    
    # add 'gappy' Weekly flow series column
    disagg.df$Daily_discharge <- weekly_flow
    
    # do linear approximation. x is the 'gappy' weekly flow series and 
    # xout tells `approx` is an index of elements in weekly_flow for which
    # to produce linearly approximated estimates
    gap_fill <- approx(weekly_flow, xout = gap)$y
    
    # add linearly approximated flows to daily discharge column
    disagg.df$Daily_discharge[gap] <- gap_fill
      
    # format for output
    out.df <- disagg.df %>%
      dplyr::rename(Date = "date",
             Daily_Nat_Q = "Daily_discharge") %>%
      dplyr::mutate(EFN_Watershed = "Vernon") %>%
      select(Date, Daily_Nat_Q, Week, Year, EFN_Watershed) 

    #----------------------
    # process Kal Lake outlet
    #----------------------
      
    # change column name to something more user-friendly
    colnames(nat.stream.flow.outlet)[ncol(nat.stream.flow.outlet)] <- "Weekly_discharge"
    
    nat.stream.flow.outlet <- nat.stream.flow.outlet %>%
      dplyr::mutate(week.year = paste0(as.numeric(Week.ID),
                                "-",
                                Year)) %>%
      select(Year, week.year, Weekly_discharge)
    
    disagg.df <- nat.stream.flow.outlet %>% left_join(Times, by = c("Year", "week.year"))
    
    # find the rows that correspond to the start of a week
    sow <- match(unique((disagg.df$week.year)), disagg.df$week.year)
    
    # create vector of weekly flows for interpolation. only start of week has
    # a value, rest of days in week are NA, to make linear interpolation
    # easier
    weekly_flow <- disagg.df$Weekly_discharge
    gap <- (1:length(weekly_flow))[-sow]
    weekly_flow[gap] <- NA
    
    # add 'gappy' Weekly flow series column
    disagg.df$Daily_discharge <- weekly_flow
    
    # do linear approximation. x is the 'gappy' weekly flow series and 
    # xout tells `approx` is an index of elements in weekly_flow for which
    # to produce linearly approximated estimates
    gap_fill <- approx(weekly_flow, xout = gap)$y
    
    # add linearly approximated flows to daily discharge column
    disagg.df$Daily_discharge[gap] <- gap_fill
    
    # format for output
    out.df.kal <- disagg.df %>%
      dplyr::rename(Date = "date",
             Daily_Nat_Q = "Daily_discharge") %>%
      dplyr::mutate(EFN_Watershed = "Vernon") %>%
      select(Date, Daily_Nat_Q, Week, Year, EFN_Watershed)

  } else {
  
    # set name of .xlsx worksheet to load from file
    required.tab <- paste(disagg.watersheds[i], "Nat Q_EFN-POI")
    
    # read in worksheet with EFN data for current watershed
    nat.stream.flow <- read.xlsx(file.path(global.input.dir, raw.nat.flows.in.dir,
                                           required.files[grep(disagg.watersheds[i], required.files)]),
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
    nfs_row <- which(nfs$WATERSHED == disagg.watersheds[i])
    wsc.gauge <- unlist(strsplit(nfs$WSC_for_EFN[nfs_row], ", "))
    wsc.id <- ratio.gauges[names(ratio.gauges) %in% wsc.gauge]
    
    # reorder wsc.id so match the same order of wsc.gauge, otherwise data
    # gets misattributed (wrong WSC ID) in the following steps
    wsc.id <- wsc.id[match(wsc.gauge, names(wsc.id))]
    
    # look up proportion of WSC flows to apply to disaggregation 
    wsc.prop <- nfs$WSC_proportion[nfs_row]
    
    # week-years for the relevant WSC gauges
    wsc.years <- strsplit(nfs$WSC_weeks_years[nfs_row], ", ")
    wsc.years <- lapply(wsc.years, strsplit, " to ")
    
    # extract 1st and 2nd elements from list to make vectors of beginning
    # and ending week-year combinations
    wsc.start <- unlist(lapply(wsc.years[[1]], "[", 1))
    wsc.end <- unlist(lapply(wsc.years[[1]], "[", 2))
    
    # building dataframe of disaggregation rules
    rules.df <- data.frame("wsc.gauge" = wsc.gauge, 
                           "wsc.id" = wsc.id,
                           "wsc.prop" = wsc.prop,
                           "start" = wsc.start,
                           "end" = wsc.end, stringsAsFactors = FALSE)
    
    # grabbing start and end dates for each disaggregation WSC gauge so that
    # the ratio.df can be filtered by WSC gauge and its relevent time period
    start <- rules.df %>% left_join(Times, by = c("start" = "week.year")) %>%
      group_by(start) %>%
      dplyr::filter(date %in% min(date)) 
    end <- rules.df %>% left_join(Times, by = c("end" = "week.year")) %>%
      group_by(start) %>%
      dplyr::filter(date %in% max(date))
    rules.df$start <- ymd(start$date)
    rules.df$end <- ymd(end$date)
    
    # filter flow ratio dataset to only the WSC gauges to be used for
    # disaggregation and for the relevant time period for each gauge
    disagg.df <- ratio.df %>%
      dplyr::filter(STATION_NUMBER %in% wsc.id) %>%
      left_join(rules.df, by = c("STATION_NUMBER" = "wsc.id")) %>%
      group_by(STATION_NUMBER) %>%
      dplyr::filter(Date >= start & Date <= end)
    
    # Penticton's rules are a bit more complex. Average Two Forty and 
    # Two Forty One Creek, and then average that with Vaseux
    if(disagg.watersheds[i] == "Penticton"){
      
      # Penticton Creek has slightly more complex rules for disagg.
      # Average the daily disaggregation ratios for 240 and 241 creeks, and
      # average those means with the Vaseux creek daily disagg. ratios. Then
      # disaggregate the weekly naturalized Q 
      out.df <- disagg.df %>%
        dplyr::mutate(rule = case_when(STATION_NUMBER == "08NM171" ~ 2,
                                STATION_NUMBER == "08NM240" ~ 1,
                                STATION_NUMBER == "08NM241" ~ 1)) %>%
        left_join(nat.stream.flow.long, by = c("Year", "Week")) %>%
        dplyr::rename("Weekly_Nat_Q" = value) %>%
        group_by(rule, Date) %>%
        dplyr::summarize(Daily_Nat_Q = unique(Weekly_Nat_Q) * mean(ratio),
                  Week = unique(Week), Year = unique(Year)) %>%
        ungroup %>% group_by(Date) %>%
        dplyr::summarize(Daily_Nat_Q = mean(Daily_Nat_Q),
                  Week = unique(Week), Year = unique(Year)) %>%
        ungroup() %>%
        dplyr::mutate(EFN_watershed = disagg.watersheds[i])

    } else {
     
      # join EFN streamflow data to WSC data, disaggregate weekly naturalized Q per
      # WSC gauge, then sum the disaggregated daily proportions.
      out.df <- disagg.df %>%
        left_join(nat.stream.flow.long, by = c("Year", "Week")) %>%
        dplyr::rename("Weekly_Nat_Q" = value) %>%
        dplyr::mutate(Daily_Nat_Q = Weekly_Nat_Q * ratio * wsc.prop) %>%
        group_by(Date) %>%
        dplyr::summarize(Daily_Nat_Q = sum(Daily_Nat_Q),
                  Week = unique(Week),
                  Year = unique(Year)) %>%
        ungroup() %>%
        dplyr::mutate(EFN_WATERSHED = disagg.watersheds[i])

    }
  } # end if-else for Vernon Ck naturalized flow and OK Tennant EFN datasets

  ###################################################################################################################################################
  ##
  ## Write daily naturalized streamflows to *.rvt file(s)
  ##
  ###################################################################################################################################################
  
  DailyNatQRVTFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, sep = ""), "daily_naturalized_flows", paste(disagg.watersheds[i], "_Nat_Q", ".rvt", sep = ""))
  
  main.RVT.file <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, sep = ""), paste(ws.interest, "-", run.number, ".rvt", sep = ""))
  
  apex.subbasin <- subbasin.codes[gsub(" Creek", "", subbasin.codes$GNIS_NAME) == disagg.watersheds[i] & subbasin.codes$Reports_to_Fan == "A", "Subbasin_ID"]
  
  
  ## Make na values = -1.2345
  out.df[is.na(out.df$Daily_Nat_Q), "Daily_Nat_Q"] <- -1.2345
  
  cat(file = DailyNatQRVTFile, sep = "", append = T,
      "# Custom rvt file for daily disaggregated naturalized streamflow datasets for ", as.character(disagg.watersheds[i]), " Creek watershed", "\n",
      ":ObservationData HYDROGRAPH " , as.character(apex.subbasin), " m3/s", "\n",
      sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(out.df$Date[1])),nrow(out.df)), "\n"
  )
  
  write.table(out.df$Daily_Nat_Q, DailyNatQRVTFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
  
  cat(file = DailyNatQRVTFile, sep = "", append = T,
      ":EndObservationData", "\n"
  )
  
  
  ### IF VERNON IS WITHIN THE WATERSHED OF INTEREST STRING, WRITE A SECOND *.RVT FILE
  ### for KAL LAKE OUTLET
  if(disagg.watersheds[i] == "Vernon"){
    
    DailyNatQRVTFile.Kal <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, sep = ""), "daily_naturalized_flows", paste(disagg.watersheds[i], "_Kal_Nat_Q", ".rvt", sep = ""))
    
    main.RVT.file <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, sep = ""), paste(ws.interest, "-", run.number, ".rvt", sep = ""))
    
    kal.subbasin <- "3017"
    
    
    ## Make na values = -1.2345
    out.df.kal[is.na(out.df.kal$Daily_Nat_Q), "Daily_Nat_Q"] <- -1.2345
    
    cat(file = DailyNatQRVTFile.Kal, sep = "", append = T,
        "# Custom rvt file for daily disaggregated naturalized streamflow datasets for ", as.character(disagg.watersheds[i]), " Creek watershed at the outlet of Kal Lake", "\n",
        ":ObservationData HYDROGRAPH " , as.character(kal.subbasin), " m3/s", "\n",
        sprintf('%s 00:00:00 1.0 %i',as.character(lubridate::date(out.df$Date[1])),nrow(out.df)), "\n"
    )
    
    write.table(out.df.kal$Daily_Nat_Q, DailyNatQRVTFile.Kal, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
    
    cat(file = DailyNatQRVTFile.Kal, sep = "", append = T,
        ":EndObservationData", "\n"
    )
    
    
    
  }
  
  #####################################################
  ##
  ## Write Observation Weights
  ##
  #####################################################
  
  if(validate.model == FALSE){
    
    out.df$weights <- ifelse(out.df$Date < base::as.Date(calibration.start) | out.df$Date > base::as.Date(calibration.end), 0, 1)
    
  } else {
    
    out.df$weights <- ifelse(out.df$Date < base::as.Date(validation.start) | out.df$Date > base::as.Date(validation.end), 0, 1)
    
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
  
  
  if(disagg.watersheds[i] == "Vernon"){
    
    cat(file = DailyNatQRVTFile.Kal, sep = "", append = T,
        "\n",
        "# Write ObservationWeights", "\n",
        ":ObservationWeights HYDROGRAPH ", as.character(kal.subbasin), "\n",
        sprintf('%s 00:00:00 1.0 %i', as.character(lubridate::date(out.df$Date[1])),nrow(out.df)), "\n"
    )
    
    write.table(out.df$weights, DailyNatQRVTFile.Kal, append = T, col.names = F, row.names = F, sep = "\t", quote = F)
    
    cat(file = DailyNatQRVTFile.Kal, sep = "", append = T,
        ":EndObservationWeights", "\n"
    )
    
    
  }
  

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
        ":RedirectToFile  ", paste("daily_naturalized_flows/", disagg.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
    )
    
  } else {
    
    cat(file = main.RVT.file, append = T, sep = "",
        ":RedirectToFile  ", paste("daily_naturalized_flows/", disagg.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
    )
    
  } # End else
  
  if(disagg.watersheds[i] == "Vernon"){
    
    cat(file = main.RVT.file, append = T, sep = "",
        "\n",
        "\n",
        "#-------------------------------------------------------", "\n",
        "#-------- Redirect to Daily Naturalized Flows at Kal Lake Outlet ----------", "\n",
        "\n",
        ":RedirectToFile  ", paste("daily_naturalized_flows/", disagg.watersheds[i], "_Kal_Nat_Q", ".rvt", sep = ""), "\n"
    )
    
  }
  
  
  ## ----------------------------------------------------------------------
  ##
  ## If Ostrich RVT.TPL exists, write the Redirect to that too
  ##
  ## ----------------------------------------------------------------------
  
  if(run.ostrich == TRUE & file.exists(file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = "")))){
    
    OstrichRVTFile <- file.path(global.simulation.dir, ws.interest, paste(ws.interest, "-", run.number, sep = ""), "templates", paste(ws.interest, "-", run.number, ".rvt.tpl", sep = ""))
    
    if(i == 1){
      
      cat(file = OstrichRVTFile, append = T, sep = "",
          "\n",
          "\n",
          "#-------------------------------------------------------", "\n",
          "#-------- Redirect to Custom Timeseries ----------------", "\n",
          "\n",
          ":RedirectToFile  ", paste("daily_naturalized_flows/", disagg.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
      )
      
    } else {
      
      cat(file = OstrichRVTFile, append = T, sep = "",
          ":RedirectToFile  ", paste("daily_naturalized_flows/", disagg.watersheds[i], "_Nat_Q", ".rvt", sep = ""), "\n"
      )
      
    } # End else
    
    if(disagg.watersheds[i] == "Vernon"){
      
      cat(file = OstrichRVTFile, append = T, sep = "",
          "\n",
          "\n",
          "#-------------------------------------------------------", "\n",
          "#-------- Redirect to Custom Timeseries for Kal Lake Outflow ----------------", "\n",
          "\n",
          ":RedirectToFile  ", paste("daily_naturalized_flows/", disagg.watersheds[i], "_Kal_Nat_Q", ".rvt", sep = ""), "\n"
      )
      
      
    }
    
    
    
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
# ## Identify which file is required to be read in based on the "disagg.watersheds" variable
# required.files <- filenames[gsub( " .*$", "", filenames) %in% disagg.watersheds]
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

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
###  Tidying up loose ends AJS 22-11-2019

# # vector of watersheds to process
# disagg.watersheds <- c("Whiteman", "Equesis", "Coldstream", "Inkaneep", 
#                         "McDougall", "McLean", "Mill", "Mission", "Naramata",
#                         "Naswhito", "Penticton", "Powers", "Shingle", "Shorts", 
#                         "Shuttleworth", "Trepanier", "Trout", "Vaseux")

# ### which WSC gauge(s)'s daily Q to use to disaggregate weekly EFN streamflows
# ### WSC_ID to make it easier to join with HYDAT timeseries dataframe
# nfs[, c("WSC_for_EFN", "WSC_ID")] <- NA
# nfs[nfs$WATERSHED %in% c("Equesis", "Mill", "Naswhito", "Shorts", "Whiteman"), c("WSC_for_EFN", "WSC_ID")] <- rep(c("Whiteman", "08NM174"), each = 5)
# nfs[nfs$WATERSHED %in% c("McLean", "Naramata", "Shuttleworth", "Vaseux"), c("WSC_for_EFN", "WSC_ID")] <- rep(c("Vaseux", "08NM171"), each = 4)
# nfs[nfs$WATERSHED %in% c("Trepanier", "Powers"), c("WSC_for_EFN", "WSC_ID")] <- rep(c("Trepanier", "08NM041"), each = 2)
# nfs[nfs$WATERSHED == "Penticton", c("WSC_for_EFN", "WSC_ID")] <- c(paste("Two Forty", "Two Forty One", "Vaseux", sep = ", "),
#                                                                    paste("08NM240", "08NM241", "08NM171", sep = ", "))
# nfs[nfs$WATERSHED == "Inkaneep", c("WSC_for_EFN", "WSC_ID")] <- c(paste("Vaseux", "Inkaneep", sep = ", "),
#                                                                    paste("08NM171", "08NM200", sep = ", "))
# nfs[nfs$WATERSHED == "McDougall", c("WSC_for_EFN", "WSC_ID")] <- c(paste("Whiteman", "Camp", sep = ", "),
#                                                                     paste("08NM174", "08NM134", sep = ", "))
# nfs[nfs$WATERSHED == "Mission", c("WSC_for_EFN", "WSC_ID")] <- c("West Kettle River", "08NN015")
# nfs[nfs$WATERSHED == "Shingle", c("WSC_for_EFN", "WSC_ID")] <- c("Shatford", "08NM037")
# nfs[nfs$WATERSHED == "Trout", c("WSC_for_EFN", "WSC_ID")] <- c("Camp", "08NM134")
# nfs[nfs$WATERSHED == "Coldstream", c("WSC_for_EFN", "WSC_ID")] <- c("Coldstream", "08NM142")
# 
# 
# # rules regarding which years to use which WSC data, and in what proportion (e.g., average 2 WSC gauge data)
# nfs[, c("WSC_weeks_years", "WSC_proportion")] <- NA
# nfs[, c("WSC_weeks_years", "WSC_proportion")] <- rep(c("1-1996 to 52-2010", 1), each = nrow(nfs))
# nfs[nfs$WATERSHED == "Inkaneep", c("WSC_weeks_years")] <- paste("1-1996 to 10-2006", "11-2006 to 52-2010", sep = ", ")
# nfs[nfs$WATERSHED == "McDougall", "WSC_proportion"] <- 0.5
# nfs[nfs$WATERSHED == "Penticton", "WSC_proportion"] <- 0.5
# 
# # change column classes to avoid error in disaggregation for loop
# # nfs$WSC_proportion <- as.numeric(nfs$WSC_proportion)
# # nfs$WSC_for_EFN <- as.character(nfs$WSC_for_EFN)
# # nfs$WSC_weeks_years <- as.character(nfs$WSC_weeks_years)
# # nfs$WSC_for_EFN <- as.character(nfs$WSC_for_EFN)
# 
# # rules done. Write naturalized-flows-summary.csv to file
# write.csv(nfs, "./raw/naturalized-flows/naturalized-flows-summary_AJS.csv",
#           row.names = FALSE)

# find which stations and days still have NAs
## Inkaneep NAs are pre- week 11 2006, so that's ok as per
## naturalized-flow-summary rules. 
# na.df <- ratio.df %>%
#   filter(is.na(Q))
# 
# unique(na.df$STATION_NUMBER)
# 
# View(na.df %>% filter(STATION_NUMBER == "08NM200"))

# plot Q vs. Date to see if filtering process worked properly. For
# Inkaneep, Q timeseries should be split between Vaseux and Inkaneep.
# for McDougall, both Whiteman and Camp should span the entire timespan
# disagg.df %>%
#   ggplot(aes(x = Date, y = Q, colour = STATION_NUMBER)) +
#   geom_line() +
#   theme_bw()

# # compute mean annual discharge as QA/QC
# # piping black magic to save intermediate steps
# mad <- out.df %>%
#   left_join(nat.stream.flow.long, by = c("Year", "Week")) %>%
#   rename("Weekly_Nat_Q" = value) %>%
#   group_by(Year) %T>%
#   write.csv("./raw/naturalized-flows/AJS_temp/Penticton_EFN_disagg.csv",
#             row.names = FALSE) %>%
#   summarize(MAD_daily_disagg = mean(Daily_Nat_Q),
#             MAD_weekly_EFN = mean(Weekly_Nat_Q)) %T>%
#   write.csv("./raw/naturalized-flows/AJS_temp/Penticton_disagg_MAD.csv",
#             row.names = FALSE)

# compute mean annual discharge as QA/QC
# mad <- out.df %>%
#   left_join(nat.stream.flow.long, by = c("Year", "Week")) %>%
#   rename("Weekly_Nat_Q" = value) %>%
#   group_by(Year) %>%
#   summarize(MAD_daily_disagg = mean(Daily_Nat_Q),
#             MAD_weekly_EFN = mean(Weekly_Nat_Q))
# 
# # # plot to check disaggregation of Q
# out.df %>%
#   ggplot(aes(x = Date, y = Daily_Nat_Q)) +
#   geom_line() +
#   theme_bw()
#  
# # # plot to compare WSC daily Q vs. disaggregated EFN Q
# out.df %>% left_join(disagg.df, by = "Date") %>%
#   ggplot(aes(x = Date)) +
#   geom_line(aes(y = Q, colour = "WSC")) +
#   geom_line(aes(y = Daily_Nat_Q, colour = "EFN")) +
#   theme_bw()

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
