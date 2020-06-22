#########################################################################################################
##
## This script calculates select diversion timeseries based on a given model run. For example, naturalized model output is required
## to estimate some diversion timeseries.
##
#########################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

# library(tidyverse)
library(lubridate)
library(ggplot2)
library(RavenR)
library(openxlsx)
library(tidyhydat)
library(reshape2)
library(dplyr)


## Determine model start and end dates:
# RVI.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVI-Template.csv")
# 
# start.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "StartDate", "DEFINITION"], format = "%m/%d/%Y")
# 
# end.date <- as.POSIXct(RVI.template[RVI.template$GROUP == "Time" & RVI.template$PARAMETER == "EndDate", "DEFINITION"], format = "%m/%d/%Y")

# read in data files
rules.df <- read.csv(file.path(global.input.dir, raw.custom.timeseries.in.dir, diversion.rules.in.file),
                     stringsAsFactors = FALSE, check.names = FALSE)

# load custom_timeseries workbook
cts.fn <- file.path(global.input.dir, raw.custom.timeseries.in.dir, custom.timeseres.in.file)
cts.wb <- loadWorkbook(cts.fn)

# load Summary sheet and vector of worksheet names
cts.sheets <- getSheetNames(cts.fn)
cts.sum <- read.xlsx(cts.fn[grep("Summary", cts.sheets)]) 

# check to see if watershed diversion sheets have been created. If not, create.
# drop Trout from auto-generated list because it will have unique tab names
ws.to.add <- paste0(include.watersheds, "_Diverted_Flow")
ws.to.add <- ws.to.add[-grep("Trout", ws.to.add)] 
wi <- ws.to.add %in% cts.sheets
# sapply(ws.to.add[!wi], addWorksheet, wb = cts.wb) # QAQC sanity check

# create strings for start and end dates
rules.df <- rules.df %>%
  dplyr::mutate(Start = ymd(paste(Year_start, Month_start, Day_start, sep = "-")),
         End = ymd(paste(Year_end, Month_end, Day_end, sep = "-")),
         Start_2 = ymd(paste(Year_start, Month_start_2, Day_start_2, sep = "-")),
         End_2 = ymd(paste(Year_end, Month_end_2, Day_end_2, sep = "-")))


## LB - Read in the hydrographs file from the given model run. Source depends on whether OSTRICH run has just been completed or not.
if(run.ostrich == TRUE){
  hydrographs <- hyd.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), "processor_0/model", paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
} else {
  hydrographs <- hyd.read(file.path(global.simulation.dir, ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))
}

## LB - Convert hydrographs to Q.df
Q.df <- hydrographs$hyd


for(i in 1:length(include.watersheds)){
  
  # for reference later 1. look up rules.df row corresponding to current
  # watershed, 2. look up column that has subbasin ID, adjacent to Watershed 
  r <- grep(include.watersheds[i], paste(rules.df$Source_Watershed, rules.df$Destination_Watershed))
  c <- grep(include.watersheds[i], rules.df[r, ])
  
  ###### create time series governing current diversion 
  ##### Powers #####
  ##### Powers has two diversion time periods. Handle separately
  if(include.watersheds[i] == "Powers"){
    
    # get the days of year that correspond to the two diversion time
    # periods. Each time period has its own alloted diversion volume and 
    # there is overlap
    yday.start <- lubridate::yday(rules.df$Start[r])
    yday.end <- lubridate::yday(rules.df$End[r])
    yday.start2 <- lubridate::yday(rules.df$Start_2[r])
    yday.end2 <- lubridate::yday(rules.df$End_2[r])
    
    
    # make time series from start dates to end dates
    # override start date to extend to begin Jan 1st 1996, not Oct 1 1996
    ts <- seq.Date(from = rules.df$Start[r], to = rules.df$End[r], by = "day",
                   tz = "UTC")
    ts2 <- seq.Date(from = floor_date(rules.df$Start_2[r], "year"), 
                    to = rules.df$End_2[r], by = "day",
                    tz = "UTC")
    
    # filter time series to only include days of year within diversion
    # times
    ts.filt <- ts[lubridate::yday(ts) >= yday.start & lubridate::yday(ts) <= yday.end]
    
    # start date is Oct 1st, end is June 15th, so filter time series to exclude
    # days after the end and before the start
    ts.filt2 <- lubridate::yday(ts2) >= yday.end2 & lubridate::yday(ts2) <= yday.start2
    ts.filt2 <- ts2[!ts.filt2]
    
    # switch hydrograph from xts object to dataframe for simplicity's sake
    Q.df <- as.data.frame(Q.df)
    Q.df$Date <- ymd(row.names(Q.df)) 
    
    # get annual divertable volumes
    Q.div <- as.numeric(rules.df[r, "Diversion_(m^3)"])
    Q.div2 <- as.numeric(rules.df[r, "Diversion_2_(m^3)"])
    
    c.name <- paste(include.watersheds[i], "_Creek", rules.df[r,"Destination_subbasin_ID"], sep = "")
    
    # filter hydrograph to only include days in either diversion period.
    # add column of diversion volume.
    # compute daily flow / total time period flow --> this will be the 
    # proportion used to estimate the daily flow diversion
    # subbasin ID is adjacent to column name that has watershed name.
    Q.df.filt <- Q.df %>%
      dplyr::filter(Date %in% ts.filt) %>%
      dplyr::mutate(Year = lubridate::year(Date)) 
    Q.df.filt <- Q.df.filt %>%
      group_by(Year) %>%
      select(Date, Year, c.name) %>% 
      dplyr::rename("Q" = c.name) %>%
      dplyr::mutate(Q.prop = Q / sum(Q, na.rm = TRUE)) 
    
    
    Q.df.filt2 <- Q.df %>%
      dplyr::filter(Date %in% ts.filt2) %>%
      dplyr::mutate(Year = lubridate::year(Date))
    Q.df.filt2 <- Q.df.filt2 %>%
      group_by(Year) %>%
      select(Date, Year, c.name) %>% 
      dplyr::rename("Q" = c.name) %>%
      dplyr::mutate(Q.prop = Q / sum(Q, na.rm = TRUE)) 
    
    # compute daily diverted flows 
    # m^3 * daily Q proportion of total Q during diversion time period / s/day
    out.df <- Q.df.filt %>%
      dplyr::mutate(Daily.diverted.Q = Q.prop * Q.div / 86400) %>%
      ungroup() %>% dplyr::rename("Daily_diverted_flow_m3s" = Daily.diverted.Q) %>%
      select(Date, Daily_diverted_flow_m3s)
    out.df2 <- Q.df.filt2 %>%
      dplyr::mutate(Daily.diverted.Q = Q.prop * Q.div2 / 86400) %>%
      ungroup() %>% dplyr::rename("Daily_diverted_flow_2" = Daily.diverted.Q) %>%
      select(Date, Daily_diverted_flow_2)
    
    # merge both daily diverted flows
    out.df <- out.df %>% 
      full_join(out.df2, by = "Date") %>%
      group_by(Date) %>%
      dplyr::summarize(Mean_Daily_Diversion_m3s = sum(Daily_diverted_flow_m3s,
                                                      Daily_diverted_flow_2,
                                                      na.rm = TRUE))
    
    # extend output to full length of Raven hydrograph timeseries
    out.df <- out.df %>% 
      right_join(Q.df %>% select(Date),
                 by = "Date")
    
    # set days with no diverted flow = 0 flow
    out.df$Mean_Daily_Diversion_m3s [is.na(out.df$Mean_Daily_Diversion_m3s)] <- 0

    # generate output worksheet name
    out.name <- paste(include.watersheds[i], "Diverted_Flow", sep = "_")
    
    # add output data to custom_timeseries workbook depending on calibration run
    writeData(cts.wb, sheet = out.name, x = out.df)
    
    ##### generate information to add to new row of summary worksheet
    
    # set 'Data_type' to DIVERSION_IN or DIVERSION_OUT depending on whether
    # the watershed[i] is a source or destination of diverted flow
    if(grepl("Source", colnames(rules.df)[c])){
      data.type <- "DIVERSION_OUT"
    } else if(grepl("Destination", colnames(rules.df)[c])){
      data.type <- "DIVERSION_IN"
    }
    
    # concatenate all info for the summary worksheet
    sum.row <- c(include.watersheds[i], data.type, "Continuous", rules.df[r, c + 1],
                 out.name)
    
    # append row to summary worksheet dataframe and add to cts workbook
    cts.nrow <- nrow(cts.sum)
    cts.sum[cts.nrow + 1, ] <- sum.row
    
    ## Remove duplicates - if the summary row already exists, this removes duplicated entried
    cts.sum <- distinct(cts.sum)
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)
    
    ##### Naramata####
  } else if(include.watersheds[i] == "Naramata"){
    
    # Highline diversion is treated as a constant inflow to Naramata Ck.
    # Mean daily discharges were estimated by DL to match RDOS intake requirements
    
    Q.div <- rules.df[r, "Diversion_(m^3)"]
    Q.div <- as.numeric(unlist(strsplit(Q.div, ", ")))
    names(Q.div) <- c("July", "August", "September", "October")
    
    # make time series from start date to end date
    ts <- seq.Date(from = rules.df$Start[r], to = rules.df$End[r], by = "day",
                   tz = "UTC")
    
    # discharge at constant rates for July, August, Sept, Oct
    Q.df <- data.frame("Date" = ts,
                       "Mean_Daily_Diversion_m3s" = NA)
    
    # set diversion volumes based on month value
    Q.df[month(Q.df$Date) == 7, 2] <- Q.div[["July"]]
    Q.df[month(Q.df$Date) == 8, 2] <- Q.div[["August"]]
    Q.df[month(Q.df$Date) == 9, 2] <- Q.div[["September"]]
    Q.df[month(Q.df$Date) == 10, 2] <- Q.div[["October"]]
    Q.df[is.na(Q.df$Mean_Daily_Diversion_m3s), 2] <- 0
    
    out.df <- Q.df
    
    # generate output worksheet name
    out.name <- paste(include.watersheds[i], "Diverted_Flow", sep = "_")
    
    # add output data to custom_timeseries 
    writeData(cts.wb, sheet = out.name, x = out.df) # manual computing
    
    ##### generate information to add to new row of summary worksheet
    
    # set 'Data_type' to DIVERSION_IN or DIVERSION_OUT depending on whether
    # the watershed[i] is a source or destination of diverted flow
    if(grepl("Source", colnames(rules.df)[c])){
      data.type <- "DIVERSION_OUT"
    } else if(grepl("Destination", colnames(rules.df)[c])){
      data.type <- "DIVERSION_IN"
    }
    
    # concatenate all info for the summary worksheet
    sum.row <- c(include.watersheds[i], data.type, "Continuous", rules.df[r, c + 1],
                 out.name)
    
    # append row to summary worksheet dataframe and add to cts workbook
    cts.nrow <- nrow(cts.sum)
    cts.sum[cts.nrow + 1, ] <- sum.row
    
    ## Remove duplicates - if the summary row already exists, this removes duplicated entried
    cts.sum <- distinct(cts.sum)
    
    writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    # saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)
    
    ##### Mission #####
  } else if(include.watersheds[i] == "Mission"){
    # Refer to 2018-8028 > data > 11_Mission_Creek > Appendix_M_Mission Creek.xlsx > Stirling tab
    # for details on how diversion volumes were computed. No specific details are available, but
    # Stirling Creek discharge is assumed to be equivalent to the volume diverted into Mission.
    # A relationship between Stirling Creek flows and 241 Creek flows is established and applied
    # to estimate Stirling Creek flows for years where there is no record.

    # switch hydrograph from xts object to dataframe for simplicity's sake
    Q.df <- as.data.frame(Q.df$hyd)
    Q.df$Date <- ymd(row.names(Q.df)) 
    
    # read in Stirling Creek data that forms the basis of the Mission
    # Creek diversion amounts.
    stirling_df <- read.csv(file.path(global.input.dir, raw.custom.timeseries.in.dir, stirling.creek.flows.in.file),
                            stringsAsFactors = FALSE, header = TRUE)
    colnames(stirling_df) <- c("Year", "2003", "2004", "2005", "2006")

    # read in 241 creek daily flows
    WSC_df <- hy_daily_flows(station_number = "08NM241",
                              hydat_path = file.path(global.input.dir, raw.hydat.in.dir, hydat.in.file))

    # select subbasin streamflow for the subbasin that has 241 Creek
    Q.df.241 <- Q.df %>%
      dplyr::select(Date, Penticton_Creek2312) 
    
    # select date and streamflow from 241 creek data
    WSC_df <- WSC_df %>%
      dplyr::select(Date, Value) %>%
      dplyr::mutate(Date = ymd(Date)) 
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Time handling
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # get the days of year that correspond to the two diversion time
    # periods. Each time period has its own alloted diversion volume and 
    # there is overlap
    yday.start <- lubridate::yday(rules.df$Start[r])
    yday.end <- lubridate::yday(rules.df$End[r])
    
    # get the range of dates covered by both WSC data from 241 Creek and model output
    ts_range <- c(min(min(WSC_df$Date, Q.df.241$Date)),
                  max(max(WSC_df$Date, Q.df.241$Date)))
    ts <- seq.Date(ts_range[1], ts_range[2], by = "day", tz = "UTC")
    
    # make dataframe to hold WSC 241 Creek 
    Q241_df <- data.frame("Date" = ts)
    
    # add both model output and WSC 241 Creek obs to the dataframe
    # retain WSC data for 241 where it exists, otherwise retain model output
    Q241_df <- Q241_df %>%
      dplyr::left_join(Q.df.241, by = "Date") %>%
      dplyr::left_join(WSC_df, by = "Date") %>%
      dplyr::mutate(Q = case_when(is.na(Penticton_Creek2312) & is.na(Value) ~ 0,
                                  is.na(Penticton_Creek2312) & !is.na(Value) ~ Value,
                                  !is.na(Penticton_Creek2312) & is.na(Value) ~ Penticton_Creek2312,
                                  !is.na(Penticton_Creek2312) & !is.na(Value) ~ Value)) %>%
      dplyr::select(Date, Q)

    # compute the monthly streamflows averaged over the period of interest for
    # Stirling Creek
    stirling_POR <- stirling_df %>%
      reshape2::melt(id.var = "Year",
                     measure.vars = as.character(2003:2006)) %>%
      dplyr::rename(Month = "Year", Year = "variable", Mean_monthly_Q_m3s = "value") %>%
      group_by(Month) %>%
      dplyr::filter(Year != 2003) %>%
      dplyr::summarize(POR_mean_Q = mean(Mean_monthly_Q_m3s, na.rm = TRUE)) %>%
      dplyr::mutate(Month = lubridate::month(Month)) %>%
      dplyr::arrange(Month)

    # compute mean monthly flow for 241 creek
    Q241_mean_monthly <- Q241_df %>%
      dplyr::rename(Q_m3s = "Q" ) %>%
      dplyr::mutate(Date = ymd(Date),
                    month = lubridate::month(Date),
                    year = lubridate::year(Date)) %>%
      group_by(year, month) %>%
      dplyr::summarize(Q_m3s = mean(Q_m3s, na.rm = TRUE))

    # compute the mean monthly flows over the Stirling Creek flow period
    # of record (2004:2006, with April missing from 2005 and 2006)
    Q241_POR <- Q241_mean_monthly %>%
      dplyr::filter(year %in% 2004:2006) %>%
      dplyr::mutate(Q_m3s = ifelse(month == 4 & year %in% 2005:2006, NA, Q_m3s)) %>% # set 241 Q to NA for April in 05/06, as it doesn't exist for Stirling Creek.
      ungroup() %>%
      group_by(month) %>%
      dplyr::summarize(Q_m3s = mean(Q_m3s, na.rm = TRUE)) %>%
      dplyr::mutate(Q_m3s = ifelse(month %in% c(1:3, 11:12), NA, Q_m3s))

    # compute the ratio of 241 Ck Q to 241 POR mean monthly Q
    Q241_monthly_ratio <- Q241_mean_monthly %>%
      left_join(Q241_POR %>% dplyr::rename(Q_241_por = "Q_m3s"),
                by = "month") %>%
      dplyr::mutate(Q_ratio = Q_m3s / Q_241_por)

    
    # format.Date(seq.Date(start.date, end.date, by = "year"), "%Y")
    # estimate Stirling Ck mean monthly flows by computing 241 Q ratio
    # times Stirling Ck mean monthly POR Q
    Q_stirling_filled <- Q241_monthly_ratio %>%
      left_join(stirling_POR %>% dplyr::rename(Qstirling_POR = "POR_mean_Q",
                                               month = "Month"),
                by = "month") %>%
      dplyr::mutate(Qbar_mean_monthly_stirling = Q_ratio * Qstirling_POR) %>%
      dplyr::filter(year %in% format.Date(seq.Date(rules.df$Start[r], rules.df$End[r], by = "year"), "%Y")) 

    # format.Date(rules.df$Start[r], "%Y")
    # compute 241 mean daily to mean monthly ratio
    Q241_daily_to_monthly_ratio <- Q241_df %>%
      dplyr::mutate(Date = ymd(Date),
                    year = lubridate::year(Date),
                    month = lubridate::month(Date)) %>%
      select(Date, year, month, Q) %>%
      dplyr::rename(Q_daily = "Q") %>%
      left_join(Q241_mean_monthly, by = c("month", "year")) %>%
      dplyr::mutate(Q_ratio_dm = Q_daily / Q_m3s) %>%
      dplyr::filter(year %in% format.Date(seq.Date(rules.df$Start[r], rules.df$End[r], by = "year"), "%Y"))

    # compute daily Stirling Creek streamflows
    Q_stirling_daily <- Q241_daily_to_monthly_ratio %>%
      select(Date, year, month, Q_ratio_dm) %>%
      left_join(Q_stirling_filled %>%
                  select(year, month, Qbar_mean_monthly_stirling),
                by = c("year", "month")) %>%
      dplyr::mutate(Q_mean_daily_m3s = Q_ratio_dm * Qbar_mean_monthly_stirling)

    # make output dataframe
    # set days with no diverted flow = 0 flow
    out.df <- Q_stirling_daily %>%
      dplyr::rename(Mean_Daily_Diversion_m3s = "Q_mean_daily_m3s") %>%
      select(Date, Mean_Daily_Diversion_m3s) %>%
      dplyr::mutate(Mean_Daily_Diversion_m3s = ifelse(is.na(Mean_Daily_Diversion_m3s),
                                                      0, Mean_Daily_Diversion_m3s))

  # generate output worksheet name
  out.name <- paste(include.watersheds[i], "Diverted_Flow", sep = "_")

  # add output data to custom_timeseries
  writeData(cts.wb, sheet = out.name, x = out.df)

  ##### generate information to add to new row of summary worksheet

  # set 'Data_type' to DIVERSION_IN or DIVERSION_OUT depending on whether
  # the watershed[i] is a source or destination of diverted flow
  if(grepl("Source", colnames(rules.df)[c])){
    data.type <- "DIVERSION_OUT"
  } else if(grepl("Destination", colnames(rules.df)[c])){
    data.type <- "DIVERSION_IN"
  }

  # concatenate all info for the summary worksheet
  sum.row <- c(include.watersheds[i], data.type, "Continuous", rules.df[r, c + 1],
               out.name)

  # append row to summary worksheet dataframe and add to cts workbook
  cts.nrow <- nrow(cts.sum)
  cts.sum[cts.nrow + 1, ] <- sum.row

  ## Remove duplicates - if the summary row already exists, this removes duplicated entried
  cts.sum <- distinct(cts.sum)

  # add output dataframe to custom_timeseries.csv as a new worksheet
  writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)

  saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)

  #### Trepanier or Shorts ####
  } else if(include.watersheds[i] == "Trepanier" | include.watersheds[i] == "Shorts"){
    
    # get the days of year that correspond to the diversion time period
    yday.start <- lubridate::yday(rules.df$Start[r])
    yday.end <- lubridate::yday(rules.df$End[r])
    
    # make time series from start date to end date
    ts <- seq.Date(from = rules.df$Start[r], to = rules.df$End[r], by = "day",
                   tz = "UTC")
    
    # filter time series to only include days of year
    ts.filt <- ts[lubridate::yday(ts) >= yday.start & lubridate::yday(ts) <= yday.end]
    
    # switch hydrograph from xts object to dataframe for simplicity's sake
    Q.df <- as.data.frame(Q.df$hyd)
    Q.df$Date <- ymd(row.names(Q.df)) 
    
    # filter hydrograph to only include days in either diversion period.
    # add column of diversion volume
    Q.df.filt <- Q.df %>%
      dplyr::filter(Date %in% ts.filt) %>%
      dplyr::mutate(Year = lubridate::year(Date))
    
    c.name <- paste(include.watersheds[i], "_Creek", rules.df[r,"Source_subbasin_ID"], sep = "")
    
    # compute daily flow / total time period flow --> this will be the 
    # proportion used to estimate the daily flow diversion
    # subbasin ID is adjacent to column name that has watershed name
    Q.df.filt <- Q.df.filt %>% 
      dplyr::rename("Q" = c.name) %>%
      group_by(Year) %>%
      dplyr::mutate(Q.prop = Q / sum(Q, na.rm = TRUE)) 
    
    # get annual divertable volume
    # need to set as.numeric because one of the Diversion col. entries is a string
    Q.div <- as.numeric(rules.df[r, "Diversion_(m^3)"])
    
    
    # compute daily diverted flows 
    # m^3 * daily Q proportion of total Q during diversion time period / s/day
    out.df <- Q.df.filt %>%
      dplyr::mutate(Daily.diverted.Q = Q.prop * Q.div / 86400) %>%
      ungroup() %>% dplyr::rename("Mean_Daily_Diversion_m3s" = Daily.diverted.Q) %>%
      select(Date, Mean_Daily_Diversion_m3s)
    
    # extend output to full length of Raven hydrograph timeseries
    out.df <- out.df %>% 
      right_join(Q.df %>% select(Date),
                 by = "Date")
    
    # set days with no diverted flow = 0 flow
    out.df$Mean_Daily_Diversion_m3s[is.na(out.df$Mean_Daily_Diversion_m3s)] <- 0
    
    # generate output worksheet name
    out.name <- paste(include.watersheds[i], "Diverted_Flow", sep = "_")
    
    # add output data to custom_timeseries workbook depending on calibration run
    writeData(cts.wb, sheet = out.name, x = out.df)
    
    ##### generate information to add to new row of summary worksheet
    
    # set 'Data_type' to DIVERSION_IN or DIVERSION_OUT depending on whether
    # the watershed[i] is a source or destination of diverted flow
    if(grepl("Source", colnames(rules.df)[c])){
      data.type <- "DIVERSION_OUT"
    } else if(grepl("Destination", colnames(rules.df)[c])){
      data.type <- "DIVERSION_IN"
    }
    
    # concatenate all info for the summary worksheet
    sum.row <- c(include.watersheds[i], data.type, "Continuous", rules.df[r, c + 1],
                 out.name)
    
     # append row to summary worksheet dataframe and add to cts workbook
    cts.nrow <- nrow(cts.sum)
    cts.sum[cts.nrow + 1, ] <- sum.row
    
    ## Remove duplicates - if the summary row already exists, this removes duplicated entried
    cts.sum <- distinct(cts.sum)
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)
    
    saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)
  } else if(include.watersheds[i] == "Trout"){
    
    # get the days of year that correspond to the diversion time period
    yday.start <- lubridate::yday(rules.df$Start[r])
    yday.end <- lubridate::yday(rules.df$End[r])
    
    # make time series from start date to end date
    ts <- seq.Date(from = rules.df$Start[r], to = rules.df$End[r], by = "day",
                   tz = "UTC")
    
    # filter time series to only include days of year
    ts.filt <- ts[lubridate::yday(ts) >= yday.start & lubridate::yday(ts) <= yday.end]
    
    # switch hydrograph from xts object to dataframe for simplicity's sake
    Q.df <- as.data.frame(Q.df$hyd) 
    Q.df$Date <- ymd(row.names(Q.df))
    
    # filter hydrograph to only include days in either diversion period.
    # add column of diversion volume
    Q.df.filt <- Q.df %>%
      dplyr::filter(Date %in% ts.filt)
    
    # diverted flow will be equal to the flow at Trout_Creek2840 * 10
    c.name <- paste(include.watersheds[i], "_Creek", 2840, sep = "")
    
    # compute daily flow / total time period flow --> this will be the 
    # proportion used to estimate the daily flow diversion
    # subbasin ID is adjacent to column name that has watershed name
    out.df <- Q.df.filt %>% 
      dplyr::rename("Q" = c.name) %>%
      dplyr::mutate(Q_surrogate = Q*10) %>%
      dplyr::rename("Mean_Daily_Diversion_m3s" = Q_surrogate) %>%
      select(Date, Mean_Daily_Diversion_m3s)
    
    
    # extend output to full length of Raven hydrograph timeseries
    out.df <- out.df %>% 
      right_join(Q.df %>% select(Date),
                 by = "Date")
    
    # set days with no diverted flow = 0 flow
    out.df$Mean_Daily_Diversion_m3s[is.na(out.df$Mean_Daily_Diversion_m3s)] <- 0
    
    # generate output worksheet name
    # irrigation provides the 'fake' water demand flow to drain the reservoir (forgot the name)
    # diversion adds the 'fake' water demand flow back to the downstream subbasin
    # black magic in action
    out.name.irrigation <- paste(include.watersheds[i], "Irrigation_Surrogate", sep = "_")
    out.name.diversion <- paste(include.watersheds[i], "Diversion_Surrogate", sep = "_")
    
    # add output data to custom_timeseries workbook
    # add worksheet name to workbook, and add surrogate irrigation and diversion
    # data to both custom_timeseries
    addWorksheet(cts.wb, sheetName = out.name.irrigation)
    writeData(cts.wb, sheet = out.name.irrigation, x = out.df)
    
    addWorksheet(cts.wb, sheetName = out.name.diversion)
    writeData(cts.wb, sheet = out.name.diversion, x = out.df)
    
    ##### generate information to add two new rows of summary worksheet
    data.type.irr <- "IRRIGATION_DEMAND"
    data.type.div <- "DIVERSION_IN"
    
    subbasin.irr <- rules.df[r, grep("Source_subbasin", colnames(rules.df))]
    subbasin.div <- rules.df[r, grep("Destination_subbasin", colnames(rules.df))]
    
    # concatenate all info for the summary worksheet
    sum.row.irr <- c(include.watersheds[i], data.type.irr, "Continuous",
                     subbasin.irr, out.name.irrigation)
    sum.row.div <- c(include.watersheds[i], data.type.div, "Continuous",
                     subbasin.div, out.name.diversion)
    
    # append row to summary worksheet dataframe and add to cts workbook. Iterate 
    # twice to get both additions in
    cts.nrow <- nrow(cts.sum)
    cts.sum[cts.nrow + 1, ] <- sum.row.irr
    cts.nrow <- nrow(cts.sum)
    cts.sum[cts.nrow + 1, ] <- sum.row.div
    
    ## Remove duplicates - if the summary row already exists, this removes duplicated entried
    cts.sum <- distinct(cts.sum)
    
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)
    
    saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)
    
    
  }
  
}

