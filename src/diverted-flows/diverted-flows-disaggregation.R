# Diversions

library(tidyverse)
library(lubridate)
library(ggplot2)
library(RavenR)
library(openxlsx)
library(tidyhydat)

rm(list = ls())

######--------- CHANGE DIRECTORY AWAY FROM TEMPORARY DIR.
setwd("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/AJS_temp/")

# include.watersheds will be pulled in from elsewhere. This 'watershed' variable is just a
# placeholder to get the script functional and to test the rules for all of the diversions
include.watersheds <- c("Trepanier", "Shorts", "Mission", "Powers", "Naramata")

# read in data files
# can't write to VM yet except for directories made by me
rules.df <- read.csv("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/AJS_temp/diversion_rules_summary.csv",
                     stringsAsFactors = FALSE, check.names = FALSE)

######--------- UPDATE DIRECTORY FOR custom_timeseries.xlsx AWAY FROM TEMPORARY
# load custom_timeseries workbook
cts.fn <- "/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/AJS_temp/custom_timeseries.xlsx"
cts.wb <- loadWorkbook(cts.fn)

# load Summary sheet and vector of worksheet names
cts.sheets <- getSheetNames(cts.fn)
cts.sum <- read.xlsx(cts.fn[grep("Summary", cts.sheets)]) 

# check to see if watershed diversion sheets have been created. If not, create.
ws.to.add <- paste0(include.watersheds, "_Diverted_Flow")
wi <- ws.to.add %in% cts.sheets
sapply(ws.to.add[!wi], addWorksheet, wb = cts.wb)

# create strings for start and end dates
rules.df <- rules.df %>%
  mutate(Start = ymd(paste(Year_start, Month_start, Day_start, sep = "-")),
         End = ymd(paste(Year_end, Month_end, Day_end, sep = "-")),
         Start_2 = ymd(paste(Year_start, Month_start_2, Day_start_2, sep = "-")),
         End_2 = ymd(paste(Year_end, Month_end_2, Day_end_2, sep = "-")))

# temporary subbasin assignment to get script up and running
# rules.df$Source_subbasin_ID[grep("Trepanier", rules.df$Source_Watershed)] <- 7205
# rules.df$Source_subbasin_ID[grep("Shorts", rules.df$Source_Watershed)] <- 7206
# rules.df$Destination_subbasin_ID[grep("Mission", rules.df$Destination_Watershed)] <- 7206
# rules.df$Destination_subbasin_ID[grep("Powers", rules.df$Destination_Watershed)] <- 7206
# rules.df$Destination_subbasin_ID[grep("Naramata", rules.df$Destination_Watershed)] <- 7206

# list of all files in naturalized streamflow directory
files <- list.files(recursive = TRUE, full.names = TRUE)

for(i in 1:length(include.watersheds)){
  
  # for reference later 1. look up rules.df row corresponding to current
  # watershed, 2. look up column that has subbasin ID, adjacent to Watershed 
  r <- grep(include.watersheds[i], paste(rules.df$Source_Watershed, rules.df$Destination_Watershed))
  c <- grep(include.watersheds[i], rules.df[r, ])
  
  # create string to lookup, via grep, the Raven output hydrograph for the  
  # current diversion's watershed
  ####  CURRENTLY FIXED ON TREPANIER.   need to replace '1' with 'i' once more naturalized flows are available  
  current.hyd <- paste0("^.*", include.watersheds[i], ".*", "Hydrographs.csv")
  
  # look up and set filename
  hyd.fn <- files[grep(current.hyd, files)]
  
  ####     currently Trepanier working example     #####
  # read in Raven naturalized streamflow hydrograph 
  hyd.file <- hyd.read(hyd.fn)
  Q.df <- hyd.file$hyd
  
  ###### create time series governing current diversion 
  # Powers has two diversion time periods. Handle separately
  if(include.watersheds[i] == "Powers"){
    
    # get the days of year that correspond to the two diversion time
    # periods. Each time period has its own alloted diversion volume and 
    # there is overlap
    yday.start <- yday(rules.df$Start[r])
    yday.end <- yday(rules.df$End[r])
    yday.start2 <- yday(rules.df$Start_2[r])
    yday.end2 <- yday(rules.df$End_2[r])
    
    
    # make time series from start dates to end dates
    # override start date to extend to begin Jan 1st 1996, not Oct 1 1996
    ts <- seq.Date(from = rules.df$Start[r], to = rules.df$End[r], by = "day",
                   tz = "UTC")
    ts2 <- seq.Date(from = floor_date(rules.df$Start_2[r], "year"), 
                    to = rules.df$End_2[r], by = "day",
                    tz = "UTC")
    
    # filter time series to only include days of year within diversion
    # times
    ts.filt <- ts[yday(ts) >= yday.start & yday(ts) <= yday.end]
    
    # start date is Oct 1st, end is June 15th, so filter time series to exclude
    # days after the end and before the start
    ts.filt2 <- yday(ts2) >= yday.end2 & yday(ts2) <= yday.start2
    ts.filt2 <- ts2[!ts.filt2]
    
    # switch hydrograph from xts object to dataframe for simplicity's sake
    Q.df <- as.data.frame(Q.df)
    Q.df$Date <- ymd(row.names(Q.df)) 
    
    # get annual divertable volumes
    Q.div <- rules.df[r, "Diversion_(m^3)"]
    Q.div2 <- rules.df[r, "Diversion_2_(m^3)"]
    
    # filter hydrograph to only include days in either diversion period.
    # add column of diversion volume.
    # compute daily flow / total time period flow --> this will be the 
    # proportion used to estimate the daily flow diversion
    # subbasin ID is adjacent to column name that has watershed name.
    Q.df.filt <- Q.df %>%
      filter(Date %in% ts.filt) %>%
      mutate(Year = year(Date)) 
    Q.df.filt <- Q.df.filt %>%
      group_by(Year) %>%
      select(Date, Year, colnames(Q.df.filt)[c+1]) %>% 
      rename("Q" = colnames(Q.df.filt)[c+1]) %>%
      mutate(Q.prop = Q / sum(Q, na.rm = TRUE)) 
    
    
    Q.df.filt2 <- Q.df %>%
      filter(Date %in% ts.filt2) %>%
      mutate(Year = year(Date))
    Q.df.filt2 <- Q.df.filt2 %>%
      group_by(Year) %>%
      select(Date, Year, colnames(Q.df.filt2)[c+1]) %>% 
      rename("Q" = colnames(Q.df.filt2)[c+1]) %>%
      mutate(Q.prop = Q / sum(Q, na.rm = TRUE)) 
    
    # compute daily diverted flows 
    # m^3 * daily Q proportion of total Q during diversion time period / s/day
    out.df <- Q.df.filt %>%
      mutate(Daily.diverted.Q = Q.prop * Q.div / 86400) %>%
      # summarize(Div.total = sum(Daily.diverted.Q))  # QA/QC: Div.total should == Q.div
      ungroup() %>% rename("Daily_diverted_flow_m3s" = Daily.diverted.Q) %>%
      select(Date, Daily_diverted_flow_m3s)
    out.df2 <- Q.df.filt2 %>%
      mutate(Daily.diverted.Q = Q.prop * Q.div2 / 86400) %>%
      # summarize(Div.total = sum(Daily.diverted.Q))  # QA/QC: Div.total should == Q.div
      ungroup() %>% rename("Daily_diverted_flow_2" = Daily.diverted.Q) %>%
      select(Date, Daily_diverted_flow_2)
    
    # merge both daily diverted flows
    out.df <- out.df %>% 
      full_join(out.df2, by = "Date") %>%
      group_by(Date) %>%
      summarize(Daily_diverted_flow_m3s = sum(Daily_diverted_flow_m3s,
                                              Daily_diverted_flow_2,
                                              na.rm = TRUE))
    
    # extend output to full length of Raven hydrograph timeseries
    out.df <- out.df %>% 
      right_join(Q.df %>% select(Date),
                 by = "Date")
    
    # set days with no diverted flow = 0 flow
    out.df$Daily_diverted_flow_m3s[is.na(out.df$Daily_diverted_flow_m3s)] <- 0
    
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
    
    writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)
    
  } else if(include.watersheds[i] == "Naramata"){
    
    # Highline diversion is treated as a constant inflow to Naramata Ck.
    # Mean daily discharges were estimated by DL to match RDOS intake requirements
    
    Q.div <- rules.df[r, "Diversion_(m^3)"]
    Q.div <- as.numeric(unlist(strsplit(Q.div, ", ")))
    names(Q.div) <- c("July", "August", "September", "October")
    
    # make full timeseries
    ts <- seq.Date(from = ymd("1994-01-01"),
                   to = ymd("2017-12-31"), by = "day")
    
    # discharge at constant rates for July, August, Sept, Oct
    Q.df <- data.frame("Date" = ts,
                       "Mean_Daily_Discharge_m3s" = NA)
    
    # set diversion volumes based on month value
    Q.df[month(Q.df$Date) == 7, 2] <- Q.div[["July"]]
    Q.df[month(Q.df$Date) == 8, 2] <- Q.div[["August"]]
    Q.df[month(Q.df$Date) == 9, 2] <- Q.div[["September"]]
    Q.df[month(Q.df$Date) == 10, 2] <- Q.div[["October"]]
    Q.df[is.na(Q.df$Mean_Daily_Discharge_m3s), 2] <- 0
    
    out.df <- Q.df
    
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
    
    writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)
    
  } else if(include.watersheds[i] == "Mission"){
    # Refer to 2018-8028 > data > 11_Mission_Creek > Appendix_M_Mission Creek.xlsx > Stirling tab
    # for details on how diversion volumes were computed. No specific details are available, but 
    # Stirling Creek discharge is assumed to be equivalent to the volume diverted into Mission.
    # A relationship between Stirling Creek flows and 241 Creek flows is established and applied
    # to estimate Stirling Creek flows for years where there is no record. 
    
    # read in Stirling Creek data that forms the basis of the Mission
    # Creek diversion amounts. 
    stirling_df <- read.csv("./stirling_ck_mean_monthly_flows.csv",
                            stringsAsFactors = FALSE, header = TRUE)
    
    # read in 241 creek daily flows
    Q241_df <- hy_daily_flows(station_number = "08NM241",
                              hydat_path = "/var/obwb-hydro-modelling/input-data/raw/wsc-hydat/Hydat.sqlite3")
    
    
    
    
    
  } else if(include.watersheds[i] == "Trepanier" | include.watersheds[i] == "Shorts"){
    
    # get the days of year that correspond to the diversion time period
    yday.start <- yday(rules.df$Start[r])
    yday.end <- yday(rules.df$End[r])
    
    # make time series from start date to end date
    ts <- seq.Date(from = rules.df$Start[r], to = rules.df$End[r], by = "day",
                   tz = "UTC")
    
    # filter time series to only include days of year
    ts.filt <- ts[yday(ts) >= yday.start & yday(ts) <= yday.end]
    
    # switch hydrograph from xts object to dataframe for simplicity's sake
    Q.df <- as.data.frame(Q.df)
    Q.df$Date <- ymd(row.names(Q.df)) 
    
    # filter hydrograph to only include days in either diversion period.
    # add column of diversion volume
    Q.df.filt <- Q.df %>%
      filter(Date %in% ts.filt) %>%
      mutate(Year = year(Date))
    
    # compute daily flow / total time period flow --> this will be the 
    # proportion used to estimate the daily flow diversion
    # subbasin ID is adjacent to column name that has watershed name
    Q.df.filt <- Q.df.filt %>% 
      group_by(Year) %>%
      select(Date, Year, colnames(Q.df.filt)[c+1]) %>% 
      rename("Q" = colnames(Q.df.filt)[c+1]) %>%
      mutate(Q.prop = Q / sum(Q, na.rm = TRUE)) 
      
    # get annual divertable volume
    Q.div <- rules.df[r, "Diversion_(m^3)"]
    
    # compute daily diverted flows 
    # m^3 * daily Q proportion of total Q during diversion time period / s/day
    out.df <- Q.df.filt %>%
      mutate(Daily.diverted.Q = Q.prop * Q.div / 86400) %>%
      # summarize(Div.total = sum(Daily.diverted.Q))  # QA/QC: Div.total should == Q.div
      ungroup() %>% rename("Daily_diverted_flow_m3s" = Daily.diverted.Q) %>%
      select(Date, Daily_diverted_flow_m3s)
    
    # extend output to full length of Raven hydrograph timeseries
    out.df <- out.df %>% 
      right_join(Q.df %>% select(Date),
                 by = "Date")
    
    # set days with no diverted flow = 0 flow
    out.df$Daily_diverted_flow_m3s[is.na(out.df$Daily_diverted_flow_m3s)] <- 0
    
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
    
    writeData(wb = cts.wb, sheet = "Summary", x = cts.sum)
    
    # add output dataframe to custom_timeseries.csv as a new worksheet
    saveWorkbook(wb = cts.wb, file = cts.fn, overwrite = TRUE)
  }
}

# load custom_timeseries workbook
cts.fn <- "/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/AJS_temp/custom_timeseries_AJS.xlsx"
cts.wb <- loadWorkbook(cts.fn)

# load Summary sheet and vector of worksheet names
cts.sheets <- getSheetNames(cts.fn)
cts.sum <- read.xlsx(cts.fn[grep("Summary", cts.sheets)]) 

test1 <- read.xlsx(cts.wb, sheet = "Trepanier_Diverted_Flow")
test2 <- read.xlsx(cts.wb, sheet = "Shorts_Diverted_Flow")

plot(test1$Date, test1$Daily_diverted_flow_m3s, type = "l")
lines(test2$Date, test2$Daily_diverted_flow_m3s, col = "blue")


