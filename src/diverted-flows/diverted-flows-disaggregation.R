# Diversions

library(tidyverse)
library(lubridate)
library(ggplot2)
library(RavenR)
library(openxlsx)

rm(list = ls())

setwd("/var/obwb-hydro-modelling/simulations/Diversion-working/")

# include.watersheds
watershed <- c("Trepanier", "Shorts")

# read in data files
# can't write to VM yet except for directories made by me
rules.df <- read.csv("/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/AJS_temp/diversion_rules_summary.csv",
                     stringsAsFactors = FALSE, check.names = FALSE)

# load custom_timeseries workbook
cts.fn <- "/var/obwb-hydro-modelling/input-data/raw/naturalized-flows/AJS_temp/custom_timeseries.xlsx"
cts.wb <- loadWorkbook(cts.fn)

# load Summary sheet and vector of worksheet names
cts.sheets <- getSheetNames(cts.fn)
cts.sum <- read.xlsx(cts.fn[grep("Summary", cts.sheets)]) 

# check to see if watershed diversion sheets have been created. If not, create.
ws.to.add <- paste0(watershed, "_Diverted_Flow")
sapply(ws.to.add, addWorksheet, wb = cts.wb)

# add missing worksheets to custom timeseries workbook
# ifelse(length(ws.to.add) > 0,
#        do.call("addWorksheet", list("wb" = cts.wb, 
#                                     "sheetName" = ws.to.add)),
#        print("No worksheets to add."))

# create strings for start and end dates
rules.df <- rules.df %>%
  mutate(Start = ymd(paste(Year_start, Month_start, Day_start, sep = "-")),
         End = ymd(paste(Year_end, Month_end, Day_end, sep = "-")),
         Start_2 = ymd(paste(Year_start, Month_start_2, Day_start_2, sep = "-")),
         End_2 = ymd(paste(Year_end, Month_end_2, Day_end_2, sep = "-")))

# temporary subbasin assignment to get script up and running
rules.df$Source_subbasin_ID[grep("Trepanier", rules.df$Source_Watershed)] <- 7205
rules.df$Source_subbasin_ID[grep("Shorts", rules.df$Source_Watershed)] <- 7206

# list of all files in naturalized streamflow directory
files <- list.files(recursive = TRUE, full.names = TRUE)

for(i in 1:length(watershed)){
  
  # for reference later 1. look up rules.df row corresponding to current
  # watershed, 2. look up column that has subbasin ID, adjacent to Watershed 
  r <- grep(watershed[i], paste(rules.df$Source_Watershed, rules.df$Destination_Watershed))
  c <- grep(watershed[i], rules.df[r, ])
  
  # create string to lookup, via grep, the Raven output hydrograph for the  
  # current diversion's watershed
  ####  CURRENTLY FIXED ON TREPANIER.   need to replace '1' with 'i' once more naturalized flows are available  
  current.hyd <- paste0("^.*", watershed[1], ".*", "Hydrographs.csv")
  
  # look up and set filename
  hyd.fn <- files[grep(current.hyd, files)]
  
  ####     currently Trepanier working example     #####
  # read in Raven naturalized streamflow hydrograph 
  hyd.file <- hyd.read(hyd.fn)
  Q.df <- hyd.file$hyd
  
  ###### create time series governing current diversion 
  if(watershed[i] == "Powers"){
    # Powers has two diversion time periods. Handle separately
    
  } else {
    
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
    
    # right join of filtered time series to hydrograph df filters the 
    # hydrograph df to diversion days.
    Q.df.filt <- Q.df %>%
      right_join(data.frame("Date" = ts.filt), by = "Date") %>%
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
    out.name <- paste(watershed[i], "Diverted_Flow", sep = "_")
    
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
    sum.row <- c(watershed[i], data.type, "Continuous", rules.df[r, c + 1],
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


