#####################################################################
#####
#####     Process OWDM surface water demands.
#####     Produces daily water demand per subbasin ID
#####
#####################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

library(dplyr)
library(lubridate)
library(reshape2)
library(data.table)
library(readxl)

########################################
##
## Read in owdm node & subbasin look-up, and owdm raw data
##
########################################

## get sheet names of worksheet with water use areas & source nodes sent to Ron
snames <- excel_sheets(file.path(global.input.dir, raw.owdm.in.dir, owdm.node.subbasin.file))

## Read in OWDM raw data and node/subbasin lookup
other_df <- fread(file.path(global.input.dir, raw.owdm.in.dir, owdm.other.in.file)) %>% as.data.frame()
purveyors <- fread(file.path(global.input.dir, raw.owdm.in.dir, owdm.water.purveyors.in.file))
subbasin_df <- read_xlsx(file.path(global.input.dir, raw.owdm.in.dir, owdm.node.subbasin.file), sheet = grep("^Purveyor.*$|^Pruveyor.*$", snames))

# get column names from subbasin demand request workbook  
sbbsn_cnames <- colnames(subbasin_df)

# get the column names from the subbasin request excel workbook that will be needed to add the
# water demand percentages to the purveyor (water demand) dataframe
prvyr_col <- grep("^Purveyor.*$|^Pruveyor.*$", sbbsn_cnames)
sys_col <- grep("^System ID", sbbsn_cnames)
src1_cols <- c(grep("Source 1 Node ", sbbsn_cnames)+1, grep("Source 1 Node ", sbbsn_cnames), # subbasin ID
               grep("Source 1 Description", sbbsn_cnames), grep("Source 1 Description", sbbsn_cnames)+1) # percent of demand supplied
src2_cols <- c(grep("Source 2 Node ", sbbsn_cnames)+1, grep("Source 2 Node ", sbbsn_cnames),  # subbasin ID
               grep("Source 2 Description", sbbsn_cnames), grep("Source 2 Description", sbbsn_cnames)+1) # percent of demand supplied

# rename columns of interest for easy referencing
join_df <- subbasin_df[, c(prvyr_col, sys_col, src1_cols, src2_cols)]
colnames(join_df) <- c("wateruseid", "system_id", "Source_1_subbasin", "Source 1 Node", "Source 1 Description", "%1",
                       "Source_2_subbasin", "Source 2 Node", "Source 2 Description", "%2")

# make wateruseid column for joining to AWDM demand output
join_df <- join_df %>%
  mutate(wateruseid = paste0(wateruseid, "_", system_id)) %>%
  select(-system_id)

# join water demand percentage supplied by subbasin to water demand by purveyor
purveyors <- purveyors %>%
  left_join(join_df, by = "wateruseid")

head(purveyors)
head(other_df)

# see what order the wateruseids are in
unique(other_df$wateruseid)
unique(other_df$subbasin)
colnames(other_df)

# check to see if Okanagan Lake is a source in the water purveyor demand
OKLake_rows <- grep("Okanagan Lake", purveyors$Source_2_subbasin)
ifelse(length(OKLake_rows) > 0, 
       purveyors <- purveyors[-OKLake_rows, ],
       print("No Okanagan Lake source present."))

# summarize water demand by subbasin ID, by day
# and then sum all of the demand types to a single demand volume
sum_df <- other_df %>% 
  group_by(subbasin, year, day) %>%
  summarize(indoor = sum(indoor, na.rm = TRUE),
            outdoor_domestic = sum(outdoor_domestic, na.rm = TRUE),
            outdoor_animal = sum(outdoor_animal, na.rm = TRUE),
            outdoor_other_irrigation = sum(outdoor_other_irrigation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(extraction.total = indoor + outdoor_domestic + outdoor_animal + outdoor_other_irrigation) %>%
  select(subbasin, year, day, extraction.total) %>%
  rename(Subbasin_ID = "subbasin")

# sum the water demand
purveyors$total_demand <- purveyors$indoor + purveyors$outdoor_domestic + purveyors$outdoor_animal + purveyors$outdoor_other_irrigation

colnames(purveyors)

# split the source 1 and source 2 columns
p1 <- purveyors[, c(1:3, 8, 11, 16)]
p2 <- purveyors[, c(1:3, 12, 15, 16)]

p1 %>% dplyr::filter(is.na(Subbasin_ID)) %>% View()

# compute the source 1 and source 2 volumes
# and drop the source 2 rows that are NA
p1$extraction.total <- p1$total_demand * p1$`%1`/100
p2$extraction.total <- p2$total_demand * p2$`%2`/100
#p1 <- p1[!is.na(p1$extraction.total), ]
#p2 <- p2[!is.na(p2$extraction.total), ]

head(p1)
head(p2)

# change column names to be consistent
colnames(p1) <- c("wateruseid", "year", "day", "Subbasin_ID", "%", "total_demand", "extraction.total")
colnames(p2) <- c("wateruseid", "year", "day", "Subbasin_ID", "%", "total_demand", "extraction.total")

# drop unwanted columns (year, day, %, total_demand)
# row bind the two sources to make one dataframe
p1 <- p1[, -c(5:6)]
p2 <- p2[, -c(5:6)]
p_df <- rbind(p1, p2) %>%
  dplyr::filter(!is.na(extraction.total)) %>%
  dplyr::filter(!is.na(Subbasin_ID))

head(p_df)
unique(p_df$Subbasin_ID)
summary(p_df)

# p_df %>% filter(Subbasin_ID == "Mill Creek1925" & year == 1994 & day == 0)

# summarize water use area demands
p_df <- p_df %>%
  group_by(Subbasin_ID, year, day) %>% 
  summarize(extraction.total = sum(extraction.total, na.rm = TRUE)) %>%
  select(Subbasin_ID, year, day, extraction.total) %>%
  ungroup()

# ensure colnames match before lumping together 
colnames(sum_df)
colnames(p_df)

##-----------------------------------------------------------------------------
##
##  Custom demand percentage lookup
##
##-----------------------------------------------------------------------------

##  Shannon Lake Golf Club
##  Subbasin ID 1702 water use purveyor and 'Other' water use proportion
purveyor1702 <- p_df[grep("1702", p_df$Subbasin_ID), ]
other1702 <- sum_df[grep("1702", sum_df$Subbasin_ID), ]
head(purveyor1702)
head(other1702)
sum(other1702$extraction.total) + sum(purveyor1702$extraction.total)
# compute the proportion of total water use that is attributed to 'other'
sum(purveyor1702$extraction.total) / sum(other1702$extraction.total, purveyor1702$extraction.total) * 100

## Coldstream Creek 1418
purveyor1418 <- p_df[grep("1418", p_df$Subbasin_ID), ]
other1418 <- sum_df[grep("1418", sum_df$Subbasin_ID), ]
sum(purveyor1418$extraction.total) / (sum(purveyor1418$extraction.total) + sum(other1418$extraction.total)) * 100

## Trout Creek 2813
purveyor2813 <- p_df[grep("2813", p_df$Subbasin_ID), ]
other2813 <- sum_df[grep("2813", sum_df$Subbasin_ID), ]
sum(purveyor2813$extraction.total) / (sum(purveyor2813$extraction.total) + sum(other2813$extraction.total)) * 100

## Trout Creek 2805
purveyor2805 <- p_df[grep("2805", p_df$Subbasin_ID), ]
other2805 <- sum_df[grep("2805", sum_df$Subbasin_ID), ]
sum(purveyor2805$extraction.total) / (sum(purveyor2805$extraction.total) + sum(other2805$extraction.total)) * 100

## Penticton Creek 2315
purveyor2315 <- p_df[grep("2315", p_df$Subbasin_ID), ]
other2315 <- sum_df[grep("2315", sum_df$Subbasin_ID), ]
sum(purveyor2315$extraction.total) / (sum(purveyor2315$extraction.total) + sum(other2315$extraction.total)) * 100


#------------------------------------------------------------------------------
#  END custom demand percentage lookup
#------------------------------------------------------------------------------

# join the water purveyors and 'Other_node" water demands,
# sum all of the extractions per subbasin ID per day so that
# there are no duplications
out_df <- rbind(sum_df, p_df) %>%
  group_by(Subbasin_ID, day, year) %>%
  summarize(extraction.total = sum(extraction.total, na.rm = TRUE)) 
summary(out_df)

# check to see if there are any duplicates
test <- rbind(sum_df, p_df)
length(unique(paste(out_df$Subbasin_ID, out_df$day, out_df$year)))
length(unique(paste(test$Subbasin_ID, test$day, test$year)))

# write output of water demand per subbasin ID per day
fwrite(out_df, file.path(global.input.dir, processed.owdm.dir, paste("OWDM_water_demands_timeseries", Sys.Date(), "csv", sep = ".")))

colnames(out_df)
length(grep(0, out_df$day))


##### QAQC  ####
####  of new auto-partitioning version of the script's output vs. 
####  the previous output
# setwd("C:/Users/szeitza/Documents/Projects/20188215/Water_demand/")
# out_df <- fread("./OWDM_water_demands_timeseries.csv")
# old_df <- fread("./graveyard/OWDM_water_demands_timeseries_old.csv")
# 
# qaqc <- data.frame("sb" = unique(old_df$Subbasin_ID))
# qaqc <- anti_join(data.frame("sb" = unique(out_df$Subbasin_ID)),
#                   qaqc, by = "sb")
# 
# summary(out_df)
# summary(old_df)
# 
# diff_df <- anti_join(out_df, old_df, by = "Subbasin_ID")
# 
# diff_df <- left_join(out_df, old_df, by = c("Subbasin_ID", "year", "day"),
#                      suffix = c(".new", ".old")) %>%
#   mutate(extraction.diff = extraction.total.new - extraction.total.old) %>%
#   filter(extraction.diff != 0) %>%
#   mutate(pcnt.diff = (extraction.total.new - extraction.total.old)/extraction.total.old * 100)
# 
# summary(diff_df)
