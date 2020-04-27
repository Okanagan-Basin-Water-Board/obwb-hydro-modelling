## OHME Configuration File


### ------------------------------
### DEFINE DIRECTORY PATHS
### ------------------------------

## Source Directory
src.dir <- "/var/obwb-hydro-modelling/src"

## Input Data Directory
input.dir <- "/var/obwb-hydro-modelling/input-data"


#####################################
#####################################
##
## DEFINE RAW INPUT DATA FILE NAMES
##
#####################################
#####################################

##-----------------------------------
## CLIMATE DATA
##-----------------------------------

raw.climate.in.dir <- "raw/climate"

precip.raw.in.file <- file.path(input.dir, raw.climate.dir, "pr.downscaled.nc")

tasmax.raw.in.file <- file.path(input.dir, raw.climate.dir, "tasmax.downscaled.nc")

tasmin.raw.in.file <- file.path(input.dir, raw.climate.dir, "tasmin.downscaled.nc")

##-----------------------------------
## CUSTOM TIMESERIES DATA
##-----------------------------------

raw.custom.timeseries.in.dir <- "raw/custom-timeseries"

custom.timeseres.in.file <- file.path(input.dir, raw.custom.timeseries.in.dir, "custom_timeseries.xlsx")

diversion.rules.in.file <- file.path(input.dir, raw.custom.timeseries.in.dir, "diversion_rules_summary.csv")

stirling.creek.flows.in.file <- file.path(input.dir raw.custom.timeseries.in.dir, "stirling_ck_mean_monthly_flows.csv")

##-----------------------------------
## NATURALIZED FLOWS DATA
##-----------------------------------

raw.nat.flows.in.dir <- "raw/naturalized-flows"

coldstream.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Coldstream Creek_OK Tennant Streamflow Dataset_Associated_FINAL_11032019_Adjust.xlsx")

equesis.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Equesis Creek_OK Tennant Streamflow Datasets_Associated_FINAL_06022019__Adjust.xlsx")

inkaneep.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Inkaneep Creek_OK Tennant Streamflow Datasets_Associated_FINAL_22022019.xlsx")

mcdougall.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "McDougall Creek_OK Tennant Streamflow Datasets_Associated_FINAL_13032019_Adjust.xlsx")

mclean.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "McLean Creek_OK Tennant Streamflow Datasets_Associated_FINAL_15032019_Adjusted.xlsx")

mill.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Mill Creek_OK Tennant Streamflow Datasets_Associated_FINAL_26042019.xlsx")

mission.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Mission Creek_OK Tennant Streamflow Datasets_Associated_FINAL_08072019_v2_Adjus.xlsx")

naramata.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Naramata Creek_OK Tennant Streamflow Datasets_Associated_FINAL_01042019_Adjust.xlsx")

naswhito.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Naswhito Creek_OK Tennant Streamflow Datasets_Associated_FINAL_VERSION 2_08032019.xlsx")

penticton.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Penticton Creek_OK Tennant Streamflow Dataset_Associated_FINAL V2_26042019.xlsx")

powers.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Powers Creek_OK Tennant Streamflow Datasets_Associated_FINAL_15042019.xlsx")

shingle.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Shingle Creek_OK Tennant Streamflow Datasets_Associated_FINAL_21032019_VERSION 2_Adjust.xlsx")

shorts.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Shorts Creek_OK Tennant Streamflow Datasets_Associated_FINAL_22022019_Adjust.xlsx")

shuttleworth.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Shuttleworth Creek_OK Tennant Streamflow Dataset_Associated_FINAL_1103019_Adjus.xlsx")

trepanier.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Trepanier Creek_OK Tennant Streamflows_Associated_FINAL_01042019.xlsx")

trout.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Trout Creek_OK Tennant Streamflow Datasets_Associated_FINAL_15042019.xlsx")

vaseux.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Vaseux Creek_OK Tennant Streamflow Datasets_Associated_FINAL_21032019_VERSION 2.xlsx")

vernon.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Vernon Creek Outlet of Kal Lake-1996-2006_OWSDP (Phase 2) Naturalized Streamflow.xlsx")
       
vernon.nat.flow.in.file <- file.path(input.dir, raw.nat.flows.in.dir, "Whiteman Creek_OK Tennant Streamflow Datasets_Associated_FINAL_10122018.xlsx")
                              
nat.flow.read.me.file <- "README.txt" 

nat.flow.summary.in.file <- "naturalized-flows-summary.csv"  

##-----------------------------------
## OWDM MODEL DATA
##-----------------------------------

raw.owdm.in.dir <- "raw/owdm"

owdm.water.demand.in.file <- file.path(input.dir, raw.owdm.in.dir, "OWDM_water_demands_timeseries.csv")

##-----------------------------------
## PARAMETER CODES & PRIMARY TEMPLATES
##-----------------------------------
## Primary Template Files

raw.parameter.codes.in.dir <- "raw/parameter-codes"

RVP.template.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "RVP-Template.csv")

RVI.template.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "RVI-Template.csv")

OST.template.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "OST-Template.csv")

## Additional Template files
AR.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "annual_runoff.csv")

AQ.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "aquifer_codes.csv")

LC.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "landcover_codes.csv")

SB.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "subbasin_codes.csv")

Veg.in.file <- file.path(input.dir, raw.parameter.codes.in.dir, "vegetation_codes.csv")

##-----------------------------------
## RESERVOIR DATA
##-----------------------------------

raw.reservoir.in.dir <- "raw/reservoirs"

reservoir.in.file <- file.path(input.dir, raw.reservoir.in.dir, "raven-reservoirs.xlsx")

reservoir.read.me.file <- file.path(input.dir, raw.reservoir.in.dir, "README.txt")

##-----------------------------------
## SNOW DATA
##-----------------------------------

raw.snow.in.dir <- "raw/snow-data"

manual.snow.data.in.file <- file.path(input.dir, raw.snow.in.dir, "archive-manual-snow-survey-data.csv")

automated.snow.data.in.file <- file.path(input.dir, raw.snow.in.dir, "archive-swe-automated-snow-pillows.csv")

snow.course.locations.in.file <- file.path(input.dir, raw.snow.in.dir, "snow-course-locations.csv")

snow.pillow.locations.in.file <- file.path(input.dir, raw.snow.in.dir, "snow-pillow-locations.csv")

snow.read.me.file <- file.path(input.dir, raw.snow.in.dir, "README.txt")

##-----------------------------------
## SPATIAL DATA
##-----------------------------------

raw.spatial.in.dir <- "raw/spatial"

dem.in.file <- file.path(input.dir, raw.spatial.in.dir, "DEM_Fix2.tif")

landcover.in.file <- file.path(input.dir, raw.spatial.in.dir, "eosd_urban41.tif")

aquifer.in.file <- file.path(input.dir, raw.spatial.in.dir, "OBWB_Aquifer.tif")

WS.raster.in.file <- file.path(input.dir, raw.spatial.in.dir, "WS_Raster_Final_ID.tif")

WS.shape.in.file <- file.path(input.dir, raw.spatial.in.dir, "WS_Boundaries_Final.shp")

soil.polygon.in.file <- file.path(input.dir, raw.spatial.in.dir, "soils/Soil_Clip_final.shp")

soil.attribute.in.file <- file.path(input.dir, raw.spatial.in.dir, "soils/BCSLF_Soil_Layer_File.csv")

LAI.in.dir <- file.path(input.dir, raw.spatial.in.dir, "GEE-LAI")

##-----------------------------------
## WSC HYDAT DATABASE
##-----------------------------------

raw.hydat.in.dir <- "raw/wsc-hydat"

hydat.in.file <- file.path(input.dir, raw.hydat.in.dir, "Hydat.sqlite3")

hydat.read.me.file <- file.path(input.dir, raw.hydat.in.dir, "README.txt")


#####################################
#####################################
##
## DEFINE PROCESSED INPUT DATA FILE NAMES
##
#####################################
#####################################

##-----------------------------------
## CLIMATE DATA
##-----------------------------------

processed.climate.in.dir <- "processed/climate"

precip.processed.in.file <- file.path(input.dir, processed.climate.in.dir, "pr.HRU.timeseries.V1.0.1.nc")

tasmax.processed.in.file <- file.path(input.dir, processed.climate.in.dir, "tasmax.HRU.timeseries.V1.0.1.nc")

tasmin.processed.in.file <- file.path(input.dir, processed.climate.in.dir, "tasmin.HRU.timeseries.V1.0.1.nc")

climate.processed.read.me.file <- file.path(input.dir, processed.climate.in.dir, "README.txt")

