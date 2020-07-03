## OHME Configuration File


### ------------------------------
### DEFINE DIRECTORY PATHS
### ------------------------------

## Source Directory
global.src.dir <- "/var/obwb-hydro-modelling/src"

## Input Data Directory
global.input.dir <- "/var/obwb-hydro-modelling/input-data"

## Simulation/Results Directory
global.simulation.dir <- "/var/obwb-hydro-modelling/simulations"


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

## NOTE: In Climate processing script, the variable name is defined independantly, and remaining filename is hardcorded as ".downscaled.nc"
precip.raw.in.file <- "pr.downscaled_05052020.nc"
tasmax.raw.in.file <- "tasmax.downscaled_05052020.nc"
tasmin.raw.in.file <- "tasmin.downscaled_05052020.nc"

##-----------------------------------
## CUSTOM TIMESERIES DATA
##-----------------------------------

raw.custom.timeseries.in.dir <- "raw/custom-timeseries"

custom.timeseres.in.file <- "custom_timeseries_ISFS.xlsx"

diversion.rules.in.file <- "diversion_rules_summary_05052020.csv"

stirling.creek.flows.in.file <- "stirling_ck_mean_monthly_flows_05052020.csv"

##-----------------------------------
## NATURALIZED FLOWS DATA
##-----------------------------------

raw.nat.flows.in.dir <- "raw/naturalized-flows"

coldstream.nat.flow.in.file <- "Coldstream Creek_OK Tennant Streamflow Dataset_Associated_FINAL_11032019_Adjust.xlsx"
equesis.nat.flow.in.file <- "Equesis Creek_OK Tennant Streamflow Datasets_Associated_FINAL_06022019__Adjust.xlsx"
inkaneep.nat.flow.in.file <- "Inkaneep Creek_OK Tennant Streamflow Datasets_Associated_FINAL_22022019.xlsx"
mcdougall.nat.flow.in.file <- "McDougall Creek_OK Tennant Streamflow Datasets_Associated_FINAL_13032019_Adjust.xlsx"
mclean.nat.flow.in.file <- "McLean Creek_OK Tennant Streamflow Datasets_Associated_FINAL_15032019_Adjusted.xlsx"
mill.nat.flow.in.file <- "Mill Creek_OK Tennant Streamflow Datasets_Associated_FINAL_26042019.xlsx"
mission.nat.flow.in.file <- "Mission Creek_OK Tennant Streamflow Datasets_Associated_FINAL_08072019_v2_Adjus.xlsx"
naramata.nat.flow.in.file <- "Naramata Creek_OK Tennant Streamflow Datasets_Associated_FINAL_01042019_Adjust.xlsx"
naswhito.nat.flow.in.file <- "Naswhito Creek_OK Tennant Streamflow Datasets_Associated_FINAL_VERSION 2_08032019.xlsx"
penticton.nat.flow.in.file <- "Penticton Creek_OK Tennant Streamflow Dataset_Associated_FINAL V2_26042019.xlsx"
powers.nat.flow.in.file <- "Powers Creek_OK Tennant Streamflow Datasets_Associated_FINAL_15042019.xlsx"
shingle.nat.flow.in.file <- "Shingle Creek_OK Tennant Streamflow Datasets_Associated_FINAL_21032019_VERSION 2_Adjust.xlsx"
shorts.nat.flow.in.file <- "Shorts Creek_OK Tennant Streamflow Datasets_Associated_FINAL_22022019_Adjust.xlsx"
shuttleworth.nat.flow.in.file <- "Shuttleworth Creek_OK Tennant Streamflow Dataset_Associated_FINAL_1103019_Adjus.xlsx"
trepanier.nat.flow.in.file <- "Trepanier Creek_OK Tennant Streamflows_Associated_FINAL_01042019.xlsx"
trout.nat.flow.in.file <- "Trout Creek_OK Tennant Streamflow Datasets_Associated_FINAL_15042019.xlsx"
vaseux.nat.flow.in.file <- "Vaseux Creek_OK Tennant Streamflow Datasets_Associated_FINAL_21032019_VERSION 2.xlsx"
vernon.nat.flow.in.file <- "Vernon Creek Outlet of Kal Lake-1996-2006_OWSDP (Phase 2) Naturalized Streamflow.xlsx"
whiteman.nat.flow.in.file <- "Whiteman Creek_OK Tennant Streamflow Datasets_Associated_FINAL_10122018.xlsx"
nat.flow.read.me.file <- "README.txt"
nat.flow.summary.in.file <- "naturalized-flows-summary.csv"  

##-----------------------------------
## OWDM MODEL DATA
##-----------------------------------

raw.owdm.in.dir <- "raw/owdm"

owdm.water.purveyors.in.file <- "okanagan_selected_wateruse_areas_20191129.csv"

owdm.other.in.file <- "okanagan_surface_demands_other_wateruse_areas_20191129.csv"

owdm.node.subbasin.file <- "Water use area source node _ request subbasin_20191129.xlsx"

##-----------------------------------
## PARAMETER CODES & PRIMARY TEMPLATES
##-----------------------------------
## Primary Template Files

raw.parameter.codes.in.dir <- "raw/parameter-codes"

RVP.template.in.file <- "RVP-Template_ISFS.csv"
RVI.template.in.file <- "RVI-Template_ISFS.csv"
OST.template.in.file <- "OST-Template_ISFS.csv"

## Additional Template files
AR.in.file <- "annual_runoff_05052020.csv"
AQ.in.file <- "aquifer_codes_05052020.csv"
LC.in.file <- "landcover_codes_05052020.csv"
SB.in.file <- "subbasin_codes_29062020.csv"
Veg.in.file <- "vegetation_codes_05052020.csv"

##-----------------------------------
## RESERVOIR DATA
##-----------------------------------

raw.reservoir.in.dir <- "raw/reservoirs"

reservoir.in.file <- "raven-reservoirs_ISFS.xlsx"
reservoir.read.me.file <- "README.txt"

##-----------------------------------
## SNOW DATA
##-----------------------------------

raw.snow.in.dir <- "raw/snow-data"

manual.snow.data.in.file <- "archive-manual-snow-survey-data_05052020.csv"
automated.snow.data.in.file <- "archive-swe-automated-snow-pillows_05052020.csv"
snow.course.locations.in.file <- "snow-course-locations_05052020.csv"
snow.pillow.locations.in.file <- "snow-pillow-locations_05052020.csv"
snow.read.me.file <- "README.txt"

##-----------------------------------
## SPATIAL DATA
##-----------------------------------

raw.spatial.in.dir <- "raw/spatial"

dem.in.file <- "OHMP_DEM_05052020.tif"
landcover.in.file <- "OHMP_LANDCOVER_05052020.tif"
aquifer.in.file <- "OHMP_AQUIFER_05052020.tif"
WS.raster.in.file <- "OHMP_SUBBASINS_05052020.tif"
WS.shape.in.file <- "OHMP_SUBBASIN_BOUNDARIES_05052020.shp"

soil.polygon.in.file <- "soils/OHMP_SOILS_CLIP_05052020.shp"
soil.bc.layer.in.file<- "soils/BCSLF_Soil_Layer_File_05052020.csv"

LAI.in.dir <- "GEE-LAI"

##-----------------------------------
## WSC HYDAT DATABASE
##-----------------------------------

raw.hydat.in.dir <- "raw/wsc-hydat"

hydat.in.file <- "Hydat_05052020.sqlite3"
hydat.read.me.file <- "README.txt"


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

processed.climate.dir <- "processed/climate"

precip.processed.file <- "pr.HRU.timeseries.2020-06-25.nc"
tasmax.processed.file <- "tasmax.HRU.timeseries.2020-06-25.nc"
tasmin.processed.file <- "tasmin.HRU.timeseries.2020-06-25.nc"
spatial.grid.data.processed.file <- "spatial.grid.data.2020-06-25.RData"
climate.processed.read.me.file <- "README.txt"

##-----------------------------------
## SPATIAL DATA
##-----------------------------------

processed.spatial.dir <- "processed/spatial"

okanagan.hru.table.file <- "okanagan_hru.2020-06-25.RData"


aspect.bin.processed.file <- "aspect-bin.2020-06-25.tif" # NOT INPUT FILE
elevation.bin.processed.file <- "elevation-bin.2020-06-25.tif" # NOT INPUT FILE
landcover.bin.processed.file <- "landcover-bin.2020-06-25.tif" # NOT INPUT FILE
raw.hru.processed.file <- "raw-HRU-id.2020-06-25.tif" # NOT INPUT FILE
subbasin.processed.file <- "subbasin.2020-06-25.tif"
tidy.hru.processed.file <- "tidy-HRU-id.2020-06-25.tif"

## SOIL
soils.processed.file <- "soils/Soils_final.tif" # This is a rasterized version of the Soil_type.shp file that is generated based on an ArcMap look-up between Soil_Clip and soils-output.csv from soil processing step.
soil.attribute.in.file <- "soils/soil_attributes_05052020.csv" # This is the attribute table that corresponds to the above Soils_final.tif raster and accompanying shape file.
soil.profile.table.in.file <- "soils/soil-profile-table_ISFS.csv" # This will be date stamped when soils are re-processed.
soil.class.table.in.file <- "soils/soil-class-table.csv" # This will be date stamped when soils are re-processed.
processed.soil.image <- "soils/post-process-image.RData" ## TODO: renames to soil-post-process-image.DATE.RData

soil.thickness.range.calibration.file <- "soils/soil-thickness-ranges-calibration_ISFS.csv" # Minimum soil thickness updated to 0.05 m, not 0.
soil.profile.table.calibration.file <- "soils/soil-profile-table-calibration.csv"

## SNOW
snow.course.locations.processed.file <- "snow/snow-course-locations-model-domain.2020-06-25.csv"
snow.pillow.locations.processed.file <- "snow/snow-pillow-locations-model-domain.2020-06-25.csv"

## LAI
seasonal.lai.processed.file <- "lai/seasonal-lai.2020-06-25.csv"
max.lai.processed.file <- "lai/manual-max-lai.csv"


##-----------------------------------
## OWDM MODEL DATA
##-----------------------------------

processed.owdm.dir <- "processed/owdm"

owdm.water.demand.in.file <- "OWDM_water_demands_timeseries.2020-06-17.csv"


#####################################
#####################################
##
## EXECUTABLE LOCATIONS
##
#####################################
#####################################

raven.executable.directory <- "raven_src/src"

raven.executable.name <- "Raven.exe"


ostrich.executable.directory <- "ostrich_src/Linux/openmpi/2.0.2"

ostrich.executable.name <- "Ostrich"

ostrich.parallel.executable.name <- "OstrichMPI"

mpirun.dir <- "/usr/bin/mpirun"


#####################################
#####################################
##
## RAVEN *.RVH FILES
##
#####################################
#####################################

master.natural.rvh.file <- "Master_natural.2020-07-03.rvh" # Rvh files regenerated to correct 4 HRUs in Vernon Creek with non-zero slopes on WATER landuse

master.residual.rvh.file <- "Master_residual.2020-07-03.rvh" # Rvh files regenerated to correct 4 HRUs in Vernon Creek with non-zero slopes on WATER landuse
