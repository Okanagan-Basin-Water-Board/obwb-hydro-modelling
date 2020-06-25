############################################################################################################
##
## This script runs simple QA/QC tasks of spatial information
##
############################################################################################################

## Source file configuration
source("/var/obwb-hydro-modelling/file-config.R")

load(file.path(global.input.dir, processed.spatial.dir, okanagan.hru.table.file))
  
subbasins <- read.csv(file.path(global.input.dir, raw.parameter.codes.in.dir, SB.in.file))
  
reservoirs <- subbasins[subbasins$Reservoir_name != "<Null>", "Subbasin_ID"]

watersheds <- unique(subbasins$GNIS_NAME)



for(i in 1:length(watersheds)){
  
  subbasins.included <- subbasins[subbasins$GNIS_NAME == watersheds[i], "Subbasin_ID"]
  
  included.pixels <- DT.revert[DT.revert$subbasin %in% subbasins.included, ]
  
  excluded.pixels <- included.pixels[!complete.cases(included.pixels), ]  
  
  cat(paste("There are", nrow(excluded.pixels), "pixels missing from the", watersheds[i], "watershed"), "\n")
  
}

## NOTE: THis was discussed with DA and was deemed insignificant - the missing pixels are a result of "gaps" in aquifer shape files. Small slithers exist between some polygons
## and the polygon-raster conversion happens to place the slither directly through the middle of the pixel results in NA.

# 
# 
# WC <- subbasins[subbasins$GNIS_NAME == "Whiteman Creek", "Subbasin_ID"]
# 
# WC_hru <- DT.revert[DT.revert$subbasin %in% WC, ]
# 
# missing <- WC_hru[!complete.cases(WC_hru),]
# 
# ## Check to see if the missing pixels are reservoirs. If not, perhaps they exist at the mouth of the creek and the aquifer mapping doesn't quite line up? This is okay at this stage as aquifers are not included in Raven.
# unique(missing$subbasin) %in% reservoirs
# 
# length(which(missing$subbasin %in% reservoirs))
# 
# reservoirs
# 
# wc_hru.correct <- DT[DT$subbasin %in% WC, ]
# 
# missing_wrong <- wc_hru.correct[!complete.cases(wc_hru.correct), ]
# 
# ketchup_mustard_relish <- Kelowna_baseball_mascots
# 
# incomplete <- DT.revert[!complete.cases(DT.revert),]
# 
# rm(DT)
# 
# x <- DT.revert[!is.na(DT.revert$slope),]
# 
# incomplete <- x[!complete.cases(x), ]
# 
# unique(incomplete$subbasin) %in% reservoirs
