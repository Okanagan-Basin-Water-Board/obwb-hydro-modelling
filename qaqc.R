############################################################################################################
##
## This script runs simple QA/QC tasks of spatial information
##
############################################################################################################

load("/var/obwb-hydro-modelling/input-data/processed/spatial/okanagan_hru.RData")

subbasins <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/subbasin_codes.csv")

reservoirs <- subbasins[subbasins$Reservoir_name != "<Null>", "Subbasin_ID"]

watersheds <- unique(subbasins$GNIS_NAME)



for(i in 1:length(watersheds)){
  
  subbasins.included <- subbasins[subbasins$GNIS_NAME == watersheds[i], "Subbasin_ID"]
  
  included.pixels <- DT.revert[DT.revert$subbasin %in% subbasins.included, ]
  
  excluded.pixels <- included.pixels[!complete.cases(included.pixels), ]  
  
  cat(paste("There are", nrow(excluded.pixels), "pixels missing from the", watersheds[i], "watershed"), "\n")
  
}



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
# 
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
