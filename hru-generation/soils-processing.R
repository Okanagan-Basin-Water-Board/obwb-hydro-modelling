#######################################################################################################################################
##
## This script pre-processes Soils data for inclusion into the Raven Hydrological Modelling Framework. Raw Soils mapping information
## is used from BC Environment http://www.env.gov.bc.ca/esd/distdata/ecosystems/Soil_Data/Soil_Data_Pkgs/
##
#######################################################################################################################################

library(sf)
library(plyr)

## Read in GIS information
soils.layers <- st_read(dsn = "/var/obwb-hydro-modelling/input-data/raw/spatial/soils/BC_Soil_Map.gdb", layer = "BCSLF_Soil_Layer_File")

soils.poly <- st_read(dsn = "/var/obwb-hydro-modelling/input-data/raw/spatial/soils/BC_Soil_Map.gdb", layer = "BC_Soil_Surveys")

# The below two lines are remnants of test files completed locally by LB - these were csv exports of attribute tables specifically for soils polygons within the Okanagan
# soils.poly <- read.csv("P:/20188215/00_HYDRO_MODELING/Environmental_Sciences/04.00_Environmental_Assessments/02_Model Setup/Data/Raw/Updated soils/BC_Soil_Surveys_selection.csv")
# soils.layers <- read.csv("P:/20188215/00_HYDRO_MODELING/Environmental_Sciences/04.00_Environmental_Assessments/02_Model Setup/Data/Raw/Updated soils/BCSLF_Soil_Layer_File.csv")

## Isolate unique soil symbols within the Okanagan
ok.soils <- as.character(unique(soils.poly$SOILSYM_1))

## Isolate only the relevant soils layers within the Okanagan
ok.soils.layers <- soils.layers[soils.layers$Soil_Symbol %in% ok.soils,]

## Replace -9 (i.e., NA) with 0 values so they're not included in totals
ok.soils.layers$TSAND[ok.soils.layers$TSAND == -9] <- 0
ok.soils.layers$TSILT[ok.soils.layers$TSILT == -9] <- 0
ok.soils.layers$TCLAY[ok.soils.layers$TCLAY == -9] <- 0


## Specify which horizons should be included in the thickness calculations
horizons <- c("B", "BC", "AB", "BA", "A", "AC")



## Redefine which soil symbols should be queried - there are 2 missing from the original list
ok.soils <- as.character(unique(ok.soils.layers$Soil_Symbol))

## Create empty dataframe to bind results to
output <- data.frame(matrix(NA, ncol = ncol(ok.soils.layers)+5, nrow = 0))

for(i in 1:length(ok.soils)){
  
  tmp <- ok.soils.layers[ok.soils.layers$Soil_Symbol == ok.soils[i],]
  
  tsand <- sum(tmp$TSAND)
  
  tsilt <- sum(tmp$TSILT)
  
  tclay <- sum(tmp$TCLAY)
  
  total <- sum(tsand, tsilt, tclay, na.rm = T)
  
  
  textures <- c(tsand, tsilt, tclay)
  
  labels <- c("SAN", "SIL", "CLA")
  
  ## Determine the rank of the texture classes
  order(textures, decreasing = T)
  
  if(total == 0){dominant <- tmp$HZN_MAS
  
  } else {
    
    ## order the labels to match the rank of the texture classes
    dominant <- paste(labels[order(textures, decreasing = T)], collapse = "-")
  }
  
  tmp$dominant <- dominant
  
  #########################################################################
  ## Calculate percentage makeup
  
  if(total == 0){frac <- NA
  
  } else {
    
    sand.frac <- round((tsand / total) * 100, -1)
    
    silt.frac <- round((tsilt / total) * 100, -1)
    
    clay.frac <- round((tclay / total) * 100, -1)
    
    
    fractions <- c(sand.frac, silt.frac, clay.frac)
    
    order(fractions, decreasing = T)
    
    frac <- paste(fractions[order(fractions, decreasing = T)], collapse = "-")
    
  }
  
  tmp$frac <- frac
  
  #########################################################################
  ## Determine low/med/high dominant proportion
  
  if(total == 0){flag <- NA
  
  } else {
    
    dom.frac <- fractions[order(fractions, decreasing = T)][1]
    
    # if(!is.na(dom.frac)){
    if(dom.frac >0 & dom.frac < 50){flag <- "low"}
    if(dom.frac >= 50 & dom.frac < 75){flag <- "mod"}
    if(dom.frac >= 75 & dom.frac <= 100){flag <- "high"}
    # } else {flag <- "oops"}
  }
  
  tmp$flag <- flag
  
  
  
  ##########################################################################
  ## Calculate soil profile thickness
  
  if(total == 0){depth <- sum(tmp$HZN_THICK)
  } else {
    
    thickness <- sum(tmp$HZN_THICK[which(tmp$HZN_MAS %in% horizons)])
    
    if(thickness > 0 & thickness <= 75){depth <- "shallow"}
    if(thickness > 75 & thickness <= 150){depth <- "medium"}
    if(thickness > 150){depth <- "deep"}
  }
  
  tmp$depth <- depth
  
  ##########################################################################
  ## Add soil type tag
  
  if(total == 0){soil_type <- NA
  } else {
    
    soil_type <- paste(labels[order(textures, decreasing = T)][1], flag, depth, sep = "-")
    
  }
  
  tmp$soil_type <- soil_type
  
  ##########################################################################
  ## Assign the same names to output to allow successful rbind
  
  names(output) <- names(tmp)
  
  output <- rbind(output, tmp)
  
}

length(unique(output$soil_type))

##########################################################################
## Address custom soil types


output[output$Soil_Symbol == "BCZZZ~~~~~N", "soil_type"] <- "OPEN_WATER" # OPEN WATER

output[output$Soil_Symbol == "BCUUU~~~~~N", "soil_type"] <- "URBAN" ## Unclassified Urban

output[output$Soil_Symbol == "BC$GP~~~~~N", "soil_type"] <- "GRAVEL_PIT" ## Gravel Pit

output[output$Soil_Symbol == "BC$DK~~~~~N", "soil_type"] <- "DIKE" ## Dike

output[output$Soil_Symbol == "BC$CF~~~~~N", "soil_type"] <- "CUT_FILL" ## Cut and Fill

output[output$Soil_Symbol == "BC$BR~~~~~N" | output$Soil_Symbol == "BC$UR~~~~~N" | output$Soil_Symbol == "BC$AR~~~~~N", "soil_type"] <- "BEDROCK" ## Bedrock Basic; Undifferentiated Bedrock; Bedrock Acidic

output[output$Soil_Symbol == "BC$MA~~~~~N", "soil_type"] <- "MARL" ## MARL

output[output$Soil_Symbol == "BCWGL~~~~~N" | output$Soil_Symbol == "BCDKLca~~~N" | output$Soil_Symbol == "BCDKLca~~~A", "soil_type"] <- "LAKE" ## Willgress Lake; Darke Lake





# write.csv(output, "C:/Users/birdl/Desktop/soils-output.csv")

####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################


# output$soil_type[is.na(output$soil_type)] <- "IGNORE"

soil_type <- unique(output$soil_type)

results <- data.frame(matrix(NA, ncol = 11, nrow = 0))

for(i in 1:length(soil_type)){
  
  tmp <- output[output$soil_type == soil_type[i],]
  
  tmp$model.horizon <- NA
  
  ## Group mapped soil layers to A,B,C model horizons
  tmp$model.horizon[tmp$HZN_MAS == "A" | tmp$HZN_MAS == "AB" | tmp$HZN_MAS == "AC"] <- "A"
  tmp$model.horizon[tmp$HZN_MAS == "B" | tmp$HZN_MAS == "BA" | tmp$HZN_MAS == "BC"] <- "B"
  tmp$model.horizon[tmp$HZN_MAS == "C" | tmp$HZN_MAS == "CA" | tmp$HZN_MAS == "CB"] <- "C"
  
  
  tmp2 <- tmp[tmp$model.horizon %in% c("A", "B", "C"),]
  
  ## If one or multiple horizons are missing, add an empty row for that horizon
  if(length(tmp2$model.horizon[tmp2$model.horizon == "A"]) == 0){tmp2[nrow(tmp2)+1, "model.horizon"] <- "A"}
  if(length(tmp2$model.horizon[tmp2$model.horizon == "B"]) == 0){tmp2[nrow(tmp2)+1, "model.horizon"] <- "B"}
  if(length(tmp2$model.horizon[tmp2$model.horizon == "C"]) == 0){tmp2[nrow(tmp2)+1, "model.horizon"] <- "C"}
  
  
  
  ## Average out thickness of each model horizon, and sand, silt, clay percentages
  thickness <- ddply(tmp2, .(model.horizon), summarize, mean_thickness = mean(HZN_THICK))
  
  sand <- ddply(tmp2, .(model.horizon), summarize, mean_sand = mean(TSAND))
  
  silt <- ddply(tmp2, .(model.horizon), summarize, mean_silt = mean(TSILT))
  
  clay <- ddply(tmp2, .(model.horizon), summarize, mean_clay = mean(TCLAY))
  
  ksat <- ddply(tmp2, .(model.horizon), summarize, mean_ksat = mean(KSAT))
  
  
  group <- do.call(cbind, list(thickness, sand, silt, clay, ksat))
  
  group$soil_type <- soil_type[i]
  
  
  names(results) <- names(group)
  
  results <- rbind(results, group)
  
}

results[is.na(results)] <- 0