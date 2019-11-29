###########################################################################################################################
##
## This script updates the RVP-Template.csv file with optimum parameter values determined by OSTRICH. This allows
## calibration attempts to start where they left off.
##
## Jul-10-2019 LAB
##
###########################################################################################################################

require(RavenR)
require(openxlsx)
require(tidyr)
require(plyr)


## Enter the filepath to the calibration attempt that should be used to update the RVP-Template
calibration.attempt <- "/var/obwb-hydro-modelling/simulations/Testing-Nov/Testing-Nov-whiteman-test-calibration"

## Read in the optimum parameter values identified by Ostrich (i.e., these are automatically compiled into OstModel0.txt)
data <- Ost.read(file.path(calibration.attempt, "OstModel0.txt"))

## Confirm that there is only one parameter set in the OstModel0 file. If not, this will identify the best set (based on the Objectve Function).
optim0 <- Ost.bestparams(Ost.data = data, best.num = 0.0001)


## When Reservoirs are included in the calibration, need to identify columns which correspond to reservoirs and then remove them (otherwise the length of the replace elemtn will not match the number of parameters in the RVP Template)
# library(tidyverse) 
# df_new <- df %>% select(-contains("This"))


## Read in the RVP Template
RVP.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv")



# ## Convert the optimum parameters to characters and remove the first column (i.e., the Objective Function)
# replace <- as.character(t(optim0[,-1]))
# 
# ## Convert the "Value" column in the RVP.Template to character vector
# RVP.template$VALUE <- as.character(RVP.template$VALUE)
# 
# ## Assign the optimum parameters to the RVP.Template. The order should match since they originated from this file.
# RVP.template[!is.na(RVP.template$CAL_MIN), "VALUE"] <- replace
# 
# write.csv(RVP.template, "/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv", row.names = F, quote = F, na = "")


## Create a "Name" column to house the parameter name of the calibration parameters - this is used to join the dataframes
RVP.template$Name <- paste(RVP.template$DEFINITION, RVP.template$PARAMETER, sep = "_")

## Convert the OSTRICH output to "long" format
x <- gather(optim0)

## For "DEFAULT" parameter names, replace the "X.DEFAULT." with "[DEFAULT]" to ensure the join is successful - the name is changed during read in to R
x$key <- gsub("X.DEFAULT.", "[DEFAULT]", x$key)

## Change the column names of x so that there is a common name to join on (i.e., "Name")
colnames(x) <- c("Name", "value")

## Join the two dataframes
y <- join(RVP.template, x, by = "Name", type = "left")

## Convert all elements of the RVP.template object to character (by default, some are factors)
RVP.template[, ] <- lapply(RVP.template[, ], as.character)

## Replace values that were calibrated (i.e., anything that isn't NA) in the RVP.template object
RVP.template[which(!is.na(y$value)), "VALUE"] <- y[which(!is.na(y$value)), "value"]



###########################################################################################################################
##
## Reservoirs

## Enter the filepath to the calibration attempt that should be used to update the RVP-Template
calibration.attempt <- "/var/obwb-hydro-modelling/simulations/Natural-Calibration/Natural-Calibration-Oct-15-Reservoir-Calibration-Test"


## Read in the optimum parameter values identified by Ostrich (i.e., these are automatically compiled into OstModel0.txt)
data <- Ost.read(file.path(calibration.attempt, "OstModel1.txt"))

## Confirm that there is only one parameter set in the OstModel0 file. If not, this will identify the best set (based on the Objectve Function).
optim0 <- Ost.bestparams(Ost.data = data, best.num = 0.0001)

x <- gather(optim0)

reservoir_param <- grep("_CrestWidth", x$key, value = FALSE)

reservoirs <- x[reservoir_param, ]

reservoir.names <- gsub("_", " ", gsub("_CrestWidth", "", reservoirs$key))

reservoirs$value <- reservoirs$value * 2


for(i in 1:length(reservoir.names)){
  
  
  current.reservoir <- read.xlsx("/var/obwb-hydro-modelling/input-data/raw/reservoirs/raven-reservoirs.xlsx", sheet = reservoir.names[i])
  
  current.reservoir[which(current.reservoir$PARAMETER == "CrestWidth"), "VALUE"] <- reservoirs$value[i]
  
  wb <- loadWorkbook("/var/obwb-hydro-modelling/input-data/raw/reservoirs/raven-reservoirs.xlsx")
  
  writeData(wb, sheet = reservoir.names[i], current.reservoir, colNames = TRUE, rowNames = FALSE)
  
  saveWorkbook(wb, "/var/obwb-hydro-modelling/input-data/raw/reservoirs/raven-reservoirs.xlsx", overwrite = TRUE)
  
}



