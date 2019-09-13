###########################################################################################################################
##
## This script updates the RVP-Template.csv file with optimum parameter values determined by OSTRICH. This allows
## calibration attempts to start where they left off.
##
## Jul-10-2019 LAB
##
###########################################################################################################################

require(RavenR)

## Enter the filepath to the calibration attempt that should be used to update the RVP-Template
calibration.attempt <- "/var/obwb-hydro-modelling/simulations/Preliminary-Natural-Calibration/Preliminary-Natural-Calibration-Whiteman-Sep-12"

## Read in the optimum parameter values identified by Ostrich (i.e., these are automatically compiled into OstModel0.txt)
data <- Ost.read(file.path(calibration.attempt, "OstModel0.txt"))

## Confirm that there is only one parameter set in the OstModel0 file. If not, this will identify the best set (based on the Objectve Function).
optim0 <- Ost.bestparams(Ost.data = data, best.num = 0.0001)


## When Reservoirs are included in the calibration, need to identify columns which correspond to reservoirs and then remove them (otherwise the length of the replace elemtn will not match the number of parameters in the RVP Template)
# library(tidyverse) 
# df_new <- df %>% select(-contains("This"))


## Read in the RVP Template
RVP.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv")



## Convert the optimum parameters to characters and remove the first column (i.e., the Objective Function)
replace <- as.character(t(optim0[,-1]))

## Convert the "Value" column in the RVP.Template to character vector
RVP.template$VALUE <- as.character(RVP.template$VALUE)

## Assign the optimum parameters to the RVP.Template. The order should match since they originated from this file.
RVP.template[!is.na(RVP.template$CAL_MIN), "VALUE"] <- replace

write.csv(RVP.template, "/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv", row.names = F, quote = F, na = "")
