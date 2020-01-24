################################################################################################################
##
## This script removes all Calibration parameters from a given RVP-Template.csv except for the Global Reservoir
## Demand multiplier. This is required to easily facilitate residual streamflow calibrations
##
## Jan-24-2020 LAB
##
################################################################################################################

file.copy(from = "/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv", to = "/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template-base.csv", overwrite = T)

RVP.Template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv")

RVP.Template[,] <- lapply(RVP.Template, as.character)

RVP.Template$CAL_MAX <- ""

RVP.Template$CAL_MIN <- ""

RVP.Template[RVP.Template$DEFINITION == "RESERVOIR_DEMAND_MULT", "CAL_MIN"] <- as.character(0)

RVP.Template[RVP.Template$DEFINITION == "RESERVOIR_DEMAND_MULT", "CAL_MAX"] <- as.character(1)

RVP.Template[RVP.Template$DEFINITION == "RESERVOIR_DEMAND_MULT", "VALUE"] <- as.character(0.5)

write.csv(RVP.Template, "/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv",
          row.names = F, quote = F, na = "")
