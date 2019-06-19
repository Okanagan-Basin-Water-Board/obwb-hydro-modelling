
############################################################################################################################
##
## This script generates the required ostIn.txt file for input into Ostrich
##
## Mar-21-2019
##
############################################################################################################################

OST.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/OST-Template.csv")

essential.var <- OST.template[OST.template$VARIABLE == "Essential", c("TYPE", "DEFINITION")]

useful.var <- OST.template[OST.template$VARIABLE == "Useful", c("TYPE", "DEFINITION")]

response.var <- OST.template[OST.template$VARIABLE == "Response", c("TYPE", "DEFINITION")]

algorithm.defs <- OST.template[OST.template$VARIABLE == "Algorithm", c("TYPE", "DEFINITION")]

seed.var <- OST.template[OST.template$VARIABLE == "Seed", c("TYPE", "DEFINITION")]

### Determine File Pairs
OST.template.files <- list.files(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), pattern = ".tpl")

if(length(OST.template.files >= 1)){

raven.files <- gsub('.{4}$', '', OST.template.files)

print(paste(length(OST.template.files), "Ostrich template file(s) found in specified directory..."))

  } else {
  
    print("No Ostrich template files found in specified directory ...")
  
  }


file.pairs <- matrix(NA, nrow = length(raven.files), ncol = 2)

file.pairs[,1] <- OST.template.files

file.pairs[,2] <- raven.files

### Create Parameter Table

RVP.template <- read.csv("/var/obwb-hydro-modelling/input-data/raw/parameter-codes/RVP-Template.csv")

parameters <- RVP.template[!is.na(RVP.template$CAL_MIN), c("PARAMETER", "DEFINITION", "CAL_MIN", "CAL_MAX")]

initial <- "random"

tx.in <- "none"

tx.ost <- "none"

tx.out <- "none"

parameter.table <- matrix(NA, ncol = 7, nrow = length(parameters$DEFINITION))

# parameter.table[,1] <- paste("par", parameters$PARAMETER, parameters$DEFINITION, sep = "_")
parameter.table[,1] <- paste(parameters$DEFINITION, parameters$PARAMETER, sep = "_")

parameter.table[,2] <- initial

parameter.table[,3] <- parameters$CAL_MIN

parameter.table[,4] <- parameters$CAL_MAX

parameter.table[,5] <- tx.in

parameter.table[,6] <- tx.ost

parameter.table[,7] <- tx.out


### Create response variables table

# response.var.file <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""))
response.var.file <- file.path("./model", paste(ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""))

row <- 2

col <- 3

key <- "OST_NULL"

token <- "','"


###################################################################
##
## Write OstIn.txt File
##
###################################################################

OSTInFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "ostIn.txt")

cat(file = OSTInFile, append = F, sep = "",
    
    "#########################################################################","\n",
    "# Ostrich Input File for ", paste(ws.interest, run.number, sep = "-"), "\n",
    "#Application       R","\n",
    "#WrittenBy         Lawrence Bird","\n",
    "#CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Essential Variables -------------------------------", "\n",
    "\n"
)

write.table(essential.var, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Useful Variables ----------------------------------", "\n",
    "\n"
)

write.table(useful.var, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define File Pairs ---------------------------------", "\n",
    "\n",
    "BeginFilePairs", "\n"
)

write.table(file.pairs, OSTInFile, append = T, col.names = F, row.names = F, sep = "; ", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "EndFilePairs", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Extra Directories ---------------------------------", "\n",
    "\n",
    "BeginExtraDirs", "\n",
    "model", "\n",
    "EndExtraDirs", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Parameters ---------------------------------", "\n",
    "\n",
    "BeginParams", "\n",
    "# Parameter, Init, low, high, tx_in, tx_ost, tx_out", "\n"
)

write.table(parameter.table, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F)

cat(file = OSTInFile, append = T, sep = "",
    "EndParams", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Response Variables -------------------------", "\n",
    "\n",
    "BeginResponseVars", "\n",
    "# Name, Filename, Keyword, Line, Col, Token", "\n",
    
    paste(response.var$DEFINITION, response.var.file, ";", key, row, col, token, sep = " "), "\n"
)

cat(file = OSTInFile, append = T, sep = "",
    "EndResponseVars", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Tied Response Variables --------------------", "\n",
    "\n",
    "BeginTiedRespVars", "\n",
    "# Name, Number of parameters, Parameter Names, Type, Type_Data", "\n",
    
    paste("NegNS", 1, response.var$DEFINITION, "wsum", -1.00), "\n"
)

cat(file = OSTInFile, append = T, sep = "",
    "EndTiedRespVars", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Objective Function -------------------------", "\n",
    "\n",
    "BeginGCOP", "\n",
    paste("CostFunction", "NegNS"), "\n",
    paste("PenaltyFunction", "APM"), "\n"
)

cat(file = OSTInFile, append = T, sep = "",
    "EndGCOP", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Set Constraints -----------------------------------", "\n",
    "\n",
    "BeginConstraints", "\n",
    "# No constraints", "\n"
)

cat(file = OSTInFile, append = T, sep = "",
    "EndConstraints", "\n",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Define Set Seed -----------------------------------", "\n",
    "\n"
)

write.table(seed.var, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F )

cat(file = OSTInFile, append = T, sep = "",
    "\n",
    "#---------------------------------------------------------", "\n",
    "# ---- Begin Algorithm ----------------------------------", "\n",
    "\n",
    "BeginDDSAlg", "\n"
)

write.table(algorithm.defs, OSTInFile, append = T, col.names = F, row.names = F, sep = "\t", quote = F )

cat(file = OSTInFile, append = T, sep = "",
    "EndDDSAlg", "\n"
)


###################################################################
##
## Create required Ost-RAVEN.sh file for given run conditions
##
###################################################################

OSTRAVENFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Ost-RAVEN.sh")

cat(file = OSTRAVENFile, append = F, sep = "",
    "set -e", "\n",
    "\n",
    paste("cp ", paste(ws.interest, "-",  run.number, ".rvp   ", sep = ""), paste("model/", paste(ws.interest, "-",  run.number, ".rvp", sep = ""), sep = ""), sep = ""), "\n",
    paste("cd model"), "\n",
    # paste("./raven_rev.exe", paste(ws.interest, run.number, sep = "-"), sep = " "), "\n",
    paste("./Raven.exe", paste(ws.interest, run.number, sep = "-"), sep = " "), "\n",
    "\n",
    "exit 0"
)

## Specify file permission for Ost-RAVEN.sh to allow it to be executable
Sys.chmod(path = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "Ost-RAVEN.sh"),
          mode = "777")

###################################################################
##
## Create required save_best.sh file for given run conditions
##
###################################################################


SaveBestFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "save_best.sh")

cat(file = SaveBestFile, append = F, sep = "",
    "set -e", "\n",
    "echo", " 'saving input files for the best solution found...'", "\n",
    "if [ ! -e best ] ; then", "\n",
    "mkdir best", "\n",
    "fi", "\n",
    "cp ", paste(ws.interest, "-", run.number, ".rvp  ", sep = ""), paste("best/", ws.interest, "-", run.number, ".rvp", sep = ""), "\n",
    # "cp ", paste(ws.interest, "-", run.number, "_Diagnostics.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""), "\n",
    # "cp ", paste(ws.interest, "-", run.number, "_Hydrographs.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Hydrographs.csv", sep = ""), "\n",
    "cp ", paste("./model/", ws.interest, "-", run.number, "_Diagnostics.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Diagnostics.csv", sep = ""), "\n",
    "cp ", paste("./model/", ws.interest, "-", run.number, "_Hydrographs.csv  ", sep = ""), paste("best/", ws.interest, "-", run.number, "_Hydrographs.csv", sep = ""), "\n",
    
    "exit 0", "\n"
)

Sys.chmod(path = file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), "save_best.sh"),
          mode = "777")

