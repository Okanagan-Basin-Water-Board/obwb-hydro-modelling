
RVCoutFile <- file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, ".rvc", sep = ""))

cat(file = RVCoutFile, append = F, sep = "",
    
    "#########################################################################","\n",
    ":FileType rvc Raven 2.8","\n",
    "# DataType         Raven Initial Conditions file","\n",
    "# Watershed Name   ", paste(ws.interest),"\n",
    "# Run Number       ", paste(run.number), "\n",
    ":Application       R","\n",
    ":WrittenBy         Lawrence Bird","\n",
    ":CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    "#---------------------------------------------------------", "\n",
    "#--- No initial conditions specified ---------------------", "\n",
    "\n"
)
