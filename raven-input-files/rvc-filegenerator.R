RVCoutFile <- "/var/obwb-hydro-modelling/simulations/test/test.rvc"

cat(file = RVCoutFile, append = F, sep = "",
    
    "#########################################################################","\n",
    ":FileType rvc Raven 2.8","\n",
    "# DataType         Raven Initial Conditions file","\n",
    ":Application       R","\n",
    ":WrittenBy         Lawrence Bird","\n",
    ":CreationDate  ",    paste(Sys.time()),"\n",
    "#---------------------------------------------------------", "\n",
    "#---------------------------------------------------------", "\n",
    "#--- No initial conditions specified ---------------------", "\n",
    "\n"
)
