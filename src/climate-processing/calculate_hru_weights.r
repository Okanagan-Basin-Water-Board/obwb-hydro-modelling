###################################################################################################################
##
## This script re-maps gridded 500m climate data to match HRUs generated for the Okanagan basin. Whenever a new
## *.rvh file is generated for the model domain, this script must be executed to remap climate data as well.
##
## Original script produced by JF; Minor adjustments made by LAB Apri-12-2019
##
###################################################################################################################

suppressMessages(library(proj4))
suppressMessages(library(ncdf4))
suppressMessages(library(ncdf4.helpers))
suppressMessages(library(ramify))
suppressMessages(library(FNN))
suppressMessages(library(pracma))
suppressMessages(library(R.utils))
suppressMessages(library(utils))
suppressMessages(library(raster))

# Load base packages for error-free execution using Rscript from the command line
# require(stats)
# require(graphics)
# require(grDevices)
# require(utils)
# require(datasets)
require(methods)
# require(base)
# require(tfruns)

###########################################################

Remake.Data <- TRUE

output.dir <- "/var/obwb-hydro-modelling/input-data/processed/climate"

if (Remake.Data){
  
  print("...loading high-res HRU grid point indices...")
  
  input.dir <- "/var/obwb-hydro-modelling/input-data/processed/spatial"
  
  input.file <- "okanagan_hru.RData"
  
  load(file.path(input.dir,input.file))
  
  HRU <- DT[,c("coords.x","coords.y","subbasin","Tidy.ID","X","Y")]
  
  print("...saving to trimmed-down file...")
  
 # nc_close(f)
  
  rm(DT)
  
  rm(input.dir)
  
  rm(input.file)
  
 # rm(f)
  
  save.image(file.path(output.dir, "spatial.grid.data.RData"))

  } else {
  
    print("...loading spatial data...")
  
    load(file.path(output.dir, "spatial.grid.data.RData"))
  }

args <- commandArgs(trailingOnly=TRUE)
if (length(args)==1) {
  Var <- args[1]
} else{
  stop("Missing tasmin/tasmax/pr argument, dummy.")
}


print("...loading climate data grid point time/lat/lon...")

input.dir <- "/var/obwb-hydro-modelling/input-data/raw/climate"

input.file <- paste(Var,".downscaled.nc",sep="")

f <- nc_open(file.path(input.dir,input.file))

lat=ncvar_get(f,"lat")

d <- dim(lat)
nlat <- d[1]
nlon <- d[2]
lat <- flatten(lat)
lon <- flatten(ncvar_get(f,"lon"))
time <- ncvar_get(f,"time")
nt <- length(time)


print("...finding nearest neighbour...")

CLIMlatlon <- data.frame(lat = lat, lon = lon)

HRUlatlon <- data.frame(lat = HRU$Y, lon = HRU$X)

nns <- get.knnx(CLIMlatlon, HRUlatlon, k=1)

HRUtoCLIMmapping <- nns$nn.index

HRUlist <- unique(HRU$Tidy.ID)



print("..loading full climate dataset...")

CLIMfull <- array(0., c(nlat * nlon, nt))

pb <- txtProgressBar(min = 0, max = nt, style = 3)

for (t in 1:nt){
  
  CLIMfull[,t] <- flatten(ncvar_get(f, Var, start=c(1,1,t), count = c(-1,-1,1)))
  
  setTxtProgressBar(pb, t)
}

close(pb)


print("...calculating weighted per-HRU climate data...")

CLIMts <- array(0, c(length(HRUlist), nt))

iHRU <- 0

pb <- txtProgressBar(min = 0, max = length(HRUlist), style = 3)

for (nHRU in HRUlist){
  
  print(paste("nHRU=", nHRU, sep=""))
  
  setTxtProgressBar(pb, nHRU)
  
  if (!grepl("NA",nHRU)){
    
    iHRU <- iHRU + 1
    #Get indices from full list that match the unique(ID)
    i <- which(HRU$Tidy.ID == nHRU)
    #For these indices, get all unique HRUtoCLIMmapping values
    HRUClimOverlaps <- HRUtoCLIMmapping[i]
    
    nTot <- length(HRUClimOverlaps)
    
    Overlaps <- unique(HRUClimOverlaps)#Loop over unique clim cells
    #print(paste("Total number of unique climate grid cells under HRU ",nHRU,"=",length(Overlaps),sep=""))
    for (c.ind in Overlaps){
      
      nOverlap <- length(which(HRUClimOverlaps == c.ind))
      
      w <- nOverlap/nTot
      
      ts <- CLIMfull[c.ind, ]
      
      if (any(is.na(ts))){
        stop("Found na in climate data time series.")
      }
      CLIMts[iHRU,]=CLIMts[iHRU,]+ ts * w
    }
  }
}

close(pb)

print("...writing HRU climate file to netCDF...")

dim1 <- ncdim_def("HRU", units="HRU", HRUlist, longname="HRU_ID")

dim2 <- ncdim_def("time", units="days since 1950-01-01 00:00:00", time, longname="days since 1950-01-01 00:00:00")

if(Var == "pr"){
  HRU_def <- ncvar_def(Var, "mm", list(dim1,dim2), prec="float")
} else {
  HRU_def <- ncvar_def(Var, "deg_C", list(dim1,dim2), prec="float")
}


ncout <- nc_create(file.path(output.dir, paste(Var, ".HRU.timeseries.V1.nc", sep="")), list(HRU_def), force_v4=T)

ncvar_put(ncout,HRU_def,CLIMts)

ncatt_put(ncout,"time","calendar","gregorian")

nc_close(ncout)

print("Done!")
