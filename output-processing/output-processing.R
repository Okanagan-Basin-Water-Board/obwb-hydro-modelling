#################################################################################################################
##
## This script reads in Raven model output and generates summary plots
##
## Mar-11-2019 LAB
##
#################################################################################################################

library(RavenR)

location <- "/var/obwb-hydro-modelling/simulations/Whiteman"

setwd(location)

runname <- "Whiteman-4"


######################################################
##
## Establish "outflow-plots.pdf" connection to write plots to.

pdf(file.path(location, runname, "output-plots.pdf"), width = 11, height = 8.5)


######################################################
##
## Read-in and plot forcings data

forcings <- forcings.read(file.path(location, runname, paste(runname, "ForcingFunctions.csv", sep = "_")))

forcings.plot(forcings$forcings)

######################################################
##
## Read-in and plot hydrograph data

hydrograph.2 <- hyd.read(file.path(location, runname, paste(runname, "Hydrographs.csv", sep = "_")))

subs <- colnames(hydrograph$hyd)

for(i in 2:length(subs)){
  
  ## If 80% of values are NA, do not plot this timeseries
  if(colMeans(is.na(hydrograph$hyd[,i])) > 0.8){
    
    print(paste(subs[i], "not plotted due to insufficient data (i.e., >80% NA values)...", sep = " "))
    
    i <- i+1
    
    }
  
  tmp <- hyd.extract(subs = subs[i], hydrograph)
  
  hyd.plot(tmp$sim, tmp$obs, precip = hydrograph$hyd$precip)

  title(subs[i])
}

######################################################
##
## Close pdf connection with "output-plots.pdf"

dev.off()



plot(hydrograph.1$hyd$Whiteman_Creek7, type = "l", col = 'red')
lines(hydrograph.2$hyd$Whiteman_Creek7, col = "blue")

lines(hydrograph.1$hyd$Whiteman_Creek7_obs, col = 'black')

plot(hydrograph.2$hyd$Whiteman_Creek7, type = "l", col = "blue")
lines(hydrograph.2$hyd$Whiteman_Creek7_obs, col = 'red', lwd = 2)

