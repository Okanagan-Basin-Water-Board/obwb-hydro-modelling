# #################################################################################################################
# ##
# ## This script reads in Raven model output and generates summary plots
# ##
# ## Mar-11-2019 LAB
# ##
# #################################################################################################################
# 
# library(RavenR)
# 
# location <- "/var/obwb-hydro-modelling/simulations/Whiteman"
# 
# setwd(location)
# 
# runname <- "Whiteman-4"
# 
# 
# ######################################################
# ##
# ## Establish "outflow-plots.pdf" connection to write plots to.
# 
# pdf(file.path(location, runname, "output-plots.pdf"), width = 11, height = 8.5)
# 
# 
# ######################################################
# ##
# ## Read-in and plot forcings data
# 
# forcings <- forcings.read(file.path(location, runname, paste(runname, "ForcingFunctions.csv", sep = "_")))
# 
# forcings.plot(forcings$forcings)
# 
# ######################################################
# ##
# ## Read-in and plot hydrograph data
# 
# hydrograph.2 <- hyd.read(file.path(location, runname, paste(runname, "Hydrographs.csv", sep = "_")))
# 
# subs <- colnames(hydrograph$hyd)
# 
# for(i in 2:length(subs)){
#   
#   ## If 80% of values are NA, do not plot this timeseries
#   if(colMeans(is.na(hydrograph$hyd[,i])) > 0.8){
#     
#     print(paste(subs[i], "not plotted due to insufficient data (i.e., >80% NA values)...", sep = " "))
#     
#     i <- i+1
#     
#     }
#   
#   tmp <- hyd.extract(subs = subs[i], hydrograph)
#   
#   hyd.plot(tmp$sim, tmp$obs, precip = hydrograph$hyd$precip)
# 
#   title(subs[i])
# }
# 
# ######################################################
# ##
# ## Close pdf connection with "output-plots.pdf"
# 
# dev.off()
# 
# 
# 
# plot(hydrograph.1$hyd$Whiteman_Creek7, type = "l", col = 'red')
# lines(hydrograph.2$hyd$Whiteman_Creek7, col = "blue")
# 
# lines(hydrograph.1$hyd$Whiteman_Creek7_obs, col = 'black')
# 
# plot(hydrograph.2$hyd$Whiteman_Creek7, type = "l", col = "blue")
# lines(hydrograph.2$hyd$Whiteman_Creek7_obs, col = 'red', lwd = 2)
# 


###################################################################
##
## Checkout some plots of reservoirs
##
###################################################################

hydrographs.natural <- read.csv("/var/obwb-hydro-modelling/simulations/Reservoir-Demand-Build/Reservoir-Demand-Build-Oct-16-8/Reservoir-Demand-Build-Oct-16-8_Hydrographs.csv")
hydrographs.unmanaged.demand <- read.csv("/var/obwb-hydro-modelling/simulations/Reservoir-Demand-Build/Reservoir-Demand-Build-Oct-16-9/Reservoir-Demand-Build-Oct-16-9_Hydrographs.csv")
hydrographs.auto.reservoirs <- read.csv("/var/obwb-hydro-modelling/simulations/Reservoir-Demand-Build/Reservoir-Demand-Build-Oct-16-10/Reservoir-Demand-Build-Oct-16-10_Hydrographs.csv")
hydrographs.forced.reservoirs <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_Hydrographs.csv", sep = "")))

MC230.out <- data.frame(tiso = seq(as.Date("1994-01-01"), as.Date("2000-12-31"), by = "day"),
                   natural = hydrographs.natural$Mission_Creek230..m3.s.,
                   unmanaged.demand = hydrographs.unmanaged.demand$Mission_Creek230..m3.s.,
                   auto.reservoirs = hydrographs.auto.reservoirs$Mission_Creek230..m3.s.,
                   forced.reservoirs = hydrographs.forced.reservoirs$Mission_Creek230..m3.s.)


MC228.out <- data.frame(tiso = seq(as.Date("1994-01-01"), as.Date("2000-12-31"), by = "day"),
                        natural = hydrographs.natural$Mission_Creek228..m3.s.,
                        unmanaged.demand = hydrographs.unmanaged.demand$Mission_Creek228..m3.s.,
                        auto.reservoirs = hydrographs.auto.reservoirs$Mission_Creek228..m3.s.,
                        forced.reservoirs = hydrographs.forced.reservoirs$Mission_Creek228..m3.s.)



plot(MC230.out$tiso, MC230.out$natural, type = 'l',
     ylim = c(0, 0.5),
     ylab = "Loch Long (MC230) Outflow")
lines(MC230.out$tiso, MC230.out$unmanaged.demand, col = 'orange')
lines(MC230.out$tiso, MC230.out$auto.reservoirs, col = 'blue')
lines(MC230.out$tiso, MC230.out$forced.reservoirs, col = 'red')


legend("topright", legend = c("natural", "unmanaged demand", "auto reservoir management", "all demand from subbasin 230"), col = c("black", "orange", "blue", "red"), lty = 1, bty = "n")



plot(MC228.out$tiso, MC228.out$natural, type = 'l',
     ylim = c(0, 1),
     ylab = "Loch Long (MC228) Outflow")
lines(MC228.out$tiso, MC228.out$unmanaged.demand, col = 'orange')
lines(MC228.out$tiso, MC228.out$auto.reservoirs, col = 'blue')
lines(MC228.out$tiso, MC228.out$forced.reservoirs, col = 'red')


legend("topright", legend = c("natural", "unmanaged demand", "auto reservoir management", "all demand from subbasin 228"), col = c("black", "orange", "blue", "red"), lty = 1, bty = "n")

#################
##
## Read in all water demand for Mission Creek
##
#################

owdm.files <- list.files("/var/obwb-hydro-modelling/simulations/Reservoir-Demand-Build/Reservoir-Demand-Build-Oct-16-9/owdm", full.names = TRUE)

owdm.data <- matrix(NA, nrow = 8766, ncol = 0)

for(i in 1:length(owdm.files)){
  
  wd <- read.table(owdm.files[i], skip = 2, nrows = 8766)
  
  owdm.data <- cbind(owdm.data, wd)
  
}

owdm.data$tiso <- seq(as.Date("1994-01-01"), as.Date("2017-12-31"), by = "day")

owdm.data$total.demand <- rowSums(owdm.data[,1:length(owdm.files)])

owdm.data$total.demand[owdm.data$tiso < as.Date("1996-01-01")] <- 0

plot(owdm.data$tiso[owdm.data$tiso <= as.Date("2000-12-31")], owdm.data$total.demand[owdm.data$tiso <= as.Date("2000-12-31")], type = 'l',
     ylab = "Mission Creek Total Water Demand")


#################
##
## Read in Reservoir Stage data.
##
#################

stage.natural <- read.csv("/var/obwb-hydro-modelling/simulations/Reservoir-Demand-Build/Reservoir-Demand-Build-Oct-16-8/Reservoir-Demand-Build-Oct-16-8_ReservoirStages.csv")
stage.unmanaged.demand <- read.csv("/var/obwb-hydro-modelling/simulations/Reservoir-Demand-Build/Reservoir-Demand-Build-Oct-16-9/Reservoir-Demand-Build-Oct-16-9_ReservoirStages.csv")
stage.auto.reservoirs <- read.csv("/var/obwb-hydro-modelling/simulations/Reservoir-Demand-Build/Reservoir-Demand-Build-Oct-16-10/Reservoir-Demand-Build-Oct-16-10_ReservoirStages.csv")
stage.forced.reservoirs <- read.csv(file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-"), paste(ws.interest, "-", run.number, "_ReservoirStages.csv", sep = "")))

MC230.stage <- data.frame(tiso = seq(as.Date("1994-01-01"), as.Date("2000-12-31"), by = "day"),
                        natural = stage.natural$Mission_Creek230,
                        unmanaged.demand = stage.unmanaged.demand$Mission_Creek230,
                        auto.reservoirs = stage.auto.reservoirs$Mission_Creek230,
                        forced.reservoirs = stage.forced.reservoirs$Mission_Creek230)



MC228.stage <- data.frame(tiso = seq(as.Date("1994-01-01"), as.Date("2000-12-31"), by = "day"),
                          natural = stage.natural$Mission_Creek228,
                          unmanaged.demand = stage.unmanaged.demand$Mission_Creek228,
                          auto.reservoirs = stage.auto.reservoirs$Mission_Creek228,
                          forced.reservoirs = stage.forced.reservoirs$Mission_Creek228)



plot(MC230.stage$tiso, MC230.stage$natural, type = 'l',
     ylim = c(min(MC230.stage$forced.reservoirs),1864),
     ylab = "Loch Long (MC230) Reservoir Stage")
lines(MC230.stage$tiso, MC230.stage$unmanaged.demand, col = 'orange')
lines(MC230.stage$tiso, MC230.stage$auto.reservoirs, col = 'blue')
lines(MC230.stage$tiso, MC230.stage$forced.reservoirs, col = 'red')

abline(h = min(MC230.stage$forced.reservoirs), col = 'grey', lty = 3)
## plot abline at absolute crest height
abline(h= 1863.852, col = 'grey', lty = 3)



plot(MC228.stage$tiso, MC228.stage$natural, type = 'l',
     ylim = c(min(MC228.stage$forced.reservoirs), 1299),
     ylab = "Ideal (MC228) Reservoir Stage")
lines(MC228.stage$tiso, MC228.stage$unmanaged.demand, col = 'orange')
lines(MC228.stage$tiso, MC228.stage$auto.reservoirs, col = 'blue')
lines(MC228.stage$tiso, MC228.stage$forced.reservoirs, col = 'red')

abline(h = min(MC228.stage$forced.reservoirs), col = 'grey', lty = 3)
## plot abline at absolute crest height
abline(h= 1298.143, col = 'grey', lty = 3)


#################
##
## Create a mixed plot displaying outflow, reservoir levels, and total water demand
##
#################

plot(MC230.out$tiso, MC230.out$forced.reservoirs, type = 'l',
     xlab = "Year",
     ylab = "")
par(new = T)
plot(MC230.stage$tiso, MC230.stage$forced.reservoirs, axes = F, type = 'l', col = 'red',
     xlab = "",
     ylab = "")
## plot abline at absolute crest height
abline(h= 1863.852, col = 'grey', lty = 3)
par(new = T)
plot(owdm.data$tiso[owdm.data$tiso <= as.Date("2000-12-31")], owdm.data$total.demand[owdm.data$tiso <= as.Date("2000-12-31")], col = 'blue', type = 'l', axes = F,
     xlab = "",
     ylab = "")



plot(MC228.out$tiso, MC228.out$forced.reservoirs, type = 'l',
     xlab = "Year",
     ylab = "")
par(new = T)
plot(MC228.stage$tiso, MC228.stage$forced.reservoirs, axes = F, type = 'l', col = 'red',
     xlab = "",
     ylab = "")
## plot abline at absolute crest height
abline(h = 1298.143, col = 'grey', lty = 3)
par(new = T)
plot(owdm.data$tiso[owdm.data$tiso <= as.Date("2000-12-31")], owdm.data$total.demand[owdm.data$tiso <= as.Date("2000-12-31")], col = 'blue', type = 'l', axes = F,
     xlab = "",
     ylab = "")

