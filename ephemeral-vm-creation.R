################################################################################################################
##
## This file creates an ephemeral VM for model calibration
##
################################################################################################################

# 
# ephemeral.vm <- file.path("/var/obwb-hydro-modelling/", "ephemeral-vm.sh")
# 
# cat(file = ephemeral.vm, append = F, sep = "",
#     "#!/bin/bash", "\n",
#     "user=", Sys.getenv("LOGNAME"), "\n",
#     "runName=", run.number, "\n",
#     "vmName=$runName", "\n",
#     "zone=northamerica-northeast1-a", "\n",
#     "WD=$PWD", "\n",
#     #
#     # ## Create ephemeral VM
#     "gcloud compute instances create $vmName --zone=$zone --boot-disk-size=50GB --boot-disk-type=pd-ssd --boot-disk-device-name=$vmName --custom-extensions --custom-cpu=8 --custom-memory=100GB --image=raven-runner-template-mpi", "\n",
#     #
#     # ## Let machine settle
#     "sleep 5s", "\n",
#     #
#     # ## Copy all files from initial Raven run for given scenario
#     "gcloud compute scp --compress --zone=$zone --recurse ", file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")),  " $user@$vmName:/var/raven", "\n",
#     #
# 
#    # "gcloud compute ssh $user@$vmName --zone=$zone --command='sudo apt-get install openmpi-bin'", "\n",
# 
#     ## change into the directory with the model files
#     "gcloud compute ssh $user@$vmName --zone=$zone --command=", paste("'cd /var/raven/", paste(ws.interest, run.number, sep = "-"),"'", sep = ""), "\n",
# 
#     ## Execute Ostrich
#     "gcloud compute ssh $user@$vmName --zone=$zone --command=", "'/usr/bin/mpirun -n ", 3, " OstrichMPI'", "\n"
# 
#     #copy results of raven/ostrich back
#     ######3
# 
# 
#     #eVM destroys itself
#     #"gcloud compute instances stop $vmName --zone=$zone", "\n",
#     #"gcloud compute instances delete $vmName --zone=$zone --quiet"
# 
# )

# ## Create ephemeral VM
system2("gcloud", args = paste("compute instances create ", run.number, " --zone=northamerica-northeast1-a --boot-disk-size=100GB --boot-disk-type=pd-ssd --boot-disk-device-name=", run.number, " --custom-extensions --custom-cpu=8 --custom-memory=10GB --image=raven-ephemeral-vm", sep = ""))
# 
# ## Let machine settle
system2("sleep", args = "5s")

# system2("gcloud", args = paste("compute ssh ", Sys.getenv("LOGNAME"), "@", run.number, " --zone=northamerica-northeast1-a command='cd /var/raven/'", sep = ""))

# ## Create a directory to house the climate data
# system2("gcloud", args = paste("compute ssh ", Sys.getenv("LOGNAME"), "@", run.number, " --zone=northamerica-northeast1-a --command='mkdir /var/raven/climate-data'", sep = ""))

system2("gcloud", args = paste("compute scp --compress --zone=northamerica-northeast1-a --recurse ", file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")), " ", Sys.getenv("LOGNAME"), "@", run.number, ":/var/raven", sep = ""))

# system2("gcloud", args = paste("compute ssh ", Sys.getenv("LOGNAME"), "@", run.number, " --zone=northamerica-northeast1-a --command='cd /var/raven/", paste(ws.interest, run.number, sep = "-"), "'", sep = ""))

system2("gcloud", args = paste("compute ssh ", Sys.getenv("LOGNAME"), "@", run.number, " --zone=northamerica-northeast1-a --command='cd /var/raven/", paste(ws.interest, run.number, sep = "-"), ";/usr/bin/mpirun -n 7 /var/raven/", paste(ws.interest, run.number, sep = "-"), "/OstrichMPI'", sep = ""), wait = F, stdout = NULL)


# 
# ## Copy all files from initial Raven run for given scenario
# "gcloud compute scp --compress --zone=$zone --recurse ", file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")),  " $user@$vmName:/var/raven", "\n",
# 

# "gcloud compute ssh $user@$vmName --zone=$zone --command='sudo apt-get install openmpi-bin'", "\n",

## change into the directory with the model files
# "gcloud compute ssh $user@$vmName --zone=$zone --command=", paste("'cd /var/raven/", paste(ws.interest, run.number, sep = "-"),"'", sep = ""), "\n",

## Execute Ostrich
"gcloud compute ssh $user@$vmName --zone=$zone --command=", "'/usr/bin/mpirun -n ", 7, " OstrichMPI'", "\n"






