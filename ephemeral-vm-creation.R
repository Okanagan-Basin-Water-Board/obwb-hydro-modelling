################################################################################################################
##
## This file creates an ephemeral VM for model calibration
##
################################################################################################################


ephemeral.vm <- file.path("/var/obwb-hydro-modelling/", "ephemeral-vm.sh")

cat(file = ephemeral.vm, append = F, sep = "",
    "#!/bin/bash", "\n",
    "user=", Sys.getenv("LOGNAME"), "\n",
    "runName=", run.number, "\n",
    "vmName=$runName", "\n",
    "zone=northamerica-northeast1-a", "\n",
    "WD=$PWD", "\n",
    # 
    # ## Create ephemeral VM
    "gcloud compute instances create $vmName --zone=$zone --boot-disk-size=50GB --boot-disk-type=pd-ssd --boot-disk-device-name=$vmName --custom-extensions --custom-cpu=8 --custom-memory=100GB --image=raven-runner-template-mpi", "\n",
    # 
    # ## Let machine settle
    "sleep 5s", "\n",
    # 
    # ## Copy all files from initial Raven run for given scenario
    "gcloud compute scp --compress --zone=$zone --recurse ", file.path("/var/obwb-hydro-modelling/simulations", ws.interest, paste(ws.interest, run.number, sep = "-")),  " $user@$vmName:/var/raven", "\n",
    # 
    
   # "gcloud compute ssh $user@$vmName --zone=$zone --command='sudo apt-get install openmpi-bin'", "\n",
    
    ## change into the directory with the model files
    "gcloud compute ssh $user@$vmName --zone=$zone --command=", paste("'cd /var/raven/", paste(ws.interest, run.number, sep = "-"),"'", sep = ""), "\n",
    
    ## Execute Ostrich
    "gcloud compute ssh $user@$vmName --zone=$zone --command=", "'/usr/bin/mpirun -n ", 3, " OstrichMPI'", "\n"
    
    #copy results of raven/ostrich back
    ######3
    
    
    #eVM destroys itself
    #"gcloud compute instances stop $vmName --zone=$zone", "\n",
    #"gcloud compute instances delete $vmName --zone=$zone --quiet"
    
)

