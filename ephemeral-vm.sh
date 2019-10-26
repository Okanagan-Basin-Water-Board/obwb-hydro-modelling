#!/bin/bash
user=birdl
runName=whiteman-ephemeral-1
vmName=$runName
zone=northamerica-northeast1-a
WD=$PWD
gcloud compute instances create $vmName --zone=$zone --boot-disk-size=50GB --boot-disk-type=pd-ssd --boot-disk-device-name=$vmName --custom-extensions --custom-cpu=8 --custom-memory=100GB --image=raven-runner-template-mpi
sleep 5s
gcloud compute scp --compress --zone=$zone --recurse /var/obwb-hydro-modelling/simulations/ephemeral-tests/ephemeral-tests-whiteman-ephemeral-1 $user@$vmName:/var/raven
gcloud compute ssh $user@$vmName --zone=$zone --command='cd /var/raven/ephemeral-tests-whiteman-ephemeral-1'
gcloud compute ssh $user@$vmName --zone=$zone --command='/usr/bin/mpirun -n 3 OstrichMPI'
