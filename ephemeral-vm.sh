#!/bin/bash
user=birdl
runName=oct-21-ephemeral-workflow
vmName=$runName
zone=northamerica-northeast1-a
WD=$PWD
gcloud compute instances create $vmName --zone=$zone --boot-disk-size=50GB --boot-disk-type=pd-ssd --boot-disk-device-name=$vmName --custom-cpu=8 --custom-memory=250GB --image=raven-runner-template-mpi
sleep 5s
gcloud compute scp --compress --zone=$zone --recurse /var/obwb-hydro-modelling/simulations/ephemeral-workflow-tests/ephemeral-workflow-tests-oct-21-ephemeral-workflow $user@$vmName:/var/raven
gcloud compute ssh $user@$vmName --zone=$zone --command='cd /var/raven/ephemeral-workflow-tests-oct-21-ephemeral-workflow'
gcloud compute ssh $user@$vmName --zone=$zone --command='/usr/bin/mpirun -n 7 OstrichMPI'
