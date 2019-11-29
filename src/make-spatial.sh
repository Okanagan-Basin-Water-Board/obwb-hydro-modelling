#!/bin/bash

echo "Ensure that soil_type.tif has been generated from Dan"

sleep 10

echo ------------ Generating HRUs --------------------------------
Rscript hru-generation/hru-generator.R
echo ------------ Done generating HRUs ---------------------------

echo ----------- Reprocessing LAI information -------------------
Rscript hru-generation/lai-processing.R
echo ----------- Done processing LAI information ---------------

echo ------------- Writing new Master.rvh file ------------------
Rscript raven-input-files/rvh-filegenerator.R
echo --------------Master.rvh has been rewritten ----------------

echo ------------ Running climate grid weighting scripts-----------
Rscript climate-processing/calculate_hru_weights.r tasmin
Rscript climate-processing/calculate_hru_weights.r tasmax
Rscript climate-processing/calculate_hru_weights.r pr
echo ------------ Done remapping all climate data -----------------

echo ------------ Remapping snow station locations to HRUs --------
Rscript snow-processing/snow-spatial-analysis.R
echo ------------ Done remapping snow station locations -----------

echo ------------ All spatial information has been successfully updated ---------
