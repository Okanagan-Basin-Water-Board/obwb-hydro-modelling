#!/bin/bash
echo ------------ Running climate grid weighting scripts-----------
Rscript calculate_hru_weights.r tasmin
Rscript calculate_hru_weights.r tasmax
Rscript calculate_hru_weights.r pr
echo ------------ Everything is complete. Praise the lord.--------------
