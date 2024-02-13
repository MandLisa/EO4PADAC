#!/bin/bash

################################################################################
# Script Name:     run_FORCE_alps.sh
# Description:     This script executes all FORCE sub-programs
# Author:          Lisa Mandl
# Created Date:    November 2, 2022
# Last Modified:   February 8, 2024
# Version:         3.2.5
################################################################################

#-------------------------------------------------------------------------------
### Step 1: Create a gsutil config file and sign in with your google account
#-------------------------------------------------------------------------------
gsutil config -f
gcloud auth login 

#-------------------------------------------------------------------------------
### Step 2: Define start and end date as well as max. cloud cover
#-------------------------------------------------------------------------------

startdate=0101
enddate=1231
maxcloud=60

# define base path
basepath=/data/eo/
#basepath=/data/public/Projects/

#-------------------------------------------------------------------------------
### Step 3: Update metadata catalogue, contains now all Landsat/Sentinel-2 images
#-------------------------------------------------------------------------------

### from 1986 to 2023
docker run \
  -v $basepath/datacube:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level1-csd -u /path/metadata/

#-------------------------------------------------------------------------------
### Step 4: Search for all available Landsat scenes + download
#-------------------------------------------------------------------------------

# Landsat search
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level1-landsat search /path/gis/AOI_alps.gpkg /path/level1 -s TM,ETM,OLI 
  -d 19860101,20231231 -c 0,60 --secret /path/lib/m2m_new.txt

# Landsat download
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force:3.7.11 \
  force-level1-landsat download /path/level1/urls_landsat_TM_ETM_OLI_missing.txt /path/level1 
  
#-------------------------------------------------------------------------------
### Step 5: de-tar Copernicus DEM
#-------------------------------------------------------------------------------
tar -xvf /data/eo/EO4Alps/dem/Copernicus_DSM_10_N29_00_E014_00.tar

unzip /data/public/Projects/DataCube/projects/foreco/alps/dem/032ab314564b9cb72c98fbeb093aeaf69720fbfd.zip -d .

#-------------------------------------------------------------------------------
### Step 6a: Level 2 processing
#-------------------------------------------------------------------------------

# run FORCE level 2 processing using the param file
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level2 /path/EO4PADAC/param_files/param_l2_alps.prm
  
# Create report based on log files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level2-report /path/log
  
#-------------------------------------------------------------------------------
### Step 6b: Coregistration
### this step is only needed when using Landsat AND Sentinel-2 data!

#-------------------------------------------------------------------------------
### Step 7: Level 3 processing
#-------------------------------------------------------------------------------

# Compute spectral-temporal-metrics from Level 2 data using the setting given
# in param file
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_l3_STM_alps.prm
  
# compute a mosaic (if you want to do so...)  
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \  
  force-mosaic /path/projects/foreco/alps/level3/l3_STMs/1990
  
#-------------------------------------------------------------------------------
### Step 8: Sampling for creating synthetic training data
#-------------------------------------------------------------------------------
  
# Run sampling
# before, make sure you created a training data file (X, Y, class (csv))
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/sampling.prm
  
  
### Create parameter files
  docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-parameter /path/EO4PADAC/param_files/param_synthmix.prm SYNTHMIX
  

### run synthmix
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-synthmix /path/EO4PADAC/param_files/synth_mix.prm

#-------------------------------------------------------------------------------
### Step 8: Model training
#-------------------------------------------------------------------------------

### Create trianing files (5 per end member)
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-magic-parameters -o /path/projects/foreco/alps/param/train_param_v3_l2 /path/EO4PADAC/param_files/train_param_SVM.prm
  
# train 5 models per endmember by calling all 45 (5*9) parameter files
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_SVM.prm
  
#for f in /path/projects/foreco/alps/param/train_param/*.prm; do dforce force-train $f; done

#-------------------------------------------------------------------------------
### Step 9: Apply all previously trained models
#-------------------------------------------------------------------------------

### Run
docker run \
  -v $basepath/datacube:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/prediction_v3_proc_mask.prm
  
### apply all previously trained models
docker run \
  -v $basepath/datacube:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-higher-level /path/projects/alps/param/prediction_v3_l2_lorien_mask.prm
  
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-magic-parameters -o /path/projects/foreco/alps/param/prediction_param /path/projects/foreco/alps/param/prediction.prm
  
  
  
for f in /path/projects/foreco/alps/param/prediction_param/*.prm; do dforce force-train $f; done
### run in loop  
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  dforce force-train $f
  wait;
  done
  

