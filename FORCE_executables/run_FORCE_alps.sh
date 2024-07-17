#!/bin/bash

################################################################################
# Script Name:     run_FORCE_alps.sh
# Description:     This script executes all FORCE sub-programs
# Author:          Lisa Mandl
# Created Date:    November 2, 2022
# Last Modified:   May 31, 2024
# Version:         3.2.6
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
  
# Export tiles as grid; either as KML or shp, format: bottom top left right
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-tabulate-grid -b 45,48,9,15 -f shp /path/level2
  
#-------------------------------------------------------------------------------
### Step 6b: Coregistration
### this step is only needed when using Landsat AND Sentinel-2 data!

# when you see that level 2 data looks fine, consider deleting level 1 data

#rm -rf $basepath/EO4Alps/level1/

#-------------------------------------------------------------------------------
### Step 7: Level 3 processing
#-------------------------------------------------------------------------------

# Compute spectral-temporal-metrics from Level 2 data using the setting given
# in param file
docker run \
  -v $basepath/EO4Alps:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_l3_STM_alps.prm
  
# compute a mosaic (if you want to do so...)  
docker run \
  -v $basepath/EO4Alps:/path 
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
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/sampling_l3_new.prm
  
# extract NDVI time serie
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/sampling_l3_NDVI.prm
  
  
### Create parameter files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-parameter /path/EO4PADAC/param_files/param_synthmix_l3.prm SYNTHMIX
  

### run synthmix
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-synthmix /path/EO4PADAC/param_files/param_synthmix_l2.prm

#-------------------------------------------------------------------------------
### Step 8: Model training
#-------------------------------------------------------------------------------

### Create trianing files (5 per end member)
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-magic-parameters -o /path/EO4PADAC/param_files/train_param_l3 /path/EO4PADAC/param_files/train_SVM_l3.prm
  
# train 5 models per endmember by calling all 40 (5*8) parameter files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00001.prm

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00002.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00003.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00004.prm

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00005.prm

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00006.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00007.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00008.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00009.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00010.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00011.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00012.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00013.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00014.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00015.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00016.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00017.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00018.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00019.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00020.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00021.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00022.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00023.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00024.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00025.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00026.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00027.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00028.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00029.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00030.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00031.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00032.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00033.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00034.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00035.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00036.prm

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00037.prm

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00038.prm

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00039.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_param_l3/train_SVM_l3_00040.prm
  
  
  
#for f in /path/projects/foreco/alps/param/train_param/*.prm; do dforce force-train $f; done

#-------------------------------------------------------------------------------
### Step 9: Apply all previously trained models
#-------------------------------------------------------------------------------

### Run
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/prediction_l3_test.prm
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-magic-parameters -o /path/EO4PADAC/param_files/predictions_l3 /path/EO4PADAC/param_files/predictions_l31.prm
  
  
  
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
  

