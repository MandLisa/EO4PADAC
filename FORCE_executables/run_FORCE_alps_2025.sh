#!/bin/bash

################################################################################
# Script Name:     run_FORCE_alps.sh
# Description:     This script executes all FORCE sub-programs
# Author:          Lisa Mandl
# Created Date:    November 2, 2022
# Last Modified:   August 8, 2025
# Version:         3.2.6
#################################################################################

#-------------------------------------------------------------------------------
### Step 1: Create a gsutil config file and sign in with your google account
#-------------------------------------------------------------------------------
gsutil config -a
gcloud auth login 

#-------------------------------------------------------------------------------
### Step 2: Define start and end date as well as max cloud cove
#-------------------------------------------------------------------------------

startdate=0101
enddate=1231
maxcloud=50

# define base path
basepath=/data/eo/
basepath=/home/lmandl/eo_nas/
basepath=/home/lmandl/
basepath=/home/
basepath=/mnt/eo/
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
  force-level1-landsat search /path/gis/AOI_alps.gpkg /path/level1 -s OLI -d 20120101,20161231 -c 0,60 --secret /path/lib/m2m_2025.txt


# Landsat download
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level1-landsat download /path/level1/urls_LC08.txt /path/level1/LC08 
  

# Sentinel-2 download
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level1-csd -c 0,50 -d 20150101,20231231 -s S2A,S2B /path/metadata /path/level1_S2 /path/level1_S2/l1_pool_S2.txt T31TFJ,T31TFK,T31TFL,T31TGJ,T31TGK,T31TGL,T31TGM,T32TLP,T32TLQ,T32TLR,T32TLS,T32TLT,T32TMP,T32TMQ,T32TMR,T32TMS,T32TMT,T32TNR,T32TNS,T32TNT,T32TPR,T32TPS,T32TPT,T32TQR,T32TQS,T32TQT,T32UNU,T32UPU,T32UQU,T33TUL,T33TUM,T33TUN,T33TVL,T33TVM,T33TVN,T33TWM,T33TWN,T33TXN,T33UUP,T33UVP,T33UWP,T33UXP
  
  
  
  #davidfrantz/force:3.7.11 \
  
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
  
# run FORCE level 2 processing using the param file
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level2 /path/EO4PADAC/param_files/param_l2_LC08.prm
  
  
# run FORCE level 2 processing for Sentinel data
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 256GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level2 /path/EO4PADAC/param_files/param_l2_S2.prm
  

docker run -it \
  -v $basepath/EO4Alps:/path \
  -v $HOME:/app/credentials \
  --user "$(id -u):10000514" \
  --env FORCE_CREDENTIALS=/app/credentials \
  davidfrantz/force \
  force-level2 /path/EO4PADAC/param_files/param_l2_S2.prm

  
#dforce force-level2 /home/lmandl/eo_nas/EO4Alps/EO4PADAC/param_files/param_l2_LC08_cmd.prm
  
# Create report based on log files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level2-report /path/log_S2
  
#dforce force-level2-report /home/lmandl/eo_nas/EO4Alps/log1
  
# Export tiles as grid; either as KML or shp, format: bottom top left right
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-tabulate-grid -b 44,49,4,15 -f shp /path/level2
  
#-------------------------------------------------------------------------------
### Step 6b: Coregistration
### this step is only needed when using Landsat AND Sentinel-2 data!

# when you see that level 2 data looks fine, consider deleting level 1 data

#rm -rf $basepath/EO4Alps/level1/

#-------------------------------------------------------------------------------
### Step 7: Level 3 processing
#-------------------------------------------------------------------------------

# # Compute spectral-temporal-metrics from Level 2 data using the setting given
# # in param file
basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2008_RBF.prm

basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2007_RBF.prm

basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2006_RBF.prm

basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2005_RBF.prm

basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2004_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2003_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2002_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2001_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2000_RBF.prm
  
#++++++

basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1999_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1998_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1997_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1996_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1995_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1994_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1993_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1992_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1991_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1990_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1989_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1988_RBF.prm


basepath=/mnt/eo/

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_1987_RBF.prm


basepath=/mnt/eo/


docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/param_STMs_LS_S2/param_STM_2022_RBF.prm



#dforce force-higher-level /home/lmandl/eo_nas/EO4Alps/EO4PADAC/param_files/param_STM_1203.prm
#force-higher-level /home/lmandl/eo_nas/EO4Alps/EO4PADAC/param_files/param_STM_1203.prm


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
  force-higher-level /path/EO4PADAC/param_files/sampling_Jun2025/sampling_l2_1986.prm

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/sampling_Jun2025/sampling_l2_1987.prm 

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/sampling_Jun2025/sampling_l2_1988.prm
  
  
  for year in {1987..2023}; do
  docker run \
    -v $basepath/EO4Alps:/path \
    --user "$(id -u):10000514" \
    --memory 128GB \
    --env FORCE_CREDENTIALS=/app/credentials \
    -v $HOME:/app/credentials \
    davidfrantz/force \
    force-higher-level /path/EO4PADAC/param_files/sampling_Jun2025/sampling_l2_${year}.prm
done



  
# extract NDVI time serie
#docker run \
  #-v $basepath/EO4Alps:/path \
  #--user "$(id -u):10000514" \
  #--memory 128GB \
  #--env FORCE_CREDENTIALS=/app/credentials \
  #-v $HOME:/app/credentials \
  #davidfrantz/force \
  #force-higher-level /path/EO4PADAC/param_files/sampling_l3_NDVI.prm
  
  
### Create parameter files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-parameter /path/EO4PADAC/param_files/param_synthmix_Jun25.prm SYNTHMIX
  

### run synthmix
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-synthmix /path/EO4PADAC/param_files/param_synthmix_Jun25.prm

#-------------------------------------------------------------------------------
### Step 8: Model training
#-------------------------------------------------------------------------------

basepath=/mnt/eo/

### Create trianing files (5 per end member)
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-magic-parameters -o /path/EO4PADAC/param_files/train_para_jun25_SVR /path/EO4PADAC/param_files/train_SVR_jun2025.prm
  
# train 5 models per endmember by calling all 40 (5*8) parameter files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-train /path/EO4PADAC/param_files/train_para_jun25_SVR/train_SVR_jun2025_00001.prm
  
### loop
for i in $(seq -f "%05g" 2 60); do
  docker run \
    -v $basepath/EO4Alps:/path \
    --user "$(id -u):10000514" \
    --memory 128GB \
    --env FORCE_CREDENTIALS=/app/credentials \
    -v $HOME:/app/credentials \
    davidfrantz/force \
    force-train /path/EO4PADAC/param_files/train_para_jun25_SVR/train_SVR_jun2025_${i}.prm
done



#-------------------------------------------------------------------------------
### Step 9: Apply all previously trained models
#-------------------------------------------------------------------------------
### Create parameter files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-parameter /path/EO4PADAC/param_files/param_predictions_jun25_v2.prm ML
  
basepath=/mnt/eo/

### Run
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 356GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/preds_jun25/prediction_2022_2.prm
  

### loop over years
for year in $(seq 2022 2022); do
  docker run \
    -v $basepath/EO4Alps:/path \
    --user "$(id -u):10000514" \
    --memory 350GB \
    --env FORCE_CREDENTIALS=/app/credentials \
    -v $HOME:/app/credentials \
    davidfrantz/force \
    force-higher-level /path/EO4PADAC/param_files/preds_jun25/prediction_${year}.prm
done




### Run
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2020.prm
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2019.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2018.prm


docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2017.prm
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2016.prm


docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2015.prm
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2014.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2013.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2012.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2011.prm
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2010.prm
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2009.prm
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2008.prm
  
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2007.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2006.prm
   
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2005.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2004.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2003.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2002.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2001.prm
  
  
  
 docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_2000.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1999.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1998.prm
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1997.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1996.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1995.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1994.prm
  
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1993.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1992.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1991.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1990.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1989.prm
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1988.prm
  
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1987.prm
  
  
  
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/EO4PADAC/param_files/predictions_l1_nov/prediction_l1_wtw_1986.prm
  
  
  

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
<<<<<<< HEAD
  force-higher-level /path/EO4PADAC/param_files/CSO_alps.prm
=======
  force-higher-level /path/EO4PADAC/param_files/CSO_alps_S2.prm
>>>>>>> 276de7e (add scripts)
  
  
  

### Create parameter files
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-parameter /path/EO4PADAC/param_files/preds_jun25/prediction_2022_1.prm ML

  
  


