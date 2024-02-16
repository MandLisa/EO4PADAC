#!/bin/bash

################################################################################
# Script Name:     run_FORCE_alps.sh
# Description:     This script executes all FORCE sub-programs
# Author:          Lisa Mandl
# Created Date:    November 2, 2022
# Last Modified:   February 13, 2024
# Version:         3.2.5
################################################################################

#-------------------------------------------------------------------------------
### Step 1: Create a gsutil config file and sign in with your google account
#-------------------------------------------------------------------------------
gsutil config -f
gcloud auth login 

#-------------------------------------------------------------------------------
### define base path
basepath=/data/eo/

#-------------------------------------------------------------------------------
### Step 3: Search and download S2 data
#-------------------------------------------------------------------------------
# here with -n (no action) setting
docker run \
-v $basepath/EO4Alps:/path \
--user "$(id -u):10000514" \
--env FORCE_CREDENTIALS=/app/credentials \
-v $HOME:/app/credentials davidfrantz/force \
force-level1-csd -n -k -c 0,60 -d 20170101,20231231 -s S2A,S2B /path/metadata /path/level1 /path/level1/tile_pool_EU.txt /path/gis/AOI_EU.gpkg




