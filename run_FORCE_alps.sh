
### Run FORCE for one tile ###

# create a gsutil config file and sign in with your google account
gsutil config -f
gcloud auth login 


### Settings

startdate=0101 
enddate=1231
maxcloud=60 #try
#basepath=/mnt/public/Projects/ ### this is the basepath for amonsul
#basepath=/data/public/Projects/ ### this is the basepath for Lorien
basepath=/data/eo/

### update metadata catalogue
docker run \
  -v $basepath/datacube:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level1-csd -u /path/metadata/

#-------------------------------------------------------------------------------
### landsat search
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level1-landsat search /path/gis/AOI_alps.gpkg /path/level1 -s TM,ETM,OLI -d 19860101,20231231 -c 0,60 --secret /path/lib/m2m_new.txt


### Landsat download
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force:3.7.11 \
  force-level1-landsat download /path/level1/urls_landsat_TM_ETM_OLI_missing.txt /path/level1 
  
### de-tar Copernicus DEM
tar -xvf /data/eo/EO4Alps/dem/Copernicus_DSM_10_N29_00_E014_00.tar

unzip /data/public/Projects/DataCube/projects/foreco/alps/dem/032ab314564b9cb72c98fbeb093aeaf69720fbfd.zip -d .


#-------------------------------------------------------------------------------
### level 2 processing
#-------------------------------------------------------------------------------

# run FORCE for the entire Alps

docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level2 /path/param/param_l2_alps.prm
  

  
### Create report LS
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-level2-report /path/log
  

### Process landsat 1997er data to leel 2
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level2 /path/projects/foreco/alps/param/param_l2_alps_ls1997.prm
  
  
###
### Coregistration
###

### search for all log files, that do not contain the string "Skip"
### his is the new tile pool
### sink writes the console output to the previously defined file

### list files
fileNames <- list.files(path = "F:/Projects/DataCube/projects/foreco/alps//temp_trash/log_coreg_II/")

### scam through list and only select files where !Skip.
### write all files in an empty txt file

sink(file = "C:/Users/ge45vaz/Documents/local/log_II.txt", append = T)
for (fileName in fileNames) {
  if (length(grep("Skip", invert = TRUE, readLines(fileName))) > 0) { print(fileName)}
}
sink()


  
### create html report
# create report for BGD datacube
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level2-report /path/projects/foreco/alps/log
  
  
### 
### Level 3 processing
###
basepath=/data/public/Projects/ ### this is the basepath for Lorien
basepath=/mnt/public/Projects/ ### this is the basepath for amonsul


docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/projects/foreco/alps/param/param_STMs_X3_Y-1/param_l3_STM_2021.prm
  
  
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \  
  force-mosaic /path/projects/foreco/alps/level3/l3_STMs/1990
  
  
### Level 3 sampling
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials \
  davidfrantz/force \
  force-higher-level /path/projects/foreco/alps/param/sampling_v21.prm
  
  
### Create parameter files
  docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-parameter /path/projects/foreco/alps/param/synth_mix.prm SYNTHMIX
  

### run synthmix
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-synthmix /path/projects/foreco/alps/param/synth_mix_l2.prm
  

### Model training
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-magic-parameters -o /path/projects/foreco/alps/param/train_param_v3_l2 /path/projects/foreco/alps/param/train_SVM_v3_l2.prm
  


### train 15 models by calling all 15 parameter files
docker run \
  -v $basepath/datacube:/path 
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-train /path/projects/foreco/alps/param/train_param_v3_l2/train_SVM_v3_l2_00025.prm
  
#for f in /path/projects/foreco/alps/param/train_param/*.prm; do dforce force-train $f; done



### apply all previously trained models
docker run \
  -v $basepath/datacube:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-higher-level /path/projects/foreco/alps/param/prediction_v3_proc_mask.prm
  
basepath=/data/eo/
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
  

