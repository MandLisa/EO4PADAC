# Landsat search
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level1-csd -u /path/metadata/

  
  
# Landsat search
docker run \
  -v $basepath/EO4Alps:/path \
  --user "$(id -u):10000514" \
  --memory 128GB \
  --env FORCE_CREDENTIALS=/app/credentials \
  -v $HOME:/app/credentials davidfrantz/force \
  force-level1-csd -n -c 0,70 -d 19860101,20231231 -s S2A,S2B,LT04,LT05,LE07,LC08 /path/metadata /path/level1 /path/level1/l1_pool_all.txt /path/gis/AOI_alps.gpkg

