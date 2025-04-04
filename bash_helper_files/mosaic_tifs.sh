#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1986"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1986.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 1987
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1987"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1987.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1988
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1988"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1988.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1989
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1989"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1989.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1990
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1990"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1990.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1991
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1991"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1991.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1992
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1992"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1992.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1993
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1993"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1993.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1994
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1994"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1994.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1995
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1995"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1995.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1996
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1996"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1996.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1997
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1997"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1997.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1998
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1998"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1998.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 1999
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/1999"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_1999.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2000
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2000"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2000.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 2001
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2001"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2001.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2002
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2002"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2002.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2003
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2003"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2003.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2004
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2004"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2004.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2005
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2005"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2005.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2006
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2006"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2006.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 2007
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2007"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2007.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 2008
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2008"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2008.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2009
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2009"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2009.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2010
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2010"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2010.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 2011
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2011"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2011.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 2012
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2012"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2012.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2013
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2013"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2013.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 2014
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2014"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2014.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2015
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2015"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2015.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"



### 2016
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2016"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2016.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2017
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2017"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2017.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2018
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2018"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2018.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2019
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2019"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2019.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2020
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2020"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2020.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2021
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2021"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2021.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2022
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2022"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2022.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


### 2023
#!/bin/bash

parent_directory="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/2023"
#parent_directory="$HOME/eo_nas/EO4Alps/level3_STMs/mosaic"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/mosaic_2023.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"




