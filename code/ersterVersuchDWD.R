#Erste Versuche mit den Wetterdaten des DWD
#https://bookdown.org/brry/rdwd/raster-data.html
library(rdwd)
library(R.utils)
library(terra)
library(ncdf4)
library(dwdradar)
library(RCurl)

#Jahreszeitenmittel der Raster der monatlich gemittelten Lufttemperatur (2m) für Deutschland
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/seasonal/air_temperature_mean/16_DJF/
link <- "seasonal/air_temperature_mean/16_DJF/grids_germany_seasonal_air_temp_mean_188216.asc.gz"
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # with dividebyten=TRUE
plotRadar(rad, main=".raster", proj="seasonal", extent=NULL)

#Tagesmittel der stündlichen Raster des Luftdrucks in Meereshöhe für Deutschland---------
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/Project_TRY/pressure/
link <- "daily/Project_TRY/pressure/PRED_199501_daymean.nc.gz"
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=1)

#jetzt Analysen radarbasierter stündlicher (RW) Niederschlagshöhen----------------------
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/reproc/2017_002/bin/2023/ 
link <- "hourly/radolan/reproc/2017_002/bin/2023/RW2017.002_202301.tar.gz"
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file, exdir=tempdir(), selection=1:3)
plotRadar(rad$dat, main=".binary RW", extent="rw", layer=1)

#jetzt Analysen radarbasierter täglicher (SF) Niederschlagshöhen------------------------
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/historical/bin/2022/ 
link <- "daily/radolan/historical/bin/2022/SF202211.tar.gz"
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file, exdir=tempdir(), selection=1:3) # with toraster=TRUE
plotRadar(rad$dat, main=".binary SF", layer=1)

#Historische stündliche RADOLAN-Raster der Niederschlagshöhe (GIS-lesbar)---------------
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/historical/asc/
link <- "hourly/radolan/historical/asc/2022/RW-202211.tar"
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE) # dbin -> mode=wb
rad <- readDWD(file, selection=1:3, dividebyten=TRUE)
plotRadar(rad, main=".asc", layer=1)

#RADVOR (Radar-Niederschlagsvorhersage)-------------------------------------------------
rqbase <- "ftp://opendata.dwd.de/weather/radar/radvor/rq"
links <- indexFTP("", base=rqbase, dir=tempdir()) # 0.07 MB
file <- dataDWD(links[1], base=rqbase, joinbf=TRUE, dir=tempdir(), read=FALSE)
rad <- readDWD(file)
plotRadar(rad$dat, main=".radar RQ")

#numerical weather prediction (NWP) Wettervorhersage------------------------------------
nwpbase <- "ftp://opendata.dwd.de/weather/nwp/icon-d2/grib/00/t_2m"
links <- indexFTP("", base=nwpbase, dir=tempdir())
file <- dataDWD(links[1], base=nwpbase, joinbf=TRUE, dir=tempdir(), read=FALSE)
forecast <- readDWD(file)
plotRadar(forecast, main=".grib2", project=FALSE)
