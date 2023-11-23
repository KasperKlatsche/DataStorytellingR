#Erste Versuche mit den Wetterdaten des DWD
#https://bookdown.org/brry/rdwd/raster-data.html
library(rdwd)
library(R.utils)
library(terra)
library(ncdf4)

#erstmal Raster daten---------------
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/seasonal/air_temperature_mean/16_DJF/
link <- "seasonal/air_temperature_mean/16_DJF/grids_germany_seasonal_air_temp_mean_188216.asc.gz"
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # with dividebyten=TRUE
rad <- readDWD(file) # runs faster at second time due to skip=TRUE
plotRadar(rad, main=".raster", proj="seasonal", extent=NULL)

#jetzt Druck daten--------------------
#https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/Project_TRY/pressure/
link <- "daily/Project_TRY/pressure/PRED_199501_daymean.nc.gz"
file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
rad <- readDWD(file) # can also have interactive selection of variable
plotRadar(rad, main=".nc", proj="nc", extent="nc", layer=1)