#Erste Versuche mit den Wetterdaten des DWD
#https://bookdown.org/brry/rdwd/raster-data.html
library(rdwd)
library(R.utils)
library(terra)
library(ncdf4)
library(dwdradar)
library(RCurl)

#DWD muss wissen wo es die Daten zwischenspeichern darf
setwd("./data")

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

#10 Zeitreihe für Luftdruck----------------------------------------------------------------
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
clim <- dataDWD(link, force=NA, varnames=TRUE)
str(clim)
par(mar=c(4,4,2,0.5), mgp=c(2.7, 0.8, 0), cex=0.8)
plot(clim[,c(2,14)], type="l", xaxt="n", las=1, main="Daily temp Potsdam")
berryFunctions::monthAxis()   ;   abline(h=0)
mtext("Source: Deutscher Wetterdienst", adj=-0.1, line=0.5, font=3)

#11 Langzeit Klimagraph--------------------------------------------------------------------
link <- selectDWD("Goettingen", res="monthly", var="kl", per="h")
clim <- dataDWD(link)
clim$month <- substr(clim$MESS_DATUM_BEGINN,5,6)
temp <- tapply(clim$MO_TT, clim$month, mean, na.rm=TRUE)
prec <- tapply(clim$MO_RR, clim$month, mean, na.rm=TRUE)
berryFunctions::climateGraph(temp, prec, main="Goettingen")
mtext("Source: Deutscher Wetterdienst", adj=-0.05, line=2.8, font=3)

#12 Historische und kürzliche Daten zusammenführen-----------------------------------------
links <- selectDWD("Potsdam", res="daily", var="kl", per="hr")
klima <- dataDWD(links, hr=4)
plot(TMK~MESS_DATUM, data=tail(klima,1500), type="l")
links <- selectDWD("Celle", res="daily", var="kl", per="hr")
klima <- dataDWD(links, hr=4, varnames=TRUE)
plotDWD(tail(klima,800), "PM.Luftdruck")

#13 Vektorisierte Daten -------------------------------------------------------------------
ids <-  c(3988, 5559, 2456, 3034, 1964, 4549, 2950, 5419, 2641, 3565)
links <- selectDWD(id=ids, res="daily", var="weather_phenomena", per="h")
phen <- dataDWD(links)
names(phen) <- substr(names(phen), 54,58) # only IDs (not paths) as name
str(phen, max.level=1)

#14 Monatlicher Niederschlag Deutschland---------------------------------------------------
data("gridIndex")
index <- grep("monthly/precipitation", gridIndex, value=TRUE) # 1'709
index <- grep('2014|2015|2016',index, value=TRUE) # n=36 (3*12)
precip <- dataDWD(index[6:8], base=gridbase, joinbf=TRUE)
plotRadar(precip[[1]], proj="seasonal", extent=NULL, main=names(precip)[1])

#15 Zeitreihe durchschnittlicher Lufttemperatur--------------------------------------------
index <- indexFTP(folder="annual/air_temperature_max", base=gridbase)
index <- index[-(1:2)] # exclude description files
index <- index[as.numeric(substr(index,62,65))>=2013] # after year 2013
tempmax <- dataDWD(index, base=gridbase, joinbf=TRUE)
names(tempmax) <- substr(names(tempmax), 62, 65)
plotRadar(tempmax[[1]], proj="seasonal",extent="seasonal", main="Annual grid of monthly averaged daily maximum air temperature (2m) - 2013")
tempmax_stack <- terra::rast(tempmax)
tempmax_stack <- projectRasterDWD(tempmax_stack, proj="seasonal",extent="seasonal")
terra::plot(tempmax_stack, range=range(terra::minmax(tempmax_stack)))
#timeseries at given location
loc <- data.frame(x=12.65295, y=53.06547) # Aussichtspunkt Kyritz-Ruppiner Heide
round(unlist(terra::extract(tempmax_stack, loc)[-1]),1)
