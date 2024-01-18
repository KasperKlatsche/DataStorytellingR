#dieses Script soll einen Niederschlagsgraphen in GGPlot erstellen auf Basis von DWD Niederschlagsdaten

library(RCurl)

#parameter zu den daten
url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/precipitation/historical/"
dir <- "./data/DWDtemporary/"
fileStart <- "10minutenwerte_nieder_"
fileEnde <- "_hist.zip"

#parameter zum load
datum <- "20220117"

#stationen laden
stat <- read.table("./data/DWDtemporary/Beschreibung_Stationen.txt", sep="#", header=T) #den sep gibt es nicht, da der eigentliche sep nicht funktioniert
kopf <- strsplit(names(stat), "\\.")[[1]]
stationen <- as.data.frame(matrix(nrow = nrow(stat), ncol = length(kopf)))
names(stationen) <- kopf
for(i in 1:nrow(stat)) {
  zeile <- stat[i,1]
  teile <- strsplit(zeile, " ")[[1]]
  teile <- teile[-which(teile=="")]
  stationen[i,] <- teile[1:ncol(stationen)]
}
stationen <- data.frame(as.numeric(stationen$Stations_id), 
                        as.numeric(stationen$geoBreite), 
                        as.numeric(stationen$geoLaenge), 
                        as.POSIXlt(stationen$von_datum,format="%Y%m%d"),
                        as.POSIXlt(stationen$bis_datum,format="%Y%m%d"))
names(stationen) <- c("statID", "geoBreite", "geoLaenge", "von_datum", "bis_datum")

#alle filenamen holen und namen rauslösen
filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
getrennt <- strsplit(filenames, fileEnde)
index <- regexpr(fileStart, getrennt[[1]])
alleFiles <- substr(getrennt[[1]], index+22, index+44)

#jetzt die richtigen files identifizieren
richtigeFiles <- vector()
elemente <- strsplit(alleFiles, "_")
datumAlsPosix <- as.POSIXlt(datum,format="%Y%m%d")
for(i in 1:length(alleFiles)) {
  stationsID <- as.numeric(elemente[[i]][1])
  von <- as.POSIXlt(elemente[[i]][2],format="%Y%m%d")
  bis <- as.POSIXlt(elemente[[i]][3],format="%Y%m%d")
  
  if((stationsID %in% stationen$statID) & (von < datumAlsPosix) & (bis > datumAlsPosix)) {
    richtigeFiles[length(richtigeFiles)+1] <- paste(fileStart, alleFiles[i], fileEnde, sep="")
  }
}

file <- "10minutenwerte_nieder_00003_19930428_19991231_hist.zip"

#zuerst einen file laden
download.file(paste(url, file, sep=""),
              paste(dir, file, sep=""))
#file entpacken
pfad <- unzip(paste(dir,file, sep=""), exdir = dir)
#file lesen
data <- read.table(pfad, sep=";", header = T)
#file wieder löschen
file.remove(pfad)
file.remove(paste(dir, file, sep=""))
