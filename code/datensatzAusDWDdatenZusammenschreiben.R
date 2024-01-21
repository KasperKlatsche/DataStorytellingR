#dieses Script soll einen Niederschlagsgraphen in GGPlot erstellen auf Basis von DWD Niederschlagsdaten

library(RCurl)

#Historical ----------------------- diesen Teil ausführen, wenn die Daten älter sind ------------------------------------------

#parameter zu den daten
url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/precipitation/historical/"
dir <- "./data/DWDtemporary/"
fileStart <- "10minutenwerte_nieder_"
fileEnde <- "_hist.zip"

#parameter zum load
datum <- "20220117" 

#stationen laden
stat <- read.table("./data/DWDtemporary/Historische_Beschreibung_Stationen.txt", sep="#", header=T) #den sep gibt es nicht, da der eigentliche sep nicht funktioniert
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

#Recent ----------------------- diesen Teil ausführen, wenn die Daten jünger sind ------------------------------------------

url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/precipitation/recent/"
dir <- "./data/DWDtemporary/"
fileStart <- "10minutenwerte_nieder_"
fileEnde <- "_akt.zip"

#parameter zum load
datum <- "20240118" 

#stationen laden
stat <- read.table("./data/DWDtemporary/Aktuelle_Beschreibung_Stationen.txt", sep="#", header=T) #den sep gibt es nicht, da der eigentliche sep nicht funktioniert
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

#jetzt die files nach Stationen selektieren
filesAlsNumeric <- as.numeric(alleFiles)
index <- filesAlsNumeric %in% stationen$statID
richtigeFiles <- paste(fileStart, alleFiles[index], fileEnde, sep="")

#Laden und zusammensetzen -------- dieser Teil wird immer ausgeführt ------------------------------------------------------

#das datum fuer die Selektion umwandeln
von <- "0700"
bis <- "1900"
vonAlsPosix <- as.POSIXlt(paste(datum, von, sep=""),format="%Y%m%d%H%M")
bisAlsPosix <- as.POSIXlt(paste(datum, bis, sep=""),format="%Y%m%d%H%M")

output <- data.frame(StatID=numeric(),MESS_DATUM=.POSIXct(character(0)),RWS_DAU_10=numeric(),RWS_10=numeric(),RWS_IND_10=numeric())
counter <- 1
insgesamt <- length(unique(richtigeFiles))
start <- Sys.time()

for(i in unique(richtigeFiles)) {
  #kurz mitschreiben, damit man den Überblick behält
  jetzt <- Sys.time()
  noch <- (jetzt-start)*(insgesamt-counter)/counter
  print(paste(counter, "von", insgesamt, "noch", as.numeric((jetzt-start)*(insgesamt-counter)/counter/60)))
  counter <- counter + 1
  
  #zuerst einen file laden
  download.file(paste(url, i, sep=""),
                paste(dir, i, sep=""))
  #file entpacken
  pfad <- unzip(paste(dir,i, sep=""), exdir = dir)
  #file lesen
  data <- read.table(pfad, sep=";", header = T)
  data <- data[,c(1,2,4:6)] #jetzt nur StationsID, MessDatum und die Niederschlagsmenge auswählen
  data$STATIONS_ID <- as.numeric(data$STATIONS_ID)
  data$MESS_DATUM <- as.POSIXlt(as.character(data$MESS_DATUM), format="%Y%m%d%H%M")
  data$RWS_10 <- as.numeric(data$RWS_10)
  #datenpunkte selektieren
  data <- data[-which(is.na(data$MESS_DATUM)),]
  data <- data[data$MESS_DATUM >= vonAlsPosix & data$MESS_DATUM <= bisAlsPosix,]
  #jetzt die daten zusammenführen
  output <- rbind(output,data)
  
  #file wieder löschen
  file.remove(pfad)
  file.remove(paste(dir, i, sep=""))
}

zusammen <- merge(output,stationen, by.x="STATIONS_ID", by.y="statID")
indexRaus <- which(names(zusammen)=="von_datum"|names(zusammen)=="bis_datum")
zusammen <- zusammen[,-indexRaus]
outputFileName <- paste("./data/",datum,"_",von,"_",bis,"_DWD_niederschlagsdaten.csv", sep="")
write.csv(zusammen,outputFileName)
