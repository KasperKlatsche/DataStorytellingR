#das Script nimmt einen zu langen Bahndatensatz (>100MB) und verkürzt ihn auf das Zeitfenster 0700-1900 in der Hoffnung, dass er dann klein genug ist für GIT

#parameter zeitfenster
von <- "07:00"
bis <- "19:00"

#woher kommt die Datei und wohin soll die Antwort geschrieben werden?
pfad <- file.choose()
outputPfad <- paste(substr(pfad,0,nchar(pfad)-4), "_gekuerzt.csv",sep="")

#jetzt daten lesen und verarbeiten
data <- read.csv(pfad)
#dazu die Zeit als Datum konvertieren
data$geplAnkunft <- as.POSIXlt(paste(data$geplAnkunftTag, data$geplAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")
data$geplAbfahrt <- as.POSIXlt(paste(data$geplAbfahrtTag, data$geplAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
#und das Zeitfenster von bis rausfiltern
start <- as.POSIXct(paste(substr(data$geplAnkunft[2],1,10), von)) #achtung es kann schon mal sein, dass das datum NA ist - dann einfach eine andere Zeile wählen
ende <- as.POSIXct(paste(substr(data$geplAnkunft[2],1,10), bis)) #achtung es kann schon mal sein, dass das datum NA ist - dann einfach eine andere Zeile wählen
output <- data[which((data$geplAnkunft >= start & data$geplAnkunft <= ende) | is.na(data$geplAnkunft)),] #Ankunft zwischen 0700 und 1900
output <- output[-which((output$geplAbfahrt < start | output$geplAbfahrt > ende) & is.na(output$geplAnkunft)),] #für alle Startpunkt Abfahrt zwischen 0700 un 1900

#jetzt die datei wieder wegschreiben
write.csv(output[,1:(ncol(data)-2)], file=outputPfad)
