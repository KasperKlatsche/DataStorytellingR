#Zwei Graphen nebeneinander zeigen Verspätungen nach Zug Typ über den Tagesverlauf

setwd("C:\\Users\\Julius\\Documents\\Daten\\Deutsche Bahn\\Datensaetze")
data <- read.csv(file.choose())
print(dim(data))
data$zug <- as.factor(data$zug)
data$von <- as.factor(data$von)
data$nach <- as.factor(data$nach)

data$geplAbfahrt <- as.POSIXlt(paste(data$geplAbfahrtTag, data$geplAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
data$verzAbfahrt <- as.POSIXlt(paste(data$verzAbfahrtTag, data$verzAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
data$geplAnkunft <- as.POSIXlt(paste(data$geplAnkunftTag, data$geplAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")
data$verzAnkunft <- as.POSIXlt(paste(data$verzAnkunftTag, data$verzAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")

data <- data[,c(1:7,17:20,16)]
print(dim(data))
print(summary(data))
