#Zwei Graphen nebeneinander zeigen Verspätungen nach Zug Typ über den Tagesverlauf

setwd("./data")
data <- read.csv(file.choose())
print(dim(data))
data$zugTyp <- as.factor(data$zugTyp)
data$von <- as.factor(data$von)
data$nach <- as.factor(data$nach)

data$geplAbfahrt <- as.POSIXlt(paste(data$geplAbfahrtTag, data$geplAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
data$verzAbfahrt <- as.POSIXlt(paste(data$verzAbfahrtTag, data$verzAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
data$geplAnkunft <- as.POSIXlt(paste(data$geplAnkunftTag, data$geplAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")
data$verzAnkunft <- as.POSIXlt(paste(data$verzAnkunftTag, data$verzAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")

data <- data[,c(1:8,18:21,17)]

#jetzt noch neue Spalten hinzufügen
data$verspaetung <- as.numeric(data$verzAnkunft - data$geplAnkunft)/60

print(dim(data))
print(summary(data))
