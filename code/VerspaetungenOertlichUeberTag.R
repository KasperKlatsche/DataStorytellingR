#dieses Script zeigt die Deutsche Bahnverspätungen örtlich über Zeit

library(ggplot2)
library(gganimate)

#--------------- daten laden und vorbereiten -----------------------------------

datenVorverarbeiten <- function(data) {
  data$zugTyp <- as.factor(data$zugTyp)
  data$von <- as.factor(data$von)
  data$nach <- as.factor(data$nach)
  
  #zeiten wieder zusammensetzen aus den daten und zeiten
  data$geplAbfahrt <- as.POSIXlt(paste(data$geplAbfahrtTag, data$geplAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
  data$verzAbfahrt <- as.POSIXlt(paste(data$verzAbfahrtTag, data$verzAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
  data$geplAnkunft <- as.POSIXlt(paste(data$geplAnkunftTag, data$geplAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")
  data$verzAnkunft <- as.POSIXlt(paste(data$verzAnkunftTag, data$verzAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")
  
  data <- data[,c(1:8,18:21,17)]
  
  #jetzt noch neue verspaetung hinzufügen
  data$verspaetung <- as.numeric(data$verzAnkunft - data$geplAnkunft)/60
  data$verspaetung[data$verspaetung<0] <- 0
  data$verspaetung[is.na(data$verspaetung)] <- 0
  
  #alle Zuege, die im Zielbahnhof angekommen sind und keine Abfahrt haben - raus
  data <- data[-which(is.na(data$geplAbfahrt)),]
  #nur zeiten zwischen 0700 und 1900
  min <- as.POSIXct(paste(substr(data$geplAbfahrt[1],1,10), "07:00"))
  max <- as.POSIXct(paste(substr(data$geplAbfahrt[1],1,10), "19:00"))
  data <- data[which(data$geplAbfahrt > min & data$geplAbfahrt < max),]
  
  return(data)
}

setwd("./data")
samstag <- read.csv("20231118_normalerSamstag.csv")
samstag <- datenVorverarbeiten(samstag)

#---------------- Graphen zeichnen ---------------------------------------------

data$geplAbfahrt <- as.POSIXct(data$geplAbfahrt)
g <- ggplot(data, aes(y=breite,x=laenge,color=verspaetung)) +
  geom_jitter() +
  transition_time(geplAbfahrt) +
  ease_aes('linear')

gif <- animate(g, duration = 50, width = 350, height = 500)

anim_save("../graphs/verspaetungenOertlichUeberTag.gif", g)
