#Zwei Graphen nebeneinander zeigen Verspätungen nach Zug Typ über den Tagesverlauf
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(viridis)
library(ggpubr)
library(gganimate)
library(magick)

#--------------- zuerst funktionen erstellen -----------------------------------

aggregationNachZugTyp <- function(data) {
  #Auswertung nach Zugtyp. Alle Einträge, bei denen NA ist, fliegen raus, weil es da keine Verspaetung geben kann
  aggregation <- data[-which(is.na(data$verspaetung)),] %>%
    group_by(zugTyp) %>%
    summarise_at(vars(verspaetung), list(name = mean))
  
  #aggregation nach zugtypen
  indexNahverkehr <- which(aggregation$zugTyp == "Bus" | aggregation$zugTyp == "S")
  indexFernverkehr <- which(aggregation$zugTyp == "EC" |aggregation$zugTyp == "IC" | aggregation$zugTyp == "ICE")
  indexRegio <- which(aggregation$zugTyp == "RB" | aggregation$zugTyp == "RE" | aggregation$zugTyp == "IR" | aggregation$zugTyp == "IRE")
  indexDrittanbieter <- (1:nrow(aggregation))[-c(indexNahverkehr, indexRegio, indexFernverkehr)]
  
  output <- data.frame(matrix(nrow=4,ncol=2))
  names(output) <- c("typ","versp")
  output[,1] <- c("Nahverkehr","Regio","Fernverkehr","Drittanbieter")
  output[,2] <- c(mean(unlist(aggregation[indexNahverkehr,2])),mean(unlist(aggregation[indexRegio,2])),mean(unlist(aggregation[indexFernverkehr,2])),mean(unlist(aggregation[indexDrittanbieter,2])))
  return(output)
}

#klassifikation der Zugtypen nach Verkehrstypen
klassifikationVerkehr <- function(zugTyp) {
  indexNahverkehr <- which(zugTyp == "Bus" | zugTyp == "S")
  indexFernverkehr <- which(zugTyp == "EC" |zugTyp == "IC" | zugTyp == "ICE")
  indexRegio <- which(zugTyp == "RB" | zugTyp == "RE" | zugTyp == "IR" | zugTyp == "IRE")
  indexDrittanbieter <- (1:length(zugTyp))[-c(indexNahverkehr, indexRegio, indexFernverkehr)]
  
  output <- character(length(zugTyp))
  output[indexNahverkehr] <- "Nahverkehr"
  output[indexRegio] <- "Regionalverkehr"
  output[indexFernverkehr] <- "Fernverkehr"
  output[indexDrittanbieter] <- "Drittanbieter"
  
  return(output)
}

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
  
  #jetzt noch neue Spalten hinzufügen
  data$verspaetung <- as.numeric(data$verzAnkunft - data$geplAnkunft)/60
  return(data)
}

setwd("./data")
unnormal <- read.csv("20231113_normalerMontag.csv")
unnormal <- datenVorverarbeiten(unnormal)

referenz <- read.csv("20231113_normalerMontag.csv")
referenz <- datenVorverarbeiten(referenz)


#--------------- jetzt gehts ans zeichnen --------------------------------------

verspaetungen <- function(data) {
  verspVerkehr <- data.frame(klassifikationVerkehr(data$zugTyp),data$verspaetung, data$geplAnkunft)
  #alle Halte rauswerfen, die Startbahnhöfe sind und damit keine Verspätungen
  verspVerkehr <- verspVerkehr[-which(is.na(verspVerkehr[,2])),]
  names(verspVerkehr) <- c("Verkehrstyp","Verspaetung","Plan")
  #jetzt noch das Zeitfenster von 0700 bis 1900 rausfiltern
  start <- as.POSIXct(paste(substr(verspVerkehr$Plan[1],1,10), "07:00"))
  ende <- as.POSIXct(paste(substr(verspVerkehr$Plan[1],1,10), "19:00"))
  verspVerkehr <- verspVerkehr[which(verspVerkehr$Plan >= start & verspVerkehr$Plan <= ende),]
  return(verspVerkehr)
}

unnormalVersp <- verspaetungen(unnormal)
referenzVersp <- verspaetungen(referenz)

g1 <- ggplot(data = unnormalVersp, aes(x = Verspaetung, y = Verkehrstyp, color = Verkehrstyp, fill = Verkehrstyp)) +
  geom_density_ridges(alpha = 0.8, scale = 5) +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  scale_color_viridis(option = "A", discrete = TRUE) + 
  theme_few() +
  theme(legend.position = "none", axis.title.y=element_blank()) +
  xlim(0,60) + 
  xlab("Verspätung in min") +
  labs(title = 'Streik: {frame_time}', x = 'Verspätung in min', y = 'Density') +
  transition_time(Plan) +
  ease_aes('linear')
g2 <- ggplot(data = referenzVersp, aes(x = Verspaetung, y = Verkehrstyp, color = Verkehrstyp, fill = Verkehrstyp)) +
  geom_density_ridges(alpha = 0.8, scale = 5) +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  scale_color_viridis(option = "A", discrete = TRUE) + 
  theme_few() +
  theme(legend.position = "none", axis.text.y=element_blank(), axis.title.y=element_blank()) +
  xlim(0,60) + 
  xlab("Verspätung in min") +
  labs(title = 'normaler Montag: {frame_time}', x = 'Verspätung in min', y = 'Density') +
  transition_time(Plan) +
  ease_aes('linear')

#Die Gifs rendern
duration <- 50
unnormalGif <- animate(g1, duration = duration, width = 390, height = 300)
referenzGif <- animate(g2, duration = duration, width = 300, height = 300)

#jetzt die Gifs zusammenführen
unnormalGif <- image_read(unnormalGif)
referenzGif <- image_read(referenzGif)

outputGif <- image_append(c(unnormalGif[1], referenzGif[1]))
for(i in 2:100){
  combined <- image_append(c(unnormalGif[i], referenzGif[i]))
  outputGif <- image_join(outputGif, combined)
}

print(outputGif)
