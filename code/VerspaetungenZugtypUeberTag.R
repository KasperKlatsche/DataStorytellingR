#Zwei Graphen nebeneinander zeigen Verspätungen nach Zug Typ über den Tagesverlauf
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(viridis)
library(ggpubr)

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
  verspVerkehr <- verspVerkehr[-which(is.na(verspVerkehr[,2])),]
  names(verspVerkehr) <- c("Verkehrstyp","Verspaetung","Plan")
  return(verspVerkehr)
}

unnormalVersp <- verspaetungen(unnormal)
referenzVersp <- verspaetungen(referenz)

von <- -1
for(bis in seq(from=as.POSIXct("2023-01-01 07:00"),to=as.POSIXct("2023-01-01 19:00"),length.out=25)) {
  if(von == -1) {
    von <- bis
    next
  }
  
  #zeitpunkte vorbereiten
  unnormalVon <- as.POSIXct(paste(substr(unnormalVersp$Plan[1],1,10), substr(as.POSIXct(von, origin='1970-01-01 00:00.00 UTC'),12,16)))
  unnormalBis <- as.POSIXct(paste(substr(unnormalVersp$Plan[1],1,10), substr(as.POSIXct(bis, origin='1970-01-01 00:00.00 UTC'),12,16)))
  referenzVon <- as.POSIXct(paste(substr(referenzVersp$Plan[1],1,10), substr(as.POSIXct(von, origin='1970-01-01 00:00.00 UTC'),12,16)))
  referenzBis <- as.POSIXct(paste(substr(referenzVersp$Plan[1],1,10), substr(as.POSIXct(bis, origin='1970-01-01 00:00.00 UTC'),12,16)))
  
  unnormalFenster <- unnormalVersp[which(unnormalVersp$Plan>unnormalVon & unnormalVersp$Plan<=unnormalBis),]
  referenzFenster <- referenzVersp[which(referenzVersp$Plan>referenzVon & referenzVersp$Plan<=referenzBis),]
  
  #Graph erstellen - Dichte der Verspätungen nach Verkehrstypen
  g1 <- ggplot(data = unnormalFenster, aes(x = Verspaetung, y = Verkehrstyp, color = Verkehrstyp, fill = Verkehrstyp)) +
    geom_density_ridges(alpha = 0.8, scale = 5) +
    scale_fill_viridis(option = "A", discrete = TRUE) +
    scale_color_viridis(option = "A", discrete = TRUE) + 
    theme_few() +
    xlim(0,60) + 
    ggtitle(paste("Streik",unnormalBis)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank())

  g2 <- ggplot(data = referenzFenster, aes(x = Verspaetung, y = Verkehrstyp, color = Verkehrstyp, fill = Verkehrstyp)) +
    geom_density_ridges(alpha = 0.8, scale = 5) +
    scale_fill_viridis(option = "A", discrete = TRUE) +
    scale_color_viridis(option = "A", discrete = TRUE) + 
    theme_few() +
    xlim(0,60) + 
    ggtitle(paste("normaler Montag", referenzBis)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank())

  g <- ggarrange(g1,g2,nrow=1,ncol=2,common.legend = T)
  print(g)
}