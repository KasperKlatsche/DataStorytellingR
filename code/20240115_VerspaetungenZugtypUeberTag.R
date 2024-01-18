#Zwei Graphen nebeneinander zeigen Verspätungen nach Zug Typ über den Tagesverlauf
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(viridis)
library(ggpubr)
library(gganimate)
library(magick)

#--------------- parameter -----------------------------------------------------
von <- "07:00" #zeitfenster beginn
bis <- "18:00" #zeitfenster ende
skalaMax <- 150 #skala ende x achse

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
  output[indexNahverkehr] <- "Local transport"
  output[indexRegio] <- "Regional transport"
  output[indexFernverkehr] <- "Long distance tran."
  output[indexDrittanbieter] <- "Non-DB providers"
  
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
  
  data$grund <- as.factor(data$grund)
  
  data <- data[,c(1:8,17:ncol(data))]
  
  #jetzt noch neue Spalten hinzufügen
  data$verspaetung <- as.numeric(data$verzAnkunft - data$geplAnkunft)/60
  return(data)
}

unnormal <- read.csv("./data/20240112_DeutscheBahn_StreikFreitag_gekuerzt.csv")
unnormal <- datenVorverarbeiten(unnormal)
#noch kurz die ausfälle als numerisch modellieren
unnormal$ausfNum <- as.numeric(rep(0,nrow(unnormal)))
unnormal$ausfNum[unnormal$ausfall == "ausfall"] <- 1

referenz <- read.csv("./data/20240109_DeutscheBahn_normalerDienstag_gekuerzt.csv")
referenz <- datenVorverarbeiten(referenz)


#--------------- zuerst die Ausfälle berechnen ----------------

ausfaelleZugtyp <- data.frame(unnormal$ausfNum, klassifikationVerkehr(unnormal$zugTyp), unnormal$geplAnkunft)
names(ausfaelleZugtyp) <- c("ausfall","zugTyp", "Plan")
#jetzt noch das Zeitfenster von bis rausfiltern
start <- as.POSIXct(paste(substr(ausfaelleZugtyp$Plan[1],1,10), von))
ende <- as.POSIXct(paste(substr(ausfaelleZugtyp$Plan[1],1,10), bis))
ausfaelleZugtyp <- ausfaelleZugtyp[which(ausfaelleZugtyp$Plan >= start & ausfaelleZugtyp$Plan <= ende),]
print(mean(ausfaelleZugtyp$ausfall))
print(aggregate(ausfall ~ zugTyp, ausfaelleZugtyp, mean))

#--------------- verspätungstabelle berechnen ----------------------------------

verspaetungen <- function(data) {
  verspVerkehr <- data.frame(klassifikationVerkehr(data$zugTyp),data$verspaetung, data$geplAnkunft)
  #alle Halte rauswerfen, die Startbahnhöfe sind und damit keine Verspätungen
  verspVerkehr <- verspVerkehr[-which(is.na(verspVerkehr[,2])),]
  names(verspVerkehr) <- c("Verkehrstyp","Verspaetung","Plan")
  #jetzt noch das Zeitfenster von bis rausfiltern
  start <- as.POSIXct(paste(substr(verspVerkehr$Plan[1],1,10), von))
  ende <- as.POSIXct(paste(substr(verspVerkehr$Plan[1],1,10), bis))
  verspVerkehr <- verspVerkehr[which(verspVerkehr$Plan >= start & verspVerkehr$Plan <= ende),]
  return(verspVerkehr)
}

unnormalVersp <- verspaetungen(unnormal)
referenzVersp <- verspaetungen(referenz)

#-------------- kurz die verspätungen pro Zugtyp berechnen ---------------------
aUnn <- aggregate(Verspaetung ~ Verkehrstyp, unnormalVersp, mean)
aRef <- aggregate(Verspaetung ~ Verkehrstyp, referenzVersp, mean)
versp <- merge(aUnn,aRef,by="Verkehrstyp")
names(versp) <- c("Verkehrstyp","unnormal","referenz")
versp$weniger <- (versp$referenz-versp$unnormal)/versp$referenz
print(versp)

#-------------- jetzt Graphen zeichnen -----------------------------------------

textSize <- 18
farbwahl <- "turbo"

g1 <- ggplot(data = unnormalVersp, aes(x = Verspaetung, y = Verkehrstyp, color = Verkehrstyp, fill = Verkehrstyp)) +
  geom_density_ridges(alpha = 0.8, scale = 5) +
  scale_fill_viridis(option = farbwahl, discrete = TRUE) +
  scale_color_viridis(option = farbwahl, discrete = TRUE) + 
  theme_few() +
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        text = element_text(size=textSize)) +
  xlim(0,skalaMax) + 
  xlab("Verspätung in min") +
  labs(title = 'Train Strike: {frame_time}', x = 'Delay in min', y = 'Density') +
  transition_time(Plan) +
  ease_aes('linear')
g2 <- ggplot(data = referenzVersp, aes(x = Verspaetung, y = Verkehrstyp, color = Verkehrstyp, fill = Verkehrstyp)) +
  geom_density_ridges(alpha = 0.8, scale = 5) +
  scale_fill_viridis(option = farbwahl, discrete = TRUE) +
  scale_color_viridis(option = farbwahl, discrete = TRUE) + 
  theme_few() +
  theme(legend.position = "none", 
        axis.text.y=element_blank(), 
        axis.title.y=element_blank(),
        text = element_text(size=textSize)) +
  xlim(0,skalaMax) + 
  xlab("Verspätung in min") +
  labs(title = 'Regular Weekday: {frame_time}', x = 'Delay in min', y = 'Density') +
  transition_time(Plan) +
  ease_aes('linear')

#Die Gifs rendern
duration <- 50
unnormalGif <- animate(g1, duration = duration, width = 520, height = 600)
referenzGif <- animate(g2, duration = duration, width = 400, height = 600)

#jetzt die Gifs zusammenführen - scheinbar nicht mehr nötig, da animate jetzt ein magick-image auswirft
#unnormalGif <- image_read(unnormalGif)
#referenzGif <- image_read(referenzGif)

outputGif <- image_append(c(unnormalGif[1], referenzGif[1]))
for(i in 2:100){
  combined <- image_append(c(unnormalGif[i], referenzGif[i]))
  outputGif <- image_join(outputGif, combined)
}

anim_save("./graphs/20240113_deutscheBahn_StreikVerspaetungen.gif", outputGif)
