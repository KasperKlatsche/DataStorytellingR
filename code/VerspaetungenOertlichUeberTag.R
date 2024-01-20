#dieses Script zeigt die Deutsche Bahnverspätungen örtlich über Zeit

library(ggplot2)
library(gganimate)

#--------------- daten laden und vorbereiten -----------------------------------

datenVorverarbeiten <- function(data) {
  data$zugTyp <- as.factor(data$zugTyp)
  data$von <- as.factor(data$von)
  data$nach <- as.factor(data$nach)
  data$ausfall <- as.factor(data$ausfall)
  
  #zeiten wieder zusammensetzen aus den daten und zeiten
  data$geplAbfahrt <- as.POSIXlt(paste(data$geplAbfahrtTag, data$geplAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
  data$verzAbfahrt <- as.POSIXlt(paste(data$verzAbfahrtTag, data$verzAbfahrtZeit, sep=" "),format="%Y-%m-%d %H:%M")
  data$geplAnkunft <- as.POSIXlt(paste(data$geplAnkunftTag, data$geplAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")
  data$verzAnkunft <- as.POSIXlt(paste(data$verzAnkunftTag, data$verzAnkunftZeit, sep=" "),format="%Y-%m-%d %H:%M")
  
  data <- data[,c(2,4:5,19:23)]
  
  #jetzt noch neue verspaetung hinzufügen
  data$verspaetung <- as.numeric(data$verzAnkunft - data$geplAnkunft)/60
  data$verspaetung[data$verspaetung<0] <- 0
  data$verspaetung[is.na(data$verspaetung)] <- 0
  
  #die Zielvariable erstellen
  data$nachPlan <- rep("on time", nrow(data))
  data$nachPlan[data$verspaetung > 5] <- "delayed"
  data$nachPlan[data$ausfall == "ausfall"] <- "cancelled"
  data$nachPlan <- as.factor(data$nachPlan)
  
  #alle Zuege, die im Zielbahnhof angekommen sind und keine Abfahrt haben - raus
  data <- data[-which(is.na(data$geplAbfahrt)),]
  #nur zeiten zwischen 0700 und 1900
  min <- as.POSIXct(paste(substr(data$geplAbfahrt[1],1,10), "07:00"))
  max <- as.POSIXct(paste(substr(data$geplAbfahrt[1],1,10), "19:00"))
  data <- data[which(data$geplAbfahrt > min & data$geplAbfahrt < max),]
  
  return(data)
}

data <- read.csv("./data/20240117_DeutscheBahn_SchneeDonnerstag_gekuerzt.csv")
data <- datenVorverarbeiten(data)

#---------------- Graphen zeichnen ---------------------------------------------

data$geplAbfahrt <- as.POSIXct(data$geplAbfahrt)
g <- ggplot(data, aes(y=breite,x=laenge)) +
  geom_jitter(aes(colour=nachPlan, shape=nachPlan)) +
  borders(database = "world", regions = c("Germany"), xlim = c(6.071, 14.979), ylim = c(47.41, 54.91)) +
  theme_light() +
  transition_time(geplAbfahrt) +
  ease_aes('linear')

animate(g, duration = 50, width = 800, height = 1200)

anim_save("./graphs/verspaetungenOertlichUeberTag.gif")
