#dieses Script zeigt die Deutsche Bahnverspätungen örtlich über Zeit

library(ggplot2)
library(gganimate)

#--------------- Deutsche Bahn Daten laden und vorbereiten -----------------------------------

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
  #Punktgröße
  data$punktGroesse <- rep(7, nrow(data))
  data$punktGroesse[data$nachPlan == "cancelled"] <- 10
  
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
data$zeit <- as.POSIXct(data$geplAbfahrt)


#---------------- Deutsche Bahn Graphen zeichnen ---------------------------------------------


#g1 <- ggplot(data[data$nachPlan=="cancelled"|data$nachPlan=="delayed",], aes(y=breite,x=laenge)) +
#  geom_jitter(data = data[data$nachPlan=="delayed",], colour="blue", size=4, shape=15) +
#  geom_jitter(data = data[data$nachPlan=="cancelled",], colour="red", size=8, shape=8) +
#  borders(database = "world", regions = c("Germany"), xlim = c(6.071, 14.979), ylim = c(47.41, 54.91)) +
#  theme_light() +
#  transition_time(zeit) +
#  ease_aes('linear') +
#  labs(title = "Time: {frame_time}")
#
#animate(g1, duration = 50, width = 800, height = 1200)

#---------------- Wetterdaten laden und zeichnen --------------------------------------------

#parameter
mittel <- 0.07
hoch <- 0.3
groesseWetter <- 30

wetter <- read.csv("./data/20240117_0700_1900_DWD_niederschlagsdaten.csv")
wetter$niederschlag <- wetter$RWS_10
wetter$niederschlag[wetter$niederschlag < 0] <- 0
wetter$regen <- rep("low", nrow(wetter))
wetter$regen[wetter$niederschlag > mittel] <- "medium"
wetter$regen[wetter$niederschlag > hoch] <- "high"
wetter$regen <- as.factor(wetter$regen)
wetter$zeit <- as.POSIXct(wetter$MESS_DATUM)
names(wetter)[7:8] <- c("breite", "laenge")

#g2 <- ggplot(wetter, aes(y=breite, x=laenge)) +
#  geom_point(data=wetter[wetter$regen=="medium",],colour="gray60", size=groesseWetter) +
#  geom_point(data=wetter[wetter$regen=="high",],colour="gray40", size=groesseWetter) +
#  borders(database = "world", regions = c("Germany"), xlim = c(6.071, 14.979), ylim = c(47.41, 54.91)) +
#  theme_light() +
#  transition_time(zeit) +
#  ease_aes('linear') +
#  labs(title = "Time: {frame_time}")

#animate(g2, duration = 50, width = 800, height = 1200)

#---------------- Graphen zusammenfügen und speichern ---------------------------------------

g <- ggplot(data, aes(y=breite,x=laenge)) +
  geom_point(data=wetter[wetter$regen=="medium",],colour="gray80", size=groesseWetter, show.legend = FALSE) +
  geom_point(data=wetter[wetter$regen=="high",],colour="gray60", size=groesseWetter, show.legend = FALSE) +
  borders(database = "world", regions = c("Germany"), xlim = c(6.071, 14.979), ylim = c(47.41, 54.91), size=2, colour="black", show.legend = FALSE)  +
  geom_jitter(data = data[data$nachPlan=="delayed",],aes(colour="blue"), size=4, shape=15, show.legend = TRUE) +
  geom_jitter(data = data[data$nachPlan=="cancelled",],aes(colour="red"), size=6, shape=15, show.legend = TRUE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        text = element_text(size=40),
        legend.title = element_blank(),
        legend.text  = element_text(colour="black", size = 40, face = "bold")) +
  transition_time(zeit) +
  ease_aes('linear') +
  labs(title = "Time: {frame_time}")

#animate(g, duration = 200, nframes = 100, fps = 40, width = 800, height = 1200)
animate(g, nframes = 10, fps = 1, width = 800, height = 1200)

anim_save("./graphs/verspaetungenOertlichUeberTag.gif")
