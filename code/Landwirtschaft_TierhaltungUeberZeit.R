#Dieses Script zeichnet die Entwicklung von Tierhaltung über Zeit auf Basis der Daten des statistischen Bundesamtes
#https://www.destatis.de/DE/Themen/Branchen-Unternehmen/Landwirtschaft-Forstwirtschaft-Fischerei/Landwirtschaftliche-Betriebe/Tabellen/ausgewaehlte-merkmale-zv.html

library(ggplot2)
library(viridis)
library(scales)
library(gganimate)

daten <- read.table("./data/20240108_Landwirtschaftlich Merkmale_statBundAm.csv", sep=";", header=T, fileEncoding="latin1")
#daten selektieren
names(daten)[3:6] <- c("2020","2016","2013","2010")
#ACHTUNG: theoretisch gäbe es noch die maximale Anzahl an Höfen
daten <- daten[c(12:23),]
typen <- c("Rinder", "Schweine", "Schafe", "Ziegen", "Gefluegel", "Einhufer")
#leeren data frame aufsetzen
data <- vector()
for(i in 3:6) {
  jahr <- names(daten)[i]
  werte <- as.numeric(gsub(",", "\\.", daten[,i]))
  if(length(data)==0) {
    data <- cbind(rep(jahr, length(typen)), typen, matrix(werte, nrow=length(typen), ncol=2, byrow=T))
  } else {
    data <- rbind(data,
                  cbind(rep(jahr, length(typen)), typen, matrix(werte, nrow=length(typen), ncol=2, byrow=T)))
  }
}
data <- as.data.frame(data)
names(data) <- c("Jahr", "Typen", "Betriebe", "Tiere")
data$Jahr <- as.numeric(data$Jahr)
data$Typen <- as.factor(data$Typen)
data$Betriebe <- as.numeric(data$Betriebe)
data$Tiere <- as.numeric(data$Tiere)

p <- ggplot(
  data, 
  aes(x = Tiere, y=Betriebe, colour = Typen)
) +
  geom_point(show.legend = T, alpha = 0.7, size=4) +
  scale_color_viridis_d() +
  scale_x_log10(labels = label_comma()) +
  labs(x = "Anzahl Tiere", y = "Betriebe") +
  transition_time(as.integer(Jahr)) +
  labs(title = "Year: {frame_time}")
print(p)
