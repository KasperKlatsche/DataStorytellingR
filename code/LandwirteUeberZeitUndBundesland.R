#Dieses Script wertet die Landwirtschaftlichen Daten des Statistischen Bundesamtes aus
#https://www.destatis.de/DE/Themen/Branchen-Unternehmen/Landwirtschaft-Forstwirtschaft-Fischerei/Landwirtschaftliche-Betriebe/Tabellen/betriebsgroessenstruktur-landwirtschaftliche-betriebe.html
#Ziel ist die Anzahl und Größe der Betriebe über Zeit und Bundesland zu betrachten

library(ggplot2)
library(gganimate)
library(stringr)
library(viridis)
library(scales)

data <- read.table("./data/20240108_Landwirtschaftlich Bundeslaender_statBundAmt.csv", sep=";", header=T, fileEncoding="latin1")
#erst die Zahlen in Zahlen wandeln
for(i in 2:ncol(data)){
  data[,i] <- as.numeric(gsub(",",".",data[,i]))
}

data10 <- data.frame(data$Bundesland, rep(2010, nrow(data)), data$X2010.Betriebe, data$X2010.Flaeche)
data20 <- data.frame(data$Bundesland, rep(2020, nrow(data)), data$X2020.Betriebe, data$X2020.Flaeche)
data21 <- data.frame(data$Bundesland, rep(2021, nrow(data)), data$X2021.Betriebe, data$X2021.Flaeche)
data22 <- data.frame(data$Bundesland, rep(2022, nrow(data)), data$X2022.Betriebe, data$X2022.Flaeche)

names(data10) <- c("Bundesland", "Jahr", "Betriebe", "Flaeche")
names(data20) <- c("Bundesland", "Jahr", "Betriebe", "Flaeche")
names(data21) <- c("Bundesland", "Jahr", "Betriebe", "Flaeche")
names(data22) <- c("Bundesland", "Jahr", "Betriebe", "Flaeche")

gesamt <- rbind(data10,data20,data21,data22)

p <- ggplot(
  gesamt, 
  aes(x = Flaeche, y=Betriebe, colour = Bundesland)
) +
  geom_point(show.legend = T, alpha = 0.7) +
  scale_fill_viridis(option = "rainbow", discrete = TRUE) +
  scale_color_viridis(option = "rainbow", discrete = TRUE) + 
  labs(x = "Flaeche", y = "Anz. Betriebe") +
  scale_x_log10(labels = label_comma()) +
  transition_time(as.integer(Jahr)) +
  labs(title = "Year: {frame_time}")
print(p)
