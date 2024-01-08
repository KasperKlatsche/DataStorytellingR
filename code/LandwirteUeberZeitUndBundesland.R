#Dieses Script wertet die Landwirtschaftlichen Daten des Statistischen Bundesamtes aus
#https://www.destatis.de/DE/Themen/Branchen-Unternehmen/Landwirtschaft-Forstwirtschaft-Fischerei/Landwirtschaftliche-Betriebe/Tabellen/betriebsgroessenstruktur-landwirtschaftliche-betriebe.html
#Ziel ist die Anzahl und Größe der Betriebe über Zeit und Bundesland zu betrachten

library(ggplot2)
library(gganimate)

data <- read.table("./data/20240108_Landwirtschaftlich Bundeslaender_statBundAmt.csv", sep=";", header=T, fileEncoding="latin1")
for(i in 2:ncol(data)){
  data[,i] <- 
}