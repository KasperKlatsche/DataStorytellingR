#Dieses Script zeichnet die Entwicklung von Tierhaltung über Zeit auf Basis der Daten des statistischen Bundesamtes
#https://www.destatis.de/DE/Themen/Branchen-Unternehmen/Landwirtschaft-Forstwirtschaft-Fischerei/Landwirtschaftliche-Betriebe/Tabellen/ausgewaehlte-merkmale-zv.html

data <- read.table("./data/20240108_Landwirtschaftlich Merkmale_statBundAm.csv", sep=";", header=T, fileEncoding="latin1")
