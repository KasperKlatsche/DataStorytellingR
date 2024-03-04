#In diesem Script werden über Bund, Länder und Gemeinden die Einnahmen und Ausgaben über Zeit dargestellt

library(ggplot2)
library(gganimate)

#-------------Daten lesen und umwandeln -----------------------------------------------------------
datenLesen <- function(pfad) {
  data <- read.table(pfad, header=F, skip = 8, sep=";", fileEncoding="latin1")
  data <- data[,c(1,2,3,7,8)]
  for(i in 3:5) {
    data[,c(i)] <- as.numeric(data[,c(i)])
    data[is.na(data[,c(i)]), c(i)] <- 0
  }
  data[,c(1)] <- as.factor(data[,c(1)])
  names(data) <- c("Posten", "Jahr", "Bund", "Laender", "Gemeinden")
  data <- data[-c(1),]
}

ein <- datenLesen("./data/Bundeshaushalt/20240303_Einnahmen Bundeshaushalt_StatBundAmt.csv")
aus <- datenLesen("./data/Bundeshaushalt/20240303_Ausgaben Bundeshaushalt_StatBundAmt.csv")

bund <- cbind(rep("Bund", length(unique(ein$Jahr))), merge(x=aggregate(Bund~Jahr, data=ein, FUN="sum"), y=aggregate(Bund~Jahr, data=aus, FUN="sum"), by="Jahr"))
laender <- cbind(rep("Laender", length(unique(ein$Jahr))), merge(x=aggregate(Laender~Jahr, data=ein, FUN="sum"), y=aggregate(Laender~Jahr, data=aus, FUN="sum"), by="Jahr"))
gemeinden <- cbind(rep("Gemeinden", length(unique(ein$Jahr))), merge(x=aggregate(Gemeinden~Jahr, data=ein, FUN="sum"), y=aggregate(Gemeinden~Jahr, data=aus, FUN="sum"), by="Jahr"))

names(bund) <- c("Einheit", "Jahr", "Einnahmen", "Ausgaben")
names(laender) <- c("Einheit", "Jahr", "Einnahmen", "Ausgaben")
names(gemeinden) <- c("Einheit", "Jahr", "Einnahmen", "Ausgaben")

data <- data.frame(rbind(bund,laender,gemeinden))

#------------Jetzt den Graphen zeichnen------------------------------------------------------------
g <- ggplot(data, aes(x=Ausgaben, y=Einnahmen, colour=Einheit)) +
  geom_point() +
  transition_time(Jahr) +
  labs(title = "Jahr: {frame_time}")

animate(g, width = 800, height = 800)

anim_save("./graphs/EinnahmenAusgabenDesBundes.gif")
  