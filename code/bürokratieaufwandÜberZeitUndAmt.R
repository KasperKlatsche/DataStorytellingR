#animierter Graph zu differenz der Bürokratieaufwände

library(ggplot2)
library(gganimate)
library(viridis)


data <- read.table("./data/20231231_diff-jaehrl-erfuellungsaufw-wirt_StatBundAmt.csv", sep=";", header=T)

aemter <- names(data)[2:ncol(data)]
jahre <- data[,1]
#daten vorbereiten
auskombiniert <- cbind(unlist(data[,2:ncol(data)]), rep(jahre, length(aemter)), rep(aemter, each=length(jahre)))
auskombiniert <- data.frame(auskombiniert)
names(auskombiniert) <- c("aufwandDiff", "jahr", "Amt")
#jetzt numerische werte erstellen
auskombiniert$aufwandDiff <- as.numeric(gsub(",",".",auskombiniert$aufwandDiff))
auskombiniert$jahr <- as.numeric(auskombiniert$jahr)
auskombiniert$Amt <- as.factor(auskombiniert$Amt)
#alle Ämter fliegen raus, die leere Werte haben
hatLeereWerte <- unique(auskombiniert$Amt[which(is.na(auskombiniert[,1]))])
auskombiniert <- auskombiniert[-which(auskombiniert$Amt==hatLeereWerte),]


g <- ggplot(data = auskombiniert, aes(x=Amt, y=aufwandDiff, fill=Amt)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  scale_color_viridis(option = "A", discrete = TRUE) + 
  transition_time(jahr) +
  labs(title = "Year: {frame_time}")
print(g)

 #Es ist nur die Diff, nicht der totale Wert
#Es fehlen Farben
#Achsenbeschriftungen sind grauenhaft
#Die Jahresangabe hat eine Nachkommazahl