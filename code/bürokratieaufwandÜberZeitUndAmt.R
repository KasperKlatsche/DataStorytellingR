#animierter Graph zu differenz der Bürokratieaufwände

library(ggplot2)
library(gganimate)
library(viridis)

#Differenzen über die Jahre
data <- read.table("./data/20231231_diff-jaehrl-erfuellungsaufw-wirt_StatBundAmt.csv", sep=";", header=T)

#erst die summen ueber Zeit bilden
data[1,2:ncol(data)] <- as.numeric(gsub(",",".",gsub("\\.","",data[1,2:ncol(data)])))
for(i in 2:nrow(data)) {
  data[i,2:ncol(data)] <- as.numeric(gsub(",",".",gsub("\\.","",data[i,2:ncol(data)]))) + as.numeric(data[i-1,2:ncol(data)])
}

#jetzt langweilige Ämter entfernen
data <- data[,c(1,4,7:13,15)]
aemter <- names(data)[2:ncol(data)]
jahre <- data[,1]

#daten vorbereiten
auskombiniert <- cbind(unlist(data[,2:ncol(data)]), rep(jahre, length(aemter)), rep(aemter, each=length(jahre)))
auskombiniert <- data.frame(auskombiniert)
names(auskombiniert) <- c("aufwandDiff", "jahr", "Amt")
#jetzt numerische werte erstellen
#auskombiniert$aufwandDiff <- as.numeric(gsub(",",".",auskombiniert$aufwandDiff))
auskombiniert$aufwandDiff <- as.numeric(auskombiniert$aufwandDiff)
auskombiniert$jahr <- as.numeric(auskombiniert$jahr)

#jetzt die Bundesämter bennenen
auskombiniert$Amt[which(auskombiniert$Amt == "BMAS")] <- "... für Arbeit und Soziales"
auskombiniert$Amt[which(auskombiniert$Amt == "BMEL")] <- "... für Ernährung und Landwirtschaft"
auskombiniert$Amt[which(auskombiniert$Amt == "BMF")] <- "... der Finanzen"
auskombiniert$Amt[which(auskombiniert$Amt == "BMFSFJ")] <- "... für Familie, Senioren, Frauen und Jugend"
auskombiniert$Amt[which(auskombiniert$Amt == "BMG")] <- "... für Gesundheit"
auskombiniert$Amt[which(auskombiniert$Amt == "BMI")] <- "... des Inneren und für Heimat"
auskombiniert$Amt[which(auskombiniert$Amt == "BMJ")] <- "... der Justiz"
auskombiniert$Amt[which(auskombiniert$Amt == "BMUV")] <- "... für Umwelt, Naturschutz, nukleare Sicherheit und Verbraucherschutz"
auskombiniert$Amt[which(auskombiniert$Amt == "BMWK")] <- "... für Wirtschaft und Klimaschutz "
names(auskombiniert)[3] <- "Bundesministerium..."

g <- ggplot(data = auskombiniert, aes(x=Bundesministerium..., y=aufwandDiff, fill=Bundesministerium...)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  scale_color_viridis(option = "A", discrete = TRUE) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylab("Aggregated difference since 2012 [mio €]") +
  transition_time(as.integer(jahr)) +
  labs(title = "Year: {frame_time}")
print(g)

#Absolute zahlen als Summe der einzelnen Vorgaben
data <- read.table("./data/20240103_Absoluter Erfüllungsaufwand_StatBundAmt.csv", skip = 3, nrow=16677, sep=";", encoding="ISO-8859-13", header=F)
#der header hat einen character, der nicht header sein darf, deshalb so
names(data) <- data[1,]
data <- data[-c(1),-c(34)]
data[,18] <- as.numeric(gsub(",",".",gsub("\\.","",data[,18])))/1000
data[,23] <- as.numeric(gsub(",",".",gsub("\\.","",data[,23])))/1000
data[,4] <- as.factor(data[,4])
data <- data[,c(4,18,23)]
names(data) <- c("Ressort", "absErfAuf", "diffErfAuf")
print(aggregate(diffErfAuf ~ Ressort, data, sum))

cbind(1:ncol(data),names(data))
summary(as.factor(substr(data[,7],7,11)))
#aggregieren über Ressorts
d
print(tapply(data[,23], data[,4], FUN=sum(na.rm=T)))

#Es ist nur die Diff, nicht der totale Wert
#Es fehlen Farben
#Achsenbeschriftungen sind grauenhaft
#Die Jahresangabe hat eine Nachkommazahl