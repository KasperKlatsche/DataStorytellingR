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
auskombiniert$Amt[which(auskombiniert$Amt == "BMAS")] <- "... für Arbeit & Soziales"
auskombiniert$Amt[which(auskombiniert$Amt == "BMEL")] <- "... für Ernährung & Landwirtschaft"
auskombiniert$Amt[which(auskombiniert$Amt == "BMF")] <- "... der Finanzen"
auskombiniert$Amt[which(auskombiniert$Amt == "BMFSFJ")] <- "... für Familie, Senioren, Frauen & Jugend"
auskombiniert$Amt[which(auskombiniert$Amt == "BMG")] <- "... für Gesundheit"
auskombiniert$Amt[which(auskombiniert$Amt == "BMI")] <- "... des Inneren & für Heimat"
auskombiniert$Amt[which(auskombiniert$Amt == "BMJ")] <- "... der Justiz"
auskombiniert$Amt[which(auskombiniert$Amt == "BMUV")] <- "... für Umwelt, Naturschutz, nukl. Sich. & Verbr."
auskombiniert$Amt[which(auskombiniert$Amt == "BMWK")] <- "... für Wirtschaft & Klimaschutz "
names(auskombiniert)[3] <- "Bundesministerium..."

g <- ggplot(data = auskombiniert, aes(x=Bundesministerium..., y=aufwandDiff, fill=Bundesministerium...)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(option = "A", discrete = TRUE) +
  scale_color_viridis(option = "A", discrete = TRUE) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=40)) +
  ylab("Change in fulfillment effort since 2011 [mio €]") +
  transition_time(as.integer(jahr)) +
  labs(title = "Year: {frame_time}")
animate(g, height = 900, width =1500)
anim_save("./graphs/20240106_bureaucracyOverTime.gif")
