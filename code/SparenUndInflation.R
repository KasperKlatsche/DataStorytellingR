#Ein Graph, der das Sparverhalten gegenüber der Inflation (Verbraucherpreisindex) darstellt

library(ggplot2)
library(gganimate)

#zuerst die daten zum sparen verarbeiten
spar <- read.table("./data/PrivateSparen/20240130_Sparen privater Haushalte_StatBundAmt.csv", header=F, skip = 1, sep=";", fileEncoding="latin1")
spar <- spar[,1:(ncol(spar)-1)] #die letzte spalte ist leer
#zuerst die Daten erstellen
sparDaten <- spar[4,]
tage <- character(length(sparDaten))
tage[spar[5,] == "1. Quartal"] <- "01.01."
tage[spar[5,] == "2. Quartal"] <- "01.04."
tage[spar[5,] == "3. Quartal"] <- "01.07."
tage[spar[5,] == "4. Quartal"] <- "01.10."
sparDaten <- as.POSIXct(paste(tage,sparDaten,sep=""), format="%d.%m.%Y")
sparDaten <- sparDaten[4:length(sparDaten)]
#spardaten nochmal säubern
names(spar) <- c("Bereinigung", "Typ", "Einheit", paste(spar[5,4:ncol(spar)], spar[4,4:ncol(spar)]))
spar <- spar[which(spar$Bereinigung=="Originalwerte"),] #nur Originaldaten
for(i in 1:ncol(spar)) {
  if(i < 4) {
    spar[,i] <- as.factor(spar[,i])
  } else {
    spar[,i] <- as.numeric(gsub(",","\\.",spar[,i]))
  }
}
data <- data.frame(
  daten=sparDaten,
  netto=t(spar[which(grepl("Nettolöhne", spar$Typ)), 4:ncol(spar)]),
  sparen=t(spar[which(grepl("Sparen der privaten", spar$Typ)), 4:ncol(spar)]),
  quote=t(spar[which(grepl("Sparquote der privaten", spar$Typ)), 4:ncol(spar)])
)
names(data) <- c("time","netto","savings","quote")

#netto <- data.frame(daten=sparDaten,
#                    typ=rep("Netto salary"),
#                    value=(t(spar[which(grepl("Nettolöhne", spar$Typ)), 4:ncol(spar)]) -
#                             t(spar[which(grepl("Sparen der privaten", spar$Typ)), 4:ncol(spar)])))
#names(netto) <- c("time", "type", "values")
#sparen <- data.frame(daten=sparDaten,
#                    typ=rep("Share of private savings"),
#                    value=t(spar[which(grepl("Sparen der privaten", spar$Typ)), 4:ncol(spar)]))
#names(sparen) <- c("time", "type", "values")
#data <- rbind(sparen,netto)

#Inflationsdaten laden
infla <- read.table("./data/PrivateSparen/20240130_Verbraucherpreisindex_StatBundAmt.csv", header=F, skip = 1, sep=";", fileEncoding="latin1")
infla <- infla[6:nrow(infla),]
jahr <- infla[,1]
tag <- character(length(jahr))
tag[which(infla[,2]=="Januar")] <- "01.01"
tag[which(infla[,2]=="Februar")] <- "01.02"
tag[which(infla[,2]=="März")] <- "01.03"
tag[which(infla[,2]=="April")] <- "01.04"
tag[which(infla[,2]=="Mai")] <- "01.05"
tag[which(infla[,2]=="Juni")] <- "01.06"
tag[which(infla[,2]=="Juli")] <- "01.07"
tag[which(infla[,2]=="August")] <- "01.08"
tag[which(infla[,2]=="September")] <- "01.09"
tag[which(infla[,2]=="Oktober")] <- "01.10"
tag[which(infla[,2]=="November")] <- "01.11"
tag[which(infla[,2]=="Dezember")] <- "01.12"
infla$time <- as.POSIXct(paste(tag,jahr,sep="."), format="%d.%m.%Y")
infla$inflation <- as.numeric(gsub(",","\\.",infla[,4]))
infla <- infla[-which(is.na(infla$inflation)),c(6,7)]

#die beiden Datensätze auf den gleichen Zeitraum passen
data <- data[-which(data$time<min(infla$time) | data$time>max(infla$time)),]


#jetzt den Graphen zeichnen
g1 <- ggplot(data, aes(x=time)) +
  geom_line(aes(y=netto)) +
  geom_line(aes(y=savings)) +
  theme_minimal() +
  transition_reveal(time) +
  labs(title = "Total netto salary with savings share {frame_time}")
animate(g1, width = 500, height = 600) 


#jetzt den Graphen zeichnen
g2 <- ggplot(left_join(infla,data,by="time"), aes(x=time)) +
  geom_line(aes(y=inflation)) +
  geom_line(aes(y=quote)) +
  theme_minimal() +
  transition_reveal(time) +
  labs(title = "Relation of relative savings to inflation in {frame_time}")
animate(g2, width = 500, height = 600) 
