#Graph für Baustatistik. Folgende 4 Statistiken
#https://www-genesis.destatis.de/genesis/online
#31111-0008
#31121-0004
#31131-0003
#31141-0003

library(ggplot2)
library(gganimate)

#----------------------------- Graph Ueberhang gegen Abgang ----------------------------------------

ueberhang <- read.table("./data/Bautaetigkeit/20240128_Bauueberhang genehmigter Bauvorhaben.csv", header=F, skip = 3, sep=";", fileEncoding="latin1")
names(ueberhang)  <- c("Stichtag", "Gebäudeart", "Bauherr", ueberhang[2,4:ncol(ueberhang)])
ueberhang <- ueberhang[4:nrow(ueberhang),]
ueberhang$Stichtag <- as.numeric(format(as.POSIXct(ueberhang$Stichtag, format="%d.%m.%Y"), "%Y"))
for(i in 2:3) {
  ueberhang[,i] <- as.factor(ueberhang[,i])
}
for(i in 4:ncol(ueberhang)) {
  ueberhang[,i] <- as.numeric(ueberhang[,i])
}

abgang <- read.table("./data/Bautaetigkeit/20240128_Bauabgang genehmigter Bauvorhaben.csv", header=F, skip = 3, sep=";", fileEncoding="latin1")
names(abgang)  <- c("Stichtag", "Gebäudeart", "Bauherr", abgang[2,4:ncol(abgang)])
abgang <- abgang[4:nrow(abgang),]
for(i in 1:ncol(abgang)){
  if(i>1 & i<4) {
    abgang[,i] <- as.factor(abgang[,i])
  } else {
    abgang[,i] <- as.numeric(abgang[,i])
  }
}

#jetzt die datensätze zusammenführen
ueberhang <- aggregate(ueberhang$`Gebäude/Baumaßnahmen`, by=list(stichtag=ueberhang$Stichtag, bauherr=ueberhang$Bauherr), FUN=sum)
names(ueberhang) <- c("Stichtag", "Bauherr", "Ueberhang")
abgang <- aggregate(abgang$`Gebäude/Gebäudeteile`, by=list(stichtag=abgang$Stichtag, bauherr=abgang$Bauherr), FUN=sum)
names(abgang) <- c("Stichtag", "Bauherr", "Abgang")
data <- merge(x=ueberhang, y=abgang, by=c("Stichtag","Bauherr"))

#Graphen zeichnen
g <- ggplot(data, aes(x=Ueberhang, y=Abgang, colour=Bauherr)) +
  geom_point() +
  transition_time(as.integer(Stichtag)) +
  ease_aes('linear') +
  labs(title = "Baustatistik {frame_time}")
animate(g, width = 600, height = 500)
#auch mega langweilig

#----------------------------- Graph Genehmigungen gegen Fertigstellungen ----------------------------------------

#Genehmigungen vorbereiten
genehmigungen <- read.table("./data/Bautaetigkeit/20240128_Baugenehmigungen neuer Gebaeude.csv", header=F, skip = 2, sep=";", fileEncoding="latin1")
names(genehmigungen) <- c("Jahr", "Energieverwendung", "Energieart", genehmigungen[6,4:ncol(genehmigungen)])
genehmigungen <- genehmigungen[7:nrow(genehmigungen),]
for(i in 1:ncol(genehmigungen)) {
  if(i<4 & i>1) {
    genehmigungen[,i] <- as.factor(genehmigungen[,i])
  } else {
    genehmigungen[,i] <- gsub("-","0",genehmigungen[,i])
    genehmigungen[,i] <- gsub("\\.","0",genehmigungen[,i])
    genehmigungen[,i] <- as.numeric(genehmigungen[,i])
  }
}
genehmigungenAgg <- aggregate(. ~ Jahr,
          genehmigungen[which(genehmigungen$Energieverwendung=="Zur Heizung verwendete primäre Energie"),c(1,4:ncol(genehmigungen))],
          sum)
temp <- data.frame(Jahr=numeric(), Typ=character(), genehmigungen=numeric())
for(i in 2:ncol(genehmigungenAgg)) {
  temp <- rbind(temp, data.frame(genehmigungenAgg[,1], rep(names(genehmigungenAgg)[i],nrow(genehmigungenAgg)), genehmigungenAgg[,i]))
}
names(temp) <- c("Jahr","Typ","Genehmigungen")
genehmigungenAgg <- temp

#Fertigstellung vorbereiten
fertigstellung <- read.table("./data/Bautaetigkeit/20240128_Baufertigstellungen neuer Gebaeude.csv", header=F, skip = 3, sep=";", fileEncoding="latin1")
names(fertigstellung) <- c("Jahr", "Energieverwendung", "Energieart", fertigstellung[5,4:ncol(fertigstellung)])
fertigstellung <- fertigstellung[6:nrow(fertigstellung),]
for(i in 1:ncol(fertigstellung)) {
  if(i<4 & i>1) {
    fertigstellung[,i] <- as.factor(fertigstellung[,i])
  } else {
    fertigstellung[,i] <- gsub("-","0",fertigstellung[,i])
    fertigstellung[,i] <- gsub("\\.","0",fertigstellung[,i])
    fertigstellung[,i] <- as.numeric(fertigstellung[,i])
  }
}
fertigstellungAgg <- aggregate(. ~ Jahr,
                               fertigstellung[which(fertigstellung$Energieverwendung=="Zur Heizung verwendete primäre Energie"),c(1,4:ncol(fertigstellung))],
                              sum)
temp <- data.frame(Jahr=numeric(), Typ=character(), fertigstellungen=numeric())
for(i in 2:ncol(fertigstellungAgg)) {
  temp <- rbind(temp, data.frame(fertigstellungAgg[,1], rep(names(fertigstellungAgg)[i],nrow(fertigstellungAgg)), fertigstellungAgg[,i]))
}
names(temp) <- c("Jahr","Typ","Fertigstellungen")
fertigstellungAgg <- temp

#Genehmigungen und Fertigstellungen zusammenführen und graphisch darstellen
data <- merge(x=genehmigungenAgg, y=fertigstellungAgg, by=c("Jahr","Typ"))
g <- ggplot(data, aes(x=Genehmigungen, y=Fertigstellungen, colour=Typ)) +
  geom_point() +
  transition_time(Jahr) +
  ease_aes('linear') +
  labs(title = "Baustatistik {frame_time}")
print(g)
#mega langweilig

