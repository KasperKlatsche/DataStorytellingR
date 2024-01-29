#Graph für Baustatistik. Folgende 4 Statistiken
#https://www-genesis.destatis.de/genesis/online
#31111-0008
#31121-0004
#31131-0003
#31141-0003

ueberhang <- read.table("./data/Bautaetigkeit/20240128_Bauueberhang genehmigter Bauvorhaben.csv", header=F, skip = 3, sep=";", fileEncoding="latin1")
abgang <- read.table("./data/Bautaetigkeit/20240128_Bauabgang genehmigter Bauvorhaben.csv", header=F, skip = 3, sep=";", fileEncoding="latin1")

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
names(genehmigungenAgg) <- paste("genehmigung", gsub(" ","",names(genehmigungenAgg)), sep="")

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
names(fertigstellungAgg) <- paste("fertigstellung", gsub(" ","",names(fertigstellungAgg)), sep="")
