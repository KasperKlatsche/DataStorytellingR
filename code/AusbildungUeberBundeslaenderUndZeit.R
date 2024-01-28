#in diesem Script geht es darum Ausbildungsvertraege über Zeit auszuwerten
#https://www-genesis.destatis.de/genesis/online?operation=abruftabelleBearbeiten&levelindex=0&levelid=1706252339249&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&code=21211-0104&auswahltext=&nummer=2&variable=2&name=BIL041&werteabruf=Werteabruf#abreadcrumb

library(ggplot2)
library(gganimate)

data <- read.table("./data/Ausbildung/20240126_Ausbildungsvertraege_StatBundAmt.csv", header=F, skip = 3, sep=";", fileEncoding="latin1")
header <- c("Jahr", "Branche", "Herkunft", "Geschlecht",unlist(data[3,5:ncol(data)]))
data <- data[4:nrow(data),]
names(data) <- header

#uebersetzen
data$Branche[data$Branche=="Freie Berufe"] <- "Liberal Professions"
data$Branche[data$Branche=="Handwerk"] <- "Craft"
data$Branche[data$Branche=="Hauswirtschaft"] <- "Home Economics"
data$Branche[data$Branche=="Industrie und Handel"] <- "Industry and Trade"
data$Branche[data$Branche=="Landwirtschaft"] <- "Agriculture"
data$Branche[data$Branche=="Öffentlicher Dienst"] <- "Public Service"
data$Geschlecht[data$Geschlecht=="männlich"] <- "male"
data$Geschlecht[data$Geschlecht=="weiblich"] <- "female"

#die felder formatieren
data$Jahr <- as.integer(data$Jahr)
for(i in 2:4) {
  data[,i] <- as.factor(data[,i])
}
for(i in 5:ncol(data)) {
  data[,i] <- as.numeric(data[,i])
  data[which(is.na(data[,i])),i] <- 0
}


#jetzt die Tabelle umformen nach Bundesländer
bundland <- data.frame(Bundesland=character(), Jahr=numeric(), Branche=factor(), Herkunft=factor(), Geschlecht=factor(), Anzahl=numeric())
for(i in 5:ncol(data)) {
  bundesland <- rep(header[i], nrow(data))
  batch <- data.frame(bundesland, data$Jahr, data$Branche, data$Herkunft, data$Geschlecht, data[,i])
  names(batch) <- c("Bundesland", "Jahr", "Branche", "Herkunft", "Geschlecht", "Anzahl")
  bundland <- rbind(bundland, batch)
}

#Aggregieren ueber Bundesländer
summe <- apply(data[,5:ncol(data)], 1, sum)
aggreg <- data.frame(data$Jahr, data$Branche, data$Herkunft, data$Geschlecht, summe)
names(aggreg) <- c("Jahr", "Branche", "Herkunft", "Geschlecht", "Anzahl")


#---------------- verschiedene Graphen zeichnen ---------------------------------------------------

data <- bundland
g1 <- ggplot(data[which(data$Branche=="Insgesamt" & data$Herkunft!="Insgesamt" & data$Geschlecht=="Insgesamt"),], 
            aes(x=Bundesland, y=Anzahl, fill=Herkunft)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  transition_time(as.integer(Jahr)) +
  labs(title = "Ausbildungsverträge nach Bundesland und Herkunft in {frame_time}")
print(g1)
#nix spannendendes

data <- bundland
g2 <- ggplot(data[which(data$Branche=="Insgesamt" & data$Herkunft=="Insgesamt" & data$Geschlecht!="Insgesamt"),], 
             aes(x=Bundesland, y=Anzahl, fill=Geschlecht)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  transition_time(as.integer(Jahr)) +
  labs(title = "Ausbildungsverträge nach Bundesland und Geschlecht in {frame_time}")
print(g2)
#möglicherweise die Erkenntnis, dass männlich rauf und runter geht, aber weiblich kontinuierlich runter

data <- bundland
g3 <- ggplot(data[which(data$Branche!="Insgesamt" & data$Herkunft=="Insgesamt" & data$Geschlecht=="Insgesamt"),], 
             aes(x=Bundesland, y=Anzahl, fill=Branche)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  transition_time(as.integer(Jahr)) +
  labs(title = "Ausbildungsverträge nach Bundesland und Branche in {frame_time}")
print(g3)
#nix spannendes

data <- aggreg
g4 <- ggplot(data[which(data$Branche!="Insgesamt" & data$Herkunft=="Insgesamt" & data$Geschlecht!="Insgesamt"),], 
             aes(x=Branche, y=Anzahl, fill=Geschlecht)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size=30),
        legend.title = element_text(colour="black", size = 30, face = "bold"),
        legend.text  = element_text(colour="black", size = 30)) +
  guides(fill=guide_legend(title="Gender")) +
  xlab("Sector") +
  ylab("Number of contracts [*thousand]") +
  transition_time(as.integer(Jahr)) +
  labs(title = "Apprenticeship contracts by sector and gender in {frame_time}")
#dieser Graph scheint interessante Aussagen zu beinhalten - nehmen wir
animate(g4, width = 1100, height = 1200)
anim_save("./graphs/AusbildungsvertraegeBrancheGeschlecht.gif")


#------------------ jetzt nochmal Zahlen zusammentragen für den Beitrag ------------------------------

#veränderung von geschlecht über Zeit
data <- aggreg
gesch <- data[which(((data$Jahr==2008)|(data$Jahr==2022)) & data$Branche=="Insgesamt" & data$Herkunft=="Insgesamt"),]
gesch <- data.frame(matrix(gesch$Anzahl, nrow=3, ncol=2))
names(gesch) <- c("2008","2022")
row.names(gesch) <- c("male","female","insgesamt")
gesch$reduktion <- (gesch$`2008`-gesch$`2022`)/gesch$`2008`
print(gesch)

#veränderung von Branche über Zeit
data <- aggreg
branch <- data[which(((data$Jahr==2008)|(data$Jahr==2022)) & data$Geschlecht=="Insgesamt" & data$Herkunft=="Insgesamt"),]
rNames <- branch$Branche[1:(nrow(branch)/2)]
branch <- data.frame(matrix(branch$Anzahl, nrow=nrow(branch)/2, ncol=2))
names(branch) <- c("2008","2022")
row.names(branch) <- rNames
branch$reduktion <- (branch$`2008`-branch$`2022`)/branch$`2008`
branch$reduktionAbsolut <- branch$`2008`-branch$`2022`
print(branch)
