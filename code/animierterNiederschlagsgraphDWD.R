#Erstellung eines animierten Niederschlagsgraphen - funktioniert noch nicht
library(rdwd)

#parameter
jahre <- 2014:2021
adresse <- "/daily/radolan/historical/bin/"
filename <- "/SF"
filetyp <- "11.tar.gz"

#DWD muss wissen wo es die Daten zwischenspeichern darf
setwd("./data")

for(i in jahre) {
  link <- paste(adresse,i,filename,i,filetyp, sep="")
  file <- dataDWD(link, base=gridbase, joinbf=TRUE, read=FALSE)
  rad <- readDWD(file, exdir=tempdir(), selection=1:3) # with toraster=TRUE
  bild <- plotRadar(rad$dat, main=paste(i,".binary SF"), layer=1)
}