#In diesem Script werden über Bund, Länder und Gemeinden die Einnahmen und Ausgaben über Zeit dargestellt

datenLesen <- function(pfad) {
  data <- read.table(pfad, header=F, skip = 8, sep=";", fileEncoding="latin1")
  ...
}