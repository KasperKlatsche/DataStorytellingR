#Auswertung der Daten des statistischen Bundesamtes zu Nachhaltigkeit

data <- read.table("./data/20231229_nachhaltige Entwicklung_statBundAmt.csv", sep=";", skip=5,nrows = 200, header=T)
