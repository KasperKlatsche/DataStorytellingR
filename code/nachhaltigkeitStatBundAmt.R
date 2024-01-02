#Auswertung der Daten des statistischen Bundesamtes zu Nachhaltigkeit

data <- read.table("./data/20231229_nachhaltige Entwicklung_statBundAmt.csv", sep=";", skip=5,nrows = 200, header=T)

#leider sind hier nur einzele Kennzahlen über Zeit enthalten über Zeit
#wir könnten also wieder einen Scatter Plot über Zeit erstellen, aber keine Wertereihen über Zeit
#vielleicht in Zukunft