#animierter Graph zu differenz der Bürokratieaufwände

library(ggplot2)
library(gganimate)

data <- read.table("./data/20231231_diff-jaehrl-erfuellungsaufw-wirt_StatBundAmt.csv", sep=";", header=T)
