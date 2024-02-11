#yahoo finance ansprechen

library(yahoofinancer)
library(dplyr)
library(ggplot2)
library(gganimate)

#daten vorbereiten - wir nutzen nur die zeit und adjusted_close
datenVorverarbeiten <- function(data) {
  daten <- data$date
  kurs <- unlist(data$adj_close)
  relKurs <- kurs/kurs[1]
  output <- data.frame(time=daten, price=relKurs)
  return(output)
}

tick <- Ticker$new('NVDA') #NVIDIA
nvda <- tick$get_history(start = '2016-01-01', interval = '1d')
nvda <- datenVorverarbeiten(nvda)

tick <- Ticker$new('^NDXT') #NVIDIA
ref <- tick$get_history(start = '2016-01-01', interval = '1d')
ref <- datenVorverarbeiten(ref)

data <- left_join(nvda,ref, by="time")
names(data) <- c("time","nvda","reference")

#jetzt den plot zeichnen
linesizes <- c("NVIDIA"=1.5, 
               "NASDAQ-100-Tech"=1
)

colors <- c("NVIDIA"="deepskyblue4", 
            "NASDAQ-100-Tech"="gray33"
)

schriftgroesse_1 <- 20
schriftgroesse_2 <- 6
hoehe_vertikale_schrifthoehe <- 40

g <- ggplot(data, aes(x=time)) +
  scale_color_manual(values=colors) +
  scale_size_manual(values=linesizes) +
  labs(x="time", y="relative performance", colour="stock/index", size = "stock/index") +
  theme_light() +
  theme(axis.text.x = element_text(angle=45, hjust=0.8, vjust=0.8),
        text = element_text(size=schriftgroesse_1),
        axis.title.x=element_blank()) +
  geom_line(aes(y=nvda, colour="NVIDIA", size="NVIDIA")) +
  geom_line(aes(y=reference, colour="NASDAQ-100-Tech", size="NASDAQ-100-Tech")) +
  geom_vline(xintercept = c(as.POSIXct("2017-06-12", format("%Y-%m-%m"))), linetype="dotted") +
  geom_text(aes(x=as.POSIXct("2017-06-12", format("%Y-%m-%m")), label="\nTransformer Models", y=hoehe_vertikale_schrifthoehe, angle=90), size=schriftgroesse_2) +
  geom_vline(xintercept = c(as.POSIXct("2020-07-22", format("%Y-%m-%m"))), linetype="dotted") +
  geom_text(aes(x=as.POSIXct("2020-07-22", format("%Y-%m-%m")), label="\nLarge Language Models (LLMs)", y=hoehe_vertikale_schrifthoehe, angle=90), size=schriftgroesse_2) +
  geom_vline(xintercept = c(as.POSIXct("2022-09-30", format("%Y-%m-%m"))), linetype="dotted") +
  geom_text(aes(x=as.POSIXct("2022-09-30", format("%Y-%m-%m")), label="\nChatGPT", y=hoehe_vertikale_schrifthoehe, angle=90), size=schriftgroesse_2) +
  geom_vline(xintercept = c(as.POSIXct("2023-09-06", format("%Y-%m-%m"))), linetype="dotted") +
  geom_text(aes(x=as.POSIXct("2023-09-06", format("%Y-%m-%m")), label="\nGPTs", y=hoehe_vertikale_schrifthoehe, angle=90), size=schriftgroesse_2) +
  transition_reveal(time) +
  labs(title = "Performance of NVIDIA in context")
animate(g, width = 1000, height = 800) 
anim_save("./graphs/20240211_NVIDIAandLLMs.gif")
