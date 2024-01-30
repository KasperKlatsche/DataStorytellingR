#Lebenserwartung über BIP pro Kopf - klassiker
#https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/

library(ggplot2)
library(gganimate)
library(gapminder)
library(scales)

#wo wird der graph später gespeichert?
setwd("./graphs")

p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10(labels = label_comma()) +
  labs(x = "GDP per capita", y = "Life expectancy") +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

anim_save("../graphs/lifeExpectanceOverGDP.gif", p)