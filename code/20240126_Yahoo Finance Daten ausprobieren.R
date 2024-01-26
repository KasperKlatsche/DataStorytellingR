#yahoo finance ansprechen

library(yahoofinancer)

data <- Ticker$new('nflx') #Netflix
data$get_history(start = '2018-01-01', interval = '1d')
