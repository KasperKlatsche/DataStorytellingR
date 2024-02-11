#yahoo finance ansprechen

library(yahoofinancer)

data <- Ticker$new('NVDA') #NVIDIA
data$get_history(start = '2018-01-01', interval = '1d')

print(data[((nrow(data)-100):nrow(data)),])
