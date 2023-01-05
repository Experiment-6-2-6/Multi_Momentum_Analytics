library(quantmod)

coin <- getSymbols("BTC-USD", from = Sys.Date() - 365, to = Sys.Date(), 
           periodicity = "daily", auto.assign = FALSE)

head(coin, 3)
