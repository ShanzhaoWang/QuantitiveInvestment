library(quantmod) # download data
quantmod::getSymbols(c("IBM", "GE", "^GSPC"), from = "2010-01-01") #stock data

head(GSPC)
tail(GSPC)
