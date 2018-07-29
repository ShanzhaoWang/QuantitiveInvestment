library(quantmod) # download data
quantmod::getSymbols(c("IBM", "GE", "^GSPC"), from = "2010-01-01") #stock data

head(GSPC)
tail(GSPC)

quantmod::barChart(GSPC) # candlestick

#rename columns
names(IBM) <- c("open", "high", "low", "closed", "volumn", "adjusted")
names(GE) <- c("open", "high", "low", "closed", "volumn", "adjusted")
names(GSPC) <- c("open", "high", "low", "closed", "volumn", "adjusted")

#select desired columns
dat <- as.data.frame(merge(IBM$adjusted, GE$adjusted, GSPC$adjusted)) 
#dat <- cbind(IBM$adjusted, GE$adjusted, GSPC$adjusted)
colnames(dat) <- c("IBM", "GE", "SP500")

# function to calculate average rate of return
ret = function(x){
  ret_x <- c()
  for(i in 1: length(x)-1) {
    ret_x[i] <- log(x[i+1]/x[i])
  }
  return(ret_x)
}

apply(dat, MARGIN = 2, ret) 
