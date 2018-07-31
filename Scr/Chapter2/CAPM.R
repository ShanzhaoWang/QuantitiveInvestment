library(quantmod) # download data
library(PerformanceAnalytics)
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

dat_ret <- apply(dat, MARGIN = 2, ret)
row.names(dat_ret) <- c(row.names(dat)[-1])

# define annualized rate of return 4%, non-risking 
Rf <- 0.04/12
results_1 <- table.AnnualizedReturns(dat_ret, Rf = Rf)
# One thing we observe from the result is that SP500 has an annual rate of return 10%, so it's not easy that we can surpass it

results_2 <- table.Stats(dat_ret) # overall view of summary stats 

# corvariance
pairs(dat_ret)
cor(dat_ret) 

# alpha, beta
PerformanceAnalytics::CAPM.alpha(dat_ret[, 1:2], dat_ret[, 3], Rf= Rf)
PerformanceAnalytics::CAPM.beta(dat_ret[, 1:2], dat_ret[, 3], Rf= Rf)

# see detailed interpretations of alpha and beta on p.60 
