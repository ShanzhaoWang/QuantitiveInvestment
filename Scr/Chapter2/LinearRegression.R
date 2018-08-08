# load time series library
library(zoo)
library(xts)
dat <-read.csv("test_data - Sheet1.csv", stringsAsFactors = F)

# change it to xts format
df <- xts(dat[, -1], order.by = as.POSIXct(dat[,1]))

x <- as.numeric(df[,1])
y <- as.numeric(df[,2])

plot(y~x+1) 

# slope and intercept, residuals, etc.
lm.ab <- lm(y~x+1)
lm.ab$coefficients
lm.ab$call
lm.ab$residuals
summary(lm.ab) # F-test, p.66

# run together 
plot(y ~ x+ 1)
abline(lm.ab)

# residuals 
y.res <- residuals(lm.ab)

shapiro.test(y.res)
plot(y.res)

# four plots p.68
plot(lm.ab)
# explanation p.69

# all four plots on one page
par(mfrow = c(2,2))
plot(lm.ab)
