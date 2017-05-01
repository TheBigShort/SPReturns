##########################
# Initial Setup
##########################

#install.packages(c('huge', 'PerformanceAnalytics', 'xts', 'cluster', 'fpc'))

# load the sp data set
library(huge)
library(xts)
data("stockdata")

# extract names and sectors
stockinfo <- as.data.frame(stockdata$info)
names(stockinfo) <- c('Ticker', 'Sector', 'Name')

# overview of the dataframe
summary(stockinfo)


# actual data
spdata <- as.data.frame(stockdata$data)
names(spdata) <- stockinfo$Ticker
spts <- xts(spdata, order.by = as.Date(as.numeric(row.names(spdata))))


##########################
# Sector Comparison
##########################
library(PerformanceAnalytics)

# get sector names
sector.groups <- split.data.frame(stockinfo[,c('Ticker', 'Sector')], stockinfo$Sector)
sector.names <- names(sector.groups)

# get returns for all stocks
sp.returns <- na.omit(CalculateReturns(spts))

# get returns by sector
sector.returns <- sapply(sector.names, FUN = function (name) rowMeans(sp.returns[, sector.groups[[name]][,'Ticker']]))
colnames(sector.returns) <- gsub(' ', '.', sector.names)
sector.returns <- xts(sector.returns, order.by = as.Date(c(1:nrow(sector.returns))))

summary(sector.returns)

# calculate the distance in standard deviations
# between the mean and the most extreme events
# also calculate the probability of witnessing
# more extreme events than these under the assumption
# of normality
extrema <- function (v) {
  m <- mean(v)
  vol <- sd(v)
  
  sd.largest <- abs((max(v) - m) / vol)
  sd.smallest <- abs((min(v) - m) / vol)
  
  p.largest <- 1 - pnorm(max(v), mean = m, sd = vol)
  p.smallest <- pnorm(min(v), mean = m, sd = vol)
  
  return(round(c(p.smallest = p.smallest, sd.smallest = sd.smallest, p.largest = p.largest, sd.largest = sd.largest), 5))
}

es <- apply(sector.returns, MARGIN = 2, extrema)
ss <- apply(sector.returns, MARGIN = 2, sd)

library(Hmisc)

# extrema
barplot(es[c(2,4),], 
        col = c('#0099cc', '#ccffcc'),
        lwd = 2,
        xlab = 'Sector',
        ylab = 'Standard Deviations',
        main = 'Extrema Distance from Mean in Standard Deviations')

legend('topleft', inset = 0.15,
       legend = c('Lowest', 'Highest'), 
       fill = c('#0099cc', '#ccffcc'),
       title = 'Returns')

minor.tick(ny = 5, nx = 0)

# up and down days


# historical volatility
barplot(ss, 
        col = c('#0099cc'),
        lwd = 2,
        xlab = 'Sector',
        ylab = 'Standard Deviations',
        main = 'Historical Sector Volatility')

minor.tick(ny = 5, nx = 0)

# rolling 20 day annualized volatility
real.vol <- rollapply(sector.returns, width = 20, FUN = sd.annualized)
real.vol <- na.omit(real.vol)
cv <- real.vol[,1]

plot(cv)
sample <- cv[1:300]
vol.vol <- rollapply(sample, width = 20, FUN = sd.annualized)

plot(sample)
lines(vol.vol, col = 'red')
# is realized vol stationary?
library(tseries)
adf.test(cv)

# there's sufficient evidence to reject the null hypothesis
half <- length(cv) / 2

# find delta vol by prediction
# this forecasts a change in volatility
fc <- c()
for (t in (half + 1):(half+ 100)) {
  acv <- arima(cv[1:t], order = c(1, 0, 0))
  fc <- append(fc, predict(acv, n.ahead = 5)$pred[1])
}

deltavol <- fc - lag.xts(cv[(half + 1):length(cv)])
realdelta <- diff(cv)



##########################
# Top Performers and Momentum
##########################

firstYear <- 1:250
secondYear <- 251:500

c.returns <- apply(sp.returns[firstYear,] + 1, MARGIN = 2, prod)
c.returns <- sort(c.returns,decreasing = TRUE)
best <- c.returns[1:20]
best

c.returns2 <- apply(sp.returns[secondYear,] + 1, MARGIN = 2, prod)
best2 <- c.returns2[names(best)]
best2

# there is not enough evidence to reject the null
# that the past performers will perform the same as the market
t.test(best2, c.returns2)


##########################
# Worst Performers and Momentum
##########################

worst <- sort(c.returns)[1:20]
worst

worst2 <- c.returns2[names(worst)]
worst2

# there is not enough evidence to reject the null
# that the worst past performers will perform the same as the market
t.test(worst2, c.returns2)
