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

# color theme
cols <- c('#0099cc', '#ccffcc')
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
  
  return(c(p.smallest = p.smallest, sd.smallest = sd.smallest, p.largest = p.largest, sd.largest = sd.largest))
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
real.vol <- rollapply(sector.returns, width = 60, FUN = sd.annualized)
real.vol <- na.omit(real.vol)
cv <- real.vol[,1]

plot(cv, 
     main = 'Consumer Discretionary Realized Volatility',
     ylab = 'Daily Volatility')

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

worst <- sort(c.returns)[1:5]
worst

worst2 <- c.returns2[names(worst)]
worst2

# there is not enough evidence to reject the null
# that the worst past performers will perform the same as the market
t.test(worst2, c.returns2)




##########################
# Model vs. Historical VaR
##########################

library(ggplot2)
set.seed(1)

# select 3 stocks for analysis
names <- as.character(stockinfo[sample(nrow(stockinfo), 3),'Ticker'])
smp <- sp.returns[,names]
names

# model VaR
mvar <- function (x, confidence = 0.95) {
  m <- mean(x)
  v <- sd(x)
  
  qnorm(p = 1 - confidence, mean = m, sd = v)
}

# historical VaR
hvar <- function (x, confidence = 0.95) {
  sort(as.vector(x))[round(length(x) * (1 - confidence), 0)]
}

# plot VaR types
plotVaR <- function (x, confidence = c(0.95, 0.995), ticker = '') {
  modelVaR <- mvar(x, confidence)
  histVaR <- hvar(x, confidence)
  
  m <- mean(x)
  vol <- sd(x)
  nmod <- function (x) dnorm(x, mean = m, sd = vol)
  
  # plot labels
  cols <- c('deepskyblue1', '#FF00FF')
  title <- paste('Value at Risk', ticker, sep = ' ')
  hLabels <- paste(as.character(round(histVaR, 4) * 100), '%', sep = ' ')
  mLabels <- paste(as.character(round(modelVaR, 4) * 100), '%', sep = ' ')
  subtitle <- paste(confidence * 100, '%', collapse = ', ', sep = '')
  title <- paste(title, subtitle, sep = '\n')
  
  # plot chart
  plot(density(x), lwd = 2, lty = 2, main = title, xlab = 'Returns')
  polygon(density(x), col = adjustcolor(cols[1], alpha = 0.5))
  s <- seq(min(x), max(x), by = 0.001)
  polygon(x = s, y = nmod(s), lwd = 2, col = adjustcolor(cols[2], alpha = 0.5))
  abline(v = histVaR, lty = 2, lwd = 2, col = cols[1])
  abline(v = modelVaR, lty = 1, lwd = 2, col = cols[2])
  text(histVaR, y = 20, labels = hLabels, srt = 90, font = 2, cex = 1.5)
  text(modelVaR, y = 30, labels = mLabels, srt = 90, font = 2, cex = 1.5)
  legend('topleft', legend = c('Historical', 'Model'), title = 'VaR Model', lwd = 2, lty = c(2, 1), col = c(cols[1], cols[2]))
  minor.tick(nx = 4, ny = 10)
}
  
plotVaR(smp[,1], ticker = names(smp)[1])
plotVaR(smp[,2], ticker = names(smp)[2])
plotVaR(smp[,3], ticker = names(smp)[3])


library(fGarch)

# losses that exceed VaR estimate
exceedVaR <- function (returns, VaR) {
  ex <- returns < VaR
  ex[ex == TRUE] <- returns[ex == TRUE]
  ex[ex == FALSE] <- NA
  
  return (ex)
}

# rolling model VaR
rollingVaR <- function (v, confidence, days = 100, width = 60, garch = TRUE) {
  df <- as.data.frame(v)
  n <- nrow(df)
  num <- days
  name <- names(v)[1]
  
  # calculate the rolling VaR
  df$Vol <- c(NA, rollapply(v, FUN = sd, width = width))[-n]
  df$Mean <- c(NA, rollapply(v, FUN = mean, width = width))[-n]
  df$VaR <- qnorm((1 - confidence), mean = df$Mean, sd = df$Vol)
  
  # garchVaR
  if (garch) {
    fit <- garchFit(data = v, trace = FALSE)
    gvol <- sqrt(fit@h.t)
    df$GarchVaR <- qnorm((1 - confidence), mean = df$Mean, sd = gvol)
  }
  
  # remove rows with VaR
  df <- head(na.omit(df), n = num)
  
  # check for losses greater than VaR
  df$Exceed <- exceedVaR(df[,1], df$VaR)
  
  # plot the returns
  b <- plot(df[,1], 
            ylim = c(min(df,na.rm = TRUE), max(df, na.rm = TRUE)), 
            pch=21, 
            bg='blue',
            xlab = 'Trading Days', 
            ylab = 'Return')
  
  title(paste('Rolling Model VaR', name, sep = ' '))

  segments(x0 = 1:nrow(df), y0 = 0,  y1 = df[,1])
  abline(h = 0)
  
  # plot the rolling VaR
  lines(df$VaR, col = 'blue', lty = 1, ylim = min(df), lwd = 2)
  points(df$Exceed, pch = 1, col = 'red', cex = 3, lwd = 2)
  minor.tick(ny = 5, nx = 5)
  
  
  # plot garch
  if (garch) {
    lines(df$GarchVaR, col = 'purple', lwd = 2)
    
    points(exceedVaR(df[,1],df$GarchVaR), pch = 1, col = 'orange', cex = 5, lwd = 2)
    
    legend('bottomright', 
           title = 'Volatility Model', 
           col = c('blue', 'purple'), 
           lty = c(1, 1), 
           lwd = c(2, 2), 
           legend = c('Equal Weighted', 'GARCH(1,1)'))
  }
}

# 95% VaR
rollingVaR(smp[,1], 0.95, garch = FALSE)
rollingVaR(smp[,2], 0.95, garch = FALSE)
rollingVaR(smp[,3], 0.95, garch = FALSE)

# 99.9% VaR
rollingVaR(smp[,1], 0.999, 500)
rollingVaR(smp[,2], 0.999, 1000)
rollingVaR(smp[,3], 0.999, 1000)

# kurtosis of returns
apply(smp, MARGIN = 2, FUN = kurtosis)

# probability of witnessing an
# event as or more extreme than the worst loss
smp.extreme <- apply(smp, MARGIN = 2, extrema)
smp.extreme

# view the drop associated with rolling VaR graph #3
plot(spts[, names(smp)[3]], main = names(smp)[3])

