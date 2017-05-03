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
set.seed(2342)

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
plotVaR <- function (x, confidence = c(0.95, 0.995)) {
  modelVaR <- mvar(x, confidence)
  histVaR <- hvar(x, confidence)
  
  m <- mean(x)
  vol <- sd(x)
  nmod <- function (x) dnorm(x, mean = m, sd = vol)
  
  # plot labels
  cols <- c('purple', '#FF00FF')
  hLabels <- paste(as.character(round(histVaR, 4) * 100), '%', sep = ' ')
  mLabels <- paste(as.character(round(modelVaR, 4) * 100), '%', sep = ' ')
  labels <- seq(from = -0.1, to = 0.1, by = 0.025)
  labels <- append(labels, histVaR)
  labels <- append(labels, modelVaR)
  labels <- sort(labels)
  
  # plot densities
  ggplot(x, aes(x, col = 'red', fill = 'red')) + 
  labs(title = 'Value at Risk', subtitle = 'Historical vs. Model') +
    
  # the historical return denisty
  geom_density(alpha = 0.2, size = 1, col = cols[1], fill = cols[1], linetype = 2) +
  
  # the model density
  stat_function(fun = nmod, alpha = 0.2, size = 1, col = cols[2], fill = cols[2], geom = 'area') +
  
  # historical VaR
  geom_vline(xintercept = histVaR, linetype = 2, size = 1.3, col = cols) +
  annotate('text', x = histVaR, y = 30, label = hLabels, angle = 90) +
  
  # model VaR
  geom_vline(xintercept = modelVaR, size = 1.3, col = cols) +
  annotate('text', x = modelVaR, y = 20, label = mLabels, angle = 90) +
  
  ylab('Density of Returns') +
  xlab('% Return') +
  xlim(qnorm(0.0001, mean = m, sd = vol), qnorm(0.9999, mean = m, sd = vol)) +
  scale_x_continuous(breaks = labels, labels = round(labels * 100, 2)) +
  scale_color_manual(name = 'VaR Type', values = c(Historical = cols[1], Model = cols[2])) +
  theme_light()
}
  
plotVaR(smp[,1])
plotVaR(smp[,2])
plotVaR(smp[,3])


