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

library(Hmisc)

barplot(es[c(2,4),], 
        col = c('#0099cc', '#ccffcc'),
        lwd = 2,
        xlab = 'Sector',
        ylab = 'Standard Deviations',
        main = 'Extrema Distance from Mean in Standard Deviations')

legend('topleft', inset = 0.15,
       legend = c('Lowest', 'Highest'), 
       fill = c('#0099cc', '#ccffcc'),,
       title = 'Returns')

minor.tick(ny = 5, nx = 0)




