##########################
# Initial Setup
##########################

install.packages(c('huge', 'PerformanceAnalytics', 'xts'))

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
# Sector Clustering
##########################

# first a look at actual sector breakdown
sectortbl <- table(stockinfo$Sector)
sectorprop <- prop.table(sectortbl)
labels <- paste(names(sectorprop), round(sectorprop, 4) * 100, '%', sep = ' ')
pie(sectortbl,main = 'S&P Sectors', labels = labels)


# index returns
library(PerformanceAnalytics)
spreturns <- na.omit(CalculateReturns(spts))

# view correlations among returns
corret <- cor(spreturns)



