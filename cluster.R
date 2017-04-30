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
library(fpc)
library(cluster)
spreturns <- na.omit(CalculateReturns(spts))

# average daily return, equal weighting
sp.avg.ret <- apply(spreturns,MARGIN = 1, mean)

# view correlations among returns
corret <- cor(spreturns)
numSectors <- length(levels(stockinfo$Sector))
pcluster <- pam(corret, k = numSectors)

# t(as.matrix(tail(spdata, n = 10)))

geomean <- function(v) {
  prod(v) ^ (1/ length(v))
}

returnsMatrix <- t(as.matrix(tail(round(spreturns, 4), n = 10)))
clusters <- kmeans(returnsMatrix, centers = numSectors)

df <- as.data.frame(cov(spreturns, sp.avg.ret) / var(sp.avg.ret))
names(df)[1] <- 'Beta'
df$Vol <- apply(spreturns, MARGIN = 2, sd)
df$GeoMean <- apply(spreturns + 1, MARGIN = 2, geomean)
beta.clusters <- kmeans(df, centers = numSectors)