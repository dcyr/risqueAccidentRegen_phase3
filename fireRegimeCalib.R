


fireObs <- read.csv("../data/fireObs.csv", header=TRUE)
thresh <- 0##prod(res(studyArea))/10000
df <- fireObs[fireObs$areaTotal_ha>thresh,]
fireZones <- raster("../data/fireZones.tif")


FCTarget <- 101
scaleFactor <-  25
simDuration <- 100
n <- 10000

# simFireCalib <- function(simDur, FCTarget, scaleFactor = 25,
#                     fireSizeFit = NULL, fireSizeMax = NULL,
#                     fireSizeMean = NULL, fireSizeDist = NULL) {


    fAAB <- 1 / FCTarget
    fireSizeMean <- mean(df$areaTotal_ha)
    totalArea <- sum(values(!is.na(fireZones))) * scaleFactor

    pAAB <- numeric()
    for(i in 1:n) {
        nFireSeq <-  rpois(lambda = nFiresMean, n = simDuration)
        areaBurned <- numeric()
        for (y in seq_along(nFireSeq)) {
            areaBurned <- append(areaBurned, sum(sample(x = df$areaTotal_ha, size = nFireSeq[y])))
        }
        mAAB <- sum(areaBurned)/simDuration
        pAAB <- append(pAAB, mAAB / totalArea)
    }
    

1/quantile(pAAB, c(0.01, 0.1, 0.25, 0.5, .75, 0.9, 0.99))   
1/mean(pAAB)
hist(1/pAAB)
