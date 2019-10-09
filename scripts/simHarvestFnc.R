simHarvest <- function(coverTypes, timeSinceFire, prescriptions) {
   
    require(raster)
    #### converting number of pixels to hectares
    scaleFactor <- prod(res(coverTypes))/10000
    
    t1 <- Sys.time()
    for (y in 1:nlayers(timeSinceFire)) {
        
        tsf <- timeSinceFire[[y]]
        
        #### initiating or aging landscape
        if(y == 1) {
            tsd <- tsh <- tsf
            tsh[] <- NA
            timeSinceHarvesting <- list()
        } else {
            tsh <- tsh + 1
            tsd <- min(tsf, tsh, na.rm = T)
        }
        
        #### applying harvesting prescriptions
        for (p in 1:nrow(prescriptions)) {
            ## determining stands eligible to harvesting
            indexCover <- coverTypes == prescriptions[p,"ID"]
            indexAge <- tsd >= prescriptions[p,"maturity"]
            rate <- prescriptions[p,"rate"]
            eligible <-  indexCover & indexAge
            
            ## determining number of cells to harvest
            nHarvCells <- round(sum(values(indexCover), na.rm = T) * rate)
            targetHarvSize <- nHarvCells * scaleFactor
            # in case there's not enough eligible cells...
            nHarvCells <- min(sum(values(eligible), na.rm = T), nHarvCells)
            ## sampling landscape
            index <- sample(which(values(eligible)>0), nHarvCells)
            ## harvesting
            tsh[index] <- 0
            ## storing realized harvesting
            timeSinceHarvesting[[y]] <- tsh
        }
        
    }
    
    timeSinceHarvesting <- stack(timeSinceHarvesting)
    names(timeSinceHarvesting) <- names(timeSinceFire) 
    t2 <- Sys.time()
    print(round(t2-t1, 2))
    
    return(timeSinceHarvesting)
}

