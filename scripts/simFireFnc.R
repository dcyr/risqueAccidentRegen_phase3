####################################################################
####################################################################
###### Fire model
######
###### Runs one simulation based of inputs provided upstream
###### Outputs TSF maps and/or individual fire map
######
###### Dominic Cyr
####################################################################
####################################################################

# #### for testing purposes
# tsfInit <- tsd
# simDur <- 1
# yearInit <- 2015
# fireZones
# fireRegime
# fireSizeFit
# outputTSF <- TRUE
# outputFire <- FALSE
# ### 

simFire <- function(tsfInit, simDur, yearInit,
                fireZones, fireRegime,
                fireSizeFit = NULL, fireSizeMax = NULL, ## either that line of the following
                fireSizeMean = NULL, fireSizeDist = NULL, ## either that line of the previous one
                outputTSF = TRUE, outputFire = FALSE, id) {
    
    require(raster)
    #### converting number of pixels to hectares
    scaleFactor <- prod(res(tsfInit))/10000
    
    source("../scripts/fireSpreadFnc.R")
    
    ## fire regime parameters
    zones <- fireRegime$ID
    zoneNames <- as.character(fireRegime$Zone_LN)
    zoneNames <- zoneNames[zoneNames != "total"]
    
    fireRegime[, "simYear"] <- as.numeric(substr(fireRegime$period, 1,4)) - yearInit
    
    #### Removing unnecessary periods
    ## in the future
    fireRegimeSim <- fireRegime[which(fireRegime$simYear <= simDur),]
    
    ## in the past
    if (1 %in% unique(fireRegimeSim$simYear)) {
        #fireRegimeSim <- fireRegimeSim[which(fireRegimeSim$simYear >= 1),]
        fireRegimeSim <- fireRegimeSim[which(fireRegimeSim$simYear >= 1),]
    } else { 
        index <- which(fireRegimeSim$simYear < 1)
        index <- which(fireRegimeSim$simYear >= unique(max(fireRegimeSim[index,"simYear"])))
        fireRegimeSim <- fireRegimeSim[index,]
    }
    ## setting simYear < 1 to 1
    fireRegimeSim[which(fireRegimeSim$simYear < 1), "simYear"] <- 1

     
    fireCycle <- fireRegimeSim[fireRegimeSim$Zone_LN %in% zoneNames, "fireCycle"]
     
    ##############################################################
    ##############################################################
    ####  fire regime
    if(!is.null(fireSizeFit)) {
        require(MASS)
        ### reordering fire Size distrib parameters if necessary
        fireSizeFit <- fireSizeFit[match(zoneNames, names(fireSizeFit))]
        fireSizeMax <- fireSizeMax[match(zoneNames, names(fireSizeMax))]  
        fireSizeMean <- numeric()
        for (i in zoneNames) {
            if (i == fireRegimeSim[1,"Zone_LN"]) {
                estimates <- names(fireSizeFit[[i]]$estimate)
            }
            if (estimates[1] == "rate") {
                distribType <- "exp"
                fireSizeMean <- append(fireSizeMean, 1/fireSizeFit[[i]]$estimate["rate"])
            }
            if (estimates[1] == "meanlog" & estimates[2] == "sdlog") {
                distribType <- "lognorm"
                fireSizeMean <- append(fireSizeMean,
                                       exp(fireSizeFit[[i]]$estimate["meanlog"] +
                                               0.5*fireSizeFit[[i]]$estimate["sdlog"]^2))
            }
        }
    }

    ##############################################################
    ##############################################################
    ### average fraction of the "burnable" area burned annually
    fAAB <- 1 / fireCycle

    
    ########
    ### Fire sequence (annually)
    nFiresMean <- fAAB * zonal(!is.na(fireZones), fireZones, fun='sum')[,"sum"] * scaleFactor / fireSizeMean
    
    
    
    ### Generate a yearly sequence of number of fires

    nFireSequence <- list()
    periodBegin <- unique(fireRegimeSim$simYear)
    for (p in seq_along(periodBegin)) {
        periodDuration <- min(c(periodBegin[p+1], simDur+1), na.rm = T) - periodBegin[p]
        nFiresTmp <- nFiresMean[which(fireRegimeSim$simYear==periodBegin[p])]
        nFireSeqTmp <- list()
        for(i in seq_along(nFiresTmp)) {
            nFireSeqTmp[[i]] <- rpois(lambda = nFiresTmp[i], n = periodDuration)
        }
        # put that in a matrix where each column corresponds to one fire zone
        nFireSequence[[p]] <- do.call("cbind", nFireSeqTmp)
        colnames(nFireSequence[[p]]) <- unique(zoneNames)  
    }
    nFireSequence <- do.call("rbind", nFireSequence)
    
    ##############################################################
    ##############################################################
    ##### aging and burning
    ##############################################################
    ##############################################################
    if (outputFire) {
        fires <- list()        
    }
    for (y in 1:simDur) {
        t1 <- Sys.time()
        #### aging landscape
        if (y == 1) {
            tsf <-  tsfInit ##
            timeSinceFire <- list()
        } else {
            tsf <- tsf + 1
        }
        
        ### simulating fires
        nFires <- nFireSequence[y,]
        if (sum(nFires) > 0) { #### skip if no fire events during that year
            
            eligible <- tsf > 0
            ### generating fire sizes
            for (i in seq_along(nFires)) {
                if (i == 1 & exists("fSize")) {
                    rm(fSize)
                }
                n <- nFires[i]
                if (n>0) {

                    if(!is.null(fireSizeFit)) {
                        ############################
                        #### Parametric fire size distribution
                        if (distribType == "exp") {
                            fTmp <- round(rexp(n, rate = fireSizeFit[[i]]$estimate))
                        }
                        if (distribType == "lognorm") {
                            fTmp <- round(rlnorm(n, meanlog = fireSizeFit[[i]]$estimate[1],
                                                 sdlog = fireSizeFit[[i]]$estimate[2]))
                        }
                        
                    } else {
                        ############################
                        #### Nonparametric fire size distribution
                        fTmp <- sample(fireSizeDist, size = n)
                        
                        
                    }
                    
                    fTmp <- data.frame(fireZone = i,
                                           fireSize = fTmp)
                        
                        
                    ## capping fire size to 1.5 times max observed fire size in last 30 years
                    if(!is.null(fireSizeFit)) {
                        fTmp[fTmp > fireSizeMax[[i]]] <- fireSizeMax[[i]]
                    }
                    
                    if (!exists("fSize")) {
                        fSize <- fTmp
                    } else {
                        fSize <- rbind(fSize, fTmp)
                    }  
                }
            }
            
            
            ## rounding fire sizes to cell resolution
            fSize$fireSize <- round(fSize$fireSize/scaleFactor)*scaleFactor
            ## removing fire of size 0
            fSize <- fSize[fSize$fireSize>0,]
            if (sum(values(eligible>0), na.rm = T) > 0) { ## skip if there's nothing eligible to burn
                f <- stack(fireSpread(eligible = eligible,
                                      fireSize = fSize,
                                      fireZones = fireZones,
                                      id = id))
                
                if (nlayers(f) == 1) {
                    tsf[f > 0] <- 0
                } else {
                    tsf[calc(f, sum, na.rm = T) > 0] <- 0
                }
                nFiresRealized <- nlayers(f)
                if (outputFire) {
                    fires[[y]] <- f
                } else {
                    rm(f)
                }
            }
            eligible <- tsf > 0     
        
            fSizeRealized <- sum(values(tsf==0), na.rm = T)*25
            #print(paste0("simulation ", id, " ; y ", yearInit, " ; ", sum(nFires), " fire(s) ; total ", sum(fSize$fireSize), " ha"))
            print(paste0("simulation ", id, " ; year ", yearInit, " ; ", nFiresRealized, " fire(s) ; total ", fSizeRealized, " ha"))
            
        } else {
            print(paste0("simulation ", id, " ; year ", yearInit, " ; no fires occurred")) 
        }
        ## updating final fire size (after rounding and possible extinctions)
        timeSinceFire[[y]] <- tsf
        
        t2 <- Sys.time()
        
        print(round(t2-t1, 2))
    }
    
    ## preparing outputs
    output <- list()
    if (outputFire){ ### these outputs record all individual fires (larger)
        output[["fires"]] <- fires
    }
    if (outputTSF){ ### these outputs record TSF maps
        output[["tsf"]] <- stack(timeSinceFire)
        names(output[["tsf"]]) <- paste0("Y", 1:simDur)
    }
    return(output)
}

