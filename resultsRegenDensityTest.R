# ####################################################################
# ####################################################################
# ### Random Forest prediction test
# ###################################################################################################
# rm(list = ls())
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # necessary on my Windows machine
# setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase2", sep ="/"))
# ####################################################################################################
# ####################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
# ####################################################################
# ####################################################################


### thresholds for young stands
thresholds <- list(EPN = c(30, 50),
                   PIG = c(20, 30))


regenDensityPredict <- function(index, dens, tsd, coverTypes,
                                dens_RAT, coverTypes_RAT) {
    
    
    tsd <- tsd[init]

        
}





index <- 




    require(foreach)
    require(tidyr)
    require(raster)
    require(randomForest)
    t1 <- Sys.time()
    ## fetching disturbance history (fire and harvest)
    fires <- get(load(paste(outputDir, filesFire[i], sep = "/")))
    simID <- gsub("[^0-9]", "", filesFire[i])
    print(paste("processing sim", simID, "..."))
    harvests <- get(load(paste(outputDir, filesHarvest[grep(paste0(simID, ".RData"), filesHarvest)], sep = "/")))
    
    fires$tsf[is.na(studyArea)] <- NA
    
    tsf <- crop(fires$tsf, c(range(df$x),
                             range(df$y)))
    timesteps <- as.numeric(gsub("[^0-9]", "", names(tsf)))
    names(tsf) <- paste0("F", timesteps)
    
    tsh <- crop(harvests, c(range(df$x),
                            range(df$y)))
    
    ### extracting minimum values (time since last disturbance)
    tsd <- list()
    for (l in 1:nlayers(tsf)) {
        x <- stack(tsf[[l]], tsh[[l]])
        tsd[[l]] <- min(x, na.rm = T)
        
    }
    tsd <- stack(tsd)
    names(tsd) <- paste0("D", timesteps)
    
    ### formating tsf
    x <- rasterToPoints(tsf)
    simResults <- merge(df, x, all = F)
    
    ## formating tsd
    x <- rasterToPoints(tsd)
    simResults <- merge(simResults, x, all = F)
    
    ## 
    densDf <- foreach(j = seq_along(timesteps), .combine = "cbind") %do% {
    #densDf <- list()
    #for (j in 1:50) {    
        ts <- timesteps[j]
        # ## creating a 'density' data frame
        # if(ts == 1) {
        #     densDf <- data.frame(densityClass_0 = df$clasDens_Ini)
        # } else {
        #     densDf[,paste("densityClass", j-1, sep = "_")] <- df$clasDens_Ini
        # }
        
        ## current tsf
        fireCol <- which(colnames(simResults) == paste0("F", ts))
        ## current tsd (actually, previous year)
        if (ts == 1) { ### actually the previous  
            ageAvant <- simResults$tsfInit
        } else {
            ageAvant <- simResults[, paste0("D", ts-1)]  
        }
        ## tsf == 0; focussing on burned cells to simulate density transitions
        index <- which(simResults[,fireCol]==0)
        index <- index[!is.na(simResults[index,"clasDens_Ini"])]
        if (sum(!is.na(simResults[index,"clasDens_Ini"])) == 0) { ## skip if there is no fire;
            # or if no productive site were burned (density 'AB', 'C' or 'D'; no density transition
            x <- simResults$clasDens_Ini
        } else {
            ## extracting predictive variables
            
            x <- data.frame(simResults[index,varPredName], ageAvant = ageAvant[index] + 1)
            ## ignoring missing values (class "E" is actually considered inreversible)
            x <- x[complete.cases(x),]
            ## simulating transition (random forest)
            transProb <- as.data.frame(predict(rfDens, x,  type= "prob"))
            
            
            #############################
            ## simulating transition (ad hoc transition prob for young forests)
            ## regen failure
            indexFailure <- which(x$essReg_Ini == "EPN" & x$ageAvant < thresholds$EPN[1] |
                                       x$essReg_Ini == "PIG" & x$ageAvant < thresholds$PIG[1])
            transProb[indexFailure,] <- data.frame(AB = 0, C = 0, D = 0, E = 1)
            ## loss in productivity
            indexLossProd <- which(x$essReg_Ini == "EPN" & x$ageAvant < thresholds$EPN[2] |
                                       x$essReg_Ini == "PIG" & x$ageAvant < thresholds$PIG[2])

            indexOverlap <- intersect(indexFailure, indexLossProd)
            if(length(indexOverlap)>0) {
                indexLossProd <- indexLossProd[-intersect(indexFailure, indexLossProd)]
            }

            prob <- as.data.frame(matrix(0, ncol = length(levels(x$clasDens_Ini))+1,
                                                     nrow = length(indexLossProd)))
            if (nrow(prob)>0) {

                for (y in 1:nrow(prob)) {
                    prob[y,as.numeric(x[indexLossProd[y], "clasDens_Ini"])+1] <- 1
                }
                transProb[indexLossProd,] <- prob
            }
            #############################
                
            newDensity <- apply(transProb, 1, function(x) sample(names(x),
                                                                 size = 1,
                                                                 prob = x))
            
            # ## converting class "E" to NA
            newDensity[newDensity == "E"] <- NA
            
            ## updating index (those with no 'NAs')
            index <- as.numeric(rownames(x))
            
            ## storing yearly density
            x <- simResults$clasDens_Ini
            x[index] <- newDensity
            
            ## updating density for next time step
            simResults[index, "clasDens_Ini"] <- newDensity
        }
        
        return(x)
       
    }
    
    ### tidying things up
    colnames(densDf) <- as.numeric(gsub("result.", "", colnames(densDf)))
    
    
    densDf <- gather(as.data.frame(densDf),
                     key = "timestep",
                     value = "densityClass",
                     convert = T)
    # ## restoring "E" class
    # densDf[is.na(densDf$densityClass), "densityClass"] <- "E"
    # 
    densDf <- data.frame(simID, densDf)
    
    ## converting density classes into a factor
    tmp <- factor(levels(df$clasDens_Ini)[densDf$densityClass],
                  levels = c(levels(df$clasDens_Ini), "E"))
    ## replacing NAs by class "E"
    tmp[is.na(tmp)] <- "E"
    densDf$densityClass <- tmp
    ## computing frequency table
    densFreqTable <- table(densDf$densityClass, densDf$timestep)
    densFreqTable <- as.data.frame(densFreqTable)
    x <- data.frame(simID = simID,
                    timestep = densFreqTable[,2],
                    densityClass = densFreqTable[,1],
                    area_ha = densFreqTable[,3] * convFactor)
                    
    t2 <- Sys.time()
    print(paste(i, t2-t1))
    return(x)
    #foo[[i]] <- list()
} 
stopCluster(cl)

write.csv(densityResults, file = "outputCompiledDensity_adHoc.csv", row.names = F)


    

