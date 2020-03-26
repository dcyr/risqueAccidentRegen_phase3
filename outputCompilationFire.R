###################################################################################################
###################################################################################################
##### Compiling raw fire outputs to a tidy data frame
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo", "clusterN"))])
# #######
# rm(list = ls())
# setwd("D:/test/risqueAccidentRegen_phase3/100rep_baseline/")
# ####################################################################################################
# scenario <- "baseline"
# ####################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)




#################
require(raster)
require(dplyr)
    ####################################################################
for(s in 1:length(simInfo$simID)) {

    simDir <- simInfo$simDir[s]
    fr <- simInfo$fire[s]
    mgmt <- simInfo$mgmt[s]
    simID <- simInfo$simID[s]
    
    studyArea <- raster(paste0("../", simDir, "/studyArea.tif"))
    fireZones <- raster(paste0("../", simDir, "/fireZones.tif"))
    
    ### copy fire regime.csv
    file.copy(paste0("../", simDir, "/fireRegime.csv"),
              paste0("fireRegime_", simID, ".csv"),
              overwrite = T)
    
    ## focusing on fireZones in studyArea
    z <- which(zonal(studyArea, fireZones, sum)[,"value"]>1)
    #fireZones_RAT <- fireZones_RAT[z, ]
    fireZones[is.na(studyArea)] <- NA
    fireZones[!is.na(fireZones)] <- 1
    
    
    ##
    convFactor <- prod(res(studyArea))/10000### to convert to hectares
    ## in all simulation area
    fireZoneArea <- as.data.frame(zonal(!is.na(fireZones), fireZones, sum))
    fireZoneArea$value <- fireZoneArea$value*convFactor
    colnames(fireZoneArea) <- c("ID", "areaZoneTotal_ha")
    
    
    ####################################################################
    ####################################################################
    ######
    ######      compiling simulation outputs
    ######
    outputFolder <- paste0("../", simDir, "/output")
    x <- list.files(outputFolder)
    
    index <- grep(".RData", x)
    index <- intersect(index, grep("Fire", x))
    x <- x[index]
    
    if(length(x) == 0) {
        break
    }
    
    replicates <- gsub(".RData", "", x)
    replicates <- strsplit(replicates, "_")
    replicates <- as.numeric(lapply(replicates, function(x) x[2]))
    ###########################################
    
    
    ###########################################
    
    require(doSNOW)
    require(parallel)
    require(foreach)
    # clusterN <- 2
    # clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
    #######
    cl = makeCluster(clusterN, outfile = "") ##
    registerDoSNOW(cl)
    #######
    outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# 
        require(raster)
        require(reshape2)
        require(dplyr)
        
        # s <- scenario[i]
        r <- replicates[i]
        ## fetching outputs
        fire <- get(load(paste(outputFolder, x[i], sep="/")))
        ## focusing on studyArea
        fire[is.na(studyArea)] <- NA
        
        ## compiling realized area burned
        
        areaBurned <- zonal(fire, fireZones,  "sum")[,-1] * convFactor
        year <- as.numeric(gsub("[^0-9]", "", names(areaBurned)))
        
        ## tidying up data frame
        areaBurned <- data.frame(year, replicate = r,
                                 areaBurned_ha = areaBurned)
        out <- data.frame(simID = simID,
                          fireScenario = fr,
                          mgmtScenario = mgmt,
                          areaBurned)
        print(paste(s, "fire", fr, r))
        return(out)
    }
    
    stopCluster(cl)
    
    outputCompiled <- merge(outputCompiled, fireZoneArea)
    outputCompiled <- arrange(outputCompiled, simID,
                              replicate, year)
    
    save(outputCompiled, file = paste0("outputCompiledFire_", simID, ".RData"))
}         
