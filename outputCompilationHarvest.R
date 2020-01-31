###################################################################################################
###################################################################################################
##### Compiling raw harvest outputs to a tidy data frame
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario", "clusterN", "fr", "mgmt", "initYear"))])
# #######
# rm(list = ls())
# setwd("D:/test/risqueAccidentRegen_phase3/100rep_baseline/")
# ####################################################################################################
# scenario <- "baseline"
# ####################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)

s <- seq_along(scenario)

#################
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)

####################################################################
studyArea <- raster(paste0("../", scenario, "/studyArea.tif"))
uaf <- raster(paste0("../", scenario, "/uaf.tif"))
#subZones <- raster
uaf_RAT <- read.csv(paste0("../", scenario, "/uaf_RAT.csv"))
##
convFactor <- prod(res(studyArea))/10000### to convert to hectares

####################################################################
####################################################################
######
######      compiling simulation outputs
######
outputFolder <- paste0("../", scenario[s], "/output")
x <- list.files(outputFolder)
index <- grep(".RData", x)
index <- intersect(index, grep("Harvest", x))
x <- x[index]
simInfo <- gsub(".RData", "", x)
simInfo <- strsplit(simInfo, "_")
#scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
replicates <- as.numeric(lapply(simInfo, function(x) x[2]))
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
    
    ## simInfo
    r <- replicates[i]
    
    ## fetching outputs
    harv <- get(load(paste(outputFolder, x[i], sep="/")))
    
    ## compiling realized harvests
    areaHarvested <- zonal(harv > 0, uaf,  "sum")[,-1] * convFactor
    volHarvested <- zonal(harv, uaf,  "sum")[,-1] * convFactor
    rm(harv)
    
    if(!is.numeric(areaHarvested)) {
        areaHarvested <- t(areaHarvested)
        volHarvested <- t(volHarvested)
        areaHarvested <- data.frame(areaHarvested, total = apply(areaHarvested, 1, "sum"))
        volHarvested <- data.frame(volHarvested, total = apply(volHarvested, 1, "sum"))
    } else {
        areaHarvested <- data.frame(total = areaHarvested)
        volHarvested <- data.frame(total = volHarvested)
    }
    
    
    yearH <- as.numeric(gsub("[^0-9]", "", rownames(areaHarvested)))
    
    
    colnames(areaHarvested)[uaf_RAT$ID] <-
        colnames(volHarvested)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
    
    # adding years and stacking
    
    areaHarvested[,"year"] <- volHarvested[,"year"] <- yearH
    
    areaHarvested <- melt(areaHarvested,
                          id.vars = c("year"),
                          variable.name = c("uaf"),
                          value.name = "areaHarvestedTotal_ha")
    volHarvested <- melt(volHarvested,
                         id.vars = c("year"),
                         variable.name = c("uaf"),
                         value.name = "volHarvestedTotal_cubMeter")
    
    
    salv <- paste(outputFolder, gsub("Harvest", "Salvage", x[i]), sep="/")
    salvage <- file.exists(salv)
    if(salvage) {
        salv <- get(load(salv)) 
        
        areaSalvaged <- zonal(salv > 0, uaf,  "sum")[,-1] * convFactor
        volSalvaged <- zonal(salv, uaf,  "sum")[,-1] * convFactor
        rm(salv)
        
        
        if(!is.numeric(areaSalvaged)) {
            areaSalvaged <- t(areaSalvaged)
            volSalvaged <- t(volSalvaged)
            areaSalvaged <- data.frame(areaSalvaged, total = apply(areaSalvaged, 1, "sum"))
            volSalvaged <- data.frame(volSalvaged, total = apply(volSalvaged, 1, "sum"))
        } else {
            areaSalvaged <- data.frame(total = areaSalvaged)
            volSalvaged <- data.frame(total = volSalvaged)
        }
        
        
        yearS <- as.numeric(gsub("[^0-9]", "", rownames(areaSalvaged)))
        
        colnames(areaSalvaged)[uaf_RAT$ID] <-
            colnames(volSalvaged)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
        
        areaSalvaged[,"year"] <- volSalvaged[,"year"] <- yearS
        
        areaSalvaged <- melt(areaSalvaged,
                             id.vars = c("year"),
                             variable.name = c("uaf"),
                             value.name = "areaSalvagedTotal_ha")
        volSalvaged <- melt(volSalvaged,
                            id.vars = c("year"),
                            variable.name = c("uaf"),
                            value.name = "volSalvagedTotal_cubMeter")
        
    } else {
        print(paste0("No output file for salvage logging - Replicate #", r))
    }
    
    
    
    #### retention cutting
    reten <- paste(outputFolder, gsub("Harvest", "VolReten", x[i]), sep="/")
    retention <- file.exists(reten)
    if(retention) {
        reten <- get(load(reten)) 
        
        areaReten <- zonal(reten > 0, uaf,  "sum")[,-1] * convFactor
        volReten <- zonal(reten, uaf,  "sum")[,-1] * convFactor
        rm(reten)
        
        
        if(!is.numeric(areaReten)) {
            areaReten <- t(areaReten)
            volReten <- t(volReten)
            areaReten <- data.frame(areaReten, total = apply(areaReten, 1, "sum"))
            volReten <- data.frame(volReten, total = apply(volReten, 1, "sum"))
        } else {
            areaReten <- data.frame(total = areaReten)
            volReten <- data.frame(total = volReten)
        }
        
        
        yearS <- as.numeric(gsub("[^0-9]", "", rownames(areaReten)))
        
        colnames(areaReten)[uaf_RAT$ID] <-
            colnames(volReten)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
        
        areaReten[,"year"] <- volReten[,"year"] <- yearS
        
        areaReten <- melt(areaReten,
                             id.vars = c("year"),
                             variable.name = c("uaf"),
                             value.name = "areaRetenTotal_ha")
        volReten <- melt(volReten,
                            id.vars = c("year"),
                            variable.name = c("uaf"),
                            value.name = "volRetenTotal_cubMeter")
        
    } else {
        print(paste0("No output file for retention logging - Replicate #", r))
    }
    
    
    ### plantation
    #### retention cutting
    plant <- paste(outputFolder, gsub("Harvest", "Plantation", x[i]), sep="/")
    plantation <- file.exists(plant)
    if(plantation) {
        plant <- get(load(plant)) 
        
        areaPlant <- zonal(plant > 0, uaf,  "sum")[,-1] * convFactor
        rm(plant)
        
        if(!is.numeric(areaPlant)) {
            areaPlant <- t(areaPlant)
            areaPlant <- data.frame(areaPlant, total = apply(areaPlant, 1, "sum"))
        } else {
            areaPlant <- data.frame(total = areaPlant)
        }
        
        yearP <- as.numeric(gsub("[^0-9]", "", rownames(areaPlant)))
        
        colnames(areaPlant)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
        
        areaPlant[,"year"] <- yearS
        
        areaPlant <- melt(areaPlant,
                          id.vars = c("year"),
                          variable.name = c("uaf"),
                          value.name = "areaPlantTotal_ha")
        
        
    } else {
        print(paste0("No output file for plantation - Replicate #", r))
    }
    
    
    
    out <- merge(areaHarvested, volHarvested)
    
    
    if(salvage) {
        out <- out %>%
            merge(areaSalvaged) %>%
            merge(volSalvaged)
    } else {
        out[,c("volSalvagedTotal_cubMeter", "areaSalvagedTotal_ha")] <- 0
    }
    
    if(retention) {
        out <- out %>%
            merge(areaReten) %>%
            merge(volReten)
    } else {
        out[,c("volRetenTotal_cubMeter", "areaRetenTotal_ha")] <- 0
    }
    
    if(plantation) {
        out <- out %>%
            merge(areaPlant)
    } else {
        out[,"areaPlantTotal_ha"] <- 0
    }
    
    out <- out %>%
        mutate(replicate = r,
               scenario = fr[s],
               mgmt = mgmt[s]) %>%
        select(scenario, mgmt, replicate, year, uaf,
               areaHarvestedTotal_ha,
               volHarvestedTotal_cubMeter,
               areaSalvagedTotal_ha,
               volSalvagedTotal_cubMeter,
               areaRetenTotal_ha,
               volRetenTotal_cubMeter,
               areaPlantTotal_ha)
        
    print(paste("harvest", scenario[s], r))
    return(out)
    
}

stopCluster(cl)
outputCompiled <- arrange(outputCompiled, scenario, mgmt, uaf, year)

save(outputCompiled, file = paste0("outputCompiledHarvest_", scenario, ".RData"))
